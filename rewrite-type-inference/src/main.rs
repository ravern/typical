use std::{
  collections::HashSet,
  fmt::{self, Display},
};

#[derive(Clone, Debug, PartialEq)]
enum Type {
  Var(usize),
  Fun(Box<Self>, Box<Self>),
  Con(String, Vec<Self>),
}

impl Type {
  fn ty_vars(&self) -> HashSet<usize> {
    match self {
      Self::Var(var) => {
        let mut ty_vars = HashSet::new();
        ty_vars.insert(*var);
        ty_vars
      }
      Self::Fun(input, output) => {
        HashSet::from_iter(input.ty_vars().union(&output.ty_vars()).cloned())
      }
      Self::Con(_, tys) => {
        let mut ty_vars = HashSet::new();
        for ty in tys {
          ty_vars.extend(ty.ty_vars());
        }
        ty_vars
      }
    }
  }
}

impl Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::Var(var) => {
        write!(f, "{}", char::from_u32(*var as u32 + 'a' as u32).unwrap())
      }
      Self::Fun(input, output) => {
        write!(f, "{} -> {}", input, output)
      }
      Self::Con(ident, tys) => {
        write!(f, "{}", ident)?;
        for ty in tys {
          write!(f, " {}", ty)?;
        }
        Ok(())
      }
    }
  }
}

#[derive(Debug)]
enum Expr {
  Int(i64),
  Bool(bool),
  Ident(String),
  Lam(String, Box<Self>),
  Call(Box<Self>, Box<Self>),
  Let(String, Box<Self>, Box<Self>),
  LetRec(String, Box<Self>, Box<Self>),
  If(Box<Self>, Box<Self>, Box<Self>),
}

enum TypeError {
  UndefinedIdent(String),
  MismatchedTypes(Type, Type),
}

impl Display for TypeError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::UndefinedIdent(ident) => {
        write!(f, "undefined identifier: {}", ident)
      }
      Self::MismatchedTypes(expected, actual) => {
        write!(
          f,
          "mismatched types: expected {} but got {}",
          expected, actual
        )
      }
    }
  }
}

#[derive(Clone, Debug)]
enum Env {
  Empty,
  Frame(String, Scheme, Box<Env>),
}

impl Env {
  fn empty() -> Self {
    Self::Empty
  }

  fn extend(&self, ident: String, scheme: Scheme) -> Self {
    Self::Frame(ident, scheme, Box::new(self.clone()))
  }

  fn get(&self, ident: &str) -> Option<Scheme> {
    match self {
      Self::Empty => None,
      Self::Frame(existing_ident, scheme, parent) => {
        if existing_ident == ident {
          Some(scheme.clone())
        } else {
          parent.get(ident)
        }
      }
    }
  }

  fn ty_vars(&self) -> HashSet<usize> {
    match self {
      Self::Empty => HashSet::new(),
      Self::Frame(_, scheme, parent) => scheme
        .ty_vars()
        .union(&parent.ty_vars())
        .into_iter()
        .cloned()
        .collect(),
    }
  }
}

#[derive(Clone, Debug)]
enum Subst {
  Empty,
  Pair(Type, Type, Box<Subst>),
}

impl Subst {
  fn empty() -> Self {
    Self::Empty
  }

  fn extend(&self, from: Type, to: Type) -> Self {
    Self::Pair(from, to, Box::new(self.clone()))
  }

  fn get(&self, ty: &Type) -> Type {
    match self {
      Self::Empty => ty.clone(),
      Self::Pair(from, to, parent) => {
        if from == ty {
          to.clone()
        } else {
          parent.get(ty)
        }
      }
    }
  }

  fn apply(&self, ty: &Type) -> Type {
    match ty {
      Type::Var(var) => {
        let to_ty = self.get(&Type::Var(*var));
        if ty == &to_ty {
          to_ty
        } else {
          self.apply(&to_ty)
        }
      }
      Type::Fun(input, output) => {
        Type::Fun(Box::new(self.apply(input)), Box::new(self.apply(output)))
      }
      Type::Con(ident, ty_vars) => Type::Con(
        ident.clone(),
        ty_vars.iter().map(|ty| self.apply(ty)).collect(),
      ),
    }
  }
}

#[derive(Clone, Debug)]
struct Scheme(Type, HashSet<usize>);

impl Scheme {
  fn generalize(env: &Env, ty: &Type) -> Scheme {
    Scheme(
      ty.clone(),
      ty.ty_vars()
        .difference(&env.ty_vars())
        .into_iter()
        .cloned()
        .collect(),
    )
  }

  fn instantiate(&self, supply: &mut Supply) -> Type {
    self
      .1
      .iter()
      .fold(Subst::empty(), |subst, var| {
        subst.extend(Type::Var(*var), Type::Var(supply.next()))
      })
      .apply(&self.0)
  }

  fn ty_vars(&self) -> HashSet<usize> {
    self.1.clone()
  }
}

struct Supply(usize);

impl Supply {
  fn new() -> Self {
    Supply(0)
  }

  fn next(&mut self) -> usize {
    let var = self.0;
    self.0 += 1;
    var
  }
}

fn unify(subst: &Subst, expected: &Type, actual: &Type) -> Result<Subst, TypeError> {
  let expected = subst.apply(expected);
  let actual = subst.apply(actual);
  match (expected, actual) {
    (Type::Var(expected_var), Type::Var(actual_var)) if expected_var == actual_var => {
      Ok(subst.clone())
    }
    (Type::Var(var), actual) if !actual.ty_vars().contains(&var) => {
      Ok(subst.extend(Type::Var(var), actual.clone()))
    }
    (expected, Type::Var(var)) => unify(subst, &Type::Var(var), &expected),
    (Type::Fun(expected_input, expected_output), Type::Fun(actual_input, actual_output)) => unify(
      &unify(subst, &expected_output, &actual_output)?,
      &expected_input,
      &actual_input,
    ),
    (Type::Con(expected_ident, expected_ty_vars), Type::Con(actual_ident, actual_ty_vars))
      if expected_ident == actual_ident =>
    {
      expected_ty_vars
        .iter()
        .zip(actual_ty_vars)
        .fold(Ok(subst.clone()), |subst, (expected, actual)| {
          unify(&subst?, &expected, &actual)
        })
    }
    (expected, actual) => Err(TypeError::MismatchedTypes(actual, expected)),
  }
}

fn infer(
  env: &Env,
  subst: &Subst,
  supply: &mut Supply,
  expr: &Expr,
  ty: &Type,
) -> Result<Subst, TypeError> {
  match expr {
    Expr::Ident(ident) => {
      if let Some(scheme) = env.get(ident) {
        unify(subst, &scheme.instantiate(supply), ty)
      } else {
        Err(TypeError::UndefinedIdent(ident.to_string()))
      }
    }
    Expr::Lam(ident, body) => {
      let input_ty = Type::Var(supply.next());
      let output_ty = Type::Var(supply.next());
      let subst = unify(
        subst,
        ty,
        &Type::Fun(Box::new(input_ty.clone()), Box::new(output_ty.clone())),
      )?;
      let env = env.extend(ident.to_string(), Scheme(input_ty, HashSet::new()));
      infer(&env, &subst, supply, body, &output_ty)
    }
    Expr::Call(lam, arg) => {
      let arg_ty = Type::Var(supply.next());
      let subst = infer(
        env,
        subst,
        supply,
        lam,
        &Type::Fun(Box::new(arg_ty.clone()), Box::new(ty.clone())),
      )?;
      infer(env, &subst, supply, arg, &arg_ty)
    }
    Expr::Let(ident, arg, body) => {
      let arg_ty = Type::Var(supply.next());
      let subst = infer(env, subst, supply, arg, &arg_ty)?;
      let env = env.extend(
        ident.clone(),
        Scheme::generalize(env, &subst.apply(&arg_ty)),
      );
      infer(&env, &subst, supply, body, ty)
    }
    Expr::LetRec(ident, arg, body) => {
      let arg_ty = Type::Var(supply.next());
      let env = env.extend(ident.clone(), Scheme::generalize(env, &arg_ty));
      let subst = infer(&env, subst, supply, arg, &arg_ty)?;
      let env = env.extend(
        ident.clone(),
        Scheme::generalize(&env, &subst.apply(&arg_ty)),
      );
      infer(&env, &subst, supply, body, ty)
    }
    Expr::Bool(_) => unify(subst, &Type::Con("Bool".to_string(), vec![]), ty),
    Expr::Int(_) => unify(subst, &Type::Con("Int".to_string(), vec![]), ty),
    Expr::If(predicate, then, otherwise) => {
      let subst = infer(
        env,
        subst,
        supply,
        predicate,
        &Type::Con("Bool".to_string(), vec![]),
      )?;
      let subst = infer(env, &subst, supply, then, &ty)?;
      infer(env, &subst, supply, otherwise, &ty)
    }
    _ => unimplemented!(),
  }
}

fn typecheck(env: &Env, expr: &Expr) -> Result<Type, TypeError> {
  let mut supply = Supply::new();
  let ty = Type::Var(supply.next());
  let subst = infer(env, &Subst::empty(), &mut supply, expr, &ty)?;
  Ok(subst.apply(&ty))
}

fn main() {
  let env = Env::empty().extend(
    "foo".to_string(),
    Scheme(Type::Con("Int".to_string(), vec![]), HashSet::new()),
  );
  let expr = Expr::Call(
    Box::new(Expr::Lam(
      "foo".to_string(),
      Box::new(Expr::If(
        Box::new(Expr::Ident("foo".to_string())),
        Box::new(Expr::Int(32)),
        Box::new(Expr::Int(32)),
      )),
    )),
    Box::new(Expr::Int(32)),
  );
  match typecheck(&env, &expr) {
    Ok(ty) => println!("{}", ty),
    Err(error) => println!("Error: {}", error),
  }
}
