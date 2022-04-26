use std::{
  collections::{HashMap, HashSet},
  fmt,
};

struct Substitution(HashMap<usize, Type>);

impl Substitution {
  fn new() -> Self {
    Substitution(HashMap::new())
  }

  fn insert(&mut self, var: usize, ty: Type) {
    self.0.insert(var, ty);
  }

  fn apply(&self, ty: Type) -> Type {
    match ty {
      Type::Var(var) => {
        if let Some(ty) = self.0.get(&var) {
          self.apply(ty.clone())
        } else {
          ty
        }
      }
      Type::Fun { parameters, body } => Type::Fun {
        parameters: parameters.into_iter().map(|ty| self.apply(ty)).collect(),
        body: Box::new(self.apply(*body)),
      },
      Type::Bool => Type::Bool,
      Type::Int => Type::Int,
    }
  }
}

enum Exp {
  Lam {
    parameters: Vec<String>,
    body: Box<Self>,
  },
  Call {
    arguments: Vec<Self>,
    callee: Box<Self>,
  },
  If {
    condition: Box<Self>,
    then: Box<Self>,
    otherwise: Box<Self>,
  },
  Int(usize),
  Bool(bool),
  Ident(String),
}

#[derive(Clone)]
struct Scheme {
  forall: HashSet<usize>,
  signature: Type,
}

impl Scheme {
  fn instantiate(self, ctx: &mut Ctx) -> Type {
    let substitution = Substitution(
      self
        .forall
        .into_iter()
        .map(|var| (var, ctx.fresh_type()))
        .collect(),
    );
    substitution.apply(self.signature)
  }
}

#[derive(Clone)]
enum Type {
  Fun {
    parameters: Vec<Self>,
    body: Box<Self>,
  },
  Int,
  Bool,
  Var(usize),
}

impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Fun { parameters, body } => {
        write!(f, "(")?;
        for (index, parameter) in parameters.iter().enumerate() {
          write!(f, "{}", parameter)?;
          if index != parameters.len() - 1 {
            write!(f, ", ")?;
          }
        }
        write!(f, ") -> ")?;
        write!(f, "{}", body)
      }
      Self::Int => write!(f, "Int"),
      Self::Bool => write!(f, "Bool"),
      Self::Var(var) => write!(f, "t{}", var),
    }
  }
}

#[derive(Clone)]
enum TypeError {
  UndefinedIdent(String),
  InfiniteType(Type),
  MismatchedTypes { actual: Type, expected: Type },
  DuplicateParameter(String),
}

impl fmt::Display for TypeError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TypeError::UndefinedIdent(ident) => {
        write!(f, "undefined identifier: {}", ident)
      }
      TypeError::InfiniteType(ty) => {
        write!(f, "infinite type: {}", ty)
      }
      TypeError::MismatchedTypes { actual, expected } => {
        write!(
          f,
          "mismatched types: expected {} but got {}",
          expected, actual
        )
      }
      TypeError::DuplicateParameter(parameter) => {
        write!(f, "duplicate parameter: {}", parameter,)
      }
    }
  }
}

struct Ctx {
  substitution: Substitution,
  schemes: HashMap<String, Scheme>,
  supply: usize,
}

impl Ctx {
  pub fn new() -> Ctx {
    Ctx {
      substitution: Substitution::new(),
      schemes: HashMap::new(),
      supply: 0,
    }
  }

  pub fn fresh_type(&mut self) -> Type {
    let var = self.supply;
    self.supply += 1;
    Type::Var(var)
  }
}

fn typecheck(exp: Exp, ty: Option<Type>) -> Result<Type, TypeError> {
  let ctx = &mut Ctx::new();
  ctx.schemes.insert(
    "foo".to_string(),
    Scheme {
      forall: HashSet::new(),
      signature: Type::Int,
    },
  );

  if let Some(ty) = ty {
    check(ctx, exp, ty.clone()).map(|_| ty)
  } else {
    infer(ctx, exp)
  }
}

fn infer(ctx: &mut Ctx, exp: Exp) -> Result<Type, TypeError> {
  match exp {
    Exp::Int(_) => Ok(Type::Int),
    Exp::Bool(_) => Ok(Type::Bool),
    Exp::Ident(varent) => {
      if let Some(scheme) = ctx.schemes.get(&varent) {
        Ok(scheme.clone().instantiate(ctx))
      } else {
        Err(TypeError::UndefinedIdent(varent))
      }
    }
    Exp::If {
      condition,
      then,
      otherwise,
    } => {
      check(ctx, *condition, Type::Bool)?;
      let then_ty = infer(ctx, *then)?;
      check(ctx, *otherwise, then_ty.clone())?;
      Ok(then_ty)
    }
    Exp::Lam { parameters, body } => {
      let mut binders: Vec<(String, Type)> = Vec::new();
      for parameter in parameters {
        if let Some((ident, _)) =
          binders.iter().find(|(ident, _)| ident == &parameter)
        {
          return Err(TypeError::DuplicateParameter(ident.clone()));
        }
        binders.push((parameter, ctx.fresh_type()))
      }

      // TODO: Implement extending of env and inference of body type.
      unimplemented!();
    }
    _ => unimplemented!(),
  }
}

fn check(ctx: &mut Ctx, exp: Exp, ty: Type) -> Result<(), TypeError> {
  let expected_ty = infer(ctx, exp)?;
  unify(ctx, ty, expected_ty)
}

fn unify(ctx: &mut Ctx, ty: Type, expected_ty: Type) -> Result<(), TypeError> {
  let ty = ctx.substitution.apply(ty);
  let expected_ty = ctx.substitution.apply(expected_ty);

  match (ty, expected_ty) {
    (Type::Var(actual), Type::Var(expected)) if actual == expected => Ok(()),
    (Type::Var(var), ty) => bind(ctx, var, ty),
    (ty, Type::Var(var)) => bind(ctx, var, ty),
    (Type::Int, Type::Int) => Ok(()),
    (Type::Bool, Type::Bool) => Ok(()),
    (
      Type::Fun { parameters, body },
      Type::Fun {
        parameters: expected_parameters,
        body: expected_body,
      },
    ) => {
      let error = TypeError::MismatchedTypes {
        actual: Type::Fun {
          parameters: parameters.clone(),
          body: body.clone(),
        },
        expected: Type::Fun {
          parameters: expected_parameters.clone(),
          body: expected_body.clone(),
        },
      };

      if parameters.len() != expected_parameters.len() {
        return Err(error);
      }

      let parameters =
        parameters.into_iter().zip(expected_parameters.into_iter());

      for (parameter, expected_parameter) in parameters {
        unify(ctx, parameter, expected_parameter).map_err(|_| error.clone())?;
      }

      unify(ctx, *body, *expected_body).map_err(|_| error)
    }
    (ty, expected_ty) => Err(TypeError::MismatchedTypes {
      actual: ty,
      expected: expected_ty,
    }),
  }
}

fn bind(ctx: &mut Ctx, var: usize, ty: Type) -> Result<(), TypeError> {
  if type_variables(&ty).contains(&var) {
    return Err(TypeError::InfiniteType(ty));
  }
  ctx.substitution.insert(var, ty);
  Ok(())
}

fn type_variables(ty: &Type) -> HashSet<usize> {
  let mut vars = HashSet::new();
  match ty {
    Type::Fun { parameters, body } => {
      parameters.iter().for_each(|ty| {
        vars.extend(type_variables(ty));
      });
      vars.extend(type_variables(&*body));
    }
    Type::Int => {}
    Type::Bool => {}
    Type::Var(var) => {
      vars.insert(*var);
    }
  }
  vars
}

fn main() {
  let exp = Exp::Lam {
    parameters: vec!["foo".to_string()],
    body: Box::new(Exp::Ident("foo".to_string())),
  };
  match typecheck(exp, Some(Type::Bool)) {
    Ok(ty) => println!("{}", ty),
    Err(error) => println!("Error: {}", error),
  }
}
