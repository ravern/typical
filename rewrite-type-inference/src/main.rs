use std::{
  collections::HashSet,
  fmt::{self, Display},
};

#[derive(Clone, PartialEq)]
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

enum Expr {
  Num(i64),
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

#[derive(Clone)]
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
          parent.get(existing_ident)
        }
      }
    }
  }
}

#[derive(Clone)]
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
}

#[derive(Clone)]
struct Scheme(Type, HashSet<usize>);

impl Scheme {
  // fn instantiate(&self, supply: &mut Supply) -> Type {}
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

fn main() {
  println!(
    "{}",
    Type::Fun(
      Box::new(Type::Con("List".to_string(), vec![Type::Var(0)])),
      Box::new(Type::Var(1))
    )
  );
}
