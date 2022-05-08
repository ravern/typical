use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
  Lit(Lit),
  Var(String),
  Exist(String),
  Quant(String, Box<Type>),
  Fun(Box<Type>, Box<Type>),
}

impl Type {
  pub fn is_monotype(&self) -> bool {
    match self {
      Self::Quant(..) => false,
      Self::Fun(arg, body) => arg.is_monotype() && body.is_monotype(),
      _ => true,
    }
  }
}

impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Lit(lit) => write!(f, "{}", lit),
      Self::Exist(exist) => write!(f, "{}", exist),
      Self::Fun(arg, body) => write!(f, "{} -> {}", arg, body),
      _ => unimplemented!(),
    }
  }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
  Unit,
  Int,
  Float,
  Bool,
  String,
}

impl fmt::Display for Lit {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Unit => write!(f, "()"),
      Self::Int => write!(f, "Int"),
      Self::Float => write!(f, "Float"),
      Self::Bool => write!(f, "Bool"),
      Self::String => write!(f, "String"),
      _ => unimplemented!(),
    }
  }
}
