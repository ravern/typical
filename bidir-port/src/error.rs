use std::fmt;

use crate::ty::Type;

#[derive(Debug)]
pub enum TypeError {
  UndefinedVar(String),
  InvalidType(Type),
  MismatchedTypes(Type, Type),
  CyclicalType,
}

impl fmt::Display for TypeError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::UndefinedVar(var) => write!(f, "undefined variable: {}", var),
      Self::InvalidType(ty) => write!(f, "invalid type: {}", ty),
      Self::MismatchedTypes(ty, expected_ty) => write!(
        f,
        "mismatched types: tried to match {} with {}",
        ty, expected_ty
      ),
      Self::CyclicalType => write!(f, "cyclical type found, unable to typecheck"),
    }
  }
}

pub type Result<T> = std::result::Result<T, TypeError>;
