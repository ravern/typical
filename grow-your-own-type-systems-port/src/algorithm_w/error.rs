use core::fmt;

use super::{
  expr::{Ty, TyRef},
  parse::Rule,
};

#[derive(Debug)]
pub enum Error {
  Parse(pest::error::Error<Rule>),
  UnboundVar(String),
  NotAFunction,
  InfiniteType,
  CannotUnify(TyRef, TyRef),
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Error::Parse(error) => write!(f, "{}", error),
      Error::UnboundVar(name) => write!(f, "unbound variable '{}'", name),
      Error::NotAFunction => write!(f, "tried to call a non-function"),
      Error::InfiniteType => write!(f, "infinite type"),
      Error::CannotUnify(_ty1, _ty2) => write!(f, "cannot unify types 'todo' and 'todo'"),
    }
  }
}

impl std::error::Error for Error {
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Error::Parse(error) => Some(error),
      Error::UnboundVar(_var) => None,
      Error::NotAFunction => None,
      Error::InfiniteType => None,
      Error::CannotUnify(_ty1, _ty2) => None,
    }
  }
}
