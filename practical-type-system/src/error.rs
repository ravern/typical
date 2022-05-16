use crate::ty::Type;

#[derive(Debug)]
pub enum TypeError {
  MismatchedTypes(Type, Type),
}
