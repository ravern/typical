use internment::Intern;

use crate::ty::Type;

#[derive(Debug)]
pub enum TypeError {
  UndefinedIdentifier(Intern<String>),
  MismatchedTypes(Type, Type),
}
