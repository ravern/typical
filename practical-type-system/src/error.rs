use internment::Intern;

use crate::ast::Expression;
use crate::ty::Type;

#[derive(Debug)]
pub enum TypeError {
  ExpressionNotCallable(Expression),
  WrongArity(Expression, usize, usize),
  UndefinedIdentifier(Intern<String>),
  MismatchedTypes(Type, Type),
}
