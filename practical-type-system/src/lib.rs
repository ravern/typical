use std::collections::HashMap;

use internment::Intern;

use ast::{Expression, Literal};
use error::TypeError;
use ty::{Primitive, Type};

mod ast;
mod error;
mod ty;

#[derive(Debug, Default)]
pub struct Environment {
  variables: HashMap<Intern<String>, Type>,
}

impl Environment {
  pub fn new(variables: HashMap<Intern<String>, Type>) -> Self {
    Self { variables }
  }

  pub fn with_parent(parent: &Self, mut variables: HashMap<Intern<String>, Type>) -> Self {
    variables.extend(parent.variables.clone());
    Self { variables }
  }

  pub fn get(&self, identifier: &Intern<String>) -> Option<&Type> {
    self.variables.get(identifier)
  }
}

pub fn check(
  environment: &Environment,
  expression: &Expression,
  expected_ty: &Type,
) -> Result<(), TypeError> {
  let ty = synthesize(environment, expression)?;
  subtype(environment, &ty, expected_ty)
}

pub fn synthesize(environment: &Environment, expression: &Expression) -> Result<Type, TypeError> {
  match expression {
    Expression::Literal(Literal::Int(..)) => Ok(Type::Primitive(Primitive::Int)),
    Expression::Literal(Literal::Float(..)) => Ok(Type::Primitive(Primitive::Float)),
    Expression::Literal(Literal::String(..)) => Ok(Type::Primitive(Primitive::String)),
    Expression::Literal(Literal::Bool(..)) => Ok(Type::Primitive(Primitive::Bool)),
    Expression::Identifier(identifier) => environment
      .get(identifier)
      .cloned()
      .ok_or(TypeError::UndefinedIdentifier(identifier.clone())),
    Expression::Call(call_expression) => {
      if let Type::Function(function_ty) = synthesize(environment, &call_expression.callee)? {
        if function_ty.parameters.len() != call_expression.arguments.len() {
          return Err(TypeError::WrongArity(
            *call_expression.callee.clone(),
            function_ty.parameters.len(),
            call_expression.arguments.len(),
          ));
        }
        function_ty
          .parameters
          .iter()
          .zip(call_expression.arguments.iter())
          .map(|(parameter, argument)| check(environment, argument, &parameter))
          .collect::<Result<_, TypeError>>()?;
        Ok(*function_ty.return_ty.clone())
      } else {
        Err(TypeError::ExpressionNotCallable(
          *call_expression.callee.clone(),
        ))
      }
    }
    _ => unimplemented!(),
  }
}

pub fn subtype(environment: &Environment, sub_ty: &Type, ty: &Type) -> Result<(), TypeError> {
  match (sub_ty, ty) {
    (Type::Primitive(Primitive::Int), Type::Primitive(Primitive::Int)) => Ok(()),
    (Type::Primitive(Primitive::Float), Type::Primitive(Primitive::Float)) => Ok(()),
    (Type::Primitive(Primitive::String), Type::Primitive(Primitive::String)) => Ok(()),
    (Type::Primitive(Primitive::Bool), Type::Primitive(Primitive::Bool)) => Ok(()),
    _ => Err(TypeError::MismatchedTypes(sub_ty.clone(), ty.clone())),
  }
}

#[cfg(test)]
mod tests {
  use std::collections::HashMap;

  use internment::Intern;

  use crate::ast::{CallExpression, Expression, Literal};
  use crate::ty::{self, FunctionType, Primitive, Type};
  use crate::{check, Environment};

  #[test]
  fn check_int_expression() {
    check(
      &Environment::default(),
      &Expression::Literal(Literal::Int(32)),
      &Type::Primitive(Primitive::Int),
    )
    .unwrap();
  }

  #[test]
  fn check_float_expression() {
    check(
      &Environment::default(),
      &Expression::Literal(Literal::Float(3.14159)),
      &Type::Primitive(Primitive::Float),
    )
    .unwrap();
  }

  #[test]
  fn check_identifier_expression() {
    let mut variables = HashMap::new();
    variables.insert(
      Intern::new("foo".to_string()),
      Type::Primitive(Primitive::String),
    );

    check(
      &Environment::new(variables),
      &Expression::Identifier(Intern::new("foo".to_string())),
      &Type::Primitive(Primitive::String),
    )
    .unwrap();
  }

  #[test]
  fn check_call_expression() {
    let mut variables = HashMap::new();
    variables.insert(
      Intern::new("foo".to_string()),
      Type::Function(FunctionType {
        parameters: vec![
          Type::Primitive(Primitive::Int),
          Type::Primitive(Primitive::Int),
        ],
        return_ty: Box::new(Type::Primitive(Primitive::Bool)),
      }),
    );

    check(
      &Environment::new(variables),
      &Expression::Call(CallExpression {
        callee: Box::new(Expression::Identifier(Intern::new("foo".to_string()))),
        arguments: vec![
          Expression::Literal(Literal::Int(3)),
          Expression::Literal(Literal::Int(2)),
        ],
      }),
      &Type::Primitive(Primitive::Bool),
    )
    .unwrap();
  }
}
