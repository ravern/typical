use std::{
  collections::{hash_map::RandomState, HashMap, HashSet},
  mem::uninitialized,
};

use internment::Intern;

use ast::{
  BinaryOperator, ClosureParameter, Expression, Literal, StructDeclaration, UnaryOperator,
};
use error::TypeError;
use ty::{Primitive, StructType, Type};

mod ast;
mod error;
mod ty;

#[derive(Debug, Default)]
pub struct Environment {
  structs: HashMap<Intern<String>, StructDeclaration>,
  variables: HashMap<Intern<String>, Type>,
}

impl Environment {
  pub fn new(
    structs: HashMap<Intern<String>, StructDeclaration>,
    variables: HashMap<Intern<String>, Type>,
  ) -> Self {
    Self { structs, variables }
  }

  pub fn with_parent(parent: &Self, mut variables: HashMap<Intern<String>, Type>) -> Self {
    variables.extend(parent.variables.clone());
    Self {
      structs: parent.structs.clone(),
      variables,
    }
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
  match (expression, expected_ty) {
    (Expression::Closure(closure_expression), Type::Function(function_ty))
      if closure_expression.parameters.len() == function_ty.parameters.len() =>
    {
      let mut variables = HashMap::new();
      closure_expression
        .parameters
        .iter()
        .zip(function_ty.parameters.iter())
        .map(|(closure_parameter, ty)| {
          if let Some(annotated_ty) = &closure_parameter.ty {
            subtype(environment, ty, annotated_ty)?;
          }
          variables.insert(closure_parameter.identifier.clone(), ty.clone());
          Ok(())
        })
        .collect::<Result<_, TypeError>>()?;
      if let Some(return_ty) = &closure_expression.return_ty {
        subtype(&environment, return_ty, &function_ty.return_ty)?;
      }
      let environment = Environment::with_parent(environment, variables);
      check(
        &environment,
        &closure_expression.body,
        &function_ty.return_ty,
      )
    }
    (expression, expected_ty) => {
      let ty = synthesize(environment, expression)?;
      subtype(environment, &ty, expected_ty)
    }
  }
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
    Expression::BinaryOperation(binary_operation) => match &binary_operation.operator {
      BinaryOperator::Add
      | BinaryOperator::Subtract
      | BinaryOperator::Multiply
      | BinaryOperator::Divide => {
        let left_ty = synthesize(environment, &binary_operation.left_operand)?;
        if let Type::Primitive(Primitive::Int) | Type::Primitive(Primitive::Float) = &left_ty {
          check(environment, &binary_operation.right_operand, &left_ty)?;
          Ok(left_ty)
        } else {
          Err(TypeError::MismatchedTypes(
            Type::Primitive(Primitive::Int),
            left_ty,
          ))
        }
      }
      BinaryOperator::Modulo => {
        check(
          environment,
          &binary_operation.left_operand,
          &Type::Primitive(Primitive::Int),
        )?;
        check(
          environment,
          &binary_operation.right_operand,
          &Type::Primitive(Primitive::Int),
        )?;
        Ok(Type::Primitive(Primitive::Int))
      }
      BinaryOperator::And | BinaryOperator::Or => {
        check(
          environment,
          &binary_operation.left_operand,
          &Type::Primitive(Primitive::Bool),
        )?;
        check(
          environment,
          &binary_operation.right_operand,
          &Type::Primitive(Primitive::Bool),
        )?;
        Ok(Type::Primitive(Primitive::Bool))
      }
      BinaryOperator::Equals | BinaryOperator::NotEquals => {
        let left_ty = synthesize(environment, &binary_operation.left_operand)?;
        let right_ty = synthesize(environment, &binary_operation.right_operand)?;
        if subtype(environment, &left_ty, &right_ty).is_err() {
          subtype(environment, &left_ty, &right_ty)?;
        }
        Ok(Type::Primitive(Primitive::Bool))
      }
    },
    Expression::UnaryOperation(unary_operation) => match &unary_operation.operator {
      UnaryOperator::Negate => {
        let ty = synthesize(environment, &unary_operation.operand)?;
        if let Type::Primitive(Primitive::Int) | Type::Primitive(Primitive::Float) = &ty {
          Ok(ty)
        } else {
          Err(TypeError::MismatchedTypes(
            ty,
            Type::Primitive(Primitive::Int),
          ))
        }
      }
      UnaryOperator::Not => {
        check(
          environment,
          &unary_operation.operand,
          &Type::Primitive(Primitive::Bool),
        )?;
        Ok(Type::Primitive(Primitive::Bool))
      }
    },
    Expression::Struct(struct_expression) => {
      let declaration = environment
        .structs
        .get(&struct_expression.identifier)
        .ok_or(TypeError::UndefinedIdentifier(
          struct_expression.identifier.clone(),
        ))?;

      let mut fields = HashMap::new();
      for field in &declaration.fields {
        fields.insert(field.identifier.clone(), field.ty.clone());
      }

      for field in &struct_expression.fields {
        if !fields.contains_key(&field.identifier) {
          return Err(TypeError::InvalidField(
            declaration.identifier.clone(),
            field.identifier.clone(),
          ));
        }
        fields.remove(&field.identifier);
      }

      if !fields.is_empty() {
        return Err(TypeError::MissingField(
          declaration.identifier.clone(),
          fields.keys().next().unwrap().clone(),
        ));
      }

      Ok(Type::Struct(declaration.clone().into()))
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
    (Type::Function(sub_function_ty), Type::Function(function_ty))
      if sub_function_ty.parameters.len() == function_ty.parameters.len() =>
    {
      sub_function_ty
        .parameters
        .iter()
        .zip(function_ty.parameters.iter())
        .map(|(sub_parameter, parameter)| subtype(environment, parameter, sub_parameter))
        .collect::<Result<_, TypeError>>()?;
      subtype(
        environment,
        &sub_function_ty.return_ty,
        &function_ty.return_ty,
      )
    }
    (Type::Struct(sub_struct_ty), Type::Struct(struct_ty))
      if sub_struct_ty.identifier == struct_ty.identifier =>
    {
      let mut sub_fields = HashMap::new();
      for field in &sub_struct_ty.fields {
        sub_fields.insert(field.identifier.clone(), field.ty.clone());
      }

      for field in &struct_ty.fields {
        if let Some(sub_field_ty) = sub_fields.get(&field.identifier) {
          subtype(environment, &sub_field_ty, &field.ty)?;
          sub_fields.remove(&field.identifier);
        } else {
          return Err(TypeError::MismatchedTypes(sub_ty.clone(), ty.clone()));
        }
      }

      if sub_fields.is_empty() {
        Ok(())
      } else {
        Err(TypeError::MismatchedTypes(sub_ty.clone(), ty.clone()))
      }
    }
    _ => Err(TypeError::MismatchedTypes(sub_ty.clone(), ty.clone())),
  }
}

#[cfg(test)]
mod tests {
  use std::collections::HashMap;

  use internment::Intern;

  use crate::ast::{
    BinaryOperation, BinaryOperator, CallExpression, ClosureExpression, ClosureParameter,
    Expression, Literal, StructDeclaration, StructExpression, StructExpressionField, StructField,
    UnaryOperation, UnaryOperator,
  };
  use crate::ty::{self, FunctionType, Primitive, StructType, Type};
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
      &Environment::new(HashMap::new(), variables),
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
      &Environment::new(HashMap::new(), variables),
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

  #[test]
  fn check_binary_expression() {
    check(
      &Environment::default(),
      &Expression::BinaryOperation(BinaryOperation {
        operator: BinaryOperator::Add,
        left_operand: Box::new(Expression::Literal(Literal::Float(3.2))),
        right_operand: Box::new(Expression::Literal(Literal::Float(4.1))),
      }),
      &Type::Primitive(Primitive::Float),
    )
    .unwrap();
    check(
      &Environment::default(),
      &Expression::BinaryOperation(BinaryOperation {
        operator: BinaryOperator::Equals,
        left_operand: Box::new(Expression::Literal(Literal::Int(32))),
        right_operand: Box::new(Expression::Literal(Literal::Int(433))),
      }),
      &Type::Primitive(Primitive::Bool),
    )
    .unwrap();
  }

  #[test]
  fn check_unary_expression() {
    check(
      &Environment::default(),
      &Expression::UnaryOperation(UnaryOperation {
        operator: UnaryOperator::Negate,
        operand: Box::new(Expression::Literal(Literal::Float(3.2))),
      }),
      &Type::Primitive(Primitive::Float),
    )
    .unwrap();
    check(
      &Environment::default(),
      &Expression::BinaryOperation(BinaryOperation {
        operator: BinaryOperator::Equals,
        left_operand: Box::new(Expression::Literal(Literal::Int(32))),
        right_operand: Box::new(Expression::Literal(Literal::Int(433))),
      }),
      &Type::Primitive(Primitive::Bool),
    )
    .unwrap();
  }

  #[test]
  fn check_closure_expression() {
    check(
      &Environment::default(),
      &Expression::Closure(ClosureExpression {
        parameters: vec![ClosureParameter {
          identifier: Intern::new("foo".to_string()),
          ty: None,
        }],
        body: Box::new(Expression::Identifier(Intern::new("foo".to_string()))),
        return_ty: Some(Type::Primitive(Primitive::Int)),
      }),
      &Type::Function(FunctionType {
        parameters: vec![Type::Primitive(Primitive::Int)],
        return_ty: Box::new(Type::Primitive(Primitive::Int)),
      }),
    )
    .unwrap();
  }

  #[test]
  fn check_struct_expression() {
    let foo = Intern::new("Foo".to_string());
    let bar = Intern::new("bar".to_string());
    let baz = Intern::new("baz".to_string());

    let mut structs = HashMap::new();
    structs.insert(
      foo,
      StructDeclaration {
        identifier: foo.clone(),
        fields: vec![
          StructField {
            identifier: bar.clone(),
            ty: Type::Primitive(Primitive::Int),
          },
          StructField {
            identifier: baz.clone(),
            ty: Type::Primitive(Primitive::Bool),
          },
        ],
      },
    );

    check(
      &Environment::new(structs, HashMap::new()),
      &Expression::Struct(StructExpression {
        identifier: foo.clone(),
        fields: vec![
          StructExpressionField {
            identifier: bar.clone(),
            expression: Expression::Literal(Literal::Int(32)),
          },
          StructExpressionField {
            identifier: baz.clone(),
            expression: Expression::Literal(Literal::Bool(false)),
          },
        ],
      }),
      &Type::Struct(StructType {
        identifier: foo.clone(),
        fields: vec![
          ty::StructField {
            identifier: bar.clone(),
            ty: Box::new(Type::Primitive(Primitive::Int)),
          },
          ty::StructField {
            identifier: baz.clone(),
            ty: Box::new(Type::Primitive(Primitive::Bool)),
          },
        ],
      }),
    )
    .unwrap();
  }
}
