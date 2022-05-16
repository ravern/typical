use std::collections::HashMap;

use ast::{BlockExpression, Declaration, Expression, Literal, Module, Statement};
use error::TypeError;
use internment::Intern;
use ty::{FunctionType, Primitive, StructType, Type};

mod ast;
mod error;
mod ty;

#[derive(Debug, Default)]
pub struct SubstitutionMap(HashMap<Type, Type>);

#[derive(Clone, Debug, Default)]
pub struct KindEnvironment {
  struct_tys: HashMap<Intern<String>, StructType>,
  function_tys: HashMap<Intern<String>, FunctionType>,
}

impl KindEnvironment {
  pub fn insert_struct(&mut self, identifier: Intern<String>, struct_ty: StructType) {
    self.struct_tys.insert(identifier, struct_ty);
  }

  pub fn insert_function(&mut self, identifier: Intern<String>, function_ty: FunctionType) {
    self.function_tys.insert(identifier, function_ty);
  }
}

#[derive(Clone, Debug, Default)]
pub struct TypeEnvironment {
  tys: HashMap<Intern<String>, Type>,
}

pub fn check(module: &Module) -> Result<(), TypeError> {
  let mut kind_environment = KindEnvironment::default();

  for declaration in module.declarations.iter() {
    match declaration {
      Declaration::Struct(struct_declaration) => kind_environment.insert_struct(
        struct_declaration.identifier.clone(),
        struct_declaration.clone().into(),
      ),
      Declaration::Function(function_declaration) => kind_environment.insert_function(
        function_declaration.identifier.clone(),
        function_declaration.clone().into(),
      ),
    }
  }

  for declaration in module.declarations.iter() {
    if let Declaration::Function(function_declaration) = declaration {
      let mut tys = HashMap::new();
      for parameter in function_declaration.parameters.iter() {
        tys.insert(parameter.identifier, parameter.ty.clone());
      }
      let ty_environment = TypeEnvironment { tys };

      check_block_expression(
        &mut Context {
          ty_supply: TypeSupply::default(),
          kind_environment: kind_environment.clone(),
          ty_environment,
          substitution_map: SubstitutionMap::default(),
        },
        &function_declaration.body,
        &function_declaration.return_ty,
      )?;
    }
  }

  Ok(())
}
pub struct Context {
  ty_supply: TypeSupply,
  substitution_map: SubstitutionMap,
  kind_environment: KindEnvironment,
  ty_environment: TypeEnvironment,
}

pub fn check_expression(
  context: &mut Context,
  expression: &Expression,
  ty: &Type,
) -> Result<(), TypeError> {
  match (expression, ty) {
    (Expression::Literal(Literal::Int(..)), Type::Primitive(Primitive::Int)) => Ok(()),
    (Expression::Literal(Literal::Int(..)), ty) => Err(TypeError::MismatchedTypes(
      Type::Primitive(Primitive::Int),
      ty.clone(),
    )),
    _ => unimplemented!(),
  }
}

pub fn check_block_expression(
  context: &mut Context,
  block_expression: &BlockExpression,
  ty: &Type,
) -> Result<(), TypeError> {
  for statement in block_expression.statements.iter() {
    check_statement(context, &statement)?;
  }
  if let Some(expression) = &block_expression.return_expression {
    check_expression(context, &expression, ty)
  } else {
    Ok(())
  }
}

pub fn check_statement(context: &mut Context, statement: &Statement) -> Result<(), TypeError> {
  match statement {
    Statement::Expression(expression) => {
      let ty = context.ty_supply.fresh();
      check_expression(context, expression, &ty)
    }
    Statement::Let(let_statement) => unimplemented!(),
  }
}

#[derive(Debug, Default)]
pub struct TypeSupply(usize);

impl TypeSupply {
  pub fn fresh(&mut self) -> Type {
    let unknown = self.0;
    self.0 += 1;
    Type::Unknown(self.0)
  }
}

#[cfg(test)]
mod tests {
  use internment::Intern;

  use crate::{
    ast::{BlockExpression, Declaration, Expression, FunctionDeclaration, Module},
    check,
    ty::{self, Type},
  };

  #[test]
  fn type_check() {
    let modules = vec![Module {
      declarations: vec![Declaration::Function(FunctionDeclaration {
        identifier: Intern::new("main".into()),
        parameters: vec![],
        body: BlockExpression {
          statements: vec![],
          return_expression: Some(Box::new(Expression::Literal(32.into()))),
        },
        return_ty: Type::Primitive(ty::Primitive::Float),
      })],
    }];

    for module in modules.iter() {
      check(&module).expect("type checking failed");
    }
  }
}
