use internment::Intern;

use crate::ast::{self, StructDeclaration};

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
  Struct(StructType),
  Function(FunctionType),
  Primitive(Primitive),
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructType {
  pub fields: Vec<StructField>,
}

impl From<StructDeclaration> for StructType {
  fn from(declaration: StructDeclaration) -> Self {
    Self {
      fields: declaration
        .fields
        .into_iter()
        .map(StructField::from)
        .collect(),
    }
  }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructField {
  pub identifier: Intern<String>,
  pub ty: Box<Type>,
}

impl From<ast::StructField> for StructField {
  fn from(field: ast::StructField) -> Self {
    Self {
      identifier: field.identifier,
      ty: Box::new(field.ty),
    }
  }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionType {
  pub parameters: Vec<Type>,
  pub return_ty: Box<Type>,
}

impl From<ast::FunctionDeclaration> for FunctionType {
  fn from(declaration: ast::FunctionDeclaration) -> Self {
    Self {
      parameters: declaration
        .parameters
        .into_iter()
        .map(|parameter| parameter.ty)
        .collect(),
      return_ty: Box::new(declaration.return_ty),
    }
  }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Primitive {
  Int,
  Float,
  Bool,
  String,
}
