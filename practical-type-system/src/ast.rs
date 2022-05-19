use internment::Intern;

use crate::ty::Type;

#[derive(Clone, Debug)]
pub struct Module {
  pub declarations: Vec<Declaration>,
}

#[derive(Clone, Debug)]
pub enum Declaration {
  Struct(StructDeclaration),
  Function(FunctionDeclaration),
}

#[derive(Clone, Debug)]
pub struct StructDeclaration {
  pub identifier: Intern<String>,
  pub fields: Vec<StructField>,
}

#[derive(Clone, Debug)]
pub struct StructField {
  pub identifier: Intern<String>,
  pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct FunctionDeclaration {
  pub identifier: Intern<String>,
  pub parameters: Vec<FunctionParameter>,
  pub body: BlockExpression,
  pub return_ty: Type,
}

#[derive(Clone, Debug)]
pub struct FunctionParameter {
  pub identifier: Intern<String>,
  pub ty: Type,
}

#[derive(Clone, Debug)]
pub enum Statement {
  Let(LetStatement),
  Expression(Expression),
}

#[derive(Clone, Debug)]
pub struct LetStatement {
  pub pattern: Pattern,
  pub ty: Option<Type>,
  pub expression: Expression,
}

#[derive(Clone, Debug)]
pub enum Pattern {
  Identifier(Intern<String>),
}

#[derive(Clone, Debug)]
pub enum Expression {
  Block(BlockExpression),
  If(IfExpression),
  Call(CallExpression),
  BinaryOperation(BinaryOperation),
  UnaryOperation(UnaryOperation),
  Literal(Literal),
  Identifier(Intern<String>),
}

#[derive(Clone, Debug)]
pub struct BlockExpression {
  pub statements: Vec<Statement>,
  pub return_expression: Option<Box<Expression>>,
}

#[derive(Clone, Debug)]
pub struct IfExpression {
  pub condition: Box<Expression>,
  pub then_clause: Box<Expression>,
  pub else_clause: Box<Expression>,
}

#[derive(Clone, Debug)]
pub struct CallExpression {
  pub callee: Box<Expression>,
  pub arguments: Vec<Expression>,
}

#[derive(Clone, Debug)]
pub struct BinaryOperation {
  pub operator: BinaryOperator,
  pub left_operand: Box<Expression>,
  pub right_operand: Box<Expression>,
}

#[derive(Clone, Debug)]
pub enum BinaryOperator {
  Add,
  Subtract,
  Multiply,
  Divide,
  Modulo,
  And,
  Or,
  Equals,
  NotEquals,
}

#[derive(Clone, Debug)]
pub struct UnaryOperation {
  pub operator: UnaryOperator,
  pub operand: Box<Expression>,
}

#[derive(Clone, Debug)]
pub enum UnaryOperator {
  Negate,
  Not,
}

#[derive(Clone, Debug)]
pub enum Literal {
  Int(isize),
  Float(f64),
  Bool(bool),
  String(Intern<String>),
}

impl From<isize> for Literal {
  fn from(int: isize) -> Self {
    Literal::Int(int)
  }
}

impl From<f64> for Literal {
  fn from(float: f64) -> Self {
    Literal::Float(float)
  }
}

impl From<bool> for Literal {
  fn from(bool: bool) -> Self {
    Literal::Bool(bool)
  }
}

impl From<String> for Literal {
  fn from(string: String) -> Self {
    Literal::String(Intern::new(string))
  }
}
