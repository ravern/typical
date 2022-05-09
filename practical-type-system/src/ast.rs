use internment::Intern;

#[derive(Debug)]
pub enum Statement {
  Let(LetStatement),
  Expression(Expression),
}

#[derive(Debug)]
pub struct LetStatement {
  pattern: Pattern,
  expression: Expression,
}

#[derive(Debug)]
pub enum Pattern {}

#[derive(Debug)]
pub enum Expression {
  Block(BlockExpression),
  If(IfExpression),
  Literal(Literal),
  Identifier(Intern<String>),
}

#[derive(Debug)]
pub struct BlockExpression {
  statements: Vec<Statement>,
  return_expression: Option<Box<Expression>>,
}

#[derive(Debug)]
pub struct IfExpression {
  condition: Box<Expression>,
  then_clause: Box<Expression>,
  else_clause: Box<Expression>,
}

#[derive(Debug)]
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
