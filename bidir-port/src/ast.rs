use crate::ty::Type;

#[derive(Debug, PartialEq)]
pub enum Expr {
  Var(String),
  Lit(Lit),
  Abs(String, Box<Expr>),
  App(Box<Expr>, Box<Expr>),
  Anno(Box<Expr>, Type),
}

#[derive(Debug, PartialEq)]
pub enum Lit {
  Unit,
  Int(isize),
  Float(f64),
  Bool(bool),
  String(String),
}
