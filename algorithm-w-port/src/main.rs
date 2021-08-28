use std::collections::{HashMap, HashSet};

enum Exp {
  Var(String),
  Lit(Lit),
  App(Box<Exp>, Box<Exp>),
  Abs(String, Box<Exp>),
  Let(String, Box<Exp>, Box<Exp>),
}

enum Lit {
  Int(i64),
  Bool(bool),
}

enum Type {
  Var(String),
  Int,
  Bool,
  Fun(Box<Type>, Box<Type>),
}

struct Scheme(Vec<String>, Type);

trait Types {
  fn free_type_variables(&self) -> HashSet<String>;

  fn apply_substitution(&self, substitution: Substitution) -> Self;
}

type Substitution = HashMap<String, Type>;

fn main() {
  println!("Hello, world!");
}
