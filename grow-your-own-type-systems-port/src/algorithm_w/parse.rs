use std::{collections::HashMap, rc::Rc};

use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

use super::{
  error::Error,
  expr::{Expr, ExprRef, Root, Ty, TyRef, TyVar},
};

#[derive(Parser)]
#[grammar = "src/algorithm_w/grammar.pest"]
struct ExprParser;

pub fn parse(input: &str) -> Result<Root, Error> {
  let mut pairs = ExprParser::parse(Rule::root, input).map_err(Error::Parse)?;
  parse_root(pairs.next().unwrap())
}

fn parse_root(pair: Pair<Rule>) -> Result<Root, Error> {
  let pair = pair.into_inner().next().unwrap();
  match pair.as_rule() {
    Rule::expr => Ok(Root::Expr(parse_expr(pair)?)),
    Rule::ty_ann => {
      let (name, ty) = parse_ty_ann(pair)?;
      Ok(Root::TyAnn(name, ty))
    }
    _ => unreachable!(),
  }
}

fn parse_expr(pair: Pair<Rule>) -> Result<ExprRef, Error> {
  let mut pairs = pair.into_inner();
  let mut expr = parse_term(pairs.next().unwrap())?;
  for pair in pairs {
    expr = Expr::Call(expr, parse_term(pair)?).into();
  }
  Ok(expr)
}

fn parse_term(pair: Pair<Rule>) -> Result<ExprRef, Error> {
  let pair = pair.into_inner().next().unwrap();
  match pair.as_rule() {
    Rule::name => Ok(Expr::Var(pair.as_str().to_string()).into()),
    Rule::fun => parse_fun(pair),
    Rule::let_ => parse_let(pair),
    Rule::group => parse_expr(pair.into_inner().next().unwrap()),
    _ => unreachable!(),
  }
}

fn parse_fun(pair: Pair<Rule>) -> Result<ExprRef, Error> {
  let mut pairs = pair.into_inner();
  let param = pairs.next().unwrap().as_str().to_string();
  let body = parse_expr(pairs.next().unwrap())?;
  Ok(Expr::Fun(param, body).into())
}

fn parse_let(pair: Pair<Rule>) -> Result<ExprRef, Error> {
  let mut pairs = pair.into_inner();
  let name = pairs.next().unwrap().as_str().to_string();
  let value = parse_term(pairs.next().unwrap())?;
  let body = parse_expr(pairs.next().unwrap())?;
  Ok(Expr::Let(name, value, body).into())
}

fn parse_ty_ann(pair: Pair<Rule>) -> Result<(String, TyRef), Error> {
  let mut pairs = pair.into_inner();
  let name = pairs.next().unwrap().as_str().to_string();
  let name_ty_map = HashMap::new();
  let ty = parse_ty(&name_ty_map, pairs.next().unwrap())?;
  Ok((name, ty).into())
}

fn parse_ty(name_ty_map: &HashMap<String, TyRef>, pair: Pair<Rule>) -> Result<TyRef, Error> {
  let mut pairs = pair.into_inner().peekable();
  let forall = if let Some(Rule::forall) = pairs.peek().map(Pair::as_rule) {
    Some(parse_forall(pairs.next().unwrap())?)
  } else {
    None
  };

  let mut child_name_ty_map = HashMap::new();
  child_name_ty_map.extend(name_ty_map.clone().into_iter());
  if let Some(name_ty_map) = &forall {
    child_name_ty_map.extend(name_ty_map.clone().into_iter());
  }

  let mut ty = parse_ty_term(&child_name_ty_map, pairs.next().unwrap())?;
  while let Some(pair) = pairs.next() {
    match pair.as_rule() {
      Rule::ty_term => {
        ty = Ty::App(ty, parse_ty_term(&child_name_ty_map, pair)?).into();
      }
      Rule::arrow => {
        ty = Ty::Arrow(
          ty,
          parse_ty_term(&child_name_ty_map, pairs.next().unwrap())?,
        )
        .into();
      }
      _ => unreachable!(),
    }
  }

  Ok(ty)
}

fn parse_forall(pair: Pair<Rule>) -> Result<HashMap<String, TyRef>, Error> {
  let mut name_ty_map = HashMap::new();
  for pair in pair.into_inner() {
    name_ty_map.insert(
      pair.as_str().to_string(),
      Ty::Var(TyVar::generic().into()).into(),
    );
  }
  Ok(name_ty_map)
}

fn parse_ty_term(name_ty_map: &HashMap<String, TyRef>, pair: Pair<Rule>) -> Result<TyRef, Error> {
  let pair = pair.into_inner().next().unwrap();
  match pair.as_rule() {
    Rule::name => Ok(if let Some(ty) = name_ty_map.get(pair.as_str()) {
      Rc::clone(ty)
    } else {
      Ty::Const(pair.as_str().to_string()).into()
    }),
    Rule::ty_group => parse_ty(name_ty_map, pair.into_inner().next().unwrap()),
    _ => unreachable!(),
  }
}
