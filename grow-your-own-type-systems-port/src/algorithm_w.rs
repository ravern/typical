use std::fmt;

enum Expr {
  Int(usize),
  Var(Name),
  Call(Box<Expr>, Vec<Expr>),
  Fun(Vec<Name>, Box<Expr>),
  Let(Name, Box<Expr>, Box<Expr>),
}

impl fmt::Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Expr::Int(int) => write!(f, "{}", int),
      Expr::Var(name) => write!(f, "{}", name),
      Expr::Call(fun, args) => {
        write!(f, "({}", fun)?;
        for arg in args {
          write!(f, " {}", arg)?;
        }
        write!(f, ")")
      }
      Expr::Fun(params, body) => {
        write!(f, "(fun (")?;
        for (index, param) in params.into_iter().enumerate() {
          if index != 0 {
            write!(f, " ")?;
          }
          write!(f, "{}", param)?;
        }
        write!(f, ") {})", body)
      }
      Expr::Let(name, expr, body) => write!(f, "(let {} = {} in {})", name, expr, body),
    }
  }
}

type Name = String;

#[derive(Clone)]
enum Ty {
  Const(Name),
  Apply(Box<Ty>, Vec<Ty>),
  Arrow(Vec<Ty>, Box<Ty>),
  Var(TyVar),
}

impl fmt::Display for Ty {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Ty::Const(name) => write!(f, "{}", name),
      Ty::Apply(ty, args) => {
        write!(f, "({}", ty)?;
        for arg in args {
          write!(f, " {}", arg)?;
        }
        write!(f, ")")
      }
      Ty::Arrow(from, to) => {
        write!(f, "((")?;
        for (index, ty) in from.into_iter().enumerate() {
          if index != 0 {
            write!(f, " ")?;
          }
          write!(f, "{}", ty)?;
        }
        write!(f, ") -> {})", to)
      }
      Ty::Var(ty_var) => write!(f, "{}", ty_var),
    }
  }
}

#[derive(Clone)]
enum TyVar {
  Unbound(Id, Level),
  Link(Box<Ty>),
  Generic(Id),
}

impl fmt::Display for TyVar {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      TyVar::Unbound(id, level) => write!(f, "{}({})", id, level),
      TyVar::Link(ty) => write!(f, "{}", ty),
      TyVar::Generic(id) => write!(f, "{}", id),
    }
  }
}

type Id = usize;
type Level = usize;

enum Env<'a> {
  Empty,
  Extend(&'a mut Env<'a>, Name, Ty),
}

impl<'a> Env<'a> {
  fn empty() -> Self {
    Env::Empty
  }

  fn extend(&'a mut self, name: Name, ty: Ty) -> Env<'a> {
    Env::Extend(self, name, ty)
  }

  fn lookup(&self, name: &Name) -> Option<&Ty> {
    match self {
      Env::Empty => None,
      Env::Extend(env, other_name, ty) => {
        if name == other_name {
          Some(ty)
        } else {
          env.lookup(name)
        }
      }
    }
  }
}

fn occurs_check_adjust_levels(id: Id, level: Level, ty: &mut Ty) -> Result<(), String> {
  match ty {
    Ty::Var(TyVar::Link(ty)) => occurs_check_adjust_levels(id, level, ty),
    Ty::Var(TyVar::Generic(_)) => unreachable!(),
    Ty::Var(TyVar::Unbound(other_id, other_level)) => {
      if *other_id == id {
        Err("recursive types".into())
      } else if *other_level > level {
        *other_level = level;
        Ok(())
      } else {
        Ok(())
      }
    }
    Ty::Apply(ty, args) => {
      occurs_check_adjust_levels(id, level, ty)?;
      for arg in args {
        occurs_check_adjust_levels(id, level, arg)?;
      }
      Ok(())
    }
    Ty::Arrow(from_ty, to_ty) => {
      for ty in from_ty {
        occurs_check_adjust_levels(id, level, ty)?;
      }
      occurs_check_adjust_levels(id, level, to_ty)
    }
    Ty::Const(_) => Ok(()),
  }
}

fn unify(ty1: &Ty, ty2: &Ty) {}

fn generalize(level: Level, ty: Ty) -> Ty {
  ty
}

fn instantiate(level: Level, ty: Ty) -> Ty {
  ty
}

fn match_fun_ty(arity: usize, ty: Ty) -> Result<(Vec<Ty>, Ty), String> {
  match ty {
    Ty::Arrow(params, result) => Ok((params, *result)),
    _ => Err("arrow type expected".into()),
  }
}

fn infer(env: &mut Env, level: Level, expr: &Expr) -> Result<Ty, String> {
  match expr {
    Expr::Int(_) => Ok(Ty::Const("Int".into())),
    Expr::Var(name) => env
      .lookup(name)
      .cloned()
      .ok_or_else(|| format!("unbound variable '{}'", name)),
    Expr::Fun(params, body) => {
      let mut env = env;
      for param in params {
        env = Env::Extend(
          Box::new(env),
          param.clone(),
          Ty::Var(dsl::unbound(0, level + 1)),
        );
      }
      let result = infer(&mut env, level + 1, body)?;
      Ok(Ty::Arrow(
        params
          .iter()
          .map(|_| Ty::Var(dsl::unbound(0, level)))
          .collect(),
        Box::new(result),
      ))
    }
  }
}

mod dsl {
  use super::*;

  pub fn int(int: usize) -> Expr {
    Expr::Int(int)
  }

  pub fn var<N>(name: N) -> Expr
  where
    N: Into<Name>,
  {
    Expr::Var(name.into())
  }

  pub fn call<A>(fun: Expr, args: A) -> Expr
  where
    A: Into<Vec<Expr>>,
  {
    Expr::Call(fun.into(), args.into())
  }

  pub fn fun<P, N>(params: P, body: Expr) -> Expr
  where
    P: Into<Vec<N>>,
    N: Into<Name>,
  {
    Expr::Fun(
      params.into().into_iter().map(|name| name.into()).collect(),
      body.into(),
    )
  }

  pub fn let_<N>(name: N, expr: Expr, body: Expr) -> Expr
  where
    N: Into<Name>,
  {
    Expr::Let(name.into(), expr.into(), body.into())
  }

  pub fn const_<N>(name: N) -> Ty
  where
    N: Into<Name>,
  {
    Ty::Const(name.into())
  }

  pub fn apply<T, A>(ty: T, args: A) -> Ty
  where
    T: Into<Box<Ty>>,
    A: Into<Vec<Ty>>,
  {
    Ty::Apply(ty.into(), args.into())
  }

  pub fn arrow<P>(params: P, result: Ty) -> Ty
  where
    P: Into<Vec<Ty>>,
  {
    Ty::Arrow(params.into(), result.into())
  }

  pub fn unbound(id: Id, level: Level) -> TyVar {
    TyVar::Unbound(id, level)
  }

  pub fn link(ty: Ty) -> TyVar {
    TyVar::Link(Box::new(ty))
  }

  pub fn generic(id: Id) -> TyVar {
    TyVar::Generic(id)
  }
}

#[cfg(test)]
mod tests {
  use super::dsl::*;

  #[test]
  fn expr_display_int() {
    let expr = int(42);
    assert_eq!(format!("{}", expr), "42");
  }

  #[test]
  fn expr_display_var() {
    let expr = var("x");
    assert_eq!(format!("{}", expr), "x");
  }

  #[test]
  fn expr_display_call() {
    let expr = call(var("x"), [var("y"), var("z")]);
    assert_eq!(format!("{}", expr), "(x y z)");
  }

  #[test]
  fn expr_display_fun() {
    let expr = fun(["x", "y"], var("x"));
    assert_eq!(format!("{}", expr), "(fun (x y) x)");
  }

  #[test]
  fn expr_display_let() {
    let expr = let_("x", var("y"), var("x"));
    assert_eq!(format!("{}", expr), "(let x = y in x)");
  }

  #[test]
  fn expr_display_complex() {
    let expr = let_(
      "x",
      fun(["y", "z"], call(var("y"), [var("z")])),
      call(var("x"), [fun(["w"], var("w")), int(1)]),
    );
    assert_eq!(
      format!("{}", expr),
      "(let x = (fun (y z) (y z)) in (x (fun (w) w) 1))"
    );
  }

  #[test]
  fn ty_display_const() {
    let ty = const_("Int");
    assert_eq!(format!("{}", ty), "Int");
  }

  #[test]
  fn ty_display_apply() {
    let ty = apply(const_("List"), [const_("Int")]);
    assert_eq!(format!("{}", ty), "(List Int)");
  }

  #[test]
  fn ty_display_arrow() {
    let ty = arrow([const_("Int"), const_("Int")], const_("Bool"));
    assert_eq!(format!("{}", ty), "((Int Int) -> Bool)");
  }

  #[test]
  fn ty_display_complex() {
    let ty = arrow(
      [apply(const_("List"), [const_("Int")]), const_("Int")],
      apply(const_("List"), [const_("Int")]),
    );
    assert_eq!(format!("{}", ty), "(((List Int) Int) -> (List Int))");
  }

  #[test]
  fn ty_var_display_unbound() {
    let ty = unbound(0, 0);
    assert_eq!(format!("{}", ty), "0(0)");
  }

  #[test]
  fn ty_var_display_link() {
    let ty = link(const_("Int"));
    assert_eq!(format!("{}", ty), "Int");
  }

  #[test]
  fn ty_var_display_generic() {
    let ty = generic(0);
    assert_eq!(format!("{}", ty), "0");
  }
}
