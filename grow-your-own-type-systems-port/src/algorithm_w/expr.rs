use std::{
  cell::RefCell,
  fmt,
  rc::Rc,
  sync::atomic::{AtomicUsize, Ordering},
};

static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

fn next_id() -> usize {
  NEXT_ID.fetch_add(1, Ordering::SeqCst)
}

pub enum Root {
  Expr(ExprRef),
  TyAnn(String, TyRef),
}

pub type ExprRef = Rc<Expr>;

#[derive(Debug)]
pub enum Expr {
  Var(String),
  Call(ExprRef, ExprRef),
  Fun(String, ExprRef),
  Let(String, ExprRef, ExprRef),
}

impl fmt::Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expr::Var(name) => write!(f, "{}", name),
      Expr::Call(fun, arg) => write!(f, "({} {})", fun, arg),
      Expr::Fun(param, body) => write!(f, "(\\{} -> {})", param, body),
      Expr::Let(name, value, body) => write!(f, "(let {} = {} in {})", name, value, body),
    }
  }
}

pub type TyRef = Rc<Ty>;

#[derive(Debug)]
pub enum Ty {
  Const(String),
  App(TyRef, TyRef),
  Arrow(TyRef, TyRef),
  Var(RefCell<TyVar>),
}

impl fmt::Display for Ty {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Ty::Const(name) => write!(f, "{}", name),
      Ty::App(ty1, ty2) => write!(f, "({} {})", ty1, ty2),
      Ty::Arrow(ty1, ty2) => write!(f, "({} -> {})", ty1, ty2),
      Ty::Var(ty_var) => write!(f, "{}", ty_var.borrow()),
    }
  }
}

#[derive(Debug)]
pub enum TyVar {
  Unbound(usize, usize),
  Link(TyRef),
  Generic(usize),
}

impl TyVar {
  pub fn unbound(level: usize) -> TyVar {
    TyVar::Unbound(next_id(), level)
  }

  pub fn generic() -> TyVar {
    TyVar::Generic(next_id())
  }
}

impl fmt::Display for TyVar {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TyVar::Unbound(id, level) => write!(f, "'{}({})", id, level),
      TyVar::Link(ty) => write!(f, "{}", ty),
      TyVar::Generic(id) => write!(f, "'{}", id),
    }
  }
}
