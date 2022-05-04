use ast::{Expr, Lit};
use ctx::Ctx;
use error::Result;
use ty::Type;

pub mod ast;
pub mod ty;

mod ctx;
mod error;

#[derive(Debug, Default)]
struct State {
  exist_count: usize,
}

pub fn synth(expr: &Expr) -> Result<Type> {
  let (ty, ctx) = synthesises_to(&mut State::default(), &Ctx::default(), expr)?;
  let ty = apply_ctx(ty, &ctx);
  Ok(ty)
}

fn synthesises_to(state: &mut State, ctx: &Ctx, expr: &Expr) -> Result<(Type, Ctx)> {
  match expr {
    Expr::Lit(Lit::Unit) => Ok((Type::Lit(ty::Lit::Unit), ctx.clone())),
    Expr::Lit(Lit::Int(..)) => Ok((Type::Lit(ty::Lit::Int), ctx.clone())),
    Expr::Lit(Lit::Float(..)) => Ok((Type::Lit(ty::Lit::Float), ctx.clone())),
    Expr::Lit(Lit::Bool(..)) => Ok((Type::Lit(ty::Lit::Bool), ctx.clone())),
    Expr::Lit(Lit::String(..)) => Ok((Type::Lit(ty::Lit::String), ctx.clone())),
    _ => unimplemented!(),
  }
}

pub fn apply_ctx(ty: Type, ctx: &Ctx) -> Type {
  match ty {
    Type::Lit(..) => ty,
    Type::Var(..) => ty,
    Type::Exist(ref alpha) => {
      if let Some(tau) = ctx.get_solved(&alpha) {
        apply_ctx(tau.clone(), ctx)
      } else {
        ty
      }
    }
    Type::Fun(arg, body) => Type::Fun(
      Box::new(apply_ctx(*arg, ctx)),
      Box::new(apply_ctx(*body, ctx)),
    ),
    Type::Quant(alpha, ty) => Type::Quant(alpha, Box::new(apply_ctx(*ty, ctx))),
  }
}
