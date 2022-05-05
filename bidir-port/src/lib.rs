use ast::{Expr, Lit};
use ctx::{Ctx, CtxElem};
use error::{Result, TypeError};
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

fn checks_against(state: &mut State, ctx: &Ctx, expr: &Expr, ty: &Type) -> Result<Ctx> {
  match (expr, ty) {
    (Expr::Lit(Lit::Unit), Type::Lit(ty::Lit::Unit)) => Ok(ctx.clone()),
    (Expr::Lit(Lit::Int(..)), Type::Lit(ty::Lit::Int)) => Ok(ctx.clone()),
    (Expr::Lit(Lit::Float(..)), Type::Lit(ty::Lit::Float)) => Ok(ctx.clone()),
    (Expr::Lit(Lit::Bool(..)), Type::Lit(ty::Lit::Bool)) => Ok(ctx.clone()),
    (Expr::Lit(Lit::String(..)), Type::Lit(ty::Lit::String)) => Ok(ctx.clone()),
    (expr, ty) => {
      let (actual_ty, ctx) = synthesises_to(state, ctx, expr)?;
      subtype(
        state,
        &ctx,
        &apply_ctx(actual_ty, &ctx),
        &apply_ctx(ty.clone(), &ctx),
      )
    }
    _ => unimplemented!(),
  }
}

fn synthesises_to(state: &mut State, ctx: &Ctx, expr: &Expr) -> Result<(Type, Ctx)> {
  match expr {
    Expr::Lit(Lit::Unit) => Ok((Type::Lit(ty::Lit::Unit), ctx.clone())),
    Expr::Lit(Lit::Int(..)) => Ok((Type::Lit(ty::Lit::Int), ctx.clone())),
    Expr::Lit(Lit::Float(..)) => Ok((Type::Lit(ty::Lit::Float), ctx.clone())),
    Expr::Lit(Lit::Bool(..)) => Ok((Type::Lit(ty::Lit::Bool), ctx.clone())),
    Expr::Lit(Lit::String(..)) => Ok((Type::Lit(ty::Lit::String), ctx.clone())),
    Expr::Var(var) => {
      if let Some(ty) = ctx.get_anno(var) {
        Ok((ty.clone(), ctx.clone()))
      } else {
        Err(TypeError::UndefinedVar(var.clone()))
      }
    }
    Expr::Anno(expr, ty) => {
      if is_well_formed(ctx, ty) {
        let ctx = checks_against(state, ctx, expr, ty)?;
        Ok((ty.clone(), ctx))
      } else {
        Err(TypeError::InvalidType(ty.clone()))
      }
    }
    _ => unimplemented!(),
  }
}

fn apply_ctx(ty: Type, ctx: &Ctx) -> Type {
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

fn is_well_formed(ctx: &Ctx, ty: &Type) -> bool {
  match ty {
    Type::Lit(..) => true,
    Type::Var(alpha) => ctx.has_var(alpha),
    Type::Fun(arg, body) => is_well_formed(ctx, arg) && is_well_formed(ctx, body),
    Type::Quant(alpha, ty) => is_well_formed(&ctx.add(CtxElem::Var(alpha.clone())), ty),
    Type::Exist(var) => ctx.has_exist(var) || ctx.get_solved(var).is_some(),
  }
}

fn subtype(state: &mut State, ctx: &Ctx, sub_ty: &Type, ty: &Type) -> Result<Ctx> {
  if !is_well_formed(ctx, sub_ty) {
    return Err(TypeError::InvalidType(sub_ty.clone()));
  }
  if !is_well_formed(ctx, ty) {
    return Err(TypeError::InvalidType(ty.clone()));
  }

  match (sub_ty, ty) {
    (Type::Lit(ty::Lit::Unit), Type::Lit(ty::Lit::Unit)) => Ok(ctx.clone()),
    (Type::Lit(ty::Lit::Int), Type::Lit(ty::Lit::Int)) => Ok(ctx.clone()),
    (Type::Lit(ty::Lit::Float), Type::Lit(ty::Lit::Float)) => Ok(ctx.clone()),
    (Type::Lit(ty::Lit::Bool), Type::Lit(ty::Lit::Bool)) => Ok(ctx.clone()),
    (Type::Lit(ty::Lit::String), Type::Lit(ty::Lit::String)) => Ok(ctx.clone()),
    (sub_ty, ty) => Err(TypeError::MismatchedTypes(sub_ty.clone(), ty.clone())),
  }
}
