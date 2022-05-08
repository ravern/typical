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

impl State {
  fn fresh_exist(&mut self) -> String {
    let exist = format!(
      "{}",
      char::from_u32(self.exist_count as u32 + 'a' as u32).unwrap()
    );
    self.exist_count += 1;
    exist
  }
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
    Expr::Abs(arg, body) => {
      let alpha = state.fresh_exist();
      let beta = state.fresh_exist();
      let alpha_ty = Type::Exist(alpha.clone());
      let beta_ty = Type::Exist(beta.clone());
      let ctx = ctx
        .add(CtxElem::Exist(alpha.clone()))
        .add(CtxElem::Exist(beta.clone()))
        .add(CtxElem::TypedVar(arg.clone(), alpha_ty.clone()));
      let ctx = checks_against(state, &ctx, body, &beta_ty)?
        .drop(CtxElem::TypedVar(arg.clone(), alpha_ty.clone()));
      Ok((Type::Fun(Box::new(alpha_ty), Box::new(beta_ty)), ctx))
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

fn occurs_in(var: &str, ty: &Type) -> bool {
  match ty {
    Type::Lit(..) => false,
    Type::Var(var) => var == var,
    Type::Fun(arg, body) => occurs_in(var, arg) || occurs_in(var, body),
    Type::Quant(alpha, ty) if var == alpha => true,
    Type::Quant(alpha, ty) => occurs_in(var, ty),
    Type::Exist(exist) => exist == var,
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
    (Type::Var(sub_var), Type::Var(var)) if sub_var == var => {
      if is_well_formed(ctx, sub_ty) {
        Ok(ctx.clone())
      } else {
        Err(TypeError::MismatchedTypes(
          apply_ctx(sub_ty.clone(), ctx),
          apply_ctx(ty.clone(), ctx),
        ))
      }
    }
    (Type::Exist(sub_exist), Type::Exist(exist)) if sub_exist == exist => {
      if is_well_formed(ctx, sub_ty) {
        Ok(ctx.clone())
      } else {
        Err(TypeError::MismatchedTypes(
          apply_ctx(sub_ty.clone(), ctx),
          apply_ctx(ty.clone(), ctx),
        ))
      }
    }
    (Type::Fun(sub_arg, sub_body), Type::Fun(arg, body)) => {
      let ctx = subtype(state, ctx, sub_arg, arg)?;
      subtype(
        state,
        &ctx,
        &apply_ctx(*sub_body.clone(), &ctx),
        &apply_ctx(*body.clone(), &ctx),
      )
    }
    (sub_ty, Type::Exist(exist)) => {
      if !occurs_in(exist, sub_ty) {
        instantiate_r(state, ctx, sub_ty, exist)
      } else {
        Err(TypeError::CyclicalType)
      }
    }
    (sub_ty, ty) => Err(TypeError::MismatchedTypes(sub_ty.clone(), ty.clone())),
  }
}

fn instantiate_r(state: &mut State, ctx: &Ctx, ty: &Type, exist: &str) -> Result<Ctx> {
  let (left_ctx, right_ctx) = ctx
    .split_at(CtxElem::Exist(exist.to_string()))
    .expect("existential not found in context");

  if ty.is_monotype() && is_well_formed(&left_ctx, ty) {
    return Ok(
      ctx
        .insert_in_place(
          CtxElem::Exist(exist.to_string()),
          vec![CtxElem::Solved(exist.to_string(), ty.clone())],
        )
        .expect("existential not found in context"),
    );
  }

  unimplemented!()
}
