use std::{collections::HashMap, fmt, rc::Rc};

use super::{
  error::Error,
  expr::{Expr, ExprRef, Ty, TyRef, TyVar},
};

pub type EnvRef = Rc<Env>;

pub enum Env {
  Empty,
  Extend(String, TyRef, EnvRef),
}

impl Env {
  pub fn empty() -> EnvRef {
    Env::Empty.into()
  }

  pub fn extend(name: String, ty: TyRef, env: EnvRef) -> EnvRef {
    Env::Extend(name, ty, env).into()
  }

  pub fn lookup(&self, name: &str) -> Option<TyRef> {
    match self {
      Env::Empty => None,
      Env::Extend(existing_name, ty, env) => {
        if name == *existing_name {
          Some(Rc::clone(&ty))
        } else {
          env.lookup(name)
        }
      }
    }
  }
}

impl fmt::Display for Env {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Env::Empty => Ok(()),
      Env::Extend(name, ty, env) => match &**env {
        Env::Empty => write!(f, "{}: {}", name, ty),
        _ => write!(f, "{}: {}\n{}", name, ty, env),
      },
    }
  }
}

pub fn infer(env: EnvRef, expr: ExprRef) -> Result<TyRef, Error> {
  infer_aux(env, 0, expr)
}

fn infer_aux(env: EnvRef, level: usize, expr: ExprRef) -> Result<TyRef, Error> {
  let ty = infer_aux_aux(env, level, Rc::clone(&expr))?;
  println!("inferred {} at level {} to be of type {}", expr, level, ty);
  Ok(ty)
}

fn infer_aux_aux(env: EnvRef, level: usize, expr: ExprRef) -> Result<TyRef, Error> {
  match &*expr {
    Expr::Var(name) => {
      let ty = env.lookup(&name).ok_or(Error::UnboundVar(name.clone()))?;
      Ok(instantiate(level, ty))
    }
    Expr::Fun(param, body) => {
      let param_ty = Ty::Var(TyVar::unbound(level).into()).into();
      let fun_env = Env::extend(param.clone(), Rc::clone(&param_ty), env);
      let body_ty = infer_aux(fun_env, level, Rc::clone(body))?;
      Ok(Ty::Arrow(param_ty, body_ty).into())
    }
    Expr::Let(name, value, body) => {
      let var_ty = infer_aux(Rc::clone(&env), level + 1, Rc::clone(value))?;
      let gen_var_ty = generalize(level, var_ty);
      let let_env = Env::extend(name.clone(), Rc::clone(&gen_var_ty), env);
      infer_aux(let_env, level, Rc::clone(body))
    }
    Expr::Call(fun, arg) => {
      let fun_ty = infer_aux(Rc::clone(&env), level, Rc::clone(fun))?;
      let (param_ty, ret_ty) = match_fun_ty(fun_ty)?;
      let arg_ty = infer_aux(Rc::clone(&env), level, Rc::clone(arg))?;
      unify(param_ty, arg_ty)?;
      Ok(ret_ty)
    }
  }
}

fn instantiate(level: usize, ty: TyRef) -> TyRef {
  let mut id_ty_map = HashMap::new();
  instantiate_aux(level, ty, &mut id_ty_map)
}

fn instantiate_aux(level: usize, ty: TyRef, id_ty_map: &mut HashMap<usize, TyRef>) -> TyRef {
  match &*ty {
    Ty::Const(_) => ty,
    Ty::Var(ty_var) => match &*ty_var.borrow() {
      TyVar::Link(ty) => instantiate_aux(level, Rc::clone(ty), id_ty_map),
      TyVar::Generic(id) => {
        if let Some(ty) = id_ty_map.get(&id) {
          Rc::clone(&ty)
        } else {
          let new_ty = Ty::Var(TyVar::unbound(level).into()).into();
          id_ty_map.insert(*id, Rc::clone(&new_ty));
          new_ty
        }
      }
      TyVar::Unbound(_id, _level) => Rc::clone(&ty),
    },
    Ty::App(ty_fun, ty_arg) => Ty::App(
      instantiate_aux(level, Rc::clone(ty_fun), id_ty_map),
      instantiate_aux(level, Rc::clone(ty_arg), id_ty_map),
    )
    .into(),
    Ty::Arrow(param_ty, ret_ty) => Ty::Arrow(
      instantiate_aux(level, Rc::clone(param_ty), id_ty_map),
      instantiate_aux(level, Rc::clone(ret_ty), id_ty_map),
    )
    .into(),
  }
}

fn generalize(level: usize, ty: TyRef) -> TyRef {
  match &*ty {
    Ty::Const(_) => ty,
    Ty::Var(ty_var) => match &*ty_var.borrow() {
      TyVar::Link(ty) => generalize(level, Rc::clone(ty)),
      TyVar::Generic(_id) => Rc::clone(&ty),
      TyVar::Unbound(other_id, other_level) => {
        if *other_level > level {
          Ty::Var(TyVar::Generic(*other_id).into()).into()
        } else {
          Rc::clone(&ty)
        }
      }
    },
    Ty::App(ty_fun, ty_arg) => Ty::App(
      generalize(level, Rc::clone(ty_fun)),
      generalize(level, Rc::clone(ty_arg)),
    )
    .into(),
    Ty::Arrow(param_ty, ret_ty) => Ty::Arrow(
      generalize(level, Rc::clone(param_ty)),
      generalize(level, Rc::clone(ret_ty)),
    )
    .into(),
  }
}

fn match_fun_ty(ty: TyRef) -> Result<(TyRef, TyRef), Error> {
  match &*ty {
    Ty::Arrow(param_ty, ret_ty) => Ok((Rc::clone(&param_ty), Rc::clone(&ret_ty))),
    Ty::Var(ty_var) => {
      let ty_var_ref = ty_var.borrow();
      match &*ty_var_ref {
        TyVar::Link(ty) => match_fun_ty(Rc::clone(ty)),
        TyVar::Unbound(_id, level) => {
          let param_ty = Ty::Var(TyVar::unbound(*level).into()).into();
          let ret_ty = Ty::Var(TyVar::unbound(*level).into()).into();
          let fun_ty = Ty::Arrow(Rc::clone(&param_ty), Rc::clone(&ret_ty)).into();
          drop(ty_var_ref);
          *ty_var.borrow_mut() = TyVar::Link(fun_ty);
          Ok((param_ty, ret_ty))
        }
        _ => Err(Error::NotAFunction),
      }
    }
    _ => todo!(),
  }
}

fn unify(ty1: TyRef, ty2: TyRef) -> Result<(), Error> {
  println!("unifying {} and {}", ty1, ty2);

  if Rc::ptr_eq(&ty1, &ty2) {
    return Ok(());
  }

  match (&*ty1, &*ty2) {
    (Ty::Const(name1), Ty::Const(name2)) if name1 == name2 => Ok(()),
    (Ty::App(ty_fun1, ty_arg1), Ty::App(ty_fun2, ty_arg2)) => {
      unify(Rc::clone(&ty_fun1), Rc::clone(&ty_fun2))?;
      unify(Rc::clone(&ty_arg1), Rc::clone(&ty_arg2))?;
      Ok(())
    }
    (Ty::Arrow(param_ty1, ret_ty1), Ty::Arrow(param_ty2, ret_ty2)) => {
      unify(Rc::clone(&param_ty1), Rc::clone(&param_ty2))?;
      unify(Rc::clone(&ret_ty1), Rc::clone(&ret_ty2))?;
      Ok(())
    }
    (Ty::Var(ty_var1), Ty::Var(ty_var2)) => {
      let ty_var_ref1 = ty_var1.borrow();
      let ty_var_ref2 = ty_var2.borrow();
      match (&*ty_var_ref1, &*ty_var_ref2) {
        (TyVar::Link(ty1), _ty_var2) => unify(Rc::clone(ty1), Rc::clone(&ty2)),
        (_ty_var_1, TyVar::Link(ty2)) => unify(Rc::clone(&ty1), Rc::clone(ty2)),
        (TyVar::Unbound(id1, _level1), TyVar::Unbound(id2, _level2)) if id1 == id2 => {
          unreachable!()
        }
        (TyVar::Unbound(id1, level1), _ty_var2) => {
          occurs_check_adjust_levels(*id1, *level1, Rc::clone(&ty2))?;
          drop(ty_var_ref2);
          *ty_var2.borrow_mut() = TyVar::Link(Rc::clone(&ty1));
          Ok(())
        }
        (_ty_var1, TyVar::Unbound(id2, level2)) => {
          occurs_check_adjust_levels(*id2, *level2, Rc::clone(&ty1))?;
          drop(ty_var_ref1);
          *ty_var1.borrow_mut() = TyVar::Link(Rc::clone(&ty1));
          Ok(())
        }
        _ => Err(Error::CannotUnify(Rc::clone(&ty1), Rc::clone(&ty2))),
      }
    }
    (Ty::Var(ty_var1), _ty2) => {
      let ty_var_ref1 = ty_var1.borrow();
      match &*ty_var_ref1 {
        TyVar::Link(ty1) => unify(Rc::clone(ty1), Rc::clone(&ty2)),
        TyVar::Unbound(id1, level1) => {
          occurs_check_adjust_levels(*id1, *level1, Rc::clone(&ty2))?;
          drop(ty_var_ref1);
          *ty_var1.borrow_mut() = TyVar::Link(Rc::clone(&ty2));
          Ok(())
        }
        _ => Err(Error::CannotUnify(Rc::clone(&ty1), Rc::clone(&ty2))),
      }
    }
    (_ty1, Ty::Var(ty_var2)) => {
      let ty_var_ref2 = ty_var2.borrow();
      match &*ty_var_ref2 {
        TyVar::Link(ty2) => unify(Rc::clone(&ty1), Rc::clone(ty2)),
        TyVar::Unbound(id2, level2) => {
          occurs_check_adjust_levels(*id2, *level2, Rc::clone(&ty1))?;
          drop(ty_var_ref2);
          *ty_var2.borrow_mut() = TyVar::Link(Rc::clone(&ty1));
          Ok(())
        }
        _ => Err(Error::CannotUnify(Rc::clone(&ty1), Rc::clone(&ty2))),
      }
    }
    _ => Err(Error::CannotUnify(Rc::clone(&ty1), Rc::clone(&ty2))),
  }
}

fn occurs_check_adjust_levels(id: usize, level: usize, ty: TyRef) -> Result<(), Error> {
  match &*ty {
    Ty::Const(_name) => Ok(()),
    Ty::Var(ty_var) => {
      let ty_var_ref = ty_var.borrow();
      match &*ty_var_ref {
        TyVar::Link(ty) => occurs_check_adjust_levels(id, level, Rc::clone(ty)),
        TyVar::Generic(_) => Ok(()),
        TyVar::Unbound(other_id, other_level) => {
          if id == *other_id {
            Err(Error::InfiniteType)
          } else {
            if level < *other_level {
              let other_id = *other_id;
              drop(ty_var_ref);
              *ty_var.borrow_mut() = TyVar::Unbound(other_id, level);
            }
            Ok(())
          }
        }
      }
    }
    Ty::App(ty_fun, ty_arg) => {
      occurs_check_adjust_levels(id, level, Rc::clone(ty_fun))?;
      occurs_check_adjust_levels(id, level, Rc::clone(ty_arg))
    }
    Ty::Arrow(param_ty, ret_ty) => {
      occurs_check_adjust_levels(id, level, Rc::clone(param_ty))?;
      occurs_check_adjust_levels(id, level, Rc::clone(ret_ty))
    }
  }
}
