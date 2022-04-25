use std::fmt;

enum Exp {
  Lam {
    parameters: Vec<String>,
    body: Box<Self>,
  },
  Call {
    arguments: Vec<Self>,
    callee: Box<Self>,
  },
  If {
    condition: Box<Self>,
    then: Box<Self>,
    otherwise: Box<Self>,
  },
  Int(usize),
  Bool(bool),
  Ident(String),
}

#[derive(Clone)]
enum Type {
  Fun {
    parameters: Vec<Self>,
    body: Box<Self>,
  },
  Int,
  Bool,
  Var {
    id: usize,
  },
}

impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Fun { parameters, body } => {
        write!(f, "(")?;
        for (index, parameter) in parameters.iter().enumerate() {
          write!(f, "{}", parameter)?;
          if index != parameters.len() - 1 {
            write!(f, ", ")?;
          }
        }
        write!(f, ") -> ")?;
        write!(f, "{}", body)
      }
      Self::Int => write!(f, "Int"),
      Self::Bool => write!(f, "Bool"),
      Self::Var { id } => write!(f, "t{}", id),
    }
  }
}

struct TypeError {}

impl fmt::Display for TypeError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Type error")
  }
}

struct Ctx {
  supply: usize,
}

impl Ctx {
  pub fn new() -> Ctx {
    Ctx { supply: 0 }
  }

  pub fn fresh_type(&mut self) -> Type {
    let id = self.supply;
    self.supply += 1;
    Type::Var { id }
  }
}

fn typecheck(exp: Exp) -> Result<Type, TypeError> {
  infer(&mut Ctx::new(), exp)
}

fn infer(ctx: &mut Ctx, exp: Exp) -> Result<Type, TypeError> {
  match exp {
    Exp::Int(_) => Ok(Type::Int),
    Exp::Bool(_) => Ok(Type::Bool),
    Exp::If {
      condition,
      then,
      otherwise,
    } => {
      check(ctx, *condition, Type::Bool)?;
      let then_ty = infer(ctx, *then)?;
      check(ctx, *otherwise, then_ty.clone())?;
      Ok(then_ty)
    }
    _ => unimplemented!(),
  }
}

fn check(ctx: &mut Ctx, exp: Exp, ty: Type) -> Result<(), TypeError> {
  let expected_ty = infer(ctx, exp)?;
  unify(ctx, ty, expected_ty)
}

fn unify(ctx: &mut Ctx, ty: Type, expected_ty: Type) -> Result<(), TypeError> {
  Ok(())
}

fn main() {
  let exp = Exp::Bool(false);
  match typecheck(exp) {
    Ok(ty) => println!("{}", ty),
    Err(error) => println!("{}", error),
  }
}
