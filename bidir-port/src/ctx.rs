use crate::ty::Type;

#[derive(Clone, Debug, Default)]
pub struct Ctx {
  elems: Vec<CtxElem>,
}

impl Ctx {
  pub fn add(&self, elem: CtxElem) -> Self {
    let mut elems = self.elems.clone();
    elems.push(elem);
    Ctx { elems }
  }

  pub fn split_at(&self, elem: CtxElem) -> Option<(Self, Self)> {
    if let Some(index) = self.elems.iter().position(|e| e == &elem) {
      let (left, right) = self.elems.split_at(index);
      Some((
        Ctx {
          elems: left.to_vec(),
        },
        Ctx {
          elems: right.to_vec(),
        },
      ))
    } else {
      None
    }
  }

  pub fn get_solved(&self, alpha: &str) -> Option<&Type> {
    self.elems.iter().find_map(|elem| {
      if let CtxElem::Solved(a, tau) = elem {
        if a == alpha {
          return Some(tau);
        }
      }
      None
    })
  }
}

#[derive(Clone, Debug, PartialEq)]
pub enum CtxElem {
  Var(String),
  Exist(String),
  Solved(String, Type),
  Marker(String),
  TypedVar(String, Type),
}
