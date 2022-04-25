enum Type {
  Fun {
    parameters: Vec<Self>,
    body: Box<Self>,
  },
  Prim(PrimType),
  Var {
    id: usize,
  },
}

enum PrimType {
  Int,
  Bool,
}

fn main() {
  println!("Hello, world!");
}
