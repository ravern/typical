use bidir_port::{
  ast::{Expr, Lit},
  synth,
  ty::{self, Type},
};

fn main() {
  let expr = Expr::Anno(Box::new(Expr::Lit(Lit::Int(32))), Type::Lit(ty::Lit::Bool));
  match synth(&expr) {
    Ok(ty) => println!("{}", ty),
    Err(error) => println!("Error: {}", error),
  }
}
