use bidir_port::{
  ast::{Expr, Lit},
  synth,
};

fn main() {
  match synth(&Expr::Lit(Lit::Int(32))) {
    Ok(ty) => println!("{}", ty),
    Err(error) => println!("Error: {:?}", error),
  }
}
