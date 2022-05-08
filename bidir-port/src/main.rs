use bidir_port::{
  ast::{Expr, Lit},
  synth,
  ty::{self, Type},
};

fn main() {
  let expr = Expr::Abs("foo".to_string(), Box::new(Expr::Lit(Lit::Int(32))));
  match synth(&expr) {
    Ok(ty) => println!("{}", ty),
    Err(error) => println!("Error: {:?}", error),
  }
}
