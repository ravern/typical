use std::rc::Rc;

use rustyline::{error::ReadlineError, DefaultEditor};

use self::{
  error::Error,
  expr::Root,
  infer::{Env, EnvRef},
};

mod error;
mod expr;
mod infer;
mod parse;

pub fn run() {
  let mut env = Env::empty();
  let mut rl = DefaultEditor::new().unwrap();
  let _ = rl.load_history("~/.typical_history");
  loop {
    match rl.readline("> ") {
      Ok(line) => {
        let _ = rl.add_history_entry(line.as_str());
        match line.as_str().trim() {
          "quit" => break,
          "show-env" => println!("{}", env),
          line => match eval(&mut env, &line) {
            Ok(output) => println!("{}", output),
            Err(error) => println!("{}", error),
          },
        }
      }
      Err(error) => match error {
        ReadlineError::Interrupted => {
          println!("CTRL-C");
          continue;
        }
        ReadlineError::Eof => {
          println!("CTRL-D");
          break;
        }
        _ => {
          println!("failed to read line: {}", error);
        }
      },
    }
  }
}

pub fn eval(env: &mut EnvRef, input: &str) -> Result<String, Error> {
  let root = parse::parse(input)?;
  match root {
    Root::TyAnn(name, ty) => {
      *env = Env::extend(name, Rc::clone(&ty), Rc::clone(env));
      Ok(format!("{}", ty))
    }
    Root::Expr(expr) => {
      let ty = infer::infer(Rc::clone(&env), expr)?;
      Ok(format!("{}", ty))
    }
  }
}
