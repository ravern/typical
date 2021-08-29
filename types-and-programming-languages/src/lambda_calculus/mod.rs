#[derive(Clone, Debug)]
enum Term {
  Var(isize),
  Abs(Box<Term>),
  App(Box<Term>, Box<Term>),
}

#[derive(Clone, Debug)]
struct Info;

#[derive(Debug)]
struct Context(Vec<(String, Binding)>);

#[derive(Debug)]
struct Binding;

fn term_shift_walk(d: isize, c: isize, term: Term) -> Term {
  match term {
    Term::Var(x) if x >= c => Term::Var(x + d),
    Term::Var(x) => Term::Var(x),
    Term::Abs(term) => Term::Abs(Box::new(term_shift_walk(d, c + 1, *term))),
    Term::App(abs_term, arg_term) => Term::App(
      Box::new(term_shift_walk(d, c, *abs_term)),
      Box::new(term_shift_walk(d, c, *arg_term)),
    ),
  }
}

fn term_shift(d: isize, term: Term) -> Term {
  term_shift_walk(d, 0, term)
}

fn term_substitute_walk(j: isize, c: isize, sub_term: Term, term: Term) -> Term {
  match term {
    Term::Var(x) if x == j + c => term_shift(c, sub_term),
    Term::Var(x) => Term::Var(x),
    Term::Abs(term) => Term::Abs(Box::new(term_substitute_walk(j, c + 1, sub_term, *term))),
    Term::App(abs_term, arg_term) => Term::App(
      Box::new(term_substitute_walk(j, c, sub_term.clone(), *abs_term)),
      Box::new(term_substitute_walk(j, c, sub_term.clone(), *arg_term)),
    ),
  }
}

fn term_substitute(j: isize, sub_term: Term, term: Term) -> Term {
  term_substitute_walk(j, 0, sub_term, term)
}

fn term_substitute_top(sub_term: Term, term: Term) -> Term {
  dbg!(term_shift(1, sub_term.clone()));
  term_shift(-1, term_substitute(0, term_shift(1, sub_term), term))
}

fn is_val(term: &Term) -> bool {
  if let Term::Abs(_) = term {
    true
  } else {
    false
  }
}

fn step(ctx: &mut Context, term: Term) -> Result<Term, ()> {
  let result = match term {
    Term::App(abs_term, arg_term) => match *abs_term {
      Term::Abs(term) if is_val(&arg_term) => term_substitute_top(*arg_term, *term),
      abs_term if is_val(&abs_term) => {
        Term::App(Box::new(abs_term), Box::new(step(ctx, *arg_term)?))
      }
      abs_term => Term::App(Box::new(step(ctx, abs_term)?), arg_term),
    },
    _ => return Err(()),
  };

  Ok(result)
}

fn eval(term: Term) -> Term {
  let mut ctx = Context(Vec::new());
  let mut current = term;
  loop {
    current = match step(&mut ctx, current.clone()) {
      Ok(current) => current,
      Err(_) => break,
    };
  }
  current
}

pub fn run() {
  dbg!(eval(Term::App(
    Box::new(Term::Abs(Box::new(Term::Abs(Box::new(Term::Var(1)))))), // \. \. 0
    Box::new(Term::Abs(Box::new(Term::Var(2)))),                      // \. 0
  ))); // (\. \. 0) (\. 0)
}
