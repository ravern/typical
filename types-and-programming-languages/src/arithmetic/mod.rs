#[derive(Debug)]
enum Term {
  True(Info),
  False(Info),
  If(Info, Box<Term>, Box<Term>, Box<Term>),
  Zero(Info),
  Succ(Info, Box<Term>),
  Pred(Info, Box<Term>),
  IsZero(Info, Box<Term>),
}

#[derive(Debug)]
struct Info;

fn is_numeric_val(term: &Term) -> bool {
  match term {
    Term::Zero(_) => true,
    Term::Succ(_, term) => is_numeric_val(&*term),
    _ => false,
  }
}

fn is_val(term: &Term) -> bool {
  match term {
    Term::True(_) => true,
    Term::False(_) => true,
    term if is_numeric_val(term) => true,
    _ => false,
  }
}

fn step(term: Term) -> Result<Term, Term> {
  let result = match term {
    Term::If(info, condition, then_term, else_term) => match *condition {
      Term::True(_) => *then_term,
      Term::False(_) => *else_term,
      condition => Term::If(info, Box::new(step(condition)?), then_term, else_term),
    },

    Term::Succ(info, term) => Term::Succ(info, Box::new(step(*term)?)),

    Term::Pred(info, term) => match *term {
      Term::Zero(_) => Term::Zero(Info),
      Term::Succ(_, term) => *term,
      term => Term::Pred(info, Box::new(step(term)?)),
    },

    Term::IsZero(info, term) => match *term {
      Term::Zero(_) => Term::True(Info),
      Term::Succ(_, _) => Term::False(Info),
      term => Term::IsZero(info, Box::new(step(term)?)),
    },

    term => return Err(term),
  };

  Ok(result)
}

fn eval(term: Term) -> Term {
  let mut current = term;
  loop {
    match step(current) {
      Ok(term) => current = term,
      Err(term) => return term,
    }
  }
}

pub fn run() {
  dbg!(eval(Term::IsZero(Info, Box::new(Term::Zero(Info)))));
}
