import { Expr } from './ast';

export function parse(source: string): Expr {
  const chars = source.split('').reverse();
  return app(chars);
}

function app(chars: string[]): Expr {
  const function_ = expr(chars);

  whitespace(chars);

  if (chars.length === 0) {
    return function_;
  }

  const argument = app(chars);

  return {
    type: Expr.Type.App,
    function: function_,
    argument,
  };
}

function expr(chars: string[]): Expr {
  whitespace(chars);

  const char = chars[chars.length-1];
  if (char === '(') {
    return group(chars);
  } else if (char === '\\') {
    return lam(chars);
  } else if (char === '"') {
    return str(chars);
  } else if (isDigit(char)) {
    return int(chars);
  } else if (isAlphabetic(char)) {
    return variable(chars);
  }

  throw new Error(`unexpected char ${char}`);
}

function group(chars: string[]): Expr {
  if (chars.pop() !== '(') {
    throw new Error(`expected '('`);
  }

  whitespace(chars);

  const expr_ = expr(chars);

  whitespace(chars);

  if (chars.pop() !== ')') {
    throw new Error(`expected '('`);
  }

  return expr_;
}

function lam(chars: string[]): Expr {
  if (chars.pop() !== '\\') {
    throw new Error(`expected '\\'`);
  }

  const binder = name(chars);

  whitespace(chars);

  if (chars.pop() !== '-') {
    throw new Error(`expected '->'`);
  }
  if (chars.pop() !== '>') {
    throw new Error(`expected '->'`);
  }

  whitespace(chars);

  const body = expr(chars);

  return {
    type: Expr.Type.Lam,
    binder,
    body,
  };
}

function int(chars: string[]): Expr {
  const digits = [];

  while (true) {
    const char = chars[chars.length-1];
    if (!isDigit(char)) {
      break;
    }
    digits.push(chars.pop());
  }

  return {
    type: Expr.Type.Int,
    int: parseInt(digits.join(''), 10),
  };
}

function str(chars: string[]): Expr {
  if (chars.pop() !== '"') {
    throw new Error(`expected '"'`);
  }

  const letters = [];

  while (true) {
    const char = chars[chars.length-1];
    if (char === '"') {
      break;
    }
    letters.push(chars.pop());
  }

  if (chars.pop() !== '"') {
    throw new Error(`expected '=' after binder`);
  }

  return {
    type: Expr.Type.Str,
    str: letters.join(''),
  };
}

function variable(chars: string[]): Expr {
  const letters = [];

  while (true) {
    const char = chars[chars.length-1];
    if (!isAlphabetic(char)) {
      break;
    }
    letters.push(chars.pop());
  }

  const name = letters.join('');

  if (name === "let") {
    return let_(chars);
  } else if (name === "if") {
    return if_(chars);
  } else if (name === "true") {
    return { type: Expr.Type.Bool, bool: true };
  } else if (name === "false") {
    return { type: Expr.Type.Bool, bool: false };
  }

  return {
    type: Expr.Type.Var,
    name,
  };
}

function let_(chars: string[]): Expr {
  whitespace(chars);

  const binder = name(chars);

  whitespace(chars);

  if (chars.pop() !== '=') {
    throw new Error(`expected '=' after binder`);
  }

  whitespace(chars);

  const expr_ = expr(chars);

  whitespace(chars);

  if (name(chars) !== "in") {
    throw new Error(`expected 'in' after binding`);
  }

  whitespace(chars);

  const body = expr(chars);

  return {
    type: Expr.Type.Let,
    binder,
    expr: expr_,
    body,
  };
}

function if_(chars: string[]): Expr {
  whitespace(chars);

  const condition = expr(chars);

  whitespace(chars);

  if (name(chars) !== "then") {
    throw new Error(`expected 'then' after condition`);
  }

  whitespace(chars);

  const then = expr(chars);

  whitespace(chars);

  if (name(chars) !== "else") {
    throw new Error(`expected 'else' after then clause`);
  }

  whitespace(chars);

  const else_ = expr(chars);

  return {
    type: Expr.Type.If,
    condition,
    then,
    else: else_,
  };
}

function name(chars: string[]) {
  const binder = variable(chars);
  if (binder.type !== Expr.Type.Var) {
    throw new Error(`expected name`);
  }
  return binder.name;
}

function whitespace(chars: string[]) {
  while (true) {
    const char = chars[chars.length-1];
    if (!isWhitespace(char)) {
      break;
    }
    chars.pop();
  }
}

function isDigit(char: string): boolean {
  return '0123456789'.indexOf(char) !== -1;
}

function isAlphabetic(char: string): boolean {
  return 'abcdefghijklmnopqrstuvwxyz'.indexOf(char) !== -1;
}

function isWhitespace(char: string): boolean {
  return ' \t\n\r'.indexOf(char) !== -1;
}
