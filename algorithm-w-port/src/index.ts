import { createInterface } from 'readline';

import { parse } from './parse';

const readline = createInterface({
  input: process.stdin,
  output: process.stdout,
});

function repl(line: string) {
  try {
    const expr = parse(line);
    console.log(expr);
  } catch (error) {
    console.error(error.message);
  } finally {
    readline.question('> ', repl);
  }
}

function main() {
  readline.question('> ', repl);
}

main();
