type Term = TmTrue | TmFalse | TmIf | TmZero | TmSucc | TmPred | TmIsZero;

type TmTrue = true;
type TmFalse = false;
type TmIf = { if: [Term, Term, Term] };
type TmZero = 0;
type TmSucc = { succ: Term };
type TmPred = { pred: Term };
type TmIsZero = { isZero: Term };

const tmTrue = true;
const tmFalse = false;
const tmIf = (t1: Term, t2: Term, t3: Term): TmIf => ({ if: [t1, t2, t3] });
const tmZero = 0;
const tmSucc = (t: Term): TmSucc => ({ succ: t });
const tmPred = (t: Term): TmPred => ({ pred: t });
const tmIsZero = (t: Term): TmIsZero => ({ isZero: t });

const isTmTrue = (t: Term): t is TmTrue => t === true;
const isTmFalse = (t: Term): t is TmFalse => t === false;
const isTmIf = (t: Term): t is TmIf => (t as TmIf).if !== undefined;
const isTmZero = (t: Term): t is TmZero => t === 0;
const isTmSucc = (t: Term): t is TmSucc => (t as TmSucc).succ !== undefined;
const isTmPred = (t: Term): t is TmPred => (t as TmPred).pred !== undefined;
const isTmIsZero = (t: Term): t is TmIsZero => (t as TmIsZero).isZero !== undefined;

class NoRuleAppliesError extends Error {
  constructor() {
    super('No rule applies.');
    this.name = 'NoRuleAppliesError';
  }
}

function isNumericVal(t: Term): boolean {
    return isTmZero(t) || (isTmSucc(t) && isNumericVal(t.succ));
}

function isVal(t: Term): boolean {
  return isTmTrue(t) || isTmFalse(t) || isNumericVal(t);
}

function eval1(t: Term): Term {
  if (isTmIf(t)) {
    if (isTmTrue(t.if[0])) {
      return t.if[1];
    } else if (isTmFalse(t.if[0])) {
      return t.if[2];
    } else {
      return tmIf(eval1(t.if[0]), t.if[1], t.if[2]);
    }
  } else if (isTmSucc(t)) {
    return tmSucc(eval1(t.succ));
  } else if (isTmPred(t)) {
    if (isTmZero(t.pred)) {
      return tmZero;
    } else if (isTmSucc(t.pred)) {
      return t.pred.succ;
    } else {
      return tmPred(eval1(t.pred));
    }
  } else if (isTmIsZero(t)) {
    if (isTmZero(t.isZero)) {
      return tmTrue;
    } else if (isTmSucc(t.isZero) && isNumericVal(t.isZero.succ)) {
      return tmFalse;
    } else {
      return tmIsZero(eval1(t.isZero));
    }
  } else {
    throw new NoRuleAppliesError();
  }
}

function eval_(t: Term): Term {
  try {
    return eval_(eval1(t))
  } catch (err) {
    if (err instanceof NoRuleAppliesError) {
      return t;
    } else {
      throw err;
    }
  }
}

console.log(eval_(tmIf(tmTrue, tmSucc(tmZero), tmZero)));
// => { succ: 'zero' }