import { Expr } from './ast';
import { Monotype, show } from './monotype';

import { Context, empty, setName, getName, freshUnk } from './context';

function infer(ctx: Context, expr: Expr): Monotype {
  switch (expr.type) {
    case Expr.Type.Int: return { type: Monotype.Type.Int };
    case Expr.Type.Bool: return { type: Monotype.Type.Bool };
    case Expr.Type.Str: return { type: Monotype.Type.Str };

    case Expr.Type.Lam: {
      // Create a fresh unknown for the argument — we're going to infer
      // the type of that argument later on through substitution and 
      // unification.
      const [ctxWithUnk, id] = freshUnk(ctx);
      const argumentType: Monotype = {
        type: Monotype.Type.Unk,
        id,
      };

      // Create the binding in the context, ensuring it doesn't already
      // exist in the context.
      if (getName(ctx, expr.binder) != null) {
        throw new Error(`variable '${expr.binder}' already bound`);
      }
      const ctxWithName = setName(ctxWithUnk, expr.binder, argumentType);

      // We recursively infer the type of the body here. The result type
      // of a function is simply the inferred type of its body.
      return {
        type: Monotype.Type.Fun,
        argument: argumentType,
        result: infer(ctxWithName, expr.body),
      };
    }

    case Expr.Type.Let: {
      const exprType: Monotype = infer(ctx, expr.expr);

      // Create the binding in the context, ensuring it doesn't already
      // exist in the context.
      if (getName(ctx, expr.binder) != null) {
        throw new Error(`variable '${expr.binder}' already bound`);
      }
      const ctxWithName = setName(ctx, expr.binder, exprType);

      return infer(ctxWithName, expr.body);
    }

    case Expr.Type.Var: {
      const type = getName(ctx, expr.name);
      if (!type) {
        throw new Error(`unknown variable '${expr.name}'`);
      }
      return type;
    };

    default: throw new Error("invalid expression");
  }
}

const expr: Expr = {
  type: Expr.Type.Let,
  binder: "x",
  expr: {
    type: Expr.Type.Int,
    int: 3,
  },
  body: {
    type: Expr.Type.Lam,
    binder: "y",
    body: {
      type: Expr.Type.Var,
      name: "x",
    },
  },
};

console.log(show(infer(empty(), expr)));
