import { Expr } from './ast';
import { Monotype, equal, show, containsUnknown } from './monotype';

import { Context, empty, addSubstitution, getSubstitution, setName, getName, freshUnk } from './context';

function substitute(ctx: Context, monotype: Monotype): Monotype {
  switch (monotype.type) {
    case Monotype.Type.Int:
    case Monotype.Type.Bool:
    case Monotype.Type.Str:
      return monotype;
    case Monotype.Type.Fun:
      return {
        type: Monotype.Type.Fun,
        argument: substitute(ctx, monotype.argument),
        result: substitute(ctx, monotype.result),
      };
    case Monotype.Type.Unk: {
      const resolvedMonotype = getSubstitution(ctx, monotype.id);
      return resolvedMonotype
        ? substitute(ctx, resolvedMonotype)
        : monotype;
    }
  }
}

// Checks if the types are equivalent with the help of substitutions from
// the given context.
function unify(ctx: Context, leftRawMonotype: Monotype, rightRawMonotype: Monotype): Context {
  const leftMonotype = substitute(ctx, leftRawMonotype);
  const rightMonotype = substitute(ctx, rightRawMonotype);

  if (equal(leftMonotype, rightMonotype)) {
    return ctx;
  }

  if (leftMonotype.type === Monotype.Type.Fun && rightMonotype.type === Monotype.Type.Fun) {
    const ctxWithUnify = unify(ctx, leftMonotype.argument, rightMonotype.argument);
    return unify(ctxWithUnify, leftMonotype.result, rightMonotype.result);
  }

  if (leftMonotype.type === Monotype.Type.Unk) {
    if (containsUnknown(rightMonotype, leftMonotype.id)) {
      throw new Error('occurs check');
    }
    return addSubstitution(ctx, leftMonotype.id, rightMonotype);
  } else if (rightMonotype.type === Monotype.Type.Unk) {
    if (containsUnknown(leftMonotype, rightMonotype.id)) {
      throw new Error('occurs check');
    }
    return addSubstitution(ctx, rightMonotype.id, leftMonotype);
  }

  throw new Error(`can't match ${show(leftMonotype)} with ${show(rightMonotype)}`);
}

function infer(ctx: Context, expr: Expr): [Context, Monotype] {
  switch (expr.type) {
    case Expr.Type.Int:  return [ctx, { type: Monotype.Type.Int }];
    case Expr.Type.Bool: return [ctx, { type: Monotype.Type.Bool }];
    case Expr.Type.Str:  return [ctx, { type: Monotype.Type.Str }];

    case Expr.Type.Lam: {
      // Create a fresh unknown for the argument — we're going to check
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
      const [ctxAfterInfer, resultType] = infer(ctxWithName, expr.body);

      return [
        ctxAfterInfer,
        {
          type: Monotype.Type.Fun,
          argument: argumentType,
          result: resultType,
        },
      ];
    }

    case Expr.Type.App: {
      const [ctxAfterFunctionInfer, functionType] = infer(ctx, expr.function);
      const [ctxAfterArgumentInfer, argumentType] = infer(ctxAfterFunctionInfer, expr.argument);

      const [ctxWithUnknown, id] = freshUnk(ctxAfterArgumentInfer);
      const resultType: Monotype = {
        type: Monotype.Type.Unk,
        id,
      };

      const ctxAfterUnification = unify(ctxWithUnknown, functionType, {
        type: Monotype.Type.Fun,
        argument: argumentType,
        result: resultType,
      });

      return [ctxAfterUnification, substitute(ctxAfterUnification, resultType)];
    }

    case Expr.Type.Let: {
      const [ctxAfterInfer, exprType] = infer(ctx, expr.expr);

      // Create the binding in the context, ensuring it doesn't already
      // exist in the context.
      if (getName(ctx, expr.binder) != null) {
        throw new Error(`variable '${expr.binder}' already bound`);
      }
      const ctxWithName = setName(ctxAfterInfer, expr.binder, exprType);

      return infer(ctxWithName, expr.body);
    }

    case Expr.Type.Var: {
      const type = getName(ctx, expr.name);
      if (!type) {
        throw new Error(`unknown variable '${expr.name}'`);
      }
      return [ctx, type];
    };

    case Expr.Type.If: {
      const [ctxAfterConditionInfer, conditionType] = infer(ctx, expr.condition);
      const ctxAfterConditionUnification = unify(ctxAfterConditionInfer, conditionType, { type: Monotype.Type.Bool });

      const [ctxAfterThenInfer, thenType] = infer(ctxAfterConditionUnification, expr.then);
      const [ctxAfterElseInfer, elseType] = infer(ctxAfterThenInfer, expr.else);

      return [
        unify(ctxAfterElseInfer, thenType, elseType),
        thenType,
      ];
    };

    default: throw new Error("invalid expression");
  }
}

//            \x -> (\y -> y) x

const expr: Expr = {
  type: Expr.Type.Lam,
  binder: "x",
  body: {
    type: Expr.Type.If,
    condition: {
      type: Expr.Type.Bool,
      bool: false,
    },
    then: {
      type: Expr.Type.Int,
      int: 3,
    },
    else: {
      type: Expr.Type.Var,
      name: "x",
    },
  },
};

const [ctx, type] = infer(empty(), expr);
console.log(show(substitute(ctx, type)));
