import { Monotype } from './monotype';

export type Context = { 
  names: { [name: string]: Monotype };
  substitutions: { [id: number]: Monotype };
  unkCount: number;
};

export function empty(): Context {
  return {
    names: {},
    substitutions: {},
    unkCount: 0,
  };
}

export function addSubstitution(ctx: Context, id: number, monotype: Monotype): Context {
  return {
    ...ctx,
    substitutions: {
      ...ctx.substitutions,
      [id]: monotype,
    }
  };
}

export function getSubstitution(ctx: Context, id: number): Monotype | null {
  return ctx.substitutions[id] ?? null;
}

export function setName(ctx: Context, name: string, monotype: Monotype): Context {
  return {
    ...ctx,
    names: {
      ...ctx.names,
      [name]: monotype,
    },
  };
}

export function getName(ctx: Context, name: string): Monotype | null {
  return ctx.names[name] ?? null;
}

export function freshUnk(ctx: Context): [Context, number] {
  return [
    {
      ...ctx,
      unkCount: ctx.unkCount + 1,
    },
    ctx.unkCount,
  ];
}
