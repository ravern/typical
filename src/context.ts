import { Monotype } from './monotype';

export type Context = { 
  names: { [name: string]: Monotype };
  unkCount: number;
};

export function empty(): Context {
  return {
    names: {},
    unkCount: 0,
  };
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
