export type Monotype =
  | Monotype.Int
  | Monotype.Bool
  | Monotype.Str
  | Monotype.Fun
  | Monotype.Unk;
    
export namespace Monotype {
  export enum Type {
    Int = "Int",
    Bool = "Bool",
    Str = "Str",
    Fun = "Fun",
    Unk = "Unk",
  };

  export type Int = {
    type: Type.Int,
  };

  export type Bool = {
    type: Type.Bool,
  };

  export type Str = {
    type: Type.Str,
  };

  export type Fun = {
    type: Type.Fun,
    argument: Monotype,
    result: Monotype,
  };

  export type Unk = {
    type: Type.Unk,
    id: number;
  };
}

export function show(monotype: Monotype): string {
  switch (monotype.type) {
    case Monotype.Type.Int:
    case Monotype.Type.Bool:
    case Monotype.Type.Str:
      return monotype.type;
    case Monotype.Type.Fun:
      return `${show(monotype.argument)} -> ${show(monotype.result)}`;
    case Monotype.Type.Unk:
      return String.fromCharCode("a".charCodeAt(0) + monotype.id);
  }
}
