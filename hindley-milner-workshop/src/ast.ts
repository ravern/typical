export type Expr =
  | Expr.Int
  | Expr.Bool
  | Expr.Str
  | Expr.Lam
  | Expr.App
  | Expr.Let
  | Expr.Var
  | Expr.If;

export namespace Expr {
  export enum Type {
    Int = "Int",
    Bool = "Bool",
    Str = "Str",
    Lam = "Lam",
    App = "App",
    Let = "Let",
    Var = "Var",
    If = "If",
  };

  export type Int = {
    type: Type.Int,
    int: number;
  };

  export type Bool = {
    type: Type.Bool,
    bool: boolean;
  };

  export type Str = {
    type: Type.Str,
    str: string;
  };

  export type Lam = {
    type: Type.Lam,
    binder: string,
    body: Expr,
  };

  export type App = {
    type: Type.App,
    function: Expr,
    argument: Expr,
  }

  export type Let = {
    type: Type.Let,
    binder: string,
    expr: Expr,
    body: Expr,
  };

  export type Var = {
    type: Type.Var,
    name: string,
  }

  export type If = {
    type: Type.If,
    condition: Expr,
    then: Expr,
    else: Expr,
  }
}
