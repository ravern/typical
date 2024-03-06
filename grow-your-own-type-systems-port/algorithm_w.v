Require Import List String Nat.
Open Scope string_scope.

Inductive result (ok : Type) (error : Type) :=
  | OK : ok -> result ok error
  | Error : error -> result ok error.

Definition name := string.

Inductive expr :=
  | ENum : nat -> expr
  | EBool : bool -> expr
  | EVar : name -> expr
  | EApp : name -> list expr -> expr
  | EAbs : list name -> expr.

Definition id := nat.
Definition level := nat.

Inductive type :=
  | TConst : name -> type
  | TApp : type -> list type -> type
  | TAbs : list type -> type -> type
  | TVar : type_var -> type
with type_var :=
  | Unbound : id -> level -> type_var
  | Link : id -> type_var
  | Generic : id -> type_var.

Inductive env :=
  | Empty : env
  | Frame : env -> name -> type -> env.

Fixpoint unify (type1 type2 : type) : result type string :=
  OK type string (TConst "int").