Inductive identifier: Type :=
  | ident (id: nat).

(* we now only deal with nature number and simple lambda/applications *)
Inductive expr: Set :=
  Const: nat -> expr | Vari: identifier -> expr | Lam: identifier -> expr -> expr | App: expr -> expr -> expr.

Inductive type: Set :=
  Nat: type | Var: identifier -> type | Arrow: type -> type -> type.

