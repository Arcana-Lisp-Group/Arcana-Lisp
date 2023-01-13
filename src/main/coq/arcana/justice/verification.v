Inductive identifier: Type :=
  | ident (id: nat).

(* we now only deal with nature number and simple lambda/applications *)
Inductive expr: Set :=
  Const: nat -> expr | Vari: identifier -> expr | Lam: identifier -> expr -> expr | App: expr -> expr -> expr.

Inductive type: Set :=
  Nat: type | Var: identifier -> type | Arrow: type -> type -> type.

Inductive optional_type: Set :=
  None: optional_type | Some: type -> optional_type.

Definition type_env := identifier -> optional_type.
Definition empty_type_env: type_env := (fun _ => None).

Fixpoint eq(n m: nat): bool :=
  match n with
    | 0 =>
      match m with
        | 0 => true
        | S _ => false
      end
    | S n' =>
      match m with
        | 0 => false
        | S m' => eq n' m'
      end
  end.

Notation "x =? y" := (eq x y) (at level 70).

Definition is_same_id(id1 id2: identifier): bool :=
  match id1 with
    | ident(n1) =>
      match id2 with
        | ident(n2) => n1 =? n2
      end
  end.

Definition bind(env: type_env)(id: identifier)(ty: type) :=
  (fun id' => if is_same_id id id' then Some ty else None).

Inductive type_of: type_env -> expr -> type -> Prop :=
  type_of_const: forall env: type_env, forall n: nat, (type_of env (Const n) Nat) |
  type_of_var: forall env: type_env, forall id: identifier, forall ty: type,
    (env id) = (Some ty) -> (type_of env (Vari id) ty) |
  type_of_lam: forall env: type_env, forall x: identifier, forall exp: expr, forall t1 t2: type,
    (type_of (bind env x t1) exp t2) -> (type_of env (Lam x exp) (Arrow t1 t2)) |
  type_op_app: forall env: type_env, forall exp1 exp2: expr, forall t1 t2: type,
    (type_of env exp1 (Arrow t1 t2)) -> (type_of env (App exp1 exp2) t2).

Inductive value: Set :=
  Int: nat -> value | Func: identifier -> expr -> value.
Inductive optional_value: Set :=
  Err: optional_value | Res: value -> optional_value.

Definition eval_env := expr -> optional_value.

Inductive value_of: eval_env -> expr -> value -> Prop :=
  eval_of_const: forall env: eval_env, forall n: nat, (value_of env (Const n) (Int n)) |
  eval_of_ident: forall env: eval_env, forall id: identifier, forall v: value,
    (env (Vari id)) = Res v -> (value_of env (Vari id) v) |
  eval_of_lam: forall env: eval_env, forall id: identifier, forall exp: expr,
    (value_of env (Lam id exp) (Func id exp)) |
  eval_of_app: forall env: eval_env, forall t1 t2 t': expr, forall u v: value, forall id: identifier,
    (value_of env t1 (Func id t')) -> (value_of env t2 u) -> (value_of env (App t1 t2) v).

Theorem progress: forall exp: expr, forall ty: type, exists exp': expr, True.
Admitted.

Theorem preservation: forall exp: expr, True.
Admitted.

