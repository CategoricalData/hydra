(* Hydra primitive library: hydra.lib.math *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Import ListNotations.

(* Float64 operations — these cannot compute in Coq and remain as axioms. *)
Axiom acos : Q -> Q.
Axiom acosh : Q -> Q.
Axiom asin : Q -> Q.
Axiom asinh : Q -> Q.
Axiom atan : Q -> Q.
Axiom atan2 : Q -> Q -> Q.
Axiom atanh : Q -> Q.
Axiom ceiling : Q -> Q.
Axiom cos : Q -> Q.
Axiom cosh : Q -> Q.
Axiom e : Q.
Axiom exp : Q -> Q.
Axiom floor : Q -> Q.
Axiom log : Q -> Q.
Axiom logBase : Q -> Q -> Q.
Axiom pi : Q.
Axiom pow : Q -> Q -> Q.
Axiom round : Q -> Q.
Axiom roundBigfloat : Z -> Q -> Q.
Axiom roundFloat32 : Z -> Q -> Q.
Axiom roundFloat64 : Z -> Q -> Q.
Axiom sin : Q -> Q.
Axiom sinh : Q -> Q.
Axiom sqrt : Q -> Q.
Axiom tan : Q -> Q.
Axiom tanh : Q -> Q.
Axiom truncate : Q -> Q.

(* Int32 operations *)
Open Scope Z_scope.

Definition abs (n : Z) : Z := Z.abs n.

Definition add (x y : Z) : Z := Z.add x y.

Definition div (x y : Z) : Z := Z.div x y.

Definition even (n : Z) : bool := Z.even n.

Definition max (x y : Z) : Z := Z.max x y.

Definition maybeDiv (x y : Z) : option Z :=
  if Z.eqb y 0 then None else Some (Z.div x y).

Definition min (x y : Z) : Z := Z.min x y.

Definition maybeMod (x y : Z) : option Z :=
  if Z.eqb y 0 then None else Some (Z.modulo x y).

Definition mod_ (x y : Z) : Z := Z.modulo x y.

Definition mul (x y : Z) : Z := Z.mul x y.

Definition negate (n : Z) : Z := Z.opp n.

Definition odd (n : Z) : bool := Z.odd n.

Definition maybePred (n : Z) : option Z :=
  if Z.eqb n (-2147483648) then None else Some (Z.pred n).

Definition pred (n : Z) : Z := Z.pred n.

Fixpoint range_aux (fuel : nat) (cur : Z) : list Z :=
  match fuel with
  | O => []
  | S fuel' => cur :: range_aux fuel' (cur + 1)
  end.

Definition range (start stop : Z) : list Z :=
  let len := stop - start + 1 in
  if Z.leb len 0 then []
  else range_aux (Z.to_nat len) start.

Definition maybeRem (x y : Z) : option Z :=
  if Z.eqb y 0 then None else Some (Z.rem x y).

Definition rem (x y : Z) : Z := Z.rem x y.

Definition signum (n : Z) : Z := Z.sgn n.

Definition sub (x y : Z) : Z := Z.sub x y.

Definition maybeSucc (n : Z) : option Z :=
  if Z.eqb n 2147483647 then None else Some (Z.succ n).

Definition succ (n : Z) : Z := Z.succ n.

Close Scope Z_scope.
