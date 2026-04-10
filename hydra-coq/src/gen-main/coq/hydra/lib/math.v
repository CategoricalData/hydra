(* Note: this is an automatically generated file. Do not edit. *)

(* Hydra primitive library: hydra.lib.math *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Import ListNotations.

(* Float64 operations *)
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
Axiom abs : Z -> Z.
Axiom add : Z -> Z -> Z.
Axiom div : Z -> Z -> Z.
Axiom even : Z -> bool.
Axiom max : Z -> Z -> Z.
Axiom maybeDiv : Z -> Z -> option Z.
Axiom min : Z -> Z -> Z.
Axiom maybeMod : Z -> Z -> option Z.
Axiom mod_ : Z -> Z -> Z.
Axiom mul : Z -> Z -> Z.
Axiom negate : Z -> Z.
Axiom odd : Z -> bool.
Axiom maybePred : Z -> option Z.
Axiom pred : Z -> Z.
Axiom range : Z -> Z -> list Z.
Axiom maybeRem : Z -> Z -> option Z.
Axiom rem : Z -> Z -> Z.
Axiom signum : Z -> Z.
Axiom sub : Z -> Z -> Z.
Axiom maybeSucc : Z -> option Z.
Axiom succ : Z -> Z.
