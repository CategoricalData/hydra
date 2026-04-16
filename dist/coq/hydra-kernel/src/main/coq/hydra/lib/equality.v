(* Hydra primitive library: hydra.lib.equality *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Require Import hydra.util.
Require Import hydra.lib.base.

Definition compare {x : Type} (a b : x) : Comparison :=
  match hydra_compare a b with
  | Lt => Comparison_LessThan tt
  | Eq => Comparison_EqualTo tt
  | Gt => Comparison_GreaterThan tt
  end.
Arguments compare {x}.

Definition equal {x : Type} (a b : x) : bool := hydra_eq a b.
Arguments equal {x}.

Definition gt {x : Type} (a b : x) : bool :=
  match hydra_compare a b with Gt => true | _ => false end.
Arguments gt {x}.

Definition gte {x : Type} (a b : x) : bool :=
  match hydra_compare a b with Lt => false | _ => true end.
Arguments gte {x}.

Definition identity {x : Type} (v : x) : x := v.
Arguments identity {x}.

Definition lt {x : Type} (a b : x) : bool :=
  match hydra_compare a b with Lt => true | _ => false end.
Arguments lt {x}.

Definition lte {x : Type} (a b : x) : bool :=
  match hydra_compare a b with Gt => false | _ => true end.
Arguments lte {x}.

Definition max {x : Type} (a b : x) : x :=
  match hydra_compare a b with Lt => b | _ => a end.
Arguments max {x}.

Definition min {x : Type} (a b : x) : x :=
  match hydra_compare a b with Gt => b | _ => a end.
Arguments min {x}.
