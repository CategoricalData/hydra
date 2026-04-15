(* Note: this is an automatically generated file. Do not edit. *)

(* Hydra primitive library: hydra.lib.logic *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.

Definition and (a b : bool) : bool := andb a b.
Axiom ifElse : forall (x : Type), bool -> x -> x -> x.
Arguments ifElse {x}.
Definition not (a : bool) : bool := negb a.
Definition or (a b : bool) : bool := orb a b.
