(* Note: this is an automatically generated file. Do not edit. *)

(* Hydra primitive library: hydra.lib.equality *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Require Import hydra.util.

Axiom compare : forall (x : Type), x -> x -> Comparison.
Arguments compare {x}.
Axiom equal : forall (x : Type), x -> x -> bool.
Arguments equal {x}.
Axiom gt : forall (x : Type), x -> x -> bool.
Arguments gt {x}.
Axiom gte : forall (x : Type), x -> x -> bool.
Arguments gte {x}.
Axiom identity : forall (x : Type), x -> x.
Arguments identity {x}.
Axiom lt : forall (x : Type), x -> x -> bool.
Arguments lt {x}.
Axiom lte : forall (x : Type), x -> x -> bool.
Arguments lte {x}.
Axiom max : forall (x : Type), x -> x -> x.
Arguments max {x}.
Axiom min : forall (x : Type), x -> x -> x.
Arguments min {x}.
