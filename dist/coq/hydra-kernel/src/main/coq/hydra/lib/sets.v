(* Note: this is an automatically generated file. Do not edit. *)

(* Hydra primitive library: hydra.lib.sets *)

(* Sets are represented as sorted lists *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Import ListNotations.

Axiom delete : forall (x : Type), x -> list x -> list x.
Arguments delete {x}.
Axiom difference : forall (x : Type), list x -> list x -> list x.
Arguments difference {x}.
Axiom empty : forall (x : Type), list x.
Arguments empty {x}.
Axiom fromList : forall (x : Type), list x -> list x.
Arguments fromList {x}.
Axiom insert : forall (x : Type), x -> list x -> list x.
Arguments insert {x}.
Axiom intersection : forall (x : Type), list x -> list x -> list x.
Arguments intersection {x}.
Axiom map : forall (x y : Type), (x -> y) -> list x -> list y.
Arguments map {x y}.
Axiom member : forall (x : Type), x -> list x -> bool.
Arguments member {x}.
Axiom null : forall (x : Type), list x -> bool.
Arguments null {x}.
Axiom singleton : forall (x : Type), x -> list x.
Arguments singleton {x}.
Axiom size : forall (x : Type), list x -> Z.
Arguments size {x}.
Axiom toList : forall (x : Type), list x -> list x.
Arguments toList {x}.
Axiom union : forall (x : Type), list x -> list x -> list x.
Arguments union {x}.
Axiom unions : forall (x : Type), list (list x) -> list x.
Arguments unions {x}.
