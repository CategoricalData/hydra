(* Note: this is an automatically generated file. Do not edit. *)

(* Hydra primitive library: hydra.lib.maybes *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Import ListNotations.

Axiom apply : forall (x y : Type), option (x -> y) -> option x -> option y.
Arguments apply {x y}.
Axiom bind : forall (x y : Type), option x -> (x -> option y) -> option y.
Arguments bind {x y}.
Axiom cases : forall (x y : Type), option x -> y -> (x -> y) -> y.
Arguments cases {x y}.
Axiom cat : forall (x : Type), list (option x) -> list x.
Arguments cat {x}.
Axiom compose : forall (x y z : Type), (x -> option y) -> (y -> option z) -> x -> option z.
Arguments compose {x y z}.
Axiom fromJust : forall (x : Type), option x -> x.
Arguments fromJust {x}.
Axiom fromMaybe : forall (x : Type), x -> option x -> x.
Arguments fromMaybe {x}.
Axiom isJust : forall (x : Type), option x -> bool.
Arguments isJust {x}.
Axiom isNothing : forall (x : Type), option x -> bool.
Arguments isNothing {x}.
Axiom map : forall (x y : Type), (x -> y) -> option x -> option y.
Arguments map {x y}.
Axiom mapMaybe : forall (x y : Type), (x -> option y) -> list x -> list y.
Arguments mapMaybe {x y}.
Axiom maybe : forall (y x : Type), y -> (x -> y) -> option x -> y.
Arguments maybe {y x}.
Axiom pure : forall (x : Type), x -> option x.
Arguments pure {x}.
Axiom toList : forall (x : Type), option x -> list x.
Arguments toList {x}.
