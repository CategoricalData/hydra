(* Note: this is an automatically generated file. Do not edit. *)

(* Hydra primitive library: hydra.lib.eithers *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Import ListNotations.

Axiom bimap : forall (x y z w : Type), (x -> z) -> (y -> w) -> (x + y) -> (z + w).
Arguments bimap {x y z w}.
Axiom bind : forall (x y z : Type), (x + y) -> (y -> (x + z)) -> (x + z).
Arguments bind {x y z}.
Axiom either : forall (x y z : Type), (x -> z) -> (y -> z) -> (x + y) -> z.
Arguments either {x y z}.
Axiom foldl : forall (x y z : Type), (x -> y -> (z + x)) -> x -> list y -> (z + x).
Arguments foldl {x y z}.
Axiom fromLeft : forall (x y : Type), x -> (x + y) -> x.
Arguments fromLeft {x y}.
Axiom fromRight : forall (x y : Type), y -> (x + y) -> y.
Arguments fromRight {x y}.
Axiom isLeft : forall (x y : Type), (x + y) -> bool.
Arguments isLeft {x y}.
Axiom isRight : forall (x y : Type), (x + y) -> bool.
Arguments isRight {x y}.
Axiom lefts : forall (x y : Type), list (x + y) -> list x.
Arguments lefts {x y}.
Axiom map : forall (x y z : Type), (x -> y) -> (z + x) -> (z + y).
Arguments map {x y z}.
Axiom mapList : forall (x y z : Type), (x -> (z + y)) -> list x -> (z + list y).
Arguments mapList {x y z}.
Axiom mapMaybe : forall (x y z : Type), (x -> (z + y)) -> option x -> (z + option y).
Arguments mapMaybe {x y z}.
Axiom mapSet : forall (x y z : Type), (x -> (z + y)) -> list x -> (z + list y).
Arguments mapSet {x y z}.
Axiom partitionEithers : forall (x y : Type), list (x + y) -> (list x * list y).
Arguments partitionEithers {x y}.
Axiom rights : forall (x y : Type), list (x + y) -> list y.
Arguments rights {x y}.
