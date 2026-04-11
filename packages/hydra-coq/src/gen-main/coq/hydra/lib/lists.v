(* Note: this is an automatically generated file. Do not edit. *)

(* Hydra primitive library: hydra.lib.lists *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Import ListNotations.

Axiom apply : forall (x y : Type), list (x -> y) -> list x -> list y.
Arguments apply {x y}.
Axiom at_ : forall (x : Type), Z -> list x -> x.
Arguments at_ {x}.
Axiom bind : forall (x y : Type), list x -> (x -> list y) -> list y.
Arguments bind {x y}.
Axiom concat : forall (x : Type), list (list x) -> list x.
Arguments concat {x}.
Axiom concat2 : forall (x : Type), list x -> list x -> list x.
Arguments concat2 {x}.
Axiom cons : forall (x : Type), x -> list x -> list x.
Arguments cons {x}.
Axiom drop : forall (x : Type), Z -> list x -> list x.
Arguments drop {x}.
Axiom dropWhile : forall (x : Type), (x -> bool) -> list x -> list x.
Arguments dropWhile {x}.
Axiom elem : forall (x : Type), x -> list x -> bool.
Arguments elem {x}.
Axiom filter : forall (x : Type), (x -> bool) -> list x -> list x.
Arguments filter {x}.
Axiom find : forall (x : Type), (x -> bool) -> list x -> option x.
Arguments find {x}.
Axiom foldl : forall (y x : Type), (y -> x -> y) -> y -> list x -> y.
Arguments foldl {y x}.
Axiom foldr : forall (x y : Type), (x -> y -> y) -> y -> list x -> y.
Arguments foldr {x y}.
Axiom group : forall (x : Type), list x -> list (list x).
Arguments group {x}.
Axiom head : forall (x : Type), list x -> x.
Arguments head {x}.
Axiom init : forall (x : Type), list x -> list x.
Arguments init {x}.
Axiom intercalate : forall (x : Type), list x -> list (list x) -> list x.
Arguments intercalate {x}.
Axiom intersperse : forall (x : Type), x -> list x -> list x.
Arguments intersperse {x}.
Axiom last : forall (x : Type), list x -> x.
Arguments last {x}.
Axiom length : forall (x : Type), list x -> Z.
Arguments length {x}.
Axiom map : forall (x y : Type), (x -> y) -> list x -> list y.
Arguments map {x y}.
Axiom maybeAt : forall (x : Type), Z -> list x -> option x.
Arguments maybeAt {x}.
Axiom maybeHead : forall (x : Type), list x -> option x.
Arguments maybeHead {x}.
Axiom maybeInit : forall (x : Type), list x -> option (list x).
Arguments maybeInit {x}.
Axiom maybeLast : forall (x : Type), list x -> option x.
Arguments maybeLast {x}.
Axiom maybeTail : forall (x : Type), list x -> option (list x).
Arguments maybeTail {x}.
Axiom nub : forall (x : Type), list x -> list x.
Arguments nub {x}.
Axiom null : forall (x : Type), list x -> bool.
Arguments null {x}.
Axiom partition : forall (x : Type), (x -> bool) -> list x -> (list x * list x).
Arguments partition {x}.
Axiom pure : forall (x : Type), x -> list x.
Arguments pure {x}.
Axiom replicate : forall (x : Type), Z -> x -> list x.
Arguments replicate {x}.
Axiom reverse : forall (x : Type), list x -> list x.
Arguments reverse {x}.
Axiom safeHead : forall (x : Type), list x -> option x.
Arguments safeHead {x}.
Axiom singleton : forall (x : Type), x -> list x.
Arguments singleton {x}.
Axiom sort : forall (x : Type), list x -> list x.
Arguments sort {x}.
Axiom sortOn : forall (x y : Type), (x -> y) -> list x -> list x.
Arguments sortOn {x y}.
Axiom span : forall (x : Type), (x -> bool) -> list x -> (list x * list x).
Arguments span {x}.
Axiom tail : forall (x : Type), list x -> list x.
Arguments tail {x}.
Axiom take : forall (x : Type), Z -> list x -> list x.
Arguments take {x}.
Axiom transpose : forall (x : Type), list (list x) -> list (list x).
Arguments transpose {x}.
Axiom zip : forall (x y : Type), list x -> list y -> list (x * y).
Arguments zip {x y}.
Axiom zipWith : forall (x y z : Type), (x -> y -> z) -> list x -> list y -> list z.
Arguments zipWith {x y z}.
