(* Note: this is an automatically generated file. Do not edit. *)

(* Hydra primitive library: hydra.lib.maps *)

(* Maps are represented as association lists: list (k * v) *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Import ListNotations.

Axiom alter : forall (v k : Type), (option v -> option v) -> k -> list (k * v) -> list (k * v).
Arguments alter {v k}.
Axiom bimap : forall (k1 k2 v1 v2 : Type), (k1 -> k2) -> (v1 -> v2) -> list (k1 * v1) -> list (k2 * v2).
Arguments bimap {k1 k2 v1 v2}.
Axiom delete : forall (k v : Type), k -> list (k * v) -> list (k * v).
Arguments delete {k v}.
Axiom elems : forall (k v : Type), list (k * v) -> list v.
Arguments elems {k v}.
Axiom empty : forall (k v : Type), list (k * v).
Arguments empty {k v}.
Axiom filter : forall (v k : Type), (v -> bool) -> list (k * v) -> list (k * v).
Arguments filter {v k}.
Axiom filterWithKey : forall (k v : Type), (k -> v -> bool) -> list (k * v) -> list (k * v).
Arguments filterWithKey {k v}.
Axiom findWithDefault : forall (v k : Type), v -> k -> list (k * v) -> v.
Arguments findWithDefault {v k}.
Axiom fromList : forall (k v : Type), list (k * v) -> list (k * v).
Arguments fromList {k v}.
Axiom insert : forall (k v : Type), k -> v -> list (k * v) -> list (k * v).
Arguments insert {k v}.
Axiom keys : forall (k v : Type), list (k * v) -> list k.
Arguments keys {k v}.
Axiom lookup : forall (k v : Type), k -> list (k * v) -> option v.
Arguments lookup {k v}.
Axiom map : forall (k v1 v2 : Type), (v1 -> v2) -> list (k * v1) -> list (k * v2).
Arguments map {k v1 v2}.
Axiom mapKeys : forall (k1 k2 v : Type), (k1 -> k2) -> list (k1 * v) -> list (k2 * v).
Arguments mapKeys {k1 k2 v}.
Axiom member : forall (k v : Type), k -> list (k * v) -> bool.
Arguments member {k v}.
Axiom null : forall (k v : Type), list (k * v) -> bool.
Arguments null {k v}.
Axiom singleton : forall (k v : Type), k -> v -> list (k * v).
Arguments singleton {k v}.
Axiom size : forall (k v : Type), list (k * v) -> Z.
Arguments size {k v}.
Axiom toList : forall (k v : Type), list (k * v) -> list (k * v).
Arguments toList {k v}.
Axiom union : forall (k v : Type), list (k * v) -> list (k * v) -> list (k * v).
Arguments union {k v}.
