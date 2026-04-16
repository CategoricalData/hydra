(* Hydra primitive library: hydra.lib.maps *)

(* Maps are represented as association lists: list (k * v) *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Require Import hydra.lib.base.
Import ListNotations.

Open Scope Z_scope.

Definition alter {v k : Type} (f : option v -> option v) (key : k) (m : list (k * v)) : list (k * v) :=
  let existing := List.find (fun '(k', _) => hydra_eq key k') m in
  let rest := List.filter (fun '(k', _) => negb (hydra_eq key k')) m in
  match f (match existing with Some (_, v0) => Some v0 | None => None end) with
  | Some v0 => (key, v0) :: rest
  | None => rest
  end.
Arguments alter {v k}.

Definition bimap {k1 k2 v1 v2 : Type} (f : k1 -> k2) (g : v1 -> v2) (m : list (k1 * v1)) : list (k2 * v2) :=
  List.map (fun '(k, v) => (f k, g v)) m.
Arguments bimap {k1 k2 v1 v2}.

Definition delete {k v : Type} (key : k) (m : list (k * v)) : list (k * v) :=
  List.filter (fun '(k', _) => negb (hydra_eq key k')) m.
Arguments delete {k v}.

Definition elems {k v : Type} (m : list (k * v)) : list v :=
  List.map snd m.
Arguments elems {k v}.

Definition empty {k v : Type} : list (k * v) := [].
Arguments empty {k v}.

Definition filter {v k : Type} (p : v -> bool) (m : list (k * v)) : list (k * v) :=
  List.filter (fun '(_, v0) => p v0) m.
Arguments filter {v k}.

Definition filterWithKey {k v : Type} (p : k -> v -> bool) (m : list (k * v)) : list (k * v) :=
  List.filter (fun '(k0, v0) => p k0 v0) m.
Arguments filterWithKey {k v}.

Definition findWithDefault {v k : Type} (def : v) (key : k) (m : list (k * v)) : v :=
  match List.find (fun '(k', _) => hydra_eq key k') m with
  | Some (_, v0) => v0
  | None => def
  end.
Arguments findWithDefault {v k}.

Definition fromList {k v : Type} (pairs : list (k * v)) : list (k * v) := pairs.
Arguments fromList {k v}.

Definition insert {k v : Type} (key : k) (val : v) (m : list (k * v)) : list (k * v) :=
  (key, val) :: List.filter (fun '(k', _) => negb (hydra_eq key k')) m.
Arguments insert {k v}.

Definition keys {k v : Type} (m : list (k * v)) : list k :=
  List.map fst m.
Arguments keys {k v}.

Definition lookup {k v : Type} (key : k) (m : list (k * v)) : option v :=
  match List.find (fun '(k', _) => hydra_eq key k') m with
  | Some (_, v0) => Some v0
  | None => None
  end.
Arguments lookup {k v}.

Definition map {k v1 v2 : Type} (f : v1 -> v2) (m : list (k * v1)) : list (k * v2) :=
  List.map (fun '(k0, v0) => (k0, f v0)) m.
Arguments map {k v1 v2}.

Definition mapKeys {k1 k2 v : Type} (f : k1 -> k2) (m : list (k1 * v)) : list (k2 * v) :=
  List.map (fun '(k0, v0) => (f k0, v0)) m.
Arguments mapKeys {k1 k2 v}.

Definition member {k v : Type} (key : k) (m : list (k * v)) : bool :=
  List.existsb (fun '(k', _) => hydra_eq key k') m.
Arguments member {k v}.

Definition null {k v : Type} (m : list (k * v)) : bool :=
  match m with [] => true | _ => false end.
Arguments null {k v}.

Definition singleton {k v : Type} (key : k) (val : v) : list (k * v) := [(key, val)].
Arguments singleton {k v}.

Definition size {k v : Type} (m : list (k * v)) : Z :=
  Z.of_nat (List.length m).
Arguments size {k v}.

Definition toList {k v : Type} (m : list (k * v)) : list (k * v) := m.
Arguments toList {k v}.

Definition union {k v : Type} (m1 m2 : list (k * v)) : list (k * v) :=
  List.fold_right (fun '(k0, v0) acc => insert k0 v0 acc) m2 m1.
Arguments union {k v}.

Close Scope Z_scope.
