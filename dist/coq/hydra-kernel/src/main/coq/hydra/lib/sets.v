(* Hydra primitive library: hydra.lib.sets *)

(* Sets are represented as deduplicated lists *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Require Import hydra.lib.base.
Import ListNotations.

Open Scope Z_scope.

Definition contains {x : Type} (v : x) (s : list x) : bool :=
  List.existsb (fun y => hydra_eq v y) s.

Definition delete {x : Type} (v : x) (s : list x) : list x :=
  List.filter (fun y => negb (hydra_eq v y)) s.
Arguments delete {x}.

Definition difference {x : Type} (s1 s2 : list x) : list x :=
  List.filter (fun v => negb (contains v s2)) s1.
Arguments difference {x}.

Definition empty {x : Type} : list x := [].
Arguments empty {x}.

Fixpoint nubAux {x : Type} (seen : list x) (xs : list x) : list x :=
  match xs with
  | [] => []
  | h :: t =>
      if List.existsb (fun y => hydra_eq h y) seen
      then nubAux seen t
      else h :: nubAux (h :: seen) t
  end.

Definition fromList {x : Type} (xs : list x) : list x := nubAux [] xs.
Arguments fromList {x}.

Definition insert {x : Type} (v : x) (s : list x) : list x :=
  if contains v s then s else v :: s.
Arguments insert {x}.

Definition intersection {x : Type} (s1 s2 : list x) : list x :=
  List.filter (fun v => contains v s2) s1.
Arguments intersection {x}.

Definition map {x y : Type} (f : x -> y) (s : list x) : list y :=
  nubAux [] (List.map f s).
Arguments map {x y}.

Definition member {x : Type} (v : x) (s : list x) : bool :=
  contains v s.
Arguments member {x}.

Definition null {x : Type} (s : list x) : bool :=
  match s with [] => true | _ => false end.
Arguments null {x}.

Definition singleton {x : Type} (v : x) : list x := [v].
Arguments singleton {x}.

Definition size {x : Type} (s : list x) : Z :=
  Z.of_nat (List.length s).
Arguments size {x}.

Definition toList {x : Type} (s : list x) : list x := s.
Arguments toList {x}.

Definition union {x : Type} (s1 s2 : list x) : list x :=
  List.fold_right (fun v acc => if List.existsb (fun y => hydra_eq v y) acc then acc else v :: acc) s2 s1.
Arguments union {x}.

Definition unions {x : Type} (ss : list (list x)) : list x :=
  List.fold_right union [] ss.
Arguments unions {x}.

Close Scope Z_scope.
