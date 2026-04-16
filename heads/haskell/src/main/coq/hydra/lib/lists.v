(* Hydra primitive library: hydra.lib.lists *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.
Import ListNotations.

Open Scope Z_scope.

(* ----- Non-equality-dependent definitions ----- *)

Definition apply {x y : Type} (fs : list (x -> y)) (xs : list x) : list y :=
  List.flat_map (fun f => List.map f xs) fs.
Arguments apply {x y}.

(* Helper: nat-indexed nth with unreachable default. *)
Fixpoint nthUnreachable {x : Type} (n : nat) (xs : list x) : x :=
  match xs with
  | [] => hydra_unreachable
  | h :: t => match n with
              | O => h
              | S n' => nthUnreachable n' t
              end
  end.

Definition at_ {x : Type} (i : Z) (xs : list x) : x :=
  nthUnreachable (Z.to_nat i) xs.
Arguments at_ {x}.

Definition bind {x y : Type} (xs : list x) (f : x -> list y) : list y :=
  List.flat_map f xs.
Arguments bind {x y}.

Definition concat {x : Type} (xss : list (list x)) : list x :=
  List.concat xss.
Arguments concat {x}.

Definition concat2 {x : Type} (xs ys : list x) : list x := xs ++ ys.
Arguments concat2 {x}.

Definition cons_ {x : Type} (h : x) (t : list x) : list x := h :: t.
Arguments cons_ {x}.
(* Expose under the Hydra primitive name. Coq's `cons` is the list constructor. *)
Notation cons := cons_ (only parsing).

Fixpoint dropNat {x : Type} (n : nat) (xs : list x) : list x :=
  match n, xs with
  | O, _ => xs
  | _, [] => []
  | S n', _ :: t => dropNat n' t
  end.

Definition drop {x : Type} (n : Z) (xs : list x) : list x :=
  dropNat (Z.to_nat n) xs.
Arguments drop {x}.

Fixpoint dropWhile {x : Type} (p : x -> bool) (xs : list x) : list x :=
  match xs with
  | [] => []
  | h :: t => if p h then dropWhile p t else xs
  end.
Arguments dropWhile {x}.

Definition filter {x : Type} (p : x -> bool) (xs : list x) : list x :=
  List.filter p xs.
Arguments filter {x}.

Fixpoint find {x : Type} (p : x -> bool) (xs : list x) : option x :=
  match xs with
  | [] => None
  | h :: t => if p h then Some h else find p t
  end.
Arguments find {x}.

Definition foldl {y x : Type} (f : y -> x -> y) (init : y) (xs : list x) : y :=
  List.fold_left f xs init.
Arguments foldl {y x}.

Definition foldr {x y : Type} (f : x -> y -> y) (init : y) (xs : list x) : y :=
  List.fold_right f init xs.
Arguments foldr {x y}.

Definition head {x : Type} (xs : list x) : x :=
  match xs with
  | [] => hydra_unreachable
  | h :: _ => h
  end.
Arguments head {x}.

Fixpoint initAux {x : Type} (h : x) (t : list x) : list x :=
  match t with
  | [] => []
  | h' :: t' => h :: initAux h' t'
  end.

Definition init {x : Type} (xs : list x) : list x :=
  match xs with
  | [] => hydra_unreachable
  | h :: t => initAux h t
  end.
Arguments init {x}.

Fixpoint intersperse {x : Type} (sep : x) (xs : list x) : list x :=
  match xs with
  | [] => []
  | [a] => [a]
  | a :: rest => a :: sep :: intersperse sep rest
  end.
Arguments intersperse {x}.

Definition intercalate {x : Type} (sep : list x) (xss : list (list x)) : list x :=
  List.concat (intersperse sep xss).
Arguments intercalate {x}.

Fixpoint lastAux {x : Type} (h : x) (t : list x) : x :=
  match t with
  | [] => h
  | h' :: t' => lastAux h' t'
  end.

Definition last {x : Type} (xs : list x) : x :=
  match xs with
  | [] => hydra_unreachable
  | h :: t => lastAux h t
  end.
Arguments last {x}.

Definition length {x : Type} (xs : list x) : Z :=
  Z.of_nat (List.length xs).
Arguments length {x}.

Definition map {x y : Type} (f : x -> y) (xs : list x) : list y :=
  List.map f xs.
Arguments map {x y}.

Fixpoint maybeAtNat {x : Type} (n : nat) (xs : list x) : option x :=
  match xs with
  | [] => None
  | h :: t => match n with
              | O => Some h
              | S n' => maybeAtNat n' t
              end
  end.

Definition maybeAt {x : Type} (i : Z) (xs : list x) : option x :=
  if Z.ltb i 0 then None else maybeAtNat (Z.to_nat i) xs.
Arguments maybeAt {x}.

Definition maybeHead {x : Type} (xs : list x) : option x :=
  match xs with
  | [] => None
  | h :: _ => Some h
  end.
Arguments maybeHead {x}.

Definition maybeInit {x : Type} (xs : list x) : option (list x) :=
  match xs with
  | [] => None
  | h :: t => Some (initAux h t)
  end.
Arguments maybeInit {x}.

Definition maybeLast {x : Type} (xs : list x) : option x :=
  match xs with
  | [] => None
  | h :: t => Some (lastAux h t)
  end.
Arguments maybeLast {x}.

Definition maybeTail {x : Type} (xs : list x) : option (list x) :=
  match xs with
  | [] => None
  | _ :: t => Some t
  end.
Arguments maybeTail {x}.

Definition null {x : Type} (xs : list x) : bool :=
  match xs with [] => true | _ => false end.
Arguments null {x}.

Definition partition {x : Type} (p : x -> bool) (xs : list x) : (list x * list x) :=
  List.partition p xs.
Arguments partition {x}.

Definition pure {x : Type} (v : x) : list x := [v].
Arguments pure {x}.

Fixpoint replicateNat {x : Type} (n : nat) (v : x) : list x :=
  match n with
  | O => []
  | S n' => v :: replicateNat n' v
  end.

Definition replicate {x : Type} (n : Z) (v : x) : list x :=
  replicateNat (Z.to_nat n) v.
Arguments replicate {x}.

Definition reverse {x : Type} (xs : list x) : list x :=
  List.rev xs.
Arguments reverse {x}.

Definition safeHead {x : Type} (xs : list x) : option x :=
  maybeHead xs.
Arguments safeHead {x}.

Definition singleton {x : Type} (v : x) : list x := [v].
Arguments singleton {x}.

Fixpoint spanAux {x : Type} (p : x -> bool) (xs : list x) : (list x * list x) :=
  match xs with
  | [] => ([], [])
  | h :: t =>
      if p h then
        let (a, b) := spanAux p t in (h :: a, b)
      else ([], xs)
  end.

Definition span {x : Type} (p : x -> bool) (xs : list x) : (list x * list x) :=
  spanAux p xs.
Arguments span {x}.

Definition tail {x : Type} (xs : list x) : list x :=
  match xs with
  | [] => hydra_unreachable
  | _ :: t => t
  end.
Arguments tail {x}.

Fixpoint takeNat {x : Type} (n : nat) (xs : list x) : list x :=
  match n, xs with
  | O, _ => []
  | _, [] => []
  | S n', h :: t => h :: takeNat n' t
  end.

Definition take {x : Type} (n : Z) (xs : list x) : list x :=
  takeNat (Z.to_nat n) xs.
Arguments take {x}.

Fixpoint zip {x y : Type} (xs : list x) (ys : list y) : list (x * y) :=
  match xs, ys with
  | a :: xs', b :: ys' => (a, b) :: zip xs' ys'
  | _, _ => []
  end.
Arguments zip {x y}.

Fixpoint zipWith {x y z : Type} (f : x -> y -> z) (xs : list x) (ys : list y) : list z :=
  match xs, ys with
  | a :: xs', b :: ys' => f a b :: zipWith f xs' ys'
  | _, _ => []
  end.
Arguments zipWith {x y z}.

(* ----- Equality-/order-dependent definitions -----

   Hydra's type system has no Eq/Ord classes; primitives like `elem`, `nub`,
   `sort`, `group`, and `sortOn` take values of unconstrained type `x`.
   Coq cannot provide decidable equality for arbitrary types, so these
   primitives defer to two runtime axioms declared in `hydra.lib.base`:

     hydra_eq      : forall X, X -> X -> bool
     hydra_compare : forall X, X -> X -> comparison

   Test cases that exercise these primitives over concrete types will stall
   at the axiom and fail to reduce. They can be unblocked per-type by
   providing rewrite lemmas that specialise `hydra_eq` to a concrete
   decidable-equality function, or by redefining the primitive for the
   specific test fixture. *)

Definition elem {x : Type} (v : x) (xs : list x) : bool :=
  List.existsb (fun y => hydra_eq v y) xs.
Arguments elem {x}.

Fixpoint groupAux {x : Type} (curHead : x) (curAcc : list x) (xs : list x) : list (list x) :=
  match xs with
  | [] => [List.rev (curHead :: curAcc)]
  | h :: t =>
      if hydra_eq curHead h
      then groupAux curHead (curHead :: curAcc) t
      else List.rev (curHead :: curAcc) :: groupAux h [] t
  end.

Definition group {x : Type} (xs : list x) : list (list x) :=
  match xs with
  | [] => []
  | h :: t => groupAux h [] t
  end.
Arguments group {x}.

Fixpoint nubAux {x : Type} (seen : list x) (xs : list x) : list x :=
  match xs with
  | [] => []
  | h :: t =>
      if List.existsb (fun y => hydra_eq h y) seen
      then nubAux seen t
      else h :: nubAux (h :: seen) t
  end.

Definition nub {x : Type} (xs : list x) : list x := nubAux [] xs.
Arguments nub {x}.

(* Insertion sort using hydra_compare. Structural on the input list. *)
Fixpoint insertBy {x : Type} (cmp : x -> x -> comparison) (v : x) (xs : list x) : list x :=
  match xs with
  | [] => [v]
  | h :: t => match cmp v h with
              | Lt | Eq => v :: xs
              | Gt => h :: insertBy cmp v t
              end
  end.

Fixpoint sortBy {x : Type} (cmp : x -> x -> comparison) (xs : list x) : list x :=
  match xs with
  | [] => []
  | h :: t => insertBy cmp h (sortBy cmp t)
  end.

Definition sort {x : Type} (xs : list x) : list x :=
  sortBy (@hydra_compare x) xs.
Arguments sort {x}.

Definition sortOn {x y : Type} (f : x -> y) (xs : list x) : list x :=
  sortBy (fun a b => hydra_compare (f a) (f b)) xs.
Arguments sortOn {x y}.

(* ----- transpose ----- *)

(* Coq cannot express `transpose` as a direct structural Fixpoint over a list
   of lists because the decreasing measure is the "head column count", not
   the outer list. We use `hydra_fix` to sidestep the termination checker. *)
Definition transpose {x : Type} : list (list x) -> list (list x) :=
  hydra_fix (fun (rec : list (list x) -> list (list x)) (xss : list (list x)) =>
    match xss with
    | [] => []
    | [] :: rest => rec rest
    | (h :: t) :: rest =>
        let heads := h :: List.map (fun ys => match ys with [] => hydra_unreachable | a :: _ => a end)
                                   (List.filter (fun ys => match ys with [] => false | _ => true end) rest) in
        let tails := t :: List.map (fun ys => match ys with [] => [] | _ :: b => b end)
                                   (List.filter (fun ys => match ys with [] => false | _ => true end) rest) in
        heads :: rec tails
    end).
Arguments transpose {x}.

Close Scope Z_scope.
