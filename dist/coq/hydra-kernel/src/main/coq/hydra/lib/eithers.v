(* Hydra primitive library: hydra.lib.eithers *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Import ListNotations.

Definition bimap (x y z w : Type) (f : x -> z) (g : y -> w) (e : x + y) : z + w :=
  match e with
  | inl a => inl (f a)
  | inr b => inr (g b)
  end.
Arguments bimap {x y z w}.

Definition bind (x y z : Type) (e : x + y) (f : y -> x + z) : x + z :=
  match e with
  | inl a => inl a
  | inr b => f b
  end.
Arguments bind {x y z}.

Definition either (x y z : Type) (f : x -> z) (g : y -> z) (e : x + y) : z :=
  match e with
  | inl a => f a
  | inr b => g b
  end.
Arguments either {x y z}.

Fixpoint foldl (x y z : Type) (f : x -> y -> z + x) (acc : x) (xs : list y) : z + x :=
  match xs with
  | [] => inr acc
  | h :: t =>
    match f acc h with
    | inl err => inl err
    | inr acc' => foldl x y z f acc' t
    end
  end.
Arguments foldl {x y z}.

Definition fromLeft (x y : Type) (def : x) (e : x + y) : x :=
  match e with
  | inl a => a
  | inr _ => def
  end.
Arguments fromLeft {x y}.

Definition fromRight (x y : Type) (def : y) (e : x + y) : y :=
  match e with
  | inl _ => def
  | inr b => b
  end.
Arguments fromRight {x y}.

Definition isLeft (x y : Type) (e : x + y) : bool :=
  match e with
  | inl _ => true
  | inr _ => false
  end.
Arguments isLeft {x y}.

Definition isRight (x y : Type) (e : x + y) : bool :=
  match e with
  | inl _ => false
  | inr _ => true
  end.
Arguments isRight {x y}.

Definition lefts (x y : Type) (es : list (x + y)) : list x :=
  List.flat_map (fun e => match e with inl a => [a] | inr _ => [] end) es.
Arguments lefts {x y}.

Definition map (x y z : Type) (f : x -> y) (e : z + x) : z + y :=
  match e with
  | inl a => inl a
  | inr b => inr (f b)
  end.
Arguments map {x y z}.

Fixpoint mapList (x y z : Type) (f : x -> z + y) (xs : list x) : z + list y :=
  match xs with
  | [] => inr []
  | h :: t =>
    match f h with
    | inl err => inl err
    | inr v =>
      match mapList x y z f t with
      | inl err => inl err
      | inr vs => inr (v :: vs)
      end
    end
  end.
Arguments mapList {x y z}.

Definition mapMaybe (x y z : Type) (f : x -> z + y) (mx : option x) : z + option y :=
  match mx with
  | None => inr None
  | Some v =>
    match f v with
    | inl err => inl err
    | inr w => inr (Some w)
    end
  end.
Arguments mapMaybe {x y z}.

Definition mapSet (x y z : Type) (f : x -> z + y) (xs : list x) : z + list y :=
  mapList f xs.
Arguments mapSet {x y z}.

Fixpoint partitionEithers (x y : Type) (es : list (x + y)) : list x * list y :=
  match es with
  | [] => ([], [])
  | h :: t =>
    let '(ls, rs) := partitionEithers x y t in
    match h with
    | inl a => (a :: ls, rs)
    | inr b => (ls, b :: rs)
    end
  end.
Arguments partitionEithers {x y}.

Definition rights (x y : Type) (es : list (x + y)) : list y :=
  List.flat_map (fun e => match e with inl _ => [] | inr b => [b] end) es.
Arguments rights {x y}.
