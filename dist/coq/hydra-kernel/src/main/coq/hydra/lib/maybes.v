(* Hydra primitive library: hydra.lib.maybes *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Require Import hydra.lib.base.
Import ListNotations.

Definition apply (x y : Type) (mf : option (x -> y)) (mx : option x) : option y :=
  match mf with
  | Some f => match mx with
              | Some v => Some (f v)
              | None => None
              end
  | None => None
  end.
Arguments apply {x y}.

Definition bind (x y : Type) (mx : option x) (f : x -> option y) : option y :=
  match mx with
  | Some v => f v
  | None => None
  end.
Arguments bind {x y}.

Definition cases (x y : Type) (mx : option x) (def : y) (f : x -> y) : y :=
  match mx with
  | Some v => f v
  | None => def
  end.
Arguments cases {x y}.

Definition cat (x : Type) (opts : list (option x)) : list x :=
  List.flat_map (fun o => match o with Some v => [v] | None => [] end) opts.
Arguments cat {x}.

Definition compose (x y z : Type) (f : x -> option y) (g : y -> option z) (v : x) : option z :=
  match f v with
  | Some w => g w
  | None => None
  end.
Arguments compose {x y z}.

Definition fromJust (x : Type) (mx : option x) : x :=
  match mx with
  | Some v => v
  | None => hydra_unreachable
  end.
Arguments fromJust {x}.

Definition fromMaybe (x : Type) (def : x) (mx : option x) : x :=
  match mx with
  | Some v => v
  | None => def
  end.
Arguments fromMaybe {x}.

Definition isJust (x : Type) (mx : option x) : bool :=
  match mx with
  | Some _ => true
  | None => false
  end.
Arguments isJust {x}.

Definition isNothing (x : Type) (mx : option x) : bool :=
  match mx with
  | Some _ => false
  | None => true
  end.
Arguments isNothing {x}.

Definition map (x y : Type) (f : x -> y) (mx : option x) : option y :=
  match mx with
  | Some v => Some (f v)
  | None => None
  end.
Arguments map {x y}.

Definition mapMaybe (x y : Type) (f : x -> option y) (xs : list x) : list y :=
  List.flat_map (fun v => match f v with Some w => [w] | None => [] end) xs.
Arguments mapMaybe {x y}.

Definition maybe (y x : Type) (def : y) (f : x -> y) (mx : option x) : y :=
  match mx with
  | Some v => f v
  | None => def
  end.
Arguments maybe {y x}.

Definition pure (x : Type) (v : x) : option x := Some v.
Arguments pure {x}.

Definition toList (x : Type) (mx : option x) : list x :=
  match mx with
  | Some v => [v]
  | None => []
  end.
Arguments toList {x}.
