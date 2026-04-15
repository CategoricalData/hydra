(* String representations of hydra.typing types *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.lib.lists hydra.lib.maps hydra.lib.pairs hydra.lib.strings hydra.show.core hydra.typing.

Definition typeConstraint : forall (_ : TypeConstraint) , string := fun (tc : TypeConstraint) => let ltyp := (fun r_ => (typeConstraint_left) (r_)) (tc) in let rtyp := (fun r_ => (typeConstraint_right) (r_)) (tc) in (strings.cat) ((cons) ((hydra.show.core.type) (ltyp)) ((cons) ("≡"%string) ((cons) ((hydra.show.core.type) (rtyp)) (nil)))).
Definition typeSubst : forall (_ : TypeSubst) , string := fun (ts : TypeSubst) => let subst := (fun w_ => w_) (ts) in let pairs := (maps.toList) (subst) in let showPair := fun (pair_ : (prod) (Name) (Type_)) => let name := (fun w_ => w_) ((pairs.first) (pair_)) in let typ := (pairs.second) (pair_) in (strings.cat) ((cons) (name) ((cons) ("↦"%string) ((cons) ((hydra.show.core.type) (typ)) (nil)))) in let pairStrs := ((lists.map) (showPair)) (pairs) in (strings.cat) ((cons) ("{"%string) ((cons) (((strings.intercalate) (","%string)) (pairStrs)) ((cons) ("}"%string) (nil)))).

