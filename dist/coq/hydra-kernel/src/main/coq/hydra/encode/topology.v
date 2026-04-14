(* Term encoders for hydra.topology *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.topology hydra.lib.maps hydra.lib.lists hydra.lib.sets.

Definition vertex : forall (_ : Z) , Term := fun (x : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (x))).
Definition tarjanState : forall (_ : TarjanState) , Term := fun (x : TarjanState) => (Term_Record) ((Build_Record_) ("TarjanState"%string) ((cons) ((Build_Field) ("counter"%string) ((fun (x2 : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (x2)))) ((fun r_ => (tarjanState_counter) (r_)) (x)))) ((cons) ((Build_Field) ("indices"%string) ((fun (m : (list) ((prod) (Z) (Z))) => (Term_Map) ((((maps.bimap) (vertex)) (fun (x2 : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (x2))))) (m))) ((fun r_ => (tarjanState_indices) (r_)) (x)))) ((cons) ((Build_Field) ("lowLinks"%string) ((fun (m : (list) ((prod) (Z) (Z))) => (Term_Map) ((((maps.bimap) (vertex)) (fun (x2 : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (x2))))) (m))) ((fun r_ => (tarjanState_lowLinks) (r_)) (x)))) ((cons) ((Build_Field) ("stack"%string) ((fun (xs : (list) (Z)) => (Term_List) (((lists.map) (vertex)) (xs))) ((fun r_ => (tarjanState_stack) (r_)) (x)))) ((cons) ((Build_Field) ("onStack"%string) ((fun (s : (list) (Z)) => (Term_Set) (((sets.map) (vertex)) (s))) ((fun r_ => (tarjanState_onStack) (r_)) (x)))) ((cons) ((Build_Field) ("sccs"%string) ((fun (xs : (list) ((list) (Z))) => (Term_List) (((lists.map) (fun (xs2 : (list) (Z)) => (Term_List) (((lists.map) (vertex)) (xs2)))) (xs))) ((fun r_ => (tarjanState_sccs) (r_)) (x)))) (nil)))))))).
Definition graph_ : forall (_ : (list) ((prod) (Z) ((list) (Z)))) , Term := fun (m : (list) ((prod) (Z) ((list) (Z)))) => (Term_Map) ((((maps.bimap) (vertex)) (fun (xs : (list) (Z)) => (Term_List) (((lists.map) (vertex)) (xs)))) (m)).

