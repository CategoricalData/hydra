(* A model for simple graphs as adjacency lists *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.
Definition Vertex : Type :=
  Z.

Record TarjanState : Type := Build_TarjanState {
  tarjanState_counter : Z ;
  tarjanState_indices : (list) ((prod) (Vertex) (Z)) ;
  tarjanState_lowLinks : (list) ((prod) (Vertex) (Z)) ;
  tarjanState_stack : (list) (Vertex) ;
  tarjanState_onStack : (list) (Vertex) ;
  tarjanState_sccs : (list) ((list) (Vertex))
}.

Record OrderingIsomorphism (a : Type) : Type := Build_OrderingIsomorphism {
  orderingIsomorphism_encode : (list) (a) -> (list) (a) ;
  orderingIsomorphism_decode : (list) (a) -> (list) (a)
}.

Definition Graph : Type :=
  (list) ((prod) (Vertex) ((list) (Vertex))).

Arguments Build_OrderingIsomorphism {a}.
Arguments orderingIsomorphism_encode {a}.
Arguments orderingIsomorphism_decode {a}.

