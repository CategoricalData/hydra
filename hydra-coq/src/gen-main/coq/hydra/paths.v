(* A model for subterm and subtype access patterns *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core.
Inductive SubtypeStep : Type :=
| SubtypeStep_AnnotatedBody : unit -> SubtypeStep
| SubtypeStep_ApplicationFunction : unit -> SubtypeStep
| SubtypeStep_ApplicationArgument : unit -> SubtypeStep
| SubtypeStep_EitherLeft : unit -> SubtypeStep
| SubtypeStep_EitherRight : unit -> SubtypeStep
| SubtypeStep_ForallBody : unit -> SubtypeStep
| SubtypeStep_FunctionDomain : unit -> SubtypeStep
| SubtypeStep_FunctionCodomain : unit -> SubtypeStep
| SubtypeStep_ListElement : unit -> SubtypeStep
| SubtypeStep_MapKeys : unit -> SubtypeStep
| SubtypeStep_MapValues : unit -> SubtypeStep
| SubtypeStep_MaybeElement : unit -> SubtypeStep
| SubtypeStep_PairFirst : unit -> SubtypeStep
| SubtypeStep_PairSecond : unit -> SubtypeStep
| SubtypeStep_RecordField : Name -> SubtypeStep
| SubtypeStep_SetElement : unit -> SubtypeStep
| SubtypeStep_UnionField : Name -> SubtypeStep
| SubtypeStep_WrappedType : unit -> SubtypeStep.

Definition SubtypePath : Type :=
  (list) (SubtypeStep).

Record SubtypeNode : Type := Build_SubtypeNode {
  subtypeNode_name : Name ;
  subtypeNode_label : string ;
  subtypeNode_id : string
}.

Record SubtypeEdge : Type := Build_SubtypeEdge {
  subtypeEdge_source : SubtypeNode ;
  subtypeEdge_path : SubtypePath ;
  subtypeEdge_target : SubtypeNode
}.

Record SubtypeGraph : Type := Build_SubtypeGraph {
  subtypeGraph_nodes : (list) (SubtypeNode) ;
  subtypeGraph_edges : (list) (SubtypeEdge)
}.

Inductive SubtermStep : Type :=
| SubtermStep_AnnotatedBody : unit -> SubtermStep
| SubtermStep_ApplicationFunction : unit -> SubtermStep
| SubtermStep_ApplicationArgument : unit -> SubtermStep
| SubtermStep_LambdaBody : unit -> SubtermStep
| SubtermStep_UnionCasesDefault : unit -> SubtermStep
| SubtermStep_UnionCasesBranch : Name -> SubtermStep
| SubtermStep_LetBody : unit -> SubtermStep
| SubtermStep_LetBinding : Name -> SubtermStep
| SubtermStep_ListElement : Z -> SubtermStep
| SubtermStep_MapKey : Z -> SubtermStep
| SubtermStep_MapValue : Z -> SubtermStep
| SubtermStep_MaybeTerm : unit -> SubtermStep
| SubtermStep_ProductTerm : Z -> SubtermStep
| SubtermStep_RecordField : Name -> SubtermStep
| SubtermStep_SetElement : Z -> SubtermStep
| SubtermStep_SumTerm : unit -> SubtermStep
| SubtermStep_TypeLambdaBody : unit -> SubtermStep
| SubtermStep_TypeApplicationTerm : unit -> SubtermStep
| SubtermStep_InjectionTerm : unit -> SubtermStep
| SubtermStep_WrappedTerm : unit -> SubtermStep.

Definition SubtermPath : Type :=
  (list) (SubtermStep).

Record SubtermNode : Type := Build_SubtermNode {
  subtermNode_name : Name ;
  subtermNode_label : string ;
  subtermNode_id : string
}.

Record SubtermEdge : Type := Build_SubtermEdge {
  subtermEdge_source : SubtermNode ;
  subtermEdge_path : SubtermPath ;
  subtermEdge_target : SubtermNode
}.

Record SubtermGraph : Type := Build_SubtermGraph {
  subtermGraph_nodes : (list) (SubtermNode) ;
  subtermGraph_edges : (list) (SubtermEdge)
}.

