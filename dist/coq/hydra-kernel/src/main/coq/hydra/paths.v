(* A model for subterm and subtype access patterns *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core.
Inductive SubtypeStep : Type :=
| SubtypeStep_AnnotatedBody : forall (_ : unit) , SubtypeStep
| SubtypeStep_ApplicationFunction : forall (_ : unit) , SubtypeStep
| SubtypeStep_ApplicationArgument : forall (_ : unit) , SubtypeStep
| SubtypeStep_EitherLeft : forall (_ : unit) , SubtypeStep
| SubtypeStep_EitherRight : forall (_ : unit) , SubtypeStep
| SubtypeStep_ForallBody : forall (_ : unit) , SubtypeStep
| SubtypeStep_FunctionDomain : forall (_ : unit) , SubtypeStep
| SubtypeStep_FunctionCodomain : forall (_ : unit) , SubtypeStep
| SubtypeStep_ListElement : forall (_ : unit) , SubtypeStep
| SubtypeStep_MapKeys : forall (_ : unit) , SubtypeStep
| SubtypeStep_MapValues : forall (_ : unit) , SubtypeStep
| SubtypeStep_MaybeElement : forall (_ : unit) , SubtypeStep
| SubtypeStep_PairFirst : forall (_ : unit) , SubtypeStep
| SubtypeStep_PairSecond : forall (_ : unit) , SubtypeStep
| SubtypeStep_RecordField : forall (_ : Name) , SubtypeStep
| SubtypeStep_SetElement : forall (_ : unit) , SubtypeStep
| SubtypeStep_UnionField : forall (_ : Name) , SubtypeStep
| SubtypeStep_WrappedType : forall (_ : unit) , SubtypeStep.

Definition SubtypePath : Type := (list) (SubtypeStep).

Record SubtypeNode : Type := Build_SubtypeNode {
subtypeNode_name : Name ;
subtypeNode_label : string ;
subtypeNode_id : string ;
}.

Record SubtypeEdge : Type := Build_SubtypeEdge {
subtypeEdge_source : SubtypeNode ;
subtypeEdge_path : SubtypePath ;
subtypeEdge_target : SubtypeNode ;
}.

Record SubtypeGraph : Type := Build_SubtypeGraph {
subtypeGraph_nodes : (list) (SubtypeNode) ;
subtypeGraph_edges : (list) (SubtypeEdge) ;
}.

Inductive SubtermStep : Type :=
| SubtermStep_AnnotatedBody : forall (_ : unit) , SubtermStep
| SubtermStep_ApplicationFunction : forall (_ : unit) , SubtermStep
| SubtermStep_ApplicationArgument : forall (_ : unit) , SubtermStep
| SubtermStep_LambdaBody : forall (_ : unit) , SubtermStep
| SubtermStep_UnionCasesDefault : forall (_ : unit) , SubtermStep
| SubtermStep_UnionCasesBranch : forall (_ : Name) , SubtermStep
| SubtermStep_LetBody : forall (_ : unit) , SubtermStep
| SubtermStep_LetBinding : forall (_ : Name) , SubtermStep
| SubtermStep_ListElement : forall (_ : Z) , SubtermStep
| SubtermStep_MapKey : forall (_ : Z) , SubtermStep
| SubtermStep_MapValue : forall (_ : Z) , SubtermStep
| SubtermStep_MaybeTerm : forall (_ : unit) , SubtermStep
| SubtermStep_ProductTerm : forall (_ : Z) , SubtermStep
| SubtermStep_RecordField : forall (_ : Name) , SubtermStep
| SubtermStep_SetElement : forall (_ : Z) , SubtermStep
| SubtermStep_SumTerm : forall (_ : unit) , SubtermStep
| SubtermStep_TypeLambdaBody : forall (_ : unit) , SubtermStep
| SubtermStep_TypeApplicationTerm : forall (_ : unit) , SubtermStep
| SubtermStep_InjectionTerm : forall (_ : unit) , SubtermStep
| SubtermStep_WrappedTerm : forall (_ : unit) , SubtermStep.

Definition SubtermPath : Type := (list) (SubtermStep).

Record SubtermNode : Type := Build_SubtermNode {
subtermNode_name : Name ;
subtermNode_label : string ;
subtermNode_id : string ;
}.

Record SubtermEdge : Type := Build_SubtermEdge {
subtermEdge_source : SubtermNode ;
subtermEdge_path : SubtermPath ;
subtermEdge_target : SubtermNode ;
}.

Record SubtermGraph : Type := Build_SubtermGraph {
subtermGraph_nodes : (list) (SubtermNode) ;
subtermGraph_edges : (list) (SubtermEdge) ;
}.

