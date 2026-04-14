(* Variant types which describe the structure of Hydra core types and terms. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.
Inductive TypeVariant : Type :=
| TypeVariant_Annotated : unit -> TypeVariant
| TypeVariant_Application : unit -> TypeVariant
| TypeVariant_Either : unit -> TypeVariant
| TypeVariant_Forall : unit -> TypeVariant
| TypeVariant_Function : unit -> TypeVariant
| TypeVariant_List : unit -> TypeVariant
| TypeVariant_Literal : unit -> TypeVariant
| TypeVariant_Map : unit -> TypeVariant
| TypeVariant_Maybe : unit -> TypeVariant
| TypeVariant_Pair : unit -> TypeVariant
| TypeVariant_Record : unit -> TypeVariant
| TypeVariant_Set : unit -> TypeVariant
| TypeVariant_Union : unit -> TypeVariant
| TypeVariant_Unit : unit -> TypeVariant
| TypeVariant_Variable : unit -> TypeVariant
| TypeVariant_Void : unit -> TypeVariant
| TypeVariant_Wrap : unit -> TypeVariant.

Inductive TermVariant : Type :=
| TermVariant_Annotated : unit -> TermVariant
| TermVariant_Application : unit -> TermVariant
| TermVariant_Cases : unit -> TermVariant
| TermVariant_Either : unit -> TermVariant
| TermVariant_Inject : unit -> TermVariant
| TermVariant_Lambda : unit -> TermVariant
| TermVariant_Let : unit -> TermVariant
| TermVariant_List : unit -> TermVariant
| TermVariant_Literal : unit -> TermVariant
| TermVariant_Map : unit -> TermVariant
| TermVariant_Maybe : unit -> TermVariant
| TermVariant_Pair : unit -> TermVariant
| TermVariant_Project : unit -> TermVariant
| TermVariant_Record : unit -> TermVariant
| TermVariant_Set : unit -> TermVariant
| TermVariant_TypeApplication : unit -> TermVariant
| TermVariant_TypeLambda : unit -> TermVariant
| TermVariant_Unit : unit -> TermVariant
| TermVariant_Unwrap : unit -> TermVariant
| TermVariant_Variable : unit -> TermVariant
| TermVariant_Wrap : unit -> TermVariant.

Inductive LiteralVariant : Type :=
| LiteralVariant_Binary : unit -> LiteralVariant
| LiteralVariant_Boolean : unit -> LiteralVariant
| LiteralVariant_Float : unit -> LiteralVariant
| LiteralVariant_Integer : unit -> LiteralVariant
| LiteralVariant_String : unit -> LiteralVariant.

Inductive FunctionVariant : Type :=
| FunctionVariant_Elimination : unit -> FunctionVariant
| FunctionVariant_Lambda : unit -> FunctionVariant.

Inductive EliminationVariant : Type :=
| EliminationVariant_Record : unit -> EliminationVariant
| EliminationVariant_Union : unit -> EliminationVariant
| EliminationVariant_Wrap : unit -> EliminationVariant.

