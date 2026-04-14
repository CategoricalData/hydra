(* Variant types which describe the structure of Hydra core types and terms. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.
Inductive TypeVariant : Type :=
| TypeVariant_Annotated : forall (_ : unit) , TypeVariant
| TypeVariant_Application : forall (_ : unit) , TypeVariant
| TypeVariant_Either : forall (_ : unit) , TypeVariant
| TypeVariant_Forall : forall (_ : unit) , TypeVariant
| TypeVariant_Function : forall (_ : unit) , TypeVariant
| TypeVariant_List : forall (_ : unit) , TypeVariant
| TypeVariant_Literal : forall (_ : unit) , TypeVariant
| TypeVariant_Map : forall (_ : unit) , TypeVariant
| TypeVariant_Maybe : forall (_ : unit) , TypeVariant
| TypeVariant_Pair : forall (_ : unit) , TypeVariant
| TypeVariant_Record : forall (_ : unit) , TypeVariant
| TypeVariant_Set : forall (_ : unit) , TypeVariant
| TypeVariant_Union : forall (_ : unit) , TypeVariant
| TypeVariant_Unit : forall (_ : unit) , TypeVariant
| TypeVariant_Variable : forall (_ : unit) , TypeVariant
| TypeVariant_Void : forall (_ : unit) , TypeVariant
| TypeVariant_Wrap : forall (_ : unit) , TypeVariant.

Inductive TermVariant : Type :=
| TermVariant_Annotated : forall (_ : unit) , TermVariant
| TermVariant_Application : forall (_ : unit) , TermVariant
| TermVariant_Cases : forall (_ : unit) , TermVariant
| TermVariant_Either : forall (_ : unit) , TermVariant
| TermVariant_Inject : forall (_ : unit) , TermVariant
| TermVariant_Lambda : forall (_ : unit) , TermVariant
| TermVariant_Let : forall (_ : unit) , TermVariant
| TermVariant_List : forall (_ : unit) , TermVariant
| TermVariant_Literal : forall (_ : unit) , TermVariant
| TermVariant_Map : forall (_ : unit) , TermVariant
| TermVariant_Maybe : forall (_ : unit) , TermVariant
| TermVariant_Pair : forall (_ : unit) , TermVariant
| TermVariant_Project : forall (_ : unit) , TermVariant
| TermVariant_Record : forall (_ : unit) , TermVariant
| TermVariant_Set : forall (_ : unit) , TermVariant
| TermVariant_TypeApplication : forall (_ : unit) , TermVariant
| TermVariant_TypeLambda : forall (_ : unit) , TermVariant
| TermVariant_Unit : forall (_ : unit) , TermVariant
| TermVariant_Unwrap : forall (_ : unit) , TermVariant
| TermVariant_Variable : forall (_ : unit) , TermVariant
| TermVariant_Wrap : forall (_ : unit) , TermVariant.

Inductive LiteralVariant : Type :=
| LiteralVariant_Binary : forall (_ : unit) , LiteralVariant
| LiteralVariant_Boolean : forall (_ : unit) , LiteralVariant
| LiteralVariant_Float : forall (_ : unit) , LiteralVariant
| LiteralVariant_Integer : forall (_ : unit) , LiteralVariant
| LiteralVariant_String : forall (_ : unit) , LiteralVariant.

Inductive FunctionVariant : Type :=
| FunctionVariant_Elimination : forall (_ : unit) , FunctionVariant
| FunctionVariant_Lambda : forall (_ : unit) , FunctionVariant.

Inductive EliminationVariant : Type :=
| EliminationVariant_Record : forall (_ : unit) , EliminationVariant
| EliminationVariant_Union : forall (_ : unit) , EliminationVariant
| EliminationVariant_Wrap : forall (_ : unit) , EliminationVariant.

