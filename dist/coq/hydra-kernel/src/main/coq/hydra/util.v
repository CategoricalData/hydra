(* General-purpose utility types used across Hydra. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.
Inductive CaseConvention : Type :=
| CaseConvention_Camel : forall (_ : unit) , CaseConvention
| CaseConvention_Pascal : forall (_ : unit) , CaseConvention
| CaseConvention_LowerSnake : forall (_ : unit) , CaseConvention
| CaseConvention_UpperSnake : forall (_ : unit) , CaseConvention.

Inductive Comparison : Type :=
| Comparison_LessThan : forall (_ : unit) , Comparison
| Comparison_EqualTo : forall (_ : unit) , Comparison
| Comparison_GreaterThan : forall (_ : unit) , Comparison.

Inductive Precision : Type :=
| Precision_Arbitrary : forall (_ : unit) , Precision
| Precision_Bits : forall (_ : Z) , Precision.

