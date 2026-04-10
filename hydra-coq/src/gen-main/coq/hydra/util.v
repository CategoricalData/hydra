(* General-purpose utility types used across Hydra. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.
Inductive Precision : Type :=
| Precision_Arbitrary : unit -> Precision
| Precision_Bits : Z -> Precision.

Inductive Comparison : Type :=
| Comparison_LessThan : unit -> Comparison
| Comparison_EqualTo : unit -> Comparison
| Comparison_GreaterThan : unit -> Comparison.

Inductive CaseConvention : Type :=
| CaseConvention_Camel : unit -> CaseConvention
| CaseConvention_Pascal : unit -> CaseConvention
| CaseConvention_LowerSnake : unit -> CaseConvention
| CaseConvention_UpperSnake : unit -> CaseConvention.

