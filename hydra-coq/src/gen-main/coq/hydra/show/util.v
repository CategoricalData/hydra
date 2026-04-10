(* String representations of hydra.util types *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.util.

Definition comparison : Comparison -> string :=
  fun (c : Comparison) => (fun x_ => match x_ with
| Comparison_LessThan _ => "lessThan"%string
| Comparison_EqualTo _ => "equalTo"%string
| Comparison_GreaterThan _ => "greaterThan"%string
end) (c).
Definition caseConvention : CaseConvention -> string :=
  fun (c : CaseConvention) => (fun x_ => match x_ with
| CaseConvention_LowerSnake _ => "lower_snake_case"%string
| CaseConvention_UpperSnake _ => "UPPER_SNAKE_CASE"%string
| CaseConvention_Camel _ => "camelCase"%string
| CaseConvention_Pascal _ => "PascalCase"%string
end) (c).

