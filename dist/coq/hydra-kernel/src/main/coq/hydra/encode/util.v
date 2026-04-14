(* Term encoders for hydra.util *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.util hydra.core.

Definition precision : forall (_ : Precision) , Term := fun x_ => match x_ with
| Precision_Arbitrary v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("Precision"%string) ((Build_Field) ("arbitrary"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| Precision_Bits v_ => (fun (y : Z) => (Term_Inject) ((Build_Injection) ("Precision"%string) ((Build_Field) ("bits"%string) ((fun (x : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (x)))) (y))))) (v_)
end.
Definition comparison : forall (_ : Comparison) , Term := fun x_ => match x_ with
| Comparison_LessThan v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("Comparison"%string) ((Build_Field) ("lessThan"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| Comparison_EqualTo v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("Comparison"%string) ((Build_Field) ("equalTo"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| Comparison_GreaterThan v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("Comparison"%string) ((Build_Field) ("greaterThan"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.
Definition caseConvention : forall (_ : CaseConvention) , Term := fun x_ => match x_ with
| CaseConvention_Camel v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("CaseConvention"%string) ((Build_Field) ("camel"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| CaseConvention_Pascal v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("CaseConvention"%string) ((Build_Field) ("pascal"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| CaseConvention_LowerSnake v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("CaseConvention"%string) ((Build_Field) ("lowerSnake"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| CaseConvention_UpperSnake v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("CaseConvention"%string) ((Build_Field) ("upperSnake"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.

