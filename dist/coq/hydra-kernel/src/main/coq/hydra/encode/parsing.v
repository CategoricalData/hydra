(* Term encoders for hydra.parsing *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.parsing.

Definition parseSuccess (t0 : Type) : (t0 -> Term) -> (ParseSuccess) (t0) -> Term :=
  fun (a : t0 -> Term) => fun (x : (ParseSuccess) (t0)) => (Term_Record) ((Build_Record_) ("ParseSuccess"%string) ((cons) ((Build_Field) ("value"%string) ((a) ((fun r_ => (parseSuccess_value) (r_)) (x)))) ((cons) ((Build_Field) ("remainder"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun r_ => (parseSuccess_remainder) (r_)) (x)))) (nil)))).
Arguments parseSuccess {t0}.
Definition parseError : ParseError -> Term :=
  fun (x : ParseError) => (Term_Record) ((Build_Record_) ("ParseError"%string) ((cons) ((Build_Field) ("message"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun r_ => (parseError_message) (r_)) (x)))) ((cons) ((Build_Field) ("remainder"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun r_ => (parseError_remainder) (r_)) (x)))) (nil)))).
Definition parseResult (t0 : Type) : (t0 -> Term) -> (ParseResult) (t0) -> Term :=
  fun (a : t0 -> Term) => fun x_ => match x_ with
| ParseResult_Success v_ => (fun (y : (ParseSuccess) (t0)) => (Term_Inject) ((Build_Injection) ("ParseResult"%string) ((Build_Field) ("success"%string) (((parseSuccess) (a)) (y))))) (v_)
| ParseResult_Failure v_ => (fun (y : ParseError) => (Term_Inject) ((Build_Injection) ("ParseResult"%string) ((Build_Field) ("failure"%string) ((parseError) (y))))) (v_)
end.
Arguments parseResult {t0}.

