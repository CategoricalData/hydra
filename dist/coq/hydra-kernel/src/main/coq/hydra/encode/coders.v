(* Term encoders for hydra.coders *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.coders hydra.core.

Definition traversalOrder : TraversalOrder -> Term :=
  fun x_ => match x_ with
| TraversalOrder_Pre v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("TraversalOrder"%string) ((Build_Field) ("pre"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| TraversalOrder_Post v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("TraversalOrder"%string) ((Build_Field) ("post"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.
Definition languageName : LanguageName -> Term :=
  fun (x : LanguageName) => (Term_Wrap) ((Build_WrappedTerm) ("LanguageName"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun w_ => w_) (x)))).
Definition coderDirection : CoderDirection -> Term :=
  fun x_ => match x_ with
| CoderDirection_Encode v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("CoderDirection"%string) ((Build_Field) ("encode"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| CoderDirection_Decode v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("CoderDirection"%string) ((Build_Field) ("decode"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.

