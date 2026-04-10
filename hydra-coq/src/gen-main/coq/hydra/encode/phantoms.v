(* Term encoders for hydra.phantoms *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.phantoms hydra.core hydra.encode.core.

Definition tTerm (t0 : Type) (t1 : Type) : t0 -> (TTerm) (t1) -> Term :=
  fun (a : t0) => fun (x : (TTerm) (t1)) => (Term_Wrap) ((Build_WrappedTerm) ("TTerm"%string) ((hydra.encode.core.term) ((fun w_ => w_) (x)))).
Arguments tTerm {t0} {t1}.
Definition tTermDefinition (t0 : Type) (t1 : Type) : t0 -> (TTermDefinition) (t1) -> Term :=
  fun (a : t0) => fun (x : (TTermDefinition) (t1)) => (Term_Record) ((Build_Record_) ("TTermDefinition"%string) ((cons) ((Build_Field) ("name"%string) ((hydra.encode.core.name) ((fun r_ => (tTermDefinition_name) (r_)) (x)))) ((cons) ((Build_Field) ("term"%string) (((tTerm) (a)) ((fun r_ => (tTermDefinition_term) (r_)) (x)))) (nil)))).
Arguments tTermDefinition {t0} {t1}.
Definition tBinding (t0 : Type) (t1 : Type) : t0 -> (TBinding) (t1) -> Term :=
  fun (a : t0) => fun (x : (TBinding) (t1)) => (Term_Record) ((Build_Record_) ("TBinding"%string) ((cons) ((Build_Field) ("name"%string) ((hydra.encode.core.name) ((fun r_ => (tBinding_name) (r_)) (x)))) ((cons) ((Build_Field) ("term"%string) (((tTerm) (a)) ((fun r_ => (tBinding_term) (r_)) (x)))) (nil)))).
Arguments tBinding {t0} {t1}.

