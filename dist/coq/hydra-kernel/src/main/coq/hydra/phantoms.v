(* Phantom types for use with Hydra DSLs *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core.
Definition TTerm (a : Type) : Type := Term.

Record TBinding (a : Type) : Type := Build_TBinding {
tBinding_name : Name ;
tBinding_term : (TTerm) (a) ;
}.

Record TTermDefinition (a : Type) : Type := Build_TTermDefinition {
tTermDefinition_name : Name ;
tTermDefinition_term : (TTerm) (a) ;
}.

Arguments Build_TBinding {a}.
Arguments tBinding_name {a}.
Arguments tBinding_term {a}.
Arguments Build_TTermDefinition {a}.
Arguments tTermDefinition_name {a}.
Arguments tTermDefinition_term {a}.

