(* Language constraints for Hydra Core *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.coders hydra.core hydra.lib.sets hydra.reflect hydra.variants.

Definition hydraLanguage : Language := let eliminationVariants := (sets.fromList) (eliminationVariants) in let floatTypes := (sets.fromList) (floatTypes) in let functionVariants := (sets.fromList) (functionVariants) in let integerTypes := (sets.fromList) (integerTypes) in let literalVariants := (sets.fromList) (literalVariants) in let termVariants := (sets.fromList) (termVariants) in let typeVariants := (sets.fromList) (typeVariants) in let types := fun (t : Type_) => (fun x_ => match x_ with
| _ => true
end) (t) in (Build_Language) ("hydra.core"%string) ((Build_LanguageConstraints) (eliminationVariants) (literalVariants) (floatTypes) (functionVariants) (integerTypes) (termVariants) (typeVariants) (types)).

