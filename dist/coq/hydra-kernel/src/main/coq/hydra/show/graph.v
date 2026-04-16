(* String representations of hydra.graph types *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.lib.lists hydra.lib.strings hydra.show.core.

Definition graph_ : forall (_ : (list) (Binding)) , string := fun (elements : (list) (Binding)) => let elementStrs := ((lists.map) (hydra.show.core.binding)) (elements) in (strings.cat) ((cons) ("{"%string) ((cons) (((strings.intercalate) (", "%string)) (elementStrs)) ((cons) ("}"%string) (nil)))).

