(* Hydra primitive library: hydra.lib.regex *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Import ListNotations.

Axiom find : string -> string -> option string.
Axiom findAll : string -> string -> list string.
Axiom matches : string -> string -> bool.
Axiom replace : string -> string -> string -> string.
Axiom replaceAll : string -> string -> string -> string.
Axiom split : string -> string -> list string.
