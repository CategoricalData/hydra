(* Note: this is an automatically generated file. Do not edit. *)

(* Hydra primitive library: hydra.lib.strings *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Import ListNotations.

Axiom cat : list string -> string.
Axiom cat2 : string -> string -> string.
Axiom charAt : Z -> string -> Z.
Axiom fromList : list Z -> string.
Axiom intercalate : string -> list string -> string.
Axiom length : string -> Z.
Axiom lines : string -> list string.
Axiom maybeCharAt : Z -> string -> option Z.
Axiom null : string -> bool.
Axiom splitOn : string -> string -> list string.
Axiom toList : string -> list Z.
Axiom toLower : string -> string.
Axiom toUpper : string -> string.
Axiom unlines : list string -> string.
