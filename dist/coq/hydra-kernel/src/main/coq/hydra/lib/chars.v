(* Hydra primitive library: hydra.lib.chars *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.

Open Scope Z_scope.

Definition isUpper (c : Z) : bool :=
  (65 <=? c) && (c <=? 90).

Definition isLower (c : Z) : bool :=
  (97 <=? c) && (c <=? 122).

Definition isAlphaNum (c : Z) : bool :=
  isUpper c || isLower c || ((48 <=? c) && (c <=? 57)).

Definition isSpace (c : Z) : bool :=
  (c =? 32) || (c =? 9) || (c =? 10) || (c =? 13) || (c =? 12) || (c =? 11).

Definition toLower (c : Z) : Z :=
  if isUpper c then c + 32 else c.

Definition toUpper (c : Z) : Z :=
  if isLower c then c - 32 else c.

Close Scope Z_scope.
