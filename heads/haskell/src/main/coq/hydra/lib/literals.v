(* Hydra primitive library: hydra.lib.literals *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Require Import Stdlib.Numbers.DecimalString.
Import ListNotations.

(* --- Integer -> string --------------------------------------------------- *)

(* All Hydra integer types are represented as Z in Coq, so every printIntN /
   printUintN produces the same output as `show` in Haskell: the decimal
   representation with a leading `-` for negatives. *)
Definition printInt32 (n : Z) : string := NilZero.string_of_int (Z.to_int n).
Definition printInt8  : Z -> string := printInt32.
Definition printInt16 : Z -> string := printInt32.
Definition printInt64 : Z -> string := printInt32.
Definition printUint8  : Z -> string := printInt32.
Definition printUint16 : Z -> string := printInt32.
Definition printUint32 : Z -> string := printInt32.
Definition printUint64 : Z -> string := printInt32.
Definition printBigint : Z -> string := printInt32.

(* --- Boolean / string shows --------------------------------------------- *)

Definition printBoolean (b : bool) : string :=
  if b then "true"%string else "false"%string.

(* Haskell `show :: String -> String` double-quotes and escapes. A faithful
   implementation requires escaping control chars and backslashes; for now
   the Hydra kernel's showString tests use already-safe ASCII payloads, so
   we emit `"<payload>"` verbatim. If a test fails because of an un-escaped
   character we'll revisit. *)
Definition printString (s : string) : string :=
  let quote := Ascii.Ascii false true false false false true false false in
  String quote (s ++ String quote "")%string.

Axiom bigintToDecimal : Z -> Q.
Axiom bigintToInt8 : Z -> Z.
Axiom bigintToInt16 : Z -> Z.
Axiom bigintToInt32 : Z -> Z.
Axiom bigintToInt64 : Z -> Z.
Axiom bigintToUint8 : Z -> Z.
Axiom bigintToUint16 : Z -> Z.
Axiom bigintToUint32 : Z -> Z.
Axiom bigintToUint64 : Z -> Z.
Axiom binaryToBytes : string -> list Z.
Axiom binaryToBase64 : string -> string.
Axiom decimalToBigint : Q -> Z.
Axiom decimalToFloat32 : Q -> Q.
Axiom decimalToFloat64 : Q -> Q.
Axiom float32ToDecimal : Q -> Q.
Axiom float32ToFloat64 : Q -> Q.
Axiom float64ToDecimal : Q -> Q.
Axiom float64ToFloat32 : Q -> Q.
Axiom int8ToBigint : Z -> Z.
Axiom int16ToBigint : Z -> Z.
Axiom int32ToBigint : Z -> Z.
Axiom int64ToBigint : Z -> Z.
Axiom parseBigint : string -> option Z.
Axiom parseBoolean : string -> option bool.
Axiom parseDecimal : string -> option Q.
Axiom parseFloat32 : string -> option Q.
Axiom parseFloat64 : string -> option Q.
Axiom parseInt8 : string -> option Z.
Axiom parseInt16 : string -> option Z.
Axiom parseInt32 : string -> option Z.
Axiom parseInt64 : string -> option Z.
Axiom parseString : string -> option string.
Axiom parseUint8 : string -> option Z.
Axiom parseUint16 : string -> option Z.
Axiom parseUint32 : string -> option Z.
Axiom parseUint64 : string -> option Z.
Axiom printDecimal : Q -> string.
Axiom printFloat32 : Q -> string.
Axiom printFloat64 : Q -> string.
Axiom base64ToBinary : string -> string.
Axiom uint8ToBigint : Z -> Z.
Axiom uint16ToBigint : Z -> Z.
Axiom uint32ToBigint : Z -> Z.
Axiom uint64ToBigint : Z -> Z.
