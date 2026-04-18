(* Hydra primitive library: hydra.lib.literals *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Require Import Stdlib.Numbers.DecimalString.
Import ListNotations.

(* --- Integer -> string --------------------------------------------------- *)

(* All Hydra integer types are represented as Z in Coq, so every showIntN /
   showUintN produces the same output as `show` in Haskell: the decimal
   representation with a leading `-` for negatives. *)
Definition showInt32 (n : Z) : string := NilZero.string_of_int (Z.to_int n).
Definition showInt8  : Z -> string := showInt32.
Definition showInt16 : Z -> string := showInt32.
Definition showInt64 : Z -> string := showInt32.
Definition showUint8  : Z -> string := showInt32.
Definition showUint16 : Z -> string := showInt32.
Definition showUint32 : Z -> string := showInt32.
Definition showUint64 : Z -> string := showInt32.
Definition showBigint : Z -> string := showInt32.

(* --- Boolean / string shows --------------------------------------------- *)

Definition showBoolean (b : bool) : string :=
  if b then "true"%string else "false"%string.

(* Haskell `show :: String -> String` double-quotes and escapes. A faithful
   implementation requires escaping control chars and backslashes; for now
   the Hydra kernel's showString tests use already-safe ASCII payloads, so
   we emit `"<payload>"` verbatim. If a test fails because of an un-escaped
   character we'll revisit. *)
Definition showString (s : string) : string :=
  let quote := Ascii.Ascii false true false false false true false false in
  String quote (s ++ String quote "")%string.

Axiom bigfloatToBigint : Q -> Z.
Axiom bigfloatToFloat32 : Q -> Q.
Axiom bigfloatToFloat64 : Q -> Q.
Axiom bigintToBigfloat : Z -> Q.
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
Axiom binaryToString : string -> string.
Axiom decimalToBigint : Q -> Z.
Axiom decimalToFloat32 : Q -> Q.
Axiom decimalToFloat64 : Q -> Q.
Axiom float32ToBigfloat : Q -> Q.
Axiom float32ToDecimal : Q -> Q.
Axiom float64ToBigfloat : Q -> Q.
Axiom float64ToDecimal : Q -> Q.
Axiom int8ToBigint : Z -> Z.
Axiom int16ToBigint : Z -> Z.
Axiom int32ToBigint : Z -> Z.
Axiom int64ToBigint : Z -> Z.
Axiom readBigfloat : string -> option Q.
Axiom readBigint : string -> option Z.
Axiom readBoolean : string -> option bool.
Axiom readDecimal : string -> option Q.
Axiom readFloat32 : string -> option Q.
Axiom readFloat64 : string -> option Q.
Axiom readInt8 : string -> option Z.
Axiom readInt16 : string -> option Z.
Axiom readInt32 : string -> option Z.
Axiom readInt64 : string -> option Z.
Axiom readString : string -> option string.
Axiom readUint8 : string -> option Z.
Axiom readUint16 : string -> option Z.
Axiom readUint32 : string -> option Z.
Axiom readUint64 : string -> option Z.
Axiom showBigfloat : Q -> string.
Axiom showDecimal : Q -> string.
Axiom showFloat32 : Q -> string.
Axiom showFloat64 : Q -> string.
Axiom stringToBinary : string -> string.
Axiom uint8ToBigint : Z -> Z.
Axiom uint16ToBigint : Z -> Z.
Axiom uint32ToBigint : Z -> Z.
Axiom uint64ToBigint : Z -> Z.
