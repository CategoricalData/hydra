(* Hydra primitive library: hydra.lib.literals *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Import ListNotations.

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
Axiom showBigint : Z -> string.
Axiom showBoolean : bool -> string.
Axiom showDecimal : Q -> string.
Axiom showFloat32 : Q -> string.
Axiom showFloat64 : Q -> string.
Axiom showInt8 : Z -> string.
Axiom showInt16 : Z -> string.
Axiom showInt32 : Z -> string.
Axiom showInt64 : Z -> string.
Axiom showString : string -> string.
Axiom showUint8 : Z -> string.
Axiom showUint16 : Z -> string.
Axiom showUint32 : Z -> string.
Axiom showUint64 : Z -> string.
Axiom stringToBinary : string -> string.
Axiom uint8ToBigint : Z -> Z.
Axiom uint16ToBigint : Z -> Z.
Axiom uint32ToBigint : Z -> Z.
Axiom uint64ToBigint : Z -> Z.
