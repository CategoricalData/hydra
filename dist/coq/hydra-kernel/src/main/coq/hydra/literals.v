(* Conversion functions for literal values. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.lib.literals.

Definition integerValueToBigint : forall (_ : IntegerValue) , Z := fun x_ => match x_ with
| IntegerValue_Bigint v_ => (fun (bi : Z) => bi) (v_)
| IntegerValue_Int8 v_ => (fun (i8 : Z) => (literals.int8ToBigint) (i8)) (v_)
| IntegerValue_Int16 v_ => (fun (i16 : Z) => (literals.int16ToBigint) (i16)) (v_)
| IntegerValue_Int32 v_ => (fun (i32 : Z) => (literals.int32ToBigint) (i32)) (v_)
| IntegerValue_Int64 v_ => (fun (i64 : Z) => (literals.int64ToBigint) (i64)) (v_)
| IntegerValue_Uint8 v_ => (fun (ui8 : Z) => (literals.uint8ToBigint) (ui8)) (v_)
| IntegerValue_Uint16 v_ => (fun (ui16 : Z) => (literals.uint16ToBigint) (ui16)) (v_)
| IntegerValue_Uint32 v_ => (fun (ui32 : Z) => (literals.uint32ToBigint) (ui32)) (v_)
| IntegerValue_Uint64 v_ => (fun (ui64 : Z) => (literals.uint64ToBigint) (ui64)) (v_)
end.
Definition floatValueToBigfloat : forall (_ : FloatValue) , Q := fun x_ => match x_ with
| FloatValue_Bigfloat v_ => (fun (bf : Q) => bf) (v_)
| FloatValue_Float32 v_ => (fun (f32 : Q) => (literals.float32ToBigfloat) (f32)) (v_)
| FloatValue_Float64 v_ => (fun (f64 : Q) => (literals.float64ToBigfloat) (f64)) (v_)
end.
Definition bigintToIntegerValue : forall (_ : IntegerType) , forall (_ : Z) , IntegerValue := fun (it : IntegerType) => fun (bi : Z) => (fun x_ => match x_ with
| IntegerType_Bigint _ => (IntegerValue_Bigint) (bi)
| IntegerType_Int8 _ => (IntegerValue_Int8) ((literals.bigintToInt8) (bi))
| IntegerType_Int16 _ => (IntegerValue_Int16) ((literals.bigintToInt16) (bi))
| IntegerType_Int32 _ => (IntegerValue_Int32) ((literals.bigintToInt32) (bi))
| IntegerType_Int64 _ => (IntegerValue_Int64) ((literals.bigintToInt64) (bi))
| IntegerType_Uint8 _ => (IntegerValue_Uint8) ((literals.bigintToUint8) (bi))
| IntegerType_Uint16 _ => (IntegerValue_Uint16) ((literals.bigintToUint16) (bi))
| IntegerType_Uint32 _ => (IntegerValue_Uint32) ((literals.bigintToUint32) (bi))
| IntegerType_Uint64 _ => (IntegerValue_Uint64) ((literals.bigintToUint64) (bi))
end) (it).
Definition bigfloatToFloatValue : forall (_ : FloatType) , forall (_ : Q) , FloatValue := fun (ft : FloatType) => fun (bf : Q) => (fun x_ => match x_ with
| FloatType_Bigfloat _ => (FloatValue_Bigfloat) (bf)
| FloatType_Float32 _ => (FloatValue_Float32) ((literals.bigfloatToFloat32) (bf))
| FloatType_Float64 _ => (FloatValue_Float64) ((literals.bigfloatToFloat64) (bf))
end) (ft).

