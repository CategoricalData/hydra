(* JSON decoding for Hydra terms. Converts JSON Values to Terms using Either for error handling. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.json.model hydra.lib.eithers hydra.lib.equality hydra.lib.lists hydra.lib.literals hydra.lib.logic hydra.lib.maps hydra.lib.maybes hydra.lib.sets hydra.lib.strings hydra.show.core hydra.strip.

Definition expectString : forall (_ : Value) , (sum) (string) (string) := fun (value : Value) => (fun x_ => match x_ with
| Value_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string"%string)
end) (value).
Definition parseSpecialFloat : forall (_ : string) , (option) (Q) := fun (s : string) => (((logic.ifElse) (((logic.or) (((equality.equal) (s)) ("NaN"%string))) (((logic.or) (((equality.equal) (s)) ("Infinity"%string))) (((logic.or) (((equality.equal) (s)) ("-Infinity"%string))) (((equality.equal) (s)) ("-0.0"%string)))))) ((literals.readFloat64) (s))) ((None) : (option) (Q)).
Definition decodeFloat : forall (_ : FloatType) , forall (_ : Value) , (sum) (string) (Term) := fun (ft : FloatType) => fun (value : Value) => (fun x_ => match x_ with
| FloatType_Bigfloat _ => (fun x_ => match x_ with
| Value_Number v_ => (fun (n : Q) => (inr) ((Term_Literal) ((Literal_Float) ((FloatValue_Bigfloat) (n))))) (v_)
| _ => (inl) ("expected number for bigfloat"%string)
end) (value)
| FloatType_Float32 _ => let strResult := (expectString) (value) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (s : string) => let parsed := (literals.readFloat32) (s) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("invalid float32: "%string) ((cons) (s) (nil)))))) (fun (v : Q) => (inr) ((Term_Literal) ((Literal_Float) ((FloatValue_Float32) (v)))))) (parsed))) (strResult)
| FloatType_Float64 _ => (fun x_ => match x_ with
| Value_Number v_ => (fun (n : Q) => (inr) ((Term_Literal) ((Literal_Float) ((FloatValue_Float64) ((literals.bigfloatToFloat64) (n)))))) (v_)
| Value_String v_ => (fun (s : string) => (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("invalid float64 sentinel: "%string) ((cons) (s) (nil)))))) (fun (v : Q) => (inr) ((Term_Literal) ((Literal_Float) ((FloatValue_Float64) (v)))))) ((parseSpecialFloat) (s))) (v_)
| _ => (inl) ("expected number or special float string for float64"%string)
end) (value)
end) (ft).
Definition expectNumber : forall (_ : Value) , (sum) (string) (Q) := fun (value : Value) => (fun x_ => match x_ with
| Value_Number v_ => (fun (n : Q) => (inr) (n)) (v_)
| _ => (inl) ("expected number"%string)
end) (value).
Definition decodeInteger : forall (_ : IntegerType) , forall (_ : Value) , (sum) (string) (Term) := fun (it : IntegerType) => fun (value : Value) => (fun x_ => match x_ with
| IntegerType_Bigint _ => let strResult := (expectString) (value) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (s : string) => let parsed := (literals.readBigint) (s) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("invalid bigint: "%string) ((cons) (s) (nil)))))) (fun (v : Z) => (inr) ((Term_Literal) ((Literal_Integer) ((IntegerValue_Bigint) (v)))))) (parsed))) (strResult)
| IntegerType_Int64 _ => let strResult := (expectString) (value) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (s : string) => let parsed := (literals.readInt64) (s) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("invalid int64: "%string) ((cons) (s) (nil)))))) (fun (v : Z) => (inr) ((Term_Literal) ((Literal_Integer) ((IntegerValue_Int64) (v)))))) (parsed))) (strResult)
| IntegerType_Uint32 _ => let strResult := (expectString) (value) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (s : string) => let parsed := (literals.readUint32) (s) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("invalid uint32: "%string) ((cons) (s) (nil)))))) (fun (v : Z) => (inr) ((Term_Literal) ((Literal_Integer) ((IntegerValue_Uint32) (v)))))) (parsed))) (strResult)
| IntegerType_Uint64 _ => let strResult := (expectString) (value) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (s : string) => let parsed := (literals.readUint64) (s) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("invalid uint64: "%string) ((cons) (s) (nil)))))) (fun (v : Z) => (inr) ((Term_Literal) ((Literal_Integer) ((IntegerValue_Uint64) (v)))))) (parsed))) (strResult)
| IntegerType_Int8 _ => let numResult := (expectNumber) (value) in ((eithers.map) (fun (n : Q) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int8) ((literals.bigintToInt8) ((literals.bigfloatToBigint) (n))))))) (numResult)
| IntegerType_Int16 _ => let numResult := (expectNumber) (value) in ((eithers.map) (fun (n : Q) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int16) ((literals.bigintToInt16) ((literals.bigfloatToBigint) (n))))))) (numResult)
| IntegerType_Int32 _ => let numResult := (expectNumber) (value) in ((eithers.map) (fun (n : Q) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) ((literals.bigintToInt32) ((literals.bigfloatToBigint) (n))))))) (numResult)
| IntegerType_Uint8 _ => let numResult := (expectNumber) (value) in ((eithers.map) (fun (n : Q) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Uint8) ((literals.bigintToUint8) ((literals.bigfloatToBigint) (n))))))) (numResult)
| IntegerType_Uint16 _ => let numResult := (expectNumber) (value) in ((eithers.map) (fun (n : Q) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Uint16) ((literals.bigintToUint16) ((literals.bigfloatToBigint) (n))))))) (numResult)
end) (it).
Definition decodeLiteral : forall (_ : LiteralType) , forall (_ : Value) , (sum) (string) (Term) := fun (lt : LiteralType) => fun (value : Value) => (fun x_ => match x_ with
| LiteralType_Binary _ => let strResult := (expectString) (value) in ((eithers.map) (fun (s : string) => (Term_Literal) ((Literal_Binary) ((literals.stringToBinary) (s))))) (strResult)
| LiteralType_Boolean _ => (fun x_ => match x_ with
| Value_Boolean v_ => (fun (b : bool) => (inr) ((Term_Literal) ((Literal_Boolean) (b)))) (v_)
| _ => (inl) ("expected boolean"%string)
end) (value)
| LiteralType_Decimal _ => (fun x_ => match x_ with
| Value_Number v_ => (fun (n : Q) => (inr) ((Term_Literal) ((Literal_Decimal) ((literals.float64ToDecimal) ((literals.bigfloatToFloat64) (n)))))) (v_)
| _ => (inl) ("expected number for decimal"%string)
end) (value)
| LiteralType_Float v_ => (fun (ft : FloatType) => ((decodeFloat) (ft)) (value)) (v_)
| LiteralType_Integer v_ => (fun (it : IntegerType) => ((decodeInteger) (it)) (value)) (v_)
| LiteralType_String _ => let strResult := (expectString) (value) in ((eithers.map) (fun (s : string) => (Term_Literal) ((Literal_String) (s)))) (strResult)
end) (lt).
Definition expectArray : forall (_ : Value) , (sum) (string) ((list) (Value)) := fun (value : Value) => (fun x_ => match x_ with
| Value_Array v_ => (fun (arr : (list) (Value)) => (inr) (arr)) (v_)
| _ => (inl) ("expected array"%string)
end) (value).
Definition expectObject : forall (_ : Value) , (sum) (string) ((list) ((prod) (string) (Value))) := fun (value : Value) => (fun x_ => match x_ with
| Value_Object v_ => (fun (obj : (list) ((prod) (string) (Value))) => (inr) (obj)) (v_)
| _ => (inl) ("expected object"%string)
end) (value).
Definition fromJson_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : (list) ((prod) (Name) (Type_))) , forall (_ : Name) , forall (_ : Type_) , forall (_ : Value) , (sum) (string) (Term)) =>
    let fromJson := bundle_ in
    fun (types : (list) ((prod) (Name) (Type_))) => fun (tname : Name) => fun (typ : Type_) => fun (value : Value) => let stripped := (deannotateType) (typ) in (fun x_ => match x_ with
| Type__Literal v_ => (fun (lt : LiteralType) => ((decodeLiteral) (lt)) (value)) (v_)
| Type__List v_ => (fun (elemType : Type_) => let arrResult := (expectArray) (value) in let decodeElem := fun (v : Value) => ((((fromJson) (types)) (tname)) (elemType)) (v) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (arr : (list) (Value)) => let decoded := ((eithers.mapList) (decodeElem)) (arr) in ((eithers.map) (fun (ts : (list) (Term)) => (Term_List) (ts))) (decoded))) (arrResult)) (v_)
| Type__Set v_ => (fun (elemType : Type_) => let arrResult := (expectArray) (value) in let decodeElem := fun (v : Value) => ((((fromJson) (types)) (tname)) (elemType)) (v) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (arr : (list) (Value)) => let decoded := ((eithers.mapList) (decodeElem)) (arr) in ((eithers.map) (fun (elems : (list) (Term)) => (Term_Set) ((sets.fromList) (elems)))) (decoded))) (arrResult)) (v_)
| Type__Maybe v_ => (fun (innerType : Type_) => let innerStripped := (deannotateType) (innerType) in let isNestedMaybe := (fun x_ => match x_ with
| Type__Maybe v_ => (fun (_ : Type_) => true) (v_)
| _ => false
end) (innerStripped) in (((logic.ifElse) (isNestedMaybe)) (let decodeJust := fun (arr : (list) (Value)) => ((eithers.map) (fun (v : Term) => (Term_Maybe) ((Some) (v)))) (((((fromJson) (types)) (tname)) (innerType)) ((lists.head) (arr))) in let decodeMaybeArray := fun (arr : (list) (Value)) => let len := (lists.length) (arr) in (((logic.ifElse) (((equality.equal) (len)) ((0)%Z))) ((inr) ((Term_Maybe) ((None) : (option) (Term))))) ((((logic.ifElse) (((equality.equal) (len)) ((1)%Z))) ((decodeJust) (arr))) ((inl) ("expected single-element array for Just"%string))) in (fun x_ => match x_ with
| Value_Null _ => (inr) ((Term_Maybe) ((None) : (option) (Term)))
| Value_Array v_ => (fun (arr : (list) (Value)) => (decodeMaybeArray) (arr)) (v_)
| _ => (inl) ("expected null or single-element array for nested Maybe"%string)
end) (value))) ((fun x_ => match x_ with
| Value_Null _ => (inr) ((Term_Maybe) ((None) : (option) (Term)))
| _ => ((eithers.map) (fun (v : Term) => (Term_Maybe) ((Some) (v)))) (((((fromJson) (types)) (tname)) (innerType)) (value))
end) (value))) (v_)
| Type__Record v_ => (fun (rt : (list) (FieldType)) => let objResult := (expectObject) (value) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (obj : (list) ((prod) (string) (Value))) => let decodeField := fun (ft : FieldType) => let ftype := (fun r_ => (fieldType_type) (r_)) (ft) in let defaultVal := (Value_Null) (tt) in let fname := (fun r_ => (fieldType_name) (r_)) (ft) in let mval := ((maps.lookup) ((fun w_ => w_) (fname))) (obj) in let jsonVal := ((maybes.fromMaybe) (defaultVal)) (mval) in let decoded := ((((fromJson) (types)) (tname)) (ftype)) (jsonVal) in ((eithers.map) (fun (v : Term) => (Build_Field) (fname) (v))) (decoded) in let decodedFields := ((eithers.mapList) (decodeField)) (rt) in ((eithers.map) (fun (fs : (list) (Field)) => (Term_Record) ((Build_Record_) (tname) (fs)))) (decodedFields))) (objResult)) (v_)
| Type__Union v_ => (fun (rt : (list) (FieldType)) => let decodeVariant := fun (key : string) => fun (val : (option) (Value)) => fun (ftype : Type_) => let jsonVal := ((maybes.fromMaybe) ((Value_Null) (tt))) (val) in let decoded := ((((fromJson) (types)) (tname)) (ftype)) (jsonVal) in ((eithers.map) (fun (v : Term) => (Term_Inject) ((Build_Injection) (tname) ((Build_Field) (key) (v))))) (decoded) in let tryField := fun (key : string) => fun (val : (option) (Value)) => fun (ft : FieldType) => (((logic.ifElse) (((equality.equal) ((fun w_ => w_) ((fun r_ => (fieldType_name) (r_)) (ft)))) (key))) ((Some) ((((decodeVariant) (key)) (val)) ((fun r_ => (fieldType_type) (r_)) (ft))))) ((None) : (option) ((sum) (string) (Term))) in let findAndDecode := (hydra_fix) (fun findAndDecode => fun (key : string) => fun (val : (option) (Value)) => fun (fts : (list) (FieldType)) => (((logic.ifElse) ((lists.null) (fts))) ((inl) ((strings.cat) ((cons) ("unknown variant: "%string) ((cons) (key) (nil)))))) ((((maybes.maybe) ((((findAndDecode) (key)) (val)) ((lists.tail) (fts)))) (fun (r : (sum) (string) (Term)) => r)) ((((tryField) (key)) (val)) ((lists.head) (fts))))) in let decodeSingleKey := fun (obj : (list) ((prod) (string) (Value))) => (((findAndDecode) ((lists.head) ((maps.keys) (obj)))) (((maps.lookup) ((lists.head) ((maps.keys) (obj)))) (obj))) (rt) in let objResult := (expectObject) (value) in let processUnion := fun (obj : (list) ((prod) (string) (Value))) => (((logic.ifElse) (((equality.equal) ((lists.length) ((maps.keys) (obj)))) ((1)%Z))) ((decodeSingleKey) (obj))) ((inl) ("expected single-key object for union"%string)) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (obj : (list) ((prod) (string) (Value))) => (processUnion) (obj))) (objResult)) (v_)
| Type__Unit _ => let objResult := (expectObject) (value) in ((eithers.map) (fun (_2 : (list) ((prod) (string) (Value))) => (Term_Unit) (tt))) (objResult)
| Type__Wrap v_ => (fun (wn : Type_) => let decoded := ((((fromJson) (types)) (tname)) (wn)) (value) in ((eithers.map) (fun (v : Term) => (Term_Wrap) ((Build_WrappedTerm) (tname) (v)))) (decoded)) (v_)
| Type__Map v_ => (fun (mt : MapType) => let arrResult := (expectArray) (value) in let keyType := (fun r_ => (mapType_keys) (r_)) (mt) in let valType := (fun r_ => (mapType_values) (r_)) (mt) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (arr : (list) (Value)) => let decodeEntry := fun (entryJson : Value) => let objResult := (expectObject) (entryJson) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (entryObj : (list) ((prod) (string) (Value))) => let keyJson := ((maps.lookup) ("@key"%string)) (entryObj) in let valJson := ((maps.lookup) ("@value"%string)) (entryObj) in (((maybes.maybe) ((inl) ("missing @key in map entry"%string))) (fun (kj : Value) => (((maybes.maybe) ((inl) ("missing @value in map entry"%string))) (fun (vj : Value) => let decodedKey := ((((fromJson) (types)) (tname)) (keyType)) (kj) in let decodedVal := ((((fromJson) (types)) (tname)) (valType)) (vj) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (k : Term) => ((eithers.map) (fun (v : Term) => (pair) (k) (v))) (decodedVal))) (decodedKey))) (valJson))) (keyJson))) (objResult) in let entries := ((eithers.mapList) (decodeEntry)) (arr) in ((eithers.map) (fun (es : (list) ((prod) (Term) (Term))) => (Term_Map) ((maps.fromList) (es)))) (entries))) (arrResult)) (v_)
| Type__Pair v_ => (fun (pt : PairType) => let firstType := (fun r_ => (pairType_first) (r_)) (pt) in let objResult := (expectObject) (value) in let secondType := (fun r_ => (pairType_second) (r_)) (pt) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (obj : (list) ((prod) (string) (Value))) => let firstJson := ((maps.lookup) ("@first"%string)) (obj) in let secondJson := ((maps.lookup) ("@second"%string)) (obj) in (((maybes.maybe) ((inl) ("missing @first in pair"%string))) (fun (fj : Value) => (((maybes.maybe) ((inl) ("missing @second in pair"%string))) (fun (sj : Value) => let decodedFirst := ((((fromJson) (types)) (tname)) (firstType)) (fj) in let decodedSecond := ((((fromJson) (types)) (tname)) (secondType)) (sj) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (f : Term) => ((eithers.map) (fun (s : Term) => (Term_Pair) ((pair) (f) (s)))) (decodedSecond))) (decodedFirst))) (secondJson))) (firstJson))) (objResult)) (v_)
| Type__Either v_ => (fun (et : EitherType) => let leftType := (fun r_ => (eitherType_left) (r_)) (et) in let objResult := (expectObject) (value) in let rightType := (fun r_ => (eitherType_right) (r_)) (et) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (obj : (list) ((prod) (string) (Value))) => let leftJson := ((maps.lookup) ("@left"%string)) (obj) in let rightJson := ((maps.lookup) ("@right"%string)) (obj) in (((maybes.maybe) ((((maybes.maybe) ((inl) ("expected @left or @right in Either"%string))) (fun (rj : Value) => let decoded := ((((fromJson) (types)) (tname)) (rightType)) (rj) in ((eithers.map) (fun (v : Term) => (Term_Either) ((inr) (v)))) (decoded))) (rightJson))) (fun (lj : Value) => let decoded := ((((fromJson) (types)) (tname)) (leftType)) (lj) in ((eithers.map) (fun (v : Term) => (Term_Either) ((inl) (v)))) (decoded))) (leftJson))) (objResult)) (v_)
| Type__Variable v_ => (fun (name : Name) => let lookedUp := ((maps.lookup) (name)) (types) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("unknown type variable: "%string) ((cons) ((fun w_ => w_) (name)) (nil)))))) (fun (resolvedType : Type_) => ((((fromJson) (types)) (name)) (resolvedType)) (value))) (lookedUp)) (v_)
| _ => (inl) ((strings.cat) ((cons) ("unsupported type for JSON decoding: "%string) ((cons) ((hydra.show.core.type) (typ)) (nil))))
end) (stripped)).

Definition fromJson : forall (_ : (list) ((prod) (Name) (Type_))) , forall (_ : Name) , forall (_ : Type_) , forall (_ : Value) , (sum) (string) (Term) :=
  fromJson_bundle.

