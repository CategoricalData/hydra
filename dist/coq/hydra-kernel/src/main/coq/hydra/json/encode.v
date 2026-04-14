(* JSON encoding for Hydra terms. Converts Terms to JSON Values using Either for error handling. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.lib.logic hydra.lib.equality hydra.core hydra.json.model hydra.lib.literals hydra.strip hydra.lib.eithers hydra.lib.sets hydra.lib.maybes hydra.lib.maps hydra.lib.pairs hydra.lib.strings hydra.show.core hydra.lib.lists.

Definition isSpecialFloatString : string -> bool :=
  fun (s : string) => ((logic.or) (((equality.equal) (s)) ("NaN"%string))) (((logic.or) (((equality.equal) (s)) ("Infinity"%string))) (((logic.or) (((equality.equal) (s)) ("-Infinity"%string))) (((equality.equal) (s)) ("-0.0"%string)))).
Definition encodeInteger (t0 : Type) : IntegerValue -> (sum) (t0) (Value) :=
  fun (iv : IntegerValue) => (fun x_ => match x_ with
| IntegerValue_Bigint v_ => (fun (bi : Z) => (inr) ((Value_String) ((literals.showBigint) (bi)))) (v_)
| IntegerValue_Int64 v_ => (fun (i : Z) => (inr) ((Value_String) ((literals.showInt64) (i)))) (v_)
| IntegerValue_Uint32 v_ => (fun (i : Z) => (inr) ((Value_String) ((literals.showUint32) (i)))) (v_)
| IntegerValue_Uint64 v_ => (fun (i : Z) => (inr) ((Value_String) ((literals.showUint64) (i)))) (v_)
| IntegerValue_Int8 v_ => (fun (i : Z) => (inr) ((Value_Number) ((literals.bigintToBigfloat) ((literals.int8ToBigint) (i))))) (v_)
| IntegerValue_Int16 v_ => (fun (i : Z) => (inr) ((Value_Number) ((literals.bigintToBigfloat) ((literals.int16ToBigint) (i))))) (v_)
| IntegerValue_Int32 v_ => (fun (i : Z) => (inr) ((Value_Number) ((literals.bigintToBigfloat) ((literals.int32ToBigint) (i))))) (v_)
| IntegerValue_Uint8 v_ => (fun (i : Z) => (inr) ((Value_Number) ((literals.bigintToBigfloat) ((literals.uint8ToBigint) (i))))) (v_)
| IntegerValue_Uint16 v_ => (fun (i : Z) => (inr) ((Value_Number) ((literals.bigintToBigfloat) ((literals.uint16ToBigint) (i))))) (v_)
end) (iv).
Arguments encodeInteger {t0}.
Definition encodeFloat (t0 : Type) : FloatValue -> (sum) (t0) (Value) :=
  fun (fv : FloatValue) => (fun x_ => match x_ with
| FloatValue_Bigfloat v_ => (fun (bf : Q) => let s := (literals.showBigfloat) (bf) in (((logic.ifElse) ((isSpecialFloatString) (s))) ((inr) ((Value_String) (s)))) ((inr) ((Value_Number) (bf)))) (v_)
| FloatValue_Float32 v_ => (fun (f : Q) => (inr) ((Value_String) ((literals.showFloat32) (f)))) (v_)
| FloatValue_Float64 v_ => (fun (f : Q) => let s := (literals.showFloat64) (f) in (((logic.ifElse) ((isSpecialFloatString) (s))) ((inr) ((Value_String) (s)))) ((inr) ((Value_Number) ((literals.float64ToBigfloat) (f))))) (v_)
end) (fv).
Arguments encodeFloat {t0}.
Definition encodeLiteral (t0 : Type) : Literal -> (sum) (t0) (Value) :=
  fun (lit : Literal) => (fun x_ => match x_ with
| Literal_Binary v_ => (fun (b : string) => (inr) ((Value_String) ((literals.binaryToString) (b)))) (v_)
| Literal_Boolean v_ => (fun (b : bool) => (inr) ((Value_Boolean) (b))) (v_)
| Literal_Float v_ => (fun (f : FloatValue) => (encodeFloat) (f)) (v_)
| Literal_Integer v_ => (fun (i : IntegerValue) => (encodeInteger) (i)) (v_)
| Literal_String v_ => (fun (s : string) => (inr) ((Value_String) (s))) (v_)
end) (lit).
Arguments encodeLiteral {t0}.
Definition toJsonUntyped_bundle :=
  hydra_fix (fun (bundle_ : Term -> (sum) (string) (Value)) =>
    let toJsonUntyped := bundle_ in
    fun (term_ : Term) => let stripped := (deannotateTerm) (term_) in (fun x_ => match x_ with
| Term_Literal v_ => (fun (lit : Literal) => (encodeLiteral) (lit)) (v_)
| Term_List v_ => (fun (terms : (list) (Term)) => let results := ((eithers.mapList) (fun (t : Term) => (toJsonUntyped) (t))) (terms) in ((eithers.map) (fun (vs : (list) (Value)) => (Value_Array) (vs))) (results)) (v_)
| Term_Set v_ => (fun (vals : (list) (Term)) => let terms := (sets.toList) (vals) in let results := ((eithers.mapList) (fun (t : Term) => (toJsonUntyped) (t))) (terms) in ((eithers.map) (fun (vs : (list) (Value)) => (Value_Array) (vs))) (results)) (v_)
| Term_Maybe v_ => (fun (opt : (option) (Term)) => (((maybes.maybe) ((inr) ((Value_Null) (tt)))) (fun (v : Term) => let encodedMaybe := (toJsonUntyped) (v) in ((eithers.map) (fun (encoded : Value) => (Value_Array) ((cons) (encoded) (nil)))) (encodedMaybe))) (opt)) (v_)
| Term_Record v_ => (fun (r : Record_) => let fields := (fun r_ => (record__fields) (r_)) (r) in let encodeField := fun (f : Field) => let fterm := (fun r_ => (field_term) (r_)) (f) in let fname := (fun w_ => w_) ((fun r_ => (field_name) (r_)) (f)) in let encodedField := (toJsonUntyped) (fterm) in ((eithers.map) (fun (v : Value) => (pair) (fname) (v))) (encodedField) in let encodedFields := ((eithers.mapList) (encodeField)) (fields) in ((eithers.map) (fun (fs : (list) ((prod) (string) (Value))) => (Value_Object) ((maps.fromList) (fs)))) (encodedFields)) (v_)
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun w_ => w_) ((fun r_ => (field_name) (r_)) (field)) in let fterm := (fun r_ => (field_term) (r_)) (field) in let encodedUnion := (toJsonUntyped) (fterm) in ((eithers.map) (fun (v : Value) => (Value_Object) ((maps.fromList) ((cons) ((pair) (fname) (v)) (nil))))) (encodedUnion)) (v_)
| Term_Unit _ => (inr) ((Value_Object) (maps.empty))
| Term_Wrap v_ => (fun (wt : WrappedTerm) => (toJsonUntyped) ((fun r_ => (wrappedTerm_body) (r_)) (wt))) (v_)
| Term_Map v_ => (fun (m : (list) ((prod) (Term) (Term))) => let encodeEntry := fun (kv : (prod) (Term) (Term)) => let v := (pairs.second) (kv) in let k := (pairs.first) (kv) in let encodedV := (toJsonUntyped) (v) in let encodedK := (toJsonUntyped) (k) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (ek : Value) => ((eithers.map) (fun (ev : Value) => (Value_Object) ((maps.fromList) ((cons) ((pair) ("@key"%string) (ek)) ((cons) ((pair) ("@value"%string) (ev)) (nil)))))) (encodedV))) (encodedK) in let entries := ((eithers.mapList) (encodeEntry)) ((maps.toList) (m)) in ((eithers.map) (fun (es : (list) (Value)) => (Value_Array) (es))) (entries)) (v_)
| Term_Pair v_ => (fun (p : (prod) (Term) (Term)) => let second := (pairs.second) (p) in let first := (pairs.first) (p) in let encodedSecond := (toJsonUntyped) (second) in let encodedFirst := (toJsonUntyped) (first) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (ef : Value) => ((eithers.map) (fun (es : Value) => (Value_Object) ((maps.fromList) ((cons) ((pair) ("@first"%string) (ef)) ((cons) ((pair) ("@second"%string) (es)) (nil)))))) (encodedSecond))) (encodedFirst)) (v_)
| Term_Either v_ => (fun (e : (sum) (Term) (Term)) => (((eithers.either) (fun (l : Term) => let encodedL := (toJsonUntyped) (l) in ((eithers.map) (fun (v : Value) => (Value_Object) ((maps.fromList) ((cons) ((pair) ("@left"%string) (v)) (nil))))) (encodedL))) (fun (r : Term) => let encodedR := (toJsonUntyped) (r) in ((eithers.map) (fun (v : Value) => (Value_Object) ((maps.fromList) ((cons) ((pair) ("@right"%string) (v)) (nil))))) (encodedR))) (e)) (v_)
| _ => (inl) ((strings.cat) ((cons) ("unsupported term variant for JSON encoding: "%string) ((cons) ((hydra.show.core.term) (term_)) (nil))))
end) (stripped)).

Definition toJsonUntyped : Term -> (sum) (string) (Value) :=
  toJsonUntyped_bundle.
Definition toJson_bundle :=
  hydra_fix (fun (bundle_ : (list) ((prod) (Name) (Type_)) -> Name -> Type_ -> Term -> (sum) (string) (Value)) =>
    let toJson := bundle_ in
    fun (types : (list) ((prod) (Name) (Type_))) => fun (tname : Name) => fun (typ : Type_) => fun (term_ : Term) => let strippedTerm := (deannotateTerm) (term_) in let stripped := (deannotateType) (typ) in (fun x_ => match x_ with
| Type__Literal v_ => (fun (_ : LiteralType) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (lit : Literal) => (encodeLiteral) (lit)) (v_)
| _ => (inl) ("expected literal term"%string)
end) (strippedTerm)) (v_)
| Type__List v_ => (fun (elemType : Type_) => (fun x_ => match x_ with
| Term_List v_ => (fun (terms : (list) (Term)) => let results := ((eithers.mapList) (fun (t : Term) => ((((toJson) (types)) (tname)) (elemType)) (t))) (terms) in ((eithers.map) (fun (vs : (list) (Value)) => (Value_Array) (vs))) (results)) (v_)
| _ => (inl) ("expected list term"%string)
end) (strippedTerm)) (v_)
| Type__Set v_ => (fun (elemType : Type_) => (fun x_ => match x_ with
| Term_Set v_ => (fun (vals : (list) (Term)) => let terms := (sets.toList) (vals) in let results := ((eithers.mapList) (fun (t : Term) => ((((toJson) (types)) (tname)) (elemType)) (t))) (terms) in ((eithers.map) (fun (vs : (list) (Value)) => (Value_Array) (vs))) (results)) (v_)
| _ => (inl) ("expected set term"%string)
end) (strippedTerm)) (v_)
| Type__Maybe v_ => (fun (innerType : Type_) => let innerStripped := (deannotateType) (innerType) in let isNestedMaybe := (fun x_ => match x_ with
| Type__Maybe v_ => (fun (_ : Type_) => true) (v_)
| _ => false
end) (innerStripped) in (fun x_ => match x_ with
| Term_Maybe v_ => (fun (opt : (option) (Term)) => (((maybes.maybe) ((inr) ((Value_Null) (tt)))) (fun (v : Term) => let encoded := ((((toJson) (types)) (tname)) (innerType)) (v) in (((logic.ifElse) (isNestedMaybe)) (((eithers.map) (fun (ev : Value) => (Value_Array) ((cons) (ev) (nil)))) (encoded))) (encoded))) (opt)) (v_)
| _ => (inl) ("expected maybe term"%string)
end) (strippedTerm)) (v_)
| Type__Record v_ => (fun (rt : (list) (FieldType)) => (fun x_ => match x_ with
| Term_Record v_ => (fun (r : Record_) => let isSimpleMaybe := fun (ftype : Type_) => (fun x_ => match x_ with
| Type__Maybe v_ => (fun (innerT : Type_) => (fun x_ => match x_ with
| Type__Maybe v_ => (fun (_ : Type_) => false) (v_)
| _ => true
end) ((deannotateType) (innerT))) (v_)
| _ => false
end) ((deannotateType) (ftype)) in let fields := (fun r_ => (record__fields) (r_)) (r) in let fieldTypes := rt in let encodeFieldWithType := fun (ft : FieldType) => fun (f : Field) => let ftype := (fun r_ => (fieldType_type) (r_)) (ft) in let fterm := (fun r_ => (field_term) (r_)) (f) in let fname := (fun w_ => w_) ((fun r_ => (field_name) (r_)) (f)) in (((logic.ifElse) ((isSimpleMaybe) (ftype))) ((fun x_ => match x_ with
| Term_Maybe v_ => (fun (opt : (option) (Term)) => (((maybes.maybe) ((inr) (None))) (fun (v : Term) => let innerType := (fun x_ => match x_ with
| Type__Maybe v_ => (fun (it : Type_) => it) (v_)
| _ => ftype
end) ((deannotateType) (ftype)) in let encoded := ((((toJson) (types)) (tname)) (innerType)) (v) in ((eithers.map) (fun (ev : Value) => (Some) ((pair) (fname) (ev)))) (encoded))) (opt)) (v_)
| _ => (inl) ("expected maybe term for optional field"%string)
end) ((deannotateTerm) (fterm)))) (let encoded := ((((toJson) (types)) (tname)) (ftype)) (fterm) in ((eithers.map) (fun (ev : Value) => (Some) ((pair) (fname) (ev)))) (encoded)) in let encodedPairs := ((eithers.mapList) (fun (ftf : (prod) (FieldType) (Field)) => ((encodeFieldWithType) ((pairs.first) (ftf))) ((pairs.second) (ftf)))) (((lists.zip) (fieldTypes)) (fields)) in ((eithers.map) (fun (pairs : (list) ((option) ((prod) (string) (Value)))) => (Value_Object) ((maps.fromList) ((maybes.cat) (pairs))))) (encodedPairs)) (v_)
| _ => (inl) ("expected record term"%string)
end) (strippedTerm)) (v_)
| Type__Union v_ => (fun (rt : (list) (FieldType)) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun w_ => w_) ((fun r_ => (field_name) (r_)) (field)) in let findFieldType := (hydra_fix) (fun findFieldType => fun (fts : (list) (FieldType)) => (((logic.ifElse) ((lists.null) (fts))) ((inl) ((strings.cat) ((cons) ("unknown variant: "%string) ((cons) (fname) (nil)))))) ((((logic.ifElse) (((equality.equal) ((fun w_ => w_) ((fun r_ => (fieldType_name) (r_)) ((lists.head) (fts))))) (fname))) ((inr) ((fun r_ => (fieldType_type) (r_)) ((lists.head) (fts))))) ((findFieldType) ((lists.tail) (fts))))) in let ftypeResult := (findFieldType) (rt) in let fterm := (fun r_ => (field_term) (r_)) (field) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (ftype : Type_) => let encodedUnion := ((((toJson) (types)) (tname)) (ftype)) (fterm) in ((eithers.map) (fun (v : Value) => (Value_Object) ((maps.fromList) ((cons) ((pair) (fname) (v)) (nil))))) (encodedUnion))) (ftypeResult)) (v_)
| _ => (inl) ("expected union term"%string)
end) (strippedTerm)) (v_)
| Type__Unit _ => (inr) ((Value_Object) (maps.empty))
| Type__Wrap v_ => (fun (wn : Type_) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wt : WrappedTerm) => ((((toJson) (types)) (tname)) (wn)) ((fun r_ => (wrappedTerm_body) (r_)) (wt))) (v_)
| _ => (inl) ("expected wrapped term"%string)
end) (strippedTerm)) (v_)
| Type__Map v_ => (fun (mt : MapType) => let valType := (fun r_ => (mapType_values) (r_)) (mt) in let keyType := (fun r_ => (mapType_keys) (r_)) (mt) in (fun x_ => match x_ with
| Term_Map v_ => (fun (m : (list) ((prod) (Term) (Term))) => let encodeEntry := fun (kv : (prod) (Term) (Term)) => let v := (pairs.second) (kv) in let k := (pairs.first) (kv) in let encodedV := ((((toJson) (types)) (tname)) (valType)) (v) in let encodedK := ((((toJson) (types)) (tname)) (keyType)) (k) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (ek : Value) => ((eithers.map) (fun (ev : Value) => (Value_Object) ((maps.fromList) ((cons) ((pair) ("@key"%string) (ek)) ((cons) ((pair) ("@value"%string) (ev)) (nil)))))) (encodedV))) (encodedK) in let entries := ((eithers.mapList) (encodeEntry)) ((maps.toList) (m)) in ((eithers.map) (fun (es : (list) (Value)) => (Value_Array) (es))) (entries)) (v_)
| _ => (inl) ("expected map term"%string)
end) (strippedTerm)) (v_)
| Type__Pair v_ => (fun (pt : PairType) => let secondType := (fun r_ => (pairType_second) (r_)) (pt) in let firstType := (fun r_ => (pairType_first) (r_)) (pt) in (fun x_ => match x_ with
| Term_Pair v_ => (fun (p : (prod) (Term) (Term)) => let second := (pairs.second) (p) in let first := (pairs.first) (p) in let encodedSecond := ((((toJson) (types)) (tname)) (secondType)) (second) in let encodedFirst := ((((toJson) (types)) (tname)) (firstType)) (first) in (((eithers.either) (fun (err : string) => (inl) (err))) (fun (ef : Value) => ((eithers.map) (fun (es : Value) => (Value_Object) ((maps.fromList) ((cons) ((pair) ("@first"%string) (ef)) ((cons) ((pair) ("@second"%string) (es)) (nil)))))) (encodedSecond))) (encodedFirst)) (v_)
| _ => (inl) ("expected pair term"%string)
end) (strippedTerm)) (v_)
| Type__Either v_ => (fun (et : EitherType) => let rightType := (fun r_ => (eitherType_right) (r_)) (et) in let leftType := (fun r_ => (eitherType_left) (r_)) (et) in (fun x_ => match x_ with
| Term_Either v_ => (fun (e : (sum) (Term) (Term)) => (((eithers.either) (fun (l : Term) => let encodedL := ((((toJson) (types)) (tname)) (leftType)) (l) in ((eithers.map) (fun (v : Value) => (Value_Object) ((maps.fromList) ((cons) ((pair) ("@left"%string) (v)) (nil))))) (encodedL))) (fun (r : Term) => let encodedR := ((((toJson) (types)) (tname)) (rightType)) (r) in ((eithers.map) (fun (v : Value) => (Value_Object) ((maps.fromList) ((cons) ((pair) ("@right"%string) (v)) (nil))))) (encodedR))) (e)) (v_)
| _ => (inl) ("expected either term"%string)
end) (strippedTerm)) (v_)
| Type__Variable v_ => (fun (name : Name) => let lookedUp := ((maps.lookup) (name)) (types) in (((maybes.maybe) ((toJsonUntyped) (term_))) (fun (resolvedType : Type_) => ((((toJson) (types)) (name)) (resolvedType)) (term_))) (lookedUp)) (v_)
| _ => (inl) ((strings.cat) ((cons) ("unsupported type for JSON encoding: "%string) ((cons) ((hydra.show.core.type) (typ)) (nil))))
end) (stripped)).

Definition toJson : (list) ((prod) (Name) (Type_)) -> Name -> Type_ -> Term -> (sum) (string) (Value) :=
  toJson_bundle.

