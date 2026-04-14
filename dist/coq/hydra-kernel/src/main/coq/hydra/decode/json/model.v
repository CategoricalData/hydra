(* Term decoders for hydra.json.model *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.graph hydra.core hydra.errors hydra.json.model hydra.lib.eithers hydra.lib.maps hydra.extract.core hydra.lib.maybes hydra.lib.strings.

Definition value_bundle :=
  hydra_fix (fun (bundle_ : hydra.graph.Graph -> Term -> (sum) (DecodingError) (Value)) =>
    let value := bundle_ in
    fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let variantMap := (maps.fromList) ((cons) ((pair) ("array"%string) (fun (input : Term) => ((eithers.map) (fun (t : (list) (Value)) => (Value_Array) (t))) ((((decodeList) (value)) (cx)) (input)))) ((cons) ((pair) ("boolean"%string) (fun (input : Term) => ((eithers.map) (fun (t : bool) => (Value_Boolean) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Boolean v_ => (fun (b : bool) => (inr) (b)) (v_)
| _ => (inl) ("expected boolean literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("null"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (Value_Null) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("number"%string) (fun (input : Term) => ((eithers.map) (fun (t : Q) => (Value_Number) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Float v_ => (fun x_ => match x_ with
| FloatValue_Bigfloat v_ => (fun (f : Q) => (inr) (f)) (v_)
| _ => (inl) ("expected bigfloat value"%string)
end) (v_)
| _ => (inl) ("expected bigfloat literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("object"%string) (fun (input : Term) => ((eithers.map) (fun (t : (list) ((prod) (string) (Value))) => (Value_Object) (t))) (((((decodeMap) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (value)) (cx)) (input)))) ((cons) ((pair) ("string"%string) (fun (input : Term) => ((eithers.map) (fun (t : string) => (Value_String) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) (nil))))))) in let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : Term -> (sum) (DecodingError) (Value)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))).

Definition value : hydra.graph.Graph -> Term -> (sum) (DecodingError) (Value) :=
  value_bundle.

