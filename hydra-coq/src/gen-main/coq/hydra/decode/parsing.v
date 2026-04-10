(* Term decoders for hydra.parsing *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.graph hydra.core hydra.errors hydra.parsing hydra.lib.eithers hydra.extract.core hydra.lib.maps hydra.lib.maybes hydra.lib.strings.

Definition parseSuccess (t0 : Type) : (hydra.graph.Graph -> Term -> (sum) (DecodingError) (t0)) -> hydra.graph.Graph -> Term -> (sum) (DecodingError) ((ParseSuccess) (t0)) :=
  fun (a : hydra.graph.Graph -> Term -> (sum) (DecodingError) (t0)) => fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("value"%string)) (a)) (fieldMap)) (cx))) (fun (field_value : t0) => ((eithers.bind) (((((requireField) ("remainder"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_remainder : string) => (inr) ((Build_ParseSuccess) (field_value) (field_remainder))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Arguments parseSuccess {t0}.
Definition parseError : hydra.graph.Graph -> Term -> (sum) (DecodingError) (ParseError) :=
  fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("message"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_message : string) => ((eithers.bind) (((((requireField) ("remainder"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => (inr) (s)) (v_)
| _ => (inl) ("expected string literal"%string)
end) (v)) (v_)
| _ => (inl) ("expected literal"%string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_remainder : string) => (inr) ((Build_ParseError) (field_message) (field_remainder))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition parseResult (t0 : Type) : (hydra.graph.Graph -> Term -> (sum) (DecodingError) (t0)) -> hydra.graph.Graph -> Term -> (sum) (DecodingError) ((ParseResult) (t0)) :=
  fun (a : hydra.graph.Graph -> Term -> (sum) (DecodingError) (t0)) => fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Union v_ => (fun (inj : Injection) => let variantMap := (maps.fromList) ((cons) ((pair) ("success"%string) (fun (input : Term) => ((eithers.map) (fun (t : (ParseSuccess) (t0)) => (ParseResult_Success) (t))) ((((parseSuccess) (a)) (cx)) (input)))) ((cons) ((pair) ("failure"%string) (fun (input : Term) => ((eithers.map) (fun (t : ParseError) => (ParseResult_Failure) (t))) (((parseError) (cx)) (input)))) (nil))) in let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in (((maybes.maybe) ((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil))))))) (fun (f : Term -> (sum) (DecodingError) ((ParseResult) (t0))) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => (inl) ("expected union"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Arguments parseResult {t0}.

