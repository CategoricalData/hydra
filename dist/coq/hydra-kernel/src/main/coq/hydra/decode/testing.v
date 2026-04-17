(* Term decoders for hydra.testing *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.errors hydra.extract.core hydra.graph hydra.lib.eithers hydra.lib.maps hydra.lib.maybes hydra.lib.strings hydra.testing.

Definition tag : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Tag) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (Tag))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : string) => b)) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (string))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (DecodingError) (string)) (v_)
| _ => ((inl) ("expected string literal"%string)) : (sum) (DecodingError) (string)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => ((inl) ("expected wrapped type"%string)) : (sum) (DecodingError) (Tag)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition universalTestCase : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (UniversalTestCase) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (UniversalTestCase))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("actual"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (string))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (DecodingError) (string)) (v_)
| _ => ((inl) ("expected string literal"%string)) : (sum) (DecodingError) (string)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_actual : string) => ((eithers.bind) (((((requireField) ("expected"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (string))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (DecodingError) (string)) (v_)
| _ => ((inl) ("expected string literal"%string)) : (sum) (DecodingError) (string)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_expected : string) => ((inr) ((Build_UniversalTestCase) (field_actual) (field_expected))) : (sum) (DecodingError) (UniversalTestCase)))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (UniversalTestCase)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition testCase : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TestCase) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (TestCase))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("universal"%string) (fun (input : Term) => ((eithers.map) (fun (t : UniversalTestCase) => (TestCase_Universal) (t))) (((universalTestCase) (cx)) (input)))) (nil)) in (((maybes.maybe) (((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil)))))) : (sum) (DecodingError) (TestCase))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (TestCase)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => ((inl) ("expected union"%string)) : (sum) (DecodingError) (TestCase)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition testCaseWithMetadata : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TestCaseWithMetadata) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (TestCaseWithMetadata))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (string))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (DecodingError) (string)) (v_)
| _ => ((inl) ("expected string literal"%string)) : (sum) (DecodingError) (string)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_name : string) => ((eithers.bind) (((((requireField) ("case"%string)) (testCase)) (fieldMap)) (cx))) (fun (field_case : TestCase) => ((eithers.bind) (((((requireField) ("description"%string)) ((decodeMaybe) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (string))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (DecodingError) (string)) (v_)
| _ => ((inl) ("expected string literal"%string)) : (sum) (DecodingError) (string)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))))) (fieldMap)) (cx))) (fun (field_description : (option) (string)) => ((eithers.bind) (((((requireField) ("tags"%string)) ((decodeList) (tag))) (fieldMap)) (cx))) (fun (field_tags : (list) (Tag)) => ((inr) ((Build_TestCaseWithMetadata) (field_name) (field_case) (field_description) (field_tags))) : (sum) (DecodingError) (TestCaseWithMetadata)))))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (TestCaseWithMetadata)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition testGroup_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TestGroup)) =>
    let testGroup := bundle_ in
    fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (TestGroup))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (string))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (DecodingError) (string)) (v_)
| _ => ((inl) ("expected string literal"%string)) : (sum) (DecodingError) (string)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_name : string) => ((eithers.bind) (((((requireField) ("description"%string)) ((decodeMaybe) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (string))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (DecodingError) (string)) (v_)
| _ => ((inl) ("expected string literal"%string)) : (sum) (DecodingError) (string)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))))) (fieldMap)) (cx))) (fun (field_description : (option) (string)) => ((eithers.bind) (((((requireField) ("subgroups"%string)) ((decodeList) (testGroup))) (fieldMap)) (cx))) (fun (field_subgroups : (list) (TestGroup)) => ((eithers.bind) (((((requireField) ("cases"%string)) ((decodeList) (testCaseWithMetadata))) (fieldMap)) (cx))) (fun (field_cases : (list) (TestCaseWithMetadata)) => ((inr) ((Build_TestGroup) (field_name) (field_description) (field_subgroups) (field_cases))) : (sum) (DecodingError) (TestGroup)))))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (TestGroup)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))).

Definition testGroup : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TestGroup) :=
  testGroup_bundle.

