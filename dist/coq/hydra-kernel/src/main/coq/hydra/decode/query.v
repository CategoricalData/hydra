(* Term decoders for hydra.query *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.decode.core hydra.errors hydra.extract.core hydra.graph hydra.lib.eithers hydra.lib.maps hydra.lib.maybes hydra.lib.strings hydra.query.

Definition comparisonConstraint : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (ComparisonConstraint) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (ComparisonConstraint))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("equal"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (ComparisonConstraint_Equal) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("notEqual"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (ComparisonConstraint_NotEqual) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("lessThan"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (ComparisonConstraint_LessThan) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("greaterThan"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (ComparisonConstraint_GreaterThan) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("lessThanOrEqual"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (ComparisonConstraint_LessThanOrEqual) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("greaterThanOrEqual"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (ComparisonConstraint_GreaterThanOrEqual) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) (nil))))))) in (((maybes.maybe) (((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil)))))) : (sum) (DecodingError) (ComparisonConstraint))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (ComparisonConstraint)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => ((inl) ("expected union"%string)) : (sum) (DecodingError) (ComparisonConstraint)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition edge : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Edge) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (Edge))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("type"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_type : Name) => ((eithers.bind) (((((requireField) ("out"%string)) ((decodeMaybe) (hydra.decode.core.name))) (fieldMap)) (cx))) (fun (field_out : (option) (Name)) => ((eithers.bind) (((((requireField) ("in"%string)) ((decodeMaybe) (hydra.decode.core.name))) (fieldMap)) (cx))) (fun (field_in : (option) (Name)) => ((inr) ((Build_Edge) (field_type) (field_out) (field_in))) : (sum) (DecodingError) (Edge))))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (Edge)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition variable : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Variable_) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (Variable_))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : string) => b)) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (string))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (DecodingError) (string)) (v_)
| _ => ((inl) ("expected string literal"%string)) : (sum) (DecodingError) (string)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => ((inl) ("expected wrapped type"%string)) : (sum) (DecodingError) (Variable_)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition node : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Node) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (Node))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("term"%string) (fun (input : Term) => ((eithers.map) (fun (t : Term) => (Node_Term) (t))) (((hydra.decode.core.term) (cx)) (input)))) ((cons) ((pair) ("variable"%string) (fun (input : Term) => ((eithers.map) (fun (t : Variable_) => (Node_Variable) (t))) (((variable) (cx)) (input)))) ((cons) ((pair) ("wildcard"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (Node_Wildcard) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) (nil)))) in (((maybes.maybe) (((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil)))))) : (sum) (DecodingError) (Node))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (Node)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => ((inl) ("expected union"%string)) : (sum) (DecodingError) (Node)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition range : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Range) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (Range))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("min"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (Z))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => ((inr) (i)) : (sum) (DecodingError) (Z)) (v_)
| _ => ((inl) ("expected int32 value"%string)) : (sum) (DecodingError) (Z)
end) (v_)
| _ => ((inl) ("expected int32 literal"%string)) : (sum) (DecodingError) (Z)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (Z)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_min : Z) => ((eithers.bind) (((((requireField) ("max"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (Z))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => ((inr) (i)) : (sum) (DecodingError) (Z)) (v_)
| _ => ((inl) ("expected int32 value"%string)) : (sum) (DecodingError) (Z)
end) (v_)
| _ => ((inl) ("expected int32 literal"%string)) : (sum) (DecodingError) (Z)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (Z)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_max : Z) => ((inr) ((Build_Range) (field_min) (field_max))) : (sum) (DecodingError) (Range)))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (Range)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition regexQuantifier : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (RegexQuantifier) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (RegexQuantifier))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("one"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (RegexQuantifier_One) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("zeroOrOne"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (RegexQuantifier_ZeroOrOne) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("zeroOrMore"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (RegexQuantifier_ZeroOrMore) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("oneOrMore"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (RegexQuantifier_OneOrMore) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("exactly"%string) (fun (input : Term) => ((eithers.map) (fun (t : Z) => (RegexQuantifier_Exactly) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (Z))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => ((inr) (i)) : (sum) (DecodingError) (Z)) (v_)
| _ => ((inl) ("expected int32 value"%string)) : (sum) (DecodingError) (Z)
end) (v_)
| _ => ((inl) ("expected int32 literal"%string)) : (sum) (DecodingError) (Z)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (Z)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("atLeast"%string) (fun (input : Term) => ((eithers.map) (fun (t : Z) => (RegexQuantifier_AtLeast) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (Z))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => ((inr) (i)) : (sum) (DecodingError) (Z)) (v_)
| _ => ((inl) ("expected int32 value"%string)) : (sum) (DecodingError) (Z)
end) (v_)
| _ => ((inl) ("expected int32 literal"%string)) : (sum) (DecodingError) (Z)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (Z)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) (input)))) ((cons) ((pair) ("range"%string) (fun (input : Term) => ((eithers.map) (fun (t : Range) => (RegexQuantifier_Range) (t))) (((range) (cx)) (input)))) (nil)))))))) in (((maybes.maybe) (((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil)))))) : (sum) (DecodingError) (RegexQuantifier))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (RegexQuantifier)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => ((inl) ("expected union"%string)) : (sum) (DecodingError) (RegexQuantifier)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition step : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Step) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (Step))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("edge"%string) (fun (input : Term) => ((eithers.map) (fun (t : Edge) => (Step_Edge) (t))) (((edge) (cx)) (input)))) ((cons) ((pair) ("project"%string) (fun (input : Term) => ((eithers.map) (fun (t : Projection) => (Step_Project) (t))) (((hydra.decode.core.projection) (cx)) (input)))) ((cons) ((pair) ("compare"%string) (fun (input : Term) => ((eithers.map) (fun (t : ComparisonConstraint) => (Step_Compare) (t))) (((comparisonConstraint) (cx)) (input)))) (nil)))) in (((maybes.maybe) (((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil)))))) : (sum) (DecodingError) (Step))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (Step)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => ((inl) ("expected union"%string)) : (sum) (DecodingError) (Step)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition path_regexSequence_bundle :=
  hydra_fix (fun (bundle_ : prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Path)) (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (RegexSequence))) =>
    let path := (fst bundle_) in
    let regexSequence := (snd bundle_) in
    (pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (Path))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("step"%string) (fun (input : Term) => ((eithers.map) (fun (t : Step) => (Path_Step) (t))) (((step) (cx)) (input)))) ((cons) ((pair) ("regex"%string) (fun (input : Term) => ((eithers.map) (fun (t : RegexSequence) => (Path_Regex) (t))) (((regexSequence) (cx)) (input)))) ((cons) ((pair) ("inverse"%string) (fun (input : Term) => ((eithers.map) (fun (t : Path) => (Path_Inverse) (t))) (((path) (cx)) (input)))) (nil)))) in (((maybes.maybe) (((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil)))))) : (sum) (DecodingError) (Path))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (Path)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => ((inl) ("expected union"%string)) : (sum) (DecodingError) (Path)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (RegexSequence))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("path"%string)) (path)) (fieldMap)) (cx))) (fun (field_path : Path) => ((eithers.bind) (((((requireField) ("quantifier"%string)) (regexQuantifier)) (fieldMap)) (cx))) (fun (field_quantifier : RegexQuantifier) => ((inr) ((Build_RegexSequence) (field_path) (field_quantifier))) : (sum) (DecodingError) (RegexSequence)))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (RegexSequence)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))))).

Definition path : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Path) :=
  (fst path_regexSequence_bundle).
Definition regexSequence : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (RegexSequence) :=
  (snd path_regexSequence_bundle).
Definition triplePattern : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TriplePattern) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (TriplePattern))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("subject"%string)) (node)) (fieldMap)) (cx))) (fun (field_subject : Node) => ((eithers.bind) (((((requireField) ("predicate"%string)) (path)) (fieldMap)) (cx))) (fun (field_predicate : Path) => ((eithers.bind) (((((requireField) ("object"%string)) (node)) (fieldMap)) (cx))) (fun (field_object : Node) => ((inr) ((Build_TriplePattern) (field_subject) (field_predicate) (field_object))) : (sum) (DecodingError) (TriplePattern))))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (TriplePattern)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition graphPattern_pattern_bundle :=
  hydra_fix (fun (bundle_ : prod (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (GraphPattern)) (forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Pattern))) =>
    let graphPattern := (fst bundle_) in
    let pattern := (snd bundle_) in
    (pair (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (GraphPattern))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("graph"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_graph : Name) => ((eithers.bind) (((((requireField) ("patterns"%string)) ((decodeList) (pattern))) (fieldMap)) (cx))) (fun (field_patterns : (list) (Pattern)) => ((inr) ((Build_GraphPattern) (field_graph) (field_patterns))) : (sum) (DecodingError) (GraphPattern)))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (GraphPattern)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))) (fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (Pattern))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("triple"%string) (fun (input : Term) => ((eithers.map) (fun (t : TriplePattern) => (Pattern_Triple) (t))) (((triplePattern) (cx)) (input)))) ((cons) ((pair) ("negation"%string) (fun (input : Term) => ((eithers.map) (fun (t : Pattern) => (Pattern_Negation) (t))) (((pattern) (cx)) (input)))) ((cons) ((pair) ("conjunction"%string) (fun (input : Term) => ((eithers.map) (fun (t : (list) (Pattern)) => (Pattern_Conjunction) (t))) ((((decodeList) (pattern)) (cx)) (input)))) ((cons) ((pair) ("disjunction"%string) (fun (input : Term) => ((eithers.map) (fun (t : (list) (Pattern)) => (Pattern_Disjunction) (t))) ((((decodeList) (pattern)) (cx)) (input)))) ((cons) ((pair) ("graph"%string) (fun (input : Term) => ((eithers.map) (fun (t : GraphPattern) => (Pattern_Graph) (t))) (((graphPattern) (cx)) (input)))) (nil)))))) in (((maybes.maybe) (((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil)))))) : (sum) (DecodingError) (Pattern))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (Pattern)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => ((inl) ("expected union"%string)) : (sum) (DecodingError) (Pattern)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw))))).

Definition graphPattern : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (GraphPattern) :=
  (fst graphPattern_pattern_bundle).
Definition pattern : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Pattern) :=
  (snd graphPattern_pattern_bundle).
Definition pathEquation : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (PathEquation) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (PathEquation))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("left"%string)) (path)) (fieldMap)) (cx))) (fun (field_left : Path) => ((eithers.bind) (((((requireField) ("right"%string)) (path)) (fieldMap)) (cx))) (fun (field_right : Path) => ((inr) ((Build_PathEquation) (field_left) (field_right))) : (sum) (DecodingError) (PathEquation)))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (PathEquation)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition patternImplication : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (PatternImplication) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (PatternImplication))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("antecedent"%string)) (pattern)) (fieldMap)) (cx))) (fun (field_antecedent : Pattern) => ((eithers.bind) (((((requireField) ("consequent"%string)) (pattern)) (fieldMap)) (cx))) (fun (field_consequent : Pattern) => ((inr) ((Build_PatternImplication) (field_antecedent) (field_consequent))) : (sum) (DecodingError) (PatternImplication)))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (PatternImplication)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition query : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Query) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (Query))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("variables"%string)) ((decodeList) (variable))) (fieldMap)) (cx))) (fun (field_variables : (list) (Variable_)) => ((eithers.bind) (((((requireField) ("patterns"%string)) ((decodeList) (pattern))) (fieldMap)) (cx))) (fun (field_patterns : (list) (Pattern)) => ((inr) ((Build_Query) (field_variables) (field_patterns))) : (sum) (DecodingError) (Query)))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (Query)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).

