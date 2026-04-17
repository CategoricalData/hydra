(* Utilities for reading and writing type and term annotations *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.classes hydra.constants hydra.context hydra.core hydra.decode.core hydra.encode.core hydra.errors hydra.extract.core hydra.graph hydra.lib.eithers hydra.lib.equality hydra.lib.lists hydra.lib.logic hydra.lib.maps hydra.lib.math hydra.lib.maybes hydra.lib.pairs hydra.lib.sets hydra.show.core hydra.strip.

Definition aggregateAnnotations (t0 : Type) (t1 : Type) (t2 : Type) (t3 : Type) : forall (_ : forall (_ : t0) , (option) (t1)) , forall (_ : forall (_ : t1) , t0) , forall (_ : forall (_ : t1) , (list) ((prod) (t2) (t3))) , forall (_ : t0) , (list) ((prod) (t2) (t3)) := fun (getValue : forall (_ : t0) , (option) (t1)) => fun (getX : forall (_ : t1) , t0) => fun (getAnns : forall (_ : t1) , (list) ((prod) (t2) (t3))) => fun (t : t0) => let toPairs := (hydra_fix) (fun toPairs => fun (rest : (list) ((list) ((prod) (t2) (t3)))) => fun (t2 : t0) => (((maybes.maybe) (rest)) (fun (yy : t1) => ((toPairs) (((lists.cons) ((maps.toList) ((getAnns) (yy)))) (rest))) ((getX) (yy)))) ((getValue) (t2))) in (maps.fromList) ((lists.concat) (((toPairs) (nil)) (t))).
Arguments aggregateAnnotations {t0} {t1} {t2} {t3}.
Definition getDescription (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : (list) ((prod) (Name) (Term))) , (sum) (Error) ((option) (string)) := fun (cx : t0) => fun (graph_ : hydra.graph.Graph) => fun (anns : (list) ((prod) (Name) (Term))) => (((maybes.maybe) ((inr) (None))) (fun (term_ : Term) => ((eithers.map) (maybes.pure)) (((string_) (graph_)) (term_)))) (((maps.lookup) ("description"%string)) (anns)).
Arguments getDescription {t0}.
Definition termAnnotationInternal : forall (_ : Term) , (list) ((prod) (Name) (Term)) := fun (term_ : Term) => let getAnn := fun (t : Term) => (fun x_ => match x_ with
| Term_Annotated v_ => (fun (a : AnnotatedTerm) => (Some) (a)) (v_)
| _ => (None) : (option) (AnnotatedTerm)
end) (t) in ((((aggregateAnnotations) (getAnn)) (fun (at_ : AnnotatedTerm) => (fun r_ => (annotatedTerm_body) (r_)) (at_))) (fun (at_ : AnnotatedTerm) => (fun r_ => (annotatedTerm_annotation) (r_)) (at_))) (term_).
Definition getTermDescription (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) ((option) (string)) := fun (cx : t0) => fun (graph_ : hydra.graph.Graph) => fun (term_ : Term) => let peel := (hydra_fix) (fun peel => fun (t : Term) => (fun x_ => match x_ with
| Term_TypeLambda v_ => (fun (tl : TypeLambda) => (peel) ((fun r_ => (typeLambda_body) (r_)) (tl))) (v_)
| Term_TypeApplication v_ => (fun (ta : TypeApplicationTerm) => (peel) ((fun r_ => (typeApplicationTerm_body) (r_)) (ta))) (v_)
| _ => t
end) (t)) in (((getDescription) (cx)) (graph_)) ((termAnnotationInternal) ((peel) (term_))).
Arguments getTermDescription {t0}.
Definition commentsFromBinding (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Binding) , (sum) (Error) ((option) (string)) := fun (cx : t0) => fun (g : hydra.graph.Graph) => fun (b : Binding) => (((getTermDescription) (cx)) (g)) ((fun r_ => (binding_term) (r_)) (b)).
Arguments commentsFromBinding {t0}.
Definition typeAnnotationInternal : forall (_ : Type_) , (list) ((prod) (Name) (Term)) := fun (typ : Type_) => let getAnn := fun (t : Type_) => (fun x_ => match x_ with
| Type__Annotated v_ => (fun (a : AnnotatedType) => (Some) (a)) (v_)
| _ => (None) : (option) (AnnotatedType)
end) (t) in ((((aggregateAnnotations) (getAnn)) (fun (at_ : AnnotatedType) => (fun r_ => (annotatedType_body) (r_)) (at_))) (fun (at_ : AnnotatedType) => (fun r_ => (annotatedType_annotation) (r_)) (at_))) (typ).
Definition getTypeDescription (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Type_) , (sum) (Error) ((option) (string)) := fun (cx : t0) => fun (graph_ : hydra.graph.Graph) => fun (typ : Type_) => (((getDescription) (cx)) (graph_)) ((typeAnnotationInternal) (typ)).
Arguments getTypeDescription {t0}.
Definition commentsFromFieldType (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : FieldType) , (sum) (Error) ((option) (string)) := fun (cx : t0) => fun (g : hydra.graph.Graph) => fun (ft : FieldType) => (((getTypeDescription) (cx)) (g)) ((fun r_ => (fieldType_type) (r_)) (ft)).
Arguments commentsFromFieldType {t0}.
Definition getAttr : forall (_ : Name) , forall (_ : Context_) , (option) (Term) := fun (key : Name) => fun (cx : Context_) => ((maps.lookup) (key)) ((fun r_ => (context__other) (r_)) (cx)).
Definition getDebugId : forall (_ : Context_) , (sum) (Error) ((option) (string)) := fun (cx : Context_) => (((maybes.maybe) (((inr) ((None) : (option) (string))) : (sum) (Error) ((option) (string)))) (fun (term_ : Term) => ((eithers.map) (maybes.pure)) (((string_) ((Build_Graph) (maps.empty) (maps.empty) (maps.empty) (sets.empty) (maps.empty) (maps.empty) (maps.empty) (sets.empty))) (term_)))) (((getAttr) (key_debugId)) (cx)).
Definition debugIf : forall (_ : Context_) , forall (_ : string) , forall (_ : string) , (sum) (Error) (unit) := fun (cx : Context_) => fun (debugId : string) => fun (message : string) => ((eithers.bind) ((getDebugId) (cx))) (fun (mid : (option) (string)) => (((logic.ifElse) (((equality.equal) (mid)) ((Some) (debugId)))) (((inl) ((Error_Other) (message))) : (sum) (Error) (unit))) (((inr) (tt)) : (sum) (Error) (unit))).
Definition getAttrWithDefault : forall (_ : Name) , forall (_ : Term) , forall (_ : Context_) , Term := fun (key : Name) => fun (def : Term) => fun (cx : Context_) => ((maybes.fromMaybe) (def)) (((getAttr) (key)) (cx)).
Definition hasFlag : forall (_ : Context_) , forall (_ : Name) , (sum) (Error) (bool) := fun (cx : Context_) => fun (flag : Name) => let term_ := (((getAttrWithDefault) (flag)) ((Term_Literal) ((Literal_Boolean) (false)))) (cx) in ((boolean) ((Build_Graph) (maps.empty) (maps.empty) (maps.empty) (sets.empty) (maps.empty) (maps.empty) (maps.empty) (sets.empty))) (term_).
Definition failOnFlag : forall (_ : Context_) , forall (_ : Name) , forall (_ : string) , (sum) (Error) (unit) := fun (cx : Context_) => fun (flag : Name) => fun (msg : string) => ((eithers.bind) (((hasFlag) (cx)) (flag))) (fun (val : bool) => (((logic.ifElse) (val)) (((inl) ((Error_Other) (msg))) : (sum) (Error) (unit))) (((inr) (tt)) : (sum) (Error) (unit))).
Definition getCount : forall (_ : Name) , forall (_ : Context_) , Z := fun (key : Name) => fun (cx : Context_) => (((maybes.maybe) ((0)%Z)) (fun (term_ : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (lit : Literal) => (fun x_ => match x_ with
| Literal_Integer v_ => (fun (iv : IntegerValue) => (fun x_ => match x_ with
| IntegerValue_Int32 v_ => (fun (i : Z) => i) (v_)
| _ => (0)%Z
end) (iv)) (v_)
| _ => (0)%Z
end) (lit)) (v_)
| _ => (0)%Z
end) (term_))) (((maps.lookup) (key)) ((fun r_ => (context__other) (r_)) (cx))).
Definition getTermAnnotation : forall (_ : Name) , forall (_ : Term) , (option) (Term) := fun (key : Name) => fun (term_ : Term) => ((maps.lookup) (key)) ((termAnnotationInternal) (term_)).
Definition getType : forall (_ : hydra.graph.Graph) , forall (_ : (list) ((prod) (Name) (Term))) , (sum) (DecodingError) ((option) (Type_)) := fun (graph_ : hydra.graph.Graph) => fun (anns : (list) ((prod) (Name) (Term))) => (((maybes.maybe) (((inr) ((None) : (option) (Type_))) : (sum) (DecodingError) ((option) (Type_)))) (fun (dat : Term) => ((eithers.map) (maybes.pure)) (((hydra.decode.core.type) (graph_)) (dat)))) (((maps.lookup) (key_type)) (anns)).
Definition getTypeAnnotation : forall (_ : Name) , forall (_ : Type_) , (option) (Term) := fun (key : Name) => fun (typ : Type_) => ((maps.lookup) (key)) ((typeAnnotationInternal) (typ)).
Definition getTypeClasses (t0 : Type) : forall (_ : t0) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (Error) ((list) ((prod) (Name) ((list) (TypeClass)))) := fun (cx : t0) => fun (graph_ : hydra.graph.Graph) => fun (term_ : Term) => let decodeClass := fun (term2 : Term) => let byName := (maps.fromList) ((cons) ((pair) ("equality"%string) ((TypeClass_Equality) (tt))) ((cons) ((pair) ("ordering"%string) ((TypeClass_Ordering) (tt))) (nil))) in ((eithers.bind) ((((unitVariant) ("hydra.classes.TypeClass"%string)) (graph_)) (term2))) (fun (fn : Name) => (((maybes.maybe) ((inl) ((Error_Extraction) ((ExtractionError_UnexpectedShape) ((Build_UnexpectedShapeError) ("type class"%string) ((hydra.show.core.term) (term2))))))) (fun (x : TypeClass) => (inr) (x))) (((maps.lookup) (fn)) (byName))) in (((maybes.maybe) ((inr) (maps.empty))) (fun (term2 : Term) => ((((hydra.extract.core.map) (fun (t : Term) => (((eithers.bimap) (fun (de : DecodingError) => (Error_Decoding) (de))) (fun (x : Name) => x)) (((hydra.decode.core.name) (graph_)) (t)))) (((setOf) (decodeClass)) (graph_))) (graph_)) (term2))) (((getTermAnnotation) (key_classes)) (term_)).
Arguments getTypeClasses {t0}.
Definition hasDescription (t0 : Type) : forall (_ : (list) ((prod) (Name) (t0))) , bool := fun (anns : (list) ((prod) (Name) (t0))) => (maybes.isJust) (((maps.lookup) (key_description)) (anns)).
Arguments hasDescription {t0}.
Definition hasTypeDescription : forall (_ : Type_) , bool := fun (typ : Type_) => (hasDescription) ((typeAnnotationInternal) (typ)).
Definition isNativeType : forall (_ : Binding) , bool := fun (el : Binding) => let isFlaggedAsFirstClassType := ((maybes.fromMaybe) (false)) (((maybes.map) (fun (_ : Term) => true)) (((getTermAnnotation) (key_firstClassType)) ((fun r_ => (binding_term) (r_)) (el)))) in (((maybes.maybe) (false)) (fun (ts : TypeScheme) => ((logic.and) (((equality.equal) (ts)) ((Build_TypeScheme) (nil) ((Type__Variable) ("hydra.core.Type"%string)) ((None) : (option) ((list) ((prod) (Name) (TypeVariableMetadata))))))) ((logic.not) (isFlaggedAsFirstClassType)))) ((fun r_ => (binding_type) (r_)) (el)).
Definition putAttr : forall (_ : Name) , forall (_ : Term) , forall (_ : Context_) , Context_ := fun (key : Name) => fun (val : Term) => fun (cx : Context_) => (Build_Context_) ((fun r_ => (context__trace) (r_)) (cx)) ((fun r_ => (context__messages) (r_)) (cx)) ((((maps.insert) (key)) (val)) ((fun r_ => (context__other) (r_)) (cx))).
Definition putCount : forall (_ : Name) , forall (_ : Z) , forall (_ : Context_) , Context_ := fun (key : Name) => fun (count : Z) => fun (cx : Context_) => (((putAttr) (key)) ((Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (count))))) (cx).
Definition nextCount : forall (_ : Name) , forall (_ : Context_) , (prod) (Z) (Context_) := fun (key : Name) => fun (cx : Context_) => let count := ((getCount) (key)) (cx) in (pair) (count) ((((putCount) (key)) (((math.add) (count)) ((1)%Z))) (cx)).
Definition normalizeTermAnnotations : forall (_ : Term) , Term := fun (term_ : Term) => let anns := (termAnnotationInternal) (term_) in let stripped := (deannotateTerm) (term_) in (((logic.ifElse) ((maps.null) (anns))) (stripped)) ((Term_Annotated) ((Build_AnnotatedTerm) (stripped) (anns))).
Definition normalizeTypeAnnotations : forall (_ : Type_) , Type_ := fun (typ : Type_) => let anns := (typeAnnotationInternal) (typ) in let stripped := (deannotateType) (typ) in (((logic.ifElse) ((maps.null) (anns))) (stripped)) ((Type__Annotated) ((Build_AnnotatedType) (stripped) (anns))).
Definition resetCount : forall (_ : Name) , forall (_ : Context_) , Context_ := fun (key : Name) => fun (cx : Context_) => (((putAttr) (key)) ((Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) ((0)%Z))))) (cx).
Definition setAnnotation (t0 : Type) (t1 : Type) : forall (_ : t0) , forall (_ : (option) (t1)) , forall (_ : (list) ((prod) (t0) (t1))) , (list) ((prod) (t0) (t1)) := fun (key : t0) => fun (val : (option) (t1)) => fun (m : (list) ((prod) (t0) (t1))) => (((maps.alter) (fun (_ : (option) (t1)) => val)) (key)) (m).
Arguments setAnnotation {t0} {t1}.
Definition setDescription : forall (_ : (option) (string)) , forall (_ : (list) ((prod) (Name) (Term))) , (list) ((prod) (Name) (Term)) := fun (d : (option) (string)) => ((setAnnotation) (key_description)) (((maybes.map) (fun (arg_ : string) => (fun (x : Literal) => (Term_Literal) (x)) ((fun (x : string) => (Literal_String) (x)) (arg_)))) (d)).
Definition setTermAnnotation : forall (_ : Name) , forall (_ : (option) (Term)) , forall (_ : Term) , Term := fun (key : Name) => fun (val : (option) (Term)) => fun (term_ : Term) => let anns := (((setAnnotation) (key)) (val)) ((termAnnotationInternal) (term_)) in let term' := (deannotateTerm) (term_) in (((logic.ifElse) ((maps.null) (anns))) (term')) ((Term_Annotated) ((Build_AnnotatedTerm) (term') (anns))).
Definition setTermDescription : forall (_ : (option) (string)) , forall (_ : Term) , Term := fun (d : (option) (string)) => ((setTermAnnotation) (key_description)) (((maybes.map) (fun (s : string) => (Term_Literal) ((Literal_String) (s)))) (d)).
Definition setType : forall (_ : (option) (Type_)) , forall (_ : (list) ((prod) (Name) (Term))) , (list) ((prod) (Name) (Term)) := fun (mt : (option) (Type_)) => ((setAnnotation) (key_type)) (((maybes.map) (hydra.encode.core.type)) (mt)).
Definition setTypeAnnotation : forall (_ : Name) , forall (_ : (option) (Term)) , forall (_ : Type_) , Type_ := fun (key : Name) => fun (val : (option) (Term)) => fun (typ : Type_) => let anns := (((setAnnotation) (key)) (val)) ((typeAnnotationInternal) (typ)) in let typ' := (deannotateType) (typ) in (((logic.ifElse) ((maps.null) (anns))) (typ')) ((Type__Annotated) ((Build_AnnotatedType) (typ') (anns))).
Definition setTypeClasses : forall (_ : (list) ((prod) (Name) ((list) (TypeClass)))) , forall (_ : Term) , Term := fun (m : (list) ((prod) (Name) ((list) (TypeClass)))) => fun (term_ : Term) => let encodeClass := fun (tc : TypeClass) => (fun x_ => match x_ with
| TypeClass_Equality _ => (Term_Inject) ((Build_Injection) ("hydra.classes.TypeClass"%string) ((Build_Field) ("equality"%string) ((Term_Unit) (tt))))
| TypeClass_Ordering _ => (Term_Inject) ((Build_Injection) ("hydra.classes.TypeClass"%string) ((Build_Field) ("ordering"%string) ((Term_Unit) (tt))))
end) (tc) in let encodePair := fun (nameClasses : (prod) (Name) ((list) (TypeClass))) => let classes := (pairs.second) (nameClasses) in let name := (pairs.first) (nameClasses) in (pair) ((hydra.encode.core.name) (name)) ((Term_Set) ((sets.fromList) (((lists.map) (encodeClass)) ((sets.toList) (classes))))) in let encoded := (((logic.ifElse) ((maps.null) (m))) ((None) : (option) (Term))) ((Some) ((Term_Map) ((maps.fromList) (((lists.map) (encodePair)) ((maps.toList) (m)))))) in (((setTermAnnotation) (key_classes)) (encoded)) (term_).
Definition setTypeDescription : forall (_ : (option) (string)) , forall (_ : Type_) , Type_ := fun (d : (option) (string)) => ((setTypeAnnotation) (key_description)) (((maybes.map) (fun (arg_ : string) => (fun (x : Literal) => (Term_Literal) (x)) ((fun (x : string) => (Literal_String) (x)) (arg_)))) (d)).
Definition whenFlag (t0 : Type) : forall (_ : Context_) , forall (_ : Name) , forall (_ : (sum) (Error) (t0)) , forall (_ : (sum) (Error) (t0)) , (sum) (Error) (t0) := fun (cx : Context_) => fun (flag : Name) => fun (ethen : (sum) (Error) (t0)) => fun (eelse : (sum) (Error) (t0)) => ((eithers.bind) (((hasFlag) (cx)) (flag))) (fun (b : bool) => (((logic.ifElse) (b)) (ethen)) (eelse)).
Arguments whenFlag {t0}.

