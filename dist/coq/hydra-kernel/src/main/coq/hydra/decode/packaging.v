(* Term decoders for hydra.packaging *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.decode.core hydra.errors hydra.extract.core hydra.graph hydra.lib.eithers hydra.lib.maps hydra.lib.maybes hydra.lib.strings hydra.packaging.

Definition termDefinition : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TermDefinition) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (TermDefinition))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => ((eithers.bind) (((((requireField) ("term"%string)) (hydra.decode.core.term)) (fieldMap)) (cx))) (fun (field_term : Term) => ((eithers.bind) (((((requireField) ("type"%string)) ((decodeMaybe) (hydra.decode.core.typeScheme))) (fieldMap)) (cx))) (fun (field_type : (option) (TypeScheme)) => ((inr) ((Build_TermDefinition) (field_name) (field_term) (field_type))) : (sum) (DecodingError) (TermDefinition))))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (TermDefinition)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition typeDefinition : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TypeDefinition) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (TypeDefinition))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (hydra.decode.core.name)) (fieldMap)) (cx))) (fun (field_name : Name) => ((eithers.bind) (((((requireField) ("type"%string)) (hydra.decode.core.typeScheme)) (fieldMap)) (cx))) (fun (field_type : TypeScheme) => ((inr) ((Build_TypeDefinition) (field_name) (field_type))) : (sum) (DecodingError) (TypeDefinition)))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (TypeDefinition)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition definition : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Definition_) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (Definition_))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("term"%string) (fun (input : Term) => ((eithers.map) (fun (t : TermDefinition) => (Definition__Term) (t))) (((termDefinition) (cx)) (input)))) ((cons) ((pair) ("type"%string) (fun (input : Term) => ((eithers.map) (fun (t : TypeDefinition) => (Definition__Type) (t))) (((typeDefinition) (cx)) (input)))) (nil))) in (((maybes.maybe) (((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil)))))) : (sum) (DecodingError) (Definition_))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (Definition_)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => ((inl) ("expected union"%string)) : (sum) (DecodingError) (Definition_)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition fileExtension : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (FileExtension) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (FileExtension))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : string) => b)) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (string))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (DecodingError) (string)) (v_)
| _ => ((inl) ("expected string literal"%string)) : (sum) (DecodingError) (string)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => ((inl) ("expected wrapped type"%string)) : (sum) (DecodingError) (FileExtension)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition namespace : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Namespace) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (Namespace))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : string) => b)) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (string))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (DecodingError) (string)) (v_)
| _ => ((inl) ("expected string literal"%string)) : (sum) (DecodingError) (string)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => ((inl) ("expected wrapped type"%string)) : (sum) (DecodingError) (Namespace)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition module : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Module_) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (Module_))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("namespace"%string)) (namespace)) (fieldMap)) (cx))) (fun (field_namespace : Namespace) => ((eithers.bind) (((((requireField) ("definitions"%string)) ((decodeList) (definition))) (fieldMap)) (cx))) (fun (field_definitions : (list) (Definition_)) => ((eithers.bind) (((((requireField) ("termDependencies"%string)) ((decodeList) (namespace))) (fieldMap)) (cx))) (fun (field_termDependencies : (list) (Namespace)) => ((eithers.bind) (((((requireField) ("typeDependencies"%string)) ((decodeList) (namespace))) (fieldMap)) (cx))) (fun (field_typeDependencies : (list) (Namespace)) => ((eithers.bind) (((((requireField) ("description"%string)) ((decodeMaybe) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (string))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (DecodingError) (string)) (v_)
| _ => ((inl) ("expected string literal"%string)) : (sum) (DecodingError) (string)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))))) (fieldMap)) (cx))) (fun (field_description : (option) (string)) => ((inr) ((Build_Module_) (field_namespace) (field_definitions) (field_termDependencies) (field_typeDependencies) (field_description))) : (sum) (DecodingError) (Module_))))))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (Module_)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition namespaces (t0 : Type) : forall (_ : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) , forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) ((Namespaces) (t0)) := fun (n : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (t0)) => fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => (inl) (err))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("focus"%string)) (((decodePair) (namespace)) (n))) (fieldMap)) (cx))) (fun (field_focus : (prod) (Namespace) (t0)) => ((eithers.bind) (((((requireField) ("mapping"%string)) (((decodeMap) (namespace)) (n))) (fieldMap)) (cx))) (fun (field_mapping : (list) ((prod) (Namespace) (t0))) => (inr) ((Build_Namespaces) (field_focus) (field_mapping))))) (v_)
| _ => (inl) ("expected record"%string)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Arguments namespaces {t0}.
Definition packageName : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (PackageName) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (PackageName))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Wrap v_ => (fun (wrappedTerm : WrappedTerm) => ((eithers.map) (fun (b : string) => b)) (((fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (string))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (DecodingError) (string)) (v_)
| _ => ((inl) ("expected string literal"%string)) : (sum) (DecodingError) (string)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))) (cx)) ((fun r_ => (wrappedTerm_body) (r_)) (wrappedTerm)))) (v_)
| _ => ((inl) ("expected wrapped type"%string)) : (sum) (DecodingError) (PackageName)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition package : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (Package) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (Package))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("name"%string)) (packageName)) (fieldMap)) (cx))) (fun (field_name : PackageName) => ((eithers.bind) (((((requireField) ("modules"%string)) ((decodeList) (module))) (fieldMap)) (cx))) (fun (field_modules : (list) (Module_)) => ((eithers.bind) (((((requireField) ("dependencies"%string)) ((decodeList) (packageName))) (fieldMap)) (cx))) (fun (field_dependencies : (list) (PackageName)) => ((eithers.bind) (((((requireField) ("description"%string)) ((decodeMaybe) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (string))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (DecodingError) (string)) (v_)
| _ => ((inl) ("expected string literal"%string)) : (sum) (DecodingError) (string)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2))))) (fieldMap)) (cx))) (fun (field_description : (option) (string)) => ((inr) ((Build_Package) (field_name) (field_modules) (field_dependencies) (field_description))) : (sum) (DecodingError) (Package)))))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (Package)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).
Definition qualifiedName : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (QualifiedName) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (QualifiedName))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Record v_ => (fun (record : Record_) => let fieldMap := (toFieldMap) (record) in ((eithers.bind) (((((requireField) ("namespace"%string)) ((decodeMaybe) (namespace))) (fieldMap)) (cx))) (fun (field_namespace : (option) (Namespace)) => ((eithers.bind) (((((requireField) ("local"%string)) (fun (cx2 : hydra.graph.Graph) => fun (raw2 : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (string))) (fun (stripped2 : Term) => (fun x_ => match x_ with
| Term_Literal v_ => (fun (v : Literal) => (fun x_ => match x_ with
| Literal_String v_ => (fun (s : string) => ((inr) (s)) : (sum) (DecodingError) (string)) (v_)
| _ => ((inl) ("expected string literal"%string)) : (sum) (DecodingError) (string)
end) (v)) (v_)
| _ => ((inl) ("expected literal"%string)) : (sum) (DecodingError) (string)
end) (stripped2))) (((stripWithDecodingError) (cx2)) (raw2)))) (fieldMap)) (cx))) (fun (field_local : string) => ((inr) ((Build_QualifiedName) (field_namespace) (field_local))) : (sum) (DecodingError) (QualifiedName)))) (v_)
| _ => ((inl) ("expected record"%string)) : (sum) (DecodingError) (QualifiedName)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).

