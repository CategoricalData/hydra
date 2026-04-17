(* Annotation and type stripping and normalization *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.lib.lists hydra.rewriting.

Definition deannotateAndDetypeTerm_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Term) , Term) =>
    let deannotateAndDetypeTerm := bundle_ in
    fun (t : Term) => (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => (deannotateAndDetypeTerm) ((fun r_ => (annotatedTerm_body) (r_)) (at_))) (v_)
| Term_TypeApplication v_ => (fun (tt : TypeApplicationTerm) => (deannotateAndDetypeTerm) ((fun r_ => (typeApplicationTerm_body) (r_)) (tt))) (v_)
| Term_TypeLambda v_ => (fun (ta : TypeLambda) => (deannotateAndDetypeTerm) ((fun r_ => (typeLambda_body) (r_)) (ta))) (v_)
| _ => t
end) (t)).

Definition deannotateAndDetypeTerm : forall (_ : Term) , Term :=
  deannotateAndDetypeTerm_bundle.
Definition deannotateTerm_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Term) , Term) =>
    let deannotateTerm := bundle_ in
    fun (t : Term) => (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => (deannotateTerm) ((fun r_ => (annotatedTerm_body) (r_)) (at_))) (v_)
| _ => t
end) (t)).

Definition deannotateTerm : forall (_ : Term) , Term :=
  deannotateTerm_bundle.
Definition deannotateType_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Type_) , Type_) =>
    let deannotateType := bundle_ in
    fun (t : Type_) => (fun x_ => match x_ with
| Type__Annotated v_ => (fun (arg_ : AnnotatedType) => (deannotateType) ((fun r_ => (annotatedType_body) (r_)) (arg_))) (v_)
| _ => t
end) (t)).

Definition deannotateType : forall (_ : Type_) , Type_ :=
  deannotateType_bundle.
Definition deannotateTypeParameters_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Type_) , Type_) =>
    let deannotateTypeParameters := bundle_ in
    fun (t : Type_) => (fun x_ => match x_ with
| Type__Forall v_ => (fun (lt : ForallType) => (deannotateTypeParameters) ((fun r_ => (forallType_body) (r_)) (lt))) (v_)
| _ => t
end) ((deannotateType) (t))).

Definition deannotateTypeParameters : forall (_ : Type_) , Type_ :=
  deannotateTypeParameters_bundle.
Definition deannotateTypeRecursive : forall (_ : Type_) , Type_ := fun (typ : Type_) => let strip := fun recurse => fun typ2 => let rewritten := (recurse) (typ2) in (fun x_ => match x_ with
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => (fun r_ => (annotatedType_body) (r_)) (at_)) (v_)
| _ => rewritten
end) (rewritten) in ((rewriteType) (strip)) (typ).
Definition deannotateTypeSchemeRecursive : forall (_ : TypeScheme) , TypeScheme := fun (ts : TypeScheme) => let constraints := (fun r_ => (typeScheme_constraints) (r_)) (ts) in let typ := (fun r_ => (typeScheme_type) (r_)) (ts) in let vars := (fun r_ => (typeScheme_variables) (r_)) (ts) in (Build_TypeScheme) (vars) ((deannotateTypeRecursive) (typ)) (constraints).
Definition detypeTerm_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Term) , Term) =>
    let detypeTerm := bundle_ in
    fun (t : Term) => (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => let ann := (fun r_ => (annotatedTerm_annotation) (r_)) (at_) in let subj := (fun r_ => (annotatedTerm_body) (r_)) (at_) in (Term_Annotated) ((Build_AnnotatedTerm) ((detypeTerm) (subj)) (ann))) (v_)
| Term_TypeApplication v_ => (fun (tt : TypeApplicationTerm) => (deannotateAndDetypeTerm) ((fun r_ => (typeApplicationTerm_body) (r_)) (tt))) (v_)
| Term_TypeLambda v_ => (fun (ta : TypeLambda) => (deannotateAndDetypeTerm) ((fun r_ => (typeLambda_body) (r_)) (ta))) (v_)
| _ => t
end) (t)).

Definition detypeTerm : forall (_ : Term) , Term :=
  detypeTerm_bundle.
Definition removeTermAnnotations : forall (_ : Term) , Term := fun (term_ : Term) => let remove := fun (recurse : forall (_ : Term) , Term) => fun (term2 : Term) => let rewritten := (recurse) (term2) in (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => (fun r_ => (annotatedTerm_body) (r_)) (at_)) (v_)
| _ => rewritten
end) (term2) in ((rewriteTerm) (remove)) (term_).
Definition removeTypeAnnotations : forall (_ : Type_) , Type_ := fun (typ : Type_) => let remove := fun recurse => fun typ2 => let rewritten := (recurse) (typ2) in (fun x_ => match x_ with
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => (fun r_ => (annotatedType_body) (r_)) (at_)) (v_)
| _ => rewritten
end) (rewritten) in ((rewriteType) (remove)) (typ).
Definition removeTypeAnnotationsFromTerm : forall (_ : Term) , Term := fun (term_ : Term) => let strip := fun recurse => fun term2 => let rewritten := (recurse) (term2) in let stripBinding := fun (b : Binding) => (Build_Binding) ((fun r_ => (binding_name) (r_)) (b)) ((fun r_ => (binding_term) (r_)) (b)) ((None) : (option) (TypeScheme)) in (fun x_ => match x_ with
| Term_Let v_ => (fun (lt : Let) => (Term_Let) ((Build_Let) (((lists.map) (stripBinding)) ((fun r_ => (let_bindings) (r_)) (lt))) ((fun r_ => (let_body) (r_)) (lt)))) (v_)
| Term_TypeApplication v_ => (fun (tt : TypeApplicationTerm) => (fun r_ => (typeApplicationTerm_body) (r_)) (tt)) (v_)
| Term_TypeLambda v_ => (fun (ta : TypeLambda) => (fun r_ => (typeLambda_body) (r_)) (ta)) (v_)
| _ => rewritten
end) (rewritten) in ((rewriteTerm) (strip)) (term_).
Definition removeTypesFromTerm : forall (_ : Term) , Term := fun (term_ : Term) => let strip := fun recurse => fun term2 => let rewritten := (recurse) (term2) in let stripBinding := fun (b : Binding) => (Build_Binding) ((fun r_ => (binding_name) (r_)) (b)) ((fun r_ => (binding_term) (r_)) (b)) ((None) : (option) (TypeScheme)) in (fun x_ => match x_ with
| Term_Lambda v_ => (fun (l : Lambda) => (Term_Lambda) ((Build_Lambda) ((fun r_ => (lambda_parameter) (r_)) (l)) ((None) : (option) (Type_)) ((fun r_ => (lambda_body) (r_)) (l)))) (v_)
| Term_Let v_ => (fun (lt : Let) => (Term_Let) ((Build_Let) (((lists.map) (stripBinding)) ((fun r_ => (let_bindings) (r_)) (lt))) ((fun r_ => (let_body) (r_)) (lt)))) (v_)
| Term_TypeApplication v_ => (fun (tt : TypeApplicationTerm) => (fun r_ => (typeApplicationTerm_body) (r_)) (tt)) (v_)
| Term_TypeLambda v_ => (fun (ta : TypeLambda) => (fun r_ => (typeLambda_body) (r_)) (ta)) (v_)
| _ => rewritten
end) (rewritten) in ((rewriteTerm) (strip)) (term_).
Definition stripTypeLambdas_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Term) , Term) =>
    let stripTypeLambdas := bundle_ in
    fun (t : Term) => (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => let ann := (fun r_ => (annotatedTerm_annotation) (r_)) (at_) in let subj := (fun r_ => (annotatedTerm_body) (r_)) (at_) in (Term_Annotated) ((Build_AnnotatedTerm) ((stripTypeLambdas) (subj)) (ann))) (v_)
| Term_TypeLambda v_ => (fun (ta : TypeLambda) => (stripTypeLambdas) ((fun r_ => (typeLambda_body) (r_)) (ta))) (v_)
| _ => t
end) (t)).

Definition stripTypeLambdas : forall (_ : Term) , Term :=
  stripTypeLambdas_bundle.

