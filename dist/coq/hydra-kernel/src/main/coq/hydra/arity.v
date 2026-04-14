(* Functions dealing with arguments and arity. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.lib.lists hydra.lib.math hydra.graph.

Definition uncurryType_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Type_) , (list) (Type_)) =>
    let uncurryType := bundle_ in
    fun (t : Type_) => (fun x_ => match x_ with
| Type__Annotated v_ => (fun (arg_ : AnnotatedType) => (uncurryType) ((fun r_ => (annotatedType_body) (r_)) (arg_))) (v_)
| Type__Application v_ => (fun (arg_ : ApplicationType) => (uncurryType) ((fun r_ => (applicationType_function) (r_)) (arg_))) (v_)
| Type__Forall v_ => (fun (arg_ : ForallType) => (uncurryType) ((fun r_ => (forallType_body) (r_)) (arg_))) (v_)
| Type__Function v_ => (fun (ft : FunctionType) => ((lists.cons) ((fun r_ => (functionType_domain) (r_)) (ft))) ((fun (arg_ : FunctionType) => (uncurryType) ((fun r_ => (functionType_codomain) (r_)) (arg_))) (ft))) (v_)
| _ => (cons) (t) (nil)
end) (t)).

Definition uncurryType : forall (_ : Type_) , (list) (Type_) :=
  uncurryType_bundle.
Definition typeArity_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Type_) , Z) =>
    let typeArity := bundle_ in
    fun x_ => match x_ with
| Type__Annotated v_ => (fun (arg_ : AnnotatedType) => (typeArity) ((fun r_ => (annotatedType_body) (r_)) (arg_))) (v_)
| Type__Application v_ => (fun (arg_ : ApplicationType) => (typeArity) ((fun r_ => (applicationType_function) (r_)) (arg_))) (v_)
| Type__Forall v_ => (fun (arg_ : ForallType) => (typeArity) ((fun r_ => (forallType_body) (r_)) (arg_))) (v_)
| Type__Function v_ => (fun (f : FunctionType) => ((math.add) ((1)%Z)) ((fun (arg_ : FunctionType) => (typeArity) ((fun r_ => (functionType_codomain) (r_)) (arg_))) (f))) (v_)
| _ => (0)%Z
end).

Definition typeArity : forall (_ : Type_) , Z :=
  typeArity_bundle.
Definition typeSchemeArity : forall (_ : TypeScheme) , Z := fun (arg_ : TypeScheme) => (typeArity) ((fun r_ => (typeScheme_type) (r_)) (arg_)).
Definition termArity_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : Term) , Z) =>
    let termArity := bundle_ in
    fun x_ => match x_ with
| Term_Application v_ => (fun (arg_ : Application) => (fun (arg_2 : Term) => (fun (xapp : Z) => ((math.sub) (xapp)) ((1)%Z)) ((termArity) (arg_2))) ((fun r_ => (application_function) (r_)) (arg_))) (v_)
| Term_Cases v_ => (fun (_ : CaseStatement) => (1)%Z) (v_)
| Term_Lambda v_ => (fun (arg_ : Lambda) => (fun (i : Z) => ((math.add) ((1)%Z)) (i)) ((fun (arg_2 : Lambda) => (termArity) ((fun r_ => (lambda_body) (r_)) (arg_2))) (arg_))) (v_)
| Term_Project v_ => (fun (_ : Projection) => (1)%Z) (v_)
| Term_Unwrap v_ => (fun (_ : Name) => (1)%Z) (v_)
| _ => (0)%Z
end).

Definition termArity : forall (_ : Term) , Z :=
  termArity_bundle.
Definition primitiveArity : forall (_ : Primitive) , Z := fun (arg_ : Primitive) => (fun (arg_2 : TypeScheme) => (typeArity) ((fun r_ => (typeScheme_type) (r_)) (arg_2))) ((fun r_ => (primitive_type) (r_)) (arg_)).

