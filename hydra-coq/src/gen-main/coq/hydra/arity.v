(* Functions dealing with arguments and arity. *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.lib.lists hydra.lib.math hydra.graph.

Definition uncurryType_bundle :=
  hydra_fix (fun (bundle_ : Type_ -> (list) (Type_)) =>
    let uncurryType := bundle_ in
    fun (t : Type_) => (fun x_ => match x_ with
| Type__Annotated v_ => (fun (arg_ : AnnotatedType) => (uncurryType) ((fun r_ => (annotatedType_body) (r_)) (arg_))) (v_)
| Type__Application v_ => (fun (arg_ : ApplicationType) => (uncurryType) ((fun r_ => (applicationType_function) (r_)) (arg_))) (v_)
| Type__Forall v_ => (fun (arg_ : ForallType) => (uncurryType) ((fun r_ => (forallType_body) (r_)) (arg_))) (v_)
| Type__Function v_ => (fun (ft : FunctionType) => ((lists.cons) ((fun r_ => (functionType_domain) (r_)) (ft))) ((fun (arg_ : FunctionType) => (uncurryType) ((fun r_ => (functionType_codomain) (r_)) (arg_))) (ft))) (v_)
| _ => (cons) (t) (nil)
end) (t)).

Definition uncurryType : Type_ -> (list) (Type_) :=
  uncurryType_bundle.
Definition typeArity_bundle :=
  hydra_fix (fun (bundle_ : Type_ -> Z) =>
    let typeArity := bundle_ in
    fun x_ => match x_ with
| Type__Annotated v_ => (fun (arg_ : AnnotatedType) => (typeArity) ((fun r_ => (annotatedType_body) (r_)) (arg_))) (v_)
| Type__Application v_ => (fun (arg_ : ApplicationType) => (typeArity) ((fun r_ => (applicationType_function) (r_)) (arg_))) (v_)
| Type__Forall v_ => (fun (arg_ : ForallType) => (typeArity) ((fun r_ => (forallType_body) (r_)) (arg_))) (v_)
| Type__Function v_ => (fun (f : FunctionType) => ((math.add) ((1)%Z)) ((fun (arg_ : FunctionType) => (typeArity) ((fun r_ => (functionType_codomain) (r_)) (arg_))) (f))) (v_)
| _ => (0)%Z
end).

Definition typeArity : Type_ -> Z :=
  typeArity_bundle.
Definition typeSchemeArity : TypeScheme -> Z :=
  fun (arg_ : TypeScheme) => (typeArity) ((fun r_ => (typeScheme_type) (r_)) (arg_)).
Definition primitiveArity : Primitive -> Z :=
  fun (arg_ : Primitive) => (fun (arg_2 : TypeScheme) => (typeArity) ((fun r_ => (typeScheme_type) (r_)) (arg_2))) ((fun r_ => (primitive_type) (r_)) (arg_)).
Definition functionArity_termArity_bundle :=
  hydra_fix (fun (bundle_ : prod (Function -> Z) (Term -> Z)) =>
    let functionArity := (fst bundle_) in
    let termArity := (snd bundle_) in
    (pair (fun x_ => match x_ with
| Function_Elimination v_ => (fun (_ : Elimination) => (1)%Z) (v_)
| Function_Lambda v_ => (fun (arg_ : Lambda) => (fun (i : Z) => ((math.add) ((1)%Z)) (i)) ((fun (arg_2 : Lambda) => (termArity) ((fun r_ => (lambda_body) (r_)) (arg_2))) (arg_))) (v_)
end) (fun x_ => match x_ with
| Term_Application v_ => (fun (arg_ : Application) => (fun (arg_2 : Term) => (fun (xapp : Z) => ((math.sub) (xapp)) ((1)%Z)) ((termArity) (arg_2))) ((fun r_ => (application_function) (r_)) (arg_))) (v_)
| Term_Function v_ => (functionArity) (v_)
| _ => (0)%Z
end))).

Definition functionArity : Function -> Z :=
  (fst functionArity_termArity_bundle).
Definition termArity : Term -> Z :=
  (snd functionArity_termArity_bundle).

