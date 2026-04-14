(* Term encoders for hydra.testing *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.testing hydra.core hydra.lib.maybes hydra.lib.lists.

Definition universalTestCase : forall (_ : UniversalTestCase) , Term := fun (x : UniversalTestCase) => (Term_Record) ((Build_Record_) ("UniversalTestCase"%string) ((cons) ((Build_Field) ("actual"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun r_ => (universalTestCase_actual) (r_)) (x)))) ((cons) ((Build_Field) ("expected"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun r_ => (universalTestCase_expected) (r_)) (x)))) (nil)))).
Definition testCase : forall (_ : TestCase) , Term := fun x_ => match x_ with
| TestCase_Universal v_ => (fun (y : UniversalTestCase) => (Term_Inject) ((Build_Injection) ("TestCase"%string) ((Build_Field) ("universal"%string) ((universalTestCase) (y))))) (v_)
end.
Definition tag : forall (_ : Tag) , Term := fun (x : Tag) => (Term_Wrap) ((Build_WrappedTerm) ("Tag"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun w_ => w_) (x)))).
Definition testCaseWithMetadata : forall (_ : TestCaseWithMetadata) , Term := fun (x : TestCaseWithMetadata) => (Term_Record) ((Build_Record_) ("TestCaseWithMetadata"%string) ((cons) ((Build_Field) ("name"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun r_ => (testCaseWithMetadata_name) (r_)) (x)))) ((cons) ((Build_Field) ("case"%string) ((testCase) ((fun r_ => (testCaseWithMetadata_case) (r_)) (x)))) ((cons) ((Build_Field) ("description"%string) ((fun (opt : (option) (string)) => (Term_Maybe) (((maybes.map) (fun (x2 : string) => (Term_Literal) ((Literal_String) (x2)))) (opt))) ((fun r_ => (testCaseWithMetadata_description) (r_)) (x)))) ((cons) ((Build_Field) ("tags"%string) ((fun (xs : (list) (Tag)) => (Term_List) (((lists.map) (tag)) (xs))) ((fun r_ => (testCaseWithMetadata_tags) (r_)) (x)))) (nil)))))).
Definition testGroup_bundle :=
  hydra_fix (fun (bundle_ : forall (_ : TestGroup) , Term) =>
    let testGroup := bundle_ in
    fun (x : TestGroup) => (Term_Record) ((Build_Record_) ("TestGroup"%string) ((cons) ((Build_Field) ("name"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun r_ => (testGroup_name) (r_)) (x)))) ((cons) ((Build_Field) ("description"%string) ((fun (opt : (option) (string)) => (Term_Maybe) (((maybes.map) (fun (x2 : string) => (Term_Literal) ((Literal_String) (x2)))) (opt))) ((fun r_ => (testGroup_description) (r_)) (x)))) ((cons) ((Build_Field) ("subgroups"%string) ((fun (xs : (list) (TestGroup)) => (Term_List) (((lists.map) (testGroup)) (xs))) ((fun r_ => (testGroup_subgroups) (r_)) (x)))) ((cons) ((Build_Field) ("cases"%string) ((fun (xs : (list) (TestCaseWithMetadata)) => (Term_List) (((lists.map) (testCaseWithMetadata)) (xs))) ((fun r_ => (testGroup_cases) (r_)) (x)))) (nil))))))).

Definition testGroup : forall (_ : TestGroup) , Term :=
  testGroup_bundle.

