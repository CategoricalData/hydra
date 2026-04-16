(* A model for unit testing *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.
Definition Tag : Type := string.

Record UniversalTestCase : Type := Build_UniversalTestCase {
universalTestCase_actual : string ;
universalTestCase_expected : string ;
}.

Inductive TestCase : Type :=
| TestCase_Universal : forall (_ : UniversalTestCase) , TestCase.

Record TestCaseWithMetadata : Type := Build_TestCaseWithMetadata {
testCaseWithMetadata_name : string ;
testCaseWithMetadata_case : TestCase ;
testCaseWithMetadata_description : (option) (string) ;
testCaseWithMetadata_tags : (list) (Tag) ;
}.

Inductive TestGroup : Type :=
| Build_TestGroup : forall (_ : string) , forall (_ : (option) (string)) , forall (_ : (list) (TestGroup)) , forall (_ : (list) (TestCaseWithMetadata)) , TestGroup.

Definition testGroup_name (r_ : TestGroup) := match r_ with
| Build_TestGroup f0 f1 f2 f3 => f0
end.

Definition testGroup_description (r_ : TestGroup) := match r_ with
| Build_TestGroup f0 f1 f2 f3 => f1
end.

Definition testGroup_subgroups (r_ : TestGroup) := match r_ with
| Build_TestGroup f0 f1 f2 f3 => f2
end.

Definition testGroup_cases (r_ : TestGroup) := match r_ with
| Build_TestGroup f0 f1 f2 f3 => f3
end.

