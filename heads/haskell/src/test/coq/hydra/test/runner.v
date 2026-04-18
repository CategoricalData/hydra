(* Hand-written Coq test runner for the Hydra common test suite.

   Walks each included test module's `allTests : TestGroup`, flattens
   into a list of (qualified name, UniversalTestCase), runs each via
   String.eqb on the actual/expected strings, and reports a
   pass/fail summary.

   Currently EXCLUDES four test modules that do not compile under
   coqc today:
     - hydra.test.lib.maps     (bare nil for empty Term_Map)
     - hydra.test.lib.sets     (bare nil for empty Term_Set)
     - hydra.test.json.yaml    (requires yaml kernel modules not emitted)
     - hydra.test.testSuite    (aggregate of the above)
   See feature_326_coq-plan.md "Known gaps to revisit".

   Note: most test cases are expected to fail until the Hydra runtime
   axioms (hydra_eq, hydra_compare) are specialised via rewrite lemmas
   for the concrete types used in tests. This runner is a measurement
   tool for that specialisation work, not a correctness proof. *)

Require Import Stdlib.Strings.String.
Require Import Stdlib.Lists.List.
Require Import Stdlib.ZArith.ZArith.
Require Import hydra.lib.base.
Require Import hydra.testing.
Import ListNotations.
Open Scope string_scope.

(* Included test modules (the 30 that compile under coqc today).
   The 4 excluded modules are listed in the header comment. *)
Require hydra.test.annotations.
Require hydra.test.checking.all.
Require hydra.test.dependencies.
Require hydra.test.differentiation.
Require hydra.test.etaExpansion.
Require hydra.test.formatting.
Require hydra.test.generation.
Require hydra.test.hoisting.all.
Require hydra.test.inference.all.
Require hydra.test.json.roundtrip.
Require hydra.test.json.writer.
Require hydra.test.lib.chars.
Require hydra.test.lib.eithers.
Require hydra.test.lib.equality.
Require hydra.test.lib.lists.
Require hydra.test.lib.literals.
Require hydra.test.lib.logic.
Require hydra.test.lib.math.
Require hydra.test.lib.maybes.
Require hydra.test.lib.pairs.
Require hydra.test.lib.regex.
Require hydra.test.lib.strings.
Require hydra.test.reduction.
Require hydra.test.rewriting.
Require hydra.test.serialization.
Require hydra.test.sorting.
Require hydra.test.strip.
Require hydra.test.substitution.
Require hydra.test.unification.
Require hydra.test.validate.all.
Require hydra.test.variables.

(* -----------------------------------------------------------------
   Test evaluation
   ----------------------------------------------------------------- *)

(* Run a UniversalTestCase: actual and expected are already strings. *)
Definition run_universal_test_case (tc : UniversalTestCase) : bool :=
  String.eqb
    (universalTestCase_actual tc)
    (universalTestCase_expected tc).

Definition run_test_case (tc : TestCase) : bool :=
  match tc with
  | TestCase_Universal u => run_universal_test_case u
  end.

(* Flatten a TestGroup tree into a list of (qualified_name, bool).
   The qualified name is the dot-joined path from the root group to
   the test case. *)
Fixpoint flatten_group (prefix : string) (g : TestGroup) {struct g}
  : list (string * bool) :=
  match g with
  | Build_TestGroup name _ subgroups cases =>
      let qualified :=
          match prefix with
          | EmptyString => name
          | _ => prefix ++ "." ++ name
          end in
      let case_results :=
          map (fun twm =>
                 let full := qualified ++ "." ++ testCaseWithMetadata_name twm in
                 (full, run_test_case (testCaseWithMetadata_case twm)))
              cases in
      let sub_results :=
          flat_map (flatten_group qualified) subgroups in
      case_results ++ sub_results
  end.

(* -----------------------------------------------------------------
   Aggregate all included module allTests
   ----------------------------------------------------------------- *)

Definition allIncludedModules : list TestGroup := [
  hydra.test.annotations.allTests;
  hydra.test.checking.all.allTests;
  hydra.test.dependencies.allTests;
  hydra.test.differentiation.allTests;
  hydra.test.etaExpansion.allTests;
  hydra.test.formatting.allTests;
  hydra.test.generation.allTests;
  hydra.test.hoisting.all.allTests;
  hydra.test.inference.all.allTests;
  hydra.test.json.roundtrip.allTests;
  hydra.test.json.writer.allTests;
  hydra.test.lib.chars.allTests;
  hydra.test.lib.eithers.allTests;
  hydra.test.lib.equality.allTests;
  hydra.test.lib.lists.allTests;
  hydra.test.lib.literals.allTests;
  hydra.test.lib.logic.allTests;
  hydra.test.lib.math.allTests;
  hydra.test.lib.maybes.allTests;
  hydra.test.lib.pairs.allTests;
  hydra.test.lib.regex.allTests;
  hydra.test.lib.strings.allTests;
  hydra.test.reduction.allTests;
  hydra.test.rewriting.allTests;
  hydra.test.serialization.allTests;
  hydra.test.sorting.allTests;
  hydra.test.strip.allTests;
  hydra.test.substitution.allTests;
  hydra.test.unification.allTests;
  hydra.test.validate.all.allTests;
  hydra.test.variables.allTests
].

Definition allResults : list (string * bool) :=
  flat_map (flatten_group ""%string) allIncludedModules.

(* -----------------------------------------------------------------
   Summary statistics
   ----------------------------------------------------------------- *)

Definition countWhere (p : string * bool -> bool) : nat :=
  length (filter p allResults).

Definition total : nat := length allResults.
Definition passed : nat := countWhere (fun p => snd p).
Definition failed : nat := countWhere (fun p => negb (snd p)).

(* List the names of failing tests. *)
Definition failingNames : list string :=
  map fst (filter (fun p => negb (snd p)) allResults).

(* -----------------------------------------------------------------
   Structure-only counting (avoids touching test actuals / expecteds)

   These definitions walk the TestGroup tree without ever projecting
   out the UniversalTestCase strings, so reduction doesn't stall on
   axiom-containing `actual` fields.
   ----------------------------------------------------------------- *)

Fixpoint countCases (g : TestGroup) {struct g} : nat :=
  match g with
  | Build_TestGroup _ _ subgroups cases =>
      length cases + fold_right (fun h acc => countCases h + acc) O subgroups
  end.

Definition totalCases : nat :=
  fold_right (fun g acc => countCases g + acc) O allIncludedModules.

(* NOTE on evaluation:

   `vm_compute` would force reduction of every test assertion at compile
   time, giving us a pass/fail count — but most Hydra test actuals call
   through the kernel (reduceTerm, show.core.term, etc.), which in turn
   depend on the escape-hatch axioms in hydra.lib.base (hydra_fix,
   hydra_eq, hydra_compare, hydra_unreachable). Coq cannot reduce
   through an axiom, so `vm_compute` stalls indefinitely.

   Getting actual pass counts requires specialising those axioms via
   per-type rewrite lemmas (e.g. realising `hydra_eq` at `Z`, `string`,
   `Name`, `Term`). That is future work; see
   feature_326_coq-plan.md gap 3.

   For now, this runner succeeds if all 31 included test modules
   type-check: `allResults` is well-formed but not reduced. Uncomment
   the following to explore interactively:

     (* Eval vm_compute in total. *)
     (* Eval vm_compute in passed. *)
     (* Eval vm_compute in failed. *)
     (* Eval vm_compute in failingNames. *)
*)
