(* Hand-written Coq test runner for the Hydra common test suite.

   Walks each included test module's `allTests : TestGroup`, flattens
   into a list of (qualified name, UniversalTestCase), runs each via
   String.eqb on the actual/expected strings, and reports a
   pass/fail summary.

   Currently EXCLUDES four test modules that do not compile under
   coqc today:
     - hydra.core.test.lib.maps     (bare nil for empty Term_Map)
     - hydra.core.test.lib.sets     (bare nil for empty Term_Set)
     - hydra.core.test.json.yaml    (requires yaml kernel modules not emitted)
     - hydra.core.test.testSuite    (aggregate of the above)
   See feature_326_coq-plan.md "Known gaps to revisit".

   Note: most test cases are expected to fail until the Hydra runtime
   axioms (hydra_eq, hydra_compare) are specialised via rewrite lemmas
   for the concrete types used in tests. This runner is a measurement
   tool for that specialisation work, not a correctness proof. *)

Require Import Stdlib.Strings.String.
Require Import Stdlib.Lists.List.
Require Import Stdlib.ZArith.ZArith.
Require Import hydra.lib.base.
Require Import hydra.core.testing.
Import ListNotations.
Open Scope string_scope.

(* Included test modules (the 30 that compile under coqc today).
   The 4 excluded modules are listed in the header comment. *)
Require hydra.core.test.annotations.
Require hydra.core.test.checking.all.
Require hydra.core.test.dependencies.
Require hydra.core.test.differentiation.
Require hydra.core.test.etaExpansion.
Require hydra.core.test.formatting.
Require hydra.core.test.generation.
Require hydra.core.test.hoisting.all.
Require hydra.core.test.inference.all.
Require hydra.core.test.json.roundtrip.
Require hydra.core.test.json.writer.
Require hydra.core.test.lib.chars.
Require hydra.core.test.lib.eithers.
Require hydra.core.test.lib.equality.
Require hydra.core.test.lib.lists.
Require hydra.core.test.lib.literals.
Require hydra.core.test.lib.logic.
Require hydra.core.test.lib.math.
Require hydra.core.test.lib.optionals.
Require hydra.core.test.lib.pairs.
Require hydra.core.test.lib.regex.
Require hydra.core.test.lib.strings.
Require hydra.core.test.reduction.
Require hydra.core.test.rewriting.
Require hydra.core.test.serialization.
Require hydra.core.test.sorting.
Require hydra.core.test.strip.
Require hydra.core.test.substitution.
Require hydra.core.test.unification.
Require hydra.core.test.validate.all.
Require hydra.core.test.variables.

(* -----------------------------------------------------------------
   Test evaluation
   ----------------------------------------------------------------- *)

(* Run a UniversalTestCase: actual and expected are unit-thunks (Hydra `\_. body`,
   emitted as `unit -> string` in Coq). Apply each to tt to obtain the string. For #311. *)
Definition run_universal_test_case (tc : UniversalTestCase) : bool :=
  String.eqb
    (universalTestCase_actual tc tt)
    (universalTestCase_expected tc tt).

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
  hydra.core.test.annotations.allTests;
  hydra.core.test.checking.all.allTests;
  hydra.core.test.dependencies.allTests;
  hydra.core.test.differentiation.allTests;
  hydra.core.test.etaExpansion.allTests;
  hydra.core.test.formatting.allTests;
  hydra.core.test.generation.allTests;
  hydra.core.test.hoisting.all.allTests;
  hydra.core.test.inference.all.allTests;
  hydra.core.test.json.roundtrip.allTests;
  hydra.core.test.json.writer.allTests;
  hydra.core.test.lib.chars.allTests;
  hydra.core.test.lib.eithers.allTests;
  hydra.core.test.lib.equality.allTests;
  hydra.core.test.lib.lists.allTests;
  hydra.core.test.lib.literals.allTests;
  hydra.core.test.lib.logic.allTests;
  hydra.core.test.lib.math.allTests;
  hydra.core.test.lib.optionals.allTests;
  hydra.core.test.lib.pairs.allTests;
  hydra.core.test.lib.regex.allTests;
  hydra.core.test.lib.strings.allTests;
  hydra.core.test.reduction.allTests;
  hydra.core.test.rewriting.allTests;
  hydra.core.test.serialization.allTests;
  hydra.core.test.sorting.allTests;
  hydra.core.test.strip.allTests;
  hydra.core.test.substitution.allTests;
  hydra.core.test.unification.allTests;
  hydra.core.test.validate.all.allTests;
  hydra.core.test.variables.allTests
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
