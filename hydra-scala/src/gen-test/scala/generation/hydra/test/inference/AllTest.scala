// Note: this is an automatically generated file. Do not edit.
// inference

package generation.hydra.test.inference

import org.scalatest.funsuite.AnyFunSuite

class AllTest extends AnyFunSuite {

  // Algebraic terms

  // Collection primitives

  // maps.map applied to a function

  test("Algebraic terms - Collection primitives - maps.map applied to a function - #1") {

    assert((

      (forall t0. (ordering t0) => (map<t0, int32> → map<t0, int32>))) == (

      (forall t0. (ordering t0) => (map<t0, int32> → map<t0, int32>))))

  }

  test("Algebraic terms - Collection primitives - maps.map applied to a function - #2") {

    assert((

      (forall t0,t1. (ordering t0) => (map<t0, t1> → map<t0, list<t1>>))) == (

      (forall t0,t1. (ordering t0) => (map<t0, t1> → map<t0, list<t1>>))))

  }

  test("Algebraic terms - Collection primitives - maps.map applied to a function - #3") {

    assert((

      (forall t0,t1. (ordering t0, ordering t1) => (map<t0, list<t1>> → map<t0, set<t1>>))) == (

      (forall t0,t1. (ordering t0, ordering t1) => (map<t0, list<t1>> → map<t0, set<t1>>))))

  }

  // sets.map applied to a function

  test("Algebraic terms - Collection primitives - sets.map applied to a function - #1") {

    assert((

      ((set<int32> → set<int32>))) == (

      ((set<int32> → set<int32>))))

  }

  test("Algebraic terms - Collection primitives - sets.map applied to a function - #2") {

    assert((

      (forall t0. (ordering t0) => (set<list<t0>> → set<int32>))) == (

      (forall t0. (ordering t0) => (set<list<t0>> → set<int32>))))

  }

  // Composing collection primitives in let

  test("Algebraic terms - Collection primitives - Composing collection primitives in let - #1") {

    assert((

      (forall t0,t1. (ordering t0, ordering t1) => (map<t0, list<t1>> → map<t0, set<t1>>))) == (

      (forall t0,t1. (ordering t0, ordering t1) => (map<t0, list<t1>> → map<t0, set<t1>>))))

  }

  test("Algebraic terms - Collection primitives - Composing collection primitives in let - #2") {

    assert((

      (map<string, set<int32>>)) == (

      (map<string, set<int32>>)))

  }

  // Map operations in lambdas

  test("Algebraic terms - Collection primitives - Map operations in lambdas - #1") {

    assert((

      (forall t0,t1. (ordering t0) => (map<t0, list<t1>> → map<t0, int32>))) == (

      (forall t0,t1. (ordering t0) => (map<t0, list<t1>> → map<t0, int32>))))

  }

  test("Algebraic terms - Collection primitives - Map operations in lambdas - #2") {

    assert((

      (forall t0,t1,t2. (ordering t2) => ((t0 → t1) → map<t2, t0> → map<t2, t1>))) == (

      (forall t0,t1,t2. (ordering t2) => ((t0 → t1) → map<t2, t0> → map<t2, t1>))))

  }

  // Fully applied collection conversions

  test("Algebraic terms - Collection primitives - Fully applied collection conversions - #1") {

    assert((

      (set<int32>)) == (

      (set<int32>)))

  }

  test("Algebraic terms - Collection primitives - Fully applied collection conversions - #2") {

    assert((

      (map<int32, int32>)) == (

      (map<int32, int32>)))

  }

  test("Algebraic terms - Collection primitives - Fully applied collection conversions - #3") {

    assert((

      (map<string, set<int32>>)) == (

      (map<string, set<int32>>)))

  }

  // Either terms

  // Left values

  test("Algebraic terms - Either terms - Left values - #1") {

    assert((

      (list<either<string, int32>>)) == (

      (list<either<string, int32>>)))

  }

  test("Algebraic terms - Either terms - Left values - #2") {

    assert((

      (forall t0. either<string, t0>)) == (

      (forall t0. either<string, t0>)))

  }

  // Right values

  test("Algebraic terms - Either terms - Right values - #1") {

    assert((

      (list<either<string, int32>>)) == (

      (list<either<string, int32>>)))

  }

  test("Algebraic terms - Either terms - Right values - #2") {

    assert((

      (forall t0. either<t0, int32>)) == (

      (forall t0. either<t0, int32>)))

  }

  // Polymorphic either values

  test("Algebraic terms - Either terms - Polymorphic either values - #1") {

    assert((

      (forall t0,t1. either<list<t0>, t1>)) == (

      (forall t0,t1. either<list<t0>, t1>)))

  }

  test("Algebraic terms - Either terms - Polymorphic either values - #2") {

    assert((

      (forall t0,t1. either<t0, list<t1>>)) == (

      (forall t0,t1. either<t0, list<t1>>)))

  }

  // Nested either values

  test("Algebraic terms - Either terms - Nested either values - #1") {

    assert((

      (list<either<either<int32, string>, boolean>>)) == (

      (list<either<either<int32, string>, boolean>>)))

  }

  test("Algebraic terms - Either terms - Nested either values - #2") {

    assert((

      (list<either<string, either<int32, boolean>>>)) == (

      (list<either<string, either<int32, boolean>>>)))

  }

  // Either in lambda

  test("Algebraic terms - Either terms - Either in lambda - #1") {

    assert((

      (forall t0,t1. (t0 → either<t0, t1>))) == (

      (forall t0,t1. (t0 → either<t0, t1>))))

  }

  test("Algebraic terms - Either terms - Either in lambda - #2") {

    assert((

      (forall t0,t1. (t0 → either<t1, t0>))) == (

      (forall t0,t1. (t0 → either<t1, t0>))))

  }

  // Either in data structures

  test("Algebraic terms - Either terms - Either in data structures - #1") {

    assert((

      (list<either<string, int32>>)) == (

      (list<either<string, int32>>)))

  }

  test("Algebraic terms - Either terms - Either in data structures - #2") {

    assert((

      (forall t0. (list<either<string, int32>>, list<t0>))) == (

      (forall t0. (list<either<string, int32>>, list<t0>))))

  }

  // Eliminations

  // List eliminations (folds)

  test("Algebraic terms - Eliminations - List eliminations (folds) - #1") {

    assert((

      ((int32 → list<int32> → int32))) == (

      ((int32 → list<int32> → int32))))

  }

  test("Algebraic terms - Eliminations - List eliminations (folds) - #2") {

    assert((

      ((list<int32> → int32))) == (

      ((list<int32> → int32))))

  }

  test("Algebraic terms - Eliminations - List eliminations (folds) - #3") {

    assert((

      (int32)) == (

      (int32)))

  }

  // Optional eliminations

  test("Algebraic terms - Eliminations - Optional eliminations - #1") {

    assert((

      ((maybe<int32> → int32))) == (

      ((maybe<int32> → int32))))

  }

  test("Algebraic terms - Eliminations - Optional eliminations - #2") {

    assert((

      (int32)) == (

      (int32)))

  }

  test("Algebraic terms - Eliminations - Optional eliminations - #3") {

    assert((

      (int32)) == (

      (int32)))

  }

  test("Algebraic terms - Eliminations - Optional eliminations - #4") {

    assert((

      (forall t0. (maybe<t0> → maybe<t0>))) == (

      (forall t0. (maybe<t0> → maybe<t0>))))

  }

  test("Algebraic terms - Eliminations - Optional eliminations - #5") {

    assert((

      (forall t0. (maybe<t0> → list<t0>))) == (

      (forall t0. (maybe<t0> → list<t0>))))

  }

  // List terms

  // List of strings

  test("Algebraic terms - List terms - List of strings - #1") {

    assert((

      (list<string>)) == (

      (list<string>)))

  }

  // List of lists of strings

  test("Algebraic terms - List terms - List of lists of strings - #1") {

    assert((

      (list<list<string>>)) == (

      (list<list<string>>)))

  }

  // Empty list

  test("Algebraic terms - List terms - Empty list - #1") {

    assert((

      (forall t0. list<t0>)) == (

      (forall t0. list<t0>)))

  }

  // List containing an empty list

  test("Algebraic terms - List terms - List containing an empty list - #1") {

    assert((

      (forall t0. list<list<t0>>)) == (

      (forall t0. list<list<t0>>)))

  }

  // Lambda producing a polymorphic list

  test("Algebraic terms - List terms - Lambda producing a polymorphic list - #1") {

    assert((

      (forall t0. (t0 → list<t0>))) == (

      (forall t0. (t0 → list<t0>))))

  }

  // Lambda producing a list of integers

  test("Algebraic terms - List terms - Lambda producing a list of integers - #1") {

    assert((

      ((int32 → list<int32>))) == (

      ((int32 → list<int32>))))

  }

  // List with repeated variables

  test("Algebraic terms - List terms - List with repeated variables - #1") {

    assert((

      ((string → list<string>))) == (

      ((string → list<string>))))

  }

  // Map terms

  test("Algebraic terms - Map terms - #1") {

    assert((

      (map<string, string>)) == (

      (map<string, string>)))

  }

  test("Algebraic terms - Map terms - #2") {

    assert((

      (forall t0,t1. (ordering t0) => map<t0, t1>)) == (

      (forall t0,t1. (ordering t0) => map<t0, t1>)))

  }

  test("Algebraic terms - Map terms - #3") {

    assert((

      (forall t0. (ordering t0) => (t0 → t0 → map<t0, float64>))) == (

      (forall t0. (ordering t0) => (t0 → t0 → map<t0, float64>))))

  }

  // Optional terms

  test("Algebraic terms - Optional terms - #1") {

    assert((

      (maybe<int32>)) == (

      (maybe<int32>)))

  }

  test("Algebraic terms - Optional terms - #2") {

    assert((

      (forall t0. maybe<t0>)) == (

      (forall t0. maybe<t0>)))

  }

  // Pair terms

  // Monotyped pairs

  test("Algebraic terms - Pair terms - Monotyped pairs - #1") {

    assert((

      ((string, int32))) == (

      ((string, int32))))

  }

  test("Algebraic terms - Pair terms - Monotyped pairs - #2") {

    assert((

      ((string, list<float32>))) == (

      ((string, list<float32>))))

  }

  // Polytyped pairs

  test("Algebraic terms - Pair terms - Polytyped pairs - #1") {

    assert((

      (forall t0. (list<t0>, string))) == (

      (forall t0. (list<t0>, string))))

  }

  test("Algebraic terms - Pair terms - Polytyped pairs - #2") {

    assert((

      (forall t0,t1. (list<t0>, list<t1>))) == (

      (forall t0,t1. (list<t0>, list<t1>))))

  }

  // Nested pairs

  test("Algebraic terms - Pair terms - Nested pairs - #1") {

    assert((

      (((int32, string), boolean))) == (

      (((int32, string), boolean))))

  }

  test("Algebraic terms - Pair terms - Nested pairs - #2") {

    assert((

      ((string, (int32, list<float32>)))) == (

      ((string, (int32, list<float32>)))))

  }

  // Pairs in lambda

  test("Algebraic terms - Pair terms - Pairs in lambda - #1") {

    assert((

      (forall t0. (t0 → (t0, string)))) == (

      (forall t0. (t0 → (t0, string)))))

  }

  test("Algebraic terms - Pair terms - Pairs in lambda - #2") {

    assert((

      (forall t0. (t0 → (t0, t0)))) == (

      (forall t0. (t0 → (t0, t0)))))

  }

  // Pairs in data structures

  test("Algebraic terms - Pair terms - Pairs in data structures - #1") {

    assert((

      (list<(string, int32)>)) == (

      (list<(string, int32)>)))

  }

  test("Algebraic terms - Pair terms - Pairs in data structures - #2") {

    assert((

      (forall t0. list<(list<t0>, string)>)) == (

      (forall t0. list<(list<t0>, string)>)))

  }

  // Additional cases

  test("Algebraic terms - Pair terms - Additional cases - #1") {

    assert((

      ((int32, string))) == (

      ((int32, string))))

  }

  test("Algebraic terms - Pair terms - Additional cases - #2") {

    assert((

      (forall t0. (list<t0>, string))) == (

      (forall t0. (list<t0>, string))))

  }

  test("Algebraic terms - Pair terms - Additional cases - #3") {

    assert((

      (forall t0,t1. (list<t0>, list<t1>))) == (

      (forall t0,t1. (list<t0>, list<t1>))))

  }

  // Set terms

  test("Algebraic terms - Set terms - #1") {

    assert((

      (set<boolean>)) == (

      (set<boolean>)))

  }

  test("Algebraic terms - Set terms - #2") {

    assert((

      (forall t0. (ordering t0) => set<set<t0>>)) == (

      (forall t0. (ordering t0) => set<set<t0>>)))

  }

  // Algorithm W test cases

  // STLC to System F

  test("Algorithm W test cases - STLC to System F - #1") {

    assert((

      (forall t0. (t0 → t0))) == (

      (forall t0. (t0 → t0))))

  }

  test("Algorithm W test cases - STLC to System F - #2") {

    assert((

      (int32)) == (

      (int32)))

  }

  test("Algorithm W test cases - STLC to System F - #3") {

    assert((

      (int32)) == (

      (int32)))

  }

  test("Algorithm W test cases - STLC to System F - #4") {

    assert((

      (int32)) == (

      (int32)))

  }

  test("Algorithm W test cases - STLC to System F - #5") {

    assert((

      (forall t0. (t0 → list<t0>))) == (

      (forall t0. (t0 → list<t0>))))

  }

  test("Algorithm W test cases - STLC to System F - #6") {

    assert((

      ((list<int32>, list<string>))) == (

      ((list<int32>, list<string>))))

  }

  test("Algorithm W test cases - STLC to System F - #7") {

    assert((

      (int32)) == (

      (int32)))

  }

  test("Algorithm W test cases - STLC to System F - #9") {

    assert((

      (forall t0. (int32 → int32 → t0))) == (

      (forall t0. (int32 → int32 → t0))))

  }

  test("Algorithm W test cases - STLC to System F - #10") {

    assert((

      (forall t0,t1. ((int32 → int32 → t0), (int32 → int32 → t1)))) == (

      (forall t0,t1. ((int32 → int32 → t0), (int32 → int32 → t1)))))

  }

  test("Algorithm W test cases - STLC to System F - #11") {

    assert((

      (forall t0,t1,t2,t3. ((t0 → int32 → t1), (int32 → t2 → t3)))) == (

      (forall t0,t1,t2,t3. ((t0 → int32 → t1), (int32 → t2 → t3)))))

  }

  test("Algorithm W test cases - STLC to System F - #12") {

    assert((

      (forall t0,t1. ((int32 → int32 → t0), (int32 → int32 → t1)))) == (

      (forall t0,t1. ((int32 → int32 → t0), (int32 → int32 → t1)))))

  }

  test("Algorithm W test cases - STLC to System F - #13") {

    assert((

      (forall t0,t1. ((int32 → int32 → t0), (int32 → int32 → t1)))) == (

      (forall t0,t1. ((int32 → int32 → t0), (int32 → int32 → t1)))))

  }

  // Type classes

  // Monomorphic (constraints vanish)

  // Map operations with concrete types

  test("Type classes - Monomorphic (constraints vanish) - Map operations with concrete types - #1") {

    assert((

      (map<string, int32>)) == (

      (map<string, int32>)))

  }

  test("Type classes - Monomorphic (constraints vanish) - Map operations with concrete types - #2") {

    assert((

      (maybe<int32>)) == (

      (maybe<int32>)))

  }

  test("Type classes - Monomorphic (constraints vanish) - Map operations with concrete types - #3") {

    assert((

      (map<string, int32>)) == (

      (map<string, int32>)))

  }

  // Set operations with concrete types

  test("Type classes - Monomorphic (constraints vanish) - Set operations with concrete types - #1") {

    assert((

      (set<int32>)) == (

      (set<int32>)))

  }

  test("Type classes - Monomorphic (constraints vanish) - Set operations with concrete types - #2") {

    assert((

      (boolean)) == (

      (boolean)))

  }

  // Equality operations with concrete types

  test("Type classes - Monomorphic (constraints vanish) - Equality operations with concrete types - #1") {

    assert((

      (boolean)) == (

      (boolean)))

  }

  test("Type classes - Monomorphic (constraints vanish) - Equality operations with concrete types - #2") {

    assert((

      (hydra.util.Comparison)) == (

      (hydra.util.Comparison)))

  }

  // List operations with concrete types

  test("Type classes - Monomorphic (constraints vanish) - List operations with concrete types - #1") {

    assert((

      (list<int32>)) == (

      (list<int32>)))

  }

  // Primitive references with constraints

  // Map primitives (ordering on key type)

  test("Type classes - Primitive references with constraints - Map primitives (ordering on key type) - #1") {

    assert((

      (forall t0,t1. (ordering t0) => (list<(t0, t1)> → map<t0, t1>))) == (

      (forall t0,t1. (ordering t0) => (list<(t0, t1)> → map<t0, t1>))))

  }

  test("Type classes - Primitive references with constraints - Map primitives (ordering on key type) - #2") {

    assert((

      (forall t0,t1. (ordering t0) => (t0 → map<t0, t1> → maybe<t1>))) == (

      (forall t0,t1. (ordering t0) => (t0 → map<t0, t1> → maybe<t1>))))

  }

  test("Type classes - Primitive references with constraints - Map primitives (ordering on key type) - #3") {

    assert((

      (forall t0,t1. (ordering t0) => (t0 → t1 → map<t0, t1> → map<t0, t1>))) == (

      (forall t0,t1. (ordering t0) => (t0 → t1 → map<t0, t1> → map<t0, t1>))))

  }

  test("Type classes - Primitive references with constraints - Map primitives (ordering on key type) - #4") {

    assert((

      (forall t0,t1,t2. (ordering t2) => ((t0 → t1) → map<t2, t0> → map<t2, t1>))) == (

      (forall t0,t1,t2. (ordering t2) => ((t0 → t1) → map<t2, t0> → map<t2, t1>))))

  }

  test("Type classes - Primitive references with constraints - Map primitives (ordering on key type) - #5") {

    assert((

      (forall t0,t1. (ordering t0) => map<t0, t1>)) == (

      (forall t0,t1. (ordering t0) => map<t0, t1>)))

  }

  // Set primitives (ordering on element type)

  test("Type classes - Primitive references with constraints - Set primitives (ordering on element type) - #1") {

    assert((

      (forall t0. (ordering t0) => (list<t0> → set<t0>))) == (

      (forall t0. (ordering t0) => (list<t0> → set<t0>))))

  }

  test("Type classes - Primitive references with constraints - Set primitives (ordering on element type) - #2") {

    assert((

      (forall t0. (ordering t0) => (t0 → set<t0> → boolean))) == (

      (forall t0. (ordering t0) => (t0 → set<t0> → boolean))))

  }

  test("Type classes - Primitive references with constraints - Set primitives (ordering on element type) - #3") {

    assert((

      (forall t0. (ordering t0) => (t0 → set<t0> → set<t0>))) == (

      (forall t0. (ordering t0) => (t0 → set<t0> → set<t0>))))

  }

  test("Type classes - Primitive references with constraints - Set primitives (ordering on element type) - #4") {

    assert((

      (forall t0,t1. (ordering t0, ordering t1) => ((t0 → t1) → set<t0> → set<t1>))) == (

      (forall t0,t1. (ordering t0, ordering t1) => ((t0 → t1) → set<t0> → set<t1>))))

  }

  // Equality primitives

  test("Type classes - Primitive references with constraints - Equality primitives - #1") {

    assert((

      (forall t0. (equality t0) => (t0 → t0 → boolean))) == (

      (forall t0. (equality t0) => (t0 → t0 → boolean))))

  }

  test("Type classes - Primitive references with constraints - Equality primitives - #2") {

    assert((

      (forall t0. (ordering t0) => (t0 → t0 → hydra.util.Comparison))) == (

      (forall t0. (ordering t0) => (t0 → t0 → hydra.util.Comparison))))

  }

  // List primitives with constraints

  test("Type classes - Primitive references with constraints - List primitives with constraints - #1") {

    assert((

      (forall t0. (ordering t0) => (list<t0> → list<t0>))) == (

      (forall t0. (ordering t0) => (list<t0> → list<t0>))))

  }

  test("Type classes - Primitive references with constraints - List primitives with constraints - #2") {

    assert((

      (forall t0. (equality t0) => (list<t0> → list<t0>))) == (

      (forall t0. (equality t0) => (list<t0> → list<t0>))))

  }

  test("Type classes - Primitive references with constraints - List primitives with constraints - #3") {

    assert((

      (forall t0. (equality t0) => (t0 → list<t0> → boolean))) == (

      (forall t0. (equality t0) => (t0 → list<t0> → boolean))))

  }

  // Partial application preserving constraints

  // Map partial application

  test("Type classes - Partial application preserving constraints - Map partial application - #1") {

    assert((

      (forall t0,t1. (ordering t0) => (t0 → map<t0, t1> → maybe<t1>))) == (

      (forall t0,t1. (ordering t0) => (t0 → map<t0, t1> → maybe<t1>))))

  }

  test("Type classes - Partial application preserving constraints - Map partial application - #2") {

    assert((

      (forall t0,t1. (ordering t0) => (t0 → t1 → map<t0, t1>))) == (

      (forall t0,t1. (ordering t0) => (t0 → t1 → map<t0, t1>))))

  }

  // Set partial application

  test("Type classes - Partial application preserving constraints - Set partial application - #1") {

    assert((

      (forall t0. (ordering t0) => (t0 → set<t0> → boolean))) == (

      (forall t0. (ordering t0) => (t0 → set<t0> → boolean))))

  }

  // Equality partial application

  test("Type classes - Partial application preserving constraints - Equality partial application - #1") {

    assert((

      (forall t0. (equality t0) => (t0 → t0 → boolean))) == (

      (forall t0. (equality t0) => (t0 → t0 → boolean))))

  }

  // Partial application fixing the constrained variable

  test("Type classes - Partial application preserving constraints - Partial application fixing the constrained variable - #1") {

    assert((

      (forall t0. (t0 → map<string, t0>))) == (

      (forall t0. (t0 → map<string, t0>))))

  }

  // Let binding constraint propagation

  // Simple let-bound wrappers

  test("Type classes - Let binding constraint propagation - Simple let-bound wrappers - #1") {

    assert((

      (forall t0,t1. (ordering t0) => (t0 → map<t0, t1> → maybe<t1>))) == (

      (forall t0,t1. (ordering t0) => (t0 → map<t0, t1> → maybe<t1>))))

  }

  test("Type classes - Let binding constraint propagation - Simple let-bound wrappers - #2") {

    assert((

      (forall t0. (ordering t0) => (t0 → set<t0> → boolean))) == (

      (forall t0. (ordering t0) => (t0 → set<t0> → boolean))))

  }

  test("Type classes - Let binding constraint propagation - Simple let-bound wrappers - #3") {

    assert((

      (forall t0,t1. (ordering t0) => (list<(t0, t1)> → map<t0, t1>))) == (

      (forall t0,t1. (ordering t0) => (list<(t0, t1)> → map<t0, t1>))))

  }

  // Let-bound with partial instantiation

  test("Type classes - Let binding constraint propagation - Let-bound with partial instantiation - #1") {

    assert((

      (forall t0. (ordering t0) => (map<t0, int32> → map<t0, int32>))) == (

      (forall t0. (ordering t0) => (map<t0, int32> → map<t0, int32>))))

  }

  test("Type classes - Let binding constraint propagation - Let-bound with partial instantiation - #2") {

    assert((

      (forall t0. (ordering t0) => (list<t0> → set<t0>))) == (

      (forall t0. (ordering t0) => (list<t0> → set<t0>))))

  }

  // Multiple uses of a constrained let binding

  test("Type classes - Let binding constraint propagation - Multiple uses of a constrained let binding - #1") {

    assert((

      ((map<string, int32>, map<boolean, string>))) == (

      ((map<string, int32>, map<boolean, string>))))

  }

  // Composition and constraint merging

  // Composing constrained primitives

  test("Type classes - Composition and constraint merging - Composing constrained primitives - #1") {

    assert((

      (forall t0. (ordering t0) => (list<t0> → map<t0, t0>))) == (

      (forall t0. (ordering t0) => (list<t0> → map<t0, t0>))))

  }

  test("Type classes - Composition and constraint merging - Composing constrained primitives - #2") {

    assert((

      (forall t0,t1. (ordering t1) => ((t0 → t1) → list<t0> → set<t1>))) == (

      (forall t0,t1. (ordering t1) => ((t0 → t1) → list<t0> → set<t1>))))

  }

  // Composing map and sort

  test("Type classes - Composition and constraint merging - Composing map and sort - #1") {

    assert((

      (forall t0,t1. (ordering t0, ordering t1) => (map<t0, list<t1>> → map<t0, list<t1>>))) == (

      (forall t0,t1. (ordering t0, ordering t1) => (map<t0, list<t1>> → map<t0, list<t1>>))))

  }

  // Nested containers

  // Maps of sets

  test("Type classes - Nested containers - Maps of sets - #1") {

    assert((

      (forall t0,t1. (ordering t0, ordering t1) => (map<t0, list<t1>> → map<t0, set<t1>>))) == (

      (forall t0,t1. (ordering t0, ordering t1) => (map<t0, list<t1>> → map<t0, set<t1>>))))

  }

  // Sets of sets

  test("Type classes - Nested containers - Sets of sets - #1") {

    assert((

      (forall t0. (ordering t0) => (set<list<t0>> → set<set<t0>>))) == (

      (forall t0. (ordering t0) => (set<list<t0>> → set<set<t0>>))))

  }

  // Map from sorted list

  test("Type classes - Nested containers - Map from sorted list - #1") {

    assert((

      (forall t0. (ordering t0) => (list<t0> → map<t0, set<t0>>))) == (

      (forall t0. (ordering t0) => (list<t0> → map<t0, set<t0>>))))

  }

  // Collection term constraints

  // Set literals

  test("Type classes - Collection term constraints - Set literals - #1") {

    assert((

      (forall t0. (ordering t0) => (t0 → set<t0>))) == (

      (forall t0. (ordering t0) => (t0 → set<t0>))))

  }

  test("Type classes - Collection term constraints - Set literals - #2") {

    assert((

      (forall t0. (ordering t0) => (t0 → t0 → set<t0>))) == (

      (forall t0. (ordering t0) => (t0 → t0 → set<t0>))))

  }

  test("Type classes - Collection term constraints - Set literals - #3") {

    assert((

      (set<int32>)) == (

      (set<int32>)))

  }

  // Map literals

  test("Type classes - Collection term constraints - Map literals - #1") {

    assert((

      (forall t0,t1. (ordering t0) => (t0 → t1 → map<t0, t1>))) == (

      (forall t0,t1. (ordering t0) => (t0 → t1 → map<t0, t1>))))

  }

  test("Type classes - Collection term constraints - Map literals - #2") {

    assert((

      (map<string, int32>)) == (

      (map<string, int32>)))

  }

  // Collection terms with primitives

  test("Type classes - Collection term constraints - Collection terms with primitives - #1") {

    assert((

      ((int32 → set<int32>))) == (

      ((int32 → set<int32>))))

  }

  test("Type classes - Collection term constraints - Collection terms with primitives - #2") {

    assert((

      (forall t0. (ordering t0) => (t0 → map<t0, list<t0>>))) == (

      (forall t0. (ordering t0) => (t0 → map<t0, list<t0>>))))

  }

  // Constraint propagation through collection elements

  test("Type classes - Collection term constraints - Constraint propagation through collection elements - #1") {

    assert((

      (forall t0. (ordering t0) => (list<t0> → map<int32, set<t0>>))) == (

      (forall t0. (ordering t0) => (list<t0> → map<int32, set<t0>>))))

  }

  test("Type classes - Collection term constraints - Constraint propagation through collection elements - #2") {

    assert((

      (forall t0. (ordering t0) => list<(list<t0> → list<t0>)>)) == (

      (forall t0. (ordering t0) => list<(list<t0> → list<t0>)>)))

  }

  test("Type classes - Collection term constraints - Constraint propagation through collection elements - #3") {

    assert((

      (forall t0. (ordering t0) => ((list<t0> → set<t0>), int32))) == (

      (forall t0. (ordering t0) => ((list<t0> → set<t0>), int32))))

  }

  test("Type classes - Collection term constraints - Constraint propagation through collection elements - #4") {

    assert((

      (forall t0. (ordering t0) => (list<t0> → set<set<t0>>))) == (

      (forall t0. (ordering t0) => (list<t0> → set<set<t0>>))))

  }

  // Expected failures

  // Undefined variable

  // Basic unbound variables

  test("Expected failures - Undefined variable - Basic unbound variables - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Undefined variable - Basic unbound variables - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Undefined variable - Basic unbound variables - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Unbound in let expressions

  test("Expected failures - Undefined variable - Unbound in let expressions - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Undefined variable - Unbound in let expressions - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Undefined variable - Unbound in let expressions - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Shadowing scope errors

  test("Expected failures - Undefined variable - Shadowing scope errors - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Undefined variable - Shadowing scope errors - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Undefined variable - Shadowing scope errors - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Unification failure

  // Basic type mismatches

  test("Expected failures - Unification failure - Basic type mismatches - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Unification failure - Basic type mismatches - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Unification failure - Basic type mismatches - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Unification failure - Basic type mismatches - #4") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Collection type mismatches

  test("Expected failures - Unification failure - Collection type mismatches - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Unification failure - Collection type mismatches - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Unification failure - Collection type mismatches - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Unification failure - Collection type mismatches - #4") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Conditional type mismatches

  test("Expected failures - Unification failure - Conditional type mismatches - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Unification failure - Conditional type mismatches - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Unification failure - Conditional type mismatches - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Polymorphic instantiation conflicts

  test("Expected failures - Unification failure - Polymorphic instantiation conflicts - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Unification failure - Polymorphic instantiation conflicts - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Unification failure - Polymorphic instantiation conflicts - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Invalid application

  // Non-function application

  test("Expected failures - Invalid application - Non-function application - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Invalid application - Non-function application - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Invalid application - Non-function application - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Invalid application - Non-function application - #4") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Collection application

  test("Expected failures - Invalid application - Collection application - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Invalid application - Collection application - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Invalid application - Collection application - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Invalid application - Collection application - #4") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Primitive misapplication

  test("Expected failures - Invalid application - Primitive misapplication - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Invalid application - Primitive misapplication - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Invalid application - Primitive misapplication - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Invalid application - Primitive misapplication - #4") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Self-application

  // Direct self-application

  test("Expected failures - Self-application - Direct self-application - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Self-application - Direct self-application - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Indirect self-application

  test("Expected failures - Self-application - Indirect self-application - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Self-application - Indirect self-application - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Self-application - Indirect self-application - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Arity mismatch

  // Too many arguments

  test("Expected failures - Arity mismatch - Too many arguments - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Arity mismatch - Too many arguments - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Arity mismatch - Too many arguments - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Wrong argument types with extra args

  test("Expected failures - Arity mismatch - Wrong argument types with extra args - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Arity mismatch - Wrong argument types with extra args - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Arity mismatch - Wrong argument types with extra args - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Recursive type construction

  // Direct recursive types

  test("Expected failures - Recursive type construction - Direct recursive types - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Recursive type construction - Direct recursive types - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Recursive type construction - Direct recursive types - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Recursive function types

  test("Expected failures - Recursive type construction - Recursive function types - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Recursive type construction - Recursive function types - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Recursive type construction - Recursive function types - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Mutually recursive types

  test("Expected failures - Recursive type construction - Mutually recursive types - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Recursive type construction - Mutually recursive types - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Recursive type construction - Mutually recursive types - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Occur check failures

  // Function occur checks

  test("Expected failures - Occur check failures - Function occur checks - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Mutual occur checks

  test("Expected failures - Occur check failures - Mutual occur checks - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Occur check failures - Mutual occur checks - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Occur check failures - Mutual occur checks - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Complex occur checks

  test("Expected failures - Occur check failures - Complex occur checks - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Occur check failures - Complex occur checks - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Type constructor misuse

  // List constructor errors

  test("Expected failures - Type constructor misuse - List constructor errors - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Type constructor misuse - List constructor errors - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Type constructor misuse - List constructor errors - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Type constructor misuse - List constructor errors - #4") {

    assert((

      FAIL) == (

      FAIL))

  }

  // String constructor errors

  test("Expected failures - Type constructor misuse - String constructor errors - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Type constructor misuse - String constructor errors - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Type constructor misuse - String constructor errors - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Type constructor misuse - String constructor errors - #4") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Math constructor errors

  test("Expected failures - Type constructor misuse - Math constructor errors - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Type constructor misuse - Math constructor errors - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Type constructor misuse - Math constructor errors - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Type constructor misuse - Math constructor errors - #4") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Polymorphism violations

  // Identity function violations

  test("Expected failures - Polymorphism violations - Identity function violations - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Polymorphism violations - Identity function violations - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Polymorphism violations - Identity function violations - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Constrained polymorphism violations

  test("Expected failures - Polymorphism violations - Constrained polymorphism violations - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Polymorphism violations - Constrained polymorphism violations - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Polymorphism violations - Constrained polymorphism violations - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Higher-order polymorphism violations

  test("Expected failures - Polymorphism violations - Higher-order polymorphism violations - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Polymorphism violations - Higher-order polymorphism violations - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Polymorphism violations - Higher-order polymorphism violations - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Let binding type mismatches

  // Application type mismatches

  test("Expected failures - Let binding type mismatches - Application type mismatches - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Let binding type mismatches - Application type mismatches - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Let binding type mismatches - Application type mismatches - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Collection type mismatches

  test("Expected failures - Let binding type mismatches - Collection type mismatches - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Let binding type mismatches - Collection type mismatches - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Let binding type mismatches - Collection type mismatches - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Function binding mismatches

  test("Expected failures - Let binding type mismatches - Function binding mismatches - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Let binding type mismatches - Function binding mismatches - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Constraint solver edge cases

  // Complex constraint propagation

  test("Expected failures - Constraint solver edge cases - Complex constraint propagation - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Fixed point combinators

  test("Expected failures - Constraint solver edge cases - Fixed point combinators - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Constraint solver edge cases - Fixed point combinators - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Constraint solver edge cases - Fixed point combinators - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Constraint cycles

  test("Expected failures - Constraint solver edge cases - Constraint cycles - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Constraint solver edge cases - Constraint cycles - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Primitive function type errors

  // Logic primitive errors

  test("Expected failures - Primitive function type errors - Logic primitive errors - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Primitive function type errors - Logic primitive errors - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Primitive function type errors - Logic primitive errors - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Primitive function type errors - Logic primitive errors - #4") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Collection primitive errors

  test("Expected failures - Primitive function type errors - Collection primitive errors - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Primitive function type errors - Collection primitive errors - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Primitive function type errors - Collection primitive errors - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Primitive function type errors - Collection primitive errors - #4") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Math primitive errors

  test("Expected failures - Primitive function type errors - Math primitive errors - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Primitive function type errors - Math primitive errors - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Primitive function type errors - Math primitive errors - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Primitive function type errors - Math primitive errors - #4") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Complex constraint failures

  // Multi-level constraint conflicts

  test("Expected failures - Complex constraint failures - Multi-level constraint conflicts - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Complex constraint failures - Multi-level constraint conflicts - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Complex constraint failures - Multi-level constraint conflicts - #3") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Function composition failures

  test("Expected failures - Complex constraint failures - Function composition failures - #1") {

    assert((

      FAIL) == (

      FAIL))

  }

  test("Expected failures - Complex constraint failures - Function composition failures - #2") {

    assert((

      FAIL) == (

      FAIL))

  }

  // Fundamentals

  // Lambdas

  // Simple lambdas

  test("Fundamentals - Lambdas - Simple lambdas - #1") {

    assert((

      (forall t0. (t0 → t0))) == (

      (forall t0. (t0 → t0))))

  }

  test("Fundamentals - Lambdas - Simple lambdas - #2") {

    assert((

      (forall t0. (t0 → int16))) == (

      (forall t0. (t0 → int16))))

  }

  // Nested lambdas

  test("Fundamentals - Lambdas - Nested lambdas - #1") {

    assert((

      ((int32 → int32 → int32))) == (

      ((int32 → int32 → int32))))

  }

  test("Fundamentals - Lambdas - Nested lambdas - #2") {

    assert((

      ((int32 → list<(int32 → int32)>))) == (

      ((int32 → list<(int32 → int32)>))))

  }

  // Nested lambdas with shadowing

  test("Fundamentals - Lambdas - Nested lambdas with shadowing - #1") {

    assert((

      (forall t0. (t0 → int32 → int32))) == (

      (forall t0. (t0 → int32 → int32))))

  }

  // Let terms

  // Simple

  test("Fundamentals - Let terms - Simple - #1") {

    assert((

      (forall t0,t1. (t0 → t1 → float32))) == (

      (forall t0,t1. (t0 → t1 → float32))))

  }

  // Empty let

  test("Fundamentals - Let terms - Empty let - #1") {

    assert((

      (int32)) == (

      (int32)))

  }

  test("Fundamentals - Let terms - Empty let - #2") {

    assert((

      (forall t0. (t0 → t0))) == (

      (forall t0. (t0 → t0))))

  }

  // Trivial let

  test("Fundamentals - Let terms - Trivial let - #1") {

    assert((

      (int32)) == (

      (int32)))

  }

  test("Fundamentals - Let terms - Trivial let - #2") {

    assert((

      (int32)) == (

      (int32)))

  }

  // Multiple references to a let-bound term

  test("Fundamentals - Let terms - Multiple references to a let-bound term - #1") {

    assert((

      (list<int32>)) == (

      (list<int32>)))

  }

  // Nested let

  test("Fundamentals - Let terms - Nested let - #1") {

    assert((

      (list<int32>)) == (

      (list<int32>)))

  }

  test("Fundamentals - Let terms - Nested let - #2") {

    assert((

      ((int32, int32))) == (

      ((int32, int32))))

  }

  test("Fundamentals - Let terms - Nested let - #3") {

    assert((

      (forall t0. (list<int32>, (list<string>, list<list<t0>>)))) == (

      (forall t0. (list<int32>, (list<string>, list<list<t0>>)))))

  }

  // Nested let with shadowing

  test("Fundamentals - Let terms - Nested let with shadowing - #1") {

    assert((

      (int32)) == (

      (int32)))

  }

  test("Fundamentals - Let terms - Nested let with shadowing - #2") {

    assert((

      ((string, int32))) == (

      ((string, int32))))

  }

  // Let-polymorphism

  test("Fundamentals - Let terms - Let-polymorphism - #1") {

    assert((

      (forall t0,t1. (t0 → t1 → float32))) == (

      (forall t0,t1. (t0 → t1 → float32))))

  }

  test("Fundamentals - Let terms - Let-polymorphism - #2") {

    assert((

      (((int32 → boolean → boolean) → int32 → boolean → boolean))) == (

      (((int32 → boolean → boolean) → int32 → boolean → boolean))))

  }

  test("Fundamentals - Let terms - Let-polymorphism - #3") {

    assert((

      (forall t0. (t0 → t0))) == (

      (forall t0. (t0 → t0))))

  }

  test("Fundamentals - Let terms - Let-polymorphism - #4") {

    assert((

      (list<int32>)) == (

      (list<int32>)))

  }

  test("Fundamentals - Let terms - Let-polymorphism - #5") {

    assert((

      (forall t0. (t0 → list<t0>))) == (

      (forall t0. (t0 → list<t0>))))

  }

  test("Fundamentals - Let terms - Let-polymorphism - #6") {

    assert((

      ((int32, string))) == (

      ((int32, string))))

  }

  test("Fundamentals - Let terms - Let-polymorphism - #7") {

    assert((

      ((list<int32>, list<string>))) == (

      ((list<int32>, list<string>))))

  }

  test("Fundamentals - Let terms - Let-polymorphism - #8") {

    assert((

      (forall t0. (int32 → t0 → list<(list<int32>, list<t0>)>))) == (

      (forall t0. list<(int32, t0)>)))

  }

  test("Fundamentals - Let terms - Let-polymorphism - #9") {

    assert((

      ((int32, string))) == (

      ((int32, string))))

  }

  test("Fundamentals - Let terms - Let-polymorphism - #10") {

    assert((

      ((int32, string))) == (

      ((int32, string))))

  }

  // Recursive and mutually recursive let (@wisnesky's test cases)

  test("Fundamentals - Let terms - Recursive and mutually recursive let (@wisnesky's test cases) - #1") {

    assert((

      (forall t0. (int32 → int32 → t0))) == (

      (forall t0. (int32 → int32 → t0))))

  }

  test("Fundamentals - Let terms - Recursive and mutually recursive let (@wisnesky's test cases) - #2") {

    assert((

      (forall t0,t1. (t0, t1))) == (

      (forall t0,t1. (t0, t1))))

  }

  test("Fundamentals - Let terms - Recursive and mutually recursive let (@wisnesky's test cases) - #3") {

    assert((

      (forall t0,t1,t2,t3. ((t0 → int32 → t1), (int32 → t2 → t3)))) == (

      (forall t0,t1. ((t0 → int32 → t1), (int32 → v0 → t1)))))

  }

  test("Fundamentals - Let terms - Recursive and mutually recursive let (@wisnesky's test cases) - #4") {

    assert((

      (int32)) == (

      (int32)))

  }

  test("Fundamentals - Let terms - Recursive and mutually recursive let (@wisnesky's test cases) - #5") {

    assert((

      (int32)) == (

      (int32)))

  }

  test("Fundamentals - Let terms - Recursive and mutually recursive let (@wisnesky's test cases) - #6") {

    assert((

      (forall t0,t1. ((t0 → t0), (t1 → t1)))) == (

      (forall t0,t1. ((t0 → t0), (t1 → t1)))))

  }

  test("Fundamentals - Let terms - Recursive and mutually recursive let (@wisnesky's test cases) - #7") {

    assert((

      (forall t0,t1,t2. ((t0 → t0), ((t1 → t1), (t2 → t2))))) == (

      (forall t0,t1,t2. ((t0 → t0), ((t1 → t1), (t2 → t2))))))

  }

  // Recursive and mutually recursive let with polymorphism

  test("Fundamentals - Let terms - Recursive and mutually recursive let with polymorphism - #1") {

    assert((

      ((int32, string))) == (

      ((int32, string))))

  }

  test("Fundamentals - Let terms - Recursive and mutually recursive let with polymorphism - #2") {

    assert((

      ((int32, string))) == (

      ((int32, string))))

  }

  test("Fundamentals - Let terms - Recursive and mutually recursive let with polymorphism - #3") {

    assert((

      ((int32, string))) == (

      ((int32, string))))

  }

  // Recursion involving polymorphic functions

  test("Fundamentals - Let terms - Recursion involving polymorphic functions - #1") {

    assert((

      (forall t0. (boolean → t0 → list<list<t0>>))) == (

      (forall t0. (boolean → t0 → list<list<t0>>))))

  }

  test("Fundamentals - Let terms - Recursion involving polymorphic functions - #2") {

    assert((

      (forall t0,t1. (boolean, ((t0 → t0) → t1 → t0)))) == (

      (forall t0,t1. (boolean, ((t0 → t0) → t1 → t0)))))

  }

  test("Fundamentals - Let terms - Recursion involving polymorphic functions - #3") {

    assert((

      (forall t0. (boolean, ((t0 → t0) → t0)))) == (

      (forall t0. (boolean, ((t0 → t0) → t0)))))

  }

  test("Fundamentals - Let terms - Recursion involving polymorphic functions - #4") {

    assert((

      (forall t0. (boolean, (int32, ((t0 → t0) → t0))))) == (

      (forall t0. (boolean, (int32, ((t0 → t0) → t0))))))

  }

  test("Fundamentals - Let terms - Recursion involving polymorphic functions - #5") {

    assert((

      (forall t0,t1. (t0, t1))) == (

      (forall t0,t1. (t0, t1))))

  }

  // Over-generalization of hoisted let-bindings

  test("Fundamentals - Let terms - Over-generalization of hoisted let-bindings - #1") {

    assert((

      (forall t0,t1. ((t0 → (t0, t1)) → t0 → (t0, t1)))) == (

      (forall t0,t1. ((t0 → (t0, t1)) → t0 → (t0, t1)))))

  }

  test("Fundamentals - Let terms - Over-generalization of hoisted let-bindings - #2") {

    assert((

      (forall t0,t1. ((t0 → (t0, t1)) → t0 → (t0, t1)))) == (

      (forall t0,t1. ((t0 → (t0, t1)) → t0 → (t0, t1)))))

  }

  test("Fundamentals - Let terms - Over-generalization of hoisted let-bindings - #3") {

    assert((

      (forall t0,t1,t2,t3. ((t0 → t1 → (t2, t3)) → t0 → t1 → (t2, t1)))) == (

      (forall t0,t1,t2,t3. ((t0 → t1 → (t2, t3)) → t0 → t1 → (t2, t1)))))

  }

  test("Fundamentals - Let terms - Over-generalization of hoisted let-bindings - #4") {

    assert((

      (forall t0. ((t0 → (t0, int32)) → t0 → boolean → (t0, int32)))) == (

      (forall t0. ((t0 → (t0, int32)) → t0 → boolean → (t0, int32)))))

  }

  test("Fundamentals - Let terms - Over-generalization of hoisted let-bindings - #5") {

    assert((

      (forall t0. ((t0 → (t0, int32)) → t0 → boolean → (t0, int32)))) == (

      (forall t0. ((t0 → (t0, int32)) → t0 → boolean → (t0, int32)))))

  }

  test("Fundamentals - Let terms - Over-generalization of hoisted let-bindings - #6") {

    assert((

      (forall t0. ((t0 → (t0, int32)) → t0 → boolean → (t0, int32)))) == (

      (forall t0. ((t0 → (t0, int32)) → t0 → boolean → (t0, int32)))))

  }

  // Literals

  test("Fundamentals - Literals - #1") {

    assert((

      (int32)) == (

      (int32)))

  }

  test("Fundamentals - Literals - #2") {

    assert((

      (string)) == (

      (string)))

  }

  test("Fundamentals - Literals - #3") {

    assert((

      (boolean)) == (

      (boolean)))

  }

  test("Fundamentals - Literals - #4") {

    assert((

      (float64)) == (

      (float64)))

  }

  // Pathological terms

  // Recursion

  test("Fundamentals - Pathological terms - Recursion - #1") {

    assert((

      (forall t0. t0)) == (

      (forall t0. t0)))

  }

  test("Fundamentals - Pathological terms - Recursion - #2") {

    assert((

      (forall t0. (t0 → t0))) == (

      (forall t0. (t0 → t0))))

  }

  test("Fundamentals - Pathological terms - Recursion - #3") {

    assert((

      (forall t0. (t0 → t0))) == (

      (forall t0. (t0 → t0))))

  }

  test("Fundamentals - Pathological terms - Recursion - #4") {

    assert((

      (forall t0,t1. (t0 → t1))) == (

      (forall t0,t1. (t0 → t1))))

  }

  test("Fundamentals - Pathological terms - Recursion - #5") {

    assert((

      (forall t0. ((t0 → t0) → t0))) == (

      (forall t0. ((t0 → t0) → t0))))

  }

  test("Fundamentals - Pathological terms - Recursion - #6") {

    assert((

      (int32)) == (

      (int32)))

  }

  // Infinite lists

  test("Fundamentals - Pathological terms - Infinite lists - #1") {

    assert((

      (list<int32>)) == (

      (list<int32>)))

  }

  test("Fundamentals - Pathological terms - Infinite lists - #2") {

    assert((

      (forall t0. (t0 → list<t0>))) == (

      (forall t0. (t0 → list<t0>))))

  }

  test("Fundamentals - Pathological terms - Infinite lists - #3") {

    assert((

      (forall t0. (t0 → list<t0>))) == (

      (forall t0. (t0 → t0))))

  }

  test("Fundamentals - Pathological terms - Infinite lists - #4") {

    assert((

      (list<int32>)) == (

      (list<int32>)))

  }

  // Polymorphism

  // Simple lists and optionals

  test("Fundamentals - Polymorphism - Simple lists and optionals - #1") {

    assert((

      (forall t0. list<t0>)) == (

      (forall t0. list<t0>)))

  }

  test("Fundamentals - Polymorphism - Simple lists and optionals - #2") {

    assert((

      (forall t0. maybe<t0>)) == (

      (forall t0. maybe<t0>)))

  }

  test("Fundamentals - Polymorphism - Simple lists and optionals - #3") {

    assert((

      (maybe<int32>)) == (

      (maybe<int32>)))

  }

  // Lambdas, lists, and products

  test("Fundamentals - Polymorphism - Lambdas, lists, and products - #1") {

    assert((

      (forall t0. (t0 → t0))) == (

      (forall t0. (t0 → t0))))

  }

  test("Fundamentals - Polymorphism - Lambdas, lists, and products - #2") {

    assert((

      (forall t0. (t0 → (t0, t0)))) == (

      (forall t0. (t0 → (t0, t0)))))

  }

  test("Fundamentals - Polymorphism - Lambdas, lists, and products - #3") {

    assert((

      (forall t0. (t0 → list<t0>))) == (

      (forall t0. (t0 → list<t0>))))

  }

  test("Fundamentals - Polymorphism - Lambdas, lists, and products - #4") {

    assert((

      (forall t0. list<(t0 → t0)>)) == (

      (forall t0. list<(t0 → t0)>)))

  }

  test("Fundamentals - Polymorphism - Lambdas, lists, and products - #5") {

    assert((

      (forall t0,t1. list<(t0 → t1 → (t1, t0))>)) == (

      (forall t0,t1. list<(t0 → t1 → (t1, t0))>)))

  }

  // Lambdas and application

  test("Fundamentals - Polymorphism - Lambdas and application - #1") {

    assert((

      (string)) == (

      (string)))

  }

  // Primitives and application

  test("Fundamentals - Polymorphism - Primitives and application - #1") {

    assert((

      (list<int32>)) == (

      (list<int32>)))

  }

  // Lambdas and primitives

  test("Fundamentals - Polymorphism - Lambdas and primitives - #1") {

    assert((

      ((int32 → int32 → int32))) == (

      ((int32 → int32 → int32))))

  }

  test("Fundamentals - Polymorphism - Lambdas and primitives - #2") {

    assert((

      ((int32 → int32 → int32))) == (

      ((int32 → int32 → int32))))

  }

  test("Fundamentals - Polymorphism - Lambdas and primitives - #3") {

    assert((

      ((int32 → int32))) == (

      ((int32 → int32))))

  }

  // Mixed expressions with lambdas, constants, and primitive functions

  test("Fundamentals - Polymorphism - Mixed expressions with lambdas, constants, and primitive functions - #1") {

    assert((

      ((int32 → int32))) == (

      ((int32 → int32))))

  }

  // Application terms

  test("Fundamentals - Polymorphism - Application terms - #1") {

    assert((

      (string)) == (

      (string)))

  }

  test("Fundamentals - Polymorphism - Application terms - #2") {

    assert((

      ((int32 → int32))) == (

      ((int32 → int32))))

  }

  // Primitives

  // Monomorphic primitive functions

  test("Fundamentals - Primitives - Monomorphic primitive functions - #1") {

    assert((

      ((string → int32))) == (

      ((string → int32))))

  }

  test("Fundamentals - Primitives - Monomorphic primitive functions - #2") {

    assert((

      ((int32 → int32 → int32))) == (

      ((int32 → int32 → int32))))

  }

  // Polymorphic primitive functions

  test("Fundamentals - Primitives - Polymorphic primitive functions - #1") {

    assert((

      (forall t0. (t0 → int32))) == (

      (forall t0. (t0 → int32))))

  }

  test("Fundamentals - Primitives - Polymorphic primitive functions - #2") {

    assert((

      ((int32 → int32))) == (

      ((int32 → int32))))

  }

  test("Fundamentals - Primitives - Polymorphic primitive functions - #3") {

    assert((

      (forall t0. (list<list<t0>> → list<t0>))) == (

      (forall t0. (list<list<t0>> → list<t0>))))

  }

  test("Fundamentals - Primitives - Polymorphic primitive functions - #4") {

    assert((

      (forall t0. (list<list<t0>> → list<t0>))) == (

      (forall t0. (list<list<t0>> → list<t0>))))

  }

  test("Fundamentals - Primitives - Polymorphic primitive functions - #5") {

    assert((

      (forall t0. (list<list<t0>> → int32))) == (

      (forall t0. (list<list<t0>> → int32))))

  }

  test("Fundamentals - Primitives - Polymorphic primitive functions - #6") {

    assert((

      (forall t0. (list<t0> → int32))) == (

      (forall t0. (list<t0> → int32))))

  }

  test("Fundamentals - Primitives - Polymorphic primitive functions - #7") {

    assert((

      (forall t0. (list<t0> → int32))) == (

      (forall t0. (list<t0> → int32))))

  }

  test("Fundamentals - Primitives - Polymorphic primitive functions - #8") {

    assert((

      (forall t0. (list<list<t0>> → int32))) == (

      (forall t0. (list<list<t0>> → int32))))

  }

  // Examples from the Hydra kernel

  // Nested let

  // hydra.formatting.mapFirstLetter

  test("Examples from the Hydra kernel - Nested let - hydra.formatting.mapFirstLetter - #1") {

    assert((

      (((string → string) → string → string))) == (

      (((string → string) → string → string))))

  }

  // Recursive let with pair return (ifElse)

  test("Examples from the Hydra kernel - Nested let - Recursive let with pair return (ifElse) - #2") {

    assert((

      ((string → (map<string, string>, string)))) == (

      ((string → (map<string, string>, string)))))

  }

  // Recursive let with pair return (case on Type)

  test("Examples from the Hydra kernel - Nested let - Recursive let with pair return (case on Type) - #3") {

    assert((

      ((hydra.core.Type → (map<hydra.core.Name, hydra.core.Name>, hydra.core.Type)))) == (

      ((hydra.core.Type → (map<hydra.core.Name, hydra.core.Name>, hydra.core.Type)))))

  }

  // Nominal terms

  // Case statements

  test("Nominal terms - Case statements - #1") {

    assert((

      ((SimpleNumber → int32))) == (

      ((SimpleNumber → int32))))

  }

  test("Nominal terms - Case statements - #2") {

    assert((

      ((UnionMonomorphic → boolean))) == (

      ((UnionMonomorphic → boolean))))

  }

  // Projections

  // Record eliminations

  test("Nominal terms - Projections - Record eliminations - #1") {

    assert((

      ((Person → string))) == (

      ((Person → string))))

  }

  // Pair projections

  test("Nominal terms - Projections - Pair projections - #1") {

    assert((

      (forall t0,t1. ((t0, t1) → t0))) == (

      (forall t0,t1. ((t0, t1) → t0))))

  }

  test("Nominal terms - Projections - Pair projections - #2") {

    assert((

      (string)) == (

      (string)))

  }

  // Records

  // Simple records

  test("Nominal terms - Records - Simple records - #1") {

    assert((

      (LatLon)) == (

      (LatLon)))

  }

  test("Nominal terms - Records - Simple records - #2") {

    assert((

      ((LatLonPoly @ float32))) == (

      ((LatLonPoly @ float32))))

  }

  test("Nominal terms - Records - Simple records - #3") {

    assert((

      ((float32 → (LatLonPoly @ float32)))) == (

      ((float32 → (LatLonPoly @ float32)))))

  }

  test("Nominal terms - Records - Simple records - #4") {

    assert((

      (forall t0. (t0 → (LatLonPoly @ t0)))) == (

      (forall t0. (t0 → (LatLonPoly @ t0)))))

  }

  test("Nominal terms - Records - Simple records - #5") {

    assert((

      (Person)) == (

      (Person)))

  }

  // Record instances of simply recursive record types

  test("Nominal terms - Records - Record instances of simply recursive record types - #1") {

    assert((

      (IntList)) == (

      (IntList)))

  }

  test("Nominal terms - Records - Record instances of simply recursive record types - #2") {

    assert((

      (IntList)) == (

      (IntList)))

  }

  test("Nominal terms - Records - Record instances of simply recursive record types - #3") {

    assert((

      ((List @ int32))) == (

      ((List @ int32))))

  }

  test("Nominal terms - Records - Record instances of simply recursive record types - #4") {

    assert((

      ((List @ int32))) == (

      ((List @ int32))))

  }

  test("Nominal terms - Records - Record instances of simply recursive record types - #5") {

    assert((

      (forall t0. (t0 → (List @ t0)))) == (

      (forall t0. (t0 → (List @ t0)))))

  }

  // Record instances of mutually recursive record types

  test("Nominal terms - Records - Record instances of mutually recursive record types - #1") {

    assert((

      ((BuddyListA @ int32))) == (

      ((BuddyListA @ int32))))

  }

  test("Nominal terms - Records - Record instances of mutually recursive record types - #2") {

    assert((

      (forall t0. (t0 → (BuddyListA @ t0)))) == (

      (forall t0. (t0 → (BuddyListA @ t0)))))

  }

  // Variant terms

  // Variants

  test("Nominal terms - Variant terms - Variants - #1") {

    assert((

      (Timestamp)) == (

      (Timestamp)))

  }

  test("Nominal terms - Variant terms - Variants - #2") {

    assert((

      (UnionMonomorphic)) == (

      (UnionMonomorphic)))

  }

  // Polymorphic and recursive variants

  test("Nominal terms - Variant terms - Polymorphic and recursive variants - #1") {

    assert((

      (forall t0. (UnionPolymorphicRecursive @ t0))) == (

      (forall t0. (UnionPolymorphicRecursive @ t0))))

  }

  test("Nominal terms - Variant terms - Polymorphic and recursive variants - #2") {

    assert((

      ((UnionPolymorphicRecursive @ string))) == (

      ((UnionPolymorphicRecursive @ string))))

  }

  test("Nominal terms - Variant terms - Polymorphic and recursive variants - #3") {

    assert((

      ((UnionPolymorphicRecursive @ int32))) == (

      ((UnionPolymorphicRecursive @ int32))))

  }

  // Wrapper introductions and eliminations

  // Wrapper introductions

  test("Nominal terms - Wrapper introductions and eliminations - Wrapper introductions - #1") {

    assert((

      (StringAlias)) == (

      (StringAlias)))

  }

  test("Nominal terms - Wrapper introductions and eliminations - Wrapper introductions - #2") {

    assert((

      ((string → StringAlias))) == (

      ((string → StringAlias))))

  }

  // Wrapper eliminations

  test("Nominal terms - Wrapper introductions and eliminations - Wrapper eliminations - #1") {

    assert((

      ((StringAlias → string))) == (

      ((StringAlias → string))))

  }

  test("Nominal terms - Wrapper introductions and eliminations - Wrapper eliminations - #2") {

    assert((

      (string)) == (

      (string)))

  }
}
