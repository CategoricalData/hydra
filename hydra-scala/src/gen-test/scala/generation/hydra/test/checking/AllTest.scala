// Note: this is an automatically generated file. Do not edit.
// checking

package generation.hydra.test.checking

import org.scalatest.funsuite.AnyFunSuite

class AllTest extends AnyFunSuite {

  // Advanced

  // Annotated terms

  // Top-level annotations

  test("Advanced - Annotated terms - Top-level annotations - annotated literal") {

    assert((

      int32) == (

      int32))

  }

  test("Advanced - Annotated terms - Top-level annotations - annotated list") {

    assert((

      list<string>) == (

      list<string>))

  }

  test("Advanced - Annotated terms - Top-level annotations - annotated record") {

    assert((

      Person) == (

      Person))

  }

  test("Advanced - Annotated terms - Top-level annotations - annotated lambda") {

    assert((

      (∀t0.(t0 → t0))) == (

      (∀t0.(t0 → t0))))

  }

  // Nested annotations

  test("Advanced - Annotated terms - Nested annotations - annotation within annotation") {

    assert((

      int32) == (

      int32))

  }

  test("Advanced - Annotated terms - Nested annotations - annotated terms in tuple") {

    assert((

      (int32, string)) == (

      (int32, string)))

  }

  test("Advanced - Annotated terms - Nested annotations - annotated term in function application") {

    assert((

      int32) == (

      int32))

  }

  // Annotations in complex contexts

  test("Advanced - Annotated terms - Annotations in complex contexts - annotated let binding") {

    assert((

      (int32, string)) == (

      (int32, string)))

  }

  test("Advanced - Annotated terms - Annotations in complex contexts - annotated record fields") {

    assert((

      Person) == (

      Person))

  }

  test("Advanced - Annotated terms - Annotations in complex contexts - annotated function in application") {

    assert((

      int32) == (

      int32))

  }

  // Algebraic types

  // Unit

  // Unit term

  test("Algebraic types - Unit - Unit term - unit literal") {

    assert((

      unit) == (

      unit))

  }

  // Unit term in polymorphic context

  test("Algebraic types - Unit - Unit term in polymorphic context - unit from lambda") {

    assert((

      (∀t0.(t0 → unit))) == (

      (∀t0.(t0 → unit))))

  }

  // Pairs

  // Basic pairs

  test("Algebraic types - Pairs - Basic pairs - pair of int and string") {

    assert((

      (int32, string)) == (

      (int32, string)))

  }

  test("Algebraic types - Pairs - Basic pairs - pair of string and boolean") {

    assert((

      (string, boolean)) == (

      (string, boolean)))

  }

  test("Algebraic types - Pairs - Basic pairs - pair of boolean and int") {

    assert((

      (boolean, int32)) == (

      (boolean, int32)))

  }

  // Polymorphic pairs

  test("Algebraic types - Pairs - Polymorphic pairs - pair from lambda (first element)") {

    assert((

      (∀t0.(t0 → (t0, string)))) == (

      (∀t0.(t0 → (t0, string)))))

  }

  test("Algebraic types - Pairs - Polymorphic pairs - pair from lambda (second element)") {

    assert((

      (∀t0.(t0 → (string, t0)))) == (

      (∀t0.(t0 → (string, t0)))))

  }

  test("Algebraic types - Pairs - Polymorphic pairs - pair from two lambdas") {

    assert((

      (∀t0.(∀t1.(t0 → t1 → (t0, t1))))) == (

      (∀t0.(∀t1.(t0 → t1 → (t0, t1))))))

  }

  test("Algebraic types - Pairs - Polymorphic pairs - pair with repeated variable") {

    assert((

      (∀t0.(t0 → (t0, t0)))) == (

      (∀t0.(t0 → (t0, t0)))))

  }

  // Pairs in complex contexts

  test("Algebraic types - Pairs - Pairs in complex contexts - pair in list") {

    assert((

      list<(int32, string)>) == (

      list<(int32, string)>))

  }

  test("Algebraic types - Pairs - Pairs in complex contexts - pair in let binding") {

    assert((

      (int32, string)) == (

      (int32, string)))

  }

  // Nested pairs

  test("Algebraic types - Pairs - Nested pairs - pair of pairs") {

    assert((

      ((int32, string), (boolean, int32))) == (

      ((int32, string), (boolean, int32))))

  }

  test("Algebraic types - Pairs - Nested pairs - pair with list") {

    assert((

      (list<int32>, string)) == (

      (list<int32>, string)))

  }

  test("Algebraic types - Pairs - Nested pairs - list of pairs") {

    assert((

      list<(int32, string)>) == (

      list<(int32, string)>))

  }

  // Pairs with complex types

  test("Algebraic types - Pairs - Pairs with complex types - pair with record on first") {

    assert((

      (Person, int32)) == (

      (Person, int32)))

  }

  test("Algebraic types - Pairs - Pairs with complex types - pair with record on second") {

    assert((

      (string, Person)) == (

      (string, Person)))

  }

  // Eithers

  // Left values

  test("Algebraic types - Eithers - Left values - left int") {

    assert((

      (∀t0.either<int32, t0>)) == (

      (∀t0.either<int32, t0>)))

  }

  test("Algebraic types - Eithers - Left values - left string") {

    assert((

      (∀t0.either<string, t0>)) == (

      (∀t0.either<string, t0>)))

  }

  test("Algebraic types - Eithers - Left values - left boolean") {

    assert((

      (∀t0.either<boolean, t0>)) == (

      (∀t0.either<boolean, t0>)))

  }

  // Right values

  test("Algebraic types - Eithers - Right values - right int") {

    assert((

      (∀t0.either<t0, int32>)) == (

      (∀t0.either<t0, int32>)))

  }

  test("Algebraic types - Eithers - Right values - right string") {

    assert((

      (∀t0.either<t0, string>)) == (

      (∀t0.either<t0, string>)))

  }

  test("Algebraic types - Eithers - Right values - right boolean") {

    assert((

      (∀t0.either<t0, boolean>)) == (

      (∀t0.either<t0, boolean>)))

  }

  // Polymorphic eithers

  test("Algebraic types - Eithers - Polymorphic eithers - left from lambda") {

    assert((

      (∀t0.(∀t1.(t0 → either<t0, t1>)))) == (

      (∀t0.(∀t1.(t0 → either<t0, t1>)))))

  }

  test("Algebraic types - Eithers - Polymorphic eithers - right from lambda") {

    assert((

      (∀t0.(∀t1.(t0 → either<t1, t0>)))) == (

      (∀t0.(∀t1.(t0 → either<t1, t0>)))))

  }

  test("Algebraic types - Eithers - Polymorphic eithers - either from two lambdas") {

    assert((

      (∀t0.(boolean → t0 → either<t0, t0>))) == (

      (∀t0.(boolean → t0 → either<t0, t0>))))

  }

  // Eithers in complex contexts

  test("Algebraic types - Eithers - Eithers in complex contexts - either in list") {

    assert((

      list<either<string, int32>>) == (

      list<either<string, int32>>))

  }

  test("Algebraic types - Eithers - Eithers in complex contexts - either in let binding") {

    assert((

      (∀t0.either<t0, int32>)) == (

      (∀t0.either<t0, int32>)))

  }

  // Nested eithers

  test("Algebraic types - Eithers - Nested eithers - either of either (left left)") {

    assert((

      (∀t0.(∀t1.either<either<int32, t0>, t1>))) == (

      (∀t0.(∀t1.either<either<int32, t0>, t1>))))

  }

  test("Algebraic types - Eithers - Nested eithers - either of either (left right)") {

    assert((

      (∀t0.(∀t1.either<either<t0, string>, t1>))) == (

      (∀t0.(∀t1.either<either<t0, string>, t1>))))

  }

  test("Algebraic types - Eithers - Nested eithers - either of either (right)") {

    assert((

      (∀t0.either<t0, boolean>)) == (

      (∀t0.either<t0, boolean>)))

  }

  test("Algebraic types - Eithers - Nested eithers - either of list") {

    assert((

      (∀t0.either<list<int32>, t0>)) == (

      (∀t0.either<list<int32>, t0>)))

  }

  test("Algebraic types - Eithers - Nested eithers - list of eithers") {

    assert((

      list<either<string, int32>>) == (

      list<either<string, int32>>))

  }

  // Eithers with complex types

  test("Algebraic types - Eithers - Eithers with complex types - either with record on left") {

    assert((

      (∀t0.either<Person, t0>)) == (

      (∀t0.either<Person, t0>)))

  }

  test("Algebraic types - Eithers - Eithers with complex types - either with record on right") {

    assert((

      (∀t0.either<t0, Person>)) == (

      (∀t0.either<t0, Person>)))

  }

  // Optionals

  // Monomorphic optionals

  test("Algebraic types - Optionals - Monomorphic optionals - nothing") {

    assert((

      (∀t0.maybe<t0>)) == (

      (∀t0.maybe<t0>)))

  }

  test("Algebraic types - Optionals - Monomorphic optionals - just int") {

    assert((

      maybe<int32>) == (

      maybe<int32>))

  }

  test("Algebraic types - Optionals - Monomorphic optionals - just string") {

    assert((

      maybe<string>) == (

      maybe<string>))

  }

  test("Algebraic types - Optionals - Monomorphic optionals - just boolean") {

    assert((

      maybe<boolean>) == (

      maybe<boolean>))

  }

  // Polymorphic optionals

  test("Algebraic types - Optionals - Polymorphic optionals - optional from lambda") {

    assert((

      (∀t0.(t0 → maybe<t0>))) == (

      (∀t0.(t0 → maybe<t0>))))

  }

  test("Algebraic types - Optionals - Polymorphic optionals - nothing from lambda") {

    assert((

      (∀t0.(∀t1.(t0 → maybe<t1>)))) == (

      (∀t0.(∀t1.(t0 → maybe<t1>)))))

  }

  test("Algebraic types - Optionals - Polymorphic optionals - conditional optional") {

    assert((

      (∀t0.(t0 → boolean → maybe<t0>))) == (

      (∀t0.(t0 → boolean → maybe<t0>))))

  }

  // Optionals in complex contexts

  test("Algebraic types - Optionals - Optionals in complex contexts - optional in record") {

    assert((

      (BuddyListA @ string)) == (

      (BuddyListA @ string)))

  }

  test("Algebraic types - Optionals - Optionals in complex contexts - optional in let binding") {

    assert((

      maybe<int32>) == (

      maybe<int32>))

  }

  // Nested optionals

  test("Algebraic types - Optionals - Nested optionals - optional of optional") {

    assert((

      maybe<maybe<string>>) == (

      maybe<maybe<string>>))

  }

  test("Algebraic types - Optionals - Nested optionals - optional of list") {

    assert((

      maybe<list<int32>>) == (

      maybe<list<int32>>))

  }

  test("Algebraic types - Optionals - Nested optionals - list of optionals") {

    assert((

      list<maybe<string>>) == (

      list<maybe<string>>))

  }

  // Optionals with complex types

  test("Algebraic types - Optionals - Optionals with complex types - optional map") {

    assert((

      maybe<map<string, int32>>) == (

      maybe<map<string, int32>>))

  }

  // Collections

  // Lists

  // Lists of literals

  test("Collections - Lists - Lists of literals - int list") {

    assert((

      list<int32>) == (

      list<int32>))

  }

  test("Collections - Lists - Lists of literals - string list") {

    assert((

      list<string>) == (

      list<string>))

  }

  test("Collections - Lists - Lists of literals - single element list") {

    assert((

      list<bigint>) == (

      list<bigint>))

  }

  test("Collections - Lists - Lists of literals - mixed numeric types") {

    assert((

      list<float32>) == (

      list<float32>))

  }

  // Empty lists

  test("Collections - Lists - Empty lists - empty list") {

    assert((

      (∀t0.list<t0>)) == (

      (∀t0.list<t0>)))

  }

  test("Collections - Lists - Empty lists - pair of empty lists") {

    assert((

      (∀t0.(∀t1.(list<t0>, list<t1>)))) == (

      (∀t0.(∀t1.(list<t0>, list<t1>)))))

  }

  test("Collections - Lists - Empty lists - empty list in tuple") {

    assert((

      (∀t0.(list<t0>, string))) == (

      (∀t0.(list<t0>, string))))

  }

  // Polymorphic lists

  test("Collections - Lists - Polymorphic lists - list from lambda") {

    assert((

      (∀t0.(t0 → list<t0>))) == (

      (∀t0.(t0 → list<t0>))))

  }

  test("Collections - Lists - Polymorphic lists - list with repeated var") {

    assert((

      (∀t0.(t0 → list<t0>))) == (

      (∀t0.(t0 → list<t0>))))

  }

  test("Collections - Lists - Polymorphic lists - list from two lambdas") {

    assert((

      (∀t0.(t0 → t0 → list<t0>))) == (

      (∀t0.(t0 → t0 → list<t0>))))

  }

  // Nested lists

  test("Collections - Lists - Nested lists - list of lists") {

    assert((

      list<list<int32>>) == (

      list<list<int32>>))

  }

  test("Collections - Lists - Nested lists - empty nested lists") {

    assert((

      (∀t0.list<list<t0>>)) == (

      (∀t0.list<list<t0>>)))

  }

  test("Collections - Lists - Nested lists - nested polymorphic") {

    assert((

      (∀t0.(t0 → list<list<t0>>))) == (

      (∀t0.(t0 → list<list<t0>>))))

  }

  // Lists in complex contexts

  test("Collections - Lists - Lists in complex contexts - multiple lists in tuple") {

    assert((

      (list<int32>, list<string>)) == (

      (list<int32>, list<string>)))

  }

  // Sets

  // Monomorphic sets

  test("Collections - Sets - Monomorphic sets - empty set") {

    assert((

      (∀t0.set<t0>)) == (

      (∀t0.set<t0>)))

  }

  test("Collections - Sets - Monomorphic sets - int set") {

    assert((

      set<int32>) == (

      set<int32>))

  }

  test("Collections - Sets - Monomorphic sets - string set") {

    assert((

      set<string>) == (

      set<string>))

  }

  test("Collections - Sets - Monomorphic sets - single element set") {

    assert((

      set<boolean>) == (

      set<boolean>))

  }

  // Polymorphic sets

  test("Collections - Sets - Polymorphic sets - set from lambda") {

    assert((

      (∀t0.(t0 → set<t0>))) == (

      (∀t0.(t0 → set<t0>))))

  }

  test("Collections - Sets - Polymorphic sets - set with repeated variable") {

    assert((

      (∀t0.(t0 → set<t0>))) == (

      (∀t0.(t0 → set<t0>))))

  }

  test("Collections - Sets - Polymorphic sets - set from two variables") {

    assert((

      (∀t0.(t0 → t0 → set<t0>))) == (

      (∀t0.(t0 → t0 → set<t0>))))

  }

  // Sets in complex contexts

  test("Collections - Sets - Sets in complex contexts - set in tuple") {

    assert((

      (set<int32>, string)) == (

      (set<int32>, string)))

  }

  test("Collections - Sets - Sets in complex contexts - set in let binding") {

    assert((

      set<int32>) == (

      set<int32>))

  }

  // Nested sets

  test("Collections - Sets - Nested sets - set of lists") {

    assert((

      set<list<string>>) == (

      set<list<string>>))

  }

  test("Collections - Sets - Nested sets - set of tuples") {

    assert((

      set<(int32, int32)>) == (

      set<(int32, int32)>))

  }

  test("Collections - Sets - Nested sets - set of sets") {

    assert((

      set<set<string>>) == (

      set<set<string>>))

  }

  // Sets with complex types

  test("Collections - Sets - Sets with complex types - set of records") {

    assert((

      set<Person>) == (

      set<Person>))

  }

  test("Collections - Sets - Sets with complex types - set of optionals") {

    assert((

      set<maybe<int32>>) == (

      set<maybe<int32>>))

  }

  test("Collections - Sets - Sets with complex types - set of maps") {

    assert((

      set<map<string, int32>>) == (

      set<map<string, int32>>))

  }

  // Maps

  // Monomorphic maps

  test("Collections - Maps - Monomorphic maps - empty map") {

    assert((

      (∀t0.(∀t1.map<t0, t1>))) == (

      (∀t0.(∀t1.map<t0, t1>))))

  }

  test("Collections - Maps - Monomorphic maps - int to string map") {

    assert((

      map<int32, string>) == (

      map<int32, string>))

  }

  test("Collections - Maps - Monomorphic maps - string to int map") {

    assert((

      map<string, int32>) == (

      map<string, int32>))

  }

  test("Collections - Maps - Monomorphic maps - single entry map") {

    assert((

      map<bigint, boolean>) == (

      map<bigint, boolean>))

  }

  // Polymorphic maps

  test("Collections - Maps - Polymorphic maps - map from lambda keys") {

    assert((

      (∀t0.(t0 → map<t0, string>))) == (

      (∀t0.(t0 → map<t0, string>))))

  }

  test("Collections - Maps - Polymorphic maps - map from lambda values") {

    assert((

      (∀t0.(t0 → map<string, t0>))) == (

      (∀t0.(t0 → map<string, t0>))))

  }

  test("Collections - Maps - Polymorphic maps - map from lambda both") {

    assert((

      (∀t0.(∀t1.(t0 → t1 → map<t0, t1>)))) == (

      (∀t0.(∀t1.(t0 → t1 → map<t0, t1>)))))

  }

  test("Collections - Maps - Polymorphic maps - map with repeated variables") {

    assert((

      (∀t0.(t0 → map<t0, t0>))) == (

      (∀t0.(t0 → map<t0, t0>))))

  }

  // Maps in complex contexts

  test("Collections - Maps - Maps in complex contexts - map in tuple") {

    assert((

      (map<int32, string>, string)) == (

      (map<int32, string>, string)))

  }

  test("Collections - Maps - Maps in complex contexts - nested maps") {

    assert((

      map<string, map<int32, boolean>>) == (

      map<string, map<int32, boolean>>))

  }

  test("Collections - Maps - Maps in complex contexts - map in let binding") {

    assert((

      map<string, int32>) == (

      map<string, int32>))

  }

  // Maps with complex types

  test("Collections - Maps - Maps with complex types - map of records") {

    assert((

      map<string, Person>) == (

      map<string, Person>))

  }

  test("Collections - Maps - Maps with complex types - map of lists") {

    assert((

      map<int32, list<string>>) == (

      map<int32, list<string>>))

  }

  test("Collections - Maps - Maps with complex types - map of tuples") {

    assert((

      map<string, (int32, int32)>) == (

      map<string, (int32, int32)>))

  }

  // Fundamentals

  // Literals

  // Boolean literals

  test("Fundamentals - Literals - Boolean literals - true") {

    assert((

      boolean) == (

      boolean))

  }

  test("Fundamentals - Literals - Boolean literals - false") {

    assert((

      boolean) == (

      boolean))

  }

  // String literals

  test("Fundamentals - Literals - String literals - simple string") {

    assert((

      string) == (

      string))

  }

  test("Fundamentals - Literals - String literals - empty string") {

    assert((

      string) == (

      string))

  }

  test("Fundamentals - Literals - String literals - unicode string") {

    assert((

      string) == (

      string))

  }

  // Integer literals

  test("Fundamentals - Literals - Integer literals - bigint") {

    assert((

      bigint) == (

      bigint))

  }

  test("Fundamentals - Literals - Integer literals - int8") {

    assert((

      int8) == (

      int8))

  }

  test("Fundamentals - Literals - Integer literals - int16") {

    assert((

      int16) == (

      int16))

  }

  test("Fundamentals - Literals - Integer literals - int32") {

    assert((

      int32) == (

      int32))

  }

  test("Fundamentals - Literals - Integer literals - int64") {

    assert((

      int64) == (

      int64))

  }

  test("Fundamentals - Literals - Integer literals - uint8") {

    assert((

      uint8) == (

      uint8))

  }

  test("Fundamentals - Literals - Integer literals - uint16") {

    assert((

      uint16) == (

      uint16))

  }

  test("Fundamentals - Literals - Integer literals - uint32") {

    assert((

      uint32) == (

      uint32))

  }

  test("Fundamentals - Literals - Integer literals - uint64") {

    assert((

      uint64) == (

      uint64))

  }

  // Float literals

  test("Fundamentals - Literals - Float literals - bigfloat") {

    assert((

      bigfloat) == (

      bigfloat))

  }

  test("Fundamentals - Literals - Float literals - float32") {

    assert((

      float32) == (

      float32))

  }

  test("Fundamentals - Literals - Float literals - float64") {

    assert((

      float64) == (

      float64))

  }

  // Literals in complex contexts

  test("Fundamentals - Literals - Literals in complex contexts - literals in tuple") {

    assert((

      (boolean, (string, (int32, float32)))) == (

      (boolean, (string, (int32, float32)))))

  }

  test("Fundamentals - Literals - Literals in complex contexts - literals in list") {

    assert((

      list<string>) == (

      list<string>))

  }

  // Variables

  // Simple variable lookup

  test("Fundamentals - Variables - Simple variable lookup - int variable") {

    assert((

      (∀t0.(t0 → t0))) == (

      (∀t0.(t0 → t0))))

  }

  test("Fundamentals - Variables - Simple variable lookup - variable in let binding") {

    assert((

      int32) == (

      int32))

  }

  test("Fundamentals - Variables - Simple variable lookup - multiple variables") {

    assert((

      (string, int32)) == (

      (string, int32)))

  }

  // Variable scoping

  test("Fundamentals - Variables - Variable scoping - lambda parameter") {

    assert((

      (∀t0.(∀t1.(t0 → t1 → t0)))) == (

      (∀t0.(∀t1.(t0 → t1 → t0)))))

  }

  test("Fundamentals - Variables - Variable scoping - let binding scope") {

    assert((

      int32) == (

      int32))

  }

  test("Fundamentals - Variables - Variable scoping - variable shadowing") {

    assert((

      (∀t0.(t0 → t0))) == (

      (∀t0.(t0 → t0))))

  }

  test("Fundamentals - Variables - Variable scoping - nested scoping") {

    assert((

      (∀t0.(∀t1.(t0 → t1 → (t0, (t0, t1)))))) == (

      (∀t0.(∀t1.(t0 → t1 → (t0, (t0, t1)))))))

  }

  // Polymorphic variables

  test("Fundamentals - Variables - Polymorphic variables - polymorphic function") {

    assert((

      (∀t0.(t0 → t0))) == (

      (∀t0.(t0 → t0))))

  }

  test("Fundamentals - Variables - Polymorphic variables - polymorphic application") {

    assert((

      (int32, string)) == (

      (int32, string)))

  }

  test("Fundamentals - Variables - Polymorphic variables - higher order polymorphic") {

    assert((

      (∀t0.(∀t1.((t0 → t1) → t0 → t1)))) == (

      (∀t0.(∀t1.((t0 → t1) → t0 → t1)))))

  }

  // Variables in complex contexts

  test("Fundamentals - Variables - Variables in complex contexts - variable in record") {

    assert((

      (string → Person)) == (

      (string → Person)))

  }

  test("Fundamentals - Variables - Variables in complex contexts - variable in list") {

    assert((

      (∀t0.(t0 → list<t0>))) == (

      (∀t0.(t0 → list<t0>))))

  }

  test("Fundamentals - Variables - Variables in complex contexts - variable in map") {

    assert((

      (∀t0.(∀t1.(t0 → t1 → map<t0, t1>)))) == (

      (∀t0.(∀t1.(t0 → t1 → map<t0, t1>)))))

  }

  test("Fundamentals - Variables - Variables in complex contexts - variable in optional") {

    assert((

      (∀t0.(t0 → maybe<t0>))) == (

      (∀t0.(t0 → maybe<t0>))))

  }

  // Recursive variables

  test("Fundamentals - Variables - Recursive variables - simple recursion") {

    assert((

      (int32 → int32)) == (

      (int32 → int32)))

  }

  test("Fundamentals - Variables - Recursive variables - mutual recursion") {

    assert((

      (int32 → int32)) == (

      (int32 → int32)))

  }

  // Lambdas

  // Simple lambdas

  test("Fundamentals - Lambdas - Simple lambdas - identity function") {

    assert((

      (∀t0.(t0 → t0))) == (

      (∀t0.(t0 → t0))))

  }

  test("Fundamentals - Lambdas - Simple lambdas - constant function") {

    assert((

      (∀t0.(t0 → int32))) == (

      (∀t0.(t0 → int32))))

  }

  // Multi-parameter lambdas

  test("Fundamentals - Lambdas - Multi-parameter lambdas - two parameters") {

    assert((

      (∀t0.(∀t1.(t0 → t1 → t0)))) == (

      (∀t0.(∀t1.(t0 → t1 → t0)))))

  }

  test("Fundamentals - Lambdas - Multi-parameter lambdas - three parameters") {

    assert((

      (∀t0.(∀t1.(∀t2.(t0 → t1 → t2 → t1))))) == (

      (∀t0.(∀t1.(∀t2.(t0 → t1 → t2 → t1))))))

  }

  test("Fundamentals - Lambdas - Multi-parameter lambdas - parameter reuse") {

    assert((

      (∀t0.(∀t1.(t0 → t1 → (t0, (t0, t1)))))) == (

      (∀t0.(∀t1.(t0 → t1 → (t0, (t0, t1)))))))

  }

  // Lambdas with operations

  test("Fundamentals - Lambdas - Lambdas with operations - lambda with primitive") {

    assert((

      (int32 → int32)) == (

      (int32 → int32)))

  }

  test("Fundamentals - Lambdas - Lambdas with operations - lambda with application") {

    assert((

      (∀t0.(∀t1.((t0 → t1) → t0 → t1)))) == (

      (∀t0.(∀t1.((t0 → t1) → t0 → t1)))))

  }

  test("Fundamentals - Lambdas - Lambdas with operations - lambda with construction") {

    assert((

      (∀t0.(∀t1.(t0 → t1 → (t0, t1))))) == (

      (∀t0.(∀t1.(t0 → t1 → (t0, t1))))))

  }

  // Nested lambdas

  test("Fundamentals - Lambdas - Nested lambdas - lambda returning lambda") {

    assert((

      (∀t0.(∀t1.(∀t2.(t0 → t1 → t2 → t0))))) == (

      (∀t0.(∀t1.(∀t2.(t0 → t1 → t2 → t0))))))

  }

  test("Fundamentals - Lambdas - Nested lambdas - lambda with let binding") {

    assert((

      (∀t0.(t0 → t0))) == (

      (∀t0.(t0 → t0))))

  }

  test("Fundamentals - Lambdas - Nested lambdas - lambda with inner lambda") {

    assert((

      (∀t0.(t0 → t0))) == (

      (∀t0.(t0 → t0))))

  }

  // Lambdas in complex contexts

  test("Fundamentals - Lambdas - Lambdas in complex contexts - lambda in tuple") {

    assert((

      (∀t0.((t0 → t0), int32))) == (

      (∀t0.((t0 → t0), int32))))

  }

  test("Fundamentals - Lambdas - Lambdas in complex contexts - lambda in list") {

    assert((

      list<(int32 → int32)>) == (

      list<(int32 → int32)>))

  }

  test("Fundamentals - Lambdas - Lambdas in complex contexts - lambda in record") {

    assert((

      (string → Person)) == (

      (string → Person)))

  }

  // Higher-order lambdas

  test("Fundamentals - Lambdas - Higher-order lambdas - function composition") {

    assert((

      (∀t0.(∀t1.(∀t2.((t0 → t1) → (t2 → t0) → t2 → t1))))) == (

      (∀t0.(∀t1.(∀t2.((t0 → t1) → (t2 → t0) → t2 → t1))))))

  }

  test("Fundamentals - Lambdas - Higher-order lambdas - function application") {

    assert((

      (∀t0.(∀t1.((t0 → t1) → t0 → t1)))) == (

      (∀t0.(∀t1.((t0 → t1) → t0 → t1)))))

  }

  test("Fundamentals - Lambdas - Higher-order lambdas - curried function") {

    assert((

      (∀t0.(boolean → t0 → t0 → t0))) == (

      (∀t0.(boolean → t0 → t0 → t0))))

  }

  // Applications

  // Simple function applications

  test("Fundamentals - Applications - Simple function applications - identity application") {

    assert((

      int32) == (

      int32))

  }

  test("Fundamentals - Applications - Simple function applications - primitive application") {

    assert((

      int32) == (

      int32))

  }

  test("Fundamentals - Applications - Simple function applications - string concatenation") {

    assert((

      string) == (

      string))

  }

  // Partial applications

  test("Fundamentals - Applications - Partial applications - partially applied add") {

    assert((

      (int32 → int32)) == (

      (int32 → int32)))

  }

  test("Fundamentals - Applications - Partial applications - partially applied string cat") {

    assert((

      (string → string)) == (

      (string → string)))

  }

  // Higher-order applications

  test("Fundamentals - Applications - Higher-order applications - apply function to function") {

    assert((

      int32) == (

      int32))

  }

  test("Fundamentals - Applications - Higher-order applications - function composition") {

    assert((

      int32) == (

      int32))

  }

  // Polymorphic applications

  test("Fundamentals - Applications - Polymorphic applications - polymorphic identity") {

    assert((

      (int32, string)) == (

      (int32, string)))

  }

  test("Fundamentals - Applications - Polymorphic applications - polymorphic const") {

    assert((

      string) == (

      string))

  }

  test("Fundamentals - Applications - Polymorphic applications - polymorphic flip") {

    assert((

      string) == (

      string))

  }

  // Applications in complex contexts

  test("Fundamentals - Applications - Applications in complex contexts - application in tuple") {

    assert((

      (int32, string)) == (

      (int32, string)))

  }

  test("Fundamentals - Applications - Applications in complex contexts - application in record") {

    assert((

      Person) == (

      Person))

  }

  test("Fundamentals - Applications - Applications in complex contexts - application in let binding") {

    assert((

      int32) == (

      int32))

  }

  test("Fundamentals - Applications - Applications in complex contexts - nested applications") {

    assert((

      int32) == (

      int32))

  }

  // Applications with complex arguments

  test("Fundamentals - Applications - Applications with complex arguments - application with record argument") {

    assert((

      string) == (

      string))

  }

  test("Fundamentals - Applications - Applications with complex arguments - application with list argument") {

    assert((

      string) == (

      string))

  }

  // Let terms

  // Simple let bindings

  test("Fundamentals - Let terms - Simple let bindings - single binding") {

    assert((

      int32) == (

      int32))

  }

  test("Fundamentals - Let terms - Simple let bindings - multiple bindings") {

    assert((

      (int32, string)) == (

      (int32, string)))

  }

  // Let terms with shadowing

  test("Fundamentals - Let terms - Let terms with shadowing - lambda parameter shadowing let binding") {

    assert((

      (∀t0.(t0 → t0))) == (

      (∀t0.(t0 → t0))))

  }

  test("Fundamentals - Let terms - Let terms with shadowing - nested lambda shadowing") {

    assert((

      (∀t0.(∀t1.(t0 → t1 → (t1, t0))))) == (

      (∀t0.(∀t1.(t0 → t1 → (t1, t0))))))

  }

  test("Fundamentals - Let terms - Let terms with shadowing - multiple levels of let shadowing") {

    assert((

      boolean) == (

      boolean))

  }

  test("Fundamentals - Let terms - Let terms with shadowing - let shadowing with lambda and reference to outer binding") {

    assert((

      (∀t0.(t0 → (t0, int32)))) == (

      (∀t0.(t0 → (t0, int32)))))

  }

  // Recursive bindings

  test("Fundamentals - Let terms - Recursive bindings - simple arithmetic recursion") {

    assert((

      int32) == (

      int32))

  }

  // Mutual recursion

  test("Fundamentals - Let terms - Mutual recursion - mutually recursive data") {

    assert((

      (BuddyListA @ int32)) == (

      (BuddyListA @ int32)))

  }

  test("Fundamentals - Let terms - Mutual recursion - (monomorphic) mutually recursive functions") {

    assert((

      int32) == (

      int32))

  }

  // Nested let terms

  test("Fundamentals - Let terms - Nested let terms - monomorphic nesting") {

    assert((

      int32) == (

      int32))

  }

  test("Fundamentals - Let terms - Nested let terms - polymorphic nesting") {

    assert((

      string) == (

      string))

  }

  test("Fundamentals - Let terms - Nested let terms - variable capture avoidance") {

    assert((

      (∀t0.(t0 → t0))) == (

      (∀t0.(t0 → t0))))

  }

  test("Fundamentals - Let terms - Nested let terms - simple let in lambda") {

    assert((

      (∀t0.(t0 → t0))) == (

      (∀t0.(t0 → t0))))

  }

  // Let with complex expressions

  test("Fundamentals - Let terms - Let with complex expressions - let in record") {

    assert((

      Person) == (

      Person))

  }

  test("Fundamentals - Let terms - Let with complex expressions - let in function application") {

    assert((

      int32) == (

      int32))

  }

  test("Fundamentals - Let terms - Let with complex expressions - polymorphic let binding") {

    assert((

      (int32, string)) == (

      (int32, string)))

  }

  test("Fundamentals - Let terms - Let with complex expressions - composition") {

    assert((

      int32) == (

      int32))

  }

  // Primitives

  // Nullary primitives

  test("Fundamentals - Primitives - Nullary primitives - empty map") {

    assert((

      (∀t0.(∀t1.map<t0, t1>))) == (

      (∀t0.(∀t1.map<t0, t1>))))

  }

  test("Fundamentals - Primitives - Nullary primitives - empty set") {

    assert((

      (∀t0.set<t0>)) == (

      (∀t0.set<t0>)))

  }

  // Unary primitives

  test("Fundamentals - Primitives - Unary primitives - lists head") {

    assert((

      (∀t0.(list<t0> → t0))) == (

      (∀t0.(list<t0> → t0))))

  }

  test("Fundamentals - Primitives - Unary primitives - math neg") {

    assert((

      (int32 → int32)) == (

      (int32 → int32)))

  }

  test("Fundamentals - Primitives - Unary primitives - logic not") {

    assert((

      (boolean → boolean)) == (

      (boolean → boolean)))

  }

  // Binary primitives

  test("Fundamentals - Primitives - Binary primitives - math add") {

    assert((

      (int32 → int32 → int32)) == (

      (int32 → int32 → int32)))

  }

  test("Fundamentals - Primitives - Binary primitives - lists cons") {

    assert((

      (∀t0.(t0 → list<t0> → list<t0>))) == (

      (∀t0.(t0 → list<t0> → list<t0>))))

  }

  test("Fundamentals - Primitives - Binary primitives - maps insert") {

    assert((

      (∀t0.(∀t1.(t0 → t1 → map<t0, t1> → map<t0, t1>)))) == (

      (∀t0.(∀t1.(t0 → t1 → map<t0, t1> → map<t0, t1>)))))

  }

  // Ternary primitives

  test("Fundamentals - Primitives - Ternary primitives - logic ifElse") {

    assert((

      (∀t0.(boolean → t0 → t0 → t0))) == (

      (∀t0.(boolean → t0 → t0 → t0))))

  }

  test("Fundamentals - Primitives - Ternary primitives - lists foldl") {

    assert((

      (∀t0.(∀t1.((t0 → t1 → t0) → t0 → list<t1> → t0)))) == (

      (∀t0.(∀t1.((t0 → t1 → t0) → t0 → list<t1> → t0)))))

  }

  // Monomorphic vs polymorphic

  test("Fundamentals - Primitives - Monomorphic vs polymorphic - monomorphic math") {

    assert((

      (int32 → int32 → int32)) == (

      (int32 → int32 → int32)))

  }

  test("Fundamentals - Primitives - Monomorphic vs polymorphic - polymorphic identity") {

    assert((

      (∀t0.(t0 → t0))) == (

      (∀t0.(t0 → t0))))

  }

  test("Fundamentals - Primitives - Monomorphic vs polymorphic - polymorphic map") {

    assert((

      (∀t0.(∀t1.((t0 → t1) → list<t0> → list<t1>)))) == (

      (∀t0.(∀t1.((t0 → t1) → list<t0> → list<t1>)))))

  }

  // Higher-order primitives

  test("Fundamentals - Primitives - Higher-order primitives - lists map function") {

    assert((

      (list<int32> → list<int32>)) == (

      (list<int32> → list<int32>)))

  }

  test("Fundamentals - Primitives - Higher-order primitives - lists filter") {

    assert((

      (∀t0.((t0 → boolean) → list<t0> → list<t0>))) == (

      (∀t0.((t0 → boolean) → list<t0> → list<t0>))))

  }

  test("Fundamentals - Primitives - Higher-order primitives - optionals maybe") {

    assert((

      (∀t0.(∀t1.(t0 → (t1 → t0) → maybe<t1> → t0)))) == (

      (∀t0.(∀t1.(t0 → (t1 → t0) → maybe<t1> → t0)))))

  }

  // Primitives in complex contexts

  test("Fundamentals - Primitives - Primitives in complex contexts - primitive composition") {

    assert((

      list<int32>) == (

      list<int32>))

  }

  test("Fundamentals - Primitives - Primitives in complex contexts - nested higher-order") {

    assert((

      list<list<int32>>) == (

      list<list<int32>>))

  }

  // Nominal types

  // Records

  // Monomorphic records

  test("Nominal types - Records - Monomorphic records - latlon record") {

    assert((

      LatLon) == (

      LatLon))

  }

  test("Nominal types - Records - Monomorphic records - latlon with variable") {

    assert((

      (float32 → LatLon)) == (

      (float32 → LatLon)))

  }

  test("Nominal types - Records - Monomorphic records - person record") {

    assert((

      Person) == (

      Person))

  }

  test("Nominal types - Records - Monomorphic records - empty record") {

    assert((

      Unit) == (

      Unit))

  }

  test("Nominal types - Records - Monomorphic records - person with variables") {

    assert((

      (string → int32 → Person)) == (

      (string → int32 → Person)))

  }

  // Polymorphic records

  test("Nominal types - Records - Polymorphic records - latlon poly float") {

    assert((

      (LatLonPoly @ float32)) == (

      (LatLonPoly @ float32)))

  }

  test("Nominal types - Records - Polymorphic records - latlon poly int64") {

    assert((

      (LatLonPoly @ int64)) == (

      (LatLonPoly @ int64)))

  }

  test("Nominal types - Records - Polymorphic records - latlon poly variable") {

    assert((

      (∀t0.(t0 → (LatLonPoly @ t0)))) == (

      (∀t0.(t0 → (LatLonPoly @ t0)))))

  }

  test("Nominal types - Records - Polymorphic records - buddylist string") {

    assert((

      (BuddyListA @ string)) == (

      (BuddyListA @ string)))

  }

  test("Nominal types - Records - Polymorphic records - buddylist variable") {

    assert((

      (∀t0.(t0 → (BuddyListA @ t0)))) == (

      (∀t0.(t0 → (BuddyListA @ t0)))))

  }

  // Records in complex contexts

  test("Nominal types - Records - Records in complex contexts - records in tuple") {

    assert((

      (Person, LatLon)) == (

      (Person, LatLon)))

  }

  test("Nominal types - Records - Records in complex contexts - poly records in tuple") {

    assert((

      ((LatLonPoly @ int32), (BuddyListA @ string))) == (

      ((LatLonPoly @ int32), (BuddyListA @ string))))

  }

  test("Nominal types - Records - Records in complex contexts - recursive record") {

    assert((

      IntList) == (

      IntList))

  }

  // Multi-parameter polymorphic records

  test("Nominal types - Records - Multi-parameter polymorphic records - triple with three monomorphic types") {

    assert((

      (Triple @ int32 @ string @ boolean)) == (

      (Triple @ int32 @ string @ boolean)))

  }

  test("Nominal types - Records - Multi-parameter polymorphic records - triple with PersonOrSomething containing map") {

    assert((

      (∀t0.(∀t1.(t0 → t1 → (Triple @ string @ (PersonOrSomething @ map<t0, t1>) @ int32))))) == (

      (∀t0.(∀t1.(t0 → t1 → (Triple @ string @ (PersonOrSomething @ map<t0, t1>) @ int32))))))

  }

  // Unions

  // Simple union injections

  test("Nominal types - Unions - Simple union injections - inject into Comparison lessThan variant") {

    assert((

      Comparison) == (

      Comparison))

  }

  test("Nominal types - Unions - Simple union injections - inject into Comparison equalTo variant") {

    assert((

      Comparison) == (

      Comparison))

  }

  test("Nominal types - Unions - Simple union injections - inject into Comparison greaterThan variant") {

    assert((

      Comparison) == (

      Comparison))

  }

  // Union injections with data

  test("Nominal types - Unions - Union injections with data - inject into Number int variant") {

    assert((

      Number) == (

      Number))

  }

  test("Nominal types - Unions - Union injections with data - inject into Number float variant") {

    assert((

      Number) == (

      Number))

  }

  test("Nominal types - Unions - Union injections with data - inject into Timestamp unixTimeMillis variant") {

    assert((

      Timestamp) == (

      Timestamp))

  }

  test("Nominal types - Unions - Union injections with data - inject into Timestamp date variant") {

    assert((

      Timestamp) == (

      Timestamp))

  }

  // Polymorphic union injections

  test("Nominal types - Unions - Polymorphic union injections - inject person into PersonOrSomething") {

    assert((

      (∀t0.(PersonOrSomething @ t0))) == (

      (∀t0.(PersonOrSomething @ t0))))

  }

  test("Nominal types - Unions - Polymorphic union injections - inject string into PersonOrSomething other variant") {

    assert((

      (PersonOrSomething @ string)) == (

      (PersonOrSomething @ string)))

  }

  test("Nominal types - Unions - Polymorphic union injections - inject int into PersonOrSomething other variant") {

    assert((

      (PersonOrSomething @ int32)) == (

      (PersonOrSomething @ int32)))

  }

  // Polymorphic recursive union injections

  test("Nominal types - Unions - Polymorphic recursive union injections - inject boolean into UnionPolymorphicRecursive") {

    assert((

      (∀t0.(UnionPolymorphicRecursive @ t0))) == (

      (∀t0.(UnionPolymorphicRecursive @ t0))))

  }

  test("Nominal types - Unions - Polymorphic recursive union injections - inject string value into UnionPolymorphicRecursive") {

    assert((

      (UnionPolymorphicRecursive @ string)) == (

      (UnionPolymorphicRecursive @ string)))

  }

  test("Nominal types - Unions - Polymorphic recursive union injections - inject int value into UnionPolymorphicRecursive") {

    assert((

      (UnionPolymorphicRecursive @ int32)) == (

      (UnionPolymorphicRecursive @ int32)))

  }

  // Polymorphic unions from lambda

  test("Nominal types - Unions - Polymorphic unions from lambda - lambda creating PersonOrSomething other variant") {

    assert((

      (∀t0.(t0 → (PersonOrSomething @ t0)))) == (

      (∀t0.(t0 → (PersonOrSomething @ t0)))))

  }

  test("Nominal types - Unions - Polymorphic unions from lambda - lambda creating UnionPolymorphicRecursive value variant") {

    assert((

      (∀t0.(t0 → (UnionPolymorphicRecursive @ t0)))) == (

      (∀t0.(t0 → (UnionPolymorphicRecursive @ t0)))))

  }

  // Unions in complex contexts

  test("Nominal types - Unions - Unions in complex contexts - union in tuple") {

    assert((

      (Number, string)) == (

      (Number, string)))

  }

  test("Nominal types - Unions - Unions in complex contexts - union in list") {

    assert((

      list<Number>) == (

      list<Number>))

  }

  test("Nominal types - Unions - Unions in complex contexts - polymorphic union in let binding") {

    assert((

      (PersonOrSomething @ string)) == (

      (PersonOrSomething @ string)))

  }

  // Multi-parameter polymorphic injections

  test("Nominal types - Unions - Multi-parameter polymorphic injections - either left with int") {

    assert((

      (∀t0.(Either @ int32 @ t0))) == (

      (∀t0.(Either @ int32 @ t0))))

  }

  test("Nominal types - Unions - Multi-parameter polymorphic injections - either right with string") {

    assert((

      (∀t0.(Either @ t0 @ string))) == (

      (∀t0.(Either @ t0 @ string))))

  }

  test("Nominal types - Unions - Multi-parameter polymorphic injections - either containing LatLonPoly in list") {

    assert((

      (∀t0.(Either @ t0 @ list<(LatLonPoly @ int32)>))) == (

      (∀t0.(Either @ t0 @ list<(LatLonPoly @ int32)>))))

  }

  test("Nominal types - Unions - Multi-parameter polymorphic injections - either in triple in map with shared type variables") {

    assert((

      (∀t0.(∀t1.(∀t2.(∀t3.(∀t4.(∀t5.(t0 → t1 → t2 → map<string, (Triple @ (Either @ t0 @ t3) @ (Either @ t0 @ t4) @ (Either @ t5 @ t1))>)))))))) == (

      (∀t0.(∀t1.(∀t2.(∀t3.(∀t4.(∀t5.(t0 → t1 → t2 → map<string, (Triple @ (Either @ t0 @ t3) @ (Either @ t0 @ t4) @ (Either @ t5 @ t1))>)))))))))

  }

  // Wrapped terms

  // Monomorphic wrapped terms

  test("Nominal types - Wrapped terms - Monomorphic wrapped terms - string alias") {

    assert((

      StringAlias) == (

      StringAlias))

  }

  test("Nominal types - Wrapped terms - Monomorphic wrapped terms - wrapped integer") {

    assert((

      StringAlias) == (

      StringAlias))

  }

  test("Nominal types - Wrapped terms - Monomorphic wrapped terms - wrapped in tuple") {

    assert((

      (StringAlias, string)) == (

      (StringAlias, string)))

  }

  // Polymorphic wrapped terms

  test("Nominal types - Wrapped terms - Polymorphic wrapped terms - polymorphic wrapper with int") {

    assert((

      (PolymorphicWrapper @ int32)) == (

      (PolymorphicWrapper @ int32)))

  }

  test("Nominal types - Wrapped terms - Polymorphic wrapped terms - polymorphic wrapper with string") {

    assert((

      (PolymorphicWrapper @ string)) == (

      (PolymorphicWrapper @ string)))

  }

  test("Nominal types - Wrapped terms - Polymorphic wrapped terms - polymorphic wrapper from lambda") {

    assert((

      (∀t0.(t0 → (PolymorphicWrapper @ t0)))) == (

      (∀t0.(t0 → (PolymorphicWrapper @ t0)))))

  }

  // Wrapped terms in complex contexts

  test("Nominal types - Wrapped terms - Wrapped terms in complex contexts - wrapped in record") {

    assert((

      Person) == (

      Person))

  }

  test("Nominal types - Wrapped terms - Wrapped terms in complex contexts - wrapped in let binding") {

    assert((

      StringAlias) == (

      StringAlias))

  }

  test("Nominal types - Wrapped terms - Wrapped terms in complex contexts - wrapped in list") {

    assert((

      list<StringAlias>) == (

      list<StringAlias>))

  }

  // Nested wrapped terms

  test("Nominal types - Wrapped terms - Nested wrapped terms - wrapped tuple") {

    assert((

      (PolymorphicWrapper @ (int32, string))) == (

      (PolymorphicWrapper @ (int32, string))))

  }

  test("Nominal types - Wrapped terms - Nested wrapped terms - wrapped optional") {

    assert((

      (PolymorphicWrapper @ maybe<int32>)) == (

      (PolymorphicWrapper @ maybe<int32>)))

  }

  test("Nominal types - Wrapped terms - Nested wrapped terms - wrapped map") {

    assert((

      (PolymorphicWrapper @ map<string, int32>)) == (

      (PolymorphicWrapper @ map<string, int32>)))

  }

  // Multiple wrapping levels

  test("Nominal types - Wrapped terms - Multiple wrapping levels - wrapped in optional") {

    assert((

      maybe<StringAlias>) == (

      maybe<StringAlias>))

  }

  test("Nominal types - Wrapped terms - Multiple wrapping levels - list of wrapped polymorphic") {

    assert((

      list<(PolymorphicWrapper @ int32)>) == (

      list<(PolymorphicWrapper @ int32)>))

  }

  // Multi-parameter polymorphic wrappers

  test("Nominal types - Wrapped terms - Multi-parameter polymorphic wrappers - symmetric triple wrapping simple types") {

    assert((

      (SymmetricTriple @ int32 @ string)) == (

      (SymmetricTriple @ int32 @ string)))

  }

  test("Nominal types - Wrapped terms - Multi-parameter polymorphic wrappers - symmetric triple from lambda") {

    assert((

      (∀t0.(∀t1.(t0 → t1 → t0 → (SymmetricTriple @ t0 @ t1))))) == (

      (∀t0.(∀t1.(t0 → t1 → t0 → (SymmetricTriple @ t0 @ t1))))))

  }

  test("Nominal types - Wrapped terms - Multi-parameter polymorphic wrappers - symmetric triple with nested polymorphic types and foldl") {

    assert((

      (list<int32> → list<int32> → (SymmetricTriple @ int32 @ list<list<int32>>))) == (

      (list<int32> → list<int32> → (SymmetricTriple @ int32 @ list<list<int32>>))))

  }

  // Eliminations

  // Record eliminations

  // Simple record projections

  test("Nominal types - Eliminations - Record eliminations - Simple record projections - project firstName from Person") {

    assert((

      (Person → string)) == (

      (Person → string)))

  }

  test("Nominal types - Eliminations - Record eliminations - Simple record projections - project lastName from Person") {

    assert((

      (Person → string)) == (

      (Person → string)))

  }

  test("Nominal types - Eliminations - Record eliminations - Simple record projections - project age from Person") {

    assert((

      (Person → int32)) == (

      (Person → int32)))

  }

  test("Nominal types - Eliminations - Record eliminations - Simple record projections - project lat from LatLon") {

    assert((

      (LatLon → float32)) == (

      (LatLon → float32)))

  }

  test("Nominal types - Eliminations - Record eliminations - Simple record projections - project lon from LatLon") {

    assert((

      (LatLon → float32)) == (

      (LatLon → float32)))

  }

  // Record projections applied to records

  test("Nominal types - Eliminations - Record eliminations - Record projections applied to records - project firstName applied to person record") {

    assert((

      string) == (

      string))

  }

  test("Nominal types - Eliminations - Record eliminations - Record projections applied to records - project age applied to person record") {

    assert((

      int32) == (

      int32))

  }

  test("Nominal types - Eliminations - Record eliminations - Record projections applied to records - project lat applied to LatLon record") {

    assert((

      float32) == (

      float32))

  }

  // Polymorphic record projections

  test("Nominal types - Eliminations - Record eliminations - Polymorphic record projections - project lat from polymorphic LatLonPoly") {

    assert((

      (∀t0.((LatLonPoly @ t0) → t0))) == (

      (∀t0.((LatLonPoly @ t0) → t0))))

  }

  test("Nominal types - Eliminations - Record eliminations - Polymorphic record projections - project lon from polymorphic LatLonPoly") {

    assert((

      (∀t0.((LatLonPoly @ t0) → t0))) == (

      (∀t0.((LatLonPoly @ t0) → t0))))

  }

  test("Nominal types - Eliminations - Record eliminations - Polymorphic record projections - project head from BuddyListA") {

    assert((

      (∀t0.((BuddyListA @ t0) → t0))) == (

      (∀t0.((BuddyListA @ t0) → t0))))

  }

  test("Nominal types - Eliminations - Record eliminations - Polymorphic record projections - project tail from BuddyListA") {

    assert((

      (∀t0.((BuddyListA @ t0) → maybe<(BuddyListB @ t0)>))) == (

      (∀t0.((BuddyListA @ t0) → maybe<(BuddyListB @ t0)>))))

  }

  // Polymorphic record projections applied

  test("Nominal types - Eliminations - Record eliminations - Polymorphic record projections applied - project lat from LatLonPoly with int32") {

    assert((

      int32) == (

      int32))

  }

  test("Nominal types - Eliminations - Record eliminations - Polymorphic record projections applied - project lon from LatLonPoly with float64") {

    assert((

      float64) == (

      float64))

  }

  test("Nominal types - Eliminations - Record eliminations - Polymorphic record projections applied - project head from BuddyListA with string") {

    assert((

      string) == (

      string))

  }

  // Record projections with variables

  test("Nominal types - Eliminations - Record eliminations - Record projections with variables - project from lambda parameter") {

    assert((

      (Person → string)) == (

      (Person → string)))

  }

  test("Nominal types - Eliminations - Record eliminations - Record projections with variables - project from polymorphic lambda parameter") {

    assert((

      (∀t0.((LatLonPoly @ t0) → t0))) == (

      (∀t0.((LatLonPoly @ t0) → t0))))

  }

  test("Nominal types - Eliminations - Record eliminations - Record projections with variables - multiple projections from same record") {

    assert((

      (Person → (string, string))) == (

      (Person → (string, string))))

  }

  // Record projections in complex contexts

  test("Nominal types - Eliminations - Record eliminations - Record projections in complex contexts - projection in let binding") {

    assert((

      string) == (

      string))

  }

  test("Nominal types - Eliminations - Record eliminations - Record projections in complex contexts - projection in tuple") {

    assert((

      ((Person → string), (Person → int32))) == (

      ((Person → string), (Person → int32))))

  }

  test("Nominal types - Eliminations - Record eliminations - Record projections in complex contexts - projection in list") {

    assert((

      list<(Person → string)>) == (

      list<(Person → string)>))

  }

  // Multi-parameter polymorphic projections

  test("Nominal types - Eliminations - Record eliminations - Multi-parameter polymorphic projections - project first from Triple") {

    assert((

      (∀t0.(∀t1.(∀t2.((Triple @ t0 @ t1 @ t2) → t0))))) == (

      (∀t0.(∀t1.(∀t2.((Triple @ t0 @ t1 @ t2) → t0))))))

  }

  test("Nominal types - Eliminations - Record eliminations - Multi-parameter polymorphic projections - project second from Triple applied") {

    assert((

      string) == (

      string))

  }

  test("Nominal types - Eliminations - Record eliminations - Multi-parameter polymorphic projections - project from Triple and use second field, which is another polymorphic record") {

    assert((

      (∀t0.(∀t1.(∀t2.(∀t3.((Triple @ t0 @ (PersonOrSomething @ map<t1, t2>) @ t3) → t1 → maybe<t2>)))))) == (

      (∀t0.(∀t1.(∀t2.(∀t3.((Triple @ t0 @ (PersonOrSomething @ map<t1, t2>) @ t3) → t1 → maybe<t2>)))))))

  }

  // Higher-order record projections

  test("Nominal types - Eliminations - Record eliminations - Higher-order record projections - map projection over list of records") {

    assert((

      list<string>) == (

      list<string>))

  }

  test("Nominal types - Eliminations - Record eliminations - Higher-order record projections - map polymorphic projection") {

    assert((

      list<int32>) == (

      list<int32>))

  }

  test("Nominal types - Eliminations - Record eliminations - Higher-order record projections - filter using projection") {

    assert((

      list<Person>) == (

      list<Person>))

  }

  // Recursive record projections

  test("Nominal types - Eliminations - Record eliminations - Recursive record projections - nested projection from recursive record") {

    assert((

      (IntList → int32)) == (

      (IntList → int32)))

  }

  // Record projections with mutual recursion

  test("Nominal types - Eliminations - Record eliminations - Record projections with mutual recursion - project head from BuddyListA") {

    assert((

      (∀t0.((BuddyListA @ t0) → t0))) == (

      (∀t0.((BuddyListA @ t0) → t0))))

  }

  test("Nominal types - Eliminations - Record eliminations - Record projections with mutual recursion - project tail from BuddyListB") {

    assert((

      (∀t0.((BuddyListB @ t0) → maybe<(BuddyListA @ t0)>))) == (

      (∀t0.((BuddyListB @ t0) → maybe<(BuddyListA @ t0)>))))

  }

  test("Nominal types - Eliminations - Record eliminations - Record projections with mutual recursion - chained projections across mutual recursion") {

    assert((

      (∀t0.((BuddyListA @ t0) → maybe<(BuddyListB @ t0)>))) == (

      (∀t0.((BuddyListA @ t0) → maybe<(BuddyListB @ t0)>))))

  }

  // Projections with variables

  test("Nominal types - Eliminations - Record eliminations - Projections with variables - project from lambda parameter") {

    assert((

      (Person → string)) == (

      (Person → string)))

  }

  test("Nominal types - Eliminations - Record eliminations - Projections with variables - project from polymorphic lambda parameter") {

    assert((

      (∀t0.((LatLonPoly @ t0) → t0))) == (

      (∀t0.((LatLonPoly @ t0) → t0))))

  }

  test("Nominal types - Eliminations - Record eliminations - Projections with variables - multiple projections from same record") {

    assert((

      (Person → (string, string))) == (

      (Person → (string, string))))

  }

  // Union eliminations

  // Simple unit inject eliminations

  test("Nominal types - Eliminations - Union eliminations - Simple unit inject eliminations - match Comparison with all cases") {

    assert((

      (Comparison → string)) == (

      (Comparison → string)))

  }

  test("Nominal types - Eliminations - Union eliminations - Simple unit inject eliminations - match Comparison returning int32") {

    assert((

      (Comparison → int32)) == (

      (Comparison → int32)))

  }

  test("Nominal types - Eliminations - Union eliminations - Simple unit inject eliminations - match applied to Comparison variant") {

    assert((

      string) == (

      string))

  }

  // Union eliminations with data

  test("Nominal types - Eliminations - Union eliminations - Union eliminations with data - match Number extracting int values") {

    assert((

      (Number → int32)) == (

      (Number → int32)))

  }

  test("Nominal types - Eliminations - Union eliminations - Union eliminations with data - match Number converting to string") {

    assert((

      (Number → string)) == (

      (Number → string)))

  }

  test("Nominal types - Eliminations - Union eliminations - Union eliminations with data - match Number applied to int variant") {

    assert((

      int32) == (

      int32))

  }

  test("Nominal types - Eliminations - Union eliminations - Union eliminations with data - match Timestamp with mixed data types") {

    assert((

      (Timestamp → string)) == (

      (Timestamp → string)))

  }

  // Polymorphic union eliminations

  // Simple polymorphic unions

  test("Nominal types - Eliminations - Union eliminations - Polymorphic union eliminations - Simple polymorphic unions - match PersonOrSomething with string") {

    assert((

      ((PersonOrSomething @ string) → string)) == (

      ((PersonOrSomething @ string) → string)))

  }

  test("Nominal types - Eliminations - Union eliminations - Polymorphic union eliminations - Simple polymorphic unions - match PersonOrSomething instantiated with string") {

    assert((

      string) == (

      string))

  }

  // using UnionPolymorphicRecursive

  test("Nominal types - Eliminations - Union eliminations - Polymorphic union eliminations - using UnionPolymorphicRecursive - non-applied UnionPolymorphicRecursive") {

    assert((

      ((UnionPolymorphicRecursive @ int32) → string)) == (

      ((UnionPolymorphicRecursive @ int32) → string)))

  }

  test("Nominal types - Eliminations - Union eliminations - Polymorphic union eliminations - using UnionPolymorphicRecursive - applied UnionPolymorphicRecursive with int32") {

    assert((

      string) == (

      string))

  }

  test("Nominal types - Eliminations - Union eliminations - Polymorphic union eliminations - using UnionPolymorphicRecursive - applied UnionPolymorphicRecursive with int32 in lambda") {

    assert((

      ((UnionPolymorphicRecursive @ int32) → string)) == (

      ((UnionPolymorphicRecursive @ int32) → string)))

  }

  test("Nominal types - Eliminations - Union eliminations - Polymorphic union eliminations - using UnionPolymorphicRecursive - applied generic UnionPolymorphicRecursive in lambda") {

    assert((

      (∀t0.((UnionPolymorphicRecursive @ t0) → string))) == (

      (∀t0.((UnionPolymorphicRecursive @ t0) → string))))

  }

  // Using kernel types

  test("Nominal types - Eliminations - Union eliminations - Polymorphic union eliminations - Using kernel types - case statement on CoderDirection applied to argument") {

    assert((

      (∀t0.(hydra.coders.CoderDirection → (hydra.coders.Coder @ t0 @ t0) → hydra.context.Context → t0 → either<(hydra.context.InContext @ hydra.errors.Error), t0>))) == (

      (∀t0.(hydra.coders.CoderDirection → (hydra.coders.Coder @ t0 @ t0) → hydra.context.Context → t0 → either<(hydra.context.InContext @ hydra.errors.Error), t0>))))

  }

  // Union eliminations with defaults

  test("Nominal types - Eliminations - Union eliminations - Union eliminations with defaults - match Comparison with default case") {

    assert((

      (Comparison → string)) == (

      (Comparison → string)))

  }

  test("Nominal types - Eliminations - Union eliminations - Union eliminations with defaults - match Number with default case") {

    assert((

      (Number → int32)) == (

      (Number → int32)))

  }

  test("Nominal types - Eliminations - Union eliminations - Union eliminations with defaults - match UnionMonomorphic with default") {

    assert((

      (UnionMonomorphic → string)) == (

      (UnionMonomorphic → string)))

  }

  // Nested union eliminations

  test("Nominal types - Eliminations - Union eliminations - Nested union eliminations - nested match statements") {

    assert((

      ((PersonOrSomething @ Number) → string)) == (

      ((PersonOrSomething @ Number) → string)))

  }

  test("Nominal types - Eliminations - Union eliminations - Nested union eliminations - match in tuple") {

    assert((

      ((Comparison → int32), string)) == (

      ((Comparison → int32), string)))

  }

  // Union eliminations in complex contexts

  test("Nominal types - Eliminations - Union eliminations - Union eliminations in complex contexts - match in let binding") {

    assert((

      (Comparison → string)) == (

      (Comparison → string)))

  }

  test("Nominal types - Eliminations - Union eliminations - Union eliminations in complex contexts - match in record") {

    assert((

      Person) == (

      Person))

  }

  test("Nominal types - Eliminations - Union eliminations - Union eliminations in complex contexts - match with polymorphic result in list") {

    assert((

      list<int32>) == (

      list<int32>))

  }

  // Multi-parameter polymorphic case statements

  test("Nominal types - Eliminations - Union eliminations - Multi-parameter polymorphic case statements - case Either converting both to string") {

    assert((

      ((Either @ int32 @ float32) → string)) == (

      ((Either @ int32 @ float32) → string)))

  }

  test("Nominal types - Eliminations - Union eliminations - Multi-parameter polymorphic case statements - case Either applied to injection") {

    assert((

      int32) == (

      int32))

  }

  test("Nominal types - Eliminations - Union eliminations - Multi-parameter polymorphic case statements - case Either with Triple and nested projections") {

    assert((

      (∀t0.(∀t1.(∀t2.(∀t3.(∀t4.((Triple @ t0 @ (Either @ (LatLonPoly @ t1) @ (Triple @ t1 @ t2 @ t3)) @ t4) → t1))))))) == (

      (∀t0.(∀t1.(∀t2.(∀t3.(∀t4.((Triple @ t0 @ (Either @ (LatLonPoly @ t1) @ (Triple @ t1 @ t2 @ t3)) @ t4) → t1))))))))

  }

  test("Nominal types - Eliminations - Union eliminations - Multi-parameter polymorphic case statements - case Either with polymorphic let bindings") {

    assert((

      ((Either @ int32 @ string) → (Either @ int32 @ int32))) == (

      ((Either @ int32 @ string) → (Either @ int32 @ int32))))

  }

  // Higher-order union eliminations

  test("Nominal types - Eliminations - Union eliminations - Higher-order union eliminations - map match over list") {

    assert((

      list<string>) == (

      list<string>))

  }

  test("Nominal types - Eliminations - Union eliminations - Higher-order union eliminations - compose match with other functions") {

    assert((

      (Comparison → int32)) == (

      (Comparison → int32)))

  }

  test("Nominal types - Eliminations - Union eliminations - Higher-order union eliminations - match in lambda body") {

    assert((

      (Number → int32)) == (

      (Number → int32)))

  }

  // Recursive union eliminations

  test("Nominal types - Eliminations - Union eliminations - Recursive union eliminations - match HydraType recursively") {

    assert((

      (HydraType → string)) == (

      (HydraType → string)))

  }

  // Wrap eliminations

  // Monomorphic unwrapping

  test("Nominal types - Eliminations - Wrap eliminations - Monomorphic unwrapping - unwrap string alias") {

    assert((

      (StringAlias → string)) == (

      (StringAlias → string)))

  }

  // Polymorphic unwrapping

  test("Nominal types - Eliminations - Wrap eliminations - Polymorphic unwrapping - unwrap polymorphic wrapper") {

    assert((

      (∀t0.((PolymorphicWrapper @ t0) → list<t0>))) == (

      (∀t0.((PolymorphicWrapper @ t0) → list<t0>))))

  }

  // Unwrap eliminations in applications

  test("Nominal types - Eliminations - Wrap eliminations - Unwrap eliminations in applications - unwrap applied to wrapped term") {

    assert((

      string) == (

      string))

  }

  test("Nominal types - Eliminations - Wrap eliminations - Unwrap eliminations in applications - unwrap polymorphic applied") {

    assert((

      list<int32>) == (

      list<int32>))

  }

  // Unwrap in complex contexts

  test("Nominal types - Eliminations - Wrap eliminations - Unwrap in complex contexts - unwrap in let binding") {

    assert((

      string) == (

      string))

  }

  test("Nominal types - Eliminations - Wrap eliminations - Unwrap in complex contexts - unwrap in tuple") {

    assert((

      ((StringAlias → string), string)) == (

      ((StringAlias → string), string)))

  }

  test("Nominal types - Eliminations - Wrap eliminations - Unwrap in complex contexts - unwrap in lambda") {

    assert((

      (StringAlias → string)) == (

      (StringAlias → string)))

  }

  // Multi-parameter polymorphic unwrappers

  test("Nominal types - Eliminations - Wrap eliminations - Multi-parameter polymorphic unwrappers - unwrap symmetric triple to tuple") {

    assert((

      (∀t0.(∀t1.((SymmetricTriple @ t0 @ t1) → (t0, t0))))) == (

      (∀t0.(∀t1.((SymmetricTriple @ t0 @ t1) → (t0, t0))))))

  }

  test("Nominal types - Eliminations - Wrap eliminations - Multi-parameter polymorphic unwrappers - unwrap and collect edges in set") {

    assert((

      (∀t0.(∀t1.(set<(SymmetricTriple @ t0 @ t1)> → set<t1>)))) == (

      (∀t0.(∀t1.(set<(SymmetricTriple @ t0 @ t1)> → set<t1>)))))

  }

  test("Nominal types - Eliminations - Wrap eliminations - Multi-parameter polymorphic unwrappers - unwrap with maybe to handle optional symmetric triple") {

    assert((

      (∀t0.(∀t1.(maybe<(SymmetricTriple @ t0 @ t1)> → maybe<t1>)))) == (

      (∀t0.(∀t1.(maybe<(SymmetricTriple @ t0 @ t1)> → maybe<t1>)))))

  }

  // Chained unwrapping

  test("Nominal types - Eliminations - Wrap eliminations - Chained unwrapping - unwrap then process") {

    assert((

      (StringAlias → string)) == (

      (StringAlias → string)))

  }

  test("Nominal types - Eliminations - Wrap eliminations - Chained unwrapping - unwrap polymorphic then map") {

    assert((

      ((PolymorphicWrapper @ int32) → list<int32>)) == (

      ((PolymorphicWrapper @ int32) → list<int32>)))

  }

  // Multiple unwrap operations

  test("Nominal types - Eliminations - Wrap eliminations - Multiple unwrap operations - unwrap different types") {

    assert((

      (∀t0.(StringAlias → (PolymorphicWrapper @ t0) → (string, list<t0>)))) == (

      (∀t0.(StringAlias → (PolymorphicWrapper @ t0) → (string, list<t0>)))))

  }
}
