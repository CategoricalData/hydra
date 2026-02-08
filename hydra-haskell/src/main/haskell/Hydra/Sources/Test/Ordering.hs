-- | Test cases for Ord instance comparisons on complex Hydra types
module Hydra.Sources.Test.Ordering where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Testing
import Hydra.Sources.Libraries


ns :: Namespace
ns = Namespace "hydra.test.ordering"

module_ :: Module
module_ = Module ns elements [] [] $
    Just "Test cases for Ord instance comparisons on complex Hydra types"
  where
    elements = [Phantoms.toBinding allTests]

-- Helper for comparison result - exactly matching Equality.hs style
compResult :: String -> TTerm Term
compResult resultField = injectUnit (name "hydra.util.Comparison") resultField

-- Coerce any TTerm to TTerm Term (safe since TTerm is just a newtype wrapper)
toTerm :: TTerm a -> TTerm Term
toTerm (TTerm t) = TTerm t

-- Test comparing two values with _equality_compare (exactly matching Equality.hs style)
compareTest :: String -> TTerm a -> TTerm a -> String -> TTerm TestCaseWithMetadata
compareTest testName x y resultField = primCase testName _equality_compare [toTerm x, toTerm y] (compResult resultField)

-- Test equality with _equality_equal
equalTest :: String -> TTerm a -> TTerm a -> TTerm b -> TTerm TestCaseWithMetadata
equalTest testName x y result = primCase testName _equality_equal [toTerm x, toTerm y] (toTerm result)

-- Test less than with _equality_lt
ltTest :: String -> TTerm a -> TTerm a -> TTerm b -> TTerm TestCaseWithMetadata
ltTest testName x y result = primCase testName _equality_lt [toTerm x, toTerm y] (toTerm result)

-- ============================================================
-- Name comparison tests
-- ============================================================

nameComparisonTests :: TTerm TestGroup
nameComparisonTests = subgroup "Name comparison" [
  -- Compare two names (using nameTerm which creates a Name wrapped term)
  compareTest "name less than (alphabetic)"
    (nameTerm "apple")
    (nameTerm "banana")
    "lessThan",
  compareTest "name equal"
    (nameTerm "hello")
    (nameTerm "hello")
    "equalTo",
  compareTest "name greater than"
    (nameTerm "zebra")
    (nameTerm "apple")
    "greaterThan",
  -- Qualified names
  compareTest "qualified name less than"
    (nameTerm "hydra.core.Term")
    (nameTerm "hydra.core.Type")
    "lessThan",
  compareTest "qualified name equal"
    (nameTerm "hydra.core.Term")
    (nameTerm "hydra.core.Term")
    "equalTo",
  -- Boolean equality
  equalTest "name equality true"
    (nameTerm "foo")
    (nameTerm "foo")
    true,
  equalTest "name equality false"
    (nameTerm "foo")
    (nameTerm "bar")
    false]

-- ============================================================
-- Literal comparison tests
-- ============================================================

literalComparisonTests :: TTerm TestGroup
literalComparisonTests = subgroup "Literal comparison" [
  -- Integer literals
  compareTest "int32 literal less than"
    (Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ Phantoms.int32 10)
    (Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ Phantoms.int32 20)
    "lessThan",
  compareTest "int32 literal equal"
    (Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ Phantoms.int32 42)
    (Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ Phantoms.int32 42)
    "equalTo",
  -- String literals
  compareTest "string literal less than"
    (Core.termLiteral $ Core.literalString $ Phantoms.string "aaa")
    (Core.termLiteral $ Core.literalString $ Phantoms.string "bbb")
    "lessThan",
  -- Boolean literals
  compareTest "boolean false < true"
    (Core.termLiteral $ Core.literalBoolean Phantoms.false)
    (Core.termLiteral $ Core.literalBoolean Phantoms.true)
    "lessThan",
  compareTest "boolean true == true"
    (Core.termLiteral $ Core.literalBoolean Phantoms.true)
    (Core.termLiteral $ Core.literalBoolean Phantoms.true)
    "equalTo"]

-- ============================================================
-- Type comparison tests
-- ============================================================

typeComparisonTests :: TTerm TestGroup
typeComparisonTests = subgroup "Type comparison" [
  -- Primitive types
  compareTest "type int32 vs string"
    T.int32
    T.string
    "lessThan",  -- int32 < string lexicographically in the Type union
  compareTest "type equal (int32)"
    T.int32
    T.int32
    "equalTo",
  -- List types
  compareTest "list type comparison by element"
    (T.list T.int32)
    (T.list T.string)
    "lessThan",  -- list<int32> < list<string>
  compareTest "list type equal"
    (T.list T.int32)
    (T.list T.int32)
    "equalTo",
  -- Equality tests
  equalTest "type equality true"
    T.int32
    T.int32
    true,
  equalTest "type equality false"
    T.int32
    T.string
    false]

-- ============================================================
-- Term comparison tests
-- ============================================================

termComparisonTests :: TTerm TestGroup
termComparisonTests = subgroup "Term comparison" [
  -- Integer term literals
  compareTest "term int32 42 vs 43"
    (Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ Phantoms.int32 42)
    (Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ Phantoms.int32 43)
    "lessThan",
  compareTest "term int32 equal"
    (Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ Phantoms.int32 42)
    (Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ Phantoms.int32 42)
    "equalTo",
  -- String term literals
  compareTest "term string a vs b"
    (Core.termLiteral $ Core.literalString $ Phantoms.string "a")
    (Core.termLiteral $ Core.literalString $ Phantoms.string "b")
    "lessThan",
  -- List terms
  compareTest "term list [1] vs [2]"
    (Core.termList $ Phantoms.list [Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ Phantoms.int32 1])
    (Core.termList $ Phantoms.list [Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ Phantoms.int32 2])
    "lessThan",
  compareTest "term list equal"
    (Core.termList $ Phantoms.list [Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ Phantoms.int32 1])
    (Core.termList $ Phantoms.list [Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ Phantoms.int32 1])
    "equalTo"]

-- ============================================================
-- Custom record type comparison tests (monomorphic)
-- ============================================================

-- Helper to build a Person record term
personTerm :: String -> String -> Int -> TTerm Term
personTerm firstName lastName age = record TestTypes.testTypePersonName [
  "firstName" >: string firstName,
  "lastName" >: string lastName,
  "age" >: int32 (fromIntegral age)]

-- Helper to build a LatLon record term
latLonTerm :: Float -> Float -> TTerm Term
latLonTerm lat lon = record TestTypes.testTypeLatLonName [
  "lat" >: float32 (realToFrac lat),
  "lon" >: float32 (realToFrac lon)]

recordComparisonTests :: TTerm TestGroup
recordComparisonTests = subgroup "Record comparison (monomorphic)" [
  -- Person records - compared field by field
  compareTest "person less than by firstName"
    (personTerm "Alice" "Smith" 30)
    (personTerm "Bob" "Smith" 30)
    "lessThan",
  compareTest "person less than by lastName"
    (personTerm "Alice" "Jones" 30)
    (personTerm "Alice" "Smith" 30)
    "lessThan",
  compareTest "person less than by age"
    (personTerm "Alice" "Smith" 25)
    (personTerm "Alice" "Smith" 30)
    "lessThan",
  compareTest "person equal"
    (personTerm "Alice" "Smith" 30)
    (personTerm "Alice" "Smith" 30)
    "equalTo",
  -- LatLon records
  compareTest "latLon less than by lat"
    (latLonTerm 10.0 20.0)
    (latLonTerm 15.0 20.0)
    "lessThan",
  compareTest "latLon less than by lon"
    (latLonTerm 10.0 20.0)
    (latLonTerm 10.0 25.0)
    "lessThan",
  compareTest "latLon equal"
    (latLonTerm 10.0 20.0)
    (latLonTerm 10.0 20.0)
    "equalTo",
  -- Equality tests
  equalTest "person equality true"
    (personTerm "Alice" "Smith" 30)
    (personTerm "Alice" "Smith" 30)
    true,
  equalTest "person equality false"
    (personTerm "Alice" "Smith" 30)
    (personTerm "Bob" "Smith" 30)
    false]

-- ============================================================
-- Polymorphic type comparison tests
-- ============================================================

-- Helper to build a LatLonPoly record term with type application
latLonPolyInt32Term :: Int -> Int -> TTerm Term
latLonPolyInt32Term lat lon = tyapp
  (record TestTypes.testTypeLatLonPolyName [
    "lat" >: int32 (fromIntegral lat),
    "lon" >: int32 (fromIntegral lon)])
  T.int32

latLonPolyStringTerm :: String -> String -> TTerm Term
latLonPolyStringTerm lat lon = tyapp
  (record TestTypes.testTypeLatLonPolyName [
    "lat" >: string lat,
    "lon" >: string lon])
  T.string

-- Helper to build a PersonOrSomething union term
personOrSomethingPersonTerm :: String -> String -> Int -> TTerm Term
personOrSomethingPersonTerm firstName lastName age = tyapp
  (inject TestTypes.testTypePersonOrSomethingName "person" (personTerm firstName lastName age))
  (T.list $ Core.typeVariable TestTypes.testTypePersonName)

personOrSomethingOtherListTerm :: [TTerm Term] -> TTerm Term
personOrSomethingOtherListTerm persons = tyapp
  (inject TestTypes.testTypePersonOrSomethingName "other" (list persons))
  (T.list $ Core.typeVariable TestTypes.testTypePersonName)

polymorphicComparisonTests :: TTerm TestGroup
polymorphicComparisonTests = subgroup "Polymorphic type comparison" [
  -- LatLonPoly Int32
  compareTest "LatLonPoly Int32 less than by lat"
    (latLonPolyInt32Term 10 20)
    (latLonPolyInt32Term 15 20)
    "lessThan",
  compareTest "LatLonPoly Int32 less than by lon"
    (latLonPolyInt32Term 10 20)
    (latLonPolyInt32Term 10 25)
    "lessThan",
  compareTest "LatLonPoly Int32 equal"
    (latLonPolyInt32Term 10 20)
    (latLonPolyInt32Term 10 20)
    "equalTo",
  -- LatLonPoly String
  compareTest "LatLonPoly String less than"
    (latLonPolyStringTerm "10N" "20W")
    (latLonPolyStringTerm "15N" "20W")
    "lessThan",
  compareTest "LatLonPoly String equal"
    (latLonPolyStringTerm "10N" "20W")
    (latLonPolyStringTerm "10N" "20W")
    "equalTo",
  -- PersonOrSomething (List Person)
  compareTest "PersonOrSomething person vs person"
    (personOrSomethingPersonTerm "Alice" "Smith" 30)
    (personOrSomethingPersonTerm "Bob" "Smith" 30)
    "lessThan",
  compareTest "PersonOrSomething person equal"
    (personOrSomethingPersonTerm "Alice" "Smith" 30)
    (personOrSomethingPersonTerm "Alice" "Smith" 30)
    "equalTo",
  -- Equality tests
  equalTest "LatLonPoly Int32 equality true"
    (latLonPolyInt32Term 10 20)
    (latLonPolyInt32Term 10 20)
    true,
  equalTest "LatLonPoly Int32 equality false"
    (latLonPolyInt32Term 10 20)
    (latLonPolyInt32Term 10 25)
    false]

-- ============================================================
-- Union type comparison tests
-- ============================================================

-- Helper for Number union (int vs float)
numberIntTerm :: Int -> TTerm Term
numberIntTerm n = inject TestTypes.testTypeNumberName "int" (int32 (fromIntegral n))

numberFloatTerm :: Float -> TTerm Term
numberFloatTerm f = inject TestTypes.testTypeNumberName "float" (float32 (realToFrac f))

unionComparisonTests :: TTerm TestGroup
unionComparisonTests = subgroup "Union comparison" [
  -- Same variant, different values
  compareTest "Number int variant less than"
    (numberIntTerm 10)
    (numberIntTerm 20)
    "lessThan",
  compareTest "Number int variant equal"
    (numberIntTerm 42)
    (numberIntTerm 42)
    "equalTo",
  compareTest "Number float variant less than"
    (numberFloatTerm 1.5)
    (numberFloatTerm 2.5)
    "lessThan",
  -- Different variants (compared by variant name)
  compareTest "Number float vs int (variant name comparison)"
    (numberFloatTerm 100.0)
    (numberIntTerm 1)
    "lessThan",  -- "float" < "int" alphabetically
  -- Equality
  equalTest "Number int equality true"
    (numberIntTerm 42)
    (numberIntTerm 42)
    true,
  equalTest "Number int equality false (different value)"
    (numberIntTerm 42)
    (numberIntTerm 43)
    false,
  equalTest "Number equality false (different variant)"
    (numberIntTerm 42)
    (numberFloatTerm 42.0)
    false]

-- ============================================================
-- All tests
-- ============================================================

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for Ord instance comparisons on complex Hydra types" $
    supergroup "ordering" [
      nameComparisonTests,
      literalComparisonTests,
      -- Note: typeComparisonTests and termComparisonTests are excluded because
      -- comparing Type and Term values (meta-level structures) causes issues
      -- with the kernel test generator's schema type inference
      recordComparisonTests,
      polymorphicComparisonTests,
      unionComparisonTests]
