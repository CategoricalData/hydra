-- | Test cases for type unification operations
module Hydra.Sources.Test.Unification where

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


ns :: Namespace
ns = Namespace "hydra.test.unification"

module_ :: Module
module_ = Module ns elements
    []
    kernelTypesNamespaces
    (Just "Test cases for type unification operations")
  where
    elements = [Phantoms.toBinding allTests]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

-- Helper to build names
nm :: String -> TTerm Name
nm s = Core.name $ Phantoms.string s

-- ============================================================
-- variableOccursInType tests
-- ============================================================

variableOccursInTypeTests :: TTerm TestGroup
variableOccursInTypeTests = subgroup "variableOccursInType" [
  -- Variable occurs in itself
  variableOccursCase "variable occurs in itself"
    (nm "a")
    (T.var "a")
    Phantoms.true,

  -- Variable does not occur in different variable
  variableOccursCase "variable does not occur in different variable"
    (nm "a")
    (T.var "b")
    Phantoms.false,

  -- Variable does not occur in primitive type
  variableOccursCase "variable does not occur in int32"
    (nm "a")
    T.int32
    Phantoms.false,

  variableOccursCase "variable does not occur in string"
    (nm "a")
    T.string
    Phantoms.false,

  -- Variable occurs in list type
  variableOccursCase "variable occurs in list element type"
    (nm "a")
    (T.list (T.var "a"))
    Phantoms.true,

  variableOccursCase "variable does not occur in list of different type"
    (nm "a")
    (T.list (T.var "b"))
    Phantoms.false,

  -- Variable occurs in function type
  variableOccursCase "variable occurs in function domain"
    (nm "a")
    (T.function (T.var "a") T.int32)
    Phantoms.true,

  variableOccursCase "variable occurs in function codomain"
    (nm "a")
    (T.function T.int32 (T.var "a"))
    Phantoms.true,

  variableOccursCase "variable does not occur in function with different vars"
    (nm "a")
    (T.function (T.var "b") (T.var "c"))
    Phantoms.false,

  -- Variable occurs in optional type
  variableOccursCase "variable occurs in optional type"
    (nm "a")
    (T.optional (T.var "a"))
    Phantoms.true,

  -- Variable occurs in pair type
  variableOccursCase "variable occurs in pair first"
    (nm "a")
    (T.pair (T.var "a") T.int32)
    Phantoms.true,

  variableOccursCase "variable occurs in pair second"
    (nm "a")
    (T.pair T.int32 (T.var "a"))
    Phantoms.true,

  -- Variable occurs in either type
  variableOccursCase "variable occurs in either left"
    (nm "a")
    (T.either_ (T.var "a") T.int32)
    Phantoms.true,

  variableOccursCase "variable occurs in either right"
    (nm "a")
    (T.either_ T.int32 (T.var "a"))
    Phantoms.true,

  -- Variable occurs in map type
  variableOccursCase "variable occurs in map key type"
    (nm "a")
    (T.map (T.var "a") T.int32)
    Phantoms.true,

  variableOccursCase "variable occurs in map value type"
    (nm "a")
    (T.map T.string (T.var "a"))
    Phantoms.true,

  -- Variable occurs in set type
  variableOccursCase "variable occurs in set type"
    (nm "a")
    (T.set (T.var "a"))
    Phantoms.true,

  -- Nested structures
  variableOccursCase "variable occurs in nested list"
    (nm "a")
    (T.list (T.list (T.var "a")))
    Phantoms.true,

  variableOccursCase "variable occurs in list of functions"
    (nm "a")
    (T.list (T.function T.int32 (T.var "a")))
    Phantoms.true,

  variableOccursCase "variable does not occur in complex type without it"
    (nm "a")
    (T.function (T.list T.int32) (T.optional (T.pair T.string (T.var "b"))))
    Phantoms.false,

  variableOccursCase "variable occurs deep in complex type"
    (nm "a")
    (T.function (T.list T.int32) (T.optional (T.pair T.string (T.var "a"))))
    Phantoms.true,

  -- Forall types (note: no distinction between free and bound)
  variableOccursCase "variable occurs in forAll body"
    (nm "a")
    (T.forAll "b" (T.function (T.var "b") (T.var "a")))
    Phantoms.true,

  variableOccursCase "variable occurs in forAll bound position"
    (nm "a")
    (T.forAll "a" (T.function (T.var "a") (T.var "a")))
    Phantoms.true]

-- ============================================================
-- unifyTypes tests
-- ============================================================

-- Helper: empty schema types list
noSchema :: TTerm [Name]
noSchema = Phantoms.list ([] :: [TTerm Name])

-- Helper: empty substitution (success with no bindings)
emptySubst :: [(TTerm Name, TTerm Type)]
emptySubst = []

unifyTypesTests :: TTerm TestGroup
unifyTypesTests = subgroup "unifyTypes" [
  -- Identical types unify with empty substitution
  unifyTypesCase "unify identical int32 types"
    noSchema
    T.int32
    T.int32
    emptySubst,

  unifyTypesCase "unify identical string types"
    noSchema
    T.string
    T.string
    emptySubst,

  unifyTypesCase "unify identical variable types"
    noSchema
    (T.var "a")
    (T.var "a")
    emptySubst,

  -- Variable unifies with concrete type
  unifyTypesCase "unify variable with int32"
    noSchema
    (T.var "a")
    T.int32
    [(nm "a", T.int32)],

  unifyTypesCase "unify int32 with variable"
    noSchema
    T.int32
    (T.var "a")
    [(nm "a", T.int32)],

  -- Variable unifies with variable (binding one to the other)
  unifyTypesCase "unify two different variables"
    noSchema
    (T.var "a")
    (T.var "b")
    [(nm "a", T.var "b")],

  -- List types unify element types
  unifyTypesCase "unify list of variables with list of int32"
    noSchema
    (T.list (T.var "a"))
    (T.list T.int32)
    [(nm "a", T.int32)],

  unifyTypesCase "unify identical list types"
    noSchema
    (T.list T.string)
    (T.list T.string)
    emptySubst,

  -- Function types unify domains and codomains
  unifyTypesCase "unify function types with variables"
    noSchema
    (T.function (T.var "a") (T.var "b"))
    (T.function T.int32 T.string)
    [(nm "a", T.int32), (nm "b", T.string)],

  unifyTypesCase "unify identical function types"
    noSchema
    (T.function T.int32 T.string)
    (T.function T.int32 T.string)
    emptySubst,

  -- Optional types
  unifyTypesCase "unify optional types"
    noSchema
    (T.optional (T.var "a"))
    (T.optional T.int32)
    [(nm "a", T.int32)],

  -- Pair types
  unifyTypesCase "unify pair types"
    noSchema
    (T.pair (T.var "a") (T.var "b"))
    (T.pair T.int32 T.string)
    [(nm "a", T.int32), (nm "b", T.string)],

  -- Either types
  unifyTypesCase "unify either types"
    noSchema
    (T.either_ (T.var "a") (T.var "b"))
    (T.either_ T.int32 T.string)
    [(nm "a", T.int32), (nm "b", T.string)],

  -- Map types
  unifyTypesCase "unify map types"
    noSchema
    (T.map (T.var "k") (T.var "v"))
    (T.map T.string T.int32)
    [(nm "k", T.string), (nm "v", T.int32)],

  -- Set types
  unifyTypesCase "unify set types"
    noSchema
    (T.set (T.var "a"))
    (T.set T.int32)
    [(nm "a", T.int32)],

  -- Unit types
  unifyTypesCase "unify unit types"
    noSchema
    T.unit
    T.unit
    emptySubst,

  -- Failure cases
  unifyTypesFailCase "fail to unify int32 with string"
    noSchema
    T.int32
    T.string
    "cannot unify",

  unifyTypesFailCase "fail to unify list with function"
    noSchema
    (T.list T.int32)
    (T.function T.int32 T.int32)
    "cannot unify",

  -- Occur check failure
  unifyTypesFailCase "occur check: variable with list containing it"
    noSchema
    (T.var "a")
    (T.list (T.var "a"))
    "appears free"]

-- ============================================================
-- joinTypes tests
-- ============================================================

-- Helper to create type constraints
constraint :: TTerm Type -> TTerm Type -> TTerm TypeConstraint
constraint left right = Phantoms.record _TypeConstraint [
  Phantoms.field _TypeConstraint_left left,
  Phantoms.field _TypeConstraint_right right,
  Phantoms.field _TypeConstraint_comment (Phantoms.string "join types; test")]

-- Helper for empty constraint list
noConstraints :: TTerm [TypeConstraint]
noConstraints = Phantoms.list ([] :: [TTerm TypeConstraint])

joinTypesTests :: TTerm TestGroup
joinTypesTests = subgroup "joinTypes" [
  -- Identical primitive types produce no constraints
  joinTypesCase "join identical int32"
    T.int32
    T.int32
    noConstraints,

  joinTypesCase "join identical string"
    T.string
    T.string
    noConstraints,

  -- Note: joinTypes doesn't handle type variables directly - those are handled by unifyTypeConstraints.
  -- Testing variable joining with joinTypes would fail because it's not the right level of abstraction.

  -- List types produce constraint on element types
  joinTypesCase "join list types"
    (T.list (T.var "a"))
    (T.list T.int32)
    (Phantoms.list [constraint (T.var "a") T.int32]),

  -- Function types produce constraints on domain and codomain
  joinTypesCase "join function types"
    (T.function (T.var "a") (T.var "b"))
    (T.function T.int32 T.string)
    (Phantoms.list [constraint (T.var "a") T.int32, constraint (T.var "b") T.string]),

  -- Optional types
  joinTypesCase "join optional types"
    (T.optional (T.var "a"))
    (T.optional T.int32)
    (Phantoms.list [constraint (T.var "a") T.int32]),

  -- Pair types
  joinTypesCase "join pair types"
    (T.pair (T.var "a") (T.var "b"))
    (T.pair T.int32 T.string)
    (Phantoms.list [constraint (T.var "a") T.int32, constraint (T.var "b") T.string]),

  -- Either types
  joinTypesCase "join either types"
    (T.either_ (T.var "a") (T.var "b"))
    (T.either_ T.int32 T.string)
    (Phantoms.list [constraint (T.var "a") T.int32, constraint (T.var "b") T.string]),

  -- Map types
  joinTypesCase "join map types"
    (T.map (T.var "k") (T.var "v"))
    (T.map T.string T.int32)
    (Phantoms.list [constraint (T.var "k") T.string, constraint (T.var "v") T.int32]),

  -- Set types
  joinTypesCase "join set types"
    (T.set (T.var "a"))
    (T.set T.int32)
    (Phantoms.list [constraint (T.var "a") T.int32]),

  -- Unit types
  joinTypesCase "join unit types"
    T.unit
    T.unit
    noConstraints,

  -- Failure cases
  joinTypesFailCase "fail to join int32 with string"
    T.int32
    T.string,

  joinTypesFailCase "fail to join list with function"
    (T.list T.int32)
    (T.function T.int32 T.int32),

  joinTypesFailCase "fail to join pair with either"
    (T.pair T.int32 T.string)
    (T.either_ T.int32 T.string)]

-- ============================================================
-- All tests
-- ============================================================

allTests :: TBinding TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for type unification operations" $
    supergroup "unification" [
      variableOccursInTypeTests,
      unifyTypesTests,
      joinTypesTests]
