-- | Test cases for type unification operations
module Hydra.Sources.Test.Unification where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Meta.Types as Ty
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes


ns :: Namespace
ns = Namespace "hydra.test.unification"

module_ :: Module
module_ = Module ns elements
    []
    KernelTypes.kernelTypesNamespaces
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
    (Ty.var "a")
    Phantoms.true,

  -- Variable does not occur in different variable
  variableOccursCase "variable does not occur in different variable"
    (nm "a")
    (Ty.var "b")
    Phantoms.false,

  -- Variable does not occur in primitive type
  variableOccursCase "variable does not occur in int32"
    (nm "a")
    Ty.int32
    Phantoms.false,

  variableOccursCase "variable does not occur in string"
    (nm "a")
    Ty.string
    Phantoms.false,

  -- Variable occurs in list type
  variableOccursCase "variable occurs in list element type"
    (nm "a")
    (Ty.list (Ty.var "a"))
    Phantoms.true,

  variableOccursCase "variable does not occur in list of different type"
    (nm "a")
    (Ty.list (Ty.var "b"))
    Phantoms.false,

  -- Variable occurs in function type
  variableOccursCase "variable occurs in function domain"
    (nm "a")
    (Ty.function (Ty.var "a") Ty.int32)
    Phantoms.true,

  variableOccursCase "variable occurs in function codomain"
    (nm "a")
    (Ty.function Ty.int32 (Ty.var "a"))
    Phantoms.true,

  variableOccursCase "variable does not occur in function with different vars"
    (nm "a")
    (Ty.function (Ty.var "b") (Ty.var "c"))
    Phantoms.false,

  -- Variable occurs in optional type
  variableOccursCase "variable occurs in optional type"
    (nm "a")
    (Ty.optional (Ty.var "a"))
    Phantoms.true,

  -- Variable occurs in pair type
  variableOccursCase "variable occurs in pair first"
    (nm "a")
    (Ty.pair (Ty.var "a") Ty.int32)
    Phantoms.true,

  variableOccursCase "variable occurs in pair second"
    (nm "a")
    (Ty.pair Ty.int32 (Ty.var "a"))
    Phantoms.true,

  -- Variable occurs in either type
  variableOccursCase "variable occurs in either left"
    (nm "a")
    (Ty.either_ (Ty.var "a") Ty.int32)
    Phantoms.true,

  variableOccursCase "variable occurs in either right"
    (nm "a")
    (Ty.either_ Ty.int32 (Ty.var "a"))
    Phantoms.true,

  -- Variable occurs in map type
  variableOccursCase "variable occurs in map key type"
    (nm "a")
    (Ty.map (Ty.var "a") Ty.int32)
    Phantoms.true,

  variableOccursCase "variable occurs in map value type"
    (nm "a")
    (Ty.map Ty.string (Ty.var "a"))
    Phantoms.true,

  -- Variable occurs in set type
  variableOccursCase "variable occurs in set type"
    (nm "a")
    (Ty.set (Ty.var "a"))
    Phantoms.true,

  -- Nested structures
  variableOccursCase "variable occurs in nested list"
    (nm "a")
    (Ty.list (Ty.list (Ty.var "a")))
    Phantoms.true,

  variableOccursCase "variable occurs in list of functions"
    (nm "a")
    (Ty.list (Ty.function Ty.int32 (Ty.var "a")))
    Phantoms.true,

  variableOccursCase "variable does not occur in complex type without it"
    (nm "a")
    (Ty.function (Ty.list Ty.int32) (Ty.optional (Ty.pair Ty.string (Ty.var "b"))))
    Phantoms.false,

  variableOccursCase "variable occurs deep in complex type"
    (nm "a")
    (Ty.function (Ty.list Ty.int32) (Ty.optional (Ty.pair Ty.string (Ty.var "a"))))
    Phantoms.true,

  -- Forall types (note: no distinction between free and bound)
  variableOccursCase "variable occurs in forAll body"
    (nm "a")
    (Ty.forAll "b" (Ty.function (Ty.var "b") (Ty.var "a")))
    Phantoms.true,

  variableOccursCase "variable occurs in forAll bound position"
    (nm "a")
    (Ty.forAll "a" (Ty.function (Ty.var "a") (Ty.var "a")))
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
    Ty.int32
    Ty.int32
    emptySubst,

  unifyTypesCase "unify identical string types"
    noSchema
    Ty.string
    Ty.string
    emptySubst,

  unifyTypesCase "unify identical variable types"
    noSchema
    (Ty.var "a")
    (Ty.var "a")
    emptySubst,

  -- Variable unifies with concrete type
  unifyTypesCase "unify variable with int32"
    noSchema
    (Ty.var "a")
    Ty.int32
    [(nm "a", Ty.int32)],

  unifyTypesCase "unify int32 with variable"
    noSchema
    Ty.int32
    (Ty.var "a")
    [(nm "a", Ty.int32)],

  -- Variable unifies with variable (binding one to the other)
  unifyTypesCase "unify two different variables"
    noSchema
    (Ty.var "a")
    (Ty.var "b")
    [(nm "a", Ty.var "b")],

  -- List types unify element types
  unifyTypesCase "unify list of variables with list of int32"
    noSchema
    (Ty.list (Ty.var "a"))
    (Ty.list Ty.int32)
    [(nm "a", Ty.int32)],

  unifyTypesCase "unify identical list types"
    noSchema
    (Ty.list Ty.string)
    (Ty.list Ty.string)
    emptySubst,

  -- Function types unify domains and codomains
  unifyTypesCase "unify function types with variables"
    noSchema
    (Ty.function (Ty.var "a") (Ty.var "b"))
    (Ty.function Ty.int32 Ty.string)
    [(nm "a", Ty.int32), (nm "b", Ty.string)],

  unifyTypesCase "unify identical function types"
    noSchema
    (Ty.function Ty.int32 Ty.string)
    (Ty.function Ty.int32 Ty.string)
    emptySubst,

  -- Optional types
  unifyTypesCase "unify optional types"
    noSchema
    (Ty.optional (Ty.var "a"))
    (Ty.optional Ty.int32)
    [(nm "a", Ty.int32)],

  -- Pair types
  unifyTypesCase "unify pair types"
    noSchema
    (Ty.pair (Ty.var "a") (Ty.var "b"))
    (Ty.pair Ty.int32 Ty.string)
    [(nm "a", Ty.int32), (nm "b", Ty.string)],

  -- Either types
  unifyTypesCase "unify either types"
    noSchema
    (Ty.either_ (Ty.var "a") (Ty.var "b"))
    (Ty.either_ Ty.int32 Ty.string)
    [(nm "a", Ty.int32), (nm "b", Ty.string)],

  -- Map types
  unifyTypesCase "unify map types"
    noSchema
    (Ty.map (Ty.var "k") (Ty.var "v"))
    (Ty.map Ty.string Ty.int32)
    [(nm "k", Ty.string), (nm "v", Ty.int32)],

  -- Set types
  unifyTypesCase "unify set types"
    noSchema
    (Ty.set (Ty.var "a"))
    (Ty.set Ty.int32)
    [(nm "a", Ty.int32)],

  -- Unit types
  unifyTypesCase "unify unit types"
    noSchema
    Ty.unit
    Ty.unit
    emptySubst,

  -- Failure cases
  unifyTypesFailCase "fail to unify int32 with string"
    noSchema
    Ty.int32
    Ty.string
    "cannot unify",

  unifyTypesFailCase "fail to unify list with function"
    noSchema
    (Ty.list Ty.int32)
    (Ty.function Ty.int32 Ty.int32)
    "cannot unify",

  -- Occur check failure
  unifyTypesFailCase "occur check: variable with list containing it"
    noSchema
    (Ty.var "a")
    (Ty.list (Ty.var "a"))
    "appears free"]

-- ============================================================
-- joinTypes tests
-- ============================================================

-- Helper to create type constraints
constraint :: TTerm Type -> TTerm Type -> TTerm TypeConstraint
constraint left right = Phantoms.record _TypeConstraint [
  _TypeConstraint_left>>: left,
  _TypeConstraint_right>>: right,
  _TypeConstraint_comment>>: Phantoms.string "join types; test"]

-- Helper for empty constraint list
noConstraints :: TTerm [TypeConstraint]
noConstraints = Phantoms.list ([] :: [TTerm TypeConstraint])

joinTypesTests :: TTerm TestGroup
joinTypesTests = subgroup "joinTypes" [
  -- Identical primitive types produce no constraints
  joinTypesCase "join identical int32"
    Ty.int32
    Ty.int32
    noConstraints,

  joinTypesCase "join identical string"
    Ty.string
    Ty.string
    noConstraints,

  -- Note: joinTypes doesn't handle type variables directly - those are handled by unifyTypeConstraints.
  -- Testing variable joining with joinTypes would fail because it's not the right level of abstraction.

  -- List types produce constraint on element types
  joinTypesCase "join list types"
    (Ty.list (Ty.var "a"))
    (Ty.list Ty.int32)
    (Phantoms.list [constraint (Ty.var "a") Ty.int32]),

  -- Function types produce constraints on domain and codomain
  joinTypesCase "join function types"
    (Ty.function (Ty.var "a") (Ty.var "b"))
    (Ty.function Ty.int32 Ty.string)
    (Phantoms.list [constraint (Ty.var "a") Ty.int32, constraint (Ty.var "b") Ty.string]),

  -- Optional types
  joinTypesCase "join optional types"
    (Ty.optional (Ty.var "a"))
    (Ty.optional Ty.int32)
    (Phantoms.list [constraint (Ty.var "a") Ty.int32]),

  -- Pair types
  joinTypesCase "join pair types"
    (Ty.pair (Ty.var "a") (Ty.var "b"))
    (Ty.pair Ty.int32 Ty.string)
    (Phantoms.list [constraint (Ty.var "a") Ty.int32, constraint (Ty.var "b") Ty.string]),

  -- Either types
  joinTypesCase "join either types"
    (Ty.either_ (Ty.var "a") (Ty.var "b"))
    (Ty.either_ Ty.int32 Ty.string)
    (Phantoms.list [constraint (Ty.var "a") Ty.int32, constraint (Ty.var "b") Ty.string]),

  -- Map types
  joinTypesCase "join map types"
    (Ty.map (Ty.var "k") (Ty.var "v"))
    (Ty.map Ty.string Ty.int32)
    (Phantoms.list [constraint (Ty.var "k") Ty.string, constraint (Ty.var "v") Ty.int32]),

  -- Set types
  joinTypesCase "join set types"
    (Ty.set (Ty.var "a"))
    (Ty.set Ty.int32)
    (Phantoms.list [constraint (Ty.var "a") Ty.int32]),

  -- Unit types
  joinTypesCase "join unit types"
    Ty.unit
    Ty.unit
    noConstraints,

  -- Failure cases
  joinTypesFailCase "fail to join int32 with string"
    Ty.int32
    Ty.string,

  joinTypesFailCase "fail to join list with function"
    (Ty.list Ty.int32)
    (Ty.function Ty.int32 Ty.int32),

  joinTypesFailCase "fail to join pair with either"
    (Ty.pair Ty.int32 Ty.string)
    (Ty.either_ Ty.int32 Ty.string)]

-- ============================================================
-- All tests
-- ============================================================

allTests :: TBinding TestGroup
allTests = define "allTests" $
    doc "Test cases for type unification operations" $
    supergroup "unification" [
      variableOccursInTypeTests,
      unifyTypesTests,
      joinTypesTests]
