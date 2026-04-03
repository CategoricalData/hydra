{-# LANGUAGE FlexibleContexts #-}

-- | Test cases for type unification operations
module Hydra.Sources.Test.Unification where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Dsl.Typing             as Typing
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Hydra.Sources.Kernel.Terms.Lexical as LexicalModule
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Sources.Kernel.Terms.Unification as UnificationModule


ns :: Namespace
ns = Namespace "hydra.test.unification"

module_ :: Module
module_ = Module ns elements
    [UnificationModule.ns, LexicalModule.ns, ShowCore.ns]
    kernelTypesNamespaces
    (Just "Test cases for type unification operations")
  where
    elements = [Phantoms.toDefinition allTests]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- Helper to build names
nm :: String -> TTerm Name
nm s = Core.name $ Phantoms.string s

-- Local alias for polymorphic application
(#) :: (AsTerm f (a -> b), AsTerm g a) => f -> g -> TTerm b
(#) = (Phantoms.@@)
infixl 1 #

-- | Universal variableOccursInType test case
variableOccursCase :: String -> TTerm Name -> TTerm Type -> TTerm Bool -> TTerm TestCaseWithMetadata
variableOccursCase cname variable typ expected =
  universalCase cname
    (Literals.showBoolean (UnificationModule.variableOccursInType # variable # typ))
    (Literals.showBoolean expected)

-- | Build schema types map from a list of names.
-- Each name gets TypeScheme [] (TypeVariable name) Nothing
buildSchemaMap :: TTerm [Name] -> TTerm (M.Map Name TypeScheme)
buildSchemaMap names = Maps.fromList (Lists.map
  (Phantoms.lambda "n" $ Phantoms.pair (Phantoms.var "n") (T.mono (Core.typeVariable (Phantoms.var "n"))))
  names)

-- | Show a TypeSubst as a sorted string like "{a: int32, b: string}"
showTypeSubst :: TTerm TypeSubst -> TTerm String
showTypeSubst ts = Strings.cat (Phantoms.list [
  Phantoms.string "{",
  Strings.intercalate (Phantoms.string ", ")
    (Lists.map (Phantoms.lambda "p" $ Strings.cat (Phantoms.list [
      Core.unName (Pairs.first (Phantoms.var "p")),
      Phantoms.string ": ",
      ShowCore.type_ # Pairs.second (Phantoms.var "p")]))
      (Maps.toList (Typing.unTypeSubst ts))),
  Phantoms.string "}"])

-- | Show a list of TypeConstraints as "[(left ~ right), ...]"
showConstraints :: TTerm [TypeConstraint] -> TTerm String
showConstraints cs = Strings.cat (Phantoms.list [
  Phantoms.string "[",
  Strings.intercalate (Phantoms.string ", ")
    (Lists.map (Phantoms.lambda "c" $ Strings.cat (Phantoms.list [
      Phantoms.string "(",
      ShowCore.type_ # Typing.typeConstraintLeft (Phantoms.var "c"),
      Phantoms.string " ~ ",
      ShowCore.type_ # Typing.typeConstraintRight (Phantoms.var "c"),
      Phantoms.string ")"]))
      cs),
  Phantoms.string "]"])

-- | Universal unifyTypes test case (expecting success)
unifyTypesCase :: String -> TTerm [Name] -> TTerm Type -> TTerm Type -> [(TTerm Name, TTerm Type)] -> TTerm TestCaseWithMetadata
unifyTypesCase cname schemaTypes left right substPairs = universalCase cname
  (Eithers.either_
    (Phantoms.lambda "_" $ Phantoms.string "failure")
    (Phantoms.lambda "ts" $ showTypeSubst (Phantoms.var "ts"))
    (UnificationModule.unifyTypes # LexicalModule.emptyContext # buildSchemaMap schemaTypes # left # right # Phantoms.string "test"))
  (showTypeSubst (Phantoms.wrap _TypeSubst (Phantoms.map (M.fromList substPairs))))

-- | Universal unifyTypes test case (expecting failure)
unifyTypesFailCase :: String -> TTerm [Name] -> TTerm Type -> TTerm Type -> String -> TTerm TestCaseWithMetadata
unifyTypesFailCase cname schemaTypes left right _errSubstring = universalCase cname
  (Eithers.either_
    (Phantoms.lambda "_" $ Phantoms.string "failure")
    (Phantoms.lambda "ts" $ showTypeSubst (Phantoms.var "ts"))
    (UnificationModule.unifyTypes # LexicalModule.emptyContext # buildSchemaMap schemaTypes # left # right # Phantoms.string "test"))
  (Phantoms.string "failure")

-- | Universal joinTypes test case (expecting success)
joinTypesCase :: String -> TTerm Type -> TTerm Type -> TTerm [TypeConstraint] -> TTerm TestCaseWithMetadata
joinTypesCase cname left right constraints = universalCase cname
  (Eithers.either_
    (Phantoms.lambda "_" $ Phantoms.string "failure")
    (Phantoms.lambda "cs" $ showConstraints (Phantoms.var "cs"))
    (UnificationModule.joinTypes # LexicalModule.emptyContext # left # right # Phantoms.string "test"))
  (showConstraints constraints)

-- | Universal joinTypes test case (expecting failure)
joinTypesFailCase :: String -> TTerm Type -> TTerm Type -> TTerm TestCaseWithMetadata
joinTypesFailCase cname left right = universalCase cname
  (Eithers.either_
    (Phantoms.lambda "_" $ Phantoms.string "failure")
    (Phantoms.lambda "cs" $ showConstraints (Phantoms.var "cs"))
    (UnificationModule.joinTypes # LexicalModule.emptyContext # left # right # Phantoms.string "test"))
  (Phantoms.string "failure")

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

allTests :: TTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for type unification operations" $
    supergroup "unification" [
      variableOccursInTypeTests,
      unifyTypesTests,
      joinTypesTests]
