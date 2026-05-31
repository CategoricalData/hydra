-- | Primitive declarations for the hydra.lib.equality namespace.

module Hydra.Sources.Kernel.Lib.Equality where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap          as Bootstrap
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import           Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Types              as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.lib.equality"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.equality module."}
  where
    definitions = [
      primNoDef "compare" "Compare two values and return a Comparison." compareSig (Just
        "compare(x, y) returns the hydra.util.Comparison value that classifies the relationship between x\
        \ and y under the type's ordering: LessThan if x < y, EqualTo if x == y, GreaterThan if x > y. The\
        \ result type's three-valued tag is the canonical primitive comparison; the boolean comparators\
        \ (lt/lte/gt/gte) and equal are derivable from it. Requires an 'ordering' type-class constraint on\
        \ the argument type, which is the closest Hydra equivalent to Haskell's Ord instance. Total.\
        \ Corresponds to Haskell's compare :: Ord a => a -> a -> Ordering."),
      primNoDef "equal"   "Check whether two values are equal." equalSig (Just
        "equal(x, y) returns true if x and y are structurally equal under the type's notion of equality.\
        \ Requires an 'equality' type-class constraint on the argument type, which is the closest Hydra\
        \ equivalent to Haskell's Eq instance. Equality is reflexive, symmetric, and transitive (no\
        \ NaN-style exception on floating-point: floating-point equality is provided per the underlying\
        \ host's IEEE 754 comparison rules, so NaN /= NaN at the level of float64/float32). Total.\
        \ Corresponds to Haskell's (==) :: Eq a => a -> a -> Bool."),
      primNoDef "gt"      "Check whether the first value is greater than the second." gtSig (Just
        "gt(x, y) returns true iff x > y under the type's ordering. Requires an 'ordering' constraint on\
        \ the argument type. Total. Corresponds to Haskell's (>) :: Ord a => a -> a -> Bool."),
      primNoDef "gte"     "Check whether the first value is greater than or equal to the second." gteSig (Just
        "gte(x, y) returns true iff x >= y under the type's ordering. Requires an 'ordering' constraint on\
        \ the argument type. Total. Corresponds to Haskell's (>=) :: Ord a => a -> a -> Bool."),
      toPrimitive "Return a value unchanged." identitySig (Just
        "identity(x) = x. The polymorphic identity function. Total. Corresponds to Haskell's\
        \ id :: a -> a.") identity_,
      primNoDef "lt"      "Check whether the first value is less than the second." ltSig (Just
        "lt(x, y) returns true iff x < y under the type's ordering. Requires an 'ordering' constraint on\
        \ the argument type. Total. Corresponds to Haskell's (<) :: Ord a => a -> a -> Bool."),
      primNoDef "lte"     "Check whether the first value is less than or equal to the second." lteSig (Just
        "lte(x, y) returns true iff x <= y under the type's ordering. Requires an 'ordering' constraint on\
        \ the argument type. Total. Corresponds to Haskell's (<=) :: Ord a => a -> a -> Bool."),
      toPrimitive "Return the maximum of two values." maxSig (Just
        "max(x, y) returns the larger of x and y under the type's ordering; if x == y, it returns y\
        \ (matching Haskell's convention). Requires an 'ordering' constraint on the argument type. Total.\
        \ Corresponds to Haskell's max :: Ord a => a -> a -> a.") max_,
      toPrimitive "Return the minimum of two values." minSig (Just
        "min(x, y) returns the smaller of x and y under the type's ordering; if x == y, it returns x\
        \ (matching Haskell's convention). Requires an 'ordering' constraint on the argument type. Total.\
        \ Corresponds to Haskell's min :: Ord a => a -> a -> a.") min_]
primNoDef :: String -> String -> TermSignature -> Maybe String -> Definition
primNoDef localName description s comments =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName)) comments

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

-- Signatures (derived from Hydra.Sources.Libraries primN declarations).

-- compare : forall x. ordering => x -> x -> hydra.util.Comparison
compareSig :: TermSignature
compareSig = sig $ Types.polyConstrained [("x", [Name "ordering"])]
  (Types.var "x" Types.~> Types.var "x" Types.~> Types.var "hydra.util.Comparison")

-- equal : forall x. equality => x -> x -> Boolean
equalSig :: TermSignature
equalSig = sig $ Types.polyConstrained [("x", [Name "equality"])]
  (Types.var "x" Types.~> Types.var "x" Types.~> Types.boolean)

-- gt : forall x. ordering => x -> x -> Boolean
gtSig :: TermSignature
gtSig = sig $ Types.polyConstrained [("x", [Name "ordering"])]
  (Types.var "x" Types.~> Types.var "x" Types.~> Types.boolean)

-- gte : forall x. ordering => x -> x -> Boolean
gteSig :: TermSignature
gteSig = sig $ Types.polyConstrained [("x", [Name "ordering"])]
  (Types.var "x" Types.~> Types.var "x" Types.~> Types.boolean)

-- identity : forall x. x -> x
identitySig :: TermSignature
identitySig = sig $ TypeScheme [Name "x"]
  (Types.var "x" Types.~> Types.var "x")
  Nothing

-- lt : forall x. ordering => x -> x -> Boolean
ltSig :: TermSignature
ltSig = sig $ Types.polyConstrained [("x", [Name "ordering"])]
  (Types.var "x" Types.~> Types.var "x" Types.~> Types.boolean)

-- lte : forall x. ordering => x -> x -> Boolean
lteSig :: TermSignature
lteSig = sig $ Types.polyConstrained [("x", [Name "ordering"])]
  (Types.var "x" Types.~> Types.var "x" Types.~> Types.boolean)

-- max : forall x. ordering => x -> x -> x
maxSig :: TermSignature
maxSig = sig $ Types.polyConstrained [("x", [Name "ordering"])]
  (Types.var "x" Types.~> Types.var "x" Types.~> Types.var "x")

-- min : forall x. ordering => x -> x -> x
minSig :: TermSignature
minSig = sig $ Types.polyConstrained [("x", [Name "ordering"])]
  (Types.var "x" Types.~> Types.var "x" Types.~> Types.var "x")

-- Default implementations.

-- identity x = x
identity_ :: TypedTermDefinition (a -> a)
identity_ = define "identity" $
  doc "Return a value unchanged." $
  "x" ~> var "x"

-- max x y = ifElse (gte x y) x y
max_ :: TypedTermDefinition (a -> a -> a)
max_ = define "max" $
  doc "Return the maximum of two values, defined in terms of gte and ifElse." $
  "x" ~> "y" ~> Logic.ifElse (Equality.gte (var "x") (var "y")) (var "x" :: TypedTerm a) (var "y")

-- min x y = ifElse (lte x y) x y
min_ :: TypedTermDefinition (a -> a -> a)
min_ = define "min" $
  doc "Return the minimum of two values, defined in terms of lte and ifElse." $
  "x" ~> "y" ~> Logic.ifElse (Equality.lte (var "x") (var "y")) (var "x" :: TypedTerm a) (var "y")
