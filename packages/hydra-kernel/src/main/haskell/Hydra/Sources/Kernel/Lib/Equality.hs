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

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.equality namespace."}
  where
    definitions = [
      primNoDef "compare" "Compare two values and return a Comparison." compareSig Nothing,
      primNoDef "equal"   "Check whether two values are equal." equalSig Nothing,
      primNoDef "gt"      "Check whether the first value is greater than the second." gtSig Nothing,
      primNoDef "gte"     "Check whether the first value is greater than or equal to the second." gteSig Nothing,
      toPrimitive "Return a value unchanged." identitySig Nothing identity_,
      primNoDef "lt"      "Check whether the first value is less than the second." ltSig Nothing,
      primNoDef "lte"     "Check whether the first value is less than or equal to the second." lteSig Nothing,
      toPrimitive "Return the maximum of two values." maxSig Nothing max_,
      toPrimitive "Return the minimum of two values." minSig Nothing min_]
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
identity_ :: TTermDefinition (a -> a)
identity_ = define "identity" $
  doc "Return a value unchanged." $
  "x" ~> var "x"

-- max x y = ifElse (gte x y) x y
max_ :: TTermDefinition (a -> a -> a)
max_ = define "max" $
  doc "Return the maximum of two values, defined in terms of gte and ifElse." $
  "x" ~> "y" ~> Logic.ifElse (Equality.gte (var "x") (var "y")) (var "x" :: TTerm a) (var "y")

-- min x y = ifElse (lte x y) x y
min_ :: TTermDefinition (a -> a -> a)
min_ = define "min" $
  doc "Return the minimum of two values, defined in terms of lte and ifElse." $
  "x" ~> "y" ~> Logic.ifElse (Equality.lte (var "x") (var "y")) (var "x" :: TTerm a) (var "y")
