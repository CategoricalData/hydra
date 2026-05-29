-- | Primitive declarations for the hydra.lib.logic namespace.

module Hydra.Sources.Kernel.Lib.Logic where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Sources.Kernel.Terms.Scoping as Scoping
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.lib.logic"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.logic namespace."}
  where
    definitions = [
      toPrimitive "Compute the logical AND of two boolean values." andSig and_,
      primNoDef "ifElse" "Compute a conditional expression." ifElseSig,
      toPrimitive "Compute the logical NOT of a boolean value." notSig not_,
      toPrimitive "Compute the logical OR of two boolean values." orSig or_]

-- Local convenience: build a no-default primitive Definition from a local name.
primNoDef :: String -> String -> TermSignature -> Definition
primNoDef localName description s =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName))

-- Local convenience: build a TermSignature from a TypeScheme.
sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

-- Signatures

andSig :: TermSignature
andSig = sig $ TypeScheme [] (Types.boolean Types.~> Types.boolean Types.~> Types.boolean) Nothing

ifElseSig :: TermSignature
ifElseSig = sig $ TypeScheme [Name "x"]
  (Types.boolean Types.~> Types.var "x" Types.~> Types.var "x" Types.~> Types.var "x")
  Nothing

notSig :: TermSignature
notSig = sig $ TypeScheme [] (Types.boolean Types.~> Types.boolean) Nothing

orSig :: TermSignature
orSig = sig $ TypeScheme [] (Types.boolean Types.~> Types.boolean Types.~> Types.boolean) Nothing

-- Default implementations (all defined in terms of ifElse).

and_ :: TTermDefinition (Bool -> Bool -> Bool)
and_ = define "and" $
  doc "Logical AND, defined in terms of ifElse." $
  "a" ~> "b" ~> Logic.ifElse (var "a") (var "b" :: TTerm Bool) false

not_ :: TTermDefinition (Bool -> Bool)
not_ = define "not" $
  doc "Logical NOT, defined in terms of ifElse." $
  "a" ~> Logic.ifElse (var "a") false true

or_ :: TTermDefinition (Bool -> Bool -> Bool)
or_ = define "or" $
  doc "Logical OR, defined in terms of ifElse." $
  "a" ~> "b" ~> Logic.ifElse (var "a") true (var "b" :: TTerm Bool)
