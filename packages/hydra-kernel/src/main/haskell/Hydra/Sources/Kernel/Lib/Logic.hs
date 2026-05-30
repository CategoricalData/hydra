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

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.logic module."}
  where
    definitions = [
      toPrimitive "Compute the logical AND of two boolean values." andSig (Just
        "and(p, q) returns true iff both p and q are true. Evaluation is strict in both arguments at the\
        \ primitive level; for short-circuiting behavior, use ifElse. Total. Corresponds to Haskell's\
        \ (&&) :: Bool -> Bool -> Bool (but without short-circuit evaluation).") and_,
      primNoDef "ifElse" "Compute a conditional expression." ifElseSig (Just
        "ifElse(p, t, f) returns t if p is true, or f if p is false. The unselected branch is not\
        \ necessarily evaluated; ifElse is the standard way to express short-circuiting boolean logic and\
        \ branching in Hydra. Total. Corresponds to Haskell's if/then/else."),
      toPrimitive "Compute the logical NOT of a boolean value." notSig (Just
        "not(p) returns false if p is true, or true if p is false. Total. Corresponds to Haskell's\
        \ not :: Bool -> Bool.") not_,
      toPrimitive "Compute the logical OR of two boolean values." orSig (Just
        "or(p, q) returns true iff at least one of p and q is true. Evaluation is strict in both\
        \ arguments at the primitive level; for short-circuiting behavior, use ifElse. Total. Corresponds\
        \ to Haskell's (||) :: Bool -> Bool -> Bool (but without short-circuit evaluation).") or_]
-- Local convenience: build a no-default primitive Definition from a local name.
primNoDef :: String -> String -> TermSignature -> Maybe String -> Definition
primNoDef localName description s comments =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName)) comments

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

and_ :: TypedTermDefinition (Bool -> Bool -> Bool)
and_ = define "and" $
  doc "Logical AND, defined in terms of ifElse." $
  "a" ~> "b" ~> Logic.ifElse (var "a") (var "b" :: TypedTerm Bool) false

not_ :: TypedTermDefinition (Bool -> Bool)
not_ = define "not" $
  doc "Logical NOT, defined in terms of ifElse." $
  "a" ~> Logic.ifElse (var "a") false true

or_ :: TypedTermDefinition (Bool -> Bool -> Bool)
or_ = define "or" $
  doc "Logical OR, defined in terms of ifElse." $
  "a" ~> "b" ~> Logic.ifElse (var "a") true (var "b" :: TypedTerm Bool)
