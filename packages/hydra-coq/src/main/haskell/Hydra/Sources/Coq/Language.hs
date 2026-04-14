{-# LANGUAGE FlexibleContexts #-}

-- | Language constraints for Coq code generation.

module Hydra.Sources.Coq.Language where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Coders                          as Coders
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.Other.Coq               as CoqSyntax
import           Prelude hiding ((++))
import qualified Data.List                                 as DL
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

import Hydra.Ast


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.coq.language"

module_ :: Module
module_ = Module ns definitions
    [Constants.ns, Formatting.ns]
    (CoqSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Language constraints for Coq code generation"
  where
    definitions = [
      toDefinition coqLanguage,
      toDefinition coqReservedWords]

-- | Language constraints for Coq.
-- Coq's type system (CIC) is a strict superset of System F, so it supports all Hydra features.
coqLanguage :: TTermDefinition Language
coqLanguage = define "coqLanguage" $ lets [
    "eliminationVariants">: Sets.fromList $ list [
      Variants.eliminationVariantRecord,
      Variants.eliminationVariantUnion,
      Variants.eliminationVariantWrap],
    "literalVariants">: Sets.fromList $ list [
      Variants.literalVariantBoolean,
      Variants.literalVariantFloat,
      Variants.literalVariantInteger,
      Variants.literalVariantString],
    "floatTypes">: Sets.fromList $ list [
      Core.floatTypeFloat64],
    "functionVariants">: Sets.fromList $ list [
      Variants.functionVariantElimination,
      Variants.functionVariantLambda],
    "integerTypes">: Sets.fromList $ list [
      Core.integerTypeBigint],
    "termVariants">: Sets.fromList $ list [
      Variants.termVariantApplication,
      Variants.termVariantEither,

      Variants.termVariantCases,

      Variants.termVariantLambda,

      Variants.termVariantProject,

      Variants.termVariantUnwrap,

      Variants.termVariantTypeApplication,

      Variants.termVariantTypeLambda,
      Variants.termVariantLet,
      Variants.termVariantList,
      Variants.termVariantLiteral,
      Variants.termVariantMap,
      Variants.termVariantMaybe,
      Variants.termVariantPair,
      Variants.termVariantRecord,
      Variants.termVariantSet,
      Variants.termVariantUnion,
      Variants.termVariantUnit,
      Variants.termVariantVariable,
      Variants.termVariantWrap],
    "typeVariants">: Sets.fromList $ list [
      Variants.typeVariantAnnotated,
      Variants.typeVariantApplication,
      Variants.typeVariantEither,
      Variants.typeVariantForall,
      Variants.typeVariantFunction,
      Variants.typeVariantList,
      Variants.typeVariantLiteral,
      Variants.typeVariantMap,
      Variants.typeVariantMaybe,
      Variants.typeVariantPair,
      Variants.typeVariantRecord,
      Variants.typeVariantSet,
      Variants.typeVariantUnion,
      Variants.typeVariantUnit,
      Variants.typeVariantVariable,
      Variants.typeVariantVoid,
      Variants.typeVariantWrap],
    "typePredicate">: constant true] $
    Coders.language
      (Coders.languageName_ $ string "hydra.coq")
      (Coders.languageConstraints_
        (var "eliminationVariants")
        (var "literalVariants")
        (var "floatTypes")
        (var "functionVariants")
        (var "integerTypes")
        (var "termVariants")
        (var "typeVariants")
        (var "typePredicate"))

-- | Reserved words in Coq
coqReservedWords :: TTermDefinition (S.Set String)
coqReservedWords = define "coqReservedWords" $
  Sets.fromList $ list $ fmap string reservedWords
  where
    reservedWords = [
      -- Gallina keywords
      "as", "at", "cofix", "else", "end", "exists", "exists2", "fix",
      "for", "forall", "fun", "if", "IF", "in", "let", "match",
      "mod", "Prop", "return", "Set", "then", "Type", "using",
      "where", "with",
      -- Vernacular keywords
      "Abort", "About", "Add", "Admit", "Admitted", "All",
      "Arguments", "Axiom", "Check", "Class", "Close",
      "CoFixpoint", "CoInductive", "Coercion", "Compute",
      "Conjecture", "Context", "Corollary", "Declare",
      "Defined", "Definition", "Derive", "End", "Eval",
      "Example", "Existing", "Export", "Fact", "Fixpoint",
      "From", "Function", "Generalizable", "Global", "Goal",
      "Hint", "Hypothesis", "Import", "Inductive", "Infix",
      "Instance", "Lemma", "Let", "Local", "Ltac", "Module",
      "Notation", "Obligation", "Opaque", "Open", "Parameter",
      "Polymorphic", "Proof", "Program", "Proposition", "Qed",
      "Record", "Require", "Remark", "Scope", "Search",
      "Section", "Set", "Show", "Strategy", "Structure",
      "Tactic", "Theorem", "Transparent", "Unset", "Variable",
      "Variables",
      -- Standard library names we'll reference
      "bool", "nat", "Z", "list", "option", "prod", "sum",
      "unit", "Empty_set", "String", "true", "false",
      "None", "Some", "nil", "cons", "pair", "inl", "inr", "tt"]
