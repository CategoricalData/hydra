-- Note: this is an automatically generated file. Do not edit.

-- | Language constraints for Coq code generation

module Hydra.Coq.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Set as S

coqLanguage :: Coders.Language
coqLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.coq"),
      Coders.languageConstraints = Coders.LanguageConstraints {
        Coders.languageConstraintsEliminationVariants = eliminationVariants,
        Coders.languageConstraintsLiteralVariants = literalVariants,
        Coders.languageConstraintsFloatTypes = floatTypes,
        Coders.languageConstraintsFunctionVariants = functionVariants,
        Coders.languageConstraintsIntegerTypes = integerTypes,
        Coders.languageConstraintsTermVariants = termVariants,
        Coders.languageConstraintsTypeVariants = typeVariants,
        Coders.languageConstraintsTypes = typePredicate}}
  where
    eliminationVariants =
        Sets.fromList [
          Variants.EliminationVariantRecord,
          Variants.EliminationVariantUnion,
          Variants.EliminationVariantWrap]
    literalVariants =
        Sets.fromList [
          Variants.LiteralVariantBoolean,
          Variants.LiteralVariantFloat,
          Variants.LiteralVariantInteger,
          Variants.LiteralVariantString]
    floatTypes = Sets.fromList [
      Core.FloatTypeFloat64]
    functionVariants =
        Sets.fromList [
          Variants.FunctionVariantElimination,
          Variants.FunctionVariantLambda]
    integerTypes = Sets.fromList [
      Core.IntegerTypeBigint]
    termVariants =
        Sets.fromList [
          Variants.TermVariantApplication,
          Variants.TermVariantEither,
          Variants.TermVariantCases,
          Variants.TermVariantLambda,
          Variants.TermVariantProject,
          Variants.TermVariantUnwrap,
          Variants.TermVariantTypeApplication,
          Variants.TermVariantTypeLambda,
          Variants.TermVariantLet,
          Variants.TermVariantList,
          Variants.TermVariantLiteral,
          Variants.TermVariantMap,
          Variants.TermVariantMaybe,
          Variants.TermVariantPair,
          Variants.TermVariantRecord,
          Variants.TermVariantSet,
          Variants.TermVariantInject,
          Variants.TermVariantUnit,
          Variants.TermVariantVariable,
          Variants.TermVariantWrap]
    typeVariants =
        Sets.fromList [
          Variants.TypeVariantAnnotated,
          Variants.TypeVariantApplication,
          Variants.TypeVariantEither,
          Variants.TypeVariantForall,
          Variants.TypeVariantFunction,
          Variants.TypeVariantList,
          Variants.TypeVariantLiteral,
          Variants.TypeVariantMap,
          Variants.TypeVariantMaybe,
          Variants.TypeVariantPair,
          Variants.TypeVariantRecord,
          Variants.TypeVariantSet,
          Variants.TypeVariantUnion,
          Variants.TypeVariantUnit,
          Variants.TypeVariantVariable,
          Variants.TypeVariantVoid,
          Variants.TypeVariantWrap]
    typePredicate = \_ -> True

coqReservedWords :: S.Set String
coqReservedWords =
    Sets.fromList [
      "as",
      "at",
      "cofix",
      "else",
      "end",
      "exists",
      "exists2",
      "fix",
      "for",
      "forall",
      "fun",
      "if",
      "IF",
      "in",
      "let",
      "match",
      "mod",
      "Prop",
      "return",
      "Set",
      "then",
      "Type",
      "using",
      "where",
      "with",
      "Abort",
      "About",
      "Add",
      "Admit",
      "Admitted",
      "All",
      "Arguments",
      "Axiom",
      "Check",
      "Class",
      "Close",
      "CoFixpoint",
      "CoInductive",
      "Coercion",
      "Compute",
      "Conjecture",
      "Context",
      "Corollary",
      "Declare",
      "Defined",
      "Definition",
      "Derive",
      "End",
      "Eval",
      "Example",
      "Existing",
      "Export",
      "Fact",
      "Fixpoint",
      "From",
      "Function",
      "Generalizable",
      "Global",
      "Goal",
      "Hint",
      "Hypothesis",
      "Import",
      "Inductive",
      "Infix",
      "Instance",
      "Lemma",
      "Let",
      "Local",
      "Ltac",
      "Module",
      "Notation",
      "Obligation",
      "Opaque",
      "Open",
      "Parameter",
      "Polymorphic",
      "Proof",
      "Program",
      "Proposition",
      "Qed",
      "Record",
      "Require",
      "Remark",
      "Scope",
      "Search",
      "Section",
      "Set",
      "Show",
      "Strategy",
      "Structure",
      "Tactic",
      "Theorem",
      "Transparent",
      "Unset",
      "Variable",
      "Variables",
      "bool",
      "nat",
      "Z",
      "list",
      "option",
      "prod",
      "sum",
      "unit",
      "Empty_set",
      "String",
      "true",
      "false",
      "None",
      "Some",
      "nil",
      "cons",
      "pair",
      "inl",
      "inr",
      "tt"]
