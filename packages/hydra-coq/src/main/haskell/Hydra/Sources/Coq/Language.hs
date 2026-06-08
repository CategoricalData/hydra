{-# LANGUAGE FlexibleContexts #-}

-- | Language constraints for Coq code generation.

module Hydra.Sources.Coq.Language where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Coders                          as Coders
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Util                              as Util
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.Coq.Syntax              as CoqSyntax
import           Prelude hiding ((++))
import qualified Data.List                                 as DL
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

import Hydra.Ast


define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

ns :: ModuleName
ns = ModuleName "hydra.coq.language"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([Constants.ns, Formatting.ns] DL.++ (CoqSyntax.ns:KernelTypes.kernelTypesModuleNames)),
            moduleMetadata = descriptionMetadata (Just "Language constraints for Coq code generation")}
  where
    definitions = [
      toDefinition coqLanguage,
      toDefinition coqReservedWords,
      toDefinition coqStrippedReservedWords]

-- | Language constraints for Coq.
-- Coq's type system (CIC) is a strict superset of System F, so it supports all Hydra features.
coqLanguage :: TypedTermDefinition Language
coqLanguage = define "coqLanguage" $ lets [
    "literalVariants">: Sets.fromList $ list [
      Variants.literalVariantBoolean,
      Variants.literalVariantFloat,
      Variants.literalVariantInteger,
      Variants.literalVariantString],
    "floatTypes">: Sets.fromList $ list [
      Core.floatTypeFloat64],
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
      Variants.termVariantOptional,
      Variants.termVariantPair,
      Variants.termVariantRecord,
      Variants.termVariantSet,
      Variants.termVariantInject,
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
      Variants.typeVariantOptional,
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
      (Coders.languageName2 $ string "hydra.coq")
      (Coders.languageConstraints2
        (var "literalVariants")
        (var "floatTypes")
        (var "integerTypes")
        (var "termVariants")
        (var "typeVariants")
        (var "typePredicate"))
      (Sets.fromList $ list [
        Coders.languageFeaturePartialApplication,
        Coders.languageFeatureNestedCaseStatements,
        Coders.languageFeatureNestedPolymorphicLetBindings])
      (Coders.caseConventions
        Util.caseConventionUpperSnake Util.caseConventionPascal Util.caseConventionPascal
        Util.caseConventionCamel Util.caseConventionPascal Util.caseConventionPascal
        Util.caseConventionCamel Util.caseConventionCamel Util.caseConventionPascal
        Util.caseConventionCamel)
      (wrap _FileExtension (string "v"))

-- | Reserved words that must be renamed when used as Coq variable or binding names.
-- This set is kept narrower than the full Coq lexicon: it includes only the tokens
-- that actually clash with Coq variable-name usage (Gallina and vernacular keywords)
-- plus a few stdlib names that appear as Hydra-generated lambda parameter names.
coqReservedWords :: TypedTermDefinition (S.Set String)
coqReservedWords = define "coqReservedWords" $
  Sets.fromList $ list $ fmap string reservedWords
  where
    reservedWords = [
      -- Gallina and vernacular keywords
      "as", "at", "cofix", "do", "else", "end", "exists", "exists2",
      "fix", "for", "forall", "fun", "if", "IF", "in", "let", "match",
      "mod", "open", "Prop", "return", "Set", "then", "Type", "using",
      "where", "with",
      "Axiom", "Class", "Coercion", "Context", "Definition", "Fixpoint",
      "Hypothesis", "Inductive", "Instance", "Lemma", "Module", "Notation",
      "Proof", "Qed", "Record", "Require", "Import", "Section", "End",
      "Theorem", "Example", "Variable", "Variables",
      -- Coq stdlib names that appear as Hydra-generated lambda parameter names
      "cons", "pair", "nil",
      -- Coq stdlib type constructors that can be shadowed by local let bindings,
      -- causing cryptic "Non-functional construction" errors when subsequent
      -- code tries to use the type.
      "list", "option", "prod", "sum", "unit", "bool", "nat", "string",
      -- Names that collide with Hydra kernel function names after namespace
      -- stripping (e.g., hydra.show.core.term, hydra.show.core.type).
      "term", "literal", "graph", "element"]

-- | Reserved words that must be renamed when they appear as a stripped-local
-- form of a cross-module Hydra reference. This set is used only by
-- `resolveQualifiedName` when it converts a `hydra.<ns>.<x>` reference to its
-- short form. It is strictly narrower than `coqReservedWords` — in particular
-- it does *not* include `term`, `literal`, `element`, whose cross-module
-- disambiguation is handled by the ambiguous-names mechanism, not by
-- underscore-escaping. It is narrower at the lambda-parameter side too, but
-- that set is tracked separately via `coqReservedWords`.
coqStrippedReservedWords :: TypedTermDefinition (S.Set String)
coqStrippedReservedWords = define "coqStrippedReservedWords" $
  Sets.fromList $ list $ fmap string reservedWords
  where
    reservedWords = [
      -- Gallina and vernacular keywords
      "as", "at", "cofix", "do", "else", "end", "exists", "exists2",
      "fix", "for", "forall", "fun", "if", "IF", "in", "let", "match",
      "mod", "Prop", "return", "Set", "then", "Type", "using",
      "where", "with",
      "Axiom", "Class", "Coercion", "Context", "Definition", "Fixpoint",
      "Hypothesis", "Inductive", "Instance", "Lemma", "Module", "Notation",
      "Proof", "Qed", "Record", "Require", "Import", "Section", "End",
      "Theorem", "Example", "Variable", "Variables",
      -- Coq stdlib type names
      "bool", "nat", "list", "option", "prod", "sum", "unit",
      "string", "String", "Empty_set",
      -- Coq stdlib value constructors
      "true", "false", "None", "Some", "nil", "cons", "pair",
      "inl", "inr", "tt",
      -- Hydra module name whose stripped form collides with generated
      -- definition names.
      "graph"]
