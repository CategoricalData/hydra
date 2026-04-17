-- Note: this is an automatically generated file. Do not edit.

-- | Language constraints and reserved words for WebAssembly (WAT text format)

module Hydra.Wasm.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S

-- | Language constraints for WebAssembly
wasmLanguage :: Coders.Language
wasmLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.wasm"),
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
          Variants.EliminationVariantUnion]
    literalVariants =
        Sets.fromList [
          Variants.LiteralVariantBoolean,
          Variants.LiteralVariantFloat,
          Variants.LiteralVariantInteger,
          Variants.LiteralVariantString]
    floatTypes =
        Sets.fromList [
          Core.FloatTypeFloat32,
          Core.FloatTypeFloat64]
    functionVariants =
        Sets.fromList [
          Variants.FunctionVariantElimination,
          Variants.FunctionVariantLambda]
    integerTypes =
        Sets.fromList [
          Core.IntegerTypeInt8,
          Core.IntegerTypeInt16,
          Core.IntegerTypeInt32,
          Core.IntegerTypeInt64,
          Core.IntegerTypeUint8,
          Core.IntegerTypeUint16,
          Core.IntegerTypeUint32,
          Core.IntegerTypeUint64]
    termVariants =
        Sets.fromList [
          Variants.TermVariantAnnotated,
          Variants.TermVariantApplication,
          Variants.TermVariantCases,
          Variants.TermVariantEither,
          Variants.TermVariantInject,
          Variants.TermVariantLambda,
          Variants.TermVariantLet,
          Variants.TermVariantList,
          Variants.TermVariantLiteral,
          Variants.TermVariantMap,
          Variants.TermVariantMaybe,
          Variants.TermVariantPair,
          Variants.TermVariantProject,
          Variants.TermVariantRecord,
          Variants.TermVariantSet,
          Variants.TermVariantTypeApplication,
          Variants.TermVariantTypeLambda,
          Variants.TermVariantUnit,
          Variants.TermVariantUnwrap,
          Variants.TermVariantVariable,
          Variants.TermVariantWrap]
    typeVariants =
        Sets.fromList [
          Variants.TypeVariantAnnotated,
          Variants.TypeVariantApplication,
          Variants.TypeVariantEither,
          Variants.TypeVariantFunction,
          Variants.TypeVariantForall,
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

-- | A set of reserved words in WebAssembly text format
wasmReservedWords :: S.Set String
wasmReservedWords =
    Sets.fromList (Lists.concat [
      keywords,
      valueTypes,
      instructionPrefixes])
  where
    keywords =
        [
          "module",
          "type",
          "func",
          "param",
          "result",
          "local",
          "global",
          "table",
          "memory",
          "elem",
          "data",
          "start",
          "import",
          "export",
          "block",
          "loop",
          "if",
          "then",
          "else",
          "end",
          "br",
          "br_if",
          "br_table",
          "return",
          "call",
          "call_indirect",
          "drop",
          "select",
          "unreachable",
          "nop",
          "mut",
          "offset",
          "align"]
    valueTypes =
        [
          "i32",
          "i64",
          "f32",
          "f64",
          "v128",
          "funcref",
          "externref"]
    instructionPrefixes =
        [
          "i32",
          "i64",
          "f32",
          "f64",
          "local.get",
          "local.set",
          "local.tee",
          "global.get",
          "global.set",
          "memory.size",
          "memory.grow"]
