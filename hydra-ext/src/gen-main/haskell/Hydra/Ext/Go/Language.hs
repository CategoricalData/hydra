-- Note: this is an automatically generated file. Do not edit.

-- | Language constraints and reserved words for Go 1.22+

module Hydra.Ext.Go.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Language constraints for Go 1.22+
goLanguage :: Coders.Language
goLanguage = Coders.Language {
  Coders.languageName = (Coders.LanguageName "hydra.ext.go"),
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
    eliminationVariants = (Sets.fromList [
      Variants.EliminationVariantRecord,
      Variants.EliminationVariantUnion,
      Variants.EliminationVariantWrap])
    literalVariants = (Sets.fromList [
      Variants.LiteralVariantBinary,
      Variants.LiteralVariantBoolean,
      Variants.LiteralVariantFloat,
      Variants.LiteralVariantInteger,
      Variants.LiteralVariantString])
    floatTypes = (Sets.fromList [
      Core.FloatTypeFloat32,
      Core.FloatTypeFloat64])
    functionVariants = (Sets.fromList [
      Variants.FunctionVariantElimination,
      Variants.FunctionVariantLambda,
      Variants.FunctionVariantPrimitive])
    integerTypes = (Sets.fromList [
      Core.IntegerTypeBigint,
      Core.IntegerTypeInt8,
      Core.IntegerTypeInt16,
      Core.IntegerTypeInt32,
      Core.IntegerTypeInt64,
      Core.IntegerTypeUint8,
      Core.IntegerTypeUint16,
      Core.IntegerTypeUint32,
      Core.IntegerTypeUint64])
    termVariants = (Sets.fromList [
      Variants.TermVariantAnnotated,
      Variants.TermVariantApplication,
      Variants.TermVariantEither,
      Variants.TermVariantFunction,
      Variants.TermVariantLet,
      Variants.TermVariantList,
      Variants.TermVariantLiteral,
      Variants.TermVariantMap,
      Variants.TermVariantMaybe,
      Variants.TermVariantPair,
      Variants.TermVariantRecord,
      Variants.TermVariantSet,
      Variants.TermVariantTypeApplication,
      Variants.TermVariantTypeLambda,
      Variants.TermVariantUnion,
      Variants.TermVariantUnit,
      Variants.TermVariantVariable,
      Variants.TermVariantWrap])
    typeVariants = (Sets.fromList [
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
      Variants.TypeVariantWrap])
    typePredicate = (\_ -> True)

-- | A set of reserved words in Go
goReservedWords :: (S.Set String)
goReservedWords = (Sets.fromList (Lists.concat [
  goKeywords,
  goPredeclaredIdentifiers,
  hydraGoKeywords])) 
  where 
    goKeywords = [
      "break",
      "case",
      "chan",
      "const",
      "continue",
      "default",
      "defer",
      "else",
      "fallthrough",
      "for",
      "func",
      "go",
      "goto",
      "if",
      "import",
      "interface",
      "map",
      "package",
      "range",
      "return",
      "select",
      "struct",
      "switch",
      "type",
      "var"]
    goPredeclaredIdentifiers = [
      "any",
      "bool",
      "byte",
      "comparable",
      "complex64",
      "complex128",
      "error",
      "float32",
      "float64",
      "int",
      "int8",
      "int16",
      "int32",
      "int64",
      "rune",
      "string",
      "uint",
      "uint8",
      "uint16",
      "uint32",
      "uint64",
      "uintptr",
      "true",
      "false",
      "iota",
      "nil",
      "append",
      "cap",
      "clear",
      "close",
      "complex",
      "copy",
      "delete",
      "imag",
      "len",
      "make",
      "max",
      "min",
      "new",
      "panic",
      "print",
      "println",
      "real",
      "recover"]
    hydraGoKeywords = [
      "Node",
      "Maybe",
      "Either",
      "Left",
      "Right",
      "Just",
      "Nothing",
      "Unit",
      "Pair"]
