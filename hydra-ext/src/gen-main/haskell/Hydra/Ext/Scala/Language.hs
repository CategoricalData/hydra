-- Note: this is an automatically generated file. Do not edit.

-- | Language constraints and reserved words for Scala

module Hydra.Ext.Scala.Language where

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

-- | Language constraints for Scala
scalaLanguage :: Coders.Language
scalaLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.ext.scala"),
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
    floatTypes =
        Sets.fromList [
          Core.FloatTypeFloat32,
          Core.FloatTypeFloat64]
    functionVariants =
        Sets.fromList [
          Variants.FunctionVariantElimination,
          Variants.FunctionVariantLambda,
          Variants.FunctionVariantPrimitive]
    integerTypes =
        Sets.fromList [
          Core.IntegerTypeBigint,
          Core.IntegerTypeInt16,
          Core.IntegerTypeInt32,
          Core.IntegerTypeInt64,
          Core.IntegerTypeUint8]
    termVariants =
        Sets.fromList [
          Variants.TermVariantApplication,
          Variants.TermVariantEither,
          Variants.TermVariantFunction,
          Variants.TermVariantList,
          Variants.TermVariantLiteral,
          Variants.TermVariantMap,
          Variants.TermVariantMaybe,
          Variants.TermVariantPair,
          Variants.TermVariantRecord,
          Variants.TermVariantSet,
          Variants.TermVariantUnion,
          Variants.TermVariantVariable,
          Variants.TermVariantWrap]
    typeVariants =
        Sets.fromList [
          Variants.TypeVariantAnnotated,
          Variants.TypeVariantEither,
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
          Variants.TypeVariantForall,
          Variants.TypeVariantVariable,
          Variants.TypeVariantWrap]
    typePredicate = \_ -> True

-- | A set of reserved words in Scala
scalaReservedWords :: S.Set String
scalaReservedWords =
    Sets.fromList (Lists.concat [
      keywords,
      classNames,
      hydraScalaKeywords]) 
  where 
    keywords =
        [
          "abstract",
          "case",
          "catch",
          "class",
          "def",
          "do",
          "else",
          "extends",
          "false",
          "final",
          "finally",
          "for",
          "forSome",
          "if",
          "implicit",
          "import",
          "lazy",
          "match",
          "new",
          "null",
          "object",
          "override",
          "package",
          "private",
          "protected",
          "return",
          "sealed",
          "super",
          "this",
          "throw",
          "trait",
          "true",
          "try",
          "type",
          "val",
          "var",
          "while",
          "with",
          "yield"]
    classNames =
        [
          "Any",
          "AnyVal",
          "App",
          "Array",
          "Boolean",
          "Byte",
          "Char",
          "Console",
          "DelayedInit",
          "Double",
          "DummyExplicit",
          "Dynamic",
          "Enumeration",
          "Equals",
          "Float",
          "Function",
          "Int",
          "Long",
          "MatchError",
          "None",
          "Nothing",
          "Null",
          "Option",
          "PartialFunction",
          "Predef",
          "Product",
          "Proxy",
          "SerialVersionUID",
          "Short",
          "Singleton",
          "Some",
          "Specializable",
          "StringContext",
          "Symbol",
          "Unit",
          "ValueOf"]
    hydraScalaKeywords = []
