module Hydra.Ext.Scala.Language where

import Hydra.Core
import Hydra.Adapter
import Hydra.Basics

import qualified Data.Set as S


scalaLanguage :: Language m
scalaLanguage = Language (LanguageName "hydra/ext/scala") $ LanguageConstraints {
  languageConstraintsEliminationVariants = S.fromList eliminationVariants,
  languageConstraintsLiteralVariants = S.fromList [
    LiteralVariantBoolean,
    LiteralVariantFloat,
    LiteralVariantInteger,
    LiteralVariantString],
  languageConstraintsFloatTypes = S.fromList [
    -- Bigfloat is excluded for now
    FloatTypeFloat32,
    FloatTypeFloat64],
  languageConstraintsFunctionVariants = S.fromList functionVariants,
  languageConstraintsIntegerTypes = S.fromList [
    IntegerTypeBigint,
    IntegerTypeInt16,
    IntegerTypeInt32,
    IntegerTypeInt64,
    IntegerTypeUint8],
  languageConstraintsDataVariants = S.fromList [
    DataVariantApplication,
    DataVariantElement,
    DataVariantFunction,
    DataVariantList,
    DataVariantLiteral,
    DataVariantMap,
    DataVariantNominal,
    DataVariantOptional,
    DataVariantRecord,
    DataVariantSet,
    DataVariantUnion,
    DataVariantVariable],
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantElement,
    TypeVariantFunction,
    TypeVariantList,
    TypeVariantLiteral,
    TypeVariantMap,
    TypeVariantNominal,
    TypeVariantOptional,
    TypeVariantRecord,
    TypeVariantSet,
    TypeVariantUnion,
    TypeVariantUniversal,
    TypeVariantVariable],
  languageConstraintsTypes = const True }

scalaReservedWords :: S.Set [Char]
scalaReservedWords = S.fromList $ keywords ++ classNames
  where
    -- Classes in the Scala Standard Library 2.13.8
    -- Note: numbered class names like Function1, Product16, and the names of exception/error classes are omitted,
    --       as they are unlikely to occur by chance.
    classNames = [
      "Any", "AnyVal", "App", "Array", "Boolean", "Byte", "Char", "Console", "DelayedInit", "Double", "DummyExplicit",
      "Dynamic", "Enumeration", "Equals", "Float", "Function", "Int", "Long", "MatchError", "None",
      "Nothing", "Null", "Option", "PartialFunction", "Predef", "Product", "Proxy",
      "SerialVersionUID", "Short", "Singleton", "Some", "Specializable", "StringContext",
      "Symbol", "Unit", "ValueOf"]
    -- Not an official or comprehensive list; taken from https://www.geeksforgeeks.org/scala-keywords
    keywords = [
      "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final", "finally", "for",
      "forSome", "if", "implicit", "import", "lazy", "match", "new", "null", "object", "override", "package", "private",
      "protected", "return", "sealed", "super", "this", "throw", "trait", "true", "try", "type", "val", "var", "while",
      "with", "yield"]