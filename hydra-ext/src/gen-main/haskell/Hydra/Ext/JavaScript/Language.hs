-- Note: this is an automatically generated file. Do not edit.

-- | Language constraints and reserved words for JavaScript (ECMAScript 2024)

module Hydra.Ext.JavaScript.Language where

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

-- | Language constraints for JavaScript (ECMAScript 2024)
javaScriptLanguage :: Coders.Language
javaScriptLanguage = Coders.Language {
  Coders.languageName = (Coders.LanguageName "hydra.ext.javaScript"),
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
      Core.FloatTypeFloat64])
    functionVariants = (Sets.fromList [
      Variants.FunctionVariantElimination,
      Variants.FunctionVariantLambda,
      Variants.FunctionVariantPrimitive])
    integerTypes = (Sets.fromList [
      Core.IntegerTypeInt32,
      Core.IntegerTypeBigint])
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

-- | A set of reserved words in JavaScript
javaScriptReservedWords :: (S.Set String)
javaScriptReservedWords = (Sets.fromList (Lists.concat [
  keywords,
  futureReserved,
  strictModeReserved,
  builtIns,
  hydraJavaScriptKeywords])) 
  where 
    keywords = [
      "await",
      "break",
      "case",
      "catch",
      "class",
      "const",
      "continue",
      "debugger",
      "default",
      "delete",
      "do",
      "else",
      "enum",
      "export",
      "extends",
      "false",
      "finally",
      "for",
      "function",
      "if",
      "import",
      "in",
      "instanceof",
      "let",
      "new",
      "null",
      "return",
      "super",
      "switch",
      "this",
      "throw",
      "true",
      "try",
      "typeof",
      "undefined",
      "var",
      "void",
      "while",
      "with",
      "yield"]
    futureReserved = [
      "implements",
      "interface",
      "package",
      "private",
      "protected",
      "public"]
    strictModeReserved = [
      "arguments",
      "eval"]
    builtIns = [
      "Array",
      "ArrayBuffer",
      "BigInt",
      "Boolean",
      "DataView",
      "Date",
      "Error",
      "Float32Array",
      "Float64Array",
      "Function",
      "Int8Array",
      "Int16Array",
      "Int32Array",
      "JSON",
      "Map",
      "Math",
      "Number",
      "Object",
      "Promise",
      "Proxy",
      "Reflect",
      "RegExp",
      "Set",
      "String",
      "Symbol",
      "Uint8Array",
      "Uint8ClampedArray",
      "Uint16Array",
      "Uint32Array",
      "WeakMap",
      "WeakSet",
      "decodeURI",
      "decodeURIComponent",
      "encodeURI",
      "encodeURIComponent",
      "eval",
      "isFinite",
      "isNaN",
      "parseFloat",
      "parseInt",
      "console",
      "document",
      "global",
      "globalThis",
      "module",
      "process",
      "require",
      "window"]
    hydraJavaScriptKeywords = [
      "Name",
      "FrozenMap",
      "TERM_ANNOTATED",
      "TERM_APPLICATION",
      "TERM_EITHER",
      "TERM_FUNCTION",
      "TERM_LET",
      "TERM_LIST",
      "TERM_LITERAL",
      "TERM_MAP",
      "TERM_MAYBE",
      "TERM_PAIR",
      "TERM_RECORD",
      "TERM_SET",
      "TERM_TYPE_APPLICATION",
      "TERM_TYPE_LAMBDA",
      "TERM_UNION",
      "TERM_UNIT",
      "TERM_VARIABLE",
      "TERM_WRAP"]
