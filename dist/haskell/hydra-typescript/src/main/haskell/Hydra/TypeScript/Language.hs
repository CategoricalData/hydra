-- Note: this is an automatically generated file. Do not edit.
-- | Language constraints and reserved words for TypeScript 5.x (ECMAScript 2024 base)

module Hydra.TypeScript.Language where
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
-- | Language constraints for TypeScript 5.x
typeScriptLanguage :: Coders.Language
typeScriptLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.typeScript"),
      Coders.languageConstraints = Coders.LanguageConstraints {
        Coders.languageConstraintsLiteralVariants = literalVariants,
        Coders.languageConstraintsFloatTypes = floatTypes,
        Coders.languageConstraintsIntegerTypes = integerTypes,
        Coders.languageConstraintsTermVariants = termVariants,
        Coders.languageConstraintsTypeVariants = typeVariants,
        Coders.languageConstraintsTypes = typePredicate}}
  where
    literalVariants =
        Sets.fromList [
          Variants.LiteralVariantBinary,
          Variants.LiteralVariantBoolean,
          Variants.LiteralVariantFloat,
          Variants.LiteralVariantInteger,
          Variants.LiteralVariantString]
    floatTypes = Sets.fromList [
      Core.FloatTypeFloat64]
    integerTypes =
        Sets.fromList [
          Core.IntegerTypeInt32,
          Core.IntegerTypeBigint]
    termVariants =
        Sets.fromList [
          Variants.TermVariantAnnotated,
          Variants.TermVariantApplication,
          Variants.TermVariantEither,
          Variants.TermVariantCases,
          Variants.TermVariantLambda,
          Variants.TermVariantProject,
          Variants.TermVariantUnwrap,
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
          Variants.TermVariantInject,
          Variants.TermVariantUnit,
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
-- | A set of reserved words in TypeScript
typeScriptReservedWords :: S.Set String
typeScriptReservedWords =
    Sets.fromList (Lists.concat [
      keywords,
      futureReserved,
      strictModeReserved,
      typeScriptKeywords,
      builtIns,
      hydraTypeScriptKeywords])
  where
    keywords =
        [
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
    futureReserved =
        [
          "implements",
          "interface",
          "package",
          "private",
          "protected",
          "public"]
    strictModeReserved =
        [
          "arguments",
          "eval",
          "static",
          "yield"]
    typeScriptKeywords =
        [
          "abstract",
          "as",
          "asserts",
          "assert",
          "any",
          "boolean",
          "constructor",
          "declare",
          "from",
          "get",
          "global",
          "infer",
          "intrinsic",
          "is",
          "keyof",
          "module",
          "namespace",
          "never",
          "object",
          "of",
          "out",
          "override",
          "readonly",
          "require",
          "satisfies",
          "set",
          "string",
          "symbol",
          "type",
          "unique",
          "unknown",
          "using"]
    builtIns =
        [
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
    hydraTypeScriptKeywords =
        [
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
