-- Note: this is an automatically generated file. Do not edit.

-- | Language constraints and reserved words for C++

module Hydra.Ext.Cpp.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Language constraints for C++
cppLanguage :: Coders.Language
cppLanguage = Coders.Language {
  Coders.languageName = (Coders.LanguageName "hydra.ext.cpp"),
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
      Core.IntegerTypeInt8,
      Core.IntegerTypeInt16,
      Core.IntegerTypeInt32,
      Core.IntegerTypeInt64,
      Core.IntegerTypeBigint])
    termVariants = (Sets.fromList [
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
      Variants.TermVariantUnion,
      Variants.TermVariantVariable,
      Variants.TermVariantWrap])
    typeVariants = (Sets.fromList [
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
      Variants.TypeVariantVariable,
      Variants.TypeVariantWrap])
    typePredicate = (\_ -> True)

-- | A set of reserved words in C++
cppReservedWords :: (S.Set String)
cppReservedWords = (Sets.fromList (Lists.concat [
  cppKeywords,
  cppPreprocessor,
  cppStlTypes,
  hydraCppKeywords])) 
  where 
    cppKeywords = [
      "alignas",
      "alignof",
      "and",
      "and_eq",
      "asm",
      "auto",
      "bitand",
      "bitor",
      "bool",
      "break",
      "case",
      "catch",
      "char",
      "char8_t",
      "char16_t",
      "char32_t",
      "class",
      "compl",
      "concept",
      "const",
      "consteval",
      "constexpr",
      "constinit",
      "const_cast",
      "continue",
      "co_await",
      "co_return",
      "co_yield",
      "decltype",
      "default",
      "delete",
      "do",
      "double",
      "dynamic_cast",
      "else",
      "enum",
      "explicit",
      "export",
      "extern",
      "false",
      "float",
      "for",
      "friend",
      "goto",
      "if",
      "inline",
      "int",
      "long",
      "mutable",
      "namespace",
      "new",
      "noexcept",
      "not",
      "not_eq",
      "nullptr",
      "operator",
      "or",
      "or_eq",
      "private",
      "protected",
      "public",
      "register",
      "reinterpret_cast",
      "requires",
      "return",
      "short",
      "signed",
      "sizeof",
      "static",
      "static_assert",
      "static_cast",
      "struct",
      "switch",
      "template",
      "this",
      "thread_local",
      "throw",
      "true",
      "try",
      "typedef",
      "typeid",
      "typename",
      "union",
      "unsigned",
      "using",
      "virtual",
      "void",
      "volatile",
      "wchar_t",
      "while",
      "xor",
      "xor_eq"]
    cppPreprocessor = [
      "#define",
      "#elif",
      "#else",
      "#endif",
      "#error",
      "#if",
      "#ifdef",
      "#ifndef",
      "#include",
      "#line",
      "#pragma",
      "#undef"]
    cppStlTypes = [
      "std",
      "string",
      "vector",
      "map",
      "set",
      "optional",
      "variant",
      "pair",
      "tuple",
      "function",
      "array",
      "deque",
      "forward_list",
      "list",
      "multimap",
      "multiset",
      "unordered_map",
      "unordered_set",
      "unordered_multimap",
      "unordered_multiset",
      "stack",
      "queue",
      "priority_queue",
      "shared_ptr",
      "unique_ptr",
      "weak_ptr"]
    hydraCppKeywords = []
