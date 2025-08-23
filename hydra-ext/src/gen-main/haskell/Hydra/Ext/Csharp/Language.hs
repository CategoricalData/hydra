-- | Language constraints and reserved words for C Sharp (C#)

module Hydra.Ext.Csharp.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Mantle as Mantle
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Language constraints for C Sharp (C#)
csharpLanguage :: Coders.Language
csharpLanguage = Coders.Language {
  Coders.languageName = (Coders.LanguageName "hydra.ext.csharp"),
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
      Mantle.EliminationVariantProduct,
      Mantle.EliminationVariantRecord,
      Mantle.EliminationVariantUnion,
      Mantle.EliminationVariantWrap])
    literalVariants = (Sets.fromList [
      Mantle.LiteralVariantBinary,
      Mantle.LiteralVariantBoolean,
      Mantle.LiteralVariantFloat,
      Mantle.LiteralVariantInteger,
      Mantle.LiteralVariantString])
    floatTypes = (Sets.fromList [
      Core.FloatTypeFloat32,
      Core.FloatTypeFloat64])
    functionVariants = (Sets.fromList [
      Mantle.FunctionVariantElimination,
      Mantle.FunctionVariantLambda,
      Mantle.FunctionVariantPrimitive])
    integerTypes = (Sets.fromList [
      Core.IntegerTypeInt8,
      Core.IntegerTypeInt16,
      Core.IntegerTypeInt32,
      Core.IntegerTypeInt64,
      Core.IntegerTypeUint8,
      Core.IntegerTypeUint16,
      Core.IntegerTypeUint32,
      Core.IntegerTypeUint64])
    termVariants = (Sets.fromList [
      Mantle.TermVariantApplication,
      Mantle.TermVariantFunction,
      Mantle.TermVariantLet,
      Mantle.TermVariantList,
      Mantle.TermVariantLiteral,
      Mantle.TermVariantMap,
      Mantle.TermVariantOptional,
      Mantle.TermVariantProduct,
      Mantle.TermVariantRecord,
      Mantle.TermVariantSet,
      Mantle.TermVariantUnion,
      Mantle.TermVariantVariable,
      Mantle.TermVariantWrap])
    typeVariants = (Sets.fromList [
      Mantle.TypeVariantAnnotated,
      Mantle.TypeVariantApplication,
      Mantle.TypeVariantFunction,
      Mantle.TypeVariantForall,
      Mantle.TypeVariantList,
      Mantle.TypeVariantLiteral,
      Mantle.TypeVariantMap,
      Mantle.TypeVariantOptional,
      Mantle.TypeVariantProduct,
      Mantle.TypeVariantRecord,
      Mantle.TypeVariantSet,
      Mantle.TypeVariantUnion,
      Mantle.TypeVariantVariable,
      Mantle.TypeVariantWrap])
    typePredicate = (\_ -> True)

-- | A set of reserved words in C#. Both the "keywords" and "contextual keywords" are drawn from section 6.4.4 of the C# documentation:
-- | https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/lexical-structure#64-tokens
csharpReservedWords :: (S.Set String)
csharpReservedWords = (Sets.fromList (Lists.concat [
  keywords,
  contextualKeywords])) 
  where 
    keywords = [
      "DEFAULT",
      "FALSE",
      "NULL",
      "TRUE",
      "abstract",
      "as",
      "base",
      "bool",
      "break",
      "byte",
      "case",
      "catch",
      "char",
      "checked",
      "class",
      "const",
      "continue",
      "decimal",
      "delegate",
      "do",
      "double",
      "else",
      "enum",
      "event",
      "explicit",
      "extern",
      "finally",
      "fixed",
      "float",
      "for",
      "foreach",
      "goto",
      "if",
      "implicit",
      "in",
      "int",
      "interface",
      "internal",
      "is",
      "lock",
      "long",
      "namespace",
      "new",
      "object",
      "operator",
      "out",
      "override",
      "params",
      "private",
      "protected",
      "public",
      "readonly",
      "ref",
      "return",
      "sbyte",
      "sealed",
      "short",
      "sizeof",
      "stackalloc",
      "static",
      "string",
      "struct",
      "switch",
      "this",
      "throw",
      "try",
      "typeof",
      "uint",
      "ulong",
      "unchecked",
      "unsafe",
      "ushort",
      "using",
      "virtual",
      "void",
      "volatile",
      "while"]
    contextualKeywords = [
      "add",
      "alias",
      "ascending",
      "async",
      "await",
      "by",
      "descending",
      "dynamic",
      "equals",
      "from",
      "get",
      "global",
      "group",
      "into",
      "join",
      "let",
      "nameof",
      "on",
      "orderby",
      "partial",
      "remove",
      "select",
      "set",
      "unmanaged",
      "value",
      "var",
      "when",
      "where",
      "yield"]
