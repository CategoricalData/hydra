-- | Language constraints and reserved words for C Sharp (C#)

module Hydra.Ext.Csharp.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Meta as Meta
import qualified Hydra.Util as Util
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
      Meta.EliminationVariantProduct,
      Meta.EliminationVariantRecord,
      Meta.EliminationVariantUnion,
      Meta.EliminationVariantWrap])
    literalVariants = (Sets.fromList [
      Meta.LiteralVariantBinary,
      Meta.LiteralVariantBoolean,
      Meta.LiteralVariantFloat,
      Meta.LiteralVariantInteger,
      Meta.LiteralVariantString])
    floatTypes = (Sets.fromList [
      Core.FloatTypeFloat32,
      Core.FloatTypeFloat64])
    functionVariants = (Sets.fromList [
      Meta.FunctionVariantElimination,
      Meta.FunctionVariantLambda,
      Meta.FunctionVariantPrimitive])
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
      Meta.TermVariantApplication,
      Meta.TermVariantFunction,
      Meta.TermVariantLet,
      Meta.TermVariantList,
      Meta.TermVariantLiteral,
      Meta.TermVariantMap,
      Meta.TermVariantMaybe,
      Meta.TermVariantProduct,
      Meta.TermVariantRecord,
      Meta.TermVariantSet,
      Meta.TermVariantUnion,
      Meta.TermVariantVariable,
      Meta.TermVariantWrap])
    typeVariants = (Sets.fromList [
      Meta.TypeVariantAnnotated,
      Meta.TypeVariantApplication,
      Meta.TypeVariantFunction,
      Meta.TypeVariantForall,
      Meta.TypeVariantList,
      Meta.TypeVariantLiteral,
      Meta.TypeVariantMap,
      Meta.TypeVariantMaybe,
      Meta.TypeVariantProduct,
      Meta.TypeVariantRecord,
      Meta.TypeVariantSet,
      Meta.TypeVariantUnion,
      Meta.TypeVariantVariable,
      Meta.TypeVariantWrap])
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
