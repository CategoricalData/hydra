-- Note: this is an automatically generated file. Do not edit.
-- | Language constraints and reserved words for C Sharp (C#)

module Hydra.Csharp.Language where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
-- | Language constraints for C Sharp (C#)
csharpLanguage :: Coders.Language
csharpLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.csharp"),
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
    floatTypes =
        Sets.fromList [
          Core.FloatTypeFloat32,
          Core.FloatTypeFloat64]
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
          Variants.TermVariantApplication,
          Variants.TermVariantCases,
          Variants.TermVariantLambda,
          Variants.TermVariantProject,
          Variants.TermVariantUnwrap,
          Variants.TermVariantTypeApplication,
          Variants.TermVariantTypeLambda,
          Variants.TermVariantLet,
          Variants.TermVariantList,
          Variants.TermVariantLiteral,
          Variants.TermVariantMap,
          Variants.TermVariantOptional,
          Variants.TermVariantRecord,
          Variants.TermVariantSet,
          Variants.TermVariantInject,
          Variants.TermVariantVariable,
          Variants.TermVariantWrap]
    typeVariants =
        Sets.fromList [
          Variants.TypeVariantAnnotated,
          Variants.TypeVariantApplication,
          Variants.TypeVariantFunction,
          Variants.TypeVariantForall,
          Variants.TypeVariantList,
          Variants.TypeVariantLiteral,
          Variants.TypeVariantMap,
          Variants.TypeVariantOptional,
          Variants.TypeVariantRecord,
          Variants.TypeVariantSet,
          Variants.TypeVariantUnion,
          Variants.TypeVariantVariable,
          Variants.TypeVariantWrap]
    typePredicate = \_ -> True
-- | A set of reserved words in C#. Both the "keywords" and "contextual keywords" are drawn from section 6.4.4 of the C# documentation:
-- | https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/lexical-structure#64-tokens
csharpReservedWords :: S.Set String
csharpReservedWords =
    Sets.fromList (Lists.concat [
      keywords,
      contextualKeywords])
  where
    keywords =
        [
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
    contextualKeywords =
        [
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
