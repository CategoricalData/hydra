{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Ext.Csharp.Language where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors                        as Accessors
import qualified Hydra.Dsl.Annotations                      as Anns
import qualified Hydra.Dsl.Ast                              as Ast
import qualified Hydra.Dsl.Coders                           as Coders
import qualified Hydra.Dsl.Compute                          as Compute
import qualified Hydra.Dsl.Core                             as Core
import qualified Hydra.Dsl.Graph                            as Graph
import qualified Hydra.Dsl.Grammar                          as Grammar
import qualified Hydra.Dsl.Lib.Chars                        as Chars
import qualified Hydra.Dsl.Lib.Equality                     as Equality
import qualified Hydra.Dsl.Lib.Flows                        as Flows
import qualified Hydra.Dsl.Lib.Lists                        as Lists
import qualified Hydra.Dsl.Lib.Literals                     as Literals
import qualified Hydra.Dsl.Lib.Logic                        as Logic
import qualified Hydra.Dsl.Lib.Maps                         as Maps
import qualified Hydra.Dsl.Lib.Math                         as Math
import qualified Hydra.Dsl.Lib.Optionals                    as Optionals
import qualified Hydra.Dsl.Lib.Sets                         as Sets
import qualified Hydra.Dsl.Lib.Strings                      as Strings
import qualified Hydra.Dsl.Mantle                           as Mantle
import qualified Hydra.Dsl.Module                           as Module
import           Hydra.Dsl.Phantoms                         as Phantoms
import qualified Hydra.Dsl.TTerms                           as TTerms
import qualified Hydra.Dsl.TTypes                           as TTypes
import qualified Hydra.Dsl.Tabular                          as Tabular
import qualified Hydra.Dsl.Terms                            as Terms
import qualified Hydra.Dsl.Topology                         as Topology
import qualified Hydra.Dsl.Types                            as Types
import qualified Hydra.Dsl.Typing                           as Typing
import qualified Hydra.Sources.Kernel.Types.All             as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals  as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules   as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms     as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils     as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations     as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Decode.Core     as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Decoding        as Decoding
import qualified Hydra.Sources.Kernel.Terms.Describe.Core   as DescribeCore
import qualified Hydra.Sources.Kernel.Terms.Describe.Mantle as DescribeMantle
import qualified Hydra.Sources.Kernel.Terms.Encode.Core     as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Core    as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Mantle  as ExtractMantle
import qualified Hydra.Sources.Kernel.Terms.Formatting      as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars        as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference       as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages       as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical         as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals        as Literals
import qualified Hydra.Sources.Kernel.Terms.Monads          as Monads
import qualified Hydra.Sources.Kernel.Terms.Names           as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction       as Reduction
import qualified Hydra.Sources.Kernel.Terms.Rewriting       as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas         as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization   as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors  as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core       as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph      as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Mantle     as ShowMantle
import qualified Hydra.Sources.Kernel.Terms.Show.Typing     as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting         as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution    as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan          as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates       as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification     as Unification
import qualified Hydra.Sources.Kernel.Terms.Variants        as Variants
import qualified Data.Int                                   as I
import qualified Data.List                                  as L
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified Data.Maybe                                 as Y

csharpLanguageDefinition :: String -> TTerm a -> TElement a
csharpLanguageDefinition = definitionInModule csharpLanguageModule

csharpLanguageModule :: Module
csharpLanguageModule = Module ns elements
    [Lexical.module_]
    KernelTypes.kernelTypesModules $
    Just "Language constraints and reserved words for C Sharp (C#)"
  where
    ns = Namespace "hydra.ext.csharp.language"
    elements = [
      el csharpLanguageDef,
      el csharpReservedWordsDef]

csharpLanguageDef :: TElement Language
csharpLanguageDef = csharpLanguageDefinition "csharpLanguage" $
    doc "Language constraints for C Sharp (C#)" $
    Coders.language "hydra.ext.csharp"
      eliminationVariants
      literalVariants
      floatTypes
      functionVariants
      integerTypes
      termVariants
      typeVariants
      typePredicate
  where
      eliminationVariants = [ -- TODO: verify whether all are supported
        EliminationVariantProduct,
        EliminationVariantRecord,
        EliminationVariantUnion,
        EliminationVariantWrap]
      literalVariants = [
        LiteralVariantBinary, -- byte[]
        LiteralVariantBoolean, -- bool
        LiteralVariantFloat, -- (see float types)
        LiteralVariantInteger, -- (see integer types)
        LiteralVariantString] -- String/string
      -- See: https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/floating-point-numeric-types
      -- Note: the other C# floating point type, decimal, has no equivalent in Hydra
      floatTypes = [
        FloatTypeFloat32,
        FloatTypeFloat64]
      functionVariants = [
        FunctionVariantElimination,
        FunctionVariantLambda,
        FunctionVariantPrimitive]
      -- See https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/integral-numeric-types
      -- Note: the other two C# integral types, nint and nuint, have no equivalents in Hydra
      integerTypes = [
        IntegerTypeInt8, -- sbyte
        IntegerTypeInt16, -- short
        IntegerTypeInt32, -- int
        IntegerTypeInt64, -- long
        IntegerTypeUint8, -- byte
        IntegerTypeUint16, -- ushort
        IntegerTypeUint32, -- uint
        IntegerTypeUint64] -- ulong
      termVariants = [ -- TODO: verify whether all are supported
        TermVariantApplication,
        TermVariantFunction,
        TermVariantLet,
        TermVariantList,
        TermVariantLiteral,
        TermVariantMap,
        TermVariantOptional,
        TermVariantProduct,
        TermVariantRecord,
        TermVariantSet,
        TermVariantUnion,
        TermVariantVariable,
        TermVariantWrap]
      typeVariants = [ -- TODO: verify whether all are supported
        TypeVariantAnnotated,
        TypeVariantApplication,
        TypeVariantFunction,
        TypeVariantForall,
        TypeVariantList,
        TypeVariantLiteral,
        TypeVariantMap,
        TypeVariantOptional,
        TypeVariantProduct,
        TypeVariantRecord,
        TypeVariantSet,
        TypeVariantUnion,
        TypeVariantVariable,
        TypeVariantWrap]
      typePredicate = constant true -- TODO: verify whether all are supported

csharpReservedWordsDef :: TElement (S.Set String)
csharpReservedWordsDef = csharpLanguageDefinition "csharpReservedWords" $
  doc ("A set of reserved words in C#. Both the \"keywords\" and \"contextual keywords\" are drawn from"
    <> " section 6.4.4 of the C# documentation:\n"
    <> "https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/lexical-structure#64-tokens") $
  lets [
    "keywords">: list [
        "DEFAULT", "FALSE", "NULL", "TRUE",
        "abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char", "checked", "class", "const",
        "continue", "decimal", "delegate", "do", "double", "else", "enum", "event", "explicit", "extern", "finally",
        "fixed", "float", "for", "foreach", "goto", "if", "implicit", "in", "int", "interface", "internal", "is",
        "lock", "long", "namespace", "new", "object", "operator", "out", "override", "params", "private", "protected",
        "public", "readonly", "ref", "return", "sbyte", "sealed", "short", "sizeof", "stackalloc", "static", "string",
        "struct", "switch", "this", "throw", "try", "typeof", "uint", "ulong", "unchecked", "unsafe", "ushort", "using",
        "virtual", "void", "volatile", "while"],
    "contextualKeywords">: list [
        "add", "alias", "ascending", "async", "await", "by", "descending", "dynamic", "equals", "from", "get", "global",
        "group", "into", "join", "let", "nameof", "on", "orderby", "partial", "remove", "select", "set", "unmanaged",
        "value", "var", "when", "where", "yield"]]
    $ Sets.fromList $ Lists.concat $ list [var "keywords", var "contextualKeywords"]
