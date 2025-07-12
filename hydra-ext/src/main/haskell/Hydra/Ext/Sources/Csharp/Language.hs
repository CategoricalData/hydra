{-# LANGUAGE OverloadedStrings #-}

module Hydra.Ext.Sources.Csharp.Language where

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
import           Hydra.Dsl.Lib.Strings                      as Strings
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
import           Prelude hiding ((++))
import qualified Data.Int                                   as I
import qualified Data.List                                  as L
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified Data.Maybe                                 as Y


csharpLanguageDefinition :: String -> TTerm a -> TElement a
csharpLanguageDefinition = definitionInModule csharpLanguageModule

csharpLanguageModule :: Module
csharpLanguageModule = Module (Namespace "hydra.ext.csharp.language")
  [el csharpLanguageDef, el csharpReservedWordsDef]
  [Lexical.module_]
  KernelTypes.kernelTypesModules $
  Just "Language constraints and reserved words for C Sharp (C#)"

csharpLanguageDef :: TElement Language
csharpLanguageDef = csharpLanguageDefinition "csharpLanguage" $
  doc "Language constraints for C Sharp (C#)" $ lets [
  "eliminationVariants">: Sets.fromList $ list [ -- TODO: verify whether all are supported
    Mantle.eliminationVariantProduct,
    Mantle.eliminationVariantRecord,
    Mantle.eliminationVariantUnion,
    Mantle.eliminationVariantWrap],
  "literalVariants">: Sets.fromList $ list [
    Mantle.literalVariantBinary, -- byte[]
    Mantle.literalVariantBoolean, -- bool
    Mantle.literalVariantFloat, -- (see float types)
    Mantle.literalVariantInteger, -- (see integer types)
    Mantle.literalVariantString], -- String/string
  "floatTypes">: Sets.fromList $ list [
    -- See: https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/floating-point-numeric-types
    -- Note: the other C# floating point type, decimal, has no equivalent in Hydra
    Core.floatTypeFloat32,
    Core.floatTypeFloat64],
  "functionVariants">: Sets.fromList $ list [
    Mantle.functionVariantElimination,
    Mantle.functionVariantLambda,
    Mantle.functionVariantPrimitive],
  "integerTypes">: Sets.fromList $ list [
    -- See https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/integral-numeric-types
    -- Note: the other two C# integral types, nint and nuint, have no equivalents in Hydra
    Core.integerTypeInt8, -- sbyte
    Core.integerTypeInt16, -- short
    Core.integerTypeInt32, -- int
    Core.integerTypeInt64, -- long
    Core.integerTypeUint8, -- byte
    Core.integerTypeUint16, -- ushort
    Core.integerTypeUint32, -- uint
    Core.integerTypeUint64], -- ulong
  "termVariants">: Sets.fromList $ list [ -- TODO: verify whether all are supported
    Mantle.termVariantApplication,
    Mantle.termVariantFunction,
    Mantle.termVariantLet,
    Mantle.termVariantList,
    Mantle.termVariantLiteral,
    Mantle.termVariantMap,
    Mantle.termVariantOptional,
    Mantle.termVariantProduct,
    Mantle.termVariantRecord,
    Mantle.termVariantSet,
    Mantle.termVariantUnion,
    Mantle.termVariantVariable,
    Mantle.termVariantWrap],
  "typeVariants">: Sets.fromList $ list [ -- TODO: verify whether all are supported
    Mantle.typeVariantAnnotated,
    Mantle.typeVariantApplication,
    Mantle.typeVariantFunction,
    Mantle.typeVariantForall,
    Mantle.typeVariantList,
    Mantle.typeVariantLiteral,
    Mantle.typeVariantMap,
    Mantle.typeVariantOptional,
    Mantle.typeVariantProduct,
    Mantle.typeVariantRecord,
    Mantle.typeVariantSet,
    Mantle.typeVariantUnion,
    Mantle.typeVariantVariable,
    Mantle.typeVariantWrap],
  "typePredicate">: constant true] $ -- TODO: verify whether all are supported
  Coders.language
    (Coders.languageName $ string "hydra.ext.csharp")
    (Coders.languageConstraints
      (var "eliminationVariants")
      (var "literalVariants")
      (var "floatTypes")
      (var "functionVariants")
      (var "integerTypes")
      (var "termVariants")
      (var "typeVariants")
      (var "typePredicate"))

csharpReservedWordsDef :: TElement (S.Set String)
csharpReservedWordsDef = csharpLanguageDefinition "csharpReservedWords" $
  doc ("A set of reserved words in C#. Both the \"keywords\" and \"contextual keywords\" are drawn from"
    <> " section 6.4.4 of the C# documentation:\n"
    <> "https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/lexical-structure#64-tokens") $ lets [
  "keywords">: list $ string <$> [
      "DEFAULT", "FALSE", "NULL", "TRUE",
      "abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char", "checked", "class", "const",
      "continue", "decimal", "delegate", "do", "double", "else", "enum", "event", "explicit", "extern", "finally",
      "fixed", "float", "for", "foreach", "goto", "if", "implicit", "in", "int", "interface", "internal", "is",
      "lock", "long", "namespace", "new", "object", "operator", "out", "override", "params", "private", "protected",
      "public", "readonly", "ref", "return", "sbyte", "sealed", "short", "sizeof", "stackalloc", "static", "string",
      "struct", "switch", "this", "throw", "try", "typeof", "uint", "ulong", "unchecked", "unsafe", "ushort", "using",
      "virtual", "void", "volatile", "while"],
  "contextualKeywords">: list $ string <$> [
      "add", "alias", "ascending", "async", "await", "by", "descending", "dynamic", "equals", "from", "get", "global",
      "group", "into", "join", "let", "nameof", "on", "orderby", "partial", "remove", "select", "set", "unmanaged",
      "value", "var", "when", "where", "yield"]] $
  Sets.fromList $ Lists.concat $ list [
    var "keywords",
    var "contextualKeywords"]
