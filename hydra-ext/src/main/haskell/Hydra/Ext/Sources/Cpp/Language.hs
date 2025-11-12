-- Note: this file was created with the help of a large language model. It requires further human review.

{-# LANGUAGE OverloadedStrings #-}

module Hydra.Ext.Sources.Cpp.Language where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta          as Meta
import qualified Hydra.Dsl.Module        as Module
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Testing       as Testing
import qualified Hydra.Dsl.TBase         as TBase
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import qualified Hydra.Dsl.Util          as Util
import qualified Hydra.Sources.Kernel.Terms.All             as KernelTerms
import qualified Hydra.Sources.Kernel.Types.All             as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals  as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules   as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple    as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms     as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils     as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations     as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking        as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Decode.Core     as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Describe.Core   as DescribeCore
import qualified Hydra.Sources.Kernel.Terms.Describe.Util   as DescribeUtil
import qualified Hydra.Sources.Kernel.Terms.Encode.Core     as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Core    as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util    as ExtractUtil
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
import qualified Hydra.Sources.Kernel.Terms.Show.Meta       as ShowMeta
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


cppLanguageDefinition :: String -> TTerm a -> TBinding a
cppLanguageDefinition = definitionInModule cppLanguageModule

cppLanguageModule :: Module
cppLanguageModule = Module (Namespace "hydra.ext.cpp.language")
  [el cppLanguageDef, el cppReservedWordsDef]
  [Lexical.module_]
  KernelTypes.kernelTypesModules $
  Just "Language constraints and reserved words for C++"

cppLanguageDef :: TBinding Language
cppLanguageDef = cppLanguageDefinition "cppLanguage" $
  doc "Language constraints for C++" $ lets [
  "eliminationVariants">: Sets.fromList $ list [
    Meta.eliminationVariantProduct,
    Meta.eliminationVariantRecord,
    Meta.eliminationVariantUnion,
    Meta.eliminationVariantWrap],
  "literalVariants">: Sets.fromList $ list [
    Meta.literalVariantBinary,  -- char arrays, std::byte arrays
    Meta.literalVariantBoolean, -- bool
    Meta.literalVariantFloat,   -- float, double
    Meta.literalVariantInteger, -- int, long, etc.
    Meta.literalVariantString], -- std::string
  "floatTypes">: Sets.fromList $ list [
    Core.floatTypeFloat32,      -- float
    Core.floatTypeFloat64],     -- double
  "functionVariants">: Sets.fromList $ list [
    Meta.functionVariantElimination,
    Meta.functionVariantLambda,
    Meta.functionVariantPrimitive],
  "integerTypes">: Sets.fromList $ list [
    Core.integerTypeInt8,       -- char, int8_t
    Core.integerTypeInt16,      -- short, int16_t
    Core.integerTypeInt32,      -- int, int32_t
    Core.integerTypeInt64,      -- long, long long, int64_t
    Core.integerTypeBigint],    -- custom big integer implementation
  "termVariants">: Sets.fromList $ list [
    Meta.termVariantApplication,
    Meta.termVariantFunction,
    Meta.termVariantLet,
    Meta.termVariantList,       -- std::vector
    Meta.termVariantLiteral,
    Meta.termVariantMap,        -- std::map
    Meta.termVariantMaybe,   -- std::optional
    Meta.termVariantProduct,    -- struct with unnamed fields
    Meta.termVariantRecord,     -- struct with named fields
    Meta.termVariantSet,        -- std::set
    Meta.termVariantUnion,      -- std::variant or enum
    Meta.termVariantVariable,
    Meta.termVariantWrap],      -- wrapper class
  "typeVariants">: Sets.fromList $ list [
    Meta.typeVariantApplication, -- template instantiation
    Meta.typeVariantFunction,    -- function types
    Meta.typeVariantForall,      -- templates
    Meta.typeVariantList,        -- std::vector
    Meta.typeVariantLiteral,     -- primitive types
    Meta.typeVariantMap,         -- std::map
    Meta.typeVariantMaybe,    -- std::optional
    Meta.typeVariantProduct,     -- anonymous structs
    Meta.typeVariantRecord,      -- structs
    Meta.typeVariantSet,         -- std::set
    Meta.typeVariantUnion,       -- std::variant, enum
    Meta.typeVariantVariable,    -- type parameters
    Meta.typeVariantWrap],       -- wrapper class
  "typePredicate">: constant true] $ -- TODO: refine this with C++ specific constraints
  Coders.language
    (Coders.languageName $ string "hydra.ext.cpp")
    (Coders.languageConstraints
      (var "eliminationVariants")
      (var "literalVariants")
      (var "floatTypes")
      (var "functionVariants")
      (var "integerTypes")
      (var "termVariants")
      (var "typeVariants")
      (var "typePredicate"))

cppReservedWordsDef :: TBinding (S.Set String)
cppReservedWordsDef = cppLanguageDefinition "cppReservedWords" $
  doc "A set of reserved words in C++" $ lets [
  "cppKeywords">:
    doc "C++ keywords, including C++11/14/17/20 additions" $
    list $ string <$> [
      -- C++ keywords
      "alignas", "alignof", "and", "and_eq", "asm", "auto", "bitand", "bitor", "bool", "break",
      "case", "catch", "char", "char8_t", "char16_t", "char32_t", "class", "compl", "concept", "const",
      "consteval", "constexpr", "constinit", "const_cast", "continue", "co_await", "co_return", "co_yield",
      "decltype", "default", "delete", "do", "double", "dynamic_cast", "else", "enum", "explicit", "export",
      "extern", "false", "float", "for", "friend", "goto", "if", "inline", "int", "long", "mutable",
      "namespace", "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq", "private",
      "protected", "public", "register", "reinterpret_cast", "requires", "return", "short", "signed",
      "sizeof", "static", "static_assert", "static_cast", "struct", "switch", "template", "this",
      "thread_local", "throw", "true", "try", "typedef", "typeid", "typename", "union", "unsigned", "using",
      "virtual", "void", "volatile", "wchar_t", "while", "xor", "xor_eq"],
  "cppPreprocessor">:
    doc "C++ preprocessor directives" $
    list $ string <$> [
      "#define", "#elif", "#else", "#endif", "#error", "#if", "#ifdef", "#ifndef", "#include", "#line",
      "#pragma", "#undef"],
  "cppStlTypes">:
    doc "Common STL types and namespaces that should be treated as reserved" $
    list $ string <$> [
      "std", "string", "vector", "map", "set", "optional", "variant", "pair", "tuple",
      "function", "array", "deque", "forward_list", "list", "multimap", "multiset",
      "unordered_map", "unordered_set", "unordered_multimap", "unordered_multiset",
      "stack", "queue", "priority_queue", "shared_ptr", "unique_ptr", "weak_ptr"],
  "hydraCppKeywords">:
    doc "Reserved words which are specific to Hydra" $
    list $ string <$> []] $
  Sets.fromList $ Lists.concat $ list [
    var "cppKeywords",
    var "cppPreprocessor",
    var "cppStlTypes",
    var "hydraCppKeywords"]
