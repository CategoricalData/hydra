-- Note: this file was created with the help of a large language model. It requires further human review.

{-# LANGUAGE OverloadedStrings #-}

module Hydra.Ext.Sources.Cpp.Language where

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


cppLanguageDefinition :: String -> TTerm a -> TElement a
cppLanguageDefinition = definitionInModule cppLanguageModule

cppLanguageModule :: Module
cppLanguageModule = Module ns elements
    [Lexical.module_]
    KernelTypes.kernelTypesModules $
    Just "Language constraints and reserved words for C++"
  where
    ns = Namespace "hydra.ext.cpp.language"
    elements = [
      el cppLanguageDef,
      el cppReservedWordsDef]

cppLanguageDef :: TElement Language
cppLanguageDef = cppLanguageDefinition "cppLanguage" $
    doc "Language constraints for C++" $
    Coders.language "hydra.ext.cpp"
      eliminationVariants
      literalVariants
      floatTypes
      functionVariants
      integerTypes
      termVariants
      typeVariants
      typePredicate
  where
      eliminationVariants = [
        EliminationVariantProduct,
        EliminationVariantRecord,
        EliminationVariantUnion,
        EliminationVariantWrap]
      literalVariants = [
        LiteralVariantBinary,  -- char arrays, std::byte arrays
        LiteralVariantBoolean, -- bool
        LiteralVariantFloat,   -- float, double
        LiteralVariantInteger, -- int, long, etc.
        LiteralVariantString]  -- std::string
      floatTypes = [
        FloatTypeFloat32,      -- float
        FloatTypeFloat64]      -- double
      functionVariants = [
        FunctionVariantElimination,
        FunctionVariantLambda,
        FunctionVariantPrimitive]
      integerTypes = [
        IntegerTypeInt8,       -- char, int8_t
        IntegerTypeInt16,      -- short, int16_t
        IntegerTypeInt32,      -- int, int32_t
        IntegerTypeInt64,      -- long, long long, int64_t
        IntegerTypeBigint]     -- custom big integer implementation
      termVariants = [
        TermVariantApplication,
        TermVariantFunction,
        TermVariantLet,
        TermVariantList,       -- std::vector
        TermVariantLiteral,
        TermVariantMap,        -- std::map
        TermVariantOptional,   -- std::optional
        TermVariantProduct,    -- struct with unnamed fields
        TermVariantRecord,     -- struct with named fields
        TermVariantSet,        -- std::set
        TermVariantUnion,      -- std::variant or enum
        TermVariantVariable,
        TermVariantWrap]       -- wrapper class
      typeVariants = [
        TypeVariantApplication, -- template instantiation
        TypeVariantFunction,    -- function types
        TypeVariantForall,      -- templates
        TypeVariantList,        -- std::vector
        TypeVariantLiteral,     -- primitive types
        TypeVariantMap,         -- std::map
        TypeVariantOptional,    -- std::optional
        TypeVariantProduct,     -- anonymous structs
        TypeVariantRecord,      -- structs
        TypeVariantSet,         -- std::set
        TypeVariantUnion,       -- std::variant, enum
        TypeVariantVariable,    -- type parameters
        TypeVariantWrap]        -- wrapper class
      typePredicate = constant true -- TODO: refine this with C++ specific constraints

cppReservedWordsDef :: TElement (S.Set String)
cppReservedWordsDef = cppLanguageDefinition "cppReservedWords" $
  doc "A set of reserved words in C++" $
  lets [
    "cppKeywords">:
      doc "C++ keywords, including C++11/14/17/20 additions" $
      list [
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
      list [
        "#define", "#elif", "#else", "#endif", "#error", "#if", "#ifdef", "#ifndef", "#include", "#line",
        "#pragma", "#undef"],
    "cppStlTypes">:
      doc "Common STL types and namespaces that should be treated as reserved" $
      list [
        "std", "string", "vector", "map", "set", "optional", "variant", "pair", "tuple",
        "function", "array", "deque", "forward_list", "list", "multimap", "multiset",
        "unordered_map", "unordered_set", "unordered_multimap", "unordered_multiset",
        "stack", "queue", "priority_queue", "shared_ptr", "unique_ptr", "weak_ptr"],
    "hydraCppKeywords">:
      doc "Reserved words which are specific to Hydra" $
      list []]
    $ Sets.fromList $ Lists.concat2 (Lists.concat2 (Lists.concat2 (var "cppKeywords") (var "cppPreprocessor")) (var "cppStlTypes")) (var "hydraCppKeywords")
