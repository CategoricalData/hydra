-- Note: this file was created with the help of a large language model. It requires further human review.

{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier3.Ext.Cpp.Language where

import Hydra.Kernel
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.All as Tier2
import Hydra.Dsl.Phantoms as Base
import Hydra.Dsl.Coders as Coders
import Hydra.Dsl.Lib.Equality as Equality
import Hydra.Dsl.Lib.Flows as Flows
import Hydra.Dsl.Lib.Lists as Lists
import Hydra.Dsl.Lib.Logic as Logic
import Hydra.Dsl.Lib.Maps as Maps
import Hydra.Dsl.Lib.Sets as Sets
import Hydra.Dsl.Lib.Strings as Strings
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Data.Set as S


cppLanguageDefinition :: String -> TTerm a -> TElement a
cppLanguageDefinition = definitionInModule cppLanguageModule

cppLanguageModule :: Module
cppLanguageModule = Module ns elements
    [KernelTypes.hydraCodersModule, Tier2.hydraLexicalModule] [KernelTypes.hydraGraphModule, KernelTypes.hydraCodersModule] $
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
