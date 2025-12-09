-- Note: this file was created with the help of a large language model. It requires further human review.

module Hydra.Ext.Sources.Cpp.Language where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
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
import qualified Hydra.Sources.Kernel.Terms.Reflect         as Reflect
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
  [toBinding cppLanguage, toBinding cppReservedWords]
  [Lexical.module_]
  KernelTypes.kernelTypesModules $
  Just "Language constraints and reserved words for C++"

cppLanguage :: TBinding Language
cppLanguage = cppLanguageDefinition "cppLanguage" $
  doc "Language constraints for C++" $ lets [
  "eliminationVariants">: Sets.fromList $ list [
    Variants.eliminationVariantRecord,
    Variants.eliminationVariantUnion,
    Variants.eliminationVariantWrap],
  "literalVariants">: Sets.fromList $ list [
    Variants.literalVariantBinary,  -- char arrays, std::byte arrays
    Variants.literalVariantBoolean, -- bool
    Variants.literalVariantFloat,   -- float, double
    Variants.literalVariantInteger, -- int, long, etc.
    Variants.literalVariantString], -- std::string
  "floatTypes">: Sets.fromList $ list [
    Core.floatTypeFloat32,      -- float
    Core.floatTypeFloat64],     -- double
  "functionVariants">: Sets.fromList $ list [
    Variants.functionVariantElimination,
    Variants.functionVariantLambda,
    Variants.functionVariantPrimitive],
  "integerTypes">: Sets.fromList $ list [
    Core.integerTypeInt8,       -- char, int8_t
    Core.integerTypeInt16,      -- short, int16_t
    Core.integerTypeInt32,      -- int, int32_t
    Core.integerTypeInt64,      -- long, long long, int64_t
    Core.integerTypeBigint],    -- custom big integer implementation
  "termVariants">: Sets.fromList $ list [
    Variants.termVariantApplication,
    Variants.termVariantEither,      -- std::variant<Left, Right> with left/right semantics
    Variants.termVariantFunction,
    Variants.termVariantLet,
    Variants.termVariantList,        -- std::vector
    Variants.termVariantLiteral,
    Variants.termVariantMap,         -- std::map
    Variants.termVariantMaybe,       -- std::optional
    Variants.termVariantPair,        -- std::pair
    Variants.termVariantRecord,      -- struct with named fields
    Variants.termVariantSet,         -- std::set
    Variants.termVariantUnion,       -- std::inject or enum
    Variants.termVariantVariable,
    Variants.termVariantWrap],       -- wrapper class
  "typeVariants">: Sets.fromList $ list [
    Variants.typeVariantApplication, -- template instantiation
    Variants.typeVariantEither,      -- std::variant<Left, Right>
    Variants.typeVariantFunction,    -- function types
    Variants.typeVariantForall,      -- templates
    Variants.typeVariantList,        -- std::vector
    Variants.typeVariantLiteral,     -- primitive types
    Variants.typeVariantMap,         -- std::map
    Variants.typeVariantMaybe,       -- std::optional
    Variants.typeVariantPair,        -- std::pair
    Variants.typeVariantRecord,      -- structs
    Variants.typeVariantSet,         -- std::set
    Variants.typeVariantUnion,       -- std::variant, enum
    Variants.typeVariantVariable,    -- type parameters
    Variants.typeVariantWrap],       -- wrapper class
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

cppReservedWords :: TBinding (S.Set String)
cppReservedWords = cppLanguageDefinition "cppReservedWords" $
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
