
-- Note: this file was created with the help of a large language model. It requires further human review.

module Hydra.Sources.Cpp.Language where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import qualified Hydra.Core.Dsl.Lib.Strings                as Strings
import Hydra.Core.File (_FileExtension)
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms                   as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Annotations                     as Annotations
import qualified Hydra.Core.Overlay.Haskell.Bootstrap                       as Bootstrap
import qualified Hydra.Core.Overlay.Haskell.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Core.Overlay.Haskell.Dsl.Literals                        as Literals
import qualified Hydra.Core.Dsl.Paths                      as Paths
import qualified Hydra.Core.Dsl.Ast                        as Ast
import qualified Hydra.Core.Overlay.Haskell.Dsl.Base                       as MetaBase
import qualified Hydra.Core.Dsl.Coders                     as Coders
import qualified Hydra.Core.Dsl.Util                    as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core                       as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Core.Dsl.Json.Model                       as Json
import qualified Hydra.Core.Dsl.Lib.Chars                  as Chars
import qualified Hydra.Core.Dsl.Lib.Eithers                as Eithers
import qualified Hydra.Core.Dsl.Lib.Equality               as Equality
import qualified Hydra.Core.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Core.Dsl.Lib.Literals               as Literals
import qualified Hydra.Core.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Core.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Core.Dsl.Lib.Math                   as Math
import qualified Hydra.Core.Dsl.Lib.Optionals                 as Optionals
import qualified Hydra.Core.Dsl.Lib.Pairs                  as Pairs
import qualified Hydra.Core.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Core.Dsl.Packaging                     as Packaging
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Core.Dsl.Topology                   as Topology
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Core.Dsl.Typing                     as Typing
import qualified Hydra.Core.Dsl.Util                       as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims                           as Prims
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Core.Overlay.Haskell.Dsl.Terms                           as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Tests                           as Tests
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types                           as Types
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Print.Paths as PrintPaths
import qualified Hydra.Sources.Kernel.Terms.Print.Core      as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Print.Graph     as PrintGraph
import qualified Hydra.Sources.Kernel.Terms.Print.Variants  as PrintVariants
import qualified Hydra.Sources.Kernel.Terms.Print.Typing    as PrintTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y


define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

module_ :: Module
module_ = Module {
            moduleName = (ModuleName "hydra.ext.cpp.language"),
            moduleDefinitions = [toDefinition cppLanguage, toDefinition cppReservedWords],
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Lexical.ns] L.++ KernelTypes.kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Language constraints and reserved words for C++")}
cppLanguage :: TypedTermDefinition Language
cppLanguage = define "cppLanguage" $
  doc "Language constraints for C++" $ lets [
  "literalVariants">: Sets.fromList $ list [
    Variants.literalVariantBinary,  -- char arrays, std::byte arrays
    Variants.literalVariantBoolean, -- bool
    Variants.literalVariantFloat,   -- float, double
    Variants.literalVariantInteger, -- int, long, etc.
    Variants.literalVariantString], -- std::string
  "floatTypes">: Sets.fromList $ list [
    Core.floatTypeFloat32,      -- float
    Core.floatTypeFloat64],     -- double
  "integerTypes">: Sets.fromList $ list [
    Core.integerTypeInt8,       -- char, int8_t
    Core.integerTypeInt16,      -- short, int16_t
    Core.integerTypeInt32,      -- int, int32_t
    Core.integerTypeInt64,      -- long, long long, int64_t
    Core.integerTypeBigint],    -- custom big integer implementation
  "termVariants">: Sets.fromList $ list [
    Variants.termVariantApplication,
    Variants.termVariantEither,      -- std::variant<Left, Right> with left/right semantics

    Variants.termVariantCases,

    Variants.termVariantLambda,

    Variants.termVariantProject,

    Variants.termVariantUnwrap,

    Variants.termVariantTypeApplication,

    Variants.termVariantTypeLambda,
    Variants.termVariantLet,
    Variants.termVariantList,        -- std::vector
    Variants.termVariantLiteral,
    Variants.termVariantMap,         -- std::map
    Variants.termVariantOptional,       -- std::optional
    Variants.termVariantPair,        -- std::pair
    Variants.termVariantRecord,      -- struct with named fields
    Variants.termVariantSet,         -- std::set
    Variants.termVariantInject,       -- std::inject or enum
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
    Variants.typeVariantOptional,       -- std::optional
    Variants.typeVariantPair,        -- std::pair
    Variants.typeVariantRecord,      -- structs
    Variants.typeVariantSet,         -- std::set
    Variants.typeVariantUnion,       -- std::variant, enum
    Variants.typeVariantVariable,    -- type parameters
    Variants.typeVariantWrap],       -- wrapper class
  "typePredicate">: constant true] $ -- TODO: refine this with C++ specific constraints
  Coders.language
    (Coders.languageName2 $ string "hydra.cpp")
    (Coders.languageConstraints2
      (var "literalVariants")
      (var "floatTypes")
      (var "integerTypes")
      (var "termVariants")
      (var "typeVariants")
      (var "typePredicate"))
      (Sets.fromList $ list [
        Coders.languageFeaturePartialApplication,
        Coders.languageFeatureNestedCaseStatements,
        Coders.languageFeatureNestedPolymorphicLetBindings])
      (Coders.caseConventions
        Util.caseConventionUpperSnake Util.caseConventionLowerSnake Util.caseConventionPascal
        Util.caseConventionCamel Util.caseConventionLowerSnake Util.caseConventionLowerSnake
        Util.caseConventionCamel Util.caseConventionCamel Util.caseConventionPascal
        Util.caseConventionPascal)
      (wrap _FileExtension (string "cpp"))

cppReservedWords :: TypedTermDefinition (S.Set String)
cppReservedWords = define "cppReservedWords" $
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
