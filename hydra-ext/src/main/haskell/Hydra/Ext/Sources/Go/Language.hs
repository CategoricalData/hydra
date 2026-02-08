module Hydra.Ext.Sources.Go.Language where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Meta.Accessors                  as Accessors
import qualified Hydra.Dsl.Meta.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Compute                    as Compute
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Meta.Json                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows                  as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Meta.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Meta.Typing                     as Typing
import qualified Hydra.Dsl.Meta.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules  as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple   as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms    as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils    as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars       as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Monads         as Monads
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan         as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

module_ :: Module
module_ = Module (Namespace "hydra.ext.go.language")
  [toBinding goLanguage, toBinding goReservedWords]
  [Lexical.ns]
  KernelTypes.kernelTypesNamespaces $
  Just "Language constraints and reserved words for Go 1.22+"

goLanguage :: TBinding Language
goLanguage = define "goLanguage" $
    doc "Language constraints for Go 1.22+" $ lets [
    -- Go supports record projection, union elimination via type switch, and wrap/unwrap
    "eliminationVariants">: Sets.fromList $ list [
      Variants.eliminationVariantRecord,
      Variants.eliminationVariantUnion,
      Variants.eliminationVariantWrap],
    -- Go supports all literal variants
    "literalVariants">: Sets.fromList $ list [
      Variants.literalVariantBinary, -- []byte
      Variants.literalVariantBoolean, -- bool
      Variants.literalVariantFloat, -- float32, float64
      Variants.literalVariantInteger, -- int8, int16, int32, int64, uint8, uint16, uint32, uint64
      Variants.literalVariantString], -- string
    -- Go has float32 and float64 but no native arbitrary-precision float
    "floatTypes">: Sets.fromList $ list [
      Core.floatTypeFloat32, -- float32
      Core.floatTypeFloat64], -- float64
    -- Go supports all three function variants
    "functionVariants">: Sets.fromList $ list [
      Variants.functionVariantElimination,
      Variants.functionVariantLambda,
      Variants.functionVariantPrimitive],
    -- Go has all standard integer types plus big.Int from math/big
    "integerTypes">: Sets.fromList $ list [
      Core.integerTypeBigint, -- math/big.Int
      Core.integerTypeInt8, -- int8
      Core.integerTypeInt16, -- int16
      Core.integerTypeInt32, -- int32 (also rune)
      Core.integerTypeInt64, -- int64
      Core.integerTypeUint8, -- uint8 (also byte)
      Core.integerTypeUint16, -- uint16
      Core.integerTypeUint32, -- uint32
      Core.integerTypeUint64], -- uint64
    -- Go supports most term variants; some require interface{} + type switch patterns
    "termVariants">: Sets.fromList $ list [
      Variants.termVariantAnnotated,
      Variants.termVariantApplication,
      Variants.termVariantEither, -- via interface with Left/Right structs
      Variants.termVariantFunction,
      Variants.termVariantLet, -- via := and var
      Variants.termVariantList, -- slices []T
      Variants.termVariantLiteral,
      Variants.termVariantMap, -- map[K]V
      Variants.termVariantMaybe, -- *T (pointer) or custom Maybe type
      Variants.termVariantPair, -- struct{First T; Second U}
      Variants.termVariantRecord, -- struct
      Variants.termVariantSet, -- map[T]struct{} or custom Set type
      Variants.termVariantTypeApplication, -- generics T[U]
      Variants.termVariantTypeLambda, -- generic functions func[T any](...)
      Variants.termVariantUnion, -- interface with sealed marker method
      Variants.termVariantUnit, -- struct{}
      Variants.termVariantVariable,
      Variants.termVariantWrap], -- type aliases / named types
    -- Go 1.22+ supports generics, so most type variants are available
    "typeVariants">: Sets.fromList $ list [
      Variants.typeVariantAnnotated,
      Variants.typeVariantApplication, -- T[U]
      Variants.typeVariantEither, -- interface
      Variants.typeVariantFunction, -- func(A) B
      Variants.typeVariantForall, -- [T any]
      Variants.typeVariantList, -- []T
      Variants.typeVariantLiteral,
      Variants.typeVariantMap, -- map[K]V
      Variants.typeVariantMaybe, -- *T
      Variants.typeVariantPair, -- struct{First T; Second U}
      Variants.typeVariantRecord, -- struct
      Variants.typeVariantSet, -- map[T]struct{}
      Variants.typeVariantUnion, -- interface
      Variants.typeVariantUnit, -- struct{}
      Variants.typeVariantVariable,
      Variants.typeVariantWrap], -- type Name T
    -- Go's type system is more restrictive than Haskell's, but with generics most types work
    "typePredicate">: constant true] $
    Coders.language
      (Coders.languageName $ string "hydra.ext.go")
      (Coders.languageConstraints
        (var "eliminationVariants")
        (var "literalVariants")
        (var "floatTypes")
        (var "functionVariants")
        (var "integerTypes")
        (var "termVariants")
        (var "typeVariants")
        (var "typePredicate"))

goReservedWords :: TBinding (S.Set String)
goReservedWords = define "goReservedWords" $
  doc "A set of reserved words in Go" $
  lets [
    "goKeywords">:
      doc "Go keywords, as enumerated at https://go.dev/ref/spec#Keywords" $
      list $ string <$> [
        "break", "case", "chan", "const", "continue", "default", "defer", "else",
        "fallthrough", "for", "func", "go", "goto", "if", "import", "interface",
        "map", "package", "range", "return", "select", "struct", "switch", "type", "var"],
    "goPredeclaredIdentifiers">:
      doc "Predeclared identifiers in Go, as enumerated at https://go.dev/ref/spec#Predeclared_identifiers" $
      list $ string <$> [
        -- Types
        "any", "bool", "byte", "comparable", "complex64", "complex128", "error",
        "float32", "float64", "int", "int8", "int16", "int32", "int64",
        "rune", "string", "uint", "uint8", "uint16", "uint32", "uint64", "uintptr",
        -- Constants
        "true", "false", "iota",
        -- Zero value
        "nil",
        -- Functions
        "append", "cap", "clear", "close", "complex", "copy", "delete", "imag",
        "len", "make", "max", "min", "new", "panic", "print", "println", "real", "recover"],
    "hydraGoKeywords">:
      doc "Reserved words which are specific to Hydra-Go" $
      list $ string <$> ["Node", "Maybe", "Either", "Left", "Right", "Just", "Nothing", "Unit", "Pair"]] $
    Sets.fromList $ Lists.concat $ list [var "goKeywords", var "goPredeclaredIdentifiers", var "hydraGoKeywords"]
