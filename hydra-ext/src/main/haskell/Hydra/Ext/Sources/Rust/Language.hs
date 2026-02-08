module Hydra.Ext.Sources.Rust.Language where

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
module_ = Module (Namespace "hydra.ext.rust.language")
  [toBinding rustLanguage, toBinding rustReservedWords]
  [Lexical.ns]
  KernelTypes.kernelTypesNamespaces $
  Just "Language constraints and reserved words for Rust"

rustLanguage :: TBinding Language
rustLanguage = define "rustLanguage" $
    doc "Language constraints for Rust" $ lets [
    "eliminationVariants">: Sets.fromList $ list [
      Variants.eliminationVariantRecord,
      Variants.eliminationVariantUnion,
      Variants.eliminationVariantWrap],
    "literalVariants">: Sets.fromList $ list [
      Variants.literalVariantBinary, -- &[u8], Vec<u8>
      Variants.literalVariantBoolean, -- bool
      Variants.literalVariantFloat, -- f32, f64
      Variants.literalVariantInteger, -- i8, i16, i32, i64, i128, u8, u16, u32, u64, u128, isize, usize
      Variants.literalVariantString], -- String, &str
    "floatTypes">: Sets.fromList $ list [
      Core.floatTypeFloat32, -- f32
      Core.floatTypeFloat64], -- f64
    "functionVariants">: Sets.fromList $ list [
      Variants.functionVariantElimination,
      Variants.functionVariantLambda,
      Variants.functionVariantPrimitive],
    "integerTypes">: Sets.fromList $ list [
      Core.integerTypeInt8,   -- i8
      Core.integerTypeInt16,  -- i16
      Core.integerTypeInt32,  -- i32
      Core.integerTypeInt64,  -- i64
      Core.integerTypeUint8,  -- u8
      Core.integerTypeUint16, -- u16
      Core.integerTypeUint32, -- u32
      Core.integerTypeUint64], -- u64
      -- Note: Rust also has i128/u128 and isize/usize, but these are not in Hydra's core integer types
    "termVariants">: Sets.fromList $ list [
      Variants.termVariantAnnotated,
      Variants.termVariantApplication,
      Variants.termVariantEither,
      Variants.termVariantFunction,
      Variants.termVariantLet,
      Variants.termVariantList,
      Variants.termVariantLiteral,
      Variants.termVariantMap,
      Variants.termVariantMaybe,
      Variants.termVariantPair,
      Variants.termVariantRecord,
      Variants.termVariantSet,
      Variants.termVariantUnion,
      Variants.termVariantUnit,
      Variants.termVariantVariable,
      Variants.termVariantWrap],
    "typeVariants">: Sets.fromList $ list [
      Variants.typeVariantAnnotated,
      Variants.typeVariantApplication,
      Variants.typeVariantEither,
      Variants.typeVariantFunction,
      Variants.typeVariantForall,
      Variants.typeVariantList,
      Variants.typeVariantLiteral,
      Variants.typeVariantMap,
      Variants.typeVariantMaybe,
      Variants.typeVariantPair,
      Variants.typeVariantRecord,
      Variants.typeVariantSet,
      Variants.typeVariantUnion,
      Variants.typeVariantUnit,
      Variants.typeVariantVariable,
      Variants.typeVariantWrap],
    "typePredicate">: constant true] $
    Coders.language
      (Coders.languageName $ string "hydra.ext.rust")
      (Coders.languageConstraints
        (var "eliminationVariants")
        (var "literalVariants")
        (var "floatTypes")
        (var "functionVariants")
        (var "integerTypes")
        (var "termVariants")
        (var "typeVariants")
        (var "typePredicate"))

rustReservedWords :: TBinding (S.Set String)
rustReservedWords = define "rustReservedWords" $
  doc "A set of reserved words in Rust" $
  lets [
    "strictKeywords">:
      doc "Rust strict keywords (cannot be used as identifiers). See https://doc.rust-lang.org/reference/keywords.html" $
      list $ string <$> [
        "as", "async", "await", "break", "const", "continue", "crate", "dyn", "else", "enum", "extern",
        "false", "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut",
        "pub", "ref", "return", "self", "Self", "static", "struct", "super", "trait", "true", "type",
        "unsafe", "use", "where", "while"],
    "reservedKeywords">:
      doc "Rust reserved keywords (reserved for future use). See https://doc.rust-lang.org/reference/keywords.html" $
      list $ string <$> [
        "abstract", "become", "box", "do", "final", "macro", "override", "priv", "try", "typeof",
        "unsized", "virtual", "yield"],
    "weakKeywords">:
      doc "Rust weak keywords (have special meaning in certain contexts). See https://doc.rust-lang.org/reference/keywords.html" $
      list $ string <$> [
        "macro_rules", "union", "'static"],
    "primitiveTypes">:
      doc "Rust primitive type names that we reserve to avoid confusion" $
      list $ string <$> [
        "bool", "char", "str", "u8", "u16", "u32", "u64", "u128", "usize",
        "i8", "i16", "i32", "i64", "i128", "isize", "f32", "f64"],
    "stdPreludeTypes">:
      doc "Common types from std::prelude that we reserve to avoid shadowing" $
      list $ string <$> [
        "Option", "Some", "None", "Result", "Ok", "Err", "Box", "String", "Vec",
        "Clone", "Copy", "Default", "Drop", "Eq", "PartialEq", "Ord", "PartialOrd",
        "Hash", "Debug", "Display", "Iterator", "IntoIterator", "From", "Into",
        "AsRef", "AsMut", "Send", "Sync", "Sized", "Unpin", "Fn", "FnMut", "FnOnce"],
    "hydraRustKeywords">:
      doc "Reserved words which are specific to Hydra-Rust code generation" $
      list $ string <$> ["Node"]] $
    Sets.fromList $ Lists.concat $ list [
      var "strictKeywords",
      var "reservedKeywords",
      var "weakKeywords",
      var "primitiveTypes",
      var "stdPreludeTypes",
      var "hydraRustKeywords"]
