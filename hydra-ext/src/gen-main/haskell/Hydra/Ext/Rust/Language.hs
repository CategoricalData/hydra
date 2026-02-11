-- Note: this is an automatically generated file. Do not edit.

-- | Language constraints and reserved words for Rust

module Hydra.Ext.Rust.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Language constraints for Rust
rustLanguage :: Coders.Language
rustLanguage = Coders.Language {
  Coders.languageName = (Coders.LanguageName "hydra.ext.rust"),
  Coders.languageConstraints = Coders.LanguageConstraints {
    Coders.languageConstraintsEliminationVariants = eliminationVariants,
    Coders.languageConstraintsLiteralVariants = literalVariants,
    Coders.languageConstraintsFloatTypes = floatTypes,
    Coders.languageConstraintsFunctionVariants = functionVariants,
    Coders.languageConstraintsIntegerTypes = integerTypes,
    Coders.languageConstraintsTermVariants = termVariants,
    Coders.languageConstraintsTypeVariants = typeVariants,
    Coders.languageConstraintsTypes = typePredicate}} 
  where 
    eliminationVariants = (Sets.fromList [
      Variants.EliminationVariantRecord,
      Variants.EliminationVariantUnion,
      Variants.EliminationVariantWrap])
    literalVariants = (Sets.fromList [
      Variants.LiteralVariantBinary,
      Variants.LiteralVariantBoolean,
      Variants.LiteralVariantFloat,
      Variants.LiteralVariantInteger,
      Variants.LiteralVariantString])
    floatTypes = (Sets.fromList [
      Core.FloatTypeFloat32,
      Core.FloatTypeFloat64])
    functionVariants = (Sets.fromList [
      Variants.FunctionVariantElimination,
      Variants.FunctionVariantLambda,
      Variants.FunctionVariantPrimitive])
    integerTypes = (Sets.fromList [
      Core.IntegerTypeInt8,
      Core.IntegerTypeInt16,
      Core.IntegerTypeInt32,
      Core.IntegerTypeInt64,
      Core.IntegerTypeUint8,
      Core.IntegerTypeUint16,
      Core.IntegerTypeUint32,
      Core.IntegerTypeUint64])
    termVariants = (Sets.fromList [
      Variants.TermVariantAnnotated,
      Variants.TermVariantApplication,
      Variants.TermVariantEither,
      Variants.TermVariantFunction,
      Variants.TermVariantLet,
      Variants.TermVariantList,
      Variants.TermVariantLiteral,
      Variants.TermVariantMap,
      Variants.TermVariantMaybe,
      Variants.TermVariantPair,
      Variants.TermVariantRecord,
      Variants.TermVariantSet,
      Variants.TermVariantUnion,
      Variants.TermVariantUnit,
      Variants.TermVariantVariable,
      Variants.TermVariantWrap])
    typeVariants = (Sets.fromList [
      Variants.TypeVariantAnnotated,
      Variants.TypeVariantApplication,
      Variants.TypeVariantEither,
      Variants.TypeVariantFunction,
      Variants.TypeVariantForall,
      Variants.TypeVariantList,
      Variants.TypeVariantLiteral,
      Variants.TypeVariantMap,
      Variants.TypeVariantMaybe,
      Variants.TypeVariantPair,
      Variants.TypeVariantRecord,
      Variants.TypeVariantSet,
      Variants.TypeVariantUnion,
      Variants.TypeVariantUnit,
      Variants.TypeVariantVariable,
      Variants.TypeVariantWrap])
    typePredicate = (\_ -> True)

-- | A set of reserved words in Rust
rustReservedWords :: (S.Set String)
rustReservedWords = (Sets.fromList (Lists.concat [
  strictKeywords,
  reservedKeywords,
  weakKeywords,
  primitiveTypes,
  stdPreludeTypes,
  hydraRustKeywords])) 
  where 
    strictKeywords = [
      "as",
      "async",
      "await",
      "break",
      "const",
      "continue",
      "crate",
      "dyn",
      "else",
      "enum",
      "extern",
      "false",
      "fn",
      "for",
      "if",
      "impl",
      "in",
      "let",
      "loop",
      "match",
      "mod",
      "move",
      "mut",
      "pub",
      "ref",
      "return",
      "self",
      "Self",
      "static",
      "struct",
      "super",
      "trait",
      "true",
      "type",
      "unsafe",
      "use",
      "where",
      "while"]
    reservedKeywords = [
      "abstract",
      "become",
      "box",
      "do",
      "final",
      "macro",
      "override",
      "priv",
      "try",
      "typeof",
      "unsized",
      "virtual",
      "yield"]
    weakKeywords = [
      "macro_rules",
      "union",
      "'static"]
    primitiveTypes = [
      "bool",
      "char",
      "str",
      "u8",
      "u16",
      "u32",
      "u64",
      "u128",
      "usize",
      "i8",
      "i16",
      "i32",
      "i64",
      "i128",
      "isize",
      "f32",
      "f64"]
    stdPreludeTypes = [
      "Option",
      "Some",
      "None",
      "Result",
      "Ok",
      "Err",
      "Box",
      "String",
      "Vec",
      "Clone",
      "Copy",
      "Default",
      "Drop",
      "Eq",
      "PartialEq",
      "Ord",
      "PartialOrd",
      "Hash",
      "Debug",
      "Display",
      "Iterator",
      "IntoIterator",
      "From",
      "Into",
      "AsRef",
      "AsMut",
      "Send",
      "Sync",
      "Sized",
      "Unpin",
      "Fn",
      "FnMut",
      "FnOnce"]
    hydraRustKeywords = [
      "Node"]
