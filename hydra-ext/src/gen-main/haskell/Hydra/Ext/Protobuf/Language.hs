-- | Language constraints for Protobuf v3

module Hydra.Ext.Protobuf.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Rewriting as Rewriting
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Language constraints for Protocol Buffers v3
protobufLanguage :: Coders.Language
protobufLanguage = Coders.Language {
  Coders.languageName = (Coders.LanguageName "hydra.ext.protobuf"),
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
    eliminationVariants = (Sets.fromList [])
    literalVariants = (Sets.fromList [
      Mantle.LiteralVariantBinary,
      Mantle.LiteralVariantBoolean,
      Mantle.LiteralVariantFloat,
      Mantle.LiteralVariantInteger,
      Mantle.LiteralVariantString])
    floatTypes = (Sets.fromList [
      Core.FloatTypeFloat32,
      Core.FloatTypeFloat64])
    functionVariants = (Sets.fromList [])
    integerTypes = (Sets.fromList [
      Core.IntegerTypeInt32,
      Core.IntegerTypeInt64,
      Core.IntegerTypeUint32,
      Core.IntegerTypeUint64])
    termVariants = (Sets.fromList [
      Mantle.TermVariantList,
      Mantle.TermVariantLiteral,
      Mantle.TermVariantMap,
      Mantle.TermVariantMaybe,
      Mantle.TermVariantRecord,
      Mantle.TermVariantUnion,
      Mantle.TermVariantUnit])
    typeVariants = (Sets.fromList [
      Mantle.TypeVariantAnnotated,
      Mantle.TypeVariantList,
      Mantle.TypeVariantLiteral,
      Mantle.TypeVariantMap,
      Mantle.TypeVariantMaybe,
      Mantle.TypeVariantRecord,
      Mantle.TypeVariantUnion,
      Mantle.TypeVariantUnit,
      Mantle.TypeVariantVariable])
    typePredicate = (\typ -> (\x -> case x of
      Core.TypeMap v1 ->  
        let valuesType = (Core.mapTypeValues v1) 
            stripped = (Rewriting.deannotateType valuesType)
        in ((\x -> case x of
          Core.TypeMaybe _ -> False
          _ -> True) stripped)
      _ -> True) typ)

-- | A set of reserved words in Protobuf
protobufReservedWords :: (S.Set String)
protobufReservedWords = (Sets.fromList (Lists.concat [
  fieldNames])) 
  where 
    fieldNames = [
      "case",
      "class",
      "data",
      "default",
      "deriving",
      "do",
      "else",
      "foreign",
      "if",
      "import",
      "in",
      "infix",
      "infixl",
      "infixr",
      "instance",
      "let",
      "mdo",
      "module",
      "newtype",
      "of",
      "pattern",
      "proc",
      "rec",
      "then",
      "type",
      "where"]
