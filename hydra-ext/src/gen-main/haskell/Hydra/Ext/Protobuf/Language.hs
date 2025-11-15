-- | Language constraints for Protobuf v3

module Hydra.Ext.Protobuf.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Meta as Meta
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
      Meta.LiteralVariantBinary,
      Meta.LiteralVariantBoolean,
      Meta.LiteralVariantFloat,
      Meta.LiteralVariantInteger,
      Meta.LiteralVariantString])
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
      Meta.TermVariantList,
      Meta.TermVariantLiteral,
      Meta.TermVariantMap,
      Meta.TermVariantMaybe,
      Meta.TermVariantRecord,
      Meta.TermVariantUnion,
      Meta.TermVariantUnit])
    typeVariants = (Sets.fromList [
      Meta.TypeVariantAnnotated,
      Meta.TypeVariantList,
      Meta.TypeVariantLiteral,
      Meta.TypeVariantMap,
      Meta.TypeVariantMaybe,
      Meta.TypeVariantRecord,
      Meta.TypeVariantUnion,
      Meta.TypeVariantUnit,
      Meta.TypeVariantVariable])
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
