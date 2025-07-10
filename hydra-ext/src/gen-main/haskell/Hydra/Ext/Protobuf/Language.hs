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
    Coders.languageConstraintsEliminationVariants = Sets.empty,
    Coders.languageConstraintsLiteralVariants = (Sets.fromList [
      Mantle.LiteralVariantBinary,
      Mantle.LiteralVariantBoolean,
      Mantle.LiteralVariantFloat,
      Mantle.LiteralVariantInteger,
      Mantle.LiteralVariantString]),
    Coders.languageConstraintsFloatTypes = (Sets.fromList [
      Core.FloatTypeFloat32,
      Core.FloatTypeFloat64]),
    Coders.languageConstraintsFunctionVariants = Sets.empty,
    Coders.languageConstraintsIntegerTypes = (Sets.fromList [
      Core.IntegerTypeInt32,
      Core.IntegerTypeInt64,
      Core.IntegerTypeUint32,
      Core.IntegerTypeUint64]),
    Coders.languageConstraintsTermVariants = (Sets.fromList [
      Mantle.TermVariantList,
      Mantle.TermVariantLiteral,
      Mantle.TermVariantMap,
      Mantle.TermVariantOptional,
      Mantle.TermVariantRecord,
      Mantle.TermVariantUnion,
      Mantle.TermVariantUnit]),
    Coders.languageConstraintsTypeVariants = (Sets.fromList [
      Mantle.TypeVariantAnnotated,
      Mantle.TypeVariantList,
      Mantle.TypeVariantLiteral,
      Mantle.TypeVariantMap,
      Mantle.TypeVariantOptional,
      Mantle.TypeVariantRecord,
      Mantle.TypeVariantUnion,
      Mantle.TypeVariantUnit,
      Mantle.TypeVariantVariable]),
    Coders.languageConstraintsTypes = (\x -> case x of
      Core.TypeMap v1 -> ((\x -> case x of
        Core.TypeOptional _ -> False
        _ -> True) (Rewriting.deannotateType (Core.mapTypeValues v1)))
      _ -> True)}}

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
