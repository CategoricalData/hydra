-- Note: this is an automatically generated file. Do not edit.
-- | Language constraints for Protobuf v3

module Hydra.Protobuf.Language where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Strip as Strip
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
-- | Language constraints for Protocol Buffers v3
protobufLanguage :: Coders.Language
protobufLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.protobuf"),
      Coders.languageConstraints = Coders.LanguageConstraints {
        Coders.languageConstraintsLiteralVariants = literalVariants,
        Coders.languageConstraintsFloatTypes = floatTypes,
        Coders.languageConstraintsIntegerTypes = integerTypes,
        Coders.languageConstraintsTermVariants = termVariants,
        Coders.languageConstraintsTypeVariants = typeVariants,
        Coders.languageConstraintsTypes = typePredicate}}
  where
    literalVariants =
        Sets.fromList [
          Variants.LiteralVariantBinary,
          Variants.LiteralVariantBoolean,
          Variants.LiteralVariantFloat,
          Variants.LiteralVariantInteger,
          Variants.LiteralVariantString]
    floatTypes =
        Sets.fromList [
          Core.FloatTypeFloat32,
          Core.FloatTypeFloat64]
    integerTypes =
        Sets.fromList [
          Core.IntegerTypeInt32,
          Core.IntegerTypeInt64,
          Core.IntegerTypeUint32,
          Core.IntegerTypeUint64]
    termVariants =
        Sets.fromList [
          Variants.TermVariantEither,
          Variants.TermVariantList,
          Variants.TermVariantLiteral,
          Variants.TermVariantMap,
          Variants.TermVariantOptional,
          Variants.TermVariantPair,
          Variants.TermVariantRecord,
          Variants.TermVariantSet,
          Variants.TermVariantInject,
          Variants.TermVariantUnit,
          Variants.TermVariantWrap]
    typeVariants =
        Sets.fromList [
          Variants.TypeVariantAnnotated,
          Variants.TypeVariantEither,
          Variants.TypeVariantList,
          Variants.TypeVariantLiteral,
          Variants.TypeVariantMap,
          Variants.TypeVariantOptional,
          Variants.TypeVariantPair,
          Variants.TypeVariantRecord,
          Variants.TypeVariantSet,
          Variants.TypeVariantUnion,
          Variants.TypeVariantUnit,
          Variants.TypeVariantVariable,
          Variants.TypeVariantVoid,
          Variants.TypeVariantWrap]
    typePredicate =
        \typ -> case typ of
          Core.TypeMap v0 ->
            let valuesType = Core.mapTypeValues v0
                stripped = Strip.deannotateType valuesType
            in case stripped of
              Core.TypeOptional _ -> False
              _ -> True
          _ -> True
-- | A set of reserved words in Protobuf
protobufReservedWords :: S.Set String
protobufReservedWords = Sets.fromList (Lists.concat [
  fieldNames])
  where
    fieldNames =
        [
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
