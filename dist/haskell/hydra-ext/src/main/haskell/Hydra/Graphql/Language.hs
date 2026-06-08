-- Note: this is an automatically generated file. Do not edit.
-- | Language constraints and reserved words for GraphQL

module Hydra.Graphql.Language where
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
-- | Language constraints for GraphQL
graphqlLanguage :: Coders.Language
graphqlLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.graphql"),
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
          Variants.LiteralVariantBoolean,
          Variants.LiteralVariantFloat,
          Variants.LiteralVariantInteger,
          Variants.LiteralVariantString]
    floatTypes = Sets.fromList [
      Core.FloatTypeFloat64]
    integerTypes = Sets.fromList [
      Core.IntegerTypeInt32]
    termVariants =
        Sets.fromList [
          Variants.TermVariantList,
          Variants.TermVariantLiteral,
          Variants.TermVariantOptional,
          Variants.TermVariantRecord,
          Variants.TermVariantInject]
    typeVariants =
        Sets.fromList [
          Variants.TypeVariantApplication,
          Variants.TypeVariantEither,
          Variants.TypeVariantForall,
          Variants.TypeVariantFunction,
          Variants.TypeVariantList,
          Variants.TypeVariantLiteral,
          Variants.TypeVariantMap,
          Variants.TypeVariantPair,
          Variants.TypeVariantSet,
          Variants.TypeVariantUnit,
          Variants.TypeVariantWrap,
          Variants.TypeVariantOptional,
          Variants.TypeVariantRecord,
          Variants.TypeVariantUnion,
          Variants.TypeVariantVariable]
    typePredicate =
        \typ -> case (Strip.deannotateType typ) of
          Core.TypeOptional v0 -> case (Strip.deannotateType v0) of
            Core.TypeOptional _ -> False
            _ -> True
          _ -> True
-- | A set of reserved words in GraphQL
graphqlReservedWords :: S.Set String
graphqlReservedWords =
    Sets.fromList [
      "true",
      "false"]
