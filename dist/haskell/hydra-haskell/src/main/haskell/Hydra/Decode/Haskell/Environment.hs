-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.haskell.environment

module Hydra.Decode.Haskell.Environment where
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Haskell.Environment as Environment
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Decoder for hydra.haskell.environment.HaskellModuleMetadata
haskellModuleMetadata :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Environment.HaskellModuleMetadata
haskellModuleMetadata cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "usesByteString" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralBoolean v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected boolean literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_usesByteString -> Eithers.bind (ExtractCore.requireField "usesInt" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralBoolean v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected boolean literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_usesInt -> Eithers.bind (ExtractCore.requireField "usesMap" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralBoolean v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected boolean literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_usesMap -> Eithers.bind (ExtractCore.requireField "usesSet" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralBoolean v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected boolean literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_usesSet -> Right (Environment.HaskellModuleMetadata {
          Environment.haskellModuleMetadataUsesByteString = field_usesByteString,
          Environment.haskellModuleMetadataUsesInt = field_usesInt,
          Environment.haskellModuleMetadataUsesMap = field_usesMap,
          Environment.haskellModuleMetadataUsesSet = field_usesSet}))))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.environment.HaskellModuleMetadata")) (ExtractCore.stripWithDecodingError cx raw)
