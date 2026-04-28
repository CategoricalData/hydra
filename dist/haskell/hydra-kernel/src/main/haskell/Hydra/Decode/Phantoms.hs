-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.phantoms

module Hydra.Decode.Phantoms where
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
tBinding :: t0 -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Phantoms.TBinding t1)
tBinding a cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "term" (tTerm a) fieldMap cx) (\field_term -> Right (Phantoms.TBinding {
          Phantoms.tBindingName = field_name,
          Phantoms.tBindingTerm = field_term}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
tTerm :: t0 -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Phantoms.TTerm t1)
tTerm a cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Phantoms.TTerm b) (DecodeCore.term cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
tTermDefinition :: t0 -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Phantoms.TTermDefinition t1)
tTermDefinition a cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "term" (tTerm a) fieldMap cx) (\field_term -> Right (Phantoms.TTermDefinition {
          Phantoms.tTermDefinitionName = field_name,
          Phantoms.tTermDefinitionTerm = field_term}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
