-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.typed

module Hydra.Decode.Typed where
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Typed as Typed
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Decoder for hydra.typed.TypedBinding
typedBinding :: t0 -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Typed.TypedBinding t1)
typedBinding a cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "term" (typedTerm a) fieldMap cx) (\field_term -> Right (Typed.TypedBinding {
          Typed.typedBindingName = field_name,
          Typed.typedBindingTerm = field_term}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.typed.TypedTerm
typedTerm :: t0 -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Typed.TypedTerm t1)
typedTerm a cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Typed.TypedTerm b) (DecodeCore.term cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.typed.TypedTermDefinition
typedTermDefinition :: t0 -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Typed.TypedTermDefinition t1)
typedTermDefinition a cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "term" (typedTerm a) fieldMap cx) (\field_term -> Right (Typed.TypedTermDefinition {
          Typed.typedTermDefinitionName = field_name,
          Typed.typedTermDefinitionTerm = field_term}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
