-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.typed

module Hydra.Decode.Typed where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Overlay.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Typed as Typed
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Decoder for hydra.typed.TypedBinding
typedBinding :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError a) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Typed.TypedBinding a)
typedBinding a cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "term" (typedTerm a) fieldMap cx) (\field_term -> Right (Typed.TypedBinding {
          Typed.typedBindingName = field_name,
          Typed.typedBindingTerm = field_term}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.typed.TypedBinding")) (ExtractCore.stripWithDecodingError cx raw)

-- | Decoder for hydra.typed.TypedName
typedName :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError a) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Typed.TypedName a)
typedName a cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Typed.TypedName b) (DecodeCore.name cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)

-- | Decoder for hydra.typed.TypedTerm
typedTerm :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError a) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Typed.TypedTerm a)
typedTerm a cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Typed.TypedTerm b) (DecodeCore.term cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)

-- | Decoder for hydra.typed.TypedTermDefinition
typedTermDefinition :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError a) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Typed.TypedTermDefinition a)
typedTermDefinition a cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" DecodeCore.name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "term" (typedTerm a) fieldMap cx) (\field_term -> Right (Typed.TypedTermDefinition {
          Typed.typedTermDefinitionName = field_name,
          Typed.typedTermDefinitionTerm = field_term}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.typed.TypedTermDefinition")) (ExtractCore.stripWithDecodingError cx raw)
