-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.coders

module Hydra.Decode.Coders where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

coderDirection :: Graph.Graph -> Core.Term -> Either Error.DecodingError Coders.CoderDirection
coderDirection cx raw =
    Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->  
        let field = Core.injectionField v0 
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "encode", (\input -> Eithers.map (\t -> Coders.CoderDirectionEncode) (Helpers.decodeUnit cx input))),
                      (Core.Name "decode", (\input -> Eithers.map (\t -> Coders.CoderDirectionDecode) (Helpers.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Error.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Error.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

languageName :: Graph.Graph -> Core.Term -> Either Error.DecodingError Coders.LanguageName
languageName cx raw =
    Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Coders.LanguageName b) ((\raw -> Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> case stripped of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Error.DecodingError "expected string literal")
        _ -> Left (Error.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v0))
      _ -> Left (Error.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

traversalOrder :: Graph.Graph -> Core.Term -> Either Error.DecodingError Coders.TraversalOrder
traversalOrder cx raw =
    Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->  
        let field = Core.injectionField v0 
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "pre", (\input -> Eithers.map (\t -> Coders.TraversalOrderPre) (Helpers.decodeUnit cx input))),
                      (Core.Name "post", (\input -> Eithers.map (\t -> Coders.TraversalOrderPost) (Helpers.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Error.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Error.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)
