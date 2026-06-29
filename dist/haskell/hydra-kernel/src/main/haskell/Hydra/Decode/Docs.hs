-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.docs

module Hydra.Decode.Docs where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Packaging as Packaging
import qualified Hydra.Docs as Docs
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Overlay.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Overlay.Haskell.Lib.Maps as Maps
import qualified Hydra.Overlay.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Overlay.Haskell.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Decoder for hydra.docs.DocSegment
docSegment :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Docs.DocSegment
docSegment cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "ref", (\input -> Eithers.map (\t -> Docs.DocSegmentRef t) (Packaging.entityReference cx input))),
                      (
                        Core.Name "text",
                        (\input -> Eithers.map (\t -> Docs.DocSegmentText t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralString v2 -> Right v2
                            _ -> Left (Errors.DecodingError "expected string literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input))))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
