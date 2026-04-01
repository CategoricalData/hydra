-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.context

module Hydra.Decode.Context where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

context :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Context.Context
context cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "trace" (Helpers.decodeList (\cx2 -> \raw2 -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx2 raw2))) fieldMap cx) (\field_trace -> Eithers.bind (Helpers.requireField "messages" (Helpers.decodeList (\cx2 -> \raw2 -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx2 raw2))) fieldMap cx) (\field_messages -> Eithers.bind (Helpers.requireField "other" (Helpers.decodeMap Core_.name Core_.term) fieldMap cx) (\field_other -> Right (Context.Context {
          Context.contextTrace = field_trace,
          Context.contextMessages = field_messages,
          Context.contextOther = field_other})))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

inContext :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Context.InContext t0)
inContext e cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "object" e fieldMap cx) (\field_object -> Eithers.bind (Helpers.requireField "context" context fieldMap cx) (\field_context -> Right (Context.InContext {
          Context.inContextObject = field_object,
          Context.inContextContext = field_context}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)
