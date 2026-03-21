-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.phantoms

module Hydra.Decode.Phantoms where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

tBinding :: t0 -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Phantoms.TBinding t1)
tBinding a cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Helpers.toFieldMap v0
        in (Eithers.bind (Helpers.requireField "name" Core_.name fieldMap cx) (\field_name -> Eithers.bind (Helpers.requireField "term" (tTerm a) fieldMap cx) (\field_term -> Right (Phantoms.TBinding {
          Phantoms.tBindingName = field_name,
          Phantoms.tBindingTerm = field_term}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

tTerm :: t0 -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Phantoms.TTerm t1)
tTerm a cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Phantoms.TTerm b) (Core_.term cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)
