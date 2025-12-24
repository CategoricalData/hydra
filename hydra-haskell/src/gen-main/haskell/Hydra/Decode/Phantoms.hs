-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.phantoms

module Hydra.Decode.Phantoms where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

tBinding :: (t0 -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Phantoms.TBinding t1))
tBinding a cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\name -> Eithers.either (\err -> Left err) (\term -> Right (Phantoms.TBinding {
      Phantoms.tBindingName = name,
      Phantoms.tBindingTerm = term})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "term",
      " in record"]))) (\fieldTerm -> tTerm a cx fieldTerm) (Maps.lookup (Core.Name "term") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "name",
      " in record"]))) (\fieldTerm -> Core_.name cx fieldTerm) (Maps.lookup (Core.Name "name") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.phantoms.TBinding"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

tTerm :: (t0 -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Phantoms.TTerm t1))
tTerm a cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Phantoms.TTerm b) (Core_.term cx (Core.wrappedTermBody v1)))
  _ -> (Left (Util.DecodingError "expected wrapped type hydra.phantoms.TTerm"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
