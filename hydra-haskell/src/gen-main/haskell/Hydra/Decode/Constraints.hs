-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.constraints

module Hydra.Decode.Constraints where

import qualified Hydra.Constraints as Constraints
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Query as Query
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

pathEquation :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Constraints.PathEquation)
pathEquation cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_left -> Eithers.either (\err -> Left err) (\field_right -> Right (Constraints.PathEquation {
      Constraints.pathEquationLeft = field_left,
      Constraints.pathEquationRight = field_right})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "right",
      " in record"]))) (\fieldTerm -> Query.path cx fieldTerm) (Maps.lookup (Core.Name "right") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "left",
      " in record"]))) (\fieldTerm -> Query.path cx fieldTerm) (Maps.lookup (Core.Name "left") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.constraints.PathEquation"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

patternImplication :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Constraints.PatternImplication)
patternImplication cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_antecedent -> Eithers.either (\err -> Left err) (\field_consequent -> Right (Constraints.PatternImplication {
      Constraints.patternImplicationAntecedent = field_antecedent,
      Constraints.patternImplicationConsequent = field_consequent})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "consequent",
      " in record"]))) (\fieldTerm -> Query.pattern cx fieldTerm) (Maps.lookup (Core.Name "consequent") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "antecedent",
      " in record"]))) (\fieldTerm -> Query.pattern cx fieldTerm) (Maps.lookup (Core.Name "antecedent") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.constraints.PatternImplication"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
