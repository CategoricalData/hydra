-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.constraints

module Hydra.Decode.Constraints where

import qualified Hydra.Constraints as Constraints
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Query as Query
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

pathEquation :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Constraints.PathEquation)
pathEquation cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "left" Query.path fieldMap cx) (\field_left -> Eithers.bind (Helpers.requireField "right" Query.path fieldMap cx) (\field_right -> Right (Constraints.PathEquation {
      Constraints.pathEquationLeft = field_left,
      Constraints.pathEquationRight = field_right}))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.constraints.PathEquation"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

patternImplication :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Constraints.PatternImplication)
patternImplication cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "antecedent" Query.pattern fieldMap cx) (\field_antecedent -> Eithers.bind (Helpers.requireField "consequent" Query.pattern fieldMap cx) (\field_consequent -> Right (Constraints.PatternImplication {
      Constraints.patternImplicationAntecedent = field_antecedent,
      Constraints.patternImplicationConsequent = field_consequent}))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.constraints.PatternImplication"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
