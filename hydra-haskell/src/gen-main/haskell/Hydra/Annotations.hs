-- | Utilities for reading and writing type and term annotations

module Hydra.Annotations where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Strings as Strings
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Get the annotated type of a given term, if any
getTermType :: (Core.Term -> Maybe Core.Type)
getTermType x = case x of
  Core.TermAnnotated v1 -> (getTermType (Core.annotatedTermSubject v1))
  Core.TermTyped v1 -> (Just (Core.typedTermType v1))
  _ -> Nothing

requireElementType :: (Graph.Element -> Compute.Flow t0 Core.Type)
requireElementType el =  
  let withType = (Optionals.maybe (Flows.fail (Strings.cat [
          "missing type annotation for element ",
          (Core.unName (Graph.elementName el))])) (\t -> Flows.pure t))
  in (withType (getTermType (Graph.elementTerm el)))

requireTermType :: (Core.Term -> Compute.Flow t0 Core.Type)
requireTermType =  
  let withType = (Optionals.maybe (Flows.fail "missing type annotation") (\t -> Flows.pure t))
  in (\arg_ -> withType (getTermType arg_))
