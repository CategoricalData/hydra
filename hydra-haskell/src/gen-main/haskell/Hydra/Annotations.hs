-- | Utilities for reading and writing type and term annotations

module Hydra.Annotations where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Strings as Strings
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

getAnnotation :: (Core.Name -> Map Core.Name Core.Term -> Maybe Core.Term)
getAnnotation key ann = (Maps.lookup key ann)

-- | Get the annotated type of a given term, if any
getTermType :: (Core.Term -> Maybe Core.Type)
getTermType x = case x of
  Core.TermAnnotated v1 -> (getTermType (Core.annotatedTermSubject v1))
  Core.TermTyped v1 -> (Just (Core.typedTermType v1))
  _ -> Nothing

-- | Get the annotated type of a given element, or fail if it is missing
requireElementType :: (Graph.Element -> Compute.Flow Graph.Graph Core.Type)
requireElementType el =  
  let withType = (\x -> case x of
          Nothing -> (Flows.fail (Strings.cat [
            "missing type annotation for element ",
            (Core.unName (Graph.elementName el))]))
          Just v1 -> (Flows.pure v1))
  in (withType (getTermType (Graph.elementTerm el)))

-- | Get the annotated type of a given term, or fail if it is missing
requireTermType :: (Core.Term -> Compute.Flow Graph.Graph Core.Type)
requireTermType x = (withType (getTermType x)) 
  where 
    withType = (\x -> case x of
      Nothing -> (Flows.fail "missing type annotation")
      Just v1 -> (Flows.pure v1))