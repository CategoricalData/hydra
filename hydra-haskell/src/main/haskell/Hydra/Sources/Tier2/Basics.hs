{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Basics where

-- Standard Tier-2 imports
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y
import           Hydra.Dsl.Base            as Base
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier1.All


basicsDefinition :: String -> TTerm a -> TElement a
basicsDefinition = definitionInModule hydraBasicsModule

hydraBasicsModule :: Module
hydraBasicsModule = Module (Namespace "hydra/basics") elements
    [hydraRewritingModule]
    tier0Modules $
    Just "A tier-2 module of basic functions for working with types and terms."
  where
   elements = [
     -- Common.hs
     el elementsToGraphDef]


-- Common.hs

elementsToGraphDef :: TElement (Graph -> Maybe Graph -> [Element] -> Graph)
elementsToGraphDef = basicsDefinition "elementsToGraph" $
  function graphT (funT (optionalT graphT) (funT (TypeList elementT) graphT)) $
  lambda "parent" $ lambda "schema" $ lambda "elements" $
    Graph.graph
      (Maps.fromList @@ (Lists.map @@ var "toPair" @@ var "elements"))
      (Graph.graphEnvironment @@ var "parent")
      (Graph.graphTypes @@ var "parent")
      (Graph.graphBody @@ var "parent")
      (Graph.graphPrimitives @@ var "parent")
      (var "schema")
  `with` [
    "toPair" >: lambda "el" $ pair (Graph.elementName @@ var "el") (var "el")]
