{-# LANGUAGE DeriveGeneric #-}
module Hydra.Graph
  ( Element(..)
  , Graph(..)
  , _Element
  , _Element_data
  , _Element_name
  , _Element_schema
  , _Graph
  , _Graph_dataTerms
  , _Graph_elements
  , _Graph_schemaGraph
  ) where

import GHC.Generics (Generic)
import Data.Int
import Data.Map
import Data.Set
import Hydra.Core

-- | A graph element, having a name, data term (value), and schema term (type)
data Element
  = Element
    -- | @type hydra/core.Name
    { elementName :: Name
    -- | @type hydra/core.Term
    , elementSchema :: Term
    -- | @type hydra/core.Term
    , elementData :: Term } deriving (Eq, Generic, Ord, Read, Show)

{-| A graph, or set of legal terms combined with a set of elements over those
    terms, as well as another graph, called the
    schema graph -}
data Graph
  = Graph
    -- | @type list: hydra/graph.Element
    { graphElements :: [Element]
    {-| @type function:
                from:
                - hydra/core.Term
                to: boolean -}
    , graphDataTerms :: Term -> Bool
    -- | @type hydra/graph.Graph
    , graphSchemaGraph :: Graph }

_Element = "hydra/graph.Element" :: String
_Element_data = "data" :: String
_Element_name = "name" :: String
_Element_schema = "schema" :: String
_Graph = "hydra/graph.Graph" :: String
_Graph_dataTerms = "dataTerms" :: String
_Graph_elements = "elements" :: String
_Graph_schemaGraph = "schemaGraph" :: String
