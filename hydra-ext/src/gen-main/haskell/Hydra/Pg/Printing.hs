-- Note: this is an automatically generated file. Do not edit.

-- | Printing functions for property graph elements

module Hydra.Pg.Printing where

import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Pg.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Print an edge using the provided value printer
printEdge :: (t0 -> String) -> Model.Edge t0 -> String
printEdge printValue edge =
     
      let label = Model.unEdgeLabel (Model.edgeLabel edge) 
          id = printValue (Model.edgeId edge)
          outId = printValue (Model.edgeOut edge)
          inId = printValue (Model.edgeIn edge)
          props =
                  Strings.intercalate ", " (Lists.map (\p -> printProperty printValue (Pairs.first p) (Pairs.second p)) (Maps.toList (Model.edgeProperties edge)))
      in (Strings.cat [
        id,
        ": ",
        "(",
        outId,
        ")-[:",
        label,
        " {",
        props,
        "}]->(",
        inId,
        ")"])

-- | Print a graph using the provided value printer
printGraph :: Ord t0 => ((t0 -> String) -> Model.Graph t0 -> String)
printGraph printValue graph =
    printLazyGraph printValue (Model.LazyGraph {
      Model.lazyGraphVertices = (Maps.elems (Model.graphVertices graph)),
      Model.lazyGraphEdges = (Maps.elems (Model.graphEdges graph))})

-- | Print a lazy graph using the provided value printer
printLazyGraph :: (t0 -> String) -> Model.LazyGraph t0 -> String
printLazyGraph printValue lg =
     
      let vertices = Model.lazyGraphVertices lg 
          edges = Model.lazyGraphEdges lg
      in (Strings.cat [
        "vertices:",
        (Strings.cat (Lists.map (\v -> Strings.cat [
          "\n\t",
          (printVertex printValue v)]) vertices)),
        "\nedges:",
        (Strings.cat (Lists.map (\e -> Strings.cat [
          "\n\t",
          (printEdge printValue e)]) edges))])

-- | Print a property using the provided value printer
printProperty :: (t0 -> String) -> Model.PropertyKey -> t0 -> String
printProperty printValue key value =
    Strings.cat [
      Model.unPropertyKey key,
      ": ",
      (printValue value)]

-- | Print a vertex using the provided value printer
printVertex :: (t0 -> String) -> Model.Vertex t0 -> String
printVertex printValue vertex =
     
      let label = Model.unVertexLabel (Model.vertexLabel vertex) 
          id = printValue (Model.vertexId vertex)
          props =
                  Strings.intercalate ", " (Lists.map (\p -> printProperty printValue (Pairs.first p) (Pairs.second p)) (Maps.toList (Model.vertexProperties vertex)))
      in (Strings.cat [
        id,
        ": (",
        label,
        ": {",
        props,
        "})"])
