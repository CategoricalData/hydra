-- Note: this is an automatically generated file. Do not edit.
-- | Printing functions for property graph elements

module Hydra.Pg.Printing where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as JsonModel
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Pg.Model as PgModel
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Print an edge using the provided value printer
printEdge :: (t0 -> String) -> PgModel.Edge t0 -> String
printEdge printValue edge =

      let label = PgModel.unEdgeLabel (PgModel.edgeLabel edge)
          id = printValue (PgModel.edgeId edge)
          outId = printValue (PgModel.edgeOut edge)
          inId = printValue (PgModel.edgeIn edge)
          props =
                  Strings.intercalate ", " (Lists.map (\p -> printProperty printValue (Pairs.first p) (Pairs.second p)) (Maps.toList (PgModel.edgeProperties edge)))
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
printGraph :: Ord t0 => ((t0 -> String) -> PgModel.Graph t0 -> String)
printGraph printValue graph =
    printLazyGraph printValue (PgModel.LazyGraph {
      PgModel.lazyGraphVertices = (Maps.elems (PgModel.graphVertices graph)),
      PgModel.lazyGraphEdges = (Maps.elems (PgModel.graphEdges graph))})
-- | Print a lazy graph using the provided value printer
printLazyGraph :: (t0 -> String) -> PgModel.LazyGraph t0 -> String
printLazyGraph printValue lg =

      let vertices = PgModel.lazyGraphVertices lg
          edges = PgModel.lazyGraphEdges lg
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
printProperty :: (t0 -> String) -> PgModel.PropertyKey -> t0 -> String
printProperty printValue key value =
    Strings.cat [
      PgModel.unPropertyKey key,
      ": ",
      (printValue value)]
-- | Print a vertex using the provided value printer
printVertex :: (t0 -> String) -> PgModel.Vertex t0 -> String
printVertex printValue vertex =

      let label = PgModel.unVertexLabel (PgModel.vertexLabel vertex)
          id = printValue (PgModel.vertexId vertex)
          props =
                  Strings.intercalate ", " (Lists.map (\p -> printProperty printValue (Pairs.first p) (Pairs.second p)) (Maps.toList (PgModel.vertexProperties vertex)))
      in (Strings.cat [
        id,
        ": (",
        label,
        ": {",
        props,
        "})"])
