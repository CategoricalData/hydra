module Hydra.Ext.Pg.Printing where

import qualified Hydra.Pg.Model as PG
import qualified Hydra.Dsl.Pg.Mappings as PGM

import qualified Data.List as L
import qualified Data.Map as M


data PropertyGraphPrinter t v = PropertyGraphPrinter {
  propertyGraphPrinterType :: t -> String,
  propertyGraphPrinterValue :: v -> String}

printEdge :: PropertyGraphPrinter t v -> PG.Edge v -> String
printEdge printer (PG.Edge (PG.EdgeLabel label) id outId inId props) =
  propertyGraphPrinterValue printer id ++ ": "
  ++ "(" ++ propertyGraphPrinterValue printer outId ++ ")-[:"
  ++ label ++ " {"
  ++ (L.intercalate ", " $ fmap (\(k, v) -> printProperty printer k v) $ M.toList props)
  ++ "}]->("
  ++ propertyGraphPrinterValue printer inId ++ ")"

printGraph :: PropertyGraphPrinter t v -> PG.Graph v -> String
printGraph printer graph = printLazyGraph printer $ PGM.LazyGraph vertices edges
  where
    vertices = M.elems $ PG.graphVertices graph
    edges = M.elems $ PG.graphEdges graph

printLazyGraph :: PropertyGraphPrinter t v -> PGM.LazyGraph v -> String
printLazyGraph printer (PGM.LazyGraph vertices edges) = "vertices:"
    ++ L.concat (fmap (\v -> "\n\t" ++ printVertex printer v) vertices)
    ++ "\nedges:"
    ++ L.concat (fmap (\e -> "\n\t" ++ printEdge printer e) edges)

printProperty :: PropertyGraphPrinter t v -> PG.PropertyKey -> v -> String
printProperty printer (PG.PropertyKey key) value = key ++ ": " ++ propertyGraphPrinterValue printer value

printVertex :: PropertyGraphPrinter t v -> PG.Vertex v -> String
printVertex printer (PG.Vertex (PG.VertexLabel label) id props) =
  propertyGraphPrinterValue printer id ++ ": (" ++ label ++ ": {"
  ++ (L.intercalate ", " $ fmap (\(k, v) -> printProperty printer k v) $ M.toList props)
  ++ "})"
