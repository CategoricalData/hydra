module Hydra.Tools.PropertyGraphToRdf where

import Hydra.Flows
import qualified Hydra.Langs.Tinkerpop.PropertyGraph as PG
import qualified Hydra.Langs.Rdf.Syntax as Rdf
import qualified Hydra.Langs.Rdf.Utils as RdfUt

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Monad as CM


data PropertyGraphRdfHelper m v e p = PropertyGraphRdfHelper {
    encodeEdgeId :: e -> GraphFlow m Rdf.Iri,
    encodeEdgeLabel :: PG.EdgeLabel -> GraphFlow m Rdf.Iri,
    encodePropertyKey :: PG.PropertyKey -> GraphFlow m Rdf.Iri,
    encodePropertyValue :: p -> GraphFlow m Rdf.Literal,
    encodeVertexId :: v -> GraphFlow m Rdf.Iri,
    encodeVertexLabel :: PG.VertexLabel -> GraphFlow m Rdf.Iri}

encodeEdge ::  PropertyGraphRdfHelper m v e p -> PG.Edge v e p -> GraphFlow m Rdf.Description
encodeEdge helper edge = do
    subj <- Rdf.ResourceIri <$> encodeVertexId helper eout
    obj <-  Rdf.NodeIri <$> encodeVertexId helper ein
    pred <- encodeEdgeLabel helper elab
    return $ Rdf.Description (RdfUt.resourceToNode subj) $ Rdf.Graph $ S.fromList [Rdf.Triple subj pred obj]
  where
    -- Note: edge id and edge properties are discarded. An RDF-star encoding would preserve them
    PG.Edge elab _ eout ein _ = edge

encodeVertex :: PropertyGraphRdfHelper m v e p -> PG.Vertex v p -> GraphFlow m Rdf.Description
encodeVertex helper vertex = do
    subj <- Rdf.ResourceIri <$> encodeVertexId helper vid
    rtype <- Rdf.NodeIri <$> encodeVertexLabel helper vlab
    let typeTriple = Rdf.Triple subj (RdfUt.rdfIri "type") rtype
    propTriples <- CM.mapM (toPropTriple subj) $ M.toList vprops
    let triples = typeTriple:propTriples
    return $ Rdf.Description (RdfUt.resourceToNode subj) $ Rdf.Graph $ S.fromList triples
  where
    PG.Vertex vlab vid vprops = vertex
    toPropTriple subj (key, val) = do
        pred <- encodePropertyKey helper key
        obj <- Rdf.NodeLiteral <$> encodePropertyValue helper val
        return $ Rdf.Triple subj pred obj
