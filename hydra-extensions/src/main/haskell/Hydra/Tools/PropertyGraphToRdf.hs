module Hydra.Tools.PropertyGraphToRdf where

import Hydra.Flows
import qualified Hydra.Langs.Tinkerpop.PropertyGraph as PG
import qualified Hydra.Langs.Rdf.Syntax as Rdf
import qualified Hydra.Langs.Rdf.Utils as RdfUt

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Monad as CM


data PropertyGraphRdfHelper a v = PropertyGraphRdfHelper {
    encodeEdgeId :: v -> GraphFlow a Rdf.Iri,
    encodeEdgeLabel :: PG.EdgeLabel -> GraphFlow a Rdf.Iri,
    encodePropertyKey :: PG.PropertyKey -> GraphFlow a Rdf.Iri,
    encodePropertyValue :: v -> GraphFlow a Rdf.Literal,
    encodeVertexId :: v -> GraphFlow a Rdf.Iri,
    encodeVertexLabel :: PG.VertexLabel -> GraphFlow a Rdf.Iri}

encodeEdge ::  PropertyGraphRdfHelper a v -> PG.Edge v -> GraphFlow a Rdf.Description
encodeEdge helper edge = do
    subj <- Rdf.ResourceIri <$> encodeVertexId helper eout
    obj <-  Rdf.NodeIri <$> encodeVertexId helper ein
    pred <- encodeEdgeLabel helper elab
    return $ Rdf.Description (RdfUt.resourceToNode subj) $ Rdf.Graph $ S.fromList [Rdf.Triple subj pred obj]
  where
    -- Note: edge id and edge properties are discarded. An RDF-star encoding would preserve them
    PG.Edge elab _ eout ein _ = edge

encodeVertex :: PropertyGraphRdfHelper a v -> PG.Vertex v -> GraphFlow a Rdf.Description
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
