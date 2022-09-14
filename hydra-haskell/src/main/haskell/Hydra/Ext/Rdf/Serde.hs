-- | Serialize RDF using the N-triples format

module Hydra.Ext.Rdf.Serde (
  rdfGraphToString,
) where

import qualified Hydra.Ext.Rdf.Syntax as Rdf
import Hydra.Util.Codetree.Script
import qualified Hydra.Util.Codetree.Ast as CT

import qualified Data.Set as S


rdfGraphToString :: Rdf.Graph -> String
rdfGraphToString = printExpr . writeGraph

writeBlankNode :: Rdf.BlankNode -> CT.Expr
writeBlankNode bnode = noSep [cst "_:", cst $ Rdf.unBlankNode bnode]

writeGraph :: Rdf.Graph -> CT.Expr
writeGraph g = newlineSep (writeTriple <$> (S.toList $ Rdf.unGraph g))

writeIri :: Rdf.Iri -> CT.Expr
writeIri iri = noSep [cst "<", cst $ Rdf.unIri iri, cst ">"]

-- | Caution: no support for escaping or extended characters at this time
writeLiteral :: Rdf.Literal -> CT.Expr
writeLiteral lit = noSep [cst "\"", cst lex, suffix, cst "\""]
  where
    suffix = case Rdf.literalLanguageTag lit of
      Nothing -> noSep [cst "^^", writeIri dt]
      Just lang -> noSep [cst "@", cst $ Rdf.unLanguageTag lang]
    lex = Rdf.literalLexicalForm lit
    dt = Rdf.literalDatatypeIri lit

writeNode :: Rdf.Node -> CT.Expr
writeNode n = case n of
  Rdf.NodeIri iri -> writeIri iri
  Rdf.NodeBnode bnode -> writeBlankNode bnode
  Rdf.NodeLiteral lit -> writeLiteral lit

writeResource :: Rdf.Resource -> CT.Expr
writeResource r = case r of
  Rdf.ResourceIri iri -> writeIri iri
  Rdf.ResourceBnode bnode -> writeBlankNode bnode

writeTriple :: Rdf.Triple -> CT.Expr
writeTriple t = spaceSep [
    writeResource $ Rdf.tripleSubject t,
    writeIri $ Rdf.triplePredicate t,
    writeNode $ Rdf.tripleObject t,
    cst "."]
