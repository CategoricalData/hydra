-- | Serialize RDF using an approximation (because it does not yet support Unicode escape sequences) of the N-triples format

module Hydra.Ext.Rdf.Serde (
  rdfGraphToNtriples,
) where

import Hydra.Tools.Serialization
import qualified Hydra.Ext.Rdf.Syntax as Rdf
import qualified Hydra.Ast as CT

import qualified Data.List as L
import qualified Data.Set as S


-- IRIREF ::= '<' ([^#x00-#x20<>"{}|^`\] | UCHAR)* '>'
-- TODO: Unicode escape sequences
escapeIriStr :: String -> String
escapeIriStr s = L.concat (esc <$> s)
  where
    esc c = if c >= '\128' || c <= '\32' || S.member c others
      then "?"
      else [c]
    others = S.fromList $ "<>\"{}|^`\\"

-- STRING_LITERAL_QUOTE ::= '"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'
-- TODO: Unicode escape sequences
escapeLiteralString :: String -> String
escapeLiteralString s = L.concat (esc <$> s)
  where
    esc c = if c >= '\128'
      then "?"
      else case c of
        '\"' -> "\\\""
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\r' -> "\\r"
        _ -> [c]

rdfGraphToNtriples :: Rdf.Graph -> String
rdfGraphToNtriples = printExpr . writeGraph

writeBlankNode :: Rdf.BlankNode -> CT.Expr
writeBlankNode bnode = noSep [cst "_:", cst $ Rdf.unBlankNode bnode]

writeGraph :: Rdf.Graph -> CT.Expr
writeGraph g = newlineSep (writeTriple <$> (S.toList $ Rdf.unGraph g))

writeIri :: Rdf.Iri -> CT.Expr
writeIri iri = noSep [cst "<", cst $ escapeIriStr $ Rdf.unIri iri, cst ">"]

-- LANGTAG ::= '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
-- Note: we simply trust language tags to be valid
writeLanguageTag :: Rdf.LanguageTag -> CT.Expr
writeLanguageTag lang = noSep [cst "@", cst $ Rdf.unLanguageTag lang]

writeLiteral :: Rdf.Literal -> CT.Expr
writeLiteral lit = noSep [cst lex, suffix]
  where
    suffix = case Rdf.literalLanguageTag lit of
      Nothing -> noSep [cst "^^", writeIri dt]
      Just lang -> writeLanguageTag lang
    lex = "\"" ++ (escapeLiteralString $ Rdf.literalLexicalForm lit) ++ "\""
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
