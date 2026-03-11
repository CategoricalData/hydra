-- Note: this is an automatically generated file. Do not edit.

-- | Serialization functions for converting RDF graphs to N-Triples format expressions

module Hydra.Ext.Rdf.Serde where

import qualified Hydra.Ast as Ast
import qualified Hydra.Ext.Org.W3.Rdf.Syntax as Syntax
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Escape a single IRI character code to a string
escapeIriChar :: (Int -> String)
escapeIriChar c = (Logic.ifElse (Logic.or (Equality.gte c 128) (Logic.or (Equality.lte c 32) (Logic.or (Equality.equal c 60) (Logic.or (Equality.equal c 62) (Logic.or (Equality.equal c 34) (Logic.or (Equality.equal c 123) (Logic.or (Equality.equal c 125) (Logic.or (Equality.equal c 124) (Logic.or (Equality.equal c 94) (Logic.or (Equality.equal c 96) (Equality.equal c 92))))))))))) "?" (Strings.fromList [
  c]))

-- | Escape a string for use in an IRI. Non-printable and special characters are replaced with ?
escapeIriStr :: (String -> String)
escapeIriStr s = (Strings.cat (Lists.map escapeIriChar (Strings.toList s)))

-- | Escape a single literal character code to a string
escapeLiteralChar :: (Int -> String)
escapeLiteralChar c = (Logic.ifElse (Equality.gte c 128) "?" (Logic.ifElse (Equality.equal c 34) "\\\"" (Logic.ifElse (Equality.equal c 92) "\\\\" (Logic.ifElse (Equality.equal c 10) "\\n" (Logic.ifElse (Equality.equal c 13) "\\r" (Strings.fromList [
  c]))))))

-- | Escape a string for use in an N-Triples literal
escapeLiteralString :: (String -> String)
escapeLiteralString s = (Strings.cat (Lists.map escapeLiteralChar (Strings.toList s)))

-- | Convert an RDF graph to an N-Triples string
rdfGraphToNtriples :: (Syntax.Graph -> String)
rdfGraphToNtriples g = (Serialization.printExpr (writeGraph g))

-- | Convert a blank node to an expression
writeBlankNode :: (Syntax.BlankNode -> Ast.Expr)
writeBlankNode bnode = (Serialization.noSep [
  Serialization.cst "_:",
  (Serialization.cst (Syntax.unBlankNode bnode))])

-- | Convert an RDF graph to an expression
writeGraph :: (Syntax.Graph -> Ast.Expr)
writeGraph g = (Serialization.newlineSep (Lists.map writeTriple (Sets.toList (Syntax.unGraph g))))

-- | Convert an IRI to an expression
writeIri :: (Syntax.Iri -> Ast.Expr)
writeIri iri = (Serialization.noSep [
  Serialization.cst "<",
  (Serialization.cst (escapeIriStr (Syntax.unIri iri))),
  (Serialization.cst ">")])

-- | Convert a language tag to an expression
writeLanguageTag :: (Syntax.LanguageTag -> Ast.Expr)
writeLanguageTag lang = (Serialization.noSep [
  Serialization.cst "@",
  (Serialization.cst (Syntax.unLanguageTag lang))])

-- | Convert a literal to an expression
writeLiteral :: (Syntax.Literal -> Ast.Expr)
writeLiteral lit =  
  let lex = (Syntax.literalLexicalForm lit) 
      dt = (Syntax.literalDatatypeIri lit)
      lang = (Syntax.literalLanguageTag lit)
      lexExpr = (Serialization.cst (Strings.cat [
              "\"",
              (escapeLiteralString lex),
              "\""]))
      suffix = (Maybes.maybe (Serialization.noSep [
              Serialization.cst "^^",
              (writeIri dt)]) writeLanguageTag lang)
  in (Serialization.noSep [
    lexExpr,
    suffix])

-- | Convert a node to an expression
writeNode :: (Syntax.Node -> Ast.Expr)
writeNode n = ((\x -> case x of
  Syntax.NodeIri v0 -> (writeIri v0)
  Syntax.NodeBnode v0 -> (writeBlankNode v0)
  Syntax.NodeLiteral v0 -> (writeLiteral v0)) n)

-- | Convert a resource to an expression
writeResource :: (Syntax.Resource -> Ast.Expr)
writeResource r = ((\x -> case x of
  Syntax.ResourceIri v0 -> (writeIri v0)
  Syntax.ResourceBnode v0 -> (writeBlankNode v0)) r)

-- | Convert a triple to an expression
writeTriple :: (Syntax.Triple -> Ast.Expr)
writeTriple t =  
  let subj = (Syntax.tripleSubject t) 
      pred = (Syntax.triplePredicate t)
      obj = (Syntax.tripleObject t)
  in (Serialization.spaceSep [
    writeResource subj,
    (writeIri pred),
    (writeNode obj),
    (Serialization.cst ".")])
