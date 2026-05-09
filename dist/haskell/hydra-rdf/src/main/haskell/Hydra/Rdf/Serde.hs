-- Note: this is an automatically generated file. Do not edit.
-- | Serialization functions for converting RDF graphs to N-Triples format expressions

module Hydra.Rdf.Serde where
import qualified Hydra.Ast as Ast
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Rdf.Syntax as Syntax
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Convert a blank node to an expression
blankNodeToExpr :: Syntax.BlankNode -> Ast.Expr
blankNodeToExpr bnode =
    Serialization.noSep [
      Serialization.cst "_:",
      (Serialization.cst (Syntax.unBlankNode bnode))]
-- | Escape a single IRI character code to a string
escapeIriChar :: Int -> String
escapeIriChar c =
    Logic.ifElse (Logic.or (Equality.lte c 32) (Logic.or (Equality.equal c 60) (Logic.or (Equality.equal c 62) (Logic.or (Equality.equal c 34) (Logic.or (Equality.equal c 123) (Logic.or (Equality.equal c 125) (Logic.or (Equality.equal c 124) (Logic.or (Equality.equal c 94) (Logic.or (Equality.equal c 96) (Equality.equal c 92)))))))))) (uchar4 c) (Strings.fromList [
      c])
-- | Escape a string for use in an N-Triples IRI. Disallowed characters are emitted as UCHAR (\uXXXX).
escapeIriStr :: String -> String
escapeIriStr s = Strings.cat (Lists.map escapeIriChar (Strings.toList s))
-- | Escape a single literal character code to a string
escapeLiteralChar :: Int -> String
escapeLiteralChar c =
    Logic.ifElse (Equality.equal c 34) "\\\"" (Logic.ifElse (Equality.equal c 92) "\\\\" (Logic.ifElse (Equality.equal c 10) "\\n" (Logic.ifElse (Equality.equal c 13) "\\r" (Strings.fromList [
      c]))))
-- | Escape a string for use in an N-Triples literal
escapeLiteralString :: String -> String
escapeLiteralString s = Strings.cat (Lists.map escapeLiteralChar (Strings.toList s))
-- | Convert an RDF graph to an expression
graphToExpr :: Syntax.Graph -> Ast.Expr
graphToExpr g = Serialization.newlineSep (Lists.map tripleToExpr (Sets.toList (Syntax.unGraph g)))
-- | Convert a value 0-15 to an uppercase hex digit code point
hexDigit :: Int -> Int
hexDigit n = Logic.ifElse (Equality.lt n 10) (Math.add n 48) (Math.add (Math.sub n 10) 65)
-- | Convert an IRI to an expression
iriToExpr :: Syntax.Iri -> Ast.Expr
iriToExpr iri =
    Serialization.noSep [
      Serialization.cst "<",
      (Serialization.cst (escapeIriStr (Syntax.unIri iri))),
      (Serialization.cst ">")]
-- | Convert a language tag to an expression
languageTagToExpr :: Syntax.LanguageTag -> Ast.Expr
languageTagToExpr lang =
    Serialization.noSep [
      Serialization.cst "@",
      (Serialization.cst (Syntax.unLanguageTag lang))]
-- | Convert a literal to an expression
literalToExpr :: Syntax.Literal -> Ast.Expr
literalToExpr lit =

      let lex = Syntax.literalLexicalForm lit
          dt = Syntax.literalDatatypeIri lit
          lang = Syntax.literalLanguageTag lit
          lexExpr =
                  Serialization.cst (Strings.cat [
                    "\"",
                    (escapeLiteralString lex),
                    "\""])
          suffix =
                  Maybes.maybe (Serialization.noSep [
                    Serialization.cst "^^",
                    (iriToExpr dt)]) languageTagToExpr lang
      in (Serialization.noSep [
        lexExpr,
        suffix])
-- | Convert a node to an expression
nodeToExpr :: Syntax.Node -> Ast.Expr
nodeToExpr n =
    case n of
      Syntax.NodeIri v0 -> iriToExpr v0
      Syntax.NodeBnode v0 -> blankNodeToExpr v0
      Syntax.NodeLiteral v0 -> literalToExpr v0
-- | Convert an RDF graph to an N-Triples string
rdfGraphToNtriples :: Syntax.Graph -> String
rdfGraphToNtriples g = Serialization.printExpr (graphToExpr g)
-- | Convert a resource to an expression
resourceToExpr :: Syntax.Resource -> Ast.Expr
resourceToExpr r =
    case r of
      Syntax.ResourceIri v0 -> iriToExpr v0
      Syntax.ResourceBnode v0 -> blankNodeToExpr v0
-- | Convert a triple to an expression
tripleToExpr :: Syntax.Triple -> Ast.Expr
tripleToExpr t =

      let subj = Syntax.tripleSubject t
          pred = Syntax.triplePredicate t
          obj = Syntax.tripleObject t
      in (Serialization.spaceSep [
        resourceToExpr subj,
        (iriToExpr pred),
        (nodeToExpr obj),
        (Serialization.cst ".")])
-- | Format a code point as a 4-digit UCHAR escape sequence
uchar4 :: Int -> String
uchar4 c =

      let d3 = Maybes.fromMaybe 0 (Math.maybeDiv c 4096)
          r3 = Maybes.fromMaybe 0 (Math.maybeMod c 4096)
          d2 = Maybes.fromMaybe 0 (Math.maybeDiv r3 256)
          r2 = Maybes.fromMaybe 0 (Math.maybeMod r3 256)
          d1 = Maybes.fromMaybe 0 (Math.maybeDiv r2 16)
          d0 = Maybes.fromMaybe 0 (Math.maybeMod r2 16)
      in (Strings.cat2 "\\u" (Strings.fromList [
        hexDigit d3,
        (hexDigit d2),
        (hexDigit d1),
        (hexDigit d0)]))
