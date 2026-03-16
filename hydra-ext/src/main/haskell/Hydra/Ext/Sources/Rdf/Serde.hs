module Hydra.Ext.Sources.Rdf.Serde where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Meta.Accessors                  as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars       as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan         as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import Hydra.Ast
import qualified Hydra.Ext.Org.W3.Rdf.Syntax as Rdf
import qualified Hydra.Ext.Sources.Rdf.Syntax as RdfSyntax


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.rdf.serde"

module_ :: Module
module_ = Module ns elements
    [Serialization.ns]
    (RdfSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Serialization functions for converting RDF graphs to N-Triples format expressions"
  where
    elements = [
      toBinding escapeIriChar,
      toBinding escapeIriStr,
      toBinding escapeLiteralChar,
      toBinding escapeLiteralString,
      toBinding rdfGraphToNtriples,
      toBinding writeBlankNode,
      toBinding writeGraph,
      toBinding writeIri,
      toBinding writeLanguageTag,
      toBinding writeLiteral,
      toBinding writeNode,
      toBinding writeResource,
      toBinding writeTriple]


-- | Escape a single IRI character (as char code). Characters outside printable ASCII or in the
--   set <>"{}|^`\ are replaced with "?"
escapeIriChar :: TBinding (Int -> String)
escapeIriChar = define "escapeIriChar" $
  doc "Escape a single IRI character code to a string" $
  lambda "c" $
    Logic.ifElse
      (Logic.or (Equality.gte (var "c") (int32 128)) $
        Logic.or (Equality.lte (var "c") (int32 32)) $
          Logic.or (Equality.equal (var "c") (int32 60))   -- '<'
          $ Logic.or (Equality.equal (var "c") (int32 62))   -- '>'
          $ Logic.or (Equality.equal (var "c") (int32 34))   -- '"'
          $ Logic.or (Equality.equal (var "c") (int32 123))  -- '{'
          $ Logic.or (Equality.equal (var "c") (int32 125))  -- '}'
          $ Logic.or (Equality.equal (var "c") (int32 124))  -- '|'
          $ Logic.or (Equality.equal (var "c") (int32 94))   -- '^'
          $ Logic.or (Equality.equal (var "c") (int32 96))   -- '`'
          $ Equality.equal (var "c") (int32 92))             -- '\\'
      (string "?")
      (Strings.fromList $ list [var "c"])

escapeIriStr :: TBinding (String -> String)
escapeIriStr = define "escapeIriStr" $
  doc "Escape a string for use in an IRI. Non-printable and special characters are replaced with ?" $
  lambda "s" $
    Strings.cat (Lists.map escapeIriChar (Strings.toList (var "s")))

-- | Escape a single literal character. Handles \", \\, \n, \r, and non-ASCII
escapeLiteralChar :: TBinding (Int -> String)
escapeLiteralChar = define "escapeLiteralChar" $
  doc "Escape a single literal character code to a string" $
  lambda "c" $
    Logic.ifElse (Equality.gte (var "c") (int32 128))
      (string "?")
      (Logic.ifElse (Equality.equal (var "c") (int32 34))   -- '"'
        (string "\\\"")
        (Logic.ifElse (Equality.equal (var "c") (int32 92))  -- '\\'
          (string "\\\\")
          (Logic.ifElse (Equality.equal (var "c") (int32 10))  -- '\n'
            (string "\\n")
            (Logic.ifElse (Equality.equal (var "c") (int32 13))  -- '\r'
              (string "\\r")
              (Strings.fromList $ list [var "c"])))))

escapeLiteralString :: TBinding (String -> String)
escapeLiteralString = define "escapeLiteralString" $
  doc "Escape a string for use in an N-Triples literal" $
  lambda "s" $
    Strings.cat (Lists.map escapeLiteralChar (Strings.toList (var "s")))

-- | Convert an RDF graph to an N-Triples string
rdfGraphToNtriples :: TBinding (Rdf.Graph -> String)
rdfGraphToNtriples = define "rdfGraphToNtriples" $
  doc "Convert an RDF graph to an N-Triples string" $
  lambda "g" $
    Serialization.printExpr @@ (writeGraph @@ var "g")

writeBlankNode :: TBinding (Rdf.BlankNode -> Expr)
writeBlankNode = define "writeBlankNode" $
  doc "Convert a blank node to an expression" $
  lambda "bnode" $
    Serialization.noSep @@ list [
      Serialization.cst @@ string "_:",
      Serialization.cst @@ (unwrap Rdf._BlankNode @@ var "bnode")]

writeGraph :: TBinding (Rdf.Graph -> Expr)
writeGraph = define "writeGraph" $
  doc "Convert an RDF graph to an expression" $
  lambda "g" $
    Serialization.newlineSep @@ (Lists.map writeTriple (Sets.toList (unwrap Rdf._Graph @@ var "g")))

writeIri :: TBinding (Rdf.Iri -> Expr)
writeIri = define "writeIri" $
  doc "Convert an IRI to an expression" $
  lambda "iri" $
    Serialization.noSep @@ list [
      Serialization.cst @@ string "<",
      Serialization.cst @@ (escapeIriStr @@ (unwrap Rdf._Iri @@ var "iri")),
      Serialization.cst @@ string ">"]

writeLanguageTag :: TBinding (Rdf.LanguageTag -> Expr)
writeLanguageTag = define "writeLanguageTag" $
  doc "Convert a language tag to an expression" $
  lambda "lang" $
    Serialization.noSep @@ list [
      Serialization.cst @@ string "@",
      Serialization.cst @@ (unwrap Rdf._LanguageTag @@ var "lang")]

writeLiteral :: TBinding (Rdf.Literal -> Expr)
writeLiteral = define "writeLiteral" $
  doc "Convert a literal to an expression" $
  lambda "lit" $ lets [
    "lex">: project Rdf._Literal Rdf._Literal_lexicalForm @@ var "lit",
    "dt">: project Rdf._Literal Rdf._Literal_datatypeIri @@ var "lit",
    "lang">: project Rdf._Literal Rdf._Literal_languageTag @@ var "lit",
    "lexExpr">: Serialization.cst @@
      (Strings.cat $ list [string "\"", escapeLiteralString @@ var "lex", string "\""]),
    "suffix">: Maybes.maybe
      (Serialization.noSep @@ list [Serialization.cst @@ string "^^", writeIri @@ var "dt"])
      writeLanguageTag
      (var "lang")] $
    Serialization.noSep @@ list [var "lexExpr", var "suffix"]

writeNode :: TBinding (Rdf.Node -> Expr)
writeNode = define "writeNode" $
  doc "Convert a node to an expression" $
  lambda "n" $
    cases Rdf._Node (var "n") Nothing [
      Rdf._Node_iri>>: lambda "iri" $ writeIri @@ var "iri",
      Rdf._Node_bnode>>: lambda "bnode" $ writeBlankNode @@ var "bnode",
      Rdf._Node_literal>>: lambda "lit" $ writeLiteral @@ var "lit"]

writeResource :: TBinding (Rdf.Resource -> Expr)
writeResource = define "writeResource" $
  doc "Convert a resource to an expression" $
  lambda "r" $
    cases Rdf._Resource (var "r") Nothing [
      Rdf._Resource_iri>>: lambda "iri" $ writeIri @@ var "iri",
      Rdf._Resource_bnode>>: lambda "bnode" $ writeBlankNode @@ var "bnode"]

writeTriple :: TBinding (Rdf.Triple -> Expr)
writeTriple = define "writeTriple" $
  doc "Convert a triple to an expression" $
  lambda "t" $ lets [
    "subj">: project Rdf._Triple Rdf._Triple_subject @@ var "t",
    "pred">: project Rdf._Triple Rdf._Triple_predicate @@ var "t",
    "obj">: project Rdf._Triple Rdf._Triple_object @@ var "t"] $
    Serialization.spaceSep @@ list [
      writeResource @@ var "subj",
      writeIri @@ var "pred",
      writeNode @@ var "obj",
      Serialization.cst @@ string "."]
