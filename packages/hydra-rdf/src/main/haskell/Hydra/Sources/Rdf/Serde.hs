module Hydra.Sources.Rdf.Serde where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
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
import qualified Hydra.Dsl.Packaging                     as Packaging
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
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants  as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
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
import qualified Hydra.Rdf.Syntax as Rdf
import qualified Hydra.Sources.Rdf.Syntax as RdfSyntax


define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

ns :: ModuleName
ns = ModuleName "hydra.rdf.serde"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Serialization.ns] L.++ (RdfSyntax.ns:KernelTypes.kernelTypesModuleNames)),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Serialization functions for converting RDF graphs to N-Triples format expressions")}
  where
    definitions = [
      toDefinition escapeIriChar,
      toDefinition escapeIriStr,
      toDefinition escapeLiteralChar,
      toDefinition escapeLiteralString,
      toDefinition hexDigit,
      toDefinition rdfGraphToNtriples,
      toDefinition uchar4,
      toDefinition blankNodeToExpr,
      toDefinition graphToExpr,
      toDefinition iriToExpr,
      toDefinition languageTagToExpr,
      toDefinition literalToExpr,
      toDefinition nodeToExpr,
      toDefinition resourceToExpr,
      toDefinition tripleToExpr]


blankNodeToExpr :: TypedTermDefinition (Rdf.BlankNode -> Expr)
blankNodeToExpr = define "blankNodeToExpr" $
  doc "Convert a blank node to an expression" $
  lambda "bnode" $
    Serialization.noSep @@ list [
      Serialization.cst @@ string "_:",
      Serialization.cst @@ (unwrap Rdf._BlankNode @@ var "bnode")]

-- | Escape a single IRI character (as code point). The N-Triples IRIREF
--   production excludes #x00-#x20 and <>"{}|^`\\; those characters are emitted
--   as UCHAR (\\u00XX). All other code points (including all of Unicode >= 0x80)
--   pass through verbatim.
escapeIriChar :: TypedTermDefinition (Int -> String)
escapeIriChar = define "escapeIriChar" $
  doc "Escape a single IRI character code to a string" $
  lambda "c" $
    Logic.ifElse
      (Logic.or (Equality.lte (var "c") (int32 32)) $
          Logic.or (Equality.equal (var "c") (int32 60))   -- '<'
          $ Logic.or (Equality.equal (var "c") (int32 62))   -- '>'
          $ Logic.or (Equality.equal (var "c") (int32 34))   -- '"'
          $ Logic.or (Equality.equal (var "c") (int32 123))  -- '{'
          $ Logic.or (Equality.equal (var "c") (int32 125))  -- '}'
          $ Logic.or (Equality.equal (var "c") (int32 124))  -- '|'
          $ Logic.or (Equality.equal (var "c") (int32 94))   -- '^'
          $ Logic.or (Equality.equal (var "c") (int32 96))   -- '`'
          $ Equality.equal (var "c") (int32 92))             -- '\\'
      (uchar4 @@ var "c")
      (Strings.fromList $ list [var "c"])

escapeIriStr :: TypedTermDefinition (String -> String)
escapeIriStr = define "escapeIriStr" $
  doc "Escape a string for use in an N-Triples IRI. Disallowed characters are emitted as 4-digit UCHAR escapes." $
  lambda "s" $
    Strings.cat (Lists.map escapeIriChar (Strings.toList (var "s")))

-- | Escape a single literal character. Handles \", \\, \n, \r;
--   non-ASCII code points pass through (N-Triples allows any Unicode code point in literals).
escapeLiteralChar :: TypedTermDefinition (Int -> String)
escapeLiteralChar = define "escapeLiteralChar" $
  doc "Escape a single literal character code to a string" $
  lambda "c" $
    Logic.ifElse (Equality.equal (var "c") (int32 34))   -- '"'
      (string "\\\"")
      (Logic.ifElse (Equality.equal (var "c") (int32 92))  -- '\\'
        (string "\\\\")
        (Logic.ifElse (Equality.equal (var "c") (int32 10))  -- '\n'
          (string "\\n")
          (Logic.ifElse (Equality.equal (var "c") (int32 13))  -- '\r'
            (string "\\r")
            (Strings.fromList $ list [var "c"]))))

escapeLiteralString :: TypedTermDefinition (String -> String)
escapeLiteralString = define "escapeLiteralString" $
  doc "Escape a string for use in an N-Triples literal" $
  lambda "s" $
    Strings.cat (Lists.map escapeLiteralChar (Strings.toList (var "s")))

graphToExpr :: TypedTermDefinition (Rdf.Graph -> Expr)
graphToExpr = define "graphToExpr" $
  doc "Convert an RDF graph to an expression" $
  lambda "g" $
    Serialization.newlineSep @@ (Lists.map tripleToExpr (Sets.toList (unwrap Rdf._Graph @@ var "g")))

-- | Convert a value 0-15 to an uppercase hex digit code point ('0'-'9' or 'A'-'F').
hexDigit :: TypedTermDefinition (Int -> Int)
hexDigit = define "hexDigit" $
  doc "Convert a value 0-15 to an uppercase hex digit code point" $
  lambda "n" $
    Logic.ifElse (Equality.lt (var "n") (int32 10))
      (Math.add (var "n") (int32 48))                        -- '0'
      (Math.add (Math.sub (var "n") (int32 10)) (int32 65))  -- 'A'

iriToExpr :: TypedTermDefinition (Rdf.Iri -> Expr)
iriToExpr = define "iriToExpr" $
  doc "Convert an IRI to an expression" $
  lambda "iri" $
    Serialization.noSep @@ list [
      Serialization.cst @@ string "<",
      Serialization.cst @@ (escapeIriStr @@ (unwrap Rdf._Iri @@ var "iri")),
      Serialization.cst @@ string ">"]

languageTagToExpr :: TypedTermDefinition (Rdf.LanguageTag -> Expr)
languageTagToExpr = define "languageTagToExpr" $
  doc "Convert a language tag to an expression" $
  lambda "lang" $
    Serialization.noSep @@ list [
      Serialization.cst @@ string "@",
      Serialization.cst @@ (unwrap Rdf._LanguageTag @@ var "lang")]

literalToExpr :: TypedTermDefinition (Rdf.Literal -> Expr)
literalToExpr = define "literalToExpr" $
  doc "Convert a literal to an expression" $
  lambda "lit" $ lets [
    "lex">: project Rdf._Literal Rdf._Literal_lexicalForm @@ var "lit",
    "dt">: project Rdf._Literal Rdf._Literal_datatypeIri @@ var "lit",
    "lang">: project Rdf._Literal Rdf._Literal_languageTag @@ var "lit",
    "lexExpr">: Serialization.cst @@
      (Strings.cat $ list [string "\"", escapeLiteralString @@ var "lex", string "\""]),
    "suffix">: Maybes.maybe
      (Serialization.noSep @@ list [Serialization.cst @@ string "^^", iriToExpr @@ var "dt"])
      languageTagToExpr
      (var "lang")] $
    Serialization.noSep @@ list [var "lexExpr", var "suffix"]

nodeToExpr :: TypedTermDefinition (Rdf.Node -> Expr)
nodeToExpr = define "nodeToExpr" $
  doc "Convert a node to an expression" $
  lambda "n" $
    cases Rdf._Node (var "n") Nothing [
      Rdf._Node_iri>>: lambda "iri" $ iriToExpr @@ var "iri",
      Rdf._Node_bnode>>: lambda "bnode" $ blankNodeToExpr @@ var "bnode",
      Rdf._Node_literal>>: lambda "lit" $ literalToExpr @@ var "lit"]

-- | Convert an RDF graph to an N-Triples string
rdfGraphToNtriples :: TypedTermDefinition (Rdf.Graph -> String)
rdfGraphToNtriples = define "rdfGraphToNtriples" $
  doc "Convert an RDF graph to an N-Triples string" $
  lambda "g" $
    Serialization.printExpr @@ (graphToExpr @@ var "g")

resourceToExpr :: TypedTermDefinition (Rdf.Resource -> Expr)
resourceToExpr = define "resourceToExpr" $
  doc "Convert a resource to an expression" $
  lambda "r" $
    cases Rdf._Resource (var "r") Nothing [
      Rdf._Resource_iri>>: lambda "iri" $ iriToExpr @@ var "iri",
      Rdf._Resource_bnode>>: lambda "bnode" $ blankNodeToExpr @@ var "bnode"]

tripleToExpr :: TypedTermDefinition (Rdf.Triple -> Expr)
tripleToExpr = define "tripleToExpr" $
  doc "Convert a triple to an expression" $
  lambda "t" $ lets [
    "subj">: project Rdf._Triple Rdf._Triple_subject @@ var "t",
    "pred">: project Rdf._Triple Rdf._Triple_predicate @@ var "t",
    "obj">: project Rdf._Triple Rdf._Triple_object @@ var "t"] $
    Serialization.spaceSep @@ list [
      resourceToExpr @@ var "subj",
      iriToExpr @@ var "pred",
      nodeToExpr @@ var "obj",
      Serialization.cst @@ string "."]

-- | Encode a code point in the range 0..0xFFFF as a 4-digit UCHAR (\\uXXXX).
--   Inputs above 0xFFFF would need the 8-digit \\U form; this helper is used only
--   for the IRIREF disallowed set, which is entirely <= 0x7F.
uchar4 :: TypedTermDefinition (Int -> String)
uchar4 = define "uchar4" $
  doc "Format a code point as a 4-digit UCHAR escape sequence" $
  lambda "c" $
    "d3" <~ Maybes.fromMaybe (int32 0) (Math.maybeDiv (var "c") (int32 4096)) $    -- c / 16^3
    "r3" <~ Maybes.fromMaybe (int32 0) (Math.maybeMod (var "c") (int32 4096)) $
    "d2" <~ Maybes.fromMaybe (int32 0) (Math.maybeDiv (var "r3") (int32 256)) $    -- r3 / 16^2
    "r2" <~ Maybes.fromMaybe (int32 0) (Math.maybeMod (var "r3") (int32 256)) $
    "d1" <~ Maybes.fromMaybe (int32 0) (Math.maybeDiv (var "r2") (int32 16)) $     -- r2 / 16
    "d0" <~ Maybes.fromMaybe (int32 0) (Math.maybeMod (var "r2") (int32 16)) $
    Strings.cat2 (string "\\u")
      (Strings.fromList $ list [hexDigit @@ var "d3", hexDigit @@ var "d2",
                                hexDigit @@ var "d1", hexDigit @@ var "d0"])
