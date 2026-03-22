module Hydra.Ext.Sources.Pg.TermsToElements where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (
  applyPattern, decodeEdgeLabel, decodeEdgeSpec, decodeElementSpec,
  decodePropertyKey, decodePropertySpec, decodeValueSpec, decodeVertexLabel,
  decodeVertexSpec, evalPath, evalStep, expectList, parseEdgeIdPattern,
  parseEdgeSpec, parseElementSpec, parsePattern, parsePropertySpec,
  parseValueSpec, parseVertexIdPattern, parseVertexSpec, readField,
  readInjection, readRecord, requireUnique, termToElementsAdapter,
  termToString)
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Accessors                  as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Error                      as Error
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
import qualified Hydra.Pg.Model as PG
import qualified Hydra.Pg.Mapping as PGM
import qualified Hydra.Ext.Sources.Pg.Model as PgModel
import qualified Hydra.Ext.Sources.Pg.Mapping as PgMapping


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.pg.termsToElements"

module_ :: Module
module_ = Module ns elements
    [Annotations.ns, ExtractCore.ns, Rewriting.ns, Schemas.ns, ShowCore.ns]
    (PgModel.ns:PgMapping.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Functions for mapping Hydra terms to property graph elements using mapping specifications"
  where
    elements = [
      toBinding applyPattern,
      toBinding decodeEdgeLabel,
      toBinding decodeEdgeSpec,
      toBinding decodeElementSpec,
      toBinding decodePropertyKey,
      toBinding decodePropertySpec,
      toBinding decodeValueSpec,
      toBinding decodeVertexLabel,
      toBinding decodeVertexSpec,
      toBinding evalPath,
      toBinding evalStep,
      toBinding expectList,
      toBinding parseEdgeIdPattern,
      toBinding parseEdgeSpec,
      toBinding parseElementSpec,
      toBinding parsePattern,
      toBinding parsePropertySpec,
      toBinding parseValueSpec,
      toBinding parseVertexIdPattern,
      toBinding parseVertexSpec,
      toBinding readField,
      toBinding readInjection,
      toBinding readRecord,
      toBinding requireUnique,
      toBinding termToElementsAdapter,
      toBinding termToString]

-- | Decode an edge label from a term
decodeEdgeLabel :: TBinding (Context -> Graph -> Term -> Either (InContext Error) PG.EdgeLabel)
decodeEdgeLabel = define "decodeEdgeLabel" $
  doc "Decode an edge label from a term" $
  "cx" ~> "g" ~> "t" ~>
    Eithers.map ("_x" ~> wrap PG._EdgeLabel (var "_x")) (ExtractCore.string @@ var "cx" @@ var "g" @@ var "t")

-- | Decode an edge specification from a term
decodeEdgeSpec :: TBinding (Context -> Graph -> Term -> Either (InContext Error) PGM.EdgeSpec)
decodeEdgeSpec = define "decodeEdgeSpec" $
  doc "Decode an edge specification from a term" $
  "cx" ~> "g" ~> "term" ~>
    (readRecord @@ var "cx" @@ var "g"
      @@ ("fields" ~>
        Eithers.bind (readField @@ var "cx" @@ var "fields" @@ (Core.name $ string "label") @@ (decodeEdgeLabel @@ var "cx" @@ var "g"))
          ("_a" ~> Eithers.bind (readField @@ var "cx" @@ var "fields" @@ (Core.name $ string "id") @@ (decodeValueSpec @@ var "cx" @@ var "g"))
            ("_b" ~> Eithers.bind (readField @@ var "cx" @@ var "fields" @@ (Core.name $ string "out") @@ (decodeValueSpec @@ var "cx" @@ var "g"))
              ("_c" ~> Eithers.bind (readField @@ var "cx" @@ var "fields" @@ (Core.name $ string "in") @@ (decodeValueSpec @@ var "cx" @@ var "g"))
                ("_d" ~> Eithers.map
                  ("_e" ~> record PGM._EdgeSpec [
                    PGM._EdgeSpec_label>>: var "_a",
                    PGM._EdgeSpec_id>>: var "_b",
                    PGM._EdgeSpec_out>>: var "_c",
                    PGM._EdgeSpec_in>>: var "_d",
                    PGM._EdgeSpec_properties>>: var "_e"])
                  (readField @@ var "cx" @@ var "fields" @@ (Core.name $ string "properties")
                    @@ (expectList @@ var "cx" @@ var "g" @@ decodePropertySpec)))))))
      @@ var "term")

-- | Decode an element specification from a term
decodeElementSpec :: TBinding (Context -> Graph -> Term -> Either (InContext Error) PGM.ElementSpec)
decodeElementSpec = define "decodeElementSpec" $
  doc "Decode an element specification from a term" $
  "cx" ~> "g" ~> "term" ~>
    readInjection @@ var "cx" @@ var "g"
      @@ list [
        pair (Core.name $ string "vertex") ("t" ~> Eithers.map ("_x" ~> inject PGM._ElementSpec PGM._ElementSpec_vertex (var "_x")) (decodeVertexSpec @@ var "cx" @@ var "g" @@ var "t")),
        pair (Core.name $ string "edge") ("t" ~> Eithers.map ("_x" ~> inject PGM._ElementSpec PGM._ElementSpec_edge (var "_x")) (decodeEdgeSpec @@ var "cx" @@ var "g" @@ var "t"))]
      @@ var "term"

-- | Decode a property key from a term
decodePropertyKey :: TBinding (Context -> Graph -> Term -> Either (InContext Error) PG.PropertyKey)
decodePropertyKey = define "decodePropertyKey" $
  doc "Decode a property key from a term" $
  "cx" ~> "g" ~> "t" ~>
    Eithers.map ("_x" ~> wrap PG._PropertyKey (var "_x")) (ExtractCore.string @@ var "cx" @@ var "g" @@ var "t")

-- | Decode a property specification from a term
decodePropertySpec :: TBinding (Context -> Graph -> Term -> Either (InContext Error) PGM.PropertySpec)
decodePropertySpec = define "decodePropertySpec" $
  doc "Decode a property specification from a term" $
  "cx" ~> "g" ~> "term" ~>
    readRecord @@ var "cx" @@ var "g"
      @@ ("fields" ~>
        Eithers.bind (readField @@ var "cx" @@ var "fields" @@ (Core.name $ string "key") @@ (decodePropertyKey @@ var "cx" @@ var "g"))
          ("_a" ~> Eithers.map
            ("_b" ~> record PGM._PropertySpec [
              PGM._PropertySpec_key>>: var "_a",
              PGM._PropertySpec_value>>: var "_b"])
            (readField @@ var "cx" @@ var "fields" @@ (Core.name $ string "value") @@ (decodeValueSpec @@ var "cx" @@ var "g"))))
      @@ var "term"

-- | Decode a value specification from a term
decodeValueSpec :: TBinding (Context -> Graph -> Term -> Either (InContext Error) PGM.ValueSpec)
decodeValueSpec = define "decodeValueSpec" $
  doc "Decode a value specification from a term" $
  "cx" ~> "g" ~> "term" ~>
    -- Allow an abbreviated specification consisting of only the pattern string
    cases _Term (Rewriting.deannotateTerm @@ var "term") (Just $
      readInjection @@ var "cx" @@ var "g"
        @@ list [
          pair (Core.name $ string "value") (constant $ right (inject PGM._ValueSpec PGM._ValueSpec_value $ unit)),
          pair (Core.name $ string "pattern") ("t" ~> Eithers.map ("_x" ~> inject PGM._ValueSpec PGM._ValueSpec_pattern (var "_x")) (ExtractCore.string @@ var "cx" @@ var "g" @@ var "t"))]
        @@ var "term") [
      _Term_literal>>: "lit" ~>
        cases _Literal (var "lit") (Just $
          readInjection @@ var "cx" @@ var "g"
            @@ list [
              pair (Core.name $ string "value") (constant $ right (inject PGM._ValueSpec PGM._ValueSpec_value $ unit)),
              pair (Core.name $ string "pattern") ("t" ~> Eithers.map ("_x" ~> inject PGM._ValueSpec PGM._ValueSpec_pattern (var "_x")) (ExtractCore.string @@ var "cx" @@ var "g" @@ var "t"))]
            @@ var "term") [
          _Literal_string>>: "s" ~>
            right (inject PGM._ValueSpec PGM._ValueSpec_pattern (var "s"))]]

-- | Decode a vertex label from a term
decodeVertexLabel :: TBinding (Context -> Graph -> Term -> Either (InContext Error) PG.VertexLabel)
decodeVertexLabel = define "decodeVertexLabel" $
  doc "Decode a vertex label from a term" $
  "cx" ~> "g" ~> "t" ~>
    Eithers.map ("_x" ~> wrap PG._VertexLabel (var "_x")) (ExtractCore.string @@ var "cx" @@ var "g" @@ var "t")

-- | Decode a vertex specification from a term
decodeVertexSpec :: TBinding (Context -> Graph -> Term -> Either (InContext Error) PGM.VertexSpec)
decodeVertexSpec = define "decodeVertexSpec" $
  doc "Decode a vertex specification from a term" $
  "cx" ~> "g" ~> "term" ~>
    readRecord @@ var "cx" @@ var "g"
      @@ ("fields" ~>
        Eithers.bind (readField @@ var "cx" @@ var "fields" @@ (Core.name $ string "label") @@ (decodeVertexLabel @@ var "cx" @@ var "g"))
          ("_a" ~> Eithers.bind (readField @@ var "cx" @@ var "fields" @@ (Core.name $ string "id") @@ (decodeValueSpec @@ var "cx" @@ var "g"))
            ("_b" ~> Eithers.map
              ("_c" ~> record PGM._VertexSpec [
                PGM._VertexSpec_label>>: var "_a",
                PGM._VertexSpec_id>>: var "_b",
                PGM._VertexSpec_properties>>: var "_c"])
              (readField @@ var "cx" @@ var "fields" @@ (Core.name $ string "properties")
                @@ (expectList @@ var "cx" @@ var "g" @@ decodePropertySpec)))))
      @@ var "term"

-- | Extract a list from a term and apply a decoder to each element
expectList :: TBinding (Context -> Graph -> (Context -> Graph -> Term -> Either (InContext Error) x) -> Term -> Either (InContext Error) [x])
expectList = define "expectList" $
  doc "Extract a list from a term and apply a decoder to each element" $
  "cx" ~> "g" ~> "f" ~> "term" ~>
    Eithers.bind (ExtractCore.list @@ var "cx" @@ var "g" @@ var "term")
      ("elems" ~> Eithers.mapList (var "f" @@ var "cx" @@ var "g") (var "elems"))

-- | Parse an edge id pattern from a value spec and schema
parseEdgeIdPattern :: TBinding (Context -> Graph -> PGM.Schema s t v -> PGM.ValueSpec -> Either (InContext Error) (Context -> Term -> Either (InContext Error) [v]))
parseEdgeIdPattern = define "parseEdgeIdPattern" $
  doc "Parse an edge id pattern from a value spec and schema" $
  "cx" ~> "g" ~> "schema" ~> "spec" ~>
    Eithers.bind (parseValueSpec @@ var "cx" @@ var "g" @@ var "spec")
      ("fun" ~> right
        ("cx'" ~> "term" ~>
          Eithers.bind (var "fun" @@ var "cx'" @@ var "term")
            ("terms" ~> Eithers.mapList (Util.coderEncode (project PGM._Schema PGM._Schema_edgeIds @@ var "schema") @@ var "cx'") (var "terms"))))

-- | Parse an edge specification into a label and encoder function
parseEdgeSpec :: TBinding (Context -> Graph -> PGM.Schema s t v -> PGM.EdgeSpec
  -> Either (InContext Error) (PG.Label, Context -> Term -> Either (InContext Error) [PG.Element v]))
parseEdgeSpec = define "parseEdgeSpec" $
  doc "Parse an edge specification into a label and encoder function" $
  "cx" ~> "g" ~> "schema" ~> "spec" ~> lets [
    "label">: project PGM._EdgeSpec PGM._EdgeSpec_label @@ var "spec",
    "id">: project PGM._EdgeSpec PGM._EdgeSpec_id @@ var "spec",
    "outV">: project PGM._EdgeSpec PGM._EdgeSpec_out @@ var "spec",
    "inV">: project PGM._EdgeSpec PGM._EdgeSpec_in @@ var "spec",
    "props">: project PGM._EdgeSpec PGM._EdgeSpec_properties @@ var "spec"] $
    Eithers.bind (parseEdgeIdPattern @@ var "cx" @@ var "g" @@ var "schema" @@ var "id")
      ("getId" ~> Eithers.bind (parseVertexIdPattern @@ var "cx" @@ var "g" @@ var "schema" @@ var "outV")
        ("getOut" ~> Eithers.bind (parseVertexIdPattern @@ var "cx" @@ var "g" @@ var "schema" @@ var "inV")
          ("getIn" ~> Eithers.bind (Eithers.mapList (parsePropertySpec @@ var "cx" @@ var "g" @@ var "schema") (var "props"))
            ("getProps" ~> right (pair
              (inject PG._Label PG._Label_edge $ var "label")
              ("cx'" ~> "term" ~>
                Eithers.bind (requireUnique @@ var "cx'" @@ string "edge id" @@ (var "getId" @@ var "cx'") @@ var "term")
                  ("tid" ~> Eithers.bind (requireUnique @@ var "cx'" @@ string "vertex id" @@ (var "getOut" @@ var "cx'") @@ var "term")
                    ("tout" ~> Eithers.bind (requireUnique @@ var "cx'" @@ string "edge id" @@ (var "getIn" @@ var "cx'") @@ var "term")
                      ("tin" ~> Eithers.bind (Eithers.map ("_xs" ~> Maps.fromList (var "_xs")) (Eithers.mapList ("gf" ~> requireUnique @@ var "cx'" @@ string "property key" @@ (var "gf" @@ var "cx'") @@ var "term") (var "getProps")))
                        ("tprops" ~> right (list [inject PG._Element PG._Element_edge
                          (record PG._Edge [
                            PG._Edge_label>>: var "label",
                            PG._Edge_id>>: var "tid",
                            PG._Edge_out>>: var "tout",
                            PG._Edge_in>>: var "tin",
                            PG._Edge_properties>>: var "tprops"])])))))))))))

-- | Parse an element specification into a label and encoder function
parseElementSpec :: TBinding (Context -> Graph -> PGM.Schema s t v -> PGM.ElementSpec
  -> Either (InContext Error) (PG.Label, Context -> Term -> Either (InContext Error) [PG.Element v]))
parseElementSpec = define "parseElementSpec" $
  doc "Parse an element specification into a label and encoder function" $
  "cx" ~> "g" ~> "schema" ~> "spec" ~>
    match PGM._ElementSpec Nothing [
      PGM._ElementSpec_vertex>>: "vspec" ~> parseVertexSpec @@ var "cx" @@ var "g" @@ var "schema" @@ var "vspec",
      PGM._ElementSpec_edge>>: "espec" ~> parseEdgeSpec @@ var "cx" @@ var "g" @@ var "schema" @@ var "espec"]
    @@ var "spec"

-- | Evaluate a single step of a path traversal on a term
evalStep :: TBinding (Context -> String -> Term -> Either (InContext Error) [Term])
evalStep = define "evalStep" $
  doc "Evaluate a single step of a path traversal on a term" $
  "cx" ~> "step" ~> "term" ~>
    Logic.ifElse (Strings.null $ var "step")
      (right (list [var "term"]))
      (cases _Term (Rewriting.deannotateTerm @@ var "term")
        (Just $ left (Ctx.inContext (Error.errorOther $ Error.otherError $ string "Can't traverse through term for step " ++ var "step") (var "cx"))) [
        _Term_list>>: "terms" ~>
          Eithers.map (lambda "xs" $ Lists.concat (var "xs")) (Eithers.mapList (evalStep @@ var "cx" @@ var "step") (var "terms")),
        _Term_maybe>>: "mt" ~>
          Maybes.maybe (right (list ([] :: [TTerm Term]))) ("t" ~> evalStep @@ var "cx" @@ var "step" @@ var "t") (var "mt"),
        _Term_record>>: "rec" ~>
          Maybes.maybe
            (left $ Ctx.inContext (Error.errorOther $ Error.otherError $ string "No such field " ++ var "step" ++ string " in record") (var "cx"))
            ("t" ~> right (list [var "t"]))
            (Maps.lookup (Core.name $ var "step") (Schemas.fieldMap @@ (Core.recordFields $ var "rec"))),
        _Term_union>>: "inj" ~>
          Logic.ifElse (Equality.equal (Core.unName $ Core.fieldName $ Core.injectionField $ var "inj") (var "step"))
            (evalStep @@ var "cx" @@ var "step" @@ (Core.fieldTerm $ Core.injectionField $ var "inj"))
            (right (list ([] :: [TTerm Term]))),
        _Term_wrap>>: "wt" ~>
          evalStep @@ var "cx" @@ var "step" @@ (Core.wrappedTermBody $ var "wt")])

-- | Evaluate a path (list of steps) on a term, returning all resulting terms
evalPath :: TBinding (Context -> [String] -> Term -> Either (InContext Error) [Term])
evalPath = define "evalPath" $
  doc "Evaluate a path (list of steps) on a term, returning all resulting terms" $
  "cx" ~> "path" ~> "term" ~>
    Logic.ifElse (Lists.null $ var "path")
      (right (list [var "term"]))
      (Eithers.bind (evalStep @@ var "cx" @@ (Lists.head $ var "path") @@ var "term")
        ("results" ~> Eithers.map (lambda "xs" $ Lists.concat (var "xs"))
          (Eithers.mapList (evalPath @@ var "cx" @@ (Lists.tail $ var "path")) (var "results"))))

-- | Convert a term to its string representation
termToString :: TBinding (Term -> String)
termToString = define "termToString" $
  doc "Convert a term to its string representation" $
  "term" ~>
    cases _Term (Rewriting.deannotateTerm @@ var "term")
      (Just $ ShowCore.term @@ var "term") [
      _Term_literal>>: "lit" ~>
        cases _Literal (var "lit") (Just $ ShowCore.term @@ var "term") [
          _Literal_string>>: lambda "s" $ var "s",
          _Literal_boolean>>: "b" ~> Logic.ifElse (var "b") (string "true") (string "false"),
          _Literal_integer>>: "i" ~>
            cases _IntegerValue (var "i") (Just $ ShowCore.term @@ var "term") [
              _IntegerValue_int32>>: "n" ~> Literals.showInt32 (var "n")],
          _Literal_float>>: "f" ~>
            cases _FloatValue (var "f") (Just $ ShowCore.term @@ var "term") [
              _FloatValue_float64>>: "n" ~> Literals.showFloat64 (var "n")]],
      _Term_maybe>>: "mt" ~>
        Maybes.maybe (string "nothing") ("t" ~> termToString @@ var "t") (var "mt")]

-- | Apply a parsed pattern (list of literal/path pairs) to a term, producing string term results.
--   The pattern is represented as: (firstLiteral, [(pathSteps, trailingLiteral), ...])
--   We build result strings by starting with firstLit, then for each pair, evaluating the path
--   on the term to get strings, and appending pathResult ++ trailingLiteral.
applyPattern :: TBinding (Context -> String -> [([String], String)] -> Term -> Either (InContext Error) [Term])
applyPattern = define "applyPattern" $
  doc "Apply a parsed pattern to a term, producing string terms" $
  "cx" ~> "firstLit" ~> "pairs" ~> "term" ~>
    Logic.ifElse (Lists.null $ var "pairs")
      -- No path expressions: just return the literal as a string term
      (right (list [inject _Term _Term_literal (inject _Literal _Literal_string (var "firstLit"))]))
      -- Evaluate all paths, then combine
      (Eithers.bind (Eithers.mapList
        ("pp" ~> Eithers.map
          ("terms" ~> pair (Lists.map ("t" ~> termToString @@ var "t") (var "terms")) (Pairs.second $ var "pp"))
          (evalPath @@ var "cx" @@ (Pairs.first $ var "pp") @@ var "term"))
        (var "pairs"))
        ("evaluated" ~>
          -- Fold over evaluated pairs, building up accumulator strings
          right (Lists.map
            ("s" ~> inject _Term _Term_literal (inject _Literal _Literal_string (var "s")))
            (Lists.foldl
              ("accum" ~> "ep" ~> lets [
                "pStrs">: Pairs.first $ var "ep",
                "litP">: Pairs.second $ var "ep"] $
                Lists.concat (Lists.map
                  ("pStr" ~> Lists.map ("a" ~> var "a" ++ var "pStr" ++ var "litP") (var "accum"))
                  (var "pStrs")))
              (list [var "firstLit"])
              (var "evaluated")))))

-- | Parse a string pattern into a function that traverses terms.
--   Patterns can contain ${path/to/field} expressions that are evaluated against terms.
parsePattern :: TBinding (Context -> Graph -> String -> Either (InContext Error) (Context -> Term -> Either (InContext Error) [Term]))
parsePattern = define "parsePattern" $
  doc "Parse a string pattern into a function that traverses terms" $
  "cx" ~> "_g" ~> "pat" ~> lets [
    -- Split on "${" to get segments. First segment is a literal prefix.
    -- Remaining segments each start with a path (up to "}") followed by a literal.
    "segments">: Strings.splitOn (string "${") (var "pat"),
    "firstLit">: Lists.head $ var "segments",
    "rest">: Lists.tail $ var "segments",
    -- Parse each remaining segment into (pathSteps, trailingLiteral) pairs
    "parsed">: Lists.map
      ("seg" ~> lets [
        "parts">: Strings.splitOn (string "}") (var "seg"),
        "pathStr">: Lists.head $ var "parts",
        "litPart">: Strings.intercalate (string "}") (Lists.tail $ var "parts"),
        "pathSteps">: Strings.splitOn (string "/") (var "pathStr")] $
        pair (var "pathSteps") (var "litPart"))
      (var "rest")] $
    right ("cx'" ~> "term" ~>
      applyPattern @@ var "cx'" @@ var "firstLit" @@ var "parsed" @@ var "term")

-- | Parse a property specification into an encoder function
parsePropertySpec :: TBinding (Context -> Graph -> PGM.Schema s t v -> PGM.PropertySpec
  -> Either (InContext Error) (Context -> Term -> Either (InContext Error) [(PG.PropertyKey, v)]))
parsePropertySpec = define "parsePropertySpec" $
  doc "Parse a property specification into an encoder function" $
  "cx" ~> "g" ~> "schema" ~> "spec" ~> lets [
    "key">: project PGM._PropertySpec PGM._PropertySpec_key @@ var "spec",
    "value">: project PGM._PropertySpec PGM._PropertySpec_value @@ var "spec"] $
    Eithers.bind (parseValueSpec @@ var "cx" @@ var "g" @@ var "value")
      ("fun" ~> right
        ("cx'" ~> "term" ~>
          Eithers.bind (var "fun" @@ var "cx'" @@ var "term")
            ("results" ~> Eithers.bind (Eithers.mapList (Util.coderEncode (project PGM._Schema PGM._Schema_propertyValues @@ var "schema") @@ var "cx'") (var "results"))
              ("values" ~> right (Lists.map ("v" ~> pair (var "key") (var "v")) (var "values"))))))

-- | Parse a value specification into a function that processes terms
parseValueSpec :: TBinding (Context -> Graph -> PGM.ValueSpec -> Either (InContext Error) (Context -> Term -> Either (InContext Error) [Term]))
parseValueSpec = define "parseValueSpec" $
  doc "Parse a value specification into a function that processes terms" $
  "cx" ~> "g" ~> "spec" ~>
    match PGM._ValueSpec Nothing [
      PGM._ValueSpec_value>>: constant $ right ("_cx" ~> "term" ~> right (list [var "term"])),
      PGM._ValueSpec_pattern>>: "pat" ~> parsePattern @@ var "cx" @@ var "g" @@ var "pat"]
    @@ var "spec"

-- | Parse a vertex id pattern from a value spec and schema
parseVertexIdPattern :: TBinding (Context -> Graph -> PGM.Schema s t v -> PGM.ValueSpec -> Either (InContext Error) (Context -> Term -> Either (InContext Error) [v]))
parseVertexIdPattern = define "parseVertexIdPattern" $
  doc "Parse a vertex id pattern from a value spec and schema" $
  "cx" ~> "g" ~> "schema" ~> "spec" ~>
    Eithers.bind (parseValueSpec @@ var "cx" @@ var "g" @@ var "spec")
      ("fun" ~> right
        ("cx'" ~> "term" ~>
          Eithers.bind (var "fun" @@ var "cx'" @@ var "term")
            ("terms" ~> Eithers.mapList (Util.coderEncode (project PGM._Schema PGM._Schema_vertexIds @@ var "schema") @@ var "cx'") (var "terms"))))

-- | Parse a vertex specification into a label and encoder function
parseVertexSpec :: TBinding (Context -> Graph -> PGM.Schema s t v -> PGM.VertexSpec
  -> Either (InContext Error) (PG.Label, Context -> Term -> Either (InContext Error) [PG.Element v]))
parseVertexSpec = define "parseVertexSpec" $
  doc "Parse a vertex specification into a label and encoder function" $
  "cx" ~> "g" ~> "schema" ~> "spec" ~> lets [
    "label">: project PGM._VertexSpec PGM._VertexSpec_label @@ var "spec",
    "id">: project PGM._VertexSpec PGM._VertexSpec_id @@ var "spec",
    "props">: project PGM._VertexSpec PGM._VertexSpec_properties @@ var "spec"] $
    Eithers.bind (parseVertexIdPattern @@ var "cx" @@ var "g" @@ var "schema" @@ var "id")
      ("getId" ~> Eithers.bind (Eithers.mapList (parsePropertySpec @@ var "cx" @@ var "g" @@ var "schema") (var "props"))
        ("getProps" ~> right (pair
          (inject PG._Label PG._Label_vertex $ var "label")
          ("cx'" ~> "term" ~>
            Eithers.bind (requireUnique @@ var "cx'" @@ string "vertex id" @@ (var "getId" @@ var "cx'") @@ var "term")
              ("tid" ~> Eithers.bind (Eithers.map ("_xs" ~> Maps.fromList (var "_xs")) (Eithers.mapList ("gf" ~> requireUnique @@ var "cx'" @@ string "property key" @@ (var "gf" @@ var "cx'") @@ var "term") (var "getProps")))
                ("tprops" ~> right (list [inject PG._Element PG._Element_vertex
                  (record PG._Vertex [
                    PG._Vertex_label>>: var "label",
                    PG._Vertex_id>>: var "tid",
                    PG._Vertex_properties>>: var "tprops"])])))))))

-- | Read a field from a map of fields by name
readField :: TBinding (Context -> M.Map Name Term -> Name -> (Term -> Either (InContext Error) a) -> Either (InContext Error) a)
readField = define "readField" $
  doc "Read a field from a map of fields by name" $
  "cx" ~> "fields" ~> "fname" ~> "fun" ~>
    Maybes.maybe
      (left $ Ctx.inContext (Error.errorOther $ Error.otherError (string "no such field: " ++ (Core.unName $ var "fname"))) (var "cx"))
      (var "fun")
      (Maps.lookup (var "fname") (var "fields"))

-- | Read an injection (union value) from a term
readInjection :: TBinding (Context -> Graph -> [(Name, Term -> Either (InContext Error) x)] -> Term -> Either (InContext Error) x)
readInjection = define "readInjection" $
  doc "Read an injection (union value) from a term" $
  "cx" ~> "g" ~> "cases" ~> "encoded" ~>
    Eithers.bind (ExtractCore.map @@ var "cx" @@ ("k" ~> Eithers.map ("_n" ~> Core.name (var "_n")) (ExtractCore.string @@ var "cx" @@ var "g" @@ var "k")) @@ ("_v" ~> right (var "_v")) @@ var "g" @@ var "encoded")
      ("mp" ~> lets [
        "entries">: Maps.toList $ var "mp"] $
        Logic.ifElse (Lists.null $ var "entries")
          (left $ Ctx.inContext (Error.errorOther $ Error.otherError $ string "empty injection") (var "cx"))
          (lets [
            "f">: Lists.head $ var "entries",
            "key">: Pairs.first $ var "f",
            "val">: Pairs.second $ var "f",
            "matching">: Lists.filter ("c" ~> Equality.equal (Pairs.first $ var "c") (var "key")) (var "cases")] $
            Logic.ifElse (Lists.null $ var "matching")
              (left $ Ctx.inContext (Error.errorOther $ Error.otherError $ string "unexpected field: " ++ (Core.unName $ var "key")) (var "cx"))
              ((Pairs.second $ Lists.head $ var "matching") @@ var "val")))

-- | Read a record from a term as a map of field names to values
readRecord :: TBinding (Context -> Graph -> (M.Map Name Term -> Either (InContext Error) x) -> Term -> Either (InContext Error) x)
readRecord = define "readRecord" $
  doc "Read a record from a term as a map of field names to values" $
  "cx" ~> "g" ~> "cons" ~> "term" ~>
    Eithers.bind (ExtractCore.map @@ var "cx" @@ ("k" ~> Eithers.map ("_n" ~> Core.name (var "_n")) (ExtractCore.string @@ var "cx" @@ var "g" @@ var "k")) @@ ("_v" ~> right (var "_v")) @@ var "g" @@ var "term")
      (var "cons")

-- | Require exactly one result from a list-producing function
requireUnique :: TBinding (Context -> String -> (Term -> Either (InContext Error) [x]) -> Term -> Either (InContext Error) x)
requireUnique = define "requireUnique" $
  doc "Require exactly one result from a list-producing function" $
  "cx" ~> "context" ~> "fun" ~> "term" ~>
    Eithers.bind (var "fun" @@ var "term")
      ("results" ~>
        Logic.ifElse (Lists.null $ var "results")
          (left $ Ctx.inContext (Error.errorOther $ Error.otherError $ string "No value found: " ++ var "context") (var "cx"))
          (Logic.ifElse (Equality.equal (Lists.length $ var "results") (int32 1))
            (right $ Lists.head $ var "results")
            (left $ Ctx.inContext (Error.errorOther $ Error.otherError $ string "Multiple values found: " ++ var "context") (var "cx"))))

-- | Create an adapter that maps terms to property graph elements using a mapping specification
termToElementsAdapter :: TBinding (Context -> Graph -> PGM.Schema s t v -> Type
  -> Either (InContext Error) (Adapter Type [PG.Label] Term [PG.Element v]))
termToElementsAdapter = define "termToElementsAdapter" $
  doc "Create an adapter that maps terms to property graph elements using a mapping specification" $
  "cx" ~> "g" ~> "schema" ~> "typ" ~> lets [
    "key_elements">: Core.name (string "elements")] $
    Maybes.maybe
      (right $ Util.adapter false (var "typ") (list ([] :: [TTerm PG.Label]))
        (Util.coder
          ("_cx" ~> "_t" ~> right (list ([] :: [TTerm (PG.Element ())])))
          ("cx'" ~> "_els" ~> left (Ctx.inContext (Error.errorOther $ Error.otherError $ string "no corresponding element type") (var "cx'")))))
      ("term" ~>
        Eithers.bind (expectList @@ var "cx" @@ var "g" @@ decodeElementSpec @@ var "term")
          ("specTerms" ~> Eithers.bind (Eithers.mapList (parseElementSpec @@ var "cx" @@ var "g" @@ var "schema") (var "specTerms"))
            ("specs" ~> lets [
              "labels">: (Lists.nub :: TTerm [PG.Label] -> TTerm [PG.Label]) (Lists.map ("_p" ~> Pairs.first (var "_p")) (var "specs")),
              "encoders">: Lists.map ("_p" ~> Pairs.second (var "_p")) (var "specs")] $
              right (Util.adapter false (var "typ") (var "labels")
                (Util.coder
                  ("cx'" ~> "t" ~>
                    Eithers.map ("_xs" ~> Lists.concat (var "_xs")) (Eithers.mapList ("e" ~> var "e" @@ var "cx'" @@ var "t") (var "encoders")))
                  ("cx'" ~> "_els" ~> left (Ctx.inContext (Error.errorOther $ Error.otherError $ string "element decoding is not yet supported") (var "cx'"))))))))
      (Annotations.getTypeAnnotation @@ var "key_elements" @@ var "typ")
