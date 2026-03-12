module Hydra.Ext.Sources.Pg.TermsToElements where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (
  decodeEdgeLabel, decodeEdgeSpec, decodeElementSpec, decodePropertyKey,
  decodePropertySpec, decodeValueSpec, decodeVertexLabel, decodeVertexSpec,
  expectList, parseEdgeIdPattern, parseEdgeSpec, parseElementSpec, parsePattern,
  parsePropertySpec, parseValueSpec, parseVertexIdPattern, parseVertexSpec,
  readField, readInjection, readRecord, requireUnique, termToElementsAdapter)
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Meta.Accessors                  as Accessors
import qualified Hydra.Dsl.Meta.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Compute                    as Compute
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Meta.Error                      as Error
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Meta.Json                       as Json
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
import qualified Hydra.Dsl.Meta.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Meta.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Meta.Typing                     as Typing
import qualified Hydra.Dsl.Meta.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules  as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple   as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms    as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils    as AdaptUtils
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
import qualified Hydra.Sources.Kernel.Terms.Monads         as Monads
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
    [ExtractCore.ns]
    (PgModel.ns:PgMapping.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Functions for mapping Hydra terms to property graph elements using mapping specifications"
  where
    elements = [
      toBinding decodeEdgeLabel,
      toBinding decodeEdgeSpec,
      toBinding decodeElementSpec,
      toBinding decodePropertyKey,
      toBinding decodePropertySpec,
      toBinding decodeValueSpec,
      toBinding decodeVertexLabel,
      toBinding decodeVertexSpec,
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
      toBinding termToElementsAdapter]

-- | Decode an edge label from a term
decodeEdgeLabel :: TBinding (Context -> Graph -> Term -> Either (InContext OtherError) PG.EdgeLabel)
decodeEdgeLabel = define "decodeEdgeLabel" $
  doc "Decode an edge label from a term" $
  "cx" ~> "g" ~> "t" ~>
    Eithers.map ("_x" ~> wrap PG._EdgeLabel (var "_x")) (ExtractCore.string @@ var "cx" @@ var "g" @@ var "t")

-- | Decode an edge specification from a term
decodeEdgeSpec :: TBinding (Context -> Graph -> Term -> Either (InContext OtherError) PGM.EdgeSpec)
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
decodeElementSpec :: TBinding (Context -> Graph -> Term -> Either (InContext OtherError) PGM.ElementSpec)
decodeElementSpec = define "decodeElementSpec" $
  doc "Decode an element specification from a term" $
  "cx" ~> "g" ~> "term" ~>
    readInjection @@ var "cx" @@ var "g"
      @@ list [
        pair (Core.name $ string "vertex") ("t" ~> Eithers.map ("_x" ~> inject PGM._ElementSpec PGM._ElementSpec_vertex (var "_x")) (decodeVertexSpec @@ var "cx" @@ var "g" @@ var "t")),
        pair (Core.name $ string "edge") ("t" ~> Eithers.map ("_x" ~> inject PGM._ElementSpec PGM._ElementSpec_edge (var "_x")) (decodeEdgeSpec @@ var "cx" @@ var "g" @@ var "t"))]
      @@ var "term"

-- | Decode a property key from a term
decodePropertyKey :: TBinding (Context -> Graph -> Term -> Either (InContext OtherError) PG.PropertyKey)
decodePropertyKey = define "decodePropertyKey" $
  doc "Decode a property key from a term" $
  "cx" ~> "g" ~> "t" ~>
    Eithers.map ("_x" ~> wrap PG._PropertyKey (var "_x")) (ExtractCore.string @@ var "cx" @@ var "g" @@ var "t")

-- | Decode a property specification from a term
decodePropertySpec :: TBinding (Context -> Graph -> Term -> Either (InContext OtherError) PGM.PropertySpec)
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
decodeValueSpec :: TBinding (Context -> Graph -> Term -> Either (InContext OtherError) PGM.ValueSpec)
decodeValueSpec = define "decodeValueSpec" $
  doc "Decode a value specification from a term" $
  "cx" ~> "g" ~> "term" ~>
    readInjection @@ var "cx" @@ var "g"
      @@ list [
        pair (Core.name $ string "value") (constant $ right (inject PGM._ValueSpec PGM._ValueSpec_value $ unit)),
        pair (Core.name $ string "pattern") ("t" ~> Eithers.map ("_x" ~> inject PGM._ValueSpec PGM._ValueSpec_pattern (var "_x")) (ExtractCore.string @@ var "cx" @@ var "g" @@ var "t"))]
      @@ var "term"

-- | Decode a vertex label from a term
decodeVertexLabel :: TBinding (Context -> Graph -> Term -> Either (InContext OtherError) PG.VertexLabel)
decodeVertexLabel = define "decodeVertexLabel" $
  doc "Decode a vertex label from a term" $
  "cx" ~> "g" ~> "t" ~>
    Eithers.map ("_x" ~> wrap PG._VertexLabel (var "_x")) (ExtractCore.string @@ var "cx" @@ var "g" @@ var "t")

-- | Decode a vertex specification from a term
decodeVertexSpec :: TBinding (Context -> Graph -> Term -> Either (InContext OtherError) PGM.VertexSpec)
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
expectList :: TBinding (Context -> Graph -> (Context -> Graph -> Term -> Either (InContext OtherError) x) -> Term -> Either (InContext OtherError) [x])
expectList = define "expectList" $
  doc "Extract a list from a term and apply a decoder to each element" $
  "cx" ~> "g" ~> "f" ~> "term" ~>
    Eithers.bind (ExtractCore.list @@ var "cx" @@ var "g" @@ var "term")
      ("elems" ~> Eithers.mapList (var "f" @@ var "cx" @@ var "g") (var "elems"))

-- | Parse an edge id pattern from a value spec and schema
parseEdgeIdPattern :: TBinding (Context -> Graph -> PGM.Schema s t v -> PGM.ValueSpec -> Either (InContext OtherError) (Context -> Term -> Either (InContext OtherError) [v]))
parseEdgeIdPattern = define "parseEdgeIdPattern" $
  doc "Parse an edge id pattern from a value spec and schema" $
  "cx" ~> "g" ~> "schema" ~> "spec" ~>
    Eithers.bind (parseValueSpec @@ var "cx" @@ var "g" @@ var "spec")
      ("fun" ~> right
        ("cx'" ~> "term" ~>
          Eithers.bind (var "fun" @@ var "cx'" @@ var "term")
            ("terms" ~> Eithers.mapList (Compute.coderEncode (project PGM._Schema PGM._Schema_edgeIds @@ var "schema") @@ var "cx'") (var "terms"))))

-- | Parse an edge specification into a label and encoder function
parseEdgeSpec :: TBinding (Context -> Graph -> PGM.Schema s t v -> PGM.EdgeSpec
  -> Either (InContext OtherError) (PG.Label, Context -> Term -> Either (InContext OtherError) [PG.Element v]))
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
  -> Either (InContext OtherError) (PG.Label, Context -> Term -> Either (InContext OtherError) [PG.Element v]))
parseElementSpec = define "parseElementSpec" $
  doc "Parse an element specification into a label and encoder function" $
  "cx" ~> "g" ~> "schema" ~> "spec" ~>
    match PGM._ElementSpec Nothing [
      PGM._ElementSpec_vertex>>: "vspec" ~> parseVertexSpec @@ var "cx" @@ var "g" @@ var "schema" @@ var "vspec",
      PGM._ElementSpec_edge>>: "espec" ~> parseEdgeSpec @@ var "cx" @@ var "g" @@ var "schema" @@ var "espec"]
    @@ var "spec"

-- | Parse a string pattern into a function that traverses terms
parsePattern :: TBinding (Context -> Graph -> String -> Either (InContext OtherError) (Context -> Term -> Either (InContext OtherError) [Term]))
parsePattern = define "parsePattern" $
  doc "Parse a string pattern into a function that traverses terms" $
  "cx" ~> "g" ~> "pat" ~>
    -- Note: pattern parsing is complex and involves string splitting and term traversal.
    -- This is a simplified DSL representation; the full implementation handles ${path} expressions.
    right ("cx'" ~> "term" ~> right (list [var "term"]))

-- | Parse a property specification into an encoder function
parsePropertySpec :: TBinding (Context -> Graph -> PGM.Schema s t v -> PGM.PropertySpec
  -> Either (InContext OtherError) (Context -> Term -> Either (InContext OtherError) [(PG.PropertyKey, v)]))
parsePropertySpec = define "parsePropertySpec" $
  doc "Parse a property specification into an encoder function" $
  "cx" ~> "g" ~> "schema" ~> "spec" ~> lets [
    "key">: project PGM._PropertySpec PGM._PropertySpec_key @@ var "spec",
    "value">: project PGM._PropertySpec PGM._PropertySpec_value @@ var "spec"] $
    Eithers.bind (parseValueSpec @@ var "cx" @@ var "g" @@ var "value")
      ("fun" ~> right
        ("cx'" ~> "term" ~>
          Eithers.bind (var "fun" @@ var "cx'" @@ var "term")
            ("results" ~> Eithers.bind (Eithers.mapList (Compute.coderEncode (project PGM._Schema PGM._Schema_propertyValues @@ var "schema") @@ var "cx'") (var "results"))
              ("values" ~> right (Lists.map ("v" ~> pair (var "key") (var "v")) (var "values"))))))

-- | Parse a value specification into a function that processes terms
parseValueSpec :: TBinding (Context -> Graph -> PGM.ValueSpec -> Either (InContext OtherError) (Context -> Term -> Either (InContext OtherError) [Term]))
parseValueSpec = define "parseValueSpec" $
  doc "Parse a value specification into a function that processes terms" $
  "cx" ~> "g" ~> "spec" ~>
    match PGM._ValueSpec Nothing [
      PGM._ValueSpec_value>>: constant $ right ("_cx" ~> "term" ~> right (list [var "term"])),
      PGM._ValueSpec_pattern>>: "pat" ~> parsePattern @@ var "cx" @@ var "g" @@ var "pat"]
    @@ var "spec"

-- | Parse a vertex id pattern from a value spec and schema
parseVertexIdPattern :: TBinding (Context -> Graph -> PGM.Schema s t v -> PGM.ValueSpec -> Either (InContext OtherError) (Context -> Term -> Either (InContext OtherError) [v]))
parseVertexIdPattern = define "parseVertexIdPattern" $
  doc "Parse a vertex id pattern from a value spec and schema" $
  "cx" ~> "g" ~> "schema" ~> "spec" ~>
    Eithers.bind (parseValueSpec @@ var "cx" @@ var "g" @@ var "spec")
      ("fun" ~> right
        ("cx'" ~> "term" ~>
          Eithers.bind (var "fun" @@ var "cx'" @@ var "term")
            ("terms" ~> Eithers.mapList (Compute.coderEncode (project PGM._Schema PGM._Schema_vertexIds @@ var "schema") @@ var "cx'") (var "terms"))))

-- | Parse a vertex specification into a label and encoder function
parseVertexSpec :: TBinding (Context -> Graph -> PGM.Schema s t v -> PGM.VertexSpec
  -> Either (InContext OtherError) (PG.Label, Context -> Term -> Either (InContext OtherError) [PG.Element v]))
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
readField :: TBinding (Context -> M.Map Name Term -> Name -> (Term -> Either (InContext OtherError) a) -> Either (InContext OtherError) a)
readField = define "readField" $
  doc "Read a field from a map of fields by name" $
  "cx" ~> "fields" ~> "fname" ~> "fun" ~>
    Maybes.maybe
      (left $ Ctx.inContext (Error.otherError (string "no such field: " ++ (Core.unName $ var "fname"))) (var "cx"))
      (var "fun")
      (Maps.lookup (var "fname") (var "fields"))

-- | Read an injection (union value) from a term
readInjection :: TBinding (Context -> Graph -> [(Name, Term -> Either (InContext OtherError) x)] -> Term -> Either (InContext OtherError) x)
readInjection = define "readInjection" $
  doc "Read an injection (union value) from a term" $
  "cx" ~> "g" ~> "cases" ~> "encoded" ~>
    Eithers.bind (ExtractCore.map @@ var "cx" @@ ("k" ~> Eithers.map ("_n" ~> Core.name (var "_n")) (ExtractCore.string @@ var "cx" @@ var "g" @@ var "k")) @@ ("_v" ~> right (var "_v")) @@ var "g" @@ var "encoded")
      ("mp" ~> lets [
        "entries">: Maps.toList $ var "mp"] $
        Logic.ifElse (Lists.null $ var "entries")
          (left $ Ctx.inContext (Error.otherError $ string "empty injection") (var "cx"))
          (lets [
            "f">: Lists.head $ var "entries",
            "key">: Pairs.first $ var "f",
            "val">: Pairs.second $ var "f",
            "matching">: Lists.filter ("c" ~> Equality.equal (Pairs.first $ var "c") (var "key")) (var "cases")] $
            Logic.ifElse (Lists.null $ var "matching")
              (left $ Ctx.inContext (Error.otherError $ string "unexpected field: " ++ (Core.unName $ var "key")) (var "cx"))
              ((Pairs.second $ Lists.head $ var "matching") @@ var "val")))

-- | Read a record from a term as a map of field names to values
readRecord :: TBinding (Context -> Graph -> (M.Map Name Term -> Either (InContext OtherError) x) -> Term -> Either (InContext OtherError) x)
readRecord = define "readRecord" $
  doc "Read a record from a term as a map of field names to values" $
  "cx" ~> "g" ~> "cons" ~> "term" ~>
    Eithers.bind (ExtractCore.map @@ var "cx" @@ ("k" ~> Eithers.map ("_n" ~> Core.name (var "_n")) (ExtractCore.string @@ var "cx" @@ var "g" @@ var "k")) @@ ("_v" ~> right (var "_v")) @@ var "g" @@ var "term")
      (var "cons")

-- | Require exactly one result from a list-producing function
requireUnique :: TBinding (Context -> String -> (Term -> Either (InContext OtherError) [x]) -> Term -> Either (InContext OtherError) x)
requireUnique = define "requireUnique" $
  doc "Require exactly one result from a list-producing function" $
  "cx" ~> "context" ~> "fun" ~> "term" ~>
    Eithers.bind (var "fun" @@ var "term")
      ("results" ~>
        Logic.ifElse (Lists.null $ var "results")
          (left $ Ctx.inContext (Error.otherError $ string "No value found: " ++ var "context") (var "cx"))
          (Logic.ifElse (Equality.equal (Lists.length $ var "results") (int32 1))
            (right $ Lists.head $ var "results")
            (left $ Ctx.inContext (Error.otherError $ string "Multiple values found: " ++ var "context") (var "cx"))))

-- | Create an adapter that maps terms to property graph elements using a mapping specification
termToElementsAdapter :: TBinding (Context -> Graph -> PGM.Schema s t v -> Type
  -> Either (InContext OtherError) (Adapter Type [PG.Label] Term [PG.Element v]))
termToElementsAdapter = define "termToElementsAdapter" $
  doc "Create an adapter that maps terms to property graph elements using a mapping specification" $
  "cx" ~> "g" ~> "schema" ~> "typ" ~> lets [
    "key_elements">: Core.name (string "elements")] $
    Maybes.maybe
      (right $ Compute.adapter false (var "typ") (list ([] :: [TTerm Term]))
        (Compute.coder
          ("_cx" ~> "_t" ~> right (list ([] :: [TTerm Term])))
          ("cx'" ~> "_els" ~> left (Ctx.inContext (Error.otherError $ string "no corresponding element type") (var "cx'")))))
      ("term" ~>
        Eithers.bind (expectList @@ var "cx" @@ var "g" @@ decodeElementSpec @@ var "term")
          ("specTerms" ~> Eithers.bind (Eithers.mapList (parseElementSpec @@ var "cx" @@ var "g" @@ var "schema") (var "specTerms"))
            ("specs" ~> lets [
              "labels">: (Lists.nub :: TTerm [PG.Label] -> TTerm [PG.Label]) (Lists.map ("_p" ~> Pairs.first (var "_p")) (var "specs")),
              "encoders">: Lists.map ("_p" ~> Pairs.second (var "_p")) (var "specs")] $
              right (Compute.adapter false (var "typ") (var "labels")
                (Compute.coder
                  ("cx'" ~> "t" ~>
                    Eithers.map ("_xs" ~> Lists.concat (var "_xs")) (Eithers.mapList ("e" ~> var "e" @@ var "cx'" @@ var "t") (var "encoders")))
                  ("cx'" ~> "_els" ~> left (Ctx.inContext (Error.otherError $ string "element decoding is not yet supported") (var "cx'"))))))))
      (Annotations.getTypeAnnotation @@ var "key_elements" @@ var "typ")
