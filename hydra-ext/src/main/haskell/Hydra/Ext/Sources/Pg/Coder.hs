module Hydra.Ext.Sources.Pg.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (
  check, checkRecordName, constructEdgeCoder, constructVertexCoder,
  edgeCoder, edgeIdAdapter, elementCoder, elementTreeEdge, elementTreeVertex,
  elementTypeTreeEdge, elementTypeTreeVertex, encodeProperties, encodeProperty,
  extractString, findAdjacenEdgeAdapters, findIdProjectionSpec,
  findIncidentVertexAdapter, findLabelString, findProjectionSpec,
  findPropertySpecs, findSingleFieldWithAnnotationKey, hasVertexAdapters,
  projectionAdapter, propertyAdapter, propertyTypes, selectEdgeId,
  selectVertexId, traverseToSingleTerm, vertexCoder, vertexIdAdapter)
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
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
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Errors                      as Error
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
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Encode.Core                         as EncodeCore
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
import qualified Hydra.Sources.Kernel.Terms.Strip          as Strip
import qualified Hydra.Sources.Kernel.Terms.Resolution    as Resolution
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
import qualified Hydra.Pg.Model as PG
import qualified Hydra.Pg.Mapping as PGM
import qualified Hydra.Ext.Sources.Pg.Model as PgModel
import qualified Hydra.Ext.Sources.Pg.Mapping as PgMapping
import qualified Hydra.Ext.Sources.Pg.TermsToElements as TermsToElements


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.pg.coder"

module_ :: Module
module_ = Module ns elements
    [Annotations.ns, ExtractCore.ns, Resolution.ns, TermsToElements.ns]
    (PgModel.ns:PgMapping.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Property graph element coders for mapping Hydra terms to property graph elements"
  where
    elements = [
      toDefinition check,
      toDefinition checkRecordName,
      toDefinition constructEdgeCoder,
      toDefinition constructVertexCoder,
      toDefinition edgeCoder,
      toDefinition edgeIdAdapter,
      toDefinition elementCoder,
      toDefinition elementTreeEdge,
      toDefinition elementTreeVertex,
      toDefinition elementTypeTreeEdge,
      toDefinition elementTypeTreeVertex,
      toDefinition encodeProperties,
      toDefinition encodeProperty,
      toDefinition extractString,
      toDefinition findAdjacenEdgeAdapters,
      toDefinition findIdProjectionSpec,
      toDefinition findIncidentVertexAdapter,
      toDefinition findLabelString,
      toDefinition findProjectionSpec,
      toDefinition findPropertySpecs,
      toDefinition findSingleFieldWithAnnotationKey,
      toDefinition hasVertexAdapters,
      toDefinition projectionAdapter,
      toDefinition propertyAdapter,
      toDefinition propertyTypes,
      toDefinition selectEdgeId,
      toDefinition selectVertexId,
      toDefinition traverseToSingleTerm,
      toDefinition vertexCoder,
      toDefinition vertexIdAdapter]

-- Helper for error results
err :: TTerm Context -> TTerm String -> TTerm (Either (InContext Error) a)
err cx msg = left $ Ctx.inContext (Error.errorOther $ Error.otherError msg) cx

unexpectedE :: TTerm Context -> TTerm String -> TTerm String -> TTerm (Either (InContext Error) a)
unexpectedE cx expected found = err cx (string "Expected " ++ expected ++ string ", found: " ++ found)

-- | Check a condition, returning an error if false
check :: TTermDefinition (Context -> Bool -> Either (InContext Error) () -> Either (InContext Error) ())
check = define "check" $
  doc "Check a condition, returning an error if false" $
  "_cx" ~> "b" ~> "e" ~>
    Logic.ifElse (var "b") (right unit) (var "e")

-- | Check that a record name matches the expected name.
--   Skips the check when the expected name is a placeholder (used when the type has no intrinsic name).
checkRecordName :: TTermDefinition (Context -> Name -> Name -> Either (InContext Error) ())
checkRecordName = define "checkRecordName" $
  doc "Check that a record name matches the expected name" $
  "cx" ~> "expected" ~> "actual" ~>
    check @@ var "cx"
      @@ (Logic.or
        (Equality.equal (Core.unName $ var "expected") (string "placeholder"))
        (Equality.equal (Core.unName $ var "actual") (Core.unName $ var "expected")))
      @@ (err (var "cx") (string "Expected record of type " ++ (Core.unName $ var "expected") ++ string ", found record of type " ++ (Core.unName $ var "actual")))

-- | Construct an edge coder from components
constructEdgeCoder :: TTermDefinition (Context -> Graph -> PG.VertexLabel -> PGM.Schema Graph t v -> Type -> t -> t -> PG.Direction -> Name -> [FieldType]
  -> [Adapter FieldType (PG.PropertyType t) Field (PG.Property v)]
  -> Y.Maybe (FieldType, PGM.ValueSpec, Y.Maybe String)
  -> Y.Maybe (FieldType, PGM.ValueSpec, Y.Maybe String)
  -> Either (InContext Error) (Adapter Type (PG.ElementTypeTree t) Term (PG.ElementTree v)))
constructEdgeCoder = define "constructEdgeCoder" $
  doc "Construct an edge coder from components" $
  "cx" ~> "g" ~> "parentLabel" ~> "schema" ~> "source" ~> "vidType" ~> "eidType" ~> "dir" ~> "name" ~> "fields" ~>
  "propAdapters" ~> "mOutSpec" ~> "mInSpec" ~>
    Eithers.bind (findLabelString @@ var "cx" @@ var "g" @@ var "source" @@ var "name" @@ (Core.name $ project PGM._AnnotationSchema PGM._AnnotationSchema_edgeLabel @@ (project PGM._Schema PGM._Schema_annotations @@ var "schema")))
      ("labelStr" ~> lets [
        "label">: wrap PG._EdgeLabel $ var "labelStr",
        "vertexIdsSchema">: project PGM._Schema PGM._Schema_vertexIds @@ var "schema"] $
        Eithers.bind (edgeIdAdapter @@ var "cx" @@ var "g" @@ var "schema" @@ var "eidType" @@ var "name"
          @@ (Core.name $ project PGM._AnnotationSchema PGM._AnnotationSchema_edgeId @@ (project PGM._Schema PGM._Schema_annotations @@ var "schema"))
          @@ var "fields")
          ("idAdapter" ~>
            -- Compute out/in id adapters from projection specs
            Eithers.bind (Maybes.maybe (right nothing)
              ("s" ~> Eithers.map (lambda "x" $ just (var "x")) (projectionAdapter @@ var "cx" @@ var "g" @@ var "vidType" @@ var "vertexIdsSchema" @@ var "s" @@ string "out"))
              (var "mOutSpec"))
              ("outIdAdapter" ~> Eithers.bind (Maybes.maybe (right nothing)
                ("s" ~> Eithers.map (lambda "x" $ just (var "x")) (projectionAdapter @@ var "cx" @@ var "g" @@ var "vidType" @@ var "vertexIdsSchema" @@ var "s" @@ string "in"))
                (var "mInSpec"))
                ("inIdAdapter" ~>
                  -- Compute out/in vertex adapters from projection specs
                  Eithers.bind (Maybes.maybe (right nothing)
                    ("s" ~> Eithers.map (lambda "x" $ just (var "x")) (findIncidentVertexAdapter @@ var "cx" @@ var "g" @@ var "schema" @@ var "vidType" @@ var "eidType" @@ var "s"))
                    (var "mOutSpec"))
                    ("outVertexAdapter" ~> Eithers.bind (Maybes.maybe (right nothing)
                      ("s" ~> Eithers.map (lambda "x" $ just (var "x")) (findIncidentVertexAdapter @@ var "cx" @@ var "g" @@ var "schema" @@ var "vidType" @@ var "eidType" @@ var "s"))
                      (var "mInSpec"))
                      ("inVertexAdapter" ~> lets [
                        "vertexAdapters">: Maybes.cat (list [var "outVertexAdapter", var "inVertexAdapter"])] $
                        -- Compute out/in vertex labels from spec aliases or fall back to parentLabel
                        Eithers.bind (Maybes.maybe (right $ var "parentLabel")
                          ("spec" ~> Maybes.maybe
                            (left $ Ctx.inContext (Error.errorOther $ Error.otherError $ string "no out-vertex label") (var "cx"))
                            (lambda "a" $ right $ wrap PG._VertexLabel (var "a"))
                            (Pairs.second $ Pairs.second $ var "spec"))
                          (var "mOutSpec"))
                          ("outLabel" ~> Eithers.bind (Maybes.maybe (right $ var "parentLabel")
                            ("spec" ~> Maybes.maybe
                              (left $ Ctx.inContext (Error.errorOther $ Error.otherError $ string "no in-vertex label") (var "cx"))
                              (lambda "a" $ right $ wrap PG._VertexLabel (var "a"))
                              (Pairs.second $ Pairs.second $ var "spec"))
                            (var "mInSpec"))
                            ("inLabel" ~>
                              right (edgeCoder @@ var "g" @@ var "dir" @@ var "schema" @@ var "source" @@ var "eidType" @@ var "name" @@ var "label"
                                @@ var "outLabel" @@ var "inLabel" @@ var "idAdapter" @@ var "outIdAdapter" @@ var "inIdAdapter" @@ var "propAdapters" @@ var "vertexAdapters")))))))))

-- | Construct a vertex coder from components
constructVertexCoder :: TTermDefinition (Context -> Graph -> PGM.Schema Graph t v -> Type -> t -> t -> Name -> [FieldType]
  -> [Adapter FieldType (PG.PropertyType t) Field (PG.Property v)]
  -> Either (InContext Error) (Adapter Type (PG.ElementTypeTree t) Term (PG.ElementTree v)))
constructVertexCoder = define "constructVertexCoder" $
  doc "Construct a vertex coder from components" $
  "cx" ~> "g" ~> "schema" ~> "source" ~> "vidType" ~> "eidType" ~> "name" ~> "fields" ~> "propAdapters" ~>
    Eithers.bind (findLabelString @@ var "cx" @@ var "g" @@ var "source" @@ var "name" @@ (Core.name $ project PGM._AnnotationSchema PGM._AnnotationSchema_vertexLabel @@ (project PGM._Schema PGM._Schema_annotations @@ var "schema")))
      ("labelStr" ~> lets [
        "label">: wrap PG._VertexLabel $ var "labelStr"] $
        Eithers.bind (vertexIdAdapter @@ var "cx" @@ var "g" @@ var "schema" @@ var "vidType" @@ var "name"
          @@ (Core.name $ project PGM._AnnotationSchema PGM._AnnotationSchema_vertexId @@ (project PGM._Schema PGM._Schema_annotations @@ var "schema"))
          @@ var "fields")
          ("idAdapter" ~>
            Eithers.bind (findAdjacenEdgeAdapters @@ var "cx" @@ var "g" @@ var "schema" @@ var "vidType" @@ var "eidType" @@ var "label" @@ (inject PG._Direction PG._Direction_out unit) @@ var "fields")
              ("outEdgeAdapters" ~> Eithers.bind (findAdjacenEdgeAdapters @@ var "cx" @@ var "g" @@ var "schema" @@ var "vidType" @@ var "eidType" @@ var "label" @@ (inject PG._Direction PG._Direction_in unit) @@ var "fields")
                ("inEdgeAdapters" ~>
                  right (vertexCoder @@ var "g" @@ var "schema" @@ var "source" @@ var "vidType" @@ var "name" @@ var "label"
                    @@ var "idAdapter" @@ var "propAdapters" @@ (Lists.concat2 (var "outEdgeAdapters") (var "inEdgeAdapters")))))))

-- | Create an edge coder given all components
edgeCoder :: TTermDefinition (Graph -> PG.Direction -> PGM.Schema Graph t v -> Type -> t -> Name
  -> PG.EdgeLabel -> PG.VertexLabel -> PG.VertexLabel
  -> Y.Maybe (Name, Adapter Type t Term v) -> Y.Maybe (Name, Adapter Type t Term v) -> Y.Maybe (Name, Adapter Type t Term v)
  -> [Adapter FieldType (PG.PropertyType t) Field (PG.Property v)]
  -> [(Name, Adapter Type (PG.ElementTypeTree t) Term (PG.ElementTree v))]
  -> Adapter Type (PG.ElementTypeTree t) Term (PG.ElementTree v))
edgeCoder = define "edgeCoder" $
  doc "Create an edge coder given all components" $
  "g" ~> "dir" ~> "schema" ~> "source" ~> "eidType" ~> "tname" ~> "label" ~> "outLabel" ~> "inLabel" ~>
  "mIdAdapter" ~> "outAdapter" ~> "inAdapter" ~> "propAdapters" ~> "vertexAdapters" ~> lets [
    "et">: record PG._EdgeType [
      PG._EdgeType_label>>: var "label",
      PG._EdgeType_id>>: var "eidType",
      PG._EdgeType_out>>: var "outLabel",
      PG._EdgeType_in>>: var "inLabel",
      PG._EdgeType_properties>>: propertyTypes @@ var "propAdapters"]] $
    Coders.adapter true (var "source")
      (elementTypeTreeEdge @@ var "et" @@ TTerm (Terms.list []))
      (Coders.coder
        ("cx" ~> "term" ~>
          lets ["deannot">: Strip.deannotateTerm @@ var "term",
                "unwrapped">: cases _Term (var "deannot") (Just $ var "deannot") [
                  _Term_maybe>>: "mt" ~> Maybes.fromMaybe (var "deannot") (var "mt")],
                "rec">: cases _Term (var "unwrapped") Nothing [
                  _Term_record>>: lambda "r" $ var "r"]] $
          Eithers.bind (checkRecordName @@ var "cx" @@ var "tname" @@ (Core.recordTypeName $ var "rec"))
            ("_chk" ~> lets [
              "fieldsm">: Resolution.fieldMap @@ (Core.recordFields $ var "rec")] $
              Eithers.bind (Maybes.maybe
                (right $ project PGM._Schema PGM._Schema_defaultEdgeId @@ var "schema")
                (selectEdgeId @@ var "cx" @@ var "fieldsm")
                (var "mIdAdapter"))
                ("edgeId" ~> Eithers.bind (encodeProperties @@ var "cx" @@ var "fieldsm" @@ var "propAdapters")
                  ("props" ~> lets [
                    "getVertexId">: "dirCheck" ~> "adapter" ~>
                      Maybes.maybe
                        (right $ project PGM._Schema PGM._Schema_defaultVertexId @@ var "schema")
                        (selectVertexId @@ var "cx" @@ var "fieldsm")
                        (Logic.ifElse (Equality.equal (var "dir") (var "dirCheck"))
                          nothing
                          (var "adapter"))] $
                    Eithers.bind (var "getVertexId" @@ (inject PG._Direction PG._Direction_out unit) @@ var "outAdapter")
                      ("outId" ~> Eithers.bind (var "getVertexId" @@ (inject PG._Direction PG._Direction_in unit) @@ var "inAdapter")
                        ("inId" ~>
                          -- Find dependencies from vertex adapters
                          Eithers.bind (Eithers.map (lambda "xs" $ Maybes.cat (var "xs"))
                            (Eithers.mapList
                              ("va" ~> lets [
                                "fname">: Pairs.first $ var "va",
                                "ad">: Pairs.second $ var "va"] $
                                Maybes.maybe
                                  (right nothing)
                                  ("fterm" ~> Eithers.map (lambda "x" $ just (var "x"))
                                    (Coders.coderEncode (Coders.adapterCoder $ var "ad") @@ var "cx" @@ var "fterm"))
                                  (Maps.lookup (var "fname") (var "fieldsm")))
                              (var "vertexAdapters")))
                            ("deps" ~>
                              right (elementTreeEdge
                                @@ (record PG._Edge [
                                  PG._Edge_label>>: var "label",
                                  PG._Edge_id>>: var "edgeId",
                                  PG._Edge_out>>: var "outId",
                                  PG._Edge_in>>: var "inId",
                                  PG._Edge_properties>>: var "props"])
                                @@ var "deps"))))))))
        ("cx" ~> "_" ~> left (Ctx.inContext (Error.errorOther $ Error.otherError $ string "edge decoding is not yet supported") (var "cx"))))

-- | Create an edge id adapter
edgeIdAdapter :: TTermDefinition (Context -> Graph -> PGM.Schema Graph t v -> t -> Name -> Name -> [FieldType]
  -> Either (InContext Error) (Y.Maybe (Name, Adapter Type t Term v)))
edgeIdAdapter = define "edgeIdAdapter" $
  doc "Create an edge id adapter" $
  "cx" ~> "g" ~> "schema" ~> "eidType" ~> "name" ~> "idKey" ~> "fields" ~>
    Eithers.bind (findIdProjectionSpec @@ var "cx" @@ false @@ var "name" @@ var "idKey" @@ var "fields")
      ("mIdSpec" ~> Maybes.maybe
        (right nothing)
        ("idSpec" ~> Eithers.map (lambda "x" $ just (var "x")) (projectionAdapter @@ var "cx" @@ var "g" @@ var "eidType" @@ (project PGM._Schema PGM._Schema_edgeIds @@ var "schema") @@ var "idSpec" @@ string "id"))
        (var "mIdSpec"))

-- | Construct an element adapter for a given type
elementCoder :: TTermDefinition (Y.Maybe (PG.Direction, PG.VertexLabel) -> PGM.Schema Graph t v -> Type -> t -> t -> Context -> Graph
  -> Either (InContext Error) (Adapter Type (PG.ElementTypeTree t) Term (PG.ElementTree v)))
elementCoder = define "elementCoder" $
  doc "Construct an element adapter for a given type, interpreting it either as a vertex specification or an edge specification" $
  "mparent" ~> "schema" ~> "source" ~> "vidType" ~> "eidType" ~> "cx" ~> "g" ~> lets [
    "dir">: Maybes.maybe (inject PG._Direction PG._Direction_both unit) (lambda "p" $ Pairs.first (var "p")) (var "mparent"),
    "parentLabel">: Maybes.maybe (wrap PG._VertexLabel $ string "NOLABEL") (lambda "p" $ Pairs.second (var "p")) (var "mparent")] $
    cases _Type (Strip.deannotateType @@ var "source")
      (Just $ unexpectedE (var "cx") (string "record type") (string "other type")) [
      _Type_maybe>>: "ot" ~>
        elementCoder @@ var "mparent" @@ var "schema" @@ var "ot" @@ var "vidType" @@ var "eidType" @@ var "cx" @@ var "g",
      _Type_record>>: "fields" ~> lets [
        "name">: Core.name (string "placeholder"),
        "outVertexKey">: Core.name $ project PGM._AnnotationSchema PGM._AnnotationSchema_outVertex @@ (project PGM._Schema PGM._Schema_annotations @@ var "schema"),
        "outVertexLabelKey">: Core.name $ project PGM._AnnotationSchema PGM._AnnotationSchema_outVertexLabel @@ (project PGM._Schema PGM._Schema_annotations @@ var "schema"),
        "inVertexKey">: Core.name $ project PGM._AnnotationSchema PGM._AnnotationSchema_inVertex @@ (project PGM._Schema PGM._Schema_annotations @@ var "schema"),
        "inVertexLabelKey">: Core.name $ project PGM._AnnotationSchema PGM._AnnotationSchema_inVertexLabel @@ (project PGM._Schema PGM._Schema_annotations @@ var "schema")] $
        Eithers.bind (findProjectionSpec @@ var "cx" @@ var "g" @@ var "name" @@ var "outVertexKey" @@ var "outVertexLabelKey" @@ var "fields")
          ("mOutSpec" ~> Eithers.bind (findProjectionSpec @@ var "cx" @@ var "g" @@ var "name" @@ var "inVertexKey" @@ var "inVertexLabelKey" @@ var "fields")
            ("mInSpec" ~> lets [
              "kind">: Logic.ifElse (hasVertexAdapters @@ var "dir" @@ var "mOutSpec" @@ var "mInSpec")
                (inject PG._ElementKind PG._ElementKind_edge unit)
                (inject PG._ElementKind PG._ElementKind_vertex unit)] $
              Eithers.bind (findPropertySpecs @@ var "cx" @@ var "g" @@ var "schema" @@ var "kind" @@ var "fields")
                ("propSpecs" ~> Eithers.bind (Eithers.mapList (propertyAdapter @@ var "cx" @@ var "g" @@ var "schema") (var "propSpecs"))
                  ("propAdapters" ~>
                    match PG._ElementKind Nothing [
                      PG._ElementKind_vertex>>: constant $
                        constructVertexCoder @@ var "cx" @@ var "g" @@ var "schema" @@ var "source" @@ var "vidType" @@ var "eidType" @@ var "name" @@ var "fields" @@ var "propAdapters",
                      PG._ElementKind_edge>>: constant $
                        constructEdgeCoder @@ var "cx" @@ var "g" @@ var "parentLabel" @@ var "schema" @@ var "source" @@ var "vidType" @@ var "eidType" @@ var "dir" @@ var "name" @@ var "fields" @@ var "propAdapters" @@ var "mOutSpec" @@ var "mInSpec"]
                    @@ var "kind"))))]

-- | Create an element tree for an edge
elementTreeEdge :: TTermDefinition (PG.Edge v -> [PG.ElementTree v] -> PG.ElementTree v)
elementTreeEdge = define "elementTreeEdge" $
  doc "Create an element tree for an edge" $
  "edge" ~> "deps" ~>
    record PG._ElementTree [
      PG._ElementTree_self>>: inject PG._Element PG._Element_edge (var "edge"),
      PG._ElementTree_dependencies>>: var "deps"]

-- | Create an element tree for a vertex
elementTreeVertex :: TTermDefinition (PG.Vertex v -> [PG.ElementTree v] -> PG.ElementTree v)
elementTreeVertex = define "elementTreeVertex" $
  doc "Create an element tree for a vertex" $
  "vertex" ~> "deps" ~>
    record PG._ElementTree [
      PG._ElementTree_self>>: inject PG._Element PG._Element_vertex (var "vertex"),
      PG._ElementTree_dependencies>>: var "deps"]

-- | Create an element type tree for an edge type
elementTypeTreeEdge :: TTermDefinition (PG.EdgeType t -> [PG.ElementTypeTree t] -> PG.ElementTypeTree t)
elementTypeTreeEdge = define "elementTypeTreeEdge" $
  doc "Create an element type tree for an edge type" $
  "etype" ~> "deps" ~>
    record PG._ElementTypeTree [
      PG._ElementTypeTree_self>>: inject PG._ElementType PG._ElementType_edge (var "etype"),
      PG._ElementTypeTree_dependencies>>: var "deps"]

-- | Create an element type tree for a vertex type
elementTypeTreeVertex :: TTermDefinition (PG.VertexType t -> [PG.ElementTypeTree t] -> PG.ElementTypeTree t)
elementTypeTreeVertex = define "elementTypeTreeVertex" $
  doc "Create an element type tree for a vertex type" $
  "vtype" ~> "deps" ~>
    record PG._ElementTypeTree [
      PG._ElementTypeTree_self>>: inject PG._ElementType PG._ElementType_vertex (var "vtype"),
      PG._ElementTypeTree_dependencies>>: var "deps"]

-- | Encode all properties from a field map using property adapters
encodeProperties :: TTermDefinition (Context -> M.Map Name Term -> [Adapter FieldType (PG.PropertyType t) Field (PG.Property v)]
  -> Either (InContext Error) (M.Map PG.PropertyKey v))
encodeProperties = define "encodeProperties" $
  doc "Encode all properties from a field map using property adapters" $
  "cx" ~> "fields" ~> "adapters" ~>
    Eithers.map
      ("props" ~> Maps.fromList (Lists.map
        ("prop" ~> pair (project PG._Property PG._Property_key @@ var "prop") (project PG._Property PG._Property_value @@ var "prop"))
        (var "props")))
      (Eithers.map (lambda "xs" $ Maybes.cat (var "xs")) (Eithers.mapList (encodeProperty @@ var "cx" @@ var "fields") (var "adapters")))

-- | Encode a single property from a field map using a property adapter
encodeProperty :: TTermDefinition (Context -> M.Map Name Term -> Adapter FieldType (PG.PropertyType t) Field (PG.Property v)
  -> Either (InContext Error) (Y.Maybe (PG.Property v)))
encodeProperty = define "encodeProperty" $
  doc "Encode a single property from a field map using a property adapter" $
  "cx" ~> "fields" ~> "adapter" ~> lets [
    "fname">: Core.fieldTypeName $ (Coders.adapterSource $ var "adapter"),
    "ftyp">: Strip.deannotateType @@ (Core.fieldTypeType $ Coders.adapterSource $ var "adapter"),
    "isMaybe">: cases _Type (var "ftyp") (Just false) [
      _Type_maybe>>: constant true],
    "encodeValue">: "v" ~> Eithers.map (lambda "x" $ just (var "x"))
      (Coders.coderEncode (Coders.adapterCoder $ var "adapter") @@ var "cx" @@ (Core.field (var "fname") (var "v")))] $
    Maybes.maybe
      -- Field not found in record
      (Logic.ifElse (var "isMaybe")
        (right nothing)
        (err (var "cx") (string "expected field not found in record: " ++ (Core.unName $ var "fname"))))
      -- Field found in record
      ("value" ~>
        Logic.ifElse (var "isMaybe")
          -- Optional field: unwrap TermMaybe
          (cases _Term (Strip.deannotateTerm @@ var "value") (Just $ var "encodeValue" @@ var "value") [
            _Term_maybe>>: "ov" ~>
              Maybes.maybe (right nothing) (var "encodeValue") (var "ov")])
          -- Required field: encode directly
          (var "encodeValue" @@ var "value"))
      (Maps.lookup (var "fname") (var "fields"))

-- | Bridge an ExtractCore function into Result
extractString :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) String)
extractString = define "extractString" $
  doc "Extract a string from a term" $
  "cx" ~> "g" ~> "t" ~>
    ExtractCore.string @@ var "cx" @@ var "g" @@ var "t"

-- | Find adjacent edge adapters for a given direction
findAdjacenEdgeAdapters :: TTermDefinition (Context -> Graph -> PGM.Schema Graph t v -> t -> t -> PG.VertexLabel -> PG.Direction -> [FieldType]
  -> Either (InContext Error) [(PG.Direction, FieldType, PG.EdgeLabel, Adapter Type (PG.ElementTypeTree t) Term (PG.ElementTree v))])
findAdjacenEdgeAdapters = define "findAdjacenEdgeAdapters" $
  doc "Find adjacent edge adapters for a given direction" $
  "cx" ~> "g" ~> "schema" ~> "vidType" ~> "eidType" ~> "parentLabel" ~> "dir" ~> "fields" ~>
    Eithers.map (lambda "xs" $ Maybes.cat (var "xs")) (Eithers.mapList
      ("field" ~> lets [
        "key">: Core.name $ match PG._Direction Nothing [
          PG._Direction_out>>: constant $ project PGM._AnnotationSchema PGM._AnnotationSchema_outEdgeLabel @@ (project PGM._Schema PGM._Schema_annotations @@ var "schema"),
          PG._Direction_in>>: constant $ project PGM._AnnotationSchema PGM._AnnotationSchema_inEdgeLabel @@ (project PGM._Schema PGM._Schema_annotations @@ var "schema")]
          @@ var "dir"] $
        Maybes.maybe
          (right nothing)
          ("a" ~> Eithers.bind (extractString @@ var "cx" @@ var "g" @@ var "a")
            ("labelStr" ~> Eithers.bind (elementCoder @@ (just (pair (var "dir") (var "parentLabel"))) @@ var "schema" @@ (Core.fieldTypeType $ var "field") @@ var "vidType" @@ var "eidType" @@ var "cx" @@ var "g")
              ("elad" ~> right (just (tuple4 (var "dir") (var "field") (wrap PG._EdgeLabel $ var "labelStr") (var "elad"))))))
          (Annotations.getTypeAnnotation @@ var "key" @@ (Core.fieldTypeType $ var "field")))
      (var "fields"))

-- | Find an id projection spec for a field
findIdProjectionSpec :: TTermDefinition (Context -> Bool -> Name -> Name -> [FieldType]
  -> Either (InContext Error) (Y.Maybe (FieldType, PGM.ValueSpec, Y.Maybe String)))
findIdProjectionSpec = define "findIdProjectionSpec" $
  doc "Find an id projection spec for a field" $
  "cx" ~> "required" ~> "tname" ~> "idKey" ~> "fields" ~>
    Eithers.bind (findSingleFieldWithAnnotationKey @@ var "cx" @@ var "tname" @@ var "idKey" @@ var "fields")
      ("mid" ~> Maybes.maybe
        (Logic.ifElse (var "required")
          (err (var "cx") (string "no " ++ (Core.unName $ var "idKey") ++ string " field"))
          (right nothing))
        ("mi" ~> Eithers.map
          ("spec" ~> just (pair (var "mi") (pair (var "spec")
            (Maybes.map ("s" ~> Strings.toUpper (var "s")) nothing))))
          (Maybes.maybe
            (right (inject PGM._ValueSpec PGM._ValueSpec_value unit))
            (TermsToElements.decodeValueSpec @@ var "cx" @@ Graph.emptyGraph)
            (Annotations.getTypeAnnotation @@ var "idKey" @@ (Core.fieldTypeType $ var "mi"))))
        (var "mid"))

-- | Find an incident vertex adapter for a projection spec
findIncidentVertexAdapter :: TTermDefinition (Context -> Graph -> PGM.Schema Graph t v -> t -> t -> (FieldType, PGM.ValueSpec, Y.Maybe String)
  -> Either (InContext Error) (Name, Adapter Type (PG.ElementTypeTree t) Term (PG.ElementTree v)))
findIncidentVertexAdapter = define "findIncidentVertexAdapter" $
  doc "Find an incident vertex adapter for a projection spec" $
  "cx" ~> "g" ~> "schema" ~> "vidType" ~> "eidType" ~> "spec" ~> lets [
    "field">: Pairs.first $ var "spec"] $
    Eithers.bind (elementCoder @@ nothing @@ var "schema" @@ (Core.fieldTypeType $ var "field") @@ var "vidType" @@ var "eidType" @@ var "cx" @@ var "g")
      ("adapter" ~> right (pair (Core.fieldTypeName $ var "field") (var "adapter")))

-- | Find a label string from annotations or the type name
findLabelString :: TTermDefinition (Context -> Graph -> Type -> Name -> Name -> Either (InContext Error) String)
findLabelString = define "findLabelString" $
  doc "Find a label string from annotations or the type name" $
  "cx" ~> "g" ~> "source" ~> "tname" ~> "labelKey" ~>
    Maybes.maybe
      (right $ Core.unName $ var "tname")
      (extractString @@ var "cx" @@ var "g")
      (Annotations.getTypeAnnotation @@ var "labelKey" @@ var "source")

-- | Find a projection spec for a field
findProjectionSpec :: TTermDefinition (Context -> Graph -> Name -> Name -> Name -> [FieldType]
  -> Either (InContext Error) (Y.Maybe (FieldType, PGM.ValueSpec, Y.Maybe String)))
findProjectionSpec = define "findProjectionSpec" $
  doc "Find a projection spec for a field" $
  "cx" ~> "g" ~> "tname" ~> "key" ~> "aliasKey" ~> "fields" ~>
    Eithers.bind (findSingleFieldWithAnnotationKey @@ var "cx" @@ var "tname" @@ var "key" @@ var "fields")
      ("mfield" ~> Maybes.maybe
        (right nothing)
        ("field" ~>
          Eithers.bind (TermsToElements.decodeValueSpec @@ var "cx" @@ var "g" @@ (Maybes.fromJust (Annotations.getTypeAnnotation @@ var "key" @@ (Core.fieldTypeType $ var "field"))))
            ("spec" ~>
              Eithers.bind (Maybes.maybe (right nothing) ("t" ~> Eithers.map (lambda "x" $ just (var "x")) (extractString @@ var "cx" @@ var "g" @@ var "t"))
                (Annotations.getTypeAnnotation @@ var "aliasKey" @@ (Core.fieldTypeType $ var "field")))
                ("alias" ~> right (just (pair (var "field") (pair (var "spec") (var "alias")))))))
        (var "mfield"))

-- | Find property specs for element fields
findPropertySpecs :: TTermDefinition (Context -> Graph -> PGM.Schema Graph t v -> PG.ElementKind -> [FieldType]
  -> Either (InContext Error) [(FieldType, PGM.ValueSpec, Y.Maybe String)])
findPropertySpecs = define "findPropertySpecs" $
  doc "Find property specs for element fields" $
  "cx" ~> "g" ~> "schema" ~> "kind" ~> "fields" ~>
    Eithers.mapList
      ("field" ~> lets [
        "propKeyKey">: Core.name $ project PGM._AnnotationSchema PGM._AnnotationSchema_propertyKey @@ (project PGM._Schema PGM._Schema_annotations @@ var "schema"),
        "propValueKey">: Core.name $ project PGM._AnnotationSchema PGM._AnnotationSchema_propertyValue @@ (project PGM._Schema PGM._Schema_annotations @@ var "schema")] $
        Eithers.bind (Maybes.maybe (right nothing) ("a" ~> Eithers.map (lambda "x" $ just (var "x")) (extractString @@ var "cx" @@ var "g" @@ var "a"))
          (Annotations.getTypeAnnotation @@ var "propKeyKey" @@ (Core.fieldTypeType $ var "field")))
          ("alias" ~> Eithers.bind (Maybes.maybe (right (inject PGM._ValueSpec PGM._ValueSpec_value unit)) (TermsToElements.decodeValueSpec @@ var "cx" @@ var "g")
            (Annotations.getTypeAnnotation @@ var "propValueKey" @@ (Core.fieldTypeType $ var "field")))
            ("values" ~> right (pair (var "field") (pair (var "values") (var "alias"))))))
      (Lists.filter
        ("field" ~> lets [
          "annots">: project PGM._Schema PGM._Schema_annotations @@ var "schema",
          "ignoreKey">: Core.name $ project PGM._AnnotationSchema PGM._AnnotationSchema_ignore @@ var "annots",
          "specialKeys">: match PG._ElementKind Nothing [
            PG._ElementKind_vertex>>: constant $ list [
              Core.name $ project PGM._AnnotationSchema PGM._AnnotationSchema_vertexId @@ var "annots",
              Core.name $ project PGM._AnnotationSchema PGM._AnnotationSchema_outEdgeLabel @@ var "annots",
              Core.name $ project PGM._AnnotationSchema PGM._AnnotationSchema_inEdgeLabel @@ var "annots"],
            PG._ElementKind_edge>>: constant $ list [
              Core.name $ project PGM._AnnotationSchema PGM._AnnotationSchema_edgeId @@ var "annots",
              Core.name $ project PGM._AnnotationSchema PGM._AnnotationSchema_outVertex @@ var "annots",
              Core.name $ project PGM._AnnotationSchema PGM._AnnotationSchema_inVertex @@ var "annots"]]
            @@ var "kind",
          "allKeys">: Lists.concat $ list [list [var "ignoreKey"], var "specialKeys"],
          "hasSpecialAnnotation">: Lists.foldl
            ("b" ~> "k" ~> Logic.or (var "b") (Maybes.isJust (Annotations.getTypeAnnotation @@ var "k" @@ (Core.fieldTypeType $ var "field"))))
            false
            (var "allKeys"),
          "hasSpecialFieldName">: Lists.foldl
            ("b" ~> "k" ~> Logic.or (var "b") (Equality.equal (Core.fieldTypeName $ var "field") (var "k")))
            false
            (var "specialKeys")] $
          Logic.not (Logic.or (var "hasSpecialAnnotation") (var "hasSpecialFieldName")))
        (var "fields"))

-- | Find a single field with a given annotation key
findSingleFieldWithAnnotationKey :: TTermDefinition (Context -> Name -> Name -> [FieldType]
  -> Either (InContext Error) (Y.Maybe FieldType))
findSingleFieldWithAnnotationKey = define "findSingleFieldWithAnnotationKey" $
  doc "Find a single field with a given annotation key" $
  "cx" ~> "tname" ~> "key" ~> "fields" ~> lets [
    "matches">: Lists.filter
      ("f" ~> Maybes.isJust (Annotations.getTypeAnnotation @@ var "key" @@ (Core.fieldTypeType $ var "f")))
      (var "fields")] $
    Logic.ifElse (Equality.gt (Lists.length $ var "matches") (int32 1))
      (err (var "cx") (string "Multiple fields marked as '" ++ (Core.unName $ var "key") ++ string "' in record type " ++ (Core.unName $ var "tname")))
      (right (Lists.safeHead $ var "matches"))

-- | Determine whether the spec has vertex adapters based on direction and out/in specs
hasVertexAdapters :: TTermDefinition (PG.Direction -> Y.Maybe a -> Y.Maybe b -> Bool)
hasVertexAdapters = define "hasVertexAdapters" $
  doc "Determine whether the spec has vertex adapters based on direction and out/in specs" $
  "dir" ~> "mOutSpec" ~> "mInSpec" ~>
    match PG._Direction Nothing [
      PG._Direction_out>>: constant $ Maybes.isJust (var "mInSpec"),
      PG._Direction_in>>: constant $ Maybes.isJust (var "mOutSpec"),
      PG._Direction_both>>: constant $ Logic.and (Maybes.isJust (var "mOutSpec")) (Maybes.isJust (var "mInSpec"))]
    @@ var "dir"

-- | Create a projection adapter from a projection spec
projectionAdapter :: TTermDefinition (Context -> Graph -> t -> Coder Term v -> (FieldType, PGM.ValueSpec, Y.Maybe String) -> String
  -> Either (InContext Error) (Name, Adapter Type t Term v))
projectionAdapter = define "projectionAdapter" $
  doc "Create a projection adapter from a projection spec" $
  "cx" ~> "g" ~> "idtype" ~> "coder" ~> "spec" ~> "key" ~> lets [
    "field">: Pairs.first $ var "spec",
    "values">: Pairs.first $ Pairs.second $ var "spec"] $
    Eithers.bind (TermsToElements.parseValueSpec @@ var "cx" @@ var "g" @@ var "values")
      ("traversal" ~> right (pair
        (Core.fieldTypeName $ var "field")
        (Coders.adapter true (Core.fieldTypeType $ var "field") (var "idtype")
          (Coders.coder
            ("cx'" ~> "typ" ~>
              Eithers.bind (traverseToSingleTerm @@ var "cx'" @@ (var "key" ++ string "-projection") @@ (var "traversal" @@ var "cx'") @@ var "typ")
                ("t" ~> Coders.coderEncode (var "coder") @@ var "cx'" @@ var "t"))
            ("cx'" ~> "_" ~> left (Ctx.inContext (Error.errorOther $ Error.otherError $ string "edge '" ++ var "key" ++ string "' decoding is not yet supported") (var "cx'")))))))

-- | Create a property adapter from a property spec
propertyAdapter :: TTermDefinition (Context -> Graph -> PGM.Schema Graph t v -> (FieldType, PGM.ValueSpec, Y.Maybe String)
  -> Either (InContext Error) (Adapter FieldType (PG.PropertyType t) Field (PG.Property v)))
propertyAdapter = define "propertyAdapter" $
  doc "Create a property adapter from a property spec" $
  "cx" ~> "g" ~> "schema" ~> "spec" ~> lets [
    "tfield">: Pairs.first $ var "spec",
    "values">: Pairs.first $ Pairs.second $ var "spec",
    "alias">: Pairs.second $ Pairs.second $ var "spec",
    "key">: wrap PG._PropertyKey $ (Maybes.fromMaybe (Core.unName $ (Core.fieldTypeName $ var "tfield")) (var "alias"))] $
    Eithers.bind (Coders.coderEncode (project PGM._Schema PGM._Schema_propertyTypes @@ var "schema") @@ var "cx" @@ (Core.fieldTypeType $ var "tfield"))
      ("pt" ~> Eithers.bind (TermsToElements.parseValueSpec @@ var "cx" @@ var "g" @@ var "values")
        ("traversal" ~> right
          (Coders.adapter true (var "tfield")
            (record PG._PropertyType [
              PG._PropertyType_key>>: var "key",
              PG._PropertyType_value>>: var "pt",
              PG._PropertyType_required>>: true])
            (Coders.coder
              ("cx'" ~> "dfield" ~>
                Eithers.bind (traverseToSingleTerm @@ var "cx'" @@ string "property traversal" @@ (var "traversal" @@ var "cx'") @@ (Core.fieldTerm $ var "dfield"))
                  ("result" ~> Eithers.bind (Coders.coderEncode (project PGM._Schema PGM._Schema_propertyValues @@ var "schema") @@ var "cx'" @@ var "result")
                    ("value" ~> right (record PG._Property [
                      PG._Property_key>>: var "key",
                      PG._Property_value>>: var "value"]))))
              ("cx'" ~> "_" ~> left (Ctx.inContext (Error.errorOther $ Error.otherError $ string "property decoding is not yet supported") (var "cx'")))))))

-- | Extract property types from property adapters
propertyTypes :: TTermDefinition ([Adapter FieldType (PG.PropertyType t) Field (PG.Property v)] -> [PG.PropertyType t])
propertyTypes = define "propertyTypes" $
  doc "Extract property types from property adapters" $
  "propAdapters" ~>
    Lists.map
      ("a" ~> record PG._PropertyType [
        PG._PropertyType_key>>: project PG._PropertyType PG._PropertyType_key @@ (Coders.adapterTarget $ var "a"),
        PG._PropertyType_value>>: project PG._PropertyType PG._PropertyType_value @@ (Coders.adapterTarget $ var "a"),
        PG._PropertyType_required>>: true])
      (var "propAdapters")

-- | Select an edge id from record fields using an id adapter
selectEdgeId :: TTermDefinition (Context -> M.Map Name Term -> (Name, Adapter Type t Term v) -> Either (InContext Error) v)
selectEdgeId = define "selectEdgeId" $
  doc "Select an edge id from record fields using an id adapter" $
  "cx" ~> "fields" ~> "ad" ~> lets [
    "fname">: Pairs.first $ var "ad",
    "adapter">: Pairs.second $ var "ad"] $
    Maybes.maybe
      (err (var "cx") (string "no " ++ (Core.unName $ var "fname") ++ string " in record"))
      ("t" ~> Coders.coderEncode (Coders.adapterCoder $ var "adapter") @@ var "cx" @@ var "t")
      (Maps.lookup (var "fname") (var "fields"))

-- | Select a vertex id from record fields using an id adapter
selectVertexId :: TTermDefinition (Context -> M.Map Name Term -> (Name, Adapter Type t Term v) -> Either (InContext Error) v)
selectVertexId = define "selectVertexId" $
  doc "Select a vertex id from record fields using an id adapter" $
  "cx" ~> "fields" ~> "ad" ~> lets [
    "fname">: Pairs.first $ var "ad",
    "adapter">: Pairs.second $ var "ad"] $
    Maybes.maybe
      (err (var "cx") (string "no " ++ (Core.unName $ var "fname") ++ string " in record"))
      ("t" ~> Coders.coderEncode (Coders.adapterCoder $ var "adapter") @@ var "cx" @@ var "t")
      (Maps.lookup (var "fname") (var "fields"))

-- | Traverse to a single term, failing if zero or multiple terms are found
traverseToSingleTerm :: TTermDefinition (Context -> String -> (Term -> Either (InContext Error) [Term]) -> Term -> Either (InContext Error) Term)
traverseToSingleTerm = define "traverseToSingleTerm" $
  doc "Traverse to a single term, failing if zero or multiple terms are found" $
  "cx" ~> "desc" ~> "traversal" ~> "term" ~>
    Eithers.bind (var "traversal" @@ var "term")
      ("terms" ~>
        Logic.ifElse (Lists.null $ var "terms")
          (err (var "cx") (var "desc" ++ string " did not resolve to a term"))
          (Logic.ifElse (Equality.equal (Lists.length $ var "terms") (int32 1))
            (right $ Lists.head $ var "terms")
            (err (var "cx") (var "desc" ++ string " resolved to multiple terms"))))

-- | Create a vertex coder given all components
vertexCoder :: TTermDefinition (Graph -> PGM.Schema Graph t v -> Type -> t -> Name -> PG.VertexLabel
  -> (Name, Adapter Type t Term v)
  -> [Adapter FieldType (PG.PropertyType t) Field (PG.Property v)]
  -> [(PG.Direction, FieldType, PG.EdgeLabel, Adapter Type (PG.ElementTypeTree t) Term (PG.ElementTree v))]
  -> Adapter Type (PG.ElementTypeTree t) Term (PG.ElementTree v))
vertexCoder = define "vertexCoder" $
  doc "Create a vertex coder given all components" $
  "g" ~> "schema" ~> "source" ~> "vidType" ~> "tname" ~> "vlabel" ~>
  "idAdapter" ~> "propAdapters" ~> "edgeAdapters" ~> lets [
    "vtype">: record PG._VertexType [
      PG._VertexType_label>>: var "vlabel",
      PG._VertexType_id>>: var "vidType",
      PG._VertexType_properties>>: propertyTypes @@ var "propAdapters"],
    "depTypes">: Lists.map
      ("ea" ~> Coders.adapterTarget $ (Pairs.second $ Pairs.second $ Pairs.second $ var "ea"))
      (var "edgeAdapters"),
    "target">: elementTypeTreeVertex @@ var "vtype" @@ var "depTypes"] $
    Coders.adapter true (var "source") (var "target")
      (Coders.coder
        ("cx" ~> "term" ~>
          lets ["deannot">: Strip.deannotateTerm @@ var "term",
                -- Unwrap TermMaybe if present
                "unwrapped">: cases _Term (var "deannot") (Just $ var "deannot") [
                  _Term_maybe>>: "mt" ~> Maybes.fromMaybe (var "deannot") (var "mt")],
                "rec">: cases _Term (var "unwrapped") Nothing [
                  _Term_record>>: lambda "r" $ var "r"],
                "fmap">: Resolution.fieldMap @@ (Core.recordFields $ var "rec")] $
          Eithers.bind (selectVertexId @@ var "cx" @@ var "fmap" @@ var "idAdapter")
            ("vid" ~> Eithers.bind (encodeProperties @@ var "cx" @@ var "fmap" @@ var "propAdapters")
              ("props" ~>
                -- Find dependencies from edge adapters
                Eithers.bind (Eithers.map (lambda "xs" $ Lists.concat (var "xs"))
                  (Eithers.mapList
                    ("ea" ~> lets [
                      "eaDir">: Pairs.first $ var "ea",
                      "eaField">: Pairs.first $ Pairs.second $ var "ea",
                      "eaLabel">: Pairs.first $ Pairs.second $ Pairs.second $ var "ea",
                      "eaAdapter">: Pairs.second $ Pairs.second $ Pairs.second $ var "ea"] $
                      Maybes.maybe
                        (right (list ([] :: [TTerm (PG.ElementTree v)])))
                        ("fterm" ~> Eithers.map
                          ("tree" ~>
                            -- fixTree: inspect element tree self
                            match PG._Element Nothing [
                              PG._Element_vertex>>: "vtx" ~> lets [
                                -- Child is a vertex: create an edge connecting parent to child
                                "otherid">: project PG._Vertex PG._Vertex_id @@ var "vtx",
                                "edgeid">: project PGM._Schema PGM._Schema_defaultEdgeId @@ var "schema",
                                "outId">: match PG._Direction Nothing [
                                  PG._Direction_out>>: constant $ var "vid",
                                  PG._Direction_in>>: constant $ var "otherid"]
                                  @@ var "eaDir",
                                "inId">: match PG._Direction Nothing [
                                  PG._Direction_out>>: constant $ var "otherid",
                                  PG._Direction_in>>: constant $ var "vid"]
                                  @@ var "eaDir",
                                "edge">: inject PG._Element PG._Element_edge (record PG._Edge [
                                  PG._Edge_label>>: var "eaLabel",
                                  PG._Edge_id>>: var "edgeid",
                                  PG._Edge_out>>: var "outId",
                                  PG._Edge_in>>: var "inId",
                                  PG._Edge_properties>>: Maps.empty])] $
                                list [record PG._ElementTree [
                                  PG._ElementTree_self>>: var "edge",
                                  PG._ElementTree_dependencies>>: list [var "tree"]]],
                              PG._Element_edge>>: "edg" ~> lets [
                                -- Child is an edge: fix its out/in vertex id
                                "fixedEdge">: match PG._Direction Nothing [
                                  PG._Direction_out>>: constant $ record PG._Edge [
                                    PG._Edge_label>>: project PG._Edge PG._Edge_label @@ var "edg",
                                    PG._Edge_id>>: project PG._Edge PG._Edge_id @@ var "edg",
                                    PG._Edge_out>>: var "vid",
                                    PG._Edge_in>>: project PG._Edge PG._Edge_in @@ var "edg",
                                    PG._Edge_properties>>: project PG._Edge PG._Edge_properties @@ var "edg"],
                                  PG._Direction_in>>: constant $ record PG._Edge [
                                    PG._Edge_label>>: project PG._Edge PG._Edge_label @@ var "edg",
                                    PG._Edge_id>>: project PG._Edge PG._Edge_id @@ var "edg",
                                    PG._Edge_out>>: project PG._Edge PG._Edge_out @@ var "edg",
                                    PG._Edge_in>>: var "vid",
                                    PG._Edge_properties>>: project PG._Edge PG._Edge_properties @@ var "edg"]]
                                  @@ var "eaDir"] $
                                list [record PG._ElementTree [
                                  PG._ElementTree_self>>: inject PG._Element PG._Element_edge (var "fixedEdge"),
                                  PG._ElementTree_dependencies>>: project PG._ElementTree PG._ElementTree_dependencies @@ var "tree"]]]
                            @@ (project PG._ElementTree PG._ElementTree_self @@ var "tree"))
                          (Coders.coderEncode (Coders.adapterCoder $ var "eaAdapter") @@ var "cx" @@ var "fterm"))
                        (Maps.lookup (Core.fieldTypeName $ var "eaField") (var "fmap")))
                    (var "edgeAdapters")))
                  ("deps" ~>
                    right (elementTreeVertex
                      @@ (record PG._Vertex [
                        PG._Vertex_label>>: var "vlabel",
                        PG._Vertex_id>>: var "vid",
                        PG._Vertex_properties>>: var "props"])
                      @@ var "deps")))))
        ("cx" ~> "_" ~> left (Ctx.inContext (Error.errorOther $ Error.otherError $ string "vertex decoding is not yet supported") (var "cx"))))

-- | Create a vertex id adapter
vertexIdAdapter :: TTermDefinition (Context -> Graph -> PGM.Schema Graph t v -> t -> Name -> Name -> [FieldType]
  -> Either (InContext Error) (Name, Adapter Type t Term v))
vertexIdAdapter = define "vertexIdAdapter" $
  doc "Create a vertex id adapter" $
  "cx" ~> "g" ~> "schema" ~> "vidType" ~> "name" ~> "idKey" ~> "fields" ~>
    Eithers.bind (findIdProjectionSpec @@ var "cx" @@ true @@ var "name" @@ var "idKey" @@ var "fields")
      ("mIdSpec" ~> Eithers.bind (right $ Maybes.fromJust (var "mIdSpec"))
        ("idSpec" ~> projectionAdapter @@ var "cx" @@ var "g" @@ var "vidType" @@ (project PGM._Schema PGM._Schema_vertexIds @@ var "schema") @@ var "idSpec" @@ string "id"))
