{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Pg.SubtermGraph where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Core.Overlay.Haskell.Libraries
import qualified Hydra.Core.Dsl.Paths        as Paths
import qualified Hydra.Core.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Core.Dsl.Ast          as Ast
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Core.Dsl.Coders       as Coders
import qualified Hydra.Core.Dsl.Util      as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core         as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph        as Graph
import qualified Hydra.Core.Dsl.Json.Model         as Json
import qualified Hydra.Core.Dsl.Lib.Chars    as Chars
import qualified Hydra.Core.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Core.Dsl.Lib.Equality as Equality
import qualified Hydra.Core.Dsl.Lib.Lists    as Lists
import qualified Hydra.Core.Dsl.Lib.Literals as Literals
import qualified Hydra.Core.Dsl.Lib.Logic    as Logic
import qualified Hydra.Core.Dsl.Lib.Maps     as Maps
import qualified Hydra.Core.Dsl.Lib.Math     as Math
import qualified Hydra.Core.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Core.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Core.Dsl.Lib.Sets     as Sets
import qualified Hydra.Core.Dsl.Lib.Strings  as Strings
import qualified Hydra.Core.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Core.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Core.Overlay.Haskell.Dsl.Base         as MetaBase
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Core.Dsl.Packaging       as Packaging
import qualified Hydra.Core.Dsl.Parsing      as Parsing
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing      as Testing
import qualified Hydra.Core.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Core.Dsl.Topology     as Topology
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Core.Dsl.Typing       as Typing
import qualified Hydra.Core.Dsl.Util         as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Print.Paths as PrintPaths
import qualified Hydra.Sources.Kernel.Terms.Print.Core as PrintCore

-- Additional imports
import qualified Hydra.Pg.Model as PG
import qualified Hydra.Sources.Pg.Model as PgModel


define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

ns :: ModuleName
ns = ModuleName "hydra.pg.subtermGraph"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$>
              ([PrintPaths.ns, PrintCore.ns] L.++ (PgModel.ns:kernelTypesModuleNames)),
            moduleMetadata = Bootstrap.descriptionMetadata (Just
              ("The property-graph encoding of a term graph (hydra.core.subterms.TermGraph): a total,"
               <> " lossless map from the already-built, already-classified term-graph view of a typed"
               <> " graph onto hydra.pg's element model. No bespoke term traversal; this module consumes"
               <> " only the TermGraph interface."))}
  where
   definitions = [
     toDefinition encodeTermGraph,
     toDefinition encodeTermNode,
     toDefinition isLeafNode,
     toDefinition termAttributeLinkTag,
     toDefinition termAttributeLinkValue,
     toDefinition termNodeIdTerm,
     toDefinition termReferenceNodeId,
     toDefinition termReferencePrimitiveName]

-- | Encode a term node id (a root binding name plus a subterm path from that root) as a term, for use
--   as a property-graph vertex/edge id. The encoding is a single string: the root name, then '#' and
--   the printed subterm path if the path is non-empty (root nodes have an empty path and are identified
--   by name alone). Both components are independently round-trippable via hydra.core.print.paths, so
--   the pairing loses nothing.
termNodeIdTerm :: TypedTermDefinition (TermNodeId -> Term)
termNodeIdTerm = define "termNodeIdTerm" $
  doc "Encode a TermNodeId as a term: the root binding name, plus '#' and the printed path if non-empty" $
  "nid" ~>
  "root" <~ project _TermNodeId _TermNodeId_root @@ var "nid" $
  "path" <~ project _TermNodeId _TermNodeId_path @@ var "nid" $
  "steps" <~ unwrap _SubtermPath @@ var "path" $
  Core.termLiteral $ Core.literalString
    (Logic.ifElse (Lists.isEmpty $ var "steps")
      (Core.unName $ var "root")
      (Strings.concat (list [Core.unName $ var "root", string "#", PrintPaths.subtermPath @@ var "path"])))

-- | A term-graph node is a leaf, for property-folding purposes, if it has no `subterm` or `reference`
--   links - only (at most) `attribute` links. Such a node's own term is a scalar leaf (a literal or
--   unit; a literal carries its value as a single attribute link, per termAttributesOf, while unit has
--   none at all; variables and primitives are never nodes - they surface as reference links on the
--   parent). A leaf node is folded into a vertex property on its parent rather than given its own vertex.
isLeafNode :: TypedTermDefinition (TermNode -> Bool)
isLeafNode = define "isLeafNode" $
  doc "Whether a term-graph node has only attribute links (no subterm or reference links), and so folds into a parent property rather than its own vertex" $
  "node" ~>
  Lists.isEmpty (Lists.filter
    ("link" ~> match _TermLink (var "link") (Just true) [_TermLink_attribute>>: constant false])
    (project _TermNode _TermNode_links @@ var "node"))

-- | Resolve the binder node id of a lambda- or let-bound variable reference; Nothing for a primitive
--   reference, which is not a graph binding and so has no node to point to.
termReferenceNodeId :: TypedTermDefinition (TermReference -> Maybe TermNodeId)
termReferenceNodeId = define "termReferenceNodeId" $
  doc "The binder node id of a lambda- or let-bound variable reference; Nothing for a primitive reference" $
  "ref" ~>
  match _TermReference (var "ref") Nothing [
    _TermReference_lambda>>: "lv" ~> just (project _LambdaVariableReference _LambdaVariableReference_boundByNode @@ var "lv"),
    _TermReference_let>>: "lv" ~> just (project _LetVariableReference _LetVariableReference_bindingNode @@ var "lv"),
    _TermReference_primitive>>: constant nothing]

-- | The primitive's name, for a primitive reference; Nothing for a lambda- or let-bound reference.
termReferencePrimitiveName :: TypedTermDefinition (TermReference -> Maybe Name)
termReferencePrimitiveName = define "termReferencePrimitiveName" $
  doc "The primitive's name, for a primitive reference; Nothing otherwise" $
  "ref" ~>
  match _TermReference (var "ref") Nothing [
    _TermReference_lambda>>: constant nothing,
    _TermReference_let>>: constant nothing,
    _TermReference_primitive>>: "p" ~> just (project _PrimitiveReference _PrimitiveReference_name @@ var "p")]

-- | The variant tag of a term attribute link, used to disambiguate multiple attribute links occurring
--   at the same node (e.g. a projection's type name and field name both attach at the projection's own
--   path, so the bare path alone is not a unique property key).
termAttributeLinkTag :: TypedTermDefinition (TermAttributeLink -> String)
termAttributeLinkTag = define "termAttributeLinkTag" $
  doc "The variant tag of a term attribute link, used to disambiguate same-path attribute keys" $
  "attr" ~>
  match _TermAttributeLink (var "attr") Nothing [
    _TermAttributeLink_casesTypeName>>: constant $ string "casesTypeName",
    _TermAttributeLink_injectTypeName>>: constant $ string "injectTypeName",
    _TermAttributeLink_lambdaDomainGiven>>: constant $ string "lambdaDomainGiven",
    _TermAttributeLink_lambdaParameter>>: constant $ string "lambdaParameter",
    _TermAttributeLink_letBindingTypeSchemeGiven>>: constant $ string "letBindingTypeSchemeGiven",
    _TermAttributeLink_literal>>: constant $ string "literal",
    _TermAttributeLink_projectFieldName>>: constant $ string "projectFieldName",
    _TermAttributeLink_projectTypeName>>: constant $ string "projectTypeName",
    _TermAttributeLink_recordTypeName>>: constant $ string "recordTypeName",
    _TermAttributeLink_typeApplicationType>>: constant $ string "typeApplicationType",
    _TermAttributeLink_typeLambdaParameter>>: constant $ string "typeLambdaParameter",
    _TermAttributeLink_unwrapTypeName>>: constant $ string "unwrapTypeName",
    _TermAttributeLink_wrapTypeName>>: constant $ string "wrapTypeName"]

-- | Encode a term attribute link's payload (a name, type, literal, or pair) as a term property value.
termAttributeLinkValue :: TypedTermDefinition (TermAttributeLink -> Term)
termAttributeLinkValue = define "termAttributeLinkValue" $
  doc "Encode a term attribute link's payload as a term" $
  "attr" ~>
  match _TermAttributeLink (var "attr") Nothing [
    _TermAttributeLink_casesTypeName>>: "n" ~> Core.termLiteral $ Core.literalString $ Core.unName (var "n"),
    _TermAttributeLink_injectTypeName>>: "n" ~> Core.termLiteral $ Core.literalString $ Core.unName (var "n"),
    _TermAttributeLink_lambdaDomainGiven>>: "t" ~> Core.termLiteral $ Core.literalString $ PrintCore.type_ @@ var "t",
    _TermAttributeLink_lambdaParameter>>: "n" ~> Core.termLiteral $ Core.literalString $ Core.unName (var "n"),
    _TermAttributeLink_letBindingTypeSchemeGiven>>: "p" ~> Core.termLiteral $ Core.literalString $ Strings.concat (list [
      Core.unName (Pairs.first $ var "p"), string ": ", PrintCore.typeScheme @@ (Pairs.second $ var "p")]),
    _TermAttributeLink_literal>>: "l" ~> Core.termLiteral (var "l"),
    _TermAttributeLink_projectFieldName>>: "n" ~> Core.termLiteral $ Core.literalString $ Core.unName (var "n"),
    _TermAttributeLink_projectTypeName>>: "n" ~> Core.termLiteral $ Core.literalString $ Core.unName (var "n"),
    _TermAttributeLink_recordTypeName>>: "n" ~> Core.termLiteral $ Core.literalString $ Core.unName (var "n"),
    _TermAttributeLink_typeApplicationType>>: "t" ~> Core.termLiteral $ Core.literalString $ PrintCore.type_ @@ var "t",
    _TermAttributeLink_typeLambdaParameter>>: "n" ~> Core.termLiteral $ Core.literalString $ Core.unName (var "n"),
    _TermAttributeLink_unwrapTypeName>>: "n" ~> Core.termLiteral $ Core.literalString $ Core.unName (var "n"),
    _TermAttributeLink_wrapTypeName>>: "n" ~> Core.termLiteral $ Core.literalString $ Core.unName (var "n")]

-- | Encode one already-identified term-graph node into (this node's own properties, the vertices and
--   edges contributed by its children). Properties come from attribute links, leaf subterm children
--   (folded in rather than given their own vertex), and primitive references (which have no target node
--   to link to, so the primitive's name becomes a property instead). Non-leaf subterm children each
--   become their own vertex (recursively encoded); non-primitive reference links each become an edge
--   from this node to the referenced binder. The accumulated path is threaded through for printed-path
--   property/edge keys, relative to the enclosing root.
encodeTermNode :: TypedTermDefinition (Name -> SubtermPath -> TermNode -> (M.Map PG.PropertyKey Term, [PG.Vertex Term], [PG.Edge Term]))
encodeTermNode = define "encodeTermNode" $
  doc "Encode one term-graph node into (this node's own properties, descendant vertices, descendant edges)" $
  "root" ~> "path" ~> "node" ~>
  "nid" <~ Paths.termNodeId (var "root") (var "path") $
  "steps" <~ unwrap _SubtermPath @@ var "path" $
  "links" <~ project _TermNode _TermNode_links @@ var "node" $
  "childPathFor" <~ ("step" ~> Paths.subtermPath (Lists.concat2 (var "steps") (list [var "step"]))) $
  "keyFor" <~ ("p" ~> wrap PG._PropertyKey (PrintPaths.subtermPath @@ var "p")) $
  -- Attribute keys are suffixed by the attribute's own variant tag: several attribute links can occur
  -- at this same node (hence the same path), so the bare path alone would not be a unique key.
  "attrKeyFor" <~ ("a" ~> wrap PG._PropertyKey (Strings.concat (list [PrintPaths.subtermPath @@ var "path", string "@", termAttributeLinkTag @@ var "a"]))) $
  -- One (own-property?, descendant vertices, descendant edges) triple per link, as a pair of pairs
  -- (Hydra DSL tuples go no wider than pairs): (ownProperty, (vertices, edges)).
  "perLink" <~ Lists.map
    ("link" ~> match _TermLink (var "link")
      Nothing
      [
      _TermLink_attribute>>: "a" ~>
        pair (just (pair (var "attrKeyFor" @@ var "a") (termAttributeLinkValue @@ var "a")))
          (pair (list ([] :: [TypedTerm (PG.Vertex Term)])) (list ([] :: [TypedTerm (PG.Edge Term)]))),
      _TermLink_subterm>>: "sl" ~> lets [
        "step">: project _SubtermLink _SubtermLink_step @@ var "sl",
        "child">: project _SubtermLink _SubtermLink_child @@ var "sl",
        "childPath">: var "childPathFor" @@ var "step"]
        $ Logic.ifElse (isLeafNode @@ var "child")
          (pair (just (pair (var "keyFor" @@ var "childPath") (project _TermNode _TermNode_term @@ var "child")))
            (pair (list ([] :: [TypedTerm (PG.Vertex Term)])) (list ([] :: [TypedTerm (PG.Edge Term)]))))
          (lets [
            "sub">: encodeTermNode @@ var "root" @@ var "childPath" @@ var "child",
            "childVertex">: record PG._Vertex [
              PG._Vertex_label>>: wrap PG._VertexLabel (PrintCore.type_ @@ (project _TermNode _TermNode_type @@ var "child")),
              PG._Vertex_id>>: termNodeIdTerm @@ (Paths.termNodeId (var "root") (var "childPath")),
              PG._Vertex_properties>>: (Pairs.first (var "sub") :: TypedTerm (M.Map PG.PropertyKey Term))]]
            $ pair (nothing :: TypedTerm (Maybe (PG.PropertyKey, Term)))
                (pair
                  (Lists.concat2 (list [var "childVertex"]) (Pairs.first (Pairs.second (var "sub"))))
                  (Pairs.second (Pairs.second (var "sub"))))),
      _TermLink_reference>>: "rl" ~> lets [
        "step">: project _TermReferenceLink _TermReferenceLink_step @@ var "rl",
        "target">: project _TermReferenceLink _TermReferenceLink_target @@ var "rl",
        "edgePath">: var "childPathFor" @@ var "step",
        "edgeKey">: PrintPaths.subtermPath @@ var "edgePath",
        "noVertices">: (list ([] :: [TypedTerm (PG.Vertex Term)])),
        "noEdges">: (list ([] :: [TypedTerm (PG.Edge Term)])),
        "noProp">: (nothing :: TypedTerm (Maybe (PG.PropertyKey, Term)))]
        $ Optionals.match (termReferenceNodeId @@ var "target")
          -- A primitive reference has no target node; record the primitive's name as a property.
          (Optionals.match (termReferencePrimitiveName @@ var "target")
            (pair (var "noProp") (pair (var "noVertices") (var "noEdges")))
            ("pname" ~> pair
              (just (pair (var "keyFor" @@ var "edgePath") (Core.termLiteral $ Core.literalString $ Core.unName (var "pname"))))
              (pair (var "noVertices") (var "noEdges"))))
          ("targetId" ~> lets [
            "edge">: record PG._Edge [
              PG._Edge_label>>: wrap PG._EdgeLabel (var "edgeKey"),
              PG._Edge_id>>: Core.termLiteral $ Core.literalString (var "edgeKey"),
              PG._Edge_out>>: termNodeIdTerm @@ var "nid",
              PG._Edge_in>>: termNodeIdTerm @@ var "targetId",
              PG._Edge_properties>>: (Maps.empty :: TypedTerm (M.Map PG.PropertyKey Term))]]
            $ pair (var "noProp") (pair (var "noVertices") (list [var "edge"])))])
    (var "links") $
  "ownProps" <~ Maps.fromList (Optionals.mapOptional (reify Pairs.first) (var "perLink")) $
  "descendants" <~ Lists.map (reify $ Pairs.second) (var "perLink") $
  pair (var "ownProps")
    (pair (Lists.concat (Lists.map (reify Pairs.first) (var "descendants")))
          (Lists.concat (Lists.map (reify Pairs.second) (var "descendants"))))

-- | Encode a term graph into a property graph: one vertex per non-leaf term-graph node (root or inline
--   subterm), one edge per non-primitive reference link, and the vertex properties contributed by
--   attribute links, leaf subterm children, and primitive references. Pure and total over an
--   already-built TermGraph - callers thread the (fallible) hydra.core.subterms.graphToTermGraph build
--   separately (the pipeline is infer -> graphToTermGraph -> encodeTermGraph).
encodeTermGraph :: TypedTermDefinition (TermGraph -> PG.Graph Term)
encodeTermGraph = define "encodeTermGraph" $
  doc ("Encode a term graph as a property graph: a vertex per non-leaf node, an edge per non-primitive"
    <> " variable reference, printed subterm paths as edge labels/ids and property keys") $
  "tg" ~>
  "roots" <~ project _TermGraph _TermGraph_roots @@ var "tg" $
  "rootResults" <~ Lists.map
    ("nt" ~> lets [
      "name">: Pairs.first (var "nt"),
      "node">: Pairs.second (var "nt"),
      "emptyPath">: Paths.subtermPath (list ([] :: [TypedTerm SubtermStep])),
      "sub">: encodeTermNode @@ var "name" @@ var "emptyPath" @@ var "node",
      "rootVertex">: record PG._Vertex [
        PG._Vertex_label>>: wrap PG._VertexLabel (PrintCore.type_ @@ (project _TermNode _TermNode_type @@ var "node")),
        PG._Vertex_id>>: termNodeIdTerm @@ (Paths.termNodeId (var "name") (var "emptyPath")),
        PG._Vertex_properties>>: (Pairs.first (var "sub") :: TypedTerm (M.Map PG.PropertyKey Term))]]
      $ pair (Lists.concat2 (list [var "rootVertex"]) (Pairs.first (Pairs.second (var "sub"))))
             (Pairs.second (Pairs.second (var "sub"))))
    (Maps.toList (var "roots" :: TypedTerm (M.Map Name TermNode))) $
  "allVertices" <~ Lists.concat (Lists.map (reify Pairs.first) (var "rootResults")) $
  "allEdges" <~ Lists.concat (Lists.map (reify Pairs.second) (var "rootResults")) $
  record PG._Graph [
    PG._Graph_vertices>>: Maps.fromList (Lists.map ("v" ~> pair (project PG._Vertex PG._Vertex_id @@ var "v") (var "v")) (var "allVertices")),
    PG._Graph_edges>>: Maps.fromList (Lists.map ("e" ~> pair (project PG._Edge PG._Edge_id @@ var "e") (var "e")) (var "allEdges"))]
