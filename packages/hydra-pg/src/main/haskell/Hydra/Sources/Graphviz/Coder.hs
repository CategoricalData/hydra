module Hydra.Sources.Graphviz.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
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
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
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
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Graphviz.Dot as Dot
import qualified Hydra.Sources.Graphviz.Dot as DotSyntax


ns :: Namespace
ns = Namespace "hydra.graphviz.coder"

module_ :: Module
module_ = Module ns definitions
    [ShowPaths.ns, Names.ns, Rewriting.ns]
    (DotSyntax.ns:kernelTypesNamespaces) $
    Just "Functions for converting Hydra terms to Graphviz DOT graphs"
  where
    definitions = [
      toDefinition labelAttr,
      toDefinition labelAttrs,
      toDefinition nodeStyleElement,
      toDefinition nodeStylePrimitive,
      toDefinition nodeStyleSimple,
      toDefinition nodeStyleVariable,
      toDefinition standardNamespaces,
      toDefinition termLabel,
      toDefinition termToDotGraph,
      toDefinition termToDotStmts,
      toDefinition termToSubtermDotGraph,
      toDefinition termToSubtermDotStmts,
      toDefinition toEdgeStmt,
      toDefinition toNodeId,
      toDefinition toNodeOrSubgraph]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- NodeStyle is represented as a string constant, as there is no custom type definition in the target schema.
-- We use string tags to distinguish styles: "simple", "element", "variable", "primitive"

-- | Create a label attribute equality pair
labelAttr :: TTermDefinition (String -> Dot.EqualityPair)
labelAttr = define "labelAttr" $
  doc "Create a DOT label attribute" $
  "lab" ~>
    record Dot._EqualityPair [
      Dot._EqualityPair_left>>: wrap Dot._Id (string "label"),
      Dot._EqualityPair_right>>: wrap Dot._Id (var "lab")]

-- | Create label attributes with style
labelAttrs :: TTermDefinition (String -> String -> Dot.AttrList)
labelAttrs = define "labelAttrs" $
  doc "Create DOT label attributes with a node style" $
  "style" ~> "lab" ~> lets [
    "styleAttrs">:
      Logic.ifElse (Equality.equal (var "style") nodeStyleSimple)
        (list ([] :: [TTerm Dot.EqualityPair]))
        (Logic.ifElse (Equality.equal (var "style") nodeStyleElement)
          (list [
            record Dot._EqualityPair [Dot._EqualityPair_left>>: wrap Dot._Id (string "style"), Dot._EqualityPair_right>>: wrap Dot._Id (string "filled")],
            record Dot._EqualityPair [Dot._EqualityPair_left>>: wrap Dot._Id (string "fillcolor"), Dot._EqualityPair_right>>: wrap Dot._Id (string "lightyellow")]])
          (Logic.ifElse (Equality.equal (var "style") nodeStyleVariable)
            (list [
              record Dot._EqualityPair [Dot._EqualityPair_left>>: wrap Dot._Id (string "style"), Dot._EqualityPair_right>>: wrap Dot._Id (string "filled")],
              record Dot._EqualityPair [Dot._EqualityPair_left>>: wrap Dot._Id (string "fillcolor"), Dot._EqualityPair_right>>: wrap Dot._Id (string "lightcyan")]])
            (list [
              record Dot._EqualityPair [Dot._EqualityPair_left>>: wrap Dot._Id (string "style"), Dot._EqualityPair_right>>: wrap Dot._Id (string "filled")],
              record Dot._EqualityPair [Dot._EqualityPair_left>>: wrap Dot._Id (string "fillcolor"), Dot._EqualityPair_right>>: wrap Dot._Id (string "linen")]])))]
    $ wrap Dot._AttrList (list [Lists.concat2 (list [labelAttr @@ var "lab"]) (var "styleAttrs")])

nodeStyleElement :: TTermDefinition String
nodeStyleElement = define "nodeStyleElement" $
  doc "The 'element' node style" $
  string "element"

nodeStylePrimitive :: TTermDefinition String
nodeStylePrimitive = define "nodeStylePrimitive" $
  doc "The 'primitive' node style" $
  string "primitive"

nodeStyleSimple :: TTermDefinition String
nodeStyleSimple = define "nodeStyleSimple" $
  doc "The 'simple' node style" $
  string "simple"

nodeStyleVariable :: TTermDefinition String
nodeStyleVariable = define "nodeStyleVariable" $
  doc "The 'variable' node style" $
  string "variable"

-- | Construct the standard namespaces map from the standard libraries
standardNamespaces :: TTermDefinition (M.Map Namespace String)
standardNamespaces = define "standardNamespaces" $
  doc "Construct a map from namespace to prefix for all standard libraries" $
  Phantoms.map $ M.fromList [(wrap _Namespace (string (unNamespace ns_)), string (libraryPrefix lib)) | lib <- standardLibraries, let ns_ = libraryNamespace lib]

-- | Compute the label and style for a term
termLabel :: TTermDefinition (Bool -> M.Map Namespace String -> Term -> (String, String))
termLabel = define "termLabel" $
  doc "Compute a label and node style for a term" $
  "compact" ~> "namespaces" ~> "term" ~> lets [
    "simpleLabel">: "lab" ~> pair (var "lab") nodeStyleSimple] $
    match _Term (Just $ var "simpleLabel" @@ string "?") [
      _Term_annotated>>: constant $ var "simpleLabel" @@ string "@{}",
      _Term_application>>: constant $ var "simpleLabel" @@ (Logic.ifElse (var "compact") (string "$") (string "apply")),
      _Term_lambda>>: constant $ var "simpleLabel" @@ (Logic.ifElse (var "compact") (string "\x03BB") (string "lambda")),
      _Term_project>>: "proj" ~>
        var "simpleLabel" @@ Strings.cat (list [
          string "{",
          Names.compactName @@ var "namespaces" @@ (project _Projection _Projection_typeName @@ var "proj"),
          string "}.",
          Core.unName (project _Projection _Projection_field @@ var "proj")]),
      _Term_cases>>: "cs" ~>
        var "simpleLabel" @@ Strings.cat (list [
          string "cases_{",
          Names.compactName @@ var "namespaces" @@ (project _CaseStatement _CaseStatement_typeName @@ var "cs"),
          string "}"]),
      _Term_unwrap>>: "name" ~>
        var "simpleLabel" @@ Strings.cat (list [
          string "unwrap_{",
          Names.compactName @@ var "namespaces" @@ var "name",
          string "}"]),
      _Term_let>>: constant $ var "simpleLabel" @@ string "let",
      _Term_list>>: constant $ var "simpleLabel" @@ (Logic.ifElse (var "compact") (string "[]") (string "list")),
      _Term_literal>>: "l" ~>
        var "simpleLabel" @@ (match _Literal (Just $ string "?") [
          _Literal_binary>>: "s" ~> Literals.binaryToString (var "s"),
          _Literal_boolean>>: "b" ~> Literals.showBoolean (var "b"),
          _Literal_integer>>: "i" ~>
            match _IntegerValue (Just $ string "?") [
              _IntegerValue_bigint>>: "v" ~> Literals.showBigint (var "v"),
              _IntegerValue_int8>>: "v" ~> Literals.showInt8 (var "v"),
              _IntegerValue_int16>>: "v" ~> Literals.showInt16 (var "v"),
              _IntegerValue_int32>>: "v" ~> Literals.showInt32 (var "v"),
              _IntegerValue_int64>>: "v" ~> Literals.showInt64 (var "v"),
              _IntegerValue_uint8>>: "v" ~> Literals.showUint8 (var "v"),
              _IntegerValue_uint16>>: "v" ~> Literals.showUint16 (var "v"),
              _IntegerValue_uint32>>: "v" ~> Literals.showUint32 (var "v"),
              _IntegerValue_uint64>>: "v" ~> Literals.showUint64 (var "v")]
              @@ var "i",
          _Literal_float>>: "f" ~>
            match _FloatValue (Just $ string "?") [
              _FloatValue_bigfloat>>: "v" ~> Literals.showBigfloat (var "v"),
              _FloatValue_float32>>: "v" ~> Literals.showFloat32 (var "v"),
              _FloatValue_float64>>: "v" ~> Literals.showFloat64 (var "v")]
              @@ var "f",
          _Literal_string>>: "s" ~> var "s"]
          @@ var "l"),
      _Term_map>>: constant $ var "simpleLabel" @@ (Logic.ifElse (var "compact") (string "<,>") (string "map")),
      _Term_maybe>>: constant $ var "simpleLabel" @@ (Logic.ifElse (var "compact") (string "opt") (string "optional")),
      _Term_record>>: "rec" ~>
        var "simpleLabel" @@ Strings.cat2 (string "\x2227") (Names.compactName @@ var "namespaces" @@ (project _Record _Record_typeName @@ var "rec")),
      _Term_typeLambda>>: constant $ var "simpleLabel" @@ string "tyabs",
      _Term_typeApplication>>: constant $ var "simpleLabel" @@ string "tyapp",
      _Term_inject>>: "inj" ~>
        var "simpleLabel" @@ Strings.cat2 (string "\x22BB") (Names.compactName @@ var "namespaces" @@ (project _Injection _Injection_typeName @@ var "inj")),
      _Term_variable>>: "name" ~>
        var "simpleLabel" @@ (Names.compactName @@ var "namespaces" @@ var "name"),
      _Term_wrap>>: "wt" ~>
        var "simpleLabel" @@ Strings.cat (list [
          string "(",
          Names.compactName @@ var "namespaces" @@ (project _WrappedTerm _WrappedTerm_typeName @@ var "wt"),
          string ")"])]
      @@ var "term"

-- | Convert a term to a full DOT graph
termToDotGraph :: TTermDefinition (Term -> Dot.Graph)
termToDotGraph = define "termToDotGraph" $
  doc "Convert a term to a full DOT graph" $
  "term" ~>
    record Dot._Graph [
      Dot._Graph_strict>>: false,
      Dot._Graph_directed>>: true,
      Dot._Graph_id>>: nothing,
      Dot._Graph_statements>>: termToDotStmts @@ standardNamespaces @@ var "term"]

-- | Convert a term to full DOT statements with structural detail
termToDotStmts :: TTermDefinition (M.Map Namespace String -> Term -> [Dot.Stmt])
termToDotStmts = define "termToDotStmts" $
  doc "Convert a term to full DOT statements showing term structure" $
  "namespaces" ~> "term" ~> lets [
    -- encode is the main recursive helper
    -- Parameters: mlabstyle isElement ids mparent stmtsVisited accessorTerm
    -- stmtsVisited is a pair of (stmts, visited)
    -- accessorTerm is a pair of (accessor, term)
    -- Returns a pair of (stmts, visited)
    "encode">: lambdas ["mlabstyle", "isElement", "ids", "mparent", "stmtsVisited", "accessorTerm"] $ lets [
      "accessor">: Pairs.first $ var "accessorTerm",
      "currentTerm">: Pairs.second $ var "accessorTerm",
      "stmts">: Pairs.first $ var "stmtsVisited",
      "visited">: Pairs.second $ var "stmtsVisited",
      -- Compute term label and style
      "termLS">: termLabel @@ true @@ var "namespaces" @@ var "currentTerm",
      "rawLabel">: Pairs.first $ var "termLS",
      "termNodeStyle">: Pairs.second $ var "termLS",
      -- labelOf: compute unique label and style for a term
      "labelOf">: "vis" ~> "t" ~> lets [
        "tls">: termLabel @@ true @@ var "namespaces" @@ var "t",
        "l">: Pairs.first $ var "tls",
        "s">: Pairs.second $ var "tls"]
        $ pair (Names.uniqueLabel @@ var "vis" @@ var "l") (var "s"),
      -- Determine actual label and style
      "labstyle">: Maybes.maybe
        (var "labelOf" @@ var "visited" @@ var "currentTerm")
        ("ls" ~> var "ls")
        (var "mlabstyle"),
      "label">: Pairs.first $ var "labstyle",
      "style">: Pairs.second $ var "labstyle",
      "nodeStyle">: Logic.ifElse (var "isElement") nodeStyleElement (var "termNodeStyle"),
      "selfId">: wrap Dot._Id (var "label"),
      "selfVisited">: Sets.insert (var "label") (var "visited"),
      -- Node statement for this term
      "nodeStmt">: inject Dot._Stmt Dot._Stmt_node (record Dot._NodeStmt [
        Dot._NodeStmt_id>>: toNodeId @@ var "selfId",
        Dot._NodeStmt_attributes>>: just $ labelAttrs @@ var "nodeStyle" @@ var "rawLabel"]),
      -- Edge to parent (accessor edge)
      "toAccessorEdgeStmt">: lambdas ["acc", "sty", "i1", "i2"] $
        toEdgeStmt @@ var "i1" @@ var "i2" @@
          (Maybes.map ("s" ~> labelAttrs @@ var "sty" @@ var "s") (ShowPaths.subtermStep @@ var "acc")),
      "edgeAttrs">: "lab" ~>
        wrap Dot._AttrList (list [list [record Dot._EqualityPair [Dot._EqualityPair_left>>: wrap Dot._Id (string "label"), Dot._EqualityPair_right>>: wrap Dot._Id (var "lab")]]]),
      "parentStmt">: Maybes.maybe
        (list ([] :: [TTerm Dot.Stmt]))
        ("parent" ~> list [var "toAccessorEdgeStmt" @@ var "accessor" @@ var "style" @@ var "parent" @@ var "selfId"])
        (var "mparent"),
      "selfStmts">: Lists.concat (list [var "stmts", list [var "nodeStmt"], var "parentStmt"]),
      -- Default case: fold over subterms
      "dflt">: Lists.foldl
        (var "encode" @@ nothing @@ false @@ var "ids" @@ just (var "selfId"))
        (pair (var "selfStmts") (var "selfVisited"))
        (Rewriting.subtermsWithSteps @@ var "currentTerm")]
      -- Main case dispatch on the current term
      $ match _Term (Just $ var "dflt") [
          _Term_lambda>>: "lam" ~> lets [
            "v">: Core.lambdaParameter $ var "lam",
            "body">: Core.lambdaBody $ var "lam",
            "vstr">: Core.unName (var "v"),
            "varLabel">: Names.uniqueLabel @@ var "selfVisited" @@ var "vstr",
            "varId">: wrap Dot._Id (var "varLabel"),
            "visited1">: Sets.insert (var "varLabel") (var "selfVisited"),
            "ids1">: Maps.insert (var "v") (var "varId") (var "ids"),
            "varNodeStmt">: inject Dot._Stmt Dot._Stmt_node (record Dot._NodeStmt [
              Dot._NodeStmt_id>>: toNodeId @@ var "varId",
              Dot._NodeStmt_attributes>>: just $ labelAttrs @@ nodeStyleVariable @@ var "vstr"]),
            "varEdgeStmt">: inject Dot._Stmt Dot._Stmt_edge (record Dot._EdgeStmt [
              Dot._EdgeStmt_left>>: toNodeOrSubgraph @@ var "selfId",
              Dot._EdgeStmt_right>>: list [toNodeOrSubgraph @@ var "varId"],
              Dot._EdgeStmt_attributes>>: just $ var "edgeAttrs" @@ string "var"])]
            $ var "encode" @@ nothing @@ false @@ var "ids1" @@ just (var "selfId")
                @@ pair (Lists.concat (list [var "selfStmts", list [var "varNodeStmt", var "varEdgeStmt"]])) (var "visited1")
                @@ pair Paths.subtermStepLambdaBody (var "body"),
          _Term_let>>: "letExpr" ~> lets [
            "bindings">: Core.letBindings $ var "letExpr",
            "env">: Core.letBody $ var "letExpr",
            -- First pass: compute ids and visited for all binding names
            "addBindingIds">: lambdas ["idsVis", "binding"] $ lets [
              "curIds">: Pairs.first $ var "idsVis",
              "curVis">: Pairs.second $ var "idsVis",
              "bname">: Core.bindingName $ var "binding",
              "bterm">: Core.bindingTerm $ var "binding",
              "bls">: var "labelOf" @@ var "curVis" @@ var "bterm",
              "blab">: Pairs.first $ var "bls"]
              $ pair (Maps.insert (var "bname") (wrap Dot._Id (var "blab")) (var "curIds"))
                     (Sets.insert (var "blab") (var "curVis")),
            "idsVis1">: Lists.foldl (var "addBindingIds") (pair (var "ids") (var "visited")) (var "bindings"),
            "ids1">: Pairs.first $ var "idsVis1",
            -- Second pass: encode each binding term
            "addBindingTerm">: lambdas ["stVis", "binding"] $ lets [
              "bname">: Core.bindingName $ var "binding",
              "bterm">: Core.bindingTerm $ var "binding",
              "blab">: unwrap Dot._Id @@ (Maybes.fromMaybe (wrap Dot._Id (string "?")) (Maps.lookup (var "bname") (var "ids1")))]
              $ var "encode"
                  @@ just (pair (var "blab") nodeStyleElement) @@ true @@ var "ids1" @@ just (var "selfId")
                  @@ var "stVis"
                  @@ pair (Paths.subtermStepLetBinding (var "bname")) (var "bterm"),
            "stmts1">: Lists.foldl (var "addBindingTerm") (pair (var "selfStmts") (var "selfVisited")) (var "bindings")]
            -- Encode the let body
            $ var "encode" @@ nothing @@ false @@ var "ids1" @@ just (var "selfId")
                @@ var "stmts1"
                @@ pair Paths.subtermStepLetBody (var "env"),
          _Term_variable>>: "name" ~>
            Maybes.maybe
              (var "dflt")
              ("i" ~> pair
                (Lists.concat2 (var "stmts") (list [var "toAccessorEdgeStmt" @@ var "accessor" @@ var "style" @@ (Maybes.fromMaybe (var "selfId") (var "mparent")) @@ var "i"]))
                (var "visited"))
              (Maps.lookup (var "name") (var "ids"))]
        @@ var "currentTerm"]
    $ Pairs.first $ var "encode"
        @@ nothing @@ false @@ Maps.empty @@ nothing
        @@ pair (list ([] :: [TTerm Dot.Stmt])) Sets.empty
        @@ pair Paths.subtermStepAnnotatedBody (var "term")

-- | Convert a term to an subterm-style DOT graph
termToSubtermDotGraph :: TTermDefinition (Term -> Dot.Graph)
termToSubtermDotGraph = define "termToSubtermDotGraph" $
  doc "Convert a term to an subterm-style DOT graph" $
  "term" ~>
    record Dot._Graph [
      Dot._Graph_strict>>: false,
      Dot._Graph_directed>>: true,
      Dot._Graph_id>>: nothing,
      Dot._Graph_statements>>: termToSubtermDotStmts @@ standardNamespaces @@ var "term"]

-- | Convert a term to subterm-style DOT statements
termToSubtermDotStmts :: TTermDefinition (M.Map Namespace String -> Term -> [Dot.Stmt])
termToSubtermDotStmts = define "termToSubtermDotStmts" $
  doc "Convert a term to subterm-style DOT statements" $
  "namespaces" ~> "term" ~> lets [
    "accessorGraph">: ShowPaths.termToSubtermGraph @@ var "namespaces" @@ var "term",
    "nodes">: project _SubtermGraph _SubtermGraph_nodes @@ var "accessorGraph",
    "edges">: project _SubtermGraph _SubtermGraph_edges @@ var "accessorGraph",
    "nodeStmt">: "node" ~>
      inject Dot._Stmt Dot._Stmt_node (record Dot._NodeStmt [
        Dot._NodeStmt_id>>: toNodeId @@ wrap Dot._Id (project _SubtermNode _SubtermNode_id @@ var "node"),
        Dot._NodeStmt_attributes>>: just $ wrap Dot._AttrList (list [list [labelAttr @@ (project _SubtermNode _SubtermNode_label @@ var "node")]])]),
    "edgeStmt">: "edge" ~> lets [
      "lab1">: project _SubtermNode _SubtermNode_id @@ (project _SubtermEdge _SubtermEdge_source @@ var "edge"),
      "lab2">: project _SubtermNode _SubtermNode_id @@ (project _SubtermEdge _SubtermEdge_target @@ var "edge"),
      "pathAccessors">: unwrap _SubtermPath @@ (project _SubtermEdge _SubtermEdge_path @@ var "edge"),
      "showPath">: Strings.intercalate (string "/") (Maybes.cat (Lists.map ShowPaths.subtermStep (var "pathAccessors")))]
      $ toEdgeStmt @@ wrap Dot._Id (var "lab1") @@ wrap Dot._Id (var "lab2") @@
          (just $ wrap Dot._AttrList (list [list [labelAttr @@ var "showPath"]]))]
    $ Lists.concat2 (Lists.map (var "nodeStmt") (var "nodes")) (Lists.map (var "edgeStmt") (var "edges"))

-- | Create an edge statement
toEdgeStmt :: TTermDefinition (Dot.Id -> Dot.Id -> Maybe Dot.AttrList -> Dot.Stmt)
toEdgeStmt = define "toEdgeStmt" $
  doc "Create a DOT edge statement" $
  "i1" ~> "i2" ~> "attrs" ~>
    inject Dot._Stmt Dot._Stmt_edge (record Dot._EdgeStmt [
      Dot._EdgeStmt_left>>: toNodeOrSubgraph @@ var "i1",
      Dot._EdgeStmt_right>>: list [toNodeOrSubgraph @@ var "i2"],
      Dot._EdgeStmt_attributes>>: var "attrs"])

-- | Create a node ID from a DOT Id
toNodeId :: TTermDefinition (Dot.Id -> Dot.NodeId)
toNodeId = define "toNodeId" $
  doc "Create a DOT NodeId from an Id" $
  "i" ~> record Dot._NodeId [Dot._NodeId_id>>: var "i", Dot._NodeId_port>>: nothing]

-- | Create a NodeOrSubgraph from an Id
toNodeOrSubgraph :: TTermDefinition (Dot.Id -> Dot.NodeOrSubgraph)
toNodeOrSubgraph = define "toNodeOrSubgraph" $
  doc "Create a DOT NodeOrSubgraph from an Id" $
  "i" ~> inject Dot._NodeOrSubgraph Dot._NodeOrSubgraph_node (toNodeId @@ var "i")
