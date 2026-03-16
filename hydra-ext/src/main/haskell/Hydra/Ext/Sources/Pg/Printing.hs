module Hydra.Ext.Sources.Pg.Printing where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (
  printEdge, printGraph, printLazyGraph, printProperty, printVertex)
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
import qualified Hydra.Pg.Model as PG
import qualified Hydra.Ext.Sources.Pg.Model as PgModel


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.pg.printing"

module_ :: Module
module_ = Module ns elements
    []
    (PgModel.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Printing functions for property graph elements"
  where
    elements = [
      toBinding printEdge,
      toBinding printGraph,
      toBinding printLazyGraph,
      toBinding printProperty,
      toBinding printVertex]

-- | Print an edge using the provided printer functions
printEdge :: TBinding ((v -> String) -> PG.Edge v -> String)
printEdge = define "printEdge" $
  doc "Print an edge using the provided value printer" $
  "printValue" ~> "edge" ~> lets [
    "label">: unwrap PG._EdgeLabel @@ (project PG._Edge PG._Edge_label @@ var "edge"),
    "id">: var "printValue" @@ (project PG._Edge PG._Edge_id @@ var "edge"),
    "outId">: var "printValue" @@ (project PG._Edge PG._Edge_out @@ var "edge"),
    "inId">: var "printValue" @@ (project PG._Edge PG._Edge_in @@ var "edge"),
    "props">: Strings.intercalate (string ", ") (Lists.map
      ("p" ~> printProperty @@ var "printValue" @@ (Pairs.first $ var "p") @@ (Pairs.second $ var "p"))
      (Maps.toList $ project PG._Edge PG._Edge_properties @@ var "edge"))] $
    Strings.cat $ list [
      var "id", string ": ",
      string "(", var "outId", string ")-[:",
      var "label", string " {",
      var "props",
      string "}]->(",
      var "inId", string ")"]

-- | Print a graph using the provided printer functions
printGraph :: TBinding ((v -> String) -> PG.Graph v -> String)
printGraph = define "printGraph" $
  doc "Print a graph using the provided value printer" $
  "printValue" ~> "graph" ~>
    printLazyGraph @@ var "printValue" @@
      (record PG._LazyGraph [
        PG._LazyGraph_vertices>>: Maps.elems (project PG._Graph PG._Graph_vertices @@ var "graph"),
        PG._LazyGraph_edges>>: Maps.elems (project PG._Graph PG._Graph_edges @@ var "graph")])

-- | Print a lazy graph using the provided printer functions
printLazyGraph :: TBinding ((v -> String) -> PG.LazyGraph v -> String)
printLazyGraph = define "printLazyGraph" $
  doc "Print a lazy graph using the provided value printer" $
  "printValue" ~> "lg" ~> lets [
    "vertices">: project PG._LazyGraph PG._LazyGraph_vertices @@ var "lg",
    "edges">: project PG._LazyGraph PG._LazyGraph_edges @@ var "lg"] $
    Strings.cat $ list [
      string "vertices:",
      Strings.cat (Lists.map ("v" ~> Strings.cat (list [string "\n\t", printVertex @@ var "printValue" @@ var "v"])) (var "vertices")),
      string "\nedges:",
      Strings.cat (Lists.map ("e" ~> Strings.cat (list [string "\n\t", printEdge @@ var "printValue" @@ var "e"])) (var "edges"))]

-- | Print a property using the provided printer functions
printProperty :: TBinding ((v -> String) -> PG.PropertyKey -> v -> String)
printProperty = define "printProperty" $
  doc "Print a property using the provided value printer" $
  "printValue" ~> "key" ~> "value" ~>
    Strings.cat $ list [
      unwrap PG._PropertyKey @@ var "key",
      string ": ",
      var "printValue" @@ var "value"]

-- | Print a vertex using the provided printer functions
printVertex :: TBinding ((v -> String) -> PG.Vertex v -> String)
printVertex = define "printVertex" $
  doc "Print a vertex using the provided value printer" $
  "printValue" ~> "vertex" ~> lets [
    "label">: unwrap PG._VertexLabel @@ (project PG._Vertex PG._Vertex_label @@ var "vertex"),
    "id">: var "printValue" @@ (project PG._Vertex PG._Vertex_id @@ var "vertex"),
    "props">: Strings.intercalate (string ", ") (Lists.map
      ("p" ~> printProperty @@ var "printValue" @@ (Pairs.first $ var "p") @@ (Pairs.second $ var "p"))
      (Maps.toList $ project PG._Vertex PG._Vertex_properties @@ var "vertex"))] $
    Strings.cat $ list [
      var "id", string ": (", var "label", string ": {",
      var "props",
      string "})"]
