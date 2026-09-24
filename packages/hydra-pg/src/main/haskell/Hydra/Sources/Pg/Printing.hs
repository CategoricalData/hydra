{-# LANGUAGE ScopedTypeVariables #-}
module Hydra.Sources.Pg.Printing where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (
  printEdge, printGraph, printLazyGraph, printProperty, printVertex)
import qualified Hydra.Core.Dsl.Lib.Strings                as Strings
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms                   as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Annotations                     as Annotations
import qualified Hydra.Core.Overlay.Haskell.Bootstrap                       as Bootstrap
import qualified Hydra.Core.Overlay.Haskell.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Core.Overlay.Haskell.Dsl.Literals                        as Literals
import qualified Hydra.Core.Dsl.Paths                      as Paths
import qualified Hydra.Core.Dsl.Ast                        as Ast
import qualified Hydra.Core.Overlay.Haskell.Dsl.Base                       as MetaBase
import qualified Hydra.Core.Dsl.Coders                     as Coders
import qualified Hydra.Core.Dsl.Util                    as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core                       as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Core.Dsl.Json.Model                       as Json
import qualified Hydra.Core.Dsl.Lib.Chars                  as Chars
import qualified Hydra.Core.Dsl.Lib.Eithers                as Eithers
import qualified Hydra.Core.Dsl.Lib.Equality               as Equality
import qualified Hydra.Core.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Core.Dsl.Lib.Literals               as Literals
import qualified Hydra.Core.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Core.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Core.Dsl.Lib.Math                   as Math
import qualified Hydra.Core.Dsl.Lib.Optionals                 as Optionals
import qualified Hydra.Core.Dsl.Lib.Pairs                  as Pairs
import qualified Hydra.Core.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Core.Dsl.Packaging                     as Packaging
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Core.Dsl.Topology                   as Topology
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Core.Dsl.Typing                     as Typing
import qualified Hydra.Core.Dsl.Util                       as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims                           as Prims
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Core.Overlay.Haskell.Dsl.Terms                           as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Tests                           as Tests
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types                           as Types
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
import qualified Hydra.Sources.Kernel.Terms.Print.Paths as PrintPaths
import qualified Hydra.Sources.Kernel.Terms.Print.Core      as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Print.Graph     as PrintGraph
import qualified Hydra.Sources.Kernel.Terms.Print.Variants  as PrintVariants
import qualified Hydra.Sources.Kernel.Terms.Print.Typing    as PrintTyping
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
import qualified Hydra.Sources.Pg.Model as PgModel


define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

ns :: ModuleName
ns = ModuleName "hydra.pg.printing"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ((PgModel.ns:KernelTypes.kernelTypesModuleNames)),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Printing functions for property graph elements")}
  where
    definitions = [
      toDefinition printEdge,
      toDefinition (printGraph :: TypedTermDefinition ((String -> String) -> PG.Graph String -> String)),
      toDefinition printLazyGraph,
      toDefinition printProperty,
      toDefinition printVertex]

-- | Print an edge using the provided printer functions
printEdge :: forall v. TypedTermDefinition ((v -> String) -> PG.Edge v -> String)
printEdge = define "printEdge" $
  doc "Print an edge using the provided value printer" $
  "printValue" ~> "edge" ~> lets [
    "label">: unwrap PG._EdgeLabel @@ (project PG._Edge PG._Edge_label @@ var "edge"),
    "id">: var "printValue" @@ (project PG._Edge PG._Edge_id @@ var "edge"),
    "outId">: var "printValue" @@ (project PG._Edge PG._Edge_out @@ var "edge"),
    "inId">: var "printValue" @@ (project PG._Edge PG._Edge_in @@ var "edge"),
    "props">: Strings.join (string ", ") (Lists.map
      ("p" ~> printProperty @@ var "printValue" @@ (Pairs.first $ var "p") @@ (Pairs.second $ var "p"))
      (Maps.toList ((project PG._Edge PG._Edge_properties @@ var "edge") :: TypedTerm (M.Map PG.PropertyKey v))))] $
    Strings.concat $ list [
      var "id", string ": ",
      string "(", var "outId", string ")-[:",
      var "label", string " {",
      var "props",
      string "}]->(",
      var "inId", string ")"]

-- | Print a graph using the provided printer functions
-- `Ord v` + `forall` because the graph's vertex/edge maps are keyed by the polymorphic vertex type
-- `v`, and the generated `Hydra.Core.Dsl.Lib.Maps` exposes the primitive's `Ord` key constraint (the old
-- hand-written `Meta.Lib.Maps` did not). This also forces a placeholder concrete type at registration
-- in `definitions`; `v` is phantom/erased so the choice is arbitrary. (printEdge/printVertex below use
-- a bare `forall v.` with no `Ord` — there the `forall` only names `v` for body annotations.) See #467.
printGraph :: forall v. Ord v => TypedTermDefinition ((v -> String) -> PG.Graph v -> String)
printGraph = define "printGraph" $
  doc "Print a graph using the provided value printer" $
  "printValue" ~> "graph" ~>
    printLazyGraph @@ var "printValue" @@
      (record PG._LazyGraph [
        PG._LazyGraph_vertices>>: Maps.elems ((project PG._Graph PG._Graph_vertices @@ var "graph") :: TypedTerm (M.Map v (PG.Vertex v))),
        PG._LazyGraph_edges>>: Maps.elems ((project PG._Graph PG._Graph_edges @@ var "graph") :: TypedTerm (M.Map v (PG.Edge v)))])

-- | Print a lazy graph using the provided printer functions
printLazyGraph :: TypedTermDefinition ((v -> String) -> PG.LazyGraph v -> String)
printLazyGraph = define "printLazyGraph" $
  doc "Print a lazy graph using the provided value printer" $
  "printValue" ~> "lg" ~> lets [
    "vertices">: project PG._LazyGraph PG._LazyGraph_vertices @@ var "lg",
    "edges">: project PG._LazyGraph PG._LazyGraph_edges @@ var "lg"] $
    Strings.concat $ list [
      string "vertices:",
      Strings.concat (Lists.map ("v" ~> Strings.concat (list [string "\n\t", printVertex @@ var "printValue" @@ var "v"])) (var "vertices")),
      string "\nedges:",
      Strings.concat (Lists.map ("e" ~> Strings.concat (list [string "\n\t", printEdge @@ var "printValue" @@ var "e"])) (var "edges"))]

-- | Print a property using the provided printer functions
printProperty :: TypedTermDefinition ((v -> String) -> PG.PropertyKey -> v -> String)
printProperty = define "printProperty" $
  doc "Print a property using the provided value printer" $
  "printValue" ~> "key" ~> "value" ~>
    Strings.concat $ list [
      unwrap PG._PropertyKey @@ var "key",
      string ": ",
      var "printValue" @@ var "value"]

-- | Print a vertex using the provided printer functions
printVertex :: forall v. TypedTermDefinition ((v -> String) -> PG.Vertex v -> String)
printVertex = define "printVertex" $
  doc "Print a vertex using the provided value printer" $
  "printValue" ~> "vertex" ~> lets [
    "label">: unwrap PG._VertexLabel @@ (project PG._Vertex PG._Vertex_label @@ var "vertex"),
    "id">: var "printValue" @@ (project PG._Vertex PG._Vertex_id @@ var "vertex"),
    "props">: Strings.join (string ", ") (Lists.map
      ("p" ~> printProperty @@ var "printValue" @@ (Pairs.first $ var "p") @@ (Pairs.second $ var "p"))
      (Maps.toList ((project PG._Vertex PG._Vertex_properties @@ var "vertex") :: TypedTerm (M.Map PG.PropertyKey v))))] $
    Strings.concat $ list [
      var "id", string ": (", var "label", string ": {",
      var "props",
      string "})"]
