{-# LANGUAGE FlexibleContexts #-}

-- | Test cases for hydra.pg.subtermGraph (the property-graph encoding of a term graph).
module Hydra.Sources.Pg.Test.SubtermGraph where

-- Standard imports for tests
import Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                 as Testing
import Hydra.Core.Overlay.Haskell.Dsl.Phantoms                as Phantoms hiding ((++))
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms                ((@@))
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core          as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph         as Graph
import qualified Hydra.Core.Dsl.Lib.Eithers                       as Eithers
import qualified Hydra.Core.Dsl.Lib.Lists                         as Lists
import qualified Hydra.Core.Dsl.Lib.Literals                      as Literals
import qualified Hydra.Core.Dsl.Lib.Maps                          as Maps
import qualified Hydra.Core.Dsl.Lib.Sets                          as Sets
import qualified Hydra.Core.Dsl.Paths                             as Paths
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Core.Testing

import qualified Hydra.Sources.Kernel.Terms.Literals as LiteralsModule
import qualified Hydra.Sources.Kernel.Terms.Print.Core as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Print.Paths as PrintPaths
import qualified Hydra.Sources.Kernel.Terms.Subterms as Subterms
import qualified Hydra.Sources.Pg.SubtermGraph as SubtermGraphEncoder

import qualified Hydra.Pg.Model as PG
import qualified Hydra.Sources.Pg.Model as PgModel


ns :: ModuleName
ns = ModuleName "hydra.pg.test.subtermGraph"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([LiteralsModule.ns, PrintCore.ns, PrintPaths.ns, Subterms.ns, SubtermGraphEncoder.ns] L.++ (PgModel.ns:kernelTypesModuleNames)),
            moduleMetadata = descriptionMetadata (Just "Test cases for the property-graph encoding of a term graph")}
  where
    definitions = [Phantoms.toDefinition allTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
    Phantoms.doc "Test cases for the SubtermGraph (term-graph) to property-graph encoding" $
    supergroup "subtermGraph" [
      helperGroup,
      endToEndGroup]

nm :: String -> TypedTerm Name
nm s = Core.name (string s)

intScheme :: TypedTerm TypeScheme
intScheme = Core.typeScheme (list ([] :: [TypedTerm Name])) (Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt32) Maps.empty

int32Term :: Int -> TypedTerm Term
int32Term n = Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ int32 n

-- | A minimal typed graph with one binding: `x = 42` (a literal). No references, no attributes
--   beyond the literal's own.
literalOnlyGraph :: TypedTerm Graph
literalOnlyGraph = Graph.graph
    (Maps.fromList (list [pair (nm "x") (int32Term 42)]))
    (Maps.fromList (list [pair (nm "x") intScheme]))
    Maps.empty
    Sets.empty
    Maps.empty
    Maps.empty
    Maps.empty
    Sets.empty

-- | A typed graph with two bindings: `x = 42` and `y = [x]` (a one-element list containing a
--   variable reference to the graph binding `x`). The reference is nested inside a container
--   rather than being y's bare top-level term: a root binding whose ENTIRE term is a bare
--   variable produces no reference link at all (graphToTermGraph's reference links are emitted
--   by the PARENT of a variable occurrence, via Rewriting.subtermsWithSteps over the parent's
--   children - a root term has no parent in this traversal, so a bare-variable root is a
--   degenerate case outside what this encoding can observe, not a bug in the encoder itself).
referenceGraph :: TypedTerm Graph
referenceGraph = Graph.graph
    (Maps.fromList (list [
      pair (nm "x") (int32Term 42),
      pair (nm "y") (Core.termList $ list [Core.termVariable $ nm "x"])]))
    (Maps.fromList (list [
      pair (nm "x") intScheme,
      pair (nm "y") (Core.typeScheme (list ([] :: [TypedTerm Name])) (Core.typeList $ Core.typeLiteral $ Core.literalTypeInteger Core.integerTypeInt32) Maps.empty)]))
    Maps.empty
    Sets.empty
    Maps.empty
    Maps.empty
    Maps.empty
    Sets.empty

-- | Build the term graph for a fixture, assuming the build succeeds (a precondition failure here is
--   a fixture bug, not something under test).
termGraphOf :: TypedTerm Graph -> TypedTerm TermGraph
termGraphOf g = Eithers.either
  (lambda "e" (Paths.termGraph Maps.empty))
  (lambda "tg" (var "tg"))
  (Subterms.graphToTermGraph @@ g)

-- | Unit-level tests for the small, hand-predictable helper functions. universalCase compares
--   printed strings (UniversalTestCase.actual/expected are both String), so a Term-typed result
--   is printed via PrintCore.term rather than compared directly.
helperGroup :: TypedTerm TestGroup
helperGroup = subgroup "helpers" [
    universalCase "termNodeIdTerm (root, empty path)"
      (PrintCore.term @@ (SubtermGraphEncoder.termNodeIdTerm @@ (Paths.termNodeId (nm "x") (Paths.subtermPath (list ([] :: [TypedTerm SubtermStep]))))))
      (PrintCore.term @@ (Core.termLiteral $ Core.literalString (string "x"))),
    universalCase "termAttributeLinkTag (literal)"
      (SubtermGraphEncoder.termAttributeLinkTag @@ (Paths.termAttributeLinkLiteral (Core.literalInteger $ Core.integerValueInt32 $ int32 1)))
      (string "literal")]

-- | End-to-end: build a term graph via the real `graphToTermGraph`, then encode it, and check the
--   resulting property graph's shape (vertex/edge counts), rather than a fully hand-predicted
--   structure - the exact intermediate TermGraph shape is not hand-predictable (per #716's own
--   documented experience with golden captures), but these counts are. Counts are printed via
--   Literals.printInt32 before comparing, since universalCase requires String on both sides (an
--   Int32-vs-Int32 comparison is just as much a mismatch at the kernel inference level as a
--   Term-vs-String one - matching Haskell types on both sides is not sufficient).
endToEndGroup :: TypedTerm TestGroup
endToEndGroup = subgroup "end-to-end (graphToTermGraph -> encodeTermGraph)" [
    universalCase "literal-only graph: one vertex"
      (Literals.printInt32 $ vertexCount literalOnlyGraph)
      (Literals.printInt32 $ int32 1),
    universalCase "literal-only graph: zero edges"
      (Literals.printInt32 $ edgeCount literalOnlyGraph)
      (Literals.printInt32 $ int32 0),
    universalCase "reference graph: two vertices (x and y)"
      (Literals.printInt32 $ vertexCount referenceGraph)
      (Literals.printInt32 $ int32 2),
    universalCase "reference graph: one edge (y references x)"
      (Literals.printInt32 $ edgeCount referenceGraph)
      (Literals.printInt32 $ int32 1)]
  where
    pgOf g = SubtermGraphEncoder.encodeTermGraph @@ (termGraphOf g)
    vertexCount g = Lists.length (Maps.elems ((project PG._Graph PG._Graph_vertices @@ pgOf g) :: TypedTerm (M.Map Term (PG.Vertex Term))))
    edgeCount g = Lists.length (Maps.elems ((project PG._Graph PG._Graph_edges @@ pgOf g) :: TypedTerm (M.Map Term (PG.Edge Term))))
