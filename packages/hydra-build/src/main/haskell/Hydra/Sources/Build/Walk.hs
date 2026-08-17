module Hydra.Sources.Build.Walk where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Dsl.Paths        as Paths
import qualified Hydra.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Lib.Chars    as Chars
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Ordering as Ordering
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base         as MetaBase
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms        as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular           as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing      as Testing
import qualified Hydra.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants     as Variants
import qualified Hydra.Dsl.Errors       as Error
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y


-- | Pure directory-traversal decision helpers (#416, #666 enumeration cluster).
--
-- The build drivers enumerate directories in several places — check-oil-and-water
-- globs @heads/**/package.yaml@, digest discovery finds packages with digests,
-- orphan reconciliation walks generated trees. #666 ratified that recursive
-- traversal is composed (not a new primitive) and, following every other #416
-- promotion, the pure DECISION is promoted while the effect stays native: a
-- host-native walker performs the @listDirectory@\/@status@ recursion and passes
-- the flat relative paths it observed into these pure helpers, which decide
-- ordering and which paths match. This keeps the family effect-free and
-- translingual (no DSL term module composes effects) and honors #666's
-- "compose, don't add a primitive": the composition lives in the native driver
-- calling these promoted pure helpers.
--
-- Determinism — the whole point of the exercise, since @listDirectory@ is
-- unordered — lives entirely in the pure part ('sortPaths'). Paths are
-- forward-slash-separated relative strings, as the drivers already normalize.
ns :: ModuleName
ns = ModuleName "hydra.build.walk"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Pure directory-traversal decision helpers: deterministic ordering and basename matching over observed paths")}
  where
   definitions = [
     toDefinition baseName,
     toDefinition filterByBaseName,
     toDefinition matchesBaseName,
     toDefinition sortPaths]

-- | The final @/@-separated segment of a path: e.g. @"heads/java/package.yaml"@
-- yields @"package.yaml"@; a path with no @/@ yields itself. Empty input yields
-- empty.
baseName :: TypedTermDefinition (String -> String)
baseName = define "baseName" $
  doc "The final /-separated segment of a path (its file name)" $
  "path" ~>
  Optionals.cases
    (Lists.last (Strings.splitOn (string "/") (var "path")))
    (var "path")
    ("seg" ~> var "seg")

-- | The paths whose base name equals the given file name, ordered deterministically
-- (via 'sortPaths'). This is the pure core of a @**/<name>@ glob — e.g.
-- @filterByBaseName "package.yaml" observed@ selects every @heads/**/package.yaml@
-- the native walker found, in a stable order. The native driver does the walking;
-- this decides which paths count and in what order.
filterByBaseName :: TypedTermDefinition (String -> [String] -> [String])
filterByBaseName = define "filterByBaseName" $
  doc "Paths whose file name equals the given name, sorted deterministically" $
  "name" ~> "paths" ~>
  sortPaths @@ (Lists.filter ("path" ~> matchesBaseName @@ var "name" @@ var "path") (var "paths"))

-- | Whether a path's base name (final @/@-segment) equals the given file name.
-- Segment-wise, so @"package.yaml"@ matches @"heads/java/package.yaml"@ but not
-- @"heads/java/my-package.yaml"@.
matchesBaseName :: TypedTermDefinition (String -> String -> Bool)
matchesBaseName = define "matchesBaseName" $
  doc "Whether a path's file name equals the given name" $
  "name" ~> "path" ~>
  Equality.equal (baseName @@ var "path") (var "name")

-- | Sort a list of paths into a deterministic (ascending) order. This is where
-- the determinism lives — @listDirectory@ makes no ordering guarantee, so any
-- observed-path list must be sorted before it feeds a digest or a stable output.
sortPaths :: TypedTermDefinition ([String] -> [String])
sortPaths = define "sortPaths" $
  doc "Sort a list of paths into a deterministic ascending order" $
  "paths" ~> Lists.sort (var "paths")
