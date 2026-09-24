module Hydra.Sources.Build.ManifestWriter where

-- Standard imports for kernel terms modules
import Hydra.Kernel
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
import qualified Hydra.Core.Dsl.Errors       as Error
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Core.Json.Model as JM
import qualified Hydra.Sources.Json.Model as JsonModel


-- | Pure per-package manifest.json field-assembly and serialization logic,
-- shared by the code-generation drivers (#607, following #560's promotion of
-- hydra.build.routing).
--
-- Each driver (Haskell/Java/Python) groups a host's generated modules by
-- owning package (via hydra.build.routing, #560) and then, for every owned
-- package, writes a manifest.json describing that package's main/test/DSL/
-- encoding module namespaces. This module promotes the field-assembly and
-- JSON-object construction -- the part previously duplicated as
-- @namespacesArray@ (Java) / @_namespaces_array@ (Python) / inline
-- @namespacesJson@ (Haskell) plus each driver's manifest field list -- into a
-- single translingual definition. Grouping-by-package (hydra.build.routing),
-- the recursive directory walk, and the file write stay host-native.
--
-- Deliberately distinct from hydra.build.modules (#560; pure module-list
-- utilities: dedup, kernel/type filters, path helpers) -- manifest
-- field-assembly/serialization is a different concern and does not belong
-- there.
ns :: ModuleName
ns = ModuleName "hydra.build.manifestWriter"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([JsonModel.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Per-package manifest.json field-assembly and serialization logic shared by the code-generation drivers")}
  where
   definitions = [
     toDefinition packageManifestJson,
     toDefinition sortedNamespaces]

-- | The sorted namespace strings of a list of modules, as a JSON string array.
--
-- Namespaces are sorted lexicographically to match the byte-stable ordering
-- the drivers have always emitted (see docs/json-format.md "Stability of
-- byte order"). Mirrors Java's @namespacesArray@ / Python's
-- @_namespaces_array@ / the inline @namespacesJson@ helper in the Haskell
-- driver.
sortedNamespaces :: TypedTermDefinition ([Module] -> JM.Value)
sortedNamespaces = define "sortedNamespaces" $
  doc "The sorted namespace strings of a list of modules, as a JSON string array" $
  "mods" ~>
  "names" <~ Lists.sort (Lists.map
    ("m" ~> Packaging.unModuleName (Packaging.moduleName $ var "m")) (var "mods")) $
  Json.valueArray $ Lists.map ("n" ~> Json.valueString $ var "n") (var "names")

-- | The manifest.json contents for one package, as a JSON value.
--
-- Fields are alphabetized (mainDslModules, mainEncodingModules, mainModules,
-- manifestFormatVersion, package, testModules) to match the byte order the
-- drivers have always emitted; this function's output must remain
-- byte-identical to the current hand-coded drivers (#607's hard constraint).
--
-- Arguments are already scoped to a single package (the caller groups by
-- package via hydra.build.routing.groupByPackageIn, #560); this function does
-- no grouping or I/O.
-- | Supported downstream entry point: a project generating code from its own Hydra
-- modules can call this directly (with its own package name and module lists) to
-- produce a manifest.json, with no routing and no dependency on a pre-existing
-- manifest tree. See docs/recipes/downstream-codegen.md. (#650)
packageManifestJson :: TypedTermDefinition (
  String -> [Module] -> [Module] -> [Module] -> [Module] -> JM.Value)
packageManifestJson = define "packageManifestJson" $
  doc "The manifest.json contents for one package: field assembly + serialization, given the package name and its main/dsl/encoding/test module lists" $
  "pkg" ~> "mainMods" ~> "dslMods" ~> "encMods" ~> "testMods" ~>
  Json.valueObject $ list [
    pair (string "mainDslModules") (sortedNamespaces @@ var "dslMods"),
    pair (string "mainEncodingModules") (sortedNamespaces @@ var "encMods"),
    pair (string "mainModules") (sortedNamespaces @@ var "mainMods"),
    pair (string "manifestFormatVersion") (Json.valueNumber $ Literals.bigintToDecimal $ bigint 1),
    pair (string "package") (Json.valueString $ var "pkg"),
    pair (string "testModules") (sortedNamespaces @@ var "testMods")]
