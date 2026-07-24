module Hydra.Sources.Build.ManifestWriter where

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

import qualified Hydra.Json.Model as JM
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
