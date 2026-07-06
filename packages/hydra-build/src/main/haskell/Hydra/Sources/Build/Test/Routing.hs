-- | Test cases for hydra.build.routing: manifest-derived, fail-loud module-to-package routing
module Hydra.Sources.Build.Test.Routing where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms hiding ((++))
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Testing
import qualified Hydra.Sources.Build.Routing as Routing
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Errors as ShowError
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Dsl.Packaging    as Packaging


ns :: ModuleName
ns = ModuleName "hydra.test.build.routing"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([Routing.ns, ShowCore.ns, ShowError.ns] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.build.routing")}
  where
    definitions = [Phantoms.toDefinition allTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
  doc "Test cases for hydra.build.routing" $
  supergroup "routing" [
    buildRoutingMapGroup,
    namespaceToPackageInGroup,
    groupByPackageInGroup]

-- Test fixture: two packages with a few declared modules each, including one whose derived
-- DSL/encode/decode names would collide with a declared module in the other package if
-- declared-vs-derived precedence were wrong.
mn :: String -> TypedTerm ModuleName
mn s = Packaging.moduleName2 (string s)

fixturePkgs :: TypedTerm [(String, [ModuleName])]
fixturePkgs = list [
  pair (string "hydra-kernel") (list [mn "hydra.strip", mn "hydra.sorting"]),
  pair (string "hydra-ext") (list [mn "hydra.avro.schema", mn "hydra.dsl.avro.schema"])]

fixtureMap :: TypedTerm (M.Map ModuleName String)
fixtureMap = Routing.buildRoutingMap @@ fixturePkgs

-- | Show a routing map as a sorted association-list string, for stable test comparison.
showRoutingMap :: TypedTerm (M.Map ModuleName String) -> TypedTerm String
showRoutingMap rm = ShowCore.list_ @@ showEntry @@ (Lists.sortOn showEntryKey (Maps.toList rm))
  where
    showEntry :: TypedTerm ((ModuleName, String) -> String)
    showEntry = lambda "e" (Strings.cat2
      (Strings.cat2 (Packaging.unModuleName (Pairs.first (var "e"))) (string " -> "))
      (Pairs.second (var "e")))
    showEntryKey :: TypedTerm ((ModuleName, String) -> String)
    showEntryKey = lambda "e" (Packaging.unModuleName (Pairs.first (var "e")))

showEither :: TypedTerm (Either Error String) -> TypedTerm String
showEither = Eithers.either
  (lambda "e" (Strings.cat2 (string "left(") (Strings.cat2 (ShowError.error_ @@ var "e") (string ")"))))
  (lambda "s" (Strings.cat2 (string "right(") (Strings.cat2 (var "s") (string ")"))))

buildRoutingMapGroup :: TypedTerm TestGroup
buildRoutingMapGroup = subgroup "buildRoutingMap" [
  -- Note: derived names are expanded for EVERY declared module, including hydra.dsl.avro.schema
  -- itself (already a derived-shaped name) -- so its own derived names (hydra.dsl.dsl.avro.schema,
  -- hydra.decode.dsl.avro.schema, etc.) also appear. This mirrors the native
  -- Hydra.PackageRouting.buildRoutingMap doc: "we expand the derived names for EVERY declared
  -- module... the extra map entries are harmless."
  universalCase "declared modules route to their declared package"
    (showRoutingMap fixtureMap)
    (string "[hydra.avro.schema -> hydra-ext, hydra.decode.avro.schema -> hydra-ext, hydra.decode.dsl.avro.schema -> hydra-ext, hydra.decode.sorting -> hydra-kernel, hydra.decode.strip -> hydra-kernel, hydra.dsl.avro.schema -> hydra-ext, hydra.dsl.dsl.avro.schema -> hydra-ext, hydra.dsl.sorting -> hydra-kernel, hydra.dsl.strip -> hydra-kernel, hydra.encode.avro.schema -> hydra-ext, hydra.encode.dsl.avro.schema -> hydra-ext, hydra.encode.sorting -> hydra-kernel, hydra.encode.strip -> hydra-kernel, hydra.sorting -> hydra-kernel, hydra.sources.decode.avro.schema -> hydra-ext, hydra.sources.decode.dsl.avro.schema -> hydra-ext, hydra.sources.decode.sorting -> hydra-kernel, hydra.sources.decode.strip -> hydra-kernel, hydra.sources.encode.avro.schema -> hydra-ext, hydra.sources.encode.dsl.avro.schema -> hydra-ext, hydra.sources.encode.sorting -> hydra-kernel, hydra.sources.encode.strip -> hydra-kernel, hydra.strip -> hydra-kernel]"),
  universalCase "an empty package list produces an empty routing map"
    (showRoutingMap (Routing.buildRoutingMap @@ list ([] :: [TypedTerm (String, [ModuleName])])))
    (string "[]"),
  -- Maps.fromList (which declaredPairs is built with) keeps the LAST entry for a duplicate key
  -- in list order; since pkgs are flattened in list order, the later package in the input list
  -- wins a same-module-name collision between two packages.
  universalCase "two packages declaring the same module name: the later package in the list wins"
    (showRoutingMap (Routing.buildRoutingMap @@ list [
      pair (string "hydra-first") (list [mn "hydra.shared"]),
      pair (string "hydra-second") (list [mn "hydra.shared"])]))
    (string "[hydra.decode.shared -> hydra-second, hydra.dsl.shared -> hydra-second, hydra.encode.shared -> hydra-second, hydra.shared -> hydra-second, hydra.sources.decode.shared -> hydra-second, hydra.sources.encode.shared -> hydra-second]")]

namespaceToPackageInGroup :: TypedTerm TestGroup
namespaceToPackageInGroup = subgroup "namespaceToPackageIn" [
  universalCase "a declared module routes to its package"
    (showEither (Routing.namespaceToPackageIn @@ fixtureMap @@ mn "hydra.strip"))
    (string "right(hydra-kernel)"),
  universalCase "a derived DSL module routes to the source module's package"
    (showEither (Routing.namespaceToPackageIn @@ fixtureMap @@ mn "hydra.dsl.strip"))
    (string "right(hydra-kernel)"),
  universalCase "a derived encode module routes to the source module's package"
    (showEither (Routing.namespaceToPackageIn @@ fixtureMap @@ mn "hydra.encode.avro.schema"))
    (string "right(hydra-ext)"),
  universalCase "a declared module that collides with another package's derived name wins as declared"
    (showEither (Routing.namespaceToPackageIn @@ fixtureMap @@ mn "hydra.dsl.avro.schema"))
    (string "right(hydra-ext)"),
  universalCase "an unrouted module fails loudly instead of falling back to a default package"
    (showEither (Routing.namespaceToPackageIn @@ fixtureMap @@ mn "hydra.nonexistent.module"))
    (string "left(unrouted module: hydra.nonexistent.module is not declared in any package's manifest (RoutingMap). Add it to the owning package's Manifest.mainModules.)")]

-- Test fixture modules for groupByPackageIn: minimal Module values naming declared namespaces.
testMod :: String -> TypedTerm Module
testMod s = Packaging.module_ (mn s) nothing (list ([] :: [TypedTerm ModuleDependency])) (list ([] :: [TypedTerm Definition]))

showGroups :: TypedTerm (Either Error [(String, [Module])]) -> TypedTerm String
showGroups = Eithers.either
  (lambda "e" (Strings.cat2 (string "left(") (Strings.cat2 (ShowError.error_ @@ var "e") (string ")"))))
  (lambda "groups" (Strings.cat2 (string "right(") (Strings.cat2
    (ShowCore.list_ @@ showGroup @@ var "groups")
    (string ")"))))
  where
    showGroup :: TypedTerm ((String, [Module]) -> String)
    showGroup = lambda "g" (Strings.cat2
      (Strings.cat2 (Pairs.first (var "g")) (string ": "))
      (ShowCore.list_ @@ showModName @@ (Lists.sortOn showModName (Pairs.second (var "g")))))
    showModName :: TypedTerm (Module -> String)
    showModName = lambda "m" (Packaging.unModuleName (Packaging.moduleName (var "m")))

groupByPackageInGroup :: TypedTerm TestGroup
groupByPackageInGroup = subgroup "groupByPackageIn" [
  universalCase "an empty module list produces an empty grouping"
    (showGroups (Routing.groupByPackageIn @@ fixtureMap @@ list ([] :: [TypedTerm Module])))
    (string "right([])"),
  universalCase "modules are partitioned and sorted by owning package"
    (showGroups (Routing.groupByPackageIn @@ fixtureMap @@ list [testMod "hydra.sorting", testMod "hydra.avro.schema", testMod "hydra.strip"]))
    (string "right([hydra-ext: [hydra.avro.schema], hydra-kernel: [hydra.sorting, hydra.strip]])"),
  universalCase "an unrouted module in the list fails the whole grouping loudly"
    (showGroups (Routing.groupByPackageIn @@ fixtureMap @@ list [testMod "hydra.strip", testMod "hydra.nonexistent.module"]))
    (string "left(unrouted module: hydra.nonexistent.module is not declared in any package's manifest (RoutingMap). Add it to the owning package's Manifest.mainModules.)")]
