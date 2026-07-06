-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for hydra.build.routing

module Hydra.Test.Build.Routing where

import qualified Hydra.Ast as Ast
import qualified Hydra.Build.Routing as Routing
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Docs as Docs
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.File as ErrorFile
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Error.System as ErrorSystem
import qualified Hydra.Errors as Errors
import qualified Hydra.File as File
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Overlay.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Overlay.Haskell.Lib.Lists as Lists
import qualified Hydra.Overlay.Haskell.Lib.Maps as Maps
import qualified Hydra.Overlay.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Overlay.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Show.Errors as ShowErrors
import qualified Hydra.System as System
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Time as Time
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Test cases for hydra.build.routing
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "routing",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "buildRoutingMap",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "declared modules route to their declared package",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.list (\e -> Strings.cat2 (Strings.cat2 (Packaging.unModuleName (Pairs.first e)) " -> ") (Pairs.second e)) (Lists.sortOn (\e -> Packaging.unModuleName (Pairs.first e)) (Maps.toList (Routing.buildRoutingMap [
                  (
                    "hydra-kernel",
                    [
                      Packaging.ModuleName "hydra.strip",
                      (Packaging.ModuleName "hydra.sorting")]),
                  (
                    "hydra-ext",
                    [
                      Packaging.ModuleName "hydra.avro.schema",
                      (Packaging.ModuleName "hydra.dsl.avro.schema")])])))),
                Testing.universalTestCaseExpected = (\_ -> "[hydra.avro.schema -> hydra-ext, hydra.decode.avro.schema -> hydra-ext, hydra.decode.dsl.avro.schema -> hydra-ext, hydra.decode.sorting -> hydra-kernel, hydra.decode.strip -> hydra-kernel, hydra.dsl.avro.schema -> hydra-ext, hydra.dsl.dsl.avro.schema -> hydra-ext, hydra.dsl.sorting -> hydra-kernel, hydra.dsl.strip -> hydra-kernel, hydra.encode.avro.schema -> hydra-ext, hydra.encode.dsl.avro.schema -> hydra-ext, hydra.encode.sorting -> hydra-kernel, hydra.encode.strip -> hydra-kernel, hydra.sorting -> hydra-kernel, hydra.sources.decode.avro.schema -> hydra-ext, hydra.sources.decode.dsl.avro.schema -> hydra-ext, hydra.sources.decode.sorting -> hydra-kernel, hydra.sources.decode.strip -> hydra-kernel, hydra.sources.encode.avro.schema -> hydra-ext, hydra.sources.encode.dsl.avro.schema -> hydra-ext, hydra.sources.encode.sorting -> hydra-kernel, hydra.sources.encode.strip -> hydra-kernel, hydra.strip -> hydra-kernel]")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "an empty package list produces an empty routing map",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.list (\e -> Strings.cat2 (Strings.cat2 (Packaging.unModuleName (Pairs.first e)) " -> ") (Pairs.second e)) (Lists.sortOn (\e -> Packaging.unModuleName (Pairs.first e)) (Maps.toList (Routing.buildRoutingMap [])))),
                Testing.universalTestCaseExpected = (\_ -> "[]")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "two packages declaring the same module name: the later package in the list wins",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.list (\e -> Strings.cat2 (Strings.cat2 (Packaging.unModuleName (Pairs.first e)) " -> ") (Pairs.second e)) (Lists.sortOn (\e -> Packaging.unModuleName (Pairs.first e)) (Maps.toList (Routing.buildRoutingMap [
                  ("hydra-first", [
                    Packaging.ModuleName "hydra.shared"]),
                  ("hydra-second", [
                    Packaging.ModuleName "hydra.shared"])])))),
                Testing.universalTestCaseExpected = (\_ -> "[hydra.decode.shared -> hydra-second, hydra.dsl.shared -> hydra-second, hydra.encode.shared -> hydra-second, hydra.shared -> hydra-second, hydra.sources.decode.shared -> hydra-second, hydra.sources.encode.shared -> hydra-second]")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "namespaceToPackageIn",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "a declared module routes to its package",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> Strings.cat2 "left(" (Strings.cat2 (ShowErrors.error e) ")")) (\s -> Strings.cat2 "right(" (Strings.cat2 s ")")) (Routing.namespaceToPackageIn (Routing.buildRoutingMap [
                  (
                    "hydra-kernel",
                    [
                      Packaging.ModuleName "hydra.strip",
                      (Packaging.ModuleName "hydra.sorting")]),
                  (
                    "hydra-ext",
                    [
                      Packaging.ModuleName "hydra.avro.schema",
                      (Packaging.ModuleName "hydra.dsl.avro.schema")])]) (Packaging.ModuleName "hydra.strip"))),
                Testing.universalTestCaseExpected = (\_ -> "right(hydra-kernel)")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "a derived DSL module routes to the source module's package",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> Strings.cat2 "left(" (Strings.cat2 (ShowErrors.error e) ")")) (\s -> Strings.cat2 "right(" (Strings.cat2 s ")")) (Routing.namespaceToPackageIn (Routing.buildRoutingMap [
                  (
                    "hydra-kernel",
                    [
                      Packaging.ModuleName "hydra.strip",
                      (Packaging.ModuleName "hydra.sorting")]),
                  (
                    "hydra-ext",
                    [
                      Packaging.ModuleName "hydra.avro.schema",
                      (Packaging.ModuleName "hydra.dsl.avro.schema")])]) (Packaging.ModuleName "hydra.dsl.strip"))),
                Testing.universalTestCaseExpected = (\_ -> "right(hydra-kernel)")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "a derived encode module routes to the source module's package",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> Strings.cat2 "left(" (Strings.cat2 (ShowErrors.error e) ")")) (\s -> Strings.cat2 "right(" (Strings.cat2 s ")")) (Routing.namespaceToPackageIn (Routing.buildRoutingMap [
                  (
                    "hydra-kernel",
                    [
                      Packaging.ModuleName "hydra.strip",
                      (Packaging.ModuleName "hydra.sorting")]),
                  (
                    "hydra-ext",
                    [
                      Packaging.ModuleName "hydra.avro.schema",
                      (Packaging.ModuleName "hydra.dsl.avro.schema")])]) (Packaging.ModuleName "hydra.encode.avro.schema"))),
                Testing.universalTestCaseExpected = (\_ -> "right(hydra-ext)")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "a declared module that collides with another package's derived name wins as declared",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> Strings.cat2 "left(" (Strings.cat2 (ShowErrors.error e) ")")) (\s -> Strings.cat2 "right(" (Strings.cat2 s ")")) (Routing.namespaceToPackageIn (Routing.buildRoutingMap [
                  (
                    "hydra-kernel",
                    [
                      Packaging.ModuleName "hydra.strip",
                      (Packaging.ModuleName "hydra.sorting")]),
                  (
                    "hydra-ext",
                    [
                      Packaging.ModuleName "hydra.avro.schema",
                      (Packaging.ModuleName "hydra.dsl.avro.schema")])]) (Packaging.ModuleName "hydra.dsl.avro.schema"))),
                Testing.universalTestCaseExpected = (\_ -> "right(hydra-ext)")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "an unrouted module fails loudly instead of falling back to a default package",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> Strings.cat2 "left(" (Strings.cat2 (ShowErrors.error e) ")")) (\s -> Strings.cat2 "right(" (Strings.cat2 s ")")) (Routing.namespaceToPackageIn (Routing.buildRoutingMap [
                  (
                    "hydra-kernel",
                    [
                      Packaging.ModuleName "hydra.strip",
                      (Packaging.ModuleName "hydra.sorting")]),
                  (
                    "hydra-ext",
                    [
                      Packaging.ModuleName "hydra.avro.schema",
                      (Packaging.ModuleName "hydra.dsl.avro.schema")])]) (Packaging.ModuleName "hydra.nonexistent.module"))),
                Testing.universalTestCaseExpected = (\_ -> "left(unrouted module: hydra.nonexistent.module is not declared in any package's manifest (RoutingMap). Add it to the owning package's Manifest.mainModules.)")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "groupByPackageIn",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "an empty module list produces an empty grouping",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> Strings.cat2 "left(" (Strings.cat2 (ShowErrors.error e) ")")) (\groups -> Strings.cat2 "right(" (Strings.cat2 (ShowCore.list (\g -> Strings.cat2 (Strings.cat2 (Pairs.first g) ": ") (ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) (Lists.sortOn (\m -> Packaging.unModuleName (Packaging.moduleName m)) (Pairs.second g)))) groups) ")")) (Routing.groupByPackageIn (Routing.buildRoutingMap [
                  (
                    "hydra-kernel",
                    [
                      Packaging.ModuleName "hydra.strip",
                      (Packaging.ModuleName "hydra.sorting")]),
                  (
                    "hydra-ext",
                    [
                      Packaging.ModuleName "hydra.avro.schema",
                      (Packaging.ModuleName "hydra.dsl.avro.schema")])]) [])),
                Testing.universalTestCaseExpected = (\_ -> "right([])")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "modules are partitioned and sorted by owning package",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> Strings.cat2 "left(" (Strings.cat2 (ShowErrors.error e) ")")) (\groups -> Strings.cat2 "right(" (Strings.cat2 (ShowCore.list (\g -> Strings.cat2 (Strings.cat2 (Pairs.first g) ": ") (ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) (Lists.sortOn (\m -> Packaging.unModuleName (Packaging.moduleName m)) (Pairs.second g)))) groups) ")")) (Routing.groupByPackageIn (Routing.buildRoutingMap [
                  (
                    "hydra-kernel",
                    [
                      Packaging.ModuleName "hydra.strip",
                      (Packaging.ModuleName "hydra.sorting")]),
                  (
                    "hydra-ext",
                    [
                      Packaging.ModuleName "hydra.avro.schema",
                      (Packaging.ModuleName "hydra.dsl.avro.schema")])]) [
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "hydra.sorting"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = []},
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "hydra.avro.schema"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = []},
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "hydra.strip"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = []}])),
                Testing.universalTestCaseExpected = (\_ -> "right([hydra-ext: [hydra.avro.schema], hydra-kernel: [hydra.sorting, hydra.strip]])")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "an unrouted module in the list fails the whole grouping loudly",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> Strings.cat2 "left(" (Strings.cat2 (ShowErrors.error e) ")")) (\groups -> Strings.cat2 "right(" (Strings.cat2 (ShowCore.list (\g -> Strings.cat2 (Strings.cat2 (Pairs.first g) ": ") (ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) (Lists.sortOn (\m -> Packaging.unModuleName (Packaging.moduleName m)) (Pairs.second g)))) groups) ")")) (Routing.groupByPackageIn (Routing.buildRoutingMap [
                  (
                    "hydra-kernel",
                    [
                      Packaging.ModuleName "hydra.strip",
                      (Packaging.ModuleName "hydra.sorting")]),
                  (
                    "hydra-ext",
                    [
                      Packaging.ModuleName "hydra.avro.schema",
                      (Packaging.ModuleName "hydra.dsl.avro.schema")])]) [
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "hydra.strip"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = []},
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "hydra.nonexistent.module"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = []}])),
                Testing.universalTestCaseExpected = (\_ -> "left(unrouted module: hydra.nonexistent.module is not declared in any package's manifest (RoutingMap). Add it to the owning package's Manifest.mainModules.)")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
