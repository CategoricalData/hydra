-- | Module-to-package routing for the Hydra packaging split.
--
-- A single source of truth for "which package owns which namespace" and
-- "which directory under dist/json/ does that package write to". Used by
-- both the JSON writers (update-json-*) and the JSON reader
-- (bootstrap-from-json), so the two sides can never disagree about where
-- a module's JSON file should live.
--
-- Today the mapping is a hardcoded prefix table. Eventually it should be
-- derived from each package's package.json (or Manifest.hs); see
-- feature_290_packaging-plan.md for the longer arc.
--
-- Routing contract:
--
--   namespaceToPackage :: Namespace -> String
--
--     Returns the package name (e.g. "hydra-kernel", "hydra-ext") that owns
--     a given Hydra namespace. Falls back to "hydra-kernel" for any
--     namespace that does not match an explicit prefix.
--
--   namespaceToPackageJsonDir :: FilePath -> Namespace -> FilePath
--
--     Given the dist-json root (e.g. "../../dist/json") and a namespace,
--     returns the absolute directory under which that module's JSON file
--     should be written: "<root>/<package>/src/main/json".
--
--   groupByPackage :: [Module] -> [(String, [Module])]
--
--     Partitions a list of modules by owning package, sorted by package
--     name for deterministic output ordering.

module Hydra.PackageRouting (
  namespaceToPackage,
  namespaceToPackageJsonDir,
  namespaceToPackageTestJsonDir,
  groupByPackage,
  packagePrefixes,
  defaultDistJsonRoot,
) where

import Hydra.Kernel

import Data.Function (on)
import Data.List (groupBy, isPrefixOf, sortOn)
import qualified System.FilePath as FP


-- | Default root under which per-package JSON trees live, relative to the
-- haskell head working directory (heads/haskell/). Callers can override
-- this when writing elsewhere (e.g. bootstrap-from-json's --ext-json-dir).
defaultDistJsonRoot :: FilePath
defaultDistJsonRoot = "../../dist/json"

-- | Map a module namespace to the package that owns it.
--
-- The ordering of 'packagePrefixes' matters: more specific prefixes must
-- come before less specific ones. The fallback "hydra-kernel" covers all
-- namespaces that don't match any explicit prefix.
namespaceToPackage :: Namespace -> String
namespaceToPackage (Namespace ns) = go packagePrefixes
  where
    go []                    = "hydra-kernel"
    go ((prefix, pkg) : rest)
      | prefix `isPrefixOf` ns = pkg
      | otherwise              = go rest

-- | Given a dist-json root and a namespace, compute the directory under
-- which that module's JSON file should be written.
namespaceToPackageJsonDir :: FilePath -> Namespace -> FilePath
namespaceToPackageJsonDir root ns =
  root FP.</> namespaceToPackage ns FP.</> "src" FP.</> "main" FP.</> "json"

-- | Like 'namespaceToPackageJsonDir' but for test JSON output.
namespaceToPackageTestJsonDir :: FilePath -> Namespace -> FilePath
namespaceToPackageTestJsonDir root ns =
  root FP.</> namespaceToPackage ns FP.</> "src" FP.</> "test" FP.</> "json"

-- | Partition a list of modules by owning package, returning a list of
--   (packageName, modules) groups. The groups are sorted by package name
--   for deterministic output ordering.
groupByPackage :: [Module] -> [(String, [Module])]
groupByPackage mods =
    fmap collapse
      $ groupBy ((==) `on` fst)
      $ sortOn fst
      $ fmap (\m -> (namespaceToPackage (moduleNamespace m), m)) mods
  where
    collapse [] = ("", [])  -- unreachable; groupBy never returns empty inner lists
    collapse grp@((pkg, _) : _) = (pkg, fmap snd grp)

-- | Prefix-to-package table. Order matters: more-specific prefixes first.
-- Any namespace not matching any prefix falls through to "hydra-kernel".
packagePrefixes :: [(String, String)]
packagePrefixes =
  [ -- Coder packages (main runtime modules)
    ("hydra.haskell.",              "hydra-haskell")
  , ("hydra.java.",                 "hydra-java")
  , ("hydra.python.",               "hydra-python")
  , ("hydra.scala.",                "hydra-scala")
  , ("hydra.lisp.",                 "hydra-lisp")
  , ("hydra.coq.",                  "hydra-coq")
  , ("hydra.javaScript.",           "hydra-javascript")
    -- DSL wrapper modules for coder packages
  , ("hydra.dsl.haskell.",          "hydra-haskell")
  , ("hydra.dsl.java.",             "hydra-java")
  , ("hydra.dsl.python.",           "hydra-python")
  , ("hydra.dsl.scala.",            "hydra-scala")
  , ("hydra.dsl.lisp.",             "hydra-lisp")
  , ("hydra.dsl.coq.",              "hydra-coq")
  , ("hydra.dsl.javaScript.",       "hydra-javascript")
    -- Synthesized decoder source modules for coder packages
  , ("hydra.sources.decode.haskell.",    "hydra-haskell")
  , ("hydra.sources.decode.java.",       "hydra-java")
  , ("hydra.sources.decode.python.",     "hydra-python")
  , ("hydra.sources.decode.scala.",      "hydra-scala")
  , ("hydra.sources.decode.lisp.",       "hydra-lisp")
  , ("hydra.sources.decode.coq.",        "hydra-coq")
  , ("hydra.sources.decode.javaScript.", "hydra-javascript")
    -- Synthesized encoder source modules for coder packages
  , ("hydra.sources.encode.haskell.",    "hydra-haskell")
  , ("hydra.sources.encode.java.",       "hydra-java")
  , ("hydra.sources.encode.python.",     "hydra-python")
  , ("hydra.sources.encode.scala.",      "hydra-scala")
  , ("hydra.sources.encode.lisp.",       "hydra-lisp")
  , ("hydra.sources.encode.coq.",        "hydra-coq")
  , ("hydra.sources.encode.javaScript.", "hydra-javascript")
    -- Property graph package
  , ("hydra.pg.",                   "hydra-pg")
  , ("hydra.cypher.",               "hydra-pg")
  , ("hydra.graphviz.",             "hydra-pg")
  , ("hydra.tinkerpop.",            "hydra-pg")
  , ("hydra.error.pg",              "hydra-pg")
  , ("hydra.validate.pg",           "hydra-pg")
  , ("hydra.decode.pg.",            "hydra-pg")
  , ("hydra.encode.pg.",            "hydra-pg")
  , ("hydra.sources.decode.pg.",    "hydra-pg")
  , ("hydra.sources.encode.pg.",    "hydra-pg")
  , ("hydra.demos.genpg.",          "hydra-pg")
  , ("openGql.grammar",             "hydra-pg")
  , ("com.gdblab.pathAlgebra.",     "hydra-pg")
  , ("hydra.dsl.pg.",               "hydra-pg")
  , ("hydra.dsl.cypher.",           "hydra-pg")
  , ("hydra.dsl.graphviz.",         "hydra-pg")
  , ("hydra.dsl.tinkerpop.",        "hydra-pg")
  , ("hydra.dsl.error.pg",          "hydra-pg")
  , ("hydra.dsl.openGql.",          "hydra-pg")
  , ("hydra.dsl.com.gdblab.pathAlgebra.", "hydra-pg")
    -- RDF / OWL / SHACL / ShEx / XML schema package
  , ("hydra.rdf.",                  "hydra-rdf")
  , ("hydra.owl.",                  "hydra-rdf")
  , ("hydra.shacl.",                "hydra-rdf")
  , ("hydra.shex.",                 "hydra-rdf")
  , ("hydra.xml.schema",            "hydra-rdf")
  , ("hydra.dsl.rdf.",              "hydra-rdf")
  , ("hydra.dsl.owl.",              "hydra-rdf")
  , ("hydra.dsl.shacl.",            "hydra-rdf")
  , ("hydra.dsl.shex.",             "hydra-rdf")
  , ("hydra.dsl.xml.schema",        "hydra-rdf")
    -- WebAssembly package
  , ("hydra.wasm.",                 "hydra-wasm")
  , ("hydra.dsl.wasm.",             "hydra-wasm")
    -- Extension package (truly-ext coders: Avro, Protobuf, GraphQL, etc.)
  , ("hydra.atlas",                 "hydra-ext")
  , ("hydra.avro.",                 "hydra-ext")
  , ("hydra.azure.",                "hydra-ext")
  , ("hydra.cpp.",                  "hydra-ext")
  , ("hydra.csharp.",               "hydra-ext")
  , ("hydra.datalog.",              "hydra-ext")
  , ("hydra.delta.",                "hydra-ext")
  , ("hydra.geojson.",              "hydra-ext")
  , ("hydra.go.",                   "hydra-ext")
  , ("hydra.graphql.",              "hydra-ext")
  , ("hydra.iana.",                 "hydra-ext")
  , ("hydra.json.schema",           "hydra-ext")
  , ("hydra.kusto.",                "hydra-ext")
  , ("hydra.osv.",                  "hydra-ext")
  , ("hydra.parquet.",              "hydra-ext")
  , ("hydra.pegasus.",              "hydra-ext")
  , ("hydra.protobuf.",             "hydra-ext")
  , ("hydra.rust.",                 "hydra-ext")
  , ("hydra.sql.",                  "hydra-ext")
  , ("hydra.stac.",                 "hydra-ext")
  , ("hydra.typeScript.",           "hydra-ext")
  , ("hydra.workflow",              "hydra-ext")
  , ("hydra.dsl.atlas",             "hydra-ext")
  , ("hydra.dsl.avro.",             "hydra-ext")
  , ("hydra.dsl.azure.",            "hydra-ext")
  , ("hydra.dsl.cpp.",              "hydra-ext")
  , ("hydra.dsl.csharp.",           "hydra-ext")
  , ("hydra.dsl.datalog.",          "hydra-ext")
  , ("hydra.dsl.delta.",            "hydra-ext")
  , ("hydra.dsl.geojson.",          "hydra-ext")
  , ("hydra.dsl.go.",               "hydra-ext")
  , ("hydra.dsl.graphql.",          "hydra-ext")
  , ("hydra.dsl.iana.",             "hydra-ext")
  , ("hydra.dsl.json.schema",       "hydra-ext")
  , ("hydra.dsl.kusto.",            "hydra-ext")
  , ("hydra.dsl.osv.",              "hydra-ext")
  , ("hydra.dsl.parquet.",          "hydra-ext")
  , ("hydra.dsl.pegasus.",          "hydra-ext")
  , ("hydra.dsl.protobuf.",         "hydra-ext")
  , ("hydra.dsl.rust.",             "hydra-ext")
  , ("hydra.dsl.sql.",              "hydra-ext")
  , ("hydra.dsl.stac.",             "hydra-ext")
  , ("hydra.dsl.typeScript.",       "hydra-ext")
  , ("hydra.dsl.workflow",          "hydra-ext")
    -- hydra.yaml.model lives in hydra-kernel, so we route the hydra-ext yaml
    -- modules (coder, language, serde) explicitly rather than with a blanket
    -- hydra.yaml. prefix.
  , ("hydra.yaml.coder",            "hydra-ext")
  , ("hydra.yaml.language",         "hydra-ext")
  , ("hydra.yaml.serde",            "hydra-ext")
  ]
