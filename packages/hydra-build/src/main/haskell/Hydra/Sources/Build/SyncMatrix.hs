module Hydra.Sources.Build.SyncMatrix where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Equality as Equality
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.List                   as L

import qualified Hydra.Sources.Kernel.Terms.Strip as Strip
import qualified Hydra.Sources.Build.LangExpansion as LangExpansion
import qualified Hydra.Sources.Build.Registry as Registry


-- | The pure sync-matrix planning core (#416 P1 / syncmatrix): the (package,
-- target) cell set that @bin/sync.sh@ must regenerate for a given @--hosts@ /
-- @--targets@ request. A DSL-level, translingual home for the matrix derivation
-- that @bin/sync.sh@ hand-computes inline (the three rules documented in its
-- header), so the sync driver — and any downstream consumer (bootstrap demo,
-- per-host build orchestration) — can share one definition instead of
-- re-deriving it. Everything here is pure list/pair logic: no primitives, no I/O.
-- The effectful orchestration (spawning stack/gradle/sbt/uv per cell) stays
-- host-native — this module promotes only the planning core (Phase-3 re-scope).
--
-- Consumes 'Hydra.Sources.Build.LangExpansion.langUnion' (the deduped
-- hosts ∪ targets order) — LangExpansion's doc names this module as its
-- consumer. An entry is a @pair packageName targetLanguage@.
--
-- The three rules (verbatim from bin/sync.sh's header):
--   1. @(hydra-kernel, L)@              for every L in (hosts ∪ targets)
--   2. @(hydra-L, haskell)@             for every L in (hosts ∪ targets)
--                                       (the Haskell head drives every generation)
--   3. @(hydra-target, host)@           for every (host, target) pair, host ≠ haskell
--                                       (so a host can emit a target's code)
--
-- Excluded packages (hydra-coq/wasm/ext/pg/rdf) are extensions, not
-- bootstrapping dependencies, and are never in the matrix; generated on demand
-- via bin/sync-packages.sh. They are simply never produced by the rules above,
-- so no explicit exclusion is needed.
ns :: ModuleName
ns = ModuleName "hydra.build.syncmatrix"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$>
              ([ModuleName "hydra.build.langexpansion", ModuleName "hydra.build.registry", Strip.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Pure sync-matrix (package, target) planning core shared by the sync driver")}
  where
   definitions = [
     toDefinition crossHostCells,
     toDefinition kernelCells,
     toDefinition packageForLanguage,
     toDefinition rootCoderCells,
     toDefinition syncMatrix]

-- | Rule 3: @(hydra-target, host)@ for every (host, target) pair where the host
-- is not Haskell — the cross-host coders (each host emits each target's coder in
-- the host's own language). Haskell is excluded as a host because rule 2 already
-- generates every coder in Haskell.
crossHostCells :: TypedTermDefinition ([String] -> [String] -> [(String, String)])
crossHostCells = define "crossHostCells" $
  doc "Rule 3: (hydra-target, host) for every (host, target) with host /= haskell" $
  "hosts" ~> "targets" ~>
    Lists.concat
      (Lists.map
        ("host" ~>
          Lists.map
            ("target" ~> pair (packageForLanguage @@ var "target") (var "host"))
            (var "targets"))
        (Lists.filter
          ("host" ~> Logic.not (Equality.equal (var "host") (string "haskell")))
          (var "hosts")))

-- | Rule 1: @(hydra-kernel, L)@ for every language L in the union — the kernel
-- generated for each participating language.
kernelCells :: TypedTermDefinition ([String] -> [(String, String)])
kernelCells = define "kernelCells" $
  doc "Rule 1: (hydra-kernel, L) for every L in (hosts union targets)" $
  "langUnion" ~>
    Lists.map
      ("lang" ~> pair (string "hydra-kernel") (var "lang"))
      (var "langUnion")

-- | The distribution package name for a language: @hydra-<language>@.
packageForLanguage :: TypedTermDefinition (String -> String)
packageForLanguage = define "packageForLanguage" $
  doc "The distribution package for a language's coder, read from the registry (data-driven; corrects the former hydra-<lang> rule for the Lisp dialects, which all share hydra-lisp)" $
  "lang" ~> Registry.coderPackageFor @@ var "lang"

-- | Rule 2: @(hydra-L, haskell)@ for every language L in the union — each
-- language's coder generated in Haskell (the Haskell head drives every
-- downstream generation).
rootCoderCells :: TypedTermDefinition ([String] -> [(String, String)])
rootCoderCells = define "rootCoderCells" $
  doc "Rule 2: (hydra-L, <rootCoderHost>) for every L in (hosts union targets) — the root coder host is read from the registry, not hardcoded" $
  "langUnion" ~>
    Lists.map
      ("lang" ~> pair (packageForLanguage @@ var "lang") (asTerm Registry.rootCoderHost))
      (var "langUnion")

-- | The full sync matrix for a @(hosts, targets)@ request: the deduped union of
-- the three rules' cells. Hosts and targets are alias-expanded once
-- (@all@/@lisp@ resolved); rules 1+2 range over their deduped union, rule 3 over
-- the (expanded host, expanded target) product. The result is the complete
-- @(package, target)@ set @bin/sync.sh@ must regenerate.
syncMatrix :: TypedTermDefinition ([String] -> [String] -> [(String, String)])
syncMatrix = define "syncMatrix" $
  doc "The full (package, target) sync matrix for a (hosts, targets) request" $
  "hosts" ~> "targets" ~>
    "expandedHosts" <~ LangExpansion.expandLangs @@ var "hosts" $
    "expandedTargets" <~ LangExpansion.expandLangs @@ var "targets" $
    "union" <~ LangExpansion.langUnion @@ var "hosts" @@ var "targets" $
    Lists.distinct
      (Lists.concat (list [
        kernelCells @@ var "union",
        rootCoderCells @@ var "union",
        crossHostCells @@ var "expandedHosts" @@ var "expandedTargets"]))
