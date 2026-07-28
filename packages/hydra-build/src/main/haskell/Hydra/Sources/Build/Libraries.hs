
module Hydra.Sources.Build.Libraries (
  module_,
  ns,
  expectedLibraries,
  expectedLibraryNames,
  isExpectedLibrary,
) where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Lib.Lists    as Lists
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


-- | The translingual expected-libraries registry (#533, #416 phase 2): the single
-- source of truth for which @hydra.lib.\<sub>@ module names every self-hosting host
-- must register, load, and test at startup.
--
-- Before this promotion, that set was duplicated by hand in at least a dozen
-- locations across nine hosts (Haskell driver allow-lists, Java's overlay alias map
-- and @Libraries.java@ dispatch, Python's registration functions, Scala's and
-- TypeScript's analogs, and — worst — four near-independent lists apiece for Common
-- Lisp and Emacs Lisp). #524 (adding @hydra.lib.hashing@) discovered most of those
-- locations only empirically, one host's test failure at a time; #533's inventory
-- found two of Common Lisp's four lists had already silently drifted (fixed
-- alongside this module, as its own commit).
--
-- 'expectedLibraries' is hand-authored, not derived from an existing Haskell-side
-- list: neither @Hydra.Sources.Kernel.Terms.All.kernelTermsModules@ (mixes lib
-- modules with ~40 unrelated kernel modules) nor its @kernelLibModules@ (a
-- deliberate 16-entry subset scoped to DSL-wrapper generation, excluding the five
-- effectful subs) is "every @hydra.lib.*@ module" — aliasing either would silently
-- ship a wrong registry. This module consolidates the true set into ONE place
-- instead of the previous dozen-plus; @Hydra.BuildLibrariesSpec@ (heads/haskell)
-- guards against the single source drifting from
-- @packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Lib/*.hs@, the
-- actual ground truth (one file per registered @hydra.lib.\<sub>@ module).
--
-- @hydra.lib.defaults@ is deliberately excluded: it holds constant term values with
-- no native per-host overlay implementation and no per-host registration
-- requirement (confirmed by #568's 'Hydra.Generation.overlayLibSubs', which excludes
-- it from overlay-directory discovery for the same reason).
--
-- This landing is types + data only (the #512/#532 promotion pattern): no host
-- driver or loader is repointed to consume 'expectedLibraries' yet, since the
-- published host does not carry this module. Host-side startup validation against
-- the registry is deferred follow-up work.
ns :: ModuleName
ns = ModuleName "hydra.build.libraries"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just
              "The translingual expected-libraries registry: every hydra.lib.<sub> module name every self-hosting host must register, load, and test. See https://github.com/CategoricalData/hydra/issues/533")}
  where
   definitions = [
     toDefinition expectedLibraries,
     toDefinition isExpectedLibrary]

-- | The plain Haskell list backing 'expectedLibraries', in alphabetical order
-- (matching the kernel coding-style convention for definition lists). Exposed
-- separately so that @Hydra.BuildLibrariesSpec@ (heads/haskell) can compare it
-- directly against the actual kernel @Lib/*.hs@ file set without deconstructing
-- the generated 'Term'.
expectedLibraryNames :: [String]
expectedLibraryNames = [
  "chars",
  "effects",
  "eithers",
  "equality",
  "files",
  "functions",
  "hashing",
  "lists",
  "literals",
  "logic",
  "maps",
  "math",
  "optionals",
  "ordering",
  "pairs",
  "regex",
  "sets",
  "strings",
  "system",
  "text"]

-- | Every @hydra.lib.\<sub>@ module name every self-hosting host is expected to
-- register, load, and test at startup.
expectedLibraries :: TypedTermDefinition [String]
expectedLibraries = define "expectedLibraries" $
  doc "Every hydra.lib.<sub> module name every self-hosting host must register" $
  list (string <$> expectedLibraryNames)

-- | Whether a bare sub-namespace name (e.g. "chars", not "hydra.lib.chars") is a
-- member of 'expectedLibraries'.
isExpectedLibrary :: TypedTermDefinition (String -> Bool)
isExpectedLibrary = define "isExpectedLibrary" $
  doc "Whether a bare sub-namespace name is a member of expectedLibraries" $
  "sub" ~> Lists.member (var "sub") (asTerm expectedLibraries)
