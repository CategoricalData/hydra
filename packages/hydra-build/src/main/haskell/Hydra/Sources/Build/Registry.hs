module Hydra.Sources.Build.Registry where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Strings  as Strings
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.List                   as L

import qualified Hydra.Sources.Kernel.Terms.Strip as Strip
import qualified Hydra.Sources.Build.Format as Format


-- | The translingual language registry (#416 / registry): the single source of
-- per-language build identity, so the sync/assemble/test drivers stop hardcoding
-- language-name literals in their LOGIC (e.g. @"hydra-"++lang@, the @lisp@ alias,
-- @haskell@ as the root coder host). The set of languages and their properties
-- is DATA (a list of profile tuples), not code: adding a language is a data edit,
-- never a branch in a neutral function (the "add Rust by editing only data" test).
--
-- Shape follows the established package-local pattern: Option-3 tuples rather than
-- constructing a package-local typed record value, avoiding the
-- generator-imports-generated concern the tuple pattern avoids (as in
-- @hydra.build.assemblyplan@ / @hydra.build.comparereportlogic@). Each profile is
-- a @(name, coderPackage, family)@ triple; the root-coder-host fact is a separate
-- scalar constant (@rootCoderHost@). Accessor helpers (@coderPackageFor@,
-- @familyFor@, @isLispDialect@, @allLanguageNames@, @lispDialectNames@) let neutral
-- logic read the data without knowing any specific name.
--
-- Consumers (first tranche): @hydra.build.syncmatrix@ (packageForLanguage,
-- rootCoderCells), @hydra.build.langexpansion@ (allLanguages, lispDialects,
-- expandLangAlias), and the four sync/test/bench shell drivers that re-hardcode
-- the language enumeration. Everything here is pure: no primitives, no I/O.
ns :: ModuleName
ns = ModuleName "hydra.build.registry"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            -- #559 Cluster 2: depends on hydra.build.format for the LanguageProfile record type
            -- (the profiles below are LanguageProfile record values, replacing the former tuples).
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Strip.ns, Format.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "The translingual language registry: per-language build identity (name, coder package, family) as LanguageProfile records, so the build drivers read language properties instead of hardcoding name literals. See https://github.com/CategoricalData/hydra/issues/416 and https://github.com/CategoricalData/hydra/issues/559")}
  where
   definitions = [
     toDefinition allLanguageNames,
     toDefinition benchDefaultNames,
     toDefinition benchHostNames,
     toDefinition coderPackageFor,
     toDefinition familyFor,
     toDefinition inferenceBenchHostNames,
     toDefinition isLispDialect,
     toDefinition languageProfiles,
     toDefinition lispDialectNames,
     toDefinition rootCoderHost,
     toDefinition testMatrixNames]

-- | The registry as data: one @(name, coderPackage, family)@ triple per language.
-- @name@ is the canonical language token (the former ad-hoc bash identifiers);
-- @coderPackage@ is the distribution package the language's coder lives in (note
-- the four Lisp dialects share @hydra-lisp@ — the collapse that @"hydra-"++name@
-- got WRONG); @family@ groups languages that share build treatment (@jvm@,
-- @python@, @lisp@, @haskell@, @typescript@, @go@). This is the ONLY place a
-- language name is written down.
--
-- ORDER IS SIGNIFICANT and matches the native @ALL_LANGS@ order in @bin/sync.sh@
-- (haskell, java, python, scala first — higher-value hosts assembled first — then
-- go, typescript, then the Lisp dialects). @allLanguageNames@ preserves this order,
-- so @expandLangAlias "all"@ and the sync matrix keep the native ordering that the
-- driver's @LANG_UNION@ deliberately does not sort. Do NOT alphabetize this list.
-- Raw Haskell source-of-truth lists (#416 piece 3 step 2). These plain
-- @[String]@/tuple bindings are the single place the language data is written;
-- the @TypedTermDefinition@ defs below WRAP them (mirroring
-- @Hydra.Sources.Build.Libraries.expectedLibraryNames@). Keeping the raw lists
-- separate lets the native @languages.json@ emitter import them directly as
-- Haskell values (no term reduction), while the DSL defs generate the same lists
-- into every host. Byte-parity: the emitted registry.json is unchanged by this
-- factoring — only the Haskell source is refactored to a shared source.

-- | Every language name in native @ALL_LANGS@ order (raw @[String]@).
--
-- #559 Cluster 2 (slice 1): the former @languageProfileData@ @(name, coderPackage,
-- family)@ TUPLES have been DELETED — the rich per-language identity (coderPackage,
-- family, and the slices-2-4 fields) now lives ONCE, as the @languageProfiles@
-- LanguageProfile RECORDS below. This raw name-list remains only as the name-set the
-- native @languages.json@ emitter (@ManifestGeneration.writeLanguagesJson@) consumes
-- alongside the other per-script scope lists (@lispDialectNameList@ etc.), in
-- byte-identical native order. Fully single-sourcing these names FROM the generated
-- registry (so the emitter reads no raw Haskell at all) is Q1a — a follow-up within
-- this tranche, since it touches the emitter + every @languages.json@ consumer;
-- slice 1 is a byte-parity refactor and keeps the name-list where the emitter reads it.
allLanguageNameList :: [String]
allLanguageNameList = [
  "haskell", "java", "python", "scala", "go", "typescript",
  "clojure", "scheme", "common-lisp", "emacs-lisp"]

-- | The Lisp-dialect names in native @lisp@-alias order (raw).
lispDialectNameList :: [String]
lispDialectNameList = ["clojure", "common-lisp", "emacs-lisp", "scheme"]

-- | Raw per-script scope lists (the languages each bash driver hardcodes). The
-- scope @TypedTermDefinition@s below and the @languages.json@ emitter both consume
-- these, so the values are written once. Order matches each script's native
-- constant exactly (see the per-def docs).
testMatrixNameList :: [String]
testMatrixNameList = [
  "haskell", "java", "python", "scala", "typescript",
  "clojure", "scheme", "common-lisp", "emacs-lisp"]

benchHostNameList :: [String]
benchHostNameList = [
  "haskell", "java", "python",
  "clojure", "common-lisp", "emacs-lisp", "scheme"]

inferenceBenchHostNameList :: [String]
inferenceBenchHostNameList = [
  "haskell", "java", "python", "python-pypy", "common-lisp", "emacs-lisp"]

benchDefaultNameList :: [String]
benchDefaultNameList = ["haskell", "java", "python"]

-- | The fully-qualified name of the LanguageProfile record type, written as a Name
-- LITERAL (not the generated @_LanguageProfile@ constant) so this authoring source
-- constructs the record WITHOUT importing generated code — 'Phantoms.record'/'project'
-- take @AsName@, so a Name literal suffices (#559 Cluster 2; the
-- generator-imports-generated concern is thereby avoided while still emitting a typed
-- record term).
languageProfileName :: Name
languageProfileName = Name "hydra.build.format.LanguageProfile"

-- | Construct one LanguageProfile record value from its three fields (slice-1 shape).
-- All three fields are PLAIN strings (see hydra.build.format.LanguageProfile) — a
-- byte-parity refactor of the former (name, coderPackage, family) tuple. Field names
-- are raw strings ('(>:)' takes String). The Haskell-side phantom is the untyped
-- 'Term' (the record's DSL type is LanguageProfile, enforced at the term/JSON level).
languageProfile :: String -> String -> String -> TypedTerm Term
languageProfile n cp fam = Phantoms.record languageProfileName [
    "name"         Phantoms.>: string n,
    "coderPackage" Phantoms.>: string cp,
    "family"       Phantoms.>: string fam]

-- | Per-language build identity as LanguageProfile RECORDS (#559 Cluster 2, replacing
-- the former (name, coderPackage, family) tuples), in native ALL_LANGS order: the
-- single source of language build identity. The emitted TERM is a list of records;
-- the 'Term' phantom is the untyped escape hatch (see 'languageProfile').
languageProfiles :: TypedTermDefinition [Term]
languageProfiles = define "languageProfiles" $
  doc "Per-language build identity as LanguageProfile records, in native ALL_LANGS order: the single source of language names" $
  list [
    languageProfile "haskell"     "hydra-haskell"    "haskell",
    languageProfile "java"        "hydra-java"       "jvm",
    languageProfile "python"      "hydra-python"     "python",
    languageProfile "scala"       "hydra-scala"      "jvm",
    languageProfile "go"          "hydra-go"         "go",
    languageProfile "typescript"  "hydra-typescript" "typescript",
    languageProfile "clojure"     "hydra-lisp"       "lisp",
    languageProfile "scheme"      "hydra-lisp"       "lisp",
    languageProfile "common-lisp" "hydra-lisp"       "lisp",
    languageProfile "emacs-lisp"  "hydra-lisp"       "lisp"]

-- | Every language name in the registry, in registry order (the former @ALL_LANGS@
-- bash constant): @map first languageProfiles@. Neutral logic uses this instead of
-- a hardcoded list.
allLanguageNames :: TypedTermDefinition [String]
allLanguageNames = define "allLanguageNames" $
  doc "Every language name in the registry (the former ALL_LANGS constant)" $
  Lists.map ("p" ~> profileName @@ var "p") (asTerm languageProfiles)

-- | Project the @name@ field of a LanguageProfile record (a plain string). Field
-- name as a Name LITERAL (project's field arg is @AsName@, which String is not — but
-- Name is), so still no generated import.
profileName :: TypedTerm (a -> String)
profileName = Phantoms.project languageProfileName (Name "name")

-- | Project the @coderPackage@ field of a LanguageProfile record (a plain string).
profileCoderPackage :: TypedTerm (a -> String)
profileCoderPackage = Phantoms.project languageProfileName (Name "coderPackage")

-- | Project the @family@ field of a LanguageProfile record (a plain string).
profileFamily :: TypedTerm (a -> String)
profileFamily = Phantoms.project languageProfileName (Name "family")

-- | The four Lisp-dialect names in the order the native @lisp@ alias expands them
-- (@clojure common-lisp emacs-lisp scheme@ — see @bin/sync.sh@ expand_langs, which
-- differs from the Lisp tail order in @allLanguageNames@/@ALL_LANGS@). All four have
-- family @lisp@ in 'languageProfiles'; this constant pins the alias-expansion order.
lispDialectNames :: TypedTermDefinition [String]
lispDialectNames = define "lispDialectNames" $
  doc "The Lisp-dialect names in native 'lisp'-alias order (clojure, common-lisp, emacs-lisp, scheme)" $
  list (string <$> ["clojure", "common-lisp", "emacs-lisp", "scheme"])

-- Per-script scope lists (#416 piece 3): each build script tests/benchmarks a
-- DIFFERENT subset of languages, in its own order — these are NOT one shared @all@.
-- Each constant is the evaluated language set the corresponding bash driver
-- currently hardcodes; encoding them here retires those hardcoded lists (the scope
-- is data, the per-script difference a legitimate property, not a neutral-logic
-- branch). Explicit ordered lists (not a boolean-per-language) because two of the
-- scopes carry order and one (inference) includes the @python-pypy@ pseudo-host,
-- which a plain family/name boolean cannot express. Order matches each script's
-- native constant exactly, for byte-parity.

-- | The languages the kernel test suite runs as targets (@bin/test.sh@ @ALL_TARGETS@):
-- every language except @go@ (the head bud does not host the suite).
testMatrixNames :: TypedTermDefinition [String]
testMatrixNames = define "testMatrixNames" $
  doc "Languages the kernel testSuite runs as targets (test.sh ALL_TARGETS; all except go)" $
  list (string <$> testMatrixNameList)

-- | The hosts with a kernel benchmark suite (@bin/run-benchmark-tests.sh@ @ALL_HOSTS@):
-- the three self-hosting non-JVM-plus-JVM hosts plus the four Lisp dialects (no
-- scala/go/typescript bench suite).
benchHostNames :: TypedTermDefinition [String]
benchHostNames = define "benchHostNames" $
  doc "Hosts with a kernel benchmark suite (run-benchmark-tests.sh ALL_HOSTS)" $
  list (string <$> benchHostNameList)

-- | The hosts with an inference benchmark (@bin/run-inference-bench.sh@ @ALL_HOSTS@).
-- Includes the @python-pypy@ pseudo-host (a distinct bench runner that maps back to
-- python's bench package); excludes scala/go/typescript/clojure/scheme.
inferenceBenchHostNames :: TypedTermDefinition [String]
inferenceBenchHostNames = define "inferenceBenchHostNames" $
  doc "Hosts with an inference benchmark (run-inference-bench.sh ALL_HOSTS; includes python-pypy pseudo-host)" $
  list (string <$> inferenceBenchHostNameList)

-- | The default bench scope (@bin/sync-bench.sh@ @DEFAULT_HOSTS@): the bootstrapping
-- triad only (lisp excluded — the coders are too slow for the default bench sync).
benchDefaultNames :: TypedTermDefinition [String]
benchDefaultNames = define "benchDefaultNames" $
  doc "Default bench scope (sync-bench.sh DEFAULT_HOSTS; the haskell/java/python triad)" $
  list (string <$> benchDefaultNameList)

-- | The distribution package a language's coder lives in: the @coderPackage@ field
-- of its profile, or @hydra-<name>@ as a fallback for an unknown name (matching the
-- prior bash default). This is the data-driven replacement for the buried
-- @"hydra-"++lang@ rule — correct for the Lisp dialects (all @hydra-lisp@), which
-- that rule got wrong.
coderPackageFor :: TypedTermDefinition (String -> String)
coderPackageFor = define "coderPackageFor" $
  doc "The distribution package for a language's coder (data-driven; hydra-<name> fallback)" $
  "name" ~>
    Optionals.match
      (Lists.head
        (Lists.filter ("p" ~> Equality.equal (profileName @@ var "p") (var "name"))
          (asTerm languageProfiles)))
      (Strings.concat2 (string "hydra-") (var "name"))
      ("p" ~> profileCoderPackage @@ var "p")

-- | The family of a language: the @family@ field of its profile, or the empty
-- string for an unknown name.
familyFor :: TypedTermDefinition (String -> String)
familyFor = define "familyFor" $
  doc "The build family of a language (jvm/python/lisp/haskell/typescript/go; empty for unknown)" $
  "name" ~>
    Optionals.match
      (Lists.head
        (Lists.filter ("p" ~> Equality.equal (profileName @@ var "p") (var "name"))
          (asTerm languageProfiles)))
      (string "")
      ("p" ~> profileFamily @@ var "p")

-- | Whether a language is a Lisp dialect (family @lisp@): the data-driven form of
-- the @clojure|scheme|common-lisp|emacs-lisp@ case arm that recurs across the
-- drivers.
isLispDialect :: TypedTermDefinition (String -> Bool)
isLispDialect = define "isLispDialect" $
  doc "Whether a language's family is 'lisp' (data-driven Lisp-dialect membership)" $
  "name" ~> Equality.equal (familyFor @@ var "name") (string "lisp")

-- | The single host that drives root-coder generation (every language's coder is
-- generated in Haskell). Was hardcoded as the literal @"haskell"@ in
-- @syncmatrix.rootCoderCells@; now a named datum.
rootCoderHost :: TypedTermDefinition String
rootCoderHost = define "rootCoderHost" $
  doc "The single host that drives root-coder generation (the former hardcoded 'haskell')" $
  string "haskell"
