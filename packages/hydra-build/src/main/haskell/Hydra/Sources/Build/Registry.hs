module Hydra.Sources.Build.Registry where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Strings  as Strings
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.List                   as L

import qualified Hydra.Sources.Kernel.Terms.Strip as Strip


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
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Strip.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "The translingual language registry: per-language build identity (name, coder package, family) as data, so the build drivers read language properties instead of hardcoding name literals. See https://github.com/CategoricalData/hydra/issues/416")}
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
languageProfiles :: TypedTermDefinition [(String, String, String)]
languageProfiles = define "languageProfiles" $
  doc "Per-language build identity as (name, coderPackage, family) triples, in native ALL_LANGS order: the single source of language names" $
  list [
    triple (string "haskell")     (string "hydra-haskell")    (string "haskell"),
    triple (string "java")        (string "hydra-java")       (string "jvm"),
    triple (string "python")      (string "hydra-python")     (string "python"),
    triple (string "scala")       (string "hydra-scala")      (string "jvm"),
    triple (string "go")          (string "hydra-go")         (string "go"),
    triple (string "typescript")  (string "hydra-typescript") (string "typescript"),
    triple (string "clojure")     (string "hydra-lisp")       (string "lisp"),
    triple (string "scheme")      (string "hydra-lisp")       (string "lisp"),
    triple (string "common-lisp") (string "hydra-lisp")       (string "lisp"),
    triple (string "emacs-lisp")  (string "hydra-lisp")       (string "lisp")]

-- | Every language name in the registry, in registry order (the former @ALL_LANGS@
-- bash constant): @map first languageProfiles@. Neutral logic uses this instead of
-- a hardcoded list.
allLanguageNames :: TypedTermDefinition [String]
allLanguageNames = define "allLanguageNames" $
  doc "Every language name in the registry (the former ALL_LANGS constant)" $
  Lists.map ("p" ~> Pairs.first (var "p")) (asTerm languageProfiles)

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
  list (string <$> [
    "haskell", "java", "python", "scala", "typescript",
    "clojure", "scheme", "common-lisp", "emacs-lisp"])

-- | The hosts with a kernel benchmark suite (@bin/run-benchmark-tests.sh@ @ALL_HOSTS@):
-- the three self-hosting non-JVM-plus-JVM hosts plus the four Lisp dialects (no
-- scala/go/typescript bench suite).
benchHostNames :: TypedTermDefinition [String]
benchHostNames = define "benchHostNames" $
  doc "Hosts with a kernel benchmark suite (run-benchmark-tests.sh ALL_HOSTS)" $
  list (string <$> [
    "haskell", "java", "python",
    "clojure", "common-lisp", "emacs-lisp", "scheme"])

-- | The hosts with an inference benchmark (@bin/run-inference-bench.sh@ @ALL_HOSTS@).
-- Includes the @python-pypy@ pseudo-host (a distinct bench runner that maps back to
-- python's bench package); excludes scala/go/typescript/clojure/scheme.
inferenceBenchHostNames :: TypedTermDefinition [String]
inferenceBenchHostNames = define "inferenceBenchHostNames" $
  doc "Hosts with an inference benchmark (run-inference-bench.sh ALL_HOSTS; includes python-pypy pseudo-host)" $
  list (string <$> [
    "haskell", "java", "python", "python-pypy", "common-lisp", "emacs-lisp"])

-- | The default bench scope (@bin/sync-bench.sh@ @DEFAULT_HOSTS@): the bootstrapping
-- triad only (lisp excluded — the coders are too slow for the default bench sync).
benchDefaultNames :: TypedTermDefinition [String]
benchDefaultNames = define "benchDefaultNames" $
  doc "Default bench scope (sync-bench.sh DEFAULT_HOSTS; the haskell/java/python triad)" $
  list (string <$> ["haskell", "java", "python"])

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
        (Lists.filter ("p" ~> Equality.equal (Pairs.first (var "p")) (var "name"))
          (asTerm languageProfiles)))
      (Strings.concat2 (string "hydra-") (var "name"))
      ("p" ~> Pairs.first (Pairs.second (var "p")))

-- | The family of a language: the @family@ field of its profile, or the empty
-- string for an unknown name.
familyFor :: TypedTermDefinition (String -> String)
familyFor = define "familyFor" $
  doc "The build family of a language (jvm/python/lisp/haskell/typescript/go; empty for unknown)" $
  "name" ~>
    Optionals.match
      (Lists.head
        (Lists.filter ("p" ~> Equality.equal (Pairs.first (var "p")) (var "name"))
          (asTerm languageProfiles)))
      (string "")
      ("p" ~> Pairs.second (Pairs.second (var "p")))

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
