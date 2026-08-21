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
     toDefinition coderPackageFor,
     toDefinition familyFor,
     toDefinition isLispDialect,
     toDefinition languageProfiles,
     toDefinition lispDialectNames,
     toDefinition rootCoderHost]

-- | The registry as data: one @(name, coderPackage, family)@ triple per language.
-- @name@ is the canonical language token (the former ad-hoc bash identifiers);
-- @coderPackage@ is the distribution package the language's coder lives in (note
-- the four Lisp dialects share @hydra-lisp@ — the collapse that @"hydra-"++name@
-- got WRONG); @family@ groups languages that share build treatment (@jvm@,
-- @python@, @lisp@, @haskell@, @typescript@, @go@). This is the ONLY place a
-- language name is written down.
languageProfiles :: TypedTermDefinition [(String, String, String)]
languageProfiles = define "languageProfiles" $
  doc "Per-language build identity as (name, coderPackage, family) triples: the single source of language names" $
  list [
    triple (string "clojure")     (string "hydra-lisp")       (string "lisp"),
    triple (string "common-lisp") (string "hydra-lisp")       (string "lisp"),
    triple (string "emacs-lisp")  (string "hydra-lisp")       (string "lisp"),
    triple (string "go")          (string "hydra-go")         (string "go"),
    triple (string "haskell")     (string "hydra-haskell")    (string "haskell"),
    triple (string "java")        (string "hydra-java")       (string "jvm"),
    triple (string "python")      (string "hydra-python")     (string "python"),
    triple (string "scala")       (string "hydra-scala")      (string "jvm"),
    triple (string "scheme")      (string "hydra-lisp")       (string "lisp"),
    triple (string "typescript")  (string "hydra-typescript") (string "typescript")]

-- | Every language name in the registry, in registry order (the former @ALL_LANGS@
-- bash constant): @map first languageProfiles@. Neutral logic uses this instead of
-- a hardcoded list.
allLanguageNames :: TypedTermDefinition [String]
allLanguageNames = define "allLanguageNames" $
  doc "Every language name in the registry (the former ALL_LANGS constant)" $
  Lists.map ("p" ~> Pairs.first (var "p")) (asTerm languageProfiles)

-- | The four Lisp-dialect names: the registry rows whose family is @lisp@. Drives
-- the @lisp@ alias expansion without hardcoding the dialect list.
lispDialectNames :: TypedTermDefinition [String]
lispDialectNames = define "lispDialectNames" $
  doc "The language names whose family is 'lisp' (drives the 'lisp' alias)" $
  Lists.map ("p" ~> Pairs.first (var "p"))
    (Lists.filter ("p" ~> Equality.equal (Pairs.second (Pairs.second (var "p"))) (string "lisp"))
      (asTerm languageProfiles))

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
