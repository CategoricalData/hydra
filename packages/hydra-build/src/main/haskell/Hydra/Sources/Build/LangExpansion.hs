module Hydra.Sources.Build.LangExpansion where

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
import qualified Hydra.Sources.Build.Registry as Registry


-- | Pure language/host list-expansion utilities shared by the sync/test/bench
-- drivers (#416 P2 / langexpansion). A DSL-level, translingual home for the
-- @all@/@lisp@ alias expansion + validation + order-preserving dedup that
-- @bin/sync.sh@ (expand_langs/validate_lang/LANG_UNION), @bin/test.sh@,
-- @bin/run-benchmark-tests.sh@, @bin/run-inference-bench.sh@, and
-- @bin/sync-bench.sh@ each re-hardcode. Everything here is pure: no primitives,
-- no I/O. Becomes the canonical source of the language/dialect enumeration
-- (P1's sync-matrix consumes it). Reuses @Lists.distinct@ for the dedup, exactly
-- as @Hydra.Sources.Build.Modules.dedupPreservingOrder@ does.
ns :: ModuleName
ns = ModuleName "hydra.build.langexpansion"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Strip.ns, ModuleName "hydra.build.registry"] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Pure language/host list-expansion utilities shared by the sync/test/bench drivers")}
  where
   definitions = [
     toDefinition allLanguages,
     toDefinition expandLangAlias,
     toDefinition expandLangs,
     toDefinition isKnownLang,
     toDefinition langUnion,
     toDefinition lispDialects,
     toDefinition validateLangs]

-- | The canonical set of all target languages (the former @ALL_LANGS@ bash
-- constant): the nine hosts plus the Go head bud.
allLanguages :: TypedTermDefinition [String]
allLanguages = define "allLanguages" $
  doc "The canonical set of all target languages, read from the registry (the language names, in registry order)" $
  asTerm Registry.allLanguageNames

-- | Expand a single language token: @all@ to every language, @lisp@ to the four
-- dialects, anything else to itself (a singleton list).
expandLangAlias :: TypedTermDefinition (String -> [String])
expandLangAlias = define "expandLangAlias" $
  doc "Expand a single language token: 'all' -> all languages, 'lisp' -> the dialects, else itself" $
  "tok" ~>
    Logic.ifElse (Equality.equal (var "tok") (string "all"))
      (asTerm allLanguages)
      (Logic.ifElse (Equality.equal (var "tok") (string "lisp"))
        (asTerm lispDialects)
        (list [var "tok"]))

-- | Expand a list of language tokens: expand each alias, flatten, and dedup
-- preserving first-occurrence order.
expandLangs :: TypedTermDefinition ([String] -> [String])
expandLangs = define "expandLangs" $
  doc "Expand a list of language tokens (aliases resolved), preserving order, deduped" $
  "toks" ~>
    Lists.distinct
      (Lists.concat (Lists.map ("tok" ~> expandLangAlias @@ var "tok") (var "toks")))

-- | Whether a language token is a known target language.
isKnownLang :: TypedTermDefinition (String -> Bool)
isKnownLang = define "isKnownLang" $
  doc "Whether a language token is a known target language" $
  "lang" ~> Lists.member (var "lang") (asTerm allLanguages)

-- | The expanded, deduped union of two token lists (hosts + targets) — the
-- former @LANG_UNION@ step.
langUnion :: TypedTermDefinition ([String] -> [String] -> [String])
langUnion = define "langUnion" $
  doc "The expanded, deduped union of two token lists (hosts + targets)" $
  "hosts" ~> "targets" ~>
    expandLangs @@ (Lists.concat (list [var "hosts", var "targets"]))

-- | The four Lisp dialects that the @lisp@ alias expands to.
lispDialects :: TypedTermDefinition [String]
lispDialects = define "lispDialects" $
  doc "The Lisp dialects the 'lisp' alias expands to, read from the registry (the family=lisp language names)" $
  asTerm Registry.lispDialectNames

-- | Validate a token list: expand it, then return the unknown tokens (empty
-- when all are valid). The caller decides how to fail on a non-empty result.
validateLangs :: TypedTermDefinition ([String] -> [String])
validateLangs = define "validateLangs" $
  doc "Validate a token list; return the unknown expanded tokens (empty = all valid)" $
  "toks" ~>
    Lists.filter
      ("lang" ~> Logic.not (isKnownLang @@ var "lang"))
      (expandLangs @@ var "toks")
