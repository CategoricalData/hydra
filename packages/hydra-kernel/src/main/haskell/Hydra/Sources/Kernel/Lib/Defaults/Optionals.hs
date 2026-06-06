
module Hydra.Sources.Kernel.Lib.Defaults.Optionals where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (maybe)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths     as Paths
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Util       as Util
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Json.Model          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import qualified Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Literals as MetaLiterals
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Packaging        as Packaging
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Meta.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import qualified Hydra.Dsl.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), maybe, map)
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Dsl.Errors       as Error
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore

ns :: ModuleName
ns = ModuleName "hydra.lib.defaults.optionals"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([ExtractCore.ns, ShowCore.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just ("Default term-level implementations of optional functions for the Hydra interpreter."))}
  where
    definitions = [
      toDefinition apply_,
      toDefinition bind_,
      toDefinition cases_,
      toDefinition cat_,
      toDefinition compose_,
      toDefinition fromJust_,
      toDefinition fromOptional_,
      toDefinition isGiven_,
      toDefinition isNone_,
      toDefinition map_,
      toDefinition mapOptional_,
      toDefinition pure_,
      toDefinition toList_]

-- | Interpreter-friendly applicative apply for optional terms.
-- apply (Just f) (Just x) = Just (f x); otherwise Nothing
-- We manually construct the result because the nested lambda would be flattened in Python.
-- The logic is: apply (Just f) (Just x) = Just (f x)
apply_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Term -> Either Error Term)
apply_ = define "apply" $
  doc "Interpreter-friendly applicative apply for optional terms." $
  "cx" ~> "g" ~> "funOptTerm" ~> "argOptTerm" ~>
  cases _Term (var "funOptTerm")
    (Just (ExtractCore.unexpected (string "optional function") (ShowCore.term @@ var "funOptTerm"))) [
    _Term_optional>>: "mf" ~>
      cases _Term (var "argOptTerm")
        (Just (ExtractCore.unexpected (string "optional value") (ShowCore.term @@ var "argOptTerm"))) [
        _Term_optional>>: "mx" ~>
          -- Manual applicative apply: for Just f, Just x => Just (f x); else Nothing
          -- We use Optionals.bind to handle the nested optionals without requiring curried lambdas
          right $ Core.termOptional $
            Optionals.bind (var "mf") $
              "f" ~> Optionals.map ("x" ~> Core.termApplication $ Core.application (var "f") (var "x")) (var "mx")]]

-- | Interpreter-friendly monadic bind for optional terms.
-- bind (Just x) f = f x; bind Nothing f = Nothing
bind_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Term -> Either Error Term)
bind_ = define "bind" $
  doc "Interpreter-friendly monadic bind for optional terms." $
  "cx" ~> "g" ~> "optTerm" ~> "funTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_optional>>: "m" ~>
      right $ Optionals.cases (var "m") (Core.termOptional nothing) ("val" ~> Core.termApplication $ Core.application (var "funTerm") (var "val"))]

-- | Interpreter-friendly case analysis for optional terms (cases variant).
-- Takes optTerm, defaultTerm, funTerm - returns defaultTerm if Nothing,
-- or applies funTerm to the value if Just.
cases_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Term -> Term -> Either Error Term)
cases_ = define "cases" $
  doc "Interpreter-friendly case analysis for optional terms (cases argument order)." $
  "cx" ~> "g" ~> "optTerm" ~> "defaultTerm" ~> "funTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_optional>>: "m" ~>
      right $ Optionals.cases (var "m") (var "defaultTerm") ("val" ~> Core.termApplication $ Core.application (var "funTerm") (var "val"))]

-- | Interpreter-friendly cat (catMaybes) for list of optional terms.
-- Filters out Nothings and unwraps Justs.
cat_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Either Error Term)
cat_ = define "cat" $
  doc "Interpreter-friendly cat for list of optional terms." $
  "cx" ~> "g" ~>
  "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
  -- Build: foldl (\acc el -> case el of TermOptional m -> maybe acc (\v -> concat2 acc [v]) m; _ -> acc) [] elements
  right $ Lists.foldl
    ("acc" ~> "el" ~>
      cases _Term (var "el")
        (Just (var "acc")) [
        _Term_optional>>: "m" ~>
          Optionals.cases (var "m") (var "acc") ("v" ~> Lists.concat2 (var "acc") (Lists.pure (var "v")))])
    (list ([] :: [TypedTerm Term]))
    (var "elements")

-- | Interpreter-friendly Kleisli composition for optionals.
-- compose f g x = bind (f x) g
compose_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Term -> Term -> Either Error Term)
compose_ = define "compose" $
  doc "Interpreter-friendly Kleisli composition for optionals." $
  "cx" ~> "g" ~> "funF" ~> "funG" ~> "xTerm" ~>
  -- Compute: bind (f x) g
  -- This builds the term: bind @ (funF @ xTerm) @ funG
  right $ Core.termApplication $ Core.application
    (Core.termApplication $ Core.application
      (Core.termVariable $ wrap _Name $ string "hydra.lib.optionals.bind")
      (Core.termApplication $ Core.application (var "funF") (var "xTerm")))
    (var "funG")

-- | Interpreter-friendly fromJust for optional terms.
-- Extracts the value from Just, or errors on Nothing.
fromJust_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Either Error Term)
fromJust_ = define "fromJust" $
  doc "Interpreter-friendly fromJust for optional terms." $
  "cx" ~> "g" ~>
  "optTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_optional>>: "m" ~>
      Optionals.cases (var "m") (ExtractCore.unexpected (string "Just value") (ShowCore.term @@ var "optTerm")) ("val" ~> right $ var "val")]

-- | Interpreter-friendly fromOptional for optional terms.
-- Returns the contained value or a default.
fromOptional_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Term -> Either Error Term)
fromOptional_ = define "fromOptional" $
  doc "Interpreter-friendly fromOptional for optional terms." $
  "cx" ~> "g" ~>
  "defaultTerm" ~> "optTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_optional>>: "m" ~>
      right $ Optionals.cases (var "m") (var "defaultTerm") ("val" ~> var "val")]

-- | Interpreter-friendly isGiven for optional terms.
isGiven_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Either Error Term)
isGiven_ = define "isGiven" $
  doc "Interpreter-friendly isGiven for optional terms." $
  "cx" ~> "g" ~>
  "optTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_optional>>: "m" ~>
      right $ Optionals.cases (var "m") (Core.termLiteral $ Core.literalBoolean $ MetaLiterals.boolean False) ("_" ~> Core.termLiteral $ Core.literalBoolean $ MetaLiterals.boolean True)]

-- | Interpreter-friendly isNone for optional terms.
isNone_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Either Error Term)
isNone_ = define "isNone" $
  doc "Interpreter-friendly isNone for optional terms." $
  "cx" ~> "g" ~>
  "optTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_optional>>: "m" ~>
      right $ Optionals.cases (var "m") (Core.termLiteral $ Core.literalBoolean $ MetaLiterals.boolean True) ("_" ~> Core.termLiteral $ Core.literalBoolean $ MetaLiterals.boolean False)]

-- | Interpreter-friendly mapOptional for List terms.
-- Applies funTerm to each element, keeping only Just results.
mapOptional_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Term -> Either Error Term)
mapOptional_ = define "mapOptional" $
  doc "Interpreter-friendly mapOptional for List terms." $
  "cx" ~> "g" ~> "funTerm" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
  -- Build: cat (map funTerm elements) - cat filters out Nothings and unwraps Justs
  right $ Core.termApplication $ Core.application
    (Core.termVariable $ wrap _Name $ string "hydra.lib.optionals.cat")
    (Core.termList $ Lists.map
      ("el" ~> Core.termApplication $ Core.application (var "funTerm") (var "el"))
      (var "elements"))

-- | Interpreter-friendly map for optional terms.
-- Returns Nothing if Nothing, or Just (fun val) if Just val.
map_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Term -> Either Error Term)
map_ = define "map" $
  doc "Interpreter-friendly map for optional terms." $
  "cx" ~> "g" ~> "funTerm" ~> "optTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_optional>>: "m" ~>
      right $ Core.termOptional $ Optionals.map
        ("val" ~> Core.termApplication $ Core.application (var "funTerm") (var "val"))
        (var "m")]

-- | Interpreter-friendly pure for optional terms.
-- Wraps a value in Just.
pure_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Either Error Term)
pure_ = define "pure" $
  doc "Interpreter-friendly pure for optional terms." $
  "cx" ~> "g" ~>
  "x" ~>
  right $ Core.termOptional $ just $ var "x"

-- | Interpreter-friendly toList for optional terms.
-- Just x -> [x], Nothing -> []
toList_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Either Error Term)
toList_ = define "toList" $
  doc "Interpreter-friendly toList for optional terms." $
  "cx" ~> "g" ~>
  "optTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_optional>>: "m" ~>
      right $ Core.termList $ Optionals.cases (var "m") (list ([] :: [TypedTerm Term])) ("val" ~> Lists.pure (var "val"))]
