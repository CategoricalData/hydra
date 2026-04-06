
module Hydra.Sources.Eval.Lib.Maybes where

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
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
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

import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Errors       as Error
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore

ns :: Namespace
ns = Namespace "hydra.eval.lib.maybes"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns definitions
    [ExtractCore.ns, ShowCore.ns]
    kernelTypesNamespaces $
    Just ("Evaluation-level implementations of Maybe functions for the Hydra interpreter.")
  where
    definitions = [
      toDefinition apply_,
      toDefinition bind_,
      toDefinition cases_,
      toDefinition cat_,
      toDefinition compose_,
      toDefinition fromJust_,
      toDefinition fromMaybe_,
      toDefinition isJust_,
      toDefinition isNothing_,
      toDefinition map_,
      toDefinition mapMaybe_,
      toDefinition maybe_,
      toDefinition pure_,
      toDefinition toList_]

-- | Interpreter-friendly applicative apply for Maybe terms.
-- apply (Just f) (Just x) = Just (f x); otherwise Nothing
-- We manually construct the result because the nested lambda would be flattened in Python.
-- The logic is: apply (Just f) (Just x) = Just (f x)
apply_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
apply_ = define "apply" $
  doc "Interpreter-friendly applicative apply for Maybe terms." $
  "cx" ~> "g" ~> "funOptTerm" ~> "argOptTerm" ~>
  cases _Term (var "funOptTerm")
    (Just (ExtractCore.unexpected (var "cx") (string "optional function") (ShowCore.term @@ var "funOptTerm"))) [
    _Term_maybe>>: "mf" ~>
      cases _Term (var "argOptTerm")
        (Just (ExtractCore.unexpected (var "cx") (string "optional value") (ShowCore.term @@ var "argOptTerm"))) [
        _Term_maybe>>: "mx" ~>
          -- Manual applicative apply: for Just f, Just x => Just (f x); else Nothing
          -- We use Maybes.bind to handle the nested Maybes without requiring curried lambdas
          right $ Core.termMaybe $
            Maybes.bind (var "mf") $
              "f" ~> Maybes.map ("x" ~> Core.termApplication $ Core.application (var "f") (var "x")) (var "mx")]]

-- | Interpreter-friendly monadic bind for Maybe terms.
-- bind (Just x) f = f x; bind Nothing f = Nothing
bind_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
bind_ = define "bind" $
  doc "Interpreter-friendly monadic bind for Maybe terms." $
  "cx" ~> "g" ~> "optTerm" ~> "funTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (var "cx") (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_maybe>>: "m" ~>
      right $ Maybes.maybe
        (Core.termMaybe nothing)
        ("val" ~> Core.termApplication $ Core.application (var "funTerm") (var "val"))
        (var "m")]

-- | Interpreter-friendly cat (catMaybes) for list of Maybe terms.
-- Filters out Nothings and unwraps Justs.
cat_ :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) Term)
cat_ = define "cat" $
  doc "Interpreter-friendly cat for list of Maybe terms." $
  "cx" ~> "g" ~>
  "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "cx" @@ var "g" @@ var "listTerm") $
  -- Build: foldl (\acc el -> case el of TermMaybe m -> maybe acc (\v -> concat2 acc [v]) m; _ -> acc) [] elements
  right $ Lists.foldl
    ("acc" ~> "el" ~>
      cases _Term (var "el")
        (Just (var "acc")) [
        _Term_maybe>>: "m" ~>
          Maybes.maybe
            (var "acc")
            ("v" ~> Lists.concat2 (var "acc") (Lists.pure (var "v")))
            (var "m")])
    (list ([] :: [TTerm Term]))
    (var "elements")

-- | Interpreter-friendly case analysis for Maybe terms (cases variant).
-- Takes optTerm, defaultTerm, funTerm - returns defaultTerm if Nothing,
-- or applies funTerm to the value if Just.
cases_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Term -> Either (InContext Error) Term)
cases_ = define "cases" $
  doc "Interpreter-friendly case analysis for Maybe terms (cases argument order)." $
  "cx" ~> "g" ~> "optTerm" ~> "defaultTerm" ~> "funTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (var "cx") (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_maybe>>: "m" ~>
      right $ Maybes.maybe
        (var "defaultTerm")
        ("val" ~> Core.termApplication $ Core.application (var "funTerm") (var "val"))
        (var "m")]

-- | Interpreter-friendly fromJust for Maybe terms.
-- Extracts the value from Just, or errors on Nothing.
fromJust_ :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) Term)
fromJust_ = define "fromJust" $
  doc "Interpreter-friendly fromJust for Maybe terms." $
  "cx" ~> "g" ~>
  "optTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (var "cx") (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_maybe>>: "m" ~>
      Maybes.maybe
        (ExtractCore.unexpected (var "cx") (string "Just value") (ShowCore.term @@ var "optTerm"))
        ("val" ~> right $ var "val")
        (var "m")]

-- | Interpreter-friendly fromMaybe for Maybe terms.
-- Returns the contained value or a default.
fromMaybe_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
fromMaybe_ = define "fromMaybe" $
  doc "Interpreter-friendly fromMaybe for Maybe terms." $
  "cx" ~> "g" ~>
  "defaultTerm" ~> "optTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (var "cx") (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_maybe>>: "m" ~>
      right $ Maybes.maybe
        (var "defaultTerm")
        ("val" ~> var "val")
        (var "m")]

-- | Interpreter-friendly isJust for Maybe terms.
isJust_ :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) Term)
isJust_ = define "isJust" $
  doc "Interpreter-friendly isJust for Maybe terms." $
  "cx" ~> "g" ~>
  "optTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (var "cx") (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_maybe>>: "m" ~>
      right $ Maybes.maybe
        (Core.termLiteral $ Core.literalBoolean $ MetaLiterals.boolean False)
        ("_" ~> Core.termLiteral $ Core.literalBoolean $ MetaLiterals.boolean True)
        (var "m")]

-- | Interpreter-friendly isNothing for Maybe terms.
isNothing_ :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) Term)
isNothing_ = define "isNothing" $
  doc "Interpreter-friendly isNothing for Maybe terms." $
  "cx" ~> "g" ~>
  "optTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (var "cx") (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_maybe>>: "m" ~>
      right $ Maybes.maybe
        (Core.termLiteral $ Core.literalBoolean $ MetaLiterals.boolean True)
        ("_" ~> Core.termLiteral $ Core.literalBoolean $ MetaLiterals.boolean False)
        (var "m")]

-- | Interpreter-friendly Kleisli composition for Maybe.
-- compose f g x = bind (f x) g
compose_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Term -> Either (InContext Error) Term)
compose_ = define "compose" $
  doc "Interpreter-friendly Kleisli composition for Maybe." $
  "cx" ~> "g" ~> "funF" ~> "funG" ~> "xTerm" ~>
  -- Compute: bind (f x) g
  -- This builds the term: bind @ (funF @ xTerm) @ funG
  right $ Core.termApplication $ Core.application
    (Core.termApplication $ Core.application
      (Core.termVariable $ wrap _Name $ string "hydra.lib.maybes.bind")
      (Core.termApplication $ Core.application (var "funF") (var "xTerm")))
    (var "funG")

-- | Interpreter-friendly map for Maybe terms.
-- Returns Nothing if Nothing, or Just (fun val) if Just val.
map_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
map_ = define "map" $
  doc "Interpreter-friendly map for Maybe terms." $
  "cx" ~> "g" ~> "funTerm" ~> "optTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (var "cx") (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_maybe>>: "m" ~>
      right $ Core.termMaybe $ Maybes.map
        ("val" ~> Core.termApplication $ Core.application (var "funTerm") (var "val"))
        (var "m")]

-- | Interpreter-friendly mapMaybe for List terms.
-- Applies funTerm to each element, keeping only Just results.
mapMaybe_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
mapMaybe_ = define "mapMaybe" $
  doc "Interpreter-friendly mapMaybe for List terms." $
  "cx" ~> "g" ~> "funTerm" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "cx" @@ var "g" @@ var "listTerm") $
  -- Build: cat (map funTerm elements) - cat filters out Nothings and unwraps Justs
  right $ Core.termApplication $ Core.application
    (Core.termVariable $ wrap _Name $ string "hydra.lib.maybes.cat")
    (Core.termList $ Lists.map
      ("el" ~> Core.termApplication $ Core.application (var "funTerm") (var "el"))
      (var "elements"))

-- | Interpreter-friendly case analysis for Maybe terms.
-- Takes defaultTerm, funTerm, optTerm - returns defaultTerm if Nothing,
-- or applies funTerm to the value if Just.
maybe_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Term -> Either (InContext Error) Term)
maybe_ = define "maybe" $
  doc "Interpreter-friendly case analysis for Maybe terms." $
  "cx" ~> "g" ~> "defaultTerm" ~> "funTerm" ~> "optTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (var "cx") (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_maybe>>: "m" ~>
      right $ Maybes.maybe
        (var "defaultTerm")
        ("val" ~> Core.termApplication $ Core.application (var "funTerm") (var "val"))
        (var "m")]

-- | Interpreter-friendly pure for Maybe terms.
-- Wraps a value in Just.
pure_ :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) Term)
pure_ = define "pure" $
  doc "Interpreter-friendly pure for Maybe terms." $
  "cx" ~> "g" ~>
  "x" ~>
  right $ Core.termMaybe $ just $ var "x"

-- | Interpreter-friendly toList for Maybe terms.
-- Just x -> [x], Nothing -> []
toList_ :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) Term)
toList_ = define "toList" $
  doc "Interpreter-friendly toList for Maybe terms." $
  "cx" ~> "g" ~>
  "optTerm" ~>
  cases _Term (var "optTerm")
    (Just (ExtractCore.unexpected (var "cx") (string "optional value") (ShowCore.term @@ var "optTerm"))) [
    _Term_maybe>>: "m" ~>
      right $ Core.termList $ Maybes.maybe
        (list ([] :: [TTerm Term]))
        ("val" ~> Lists.pure (var "val"))
        (var "m")]
