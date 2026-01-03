
module Hydra.Sources.Eval.Lib.Maybes where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (maybe)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
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
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), maybe, map)
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


ns :: Namespace
ns = Namespace "hydra.eval.lib.maybes"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [ExtractCore.ns, Monads.ns, ShowCore.ns]
    kernelTypesNamespaces $
    Just ("Evaluation-level implementations of Maybe functions for the Hydra interpreter.")
  where
    elements = [
      toBinding apply_,
      toBinding bind_,
      toBinding cases_,
      toBinding compose_,
      toBinding map_,
      toBinding mapMaybe_,
      toBinding maybe_]

-- | Interpreter-friendly applicative apply for Maybe terms.
-- apply (Just f) (Just x) = Just (f x); otherwise Nothing
-- We manually construct the result because the nested lambda would be flattened in Python.
-- The logic is: apply (Just f) (Just x) = Just (f x)
apply_ :: TBinding (Term -> Term -> Flow s Term)
apply_ = define "apply" $
  doc "Interpreter-friendly applicative apply for Maybe terms." $
  "funOptTerm" ~> "argOptTerm" ~>
  cases _Term (var "funOptTerm")
    (Just (Monads.unexpected @@ string "optional function" @@ (ShowCore.term @@ var "funOptTerm"))) [
    _Term_maybe>>: "mf" ~>
      cases _Term (var "argOptTerm")
        (Just (Monads.unexpected @@ string "optional value" @@ (ShowCore.term @@ var "argOptTerm"))) [
        _Term_maybe>>: "mx" ~>
          -- Manual applicative apply: for Just f, Just x => Just (f x); else Nothing
          -- We use Maybes.bind to handle the nested Maybes without requiring curried lambdas
          produce $ Core.termMaybe $
            Maybes.bind (var "mf") $
              "f" ~> Maybes.map ("x" ~> Core.termApplication $ Core.application (var "f") (var "x")) (var "mx")]]

-- | Interpreter-friendly monadic bind for Maybe terms.
-- bind (Just x) f = f x; bind Nothing f = Nothing
bind_ :: TBinding (Term -> Term -> Flow s Term)
bind_ = define "bind" $
  doc "Interpreter-friendly monadic bind for Maybe terms." $
  "optTerm" ~> "funTerm" ~>
  cases _Term (var "optTerm")
    (Just (Monads.unexpected @@ string "optional value" @@ (ShowCore.term @@ var "optTerm"))) [
    _Term_maybe>>: "m" ~>
      produce $ Maybes.maybe
        (Core.termMaybe nothing)
        ("val" ~> Core.termApplication $ Core.application (var "funTerm") (var "val"))
        (var "m")]

-- | Interpreter-friendly case analysis for Maybe terms (cases variant).
-- Takes optTerm, defaultTerm, funTerm - returns defaultTerm if Nothing,
-- or applies funTerm to the value if Just.
cases_ :: TBinding (Term -> Term -> Term -> Flow s Term)
cases_ = define "cases" $
  doc "Interpreter-friendly case analysis for Maybe terms (cases argument order)." $
  "optTerm" ~> "defaultTerm" ~> "funTerm" ~>
  cases _Term (var "optTerm")
    (Just (Monads.unexpected @@ string "optional value" @@ (ShowCore.term @@ var "optTerm"))) [
    _Term_maybe>>: "m" ~>
      produce $ Maybes.maybe
        (var "defaultTerm")
        ("val" ~> Core.termApplication $ Core.application (var "funTerm") (var "val"))
        (var "m")]

-- | Interpreter-friendly Kleisli composition for Maybe.
-- compose f g x = bind (f x) g
compose_ :: TBinding (Term -> Term -> Term -> Flow s Term)
compose_ = define "compose" $
  doc "Interpreter-friendly Kleisli composition for Maybe." $
  "funF" ~> "funG" ~> "xTerm" ~>
  -- Compute: bind (f x) g
  -- This builds the term: bind @ (funF @ xTerm) @ funG
  produce $ Core.termApplication $ Core.application
    (Core.termApplication $ Core.application
      (Core.termFunction $ Core.functionPrimitive $ wrap _Name $ string "hydra.lib.maybes.bind")
      (Core.termApplication $ Core.application (var "funF") (var "xTerm")))
    (var "funG")

-- | Interpreter-friendly map for Maybe terms.
-- Returns Nothing if Nothing, or Just (fun val) if Just val.
map_ :: TBinding (Term -> Term -> Flow s Term)
map_ = define "map" $
  doc "Interpreter-friendly map for Maybe terms." $
  "funTerm" ~> "optTerm" ~>
  cases _Term (var "optTerm")
    (Just (Monads.unexpected @@ string "optional value" @@ (ShowCore.term @@ var "optTerm"))) [
    _Term_maybe>>: "m" ~>
      produce $ Core.termMaybe $ Maybes.map
        ("val" ~> Core.termApplication $ Core.application (var "funTerm") (var "val"))
        (var "m")]

-- | Interpreter-friendly mapMaybe for List terms.
-- Applies funTerm to each element, keeping only Just results.
mapMaybe_ :: TBinding (Term -> Term -> Flow s Term)
mapMaybe_ = define "mapMaybe" $
  doc "Interpreter-friendly mapMaybe for List terms." $
  "funTerm" ~> "listTerm" ~>
  "elements" <<~ ExtractCore.list @@ var "listTerm" $
  -- Build: cat (map funTerm elements) - cat filters out Nothings and unwraps Justs
  produce $ Core.termApplication $ Core.application
    (Core.termFunction $ Core.functionPrimitive $ wrap _Name $ string "hydra.lib.maybes.cat")
    (Core.termList $ Lists.map
      ("el" ~> Core.termApplication $ Core.application (var "funTerm") (var "el"))
      (var "elements"))

-- | Interpreter-friendly case analysis for Maybe terms.
-- Takes defaultTerm, funTerm, optTerm - returns defaultTerm if Nothing,
-- or applies funTerm to the value if Just.
maybe_ :: TBinding (Term -> Term -> Term -> Flow s Term)
maybe_ = define "maybe" $
  doc "Interpreter-friendly case analysis for Maybe terms." $
  "defaultTerm" ~> "funTerm" ~> "optTerm" ~>
  cases _Term (var "optTerm")
    (Just (Monads.unexpected @@ string "optional value" @@ (ShowCore.term @@ var "optTerm"))) [
    _Term_maybe>>: "m" ~>
      produce $ Maybes.maybe
        (var "defaultTerm")
        ("val" ~> Core.termApplication $ Core.application (var "funTerm") (var "val"))
        (var "m")]
