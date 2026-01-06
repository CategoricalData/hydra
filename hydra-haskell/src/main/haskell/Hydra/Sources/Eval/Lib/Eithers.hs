
module Hydra.Sources.Eval.Lib.Eithers where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (either)
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
import           Prelude hiding ((++), either)
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


ns :: Namespace
ns = Namespace "hydra.eval.lib.eithers"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [Monads.ns, ShowCore.ns]
    kernelTypesNamespaces $
    Just ("Evaluation-level implementations of Either functions for the Hydra interpreter.")
  where
    elements = [
      toBinding bind_,
      toBinding bimap_,
      toBinding either_,
      toBinding map_,
      toBinding mapList_,
      toBinding mapMaybe_]

-- | Interpreter-friendly bind for Either terms.
-- Takes an Either term and a function term, applies the function to the Right
-- value and returns the result (which should be an Either), or returns the Left unchanged.
bind_ :: TBinding (Term -> Term -> Flow s Term)
bind_ = define "bind" $
  doc "Interpreter-friendly bind for Either terms." $
  "eitherTerm" ~> "funTerm" ~>
  cases _Term (var "eitherTerm")
    (Just (Monads.unexpected @@ string "either value" @@ (ShowCore.term @@ var "eitherTerm"))) [
    _Term_either>>: "e" ~>
      produce $ Eithers.either_
        -- If Left: return the Left unchanged
        ("val" ~> Core.termEither $ left $ var "val")
        -- If Right: apply funTerm to the value
        ("val" ~> Core.termApplication $ Core.application (var "funTerm") (var "val"))
        (var "e")]

-- | Interpreter-friendly bimap for Either terms.
-- Takes two function terms (for left and right) and an Either term, applies
-- the appropriate function to the contained value and re-wraps the result.
bimap_ :: TBinding (Term -> Term -> Term -> Flow s Term)
bimap_ = define "bimap" $
  doc "Interpreter-friendly bimap for Either terms." $
  "leftFun" ~> "rightFun" ~> "eitherTerm" ~>
  cases _Term (var "eitherTerm")
    (Just (Monads.unexpected @@ string "either value" @@ (ShowCore.term @@ var "eitherTerm"))) [
    _Term_either>>: "e" ~>
      produce $ Eithers.either_
        ("val" ~> Core.termEither $ left $ Core.termApplication $ Core.application (var "leftFun") (var "val"))
        ("val" ~> Core.termEither $ right $ Core.termApplication $ Core.application (var "rightFun") (var "val"))
        (var "e")]

-- | Interpreter-friendly case analysis for Either terms.
-- Takes two function terms and an Either term, applies the appropriate function
-- to the contained value.
either_ :: TBinding (Term -> Term -> Term -> Flow s Term)
either_ = define "either" $
  doc "Interpreter-friendly case analysis for Either terms." $
  "leftFun" ~> "rightFun" ~> "eitherTerm" ~>
  cases _Term (var "eitherTerm")
    (Just (Monads.unexpected @@ string "either value" @@ (ShowCore.term @@ var "eitherTerm"))) [
    _Term_either>>: "e" ~>
      produce $ Eithers.either_
        ("val" ~> Core.termApplication $ Core.application (var "leftFun") (var "val"))
        ("val" ~> Core.termApplication $ Core.application (var "rightFun") (var "val"))
        (var "e")]

map_ :: TBinding (Term -> Term -> Flow s Term)
map_ = define "map" $
  doc "Interpreter-friendly map for Either terms." $
  "rightFun" ~> "eitherTerm" ~>
  cases _Term (var "eitherTerm")
    (Just (Monads.unexpected @@ string "either value" @@ (ShowCore.term @@ var "eitherTerm"))) [
    _Term_either>>: "e" ~>
      produce $ Eithers.either_
        ("val" ~> Core.termEither $ left $ Core.termEither $ left $ var "val")
        ("val" ~> Core.termEither $ right $ Core.termApplication $ Core.application (var "rightFun") (var "val"))
        (var "e")]

-- | Interpreter-friendly mapList for Either (traverse).
-- mapList funTerm listTerm: applies funTerm to each element, collecting results.
-- Short-circuits on first Left error.
mapList_ :: TBinding (Term -> Term -> Flow s Term)
mapList_ = define "mapList" $
  doc "Interpreter-friendly mapList for Either (traverse)." $
  "funTerm" ~> "listTerm" ~>
  "elements" <<~ ExtractCore.list @@ var "listTerm" $
  -- Fold over elements from right to left, building up Either [results]
  -- foldr (\x acc -> bind (f x) (\y -> map (cons y) acc)) (Right []) xs
  produce $ Lists.foldl
    -- Accumulator function: acc -> el -> Either err [results]
    ("acc" ~> "el" ~>
      -- First apply funTerm to element: funTerm el
      -- Then bind the result to check if Left or Right
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termFunction $ Core.functionPrimitive $ encodedName _eithers_either)
            -- If Left: return the Left unchanged (short-circuit)
            (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "err") nothing $
              Core.termEither $ left $ Core.termVariable $ wrap _Name $ string "err"))
          -- If Right: check acc, if acc is Right then cons, else return acc's Left
          (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "y") nothing $
            Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termApplication $ Core.application
                  (Core.termFunction $ Core.functionPrimitive $ encodedName _eithers_either)
                  -- If acc is Left: return it
                  (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "accErr") nothing $
                    Core.termEither $ left $ Core.termVariable $ wrap _Name $ string "accErr"))
                -- If acc is Right: cons y onto the list using primitive
                (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "ys") nothing $
                  Core.termEither $ right $
                    Core.termApplication $ Core.application
                      (Core.termApplication $ Core.application
                        (Core.termFunction $ Core.functionPrimitive $ encodedName _lists_cons)
                        (Core.termVariable $ wrap _Name $ string "y"))
                      (Core.termVariable $ wrap _Name $ string "ys")))
              (var "acc")))
        (Core.termApplication $ Core.application (var "funTerm") (var "el")))
    -- Initial accumulator: Right []
    (Core.termEither $ right $ Core.termList $ list ([] :: [TTerm Term]))
    (var "elements")

-- | Interpreter-friendly mapMaybe for Either (traverse over Maybe).
-- mapMaybe funTerm maybeTerm: if Just, applies funTerm to the value.
mapMaybe_ :: TBinding (Term -> Term -> Flow s Term)
mapMaybe_ = define "mapMaybe" $
  doc "Interpreter-friendly mapMaybe for Either (traverse over Maybe)." $
  "funTerm" ~> "maybeTerm" ~>
  cases _Term (var "maybeTerm")
    (Just (Monads.unexpected @@ string "maybe value" @@ (ShowCore.term @@ var "maybeTerm"))) [
    _Term_maybe>>: "opt" ~>
      produce $ Maybes.maybe
        -- Nothing: return Right Nothing
        (Core.termEither $ right $ Core.termMaybe nothing)
        -- Just val: apply funTerm, wrap result in Just
        ("val" ~>
          Core.termApplication $ Core.application
            (Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termFunction $ Core.functionPrimitive $ encodedName _eithers_either)
                (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "err") nothing $
                  Core.termEither $ left $ Core.termVariable $ wrap _Name $ string "err"))
              (Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "y") nothing $
                Core.termEither $ right $ Core.termMaybe $ just $ Core.termVariable $ wrap _Name $ string "y"))
            (Core.termApplication $ Core.application (var "funTerm") (var "val")))
        (var "opt")]

