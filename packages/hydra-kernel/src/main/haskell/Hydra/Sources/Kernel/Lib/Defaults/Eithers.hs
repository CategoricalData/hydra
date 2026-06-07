
module Hydra.Sources.Kernel.Lib.Defaults.Eithers where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (either)
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
import           Prelude hiding ((++), either)
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore

ns :: ModuleName
ns = ModuleName "hydra.lib.defaults.eithers"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([ExtractCore.ns, ShowCore.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just ("Default term-level implementations of Either functions for the Hydra interpreter."))}
  where
    definitions = [
      toDefinition either_,
      toDefinition foldl_,
      toDefinition lefts_,
      toDefinition mapList_,
      toDefinition mapMaybe_,
      toDefinition mapSet_,
      toDefinition partitionEithers_,
      toDefinition rights_]

-- | Interpreter-friendly case analysis for Either terms.
-- Takes two function terms and an Either term, applies the appropriate function
-- to the contained value.
either_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Term -> Term -> Either Error Term)
either_ = define "either" $
  doc "Interpreter-friendly case analysis for Either terms." $
  "cx" ~> "g" ~>
  "leftFun" ~> "rightFun" ~> "eitherTerm" ~>
  cases _Term (var "eitherTerm")
    (Just (ExtractCore.unexpected (string "either value") (ShowCore.term @@ var "eitherTerm"))) [
    _Term_either>>: "e" ~>
      right $ Eithers.either_
        ("val" ~> Core.termApplication $ Core.application (var "leftFun") (var "val"))
        ("val" ~> Core.termApplication $ Core.application (var "rightFun") (var "val"))
        (var "e")]

-- | Interpreter-friendly foldl for Either.
-- foldl funTerm initTerm listTerm: folds funTerm over elements of listTerm,
-- threading an accumulator starting from initTerm. Short-circuits on first Left.
foldl_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Term -> Term -> Either Error Term)
foldl_ = define "foldl" $
  doc "Interpreter-friendly foldl for Either." $
  "cx" ~> "g" ~>
  "funTerm" ~> "initTerm" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
  -- Fold: for each element, apply funTerm acc el, then bind to check Left/Right
  right $ Lists.foldl
    ("acc" ~> "el" ~>
      -- bind acc (\a -> funTerm a el)
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termVariable $ encodedName _eithers_either)
            -- If acc is Left: short-circuit
            (Core.termLambda $ Core.lambda (wrap _Name $ string "err") nothing $
              Core.termEither $ left $ Core.termVariable $ wrap _Name $ string "err"))
          -- If acc is Right: apply funTerm to acc value and element
          (Core.termLambda $ Core.lambda (wrap _Name $ string "a") nothing $
            Core.termApplication $ Core.application
              (Core.termApplication $ Core.application (var "funTerm") (Core.termVariable $ wrap _Name $ string "a"))
              (var "el")))
        (var "acc"))
    -- Initial accumulator: Right initTerm
    (Core.termEither $ right $ var "initTerm")
    (var "elements")

-- | Interpreter-friendly lefts for list of Either terms.
-- Extracts all Left values from a list.
lefts_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Either Error Term)
lefts_ = define "lefts" $
  doc "Interpreter-friendly lefts for list of Either terms." $
  "cx" ~> "g" ~>
  "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
  right $ Core.termList $ Lists.foldl
    ("acc" ~> "el" ~>
      cases _Term (var "el")
        (Just (var "acc")) [
        _Term_either>>: "e" ~>
          Eithers.either_
            ("val" ~> Lists.concat2 (var "acc") (Lists.pure (var "val")))
            ("_" ~> var "acc")
            (var "e")])
    (list ([] :: [TypedTerm Term]))
    (var "elements")

-- | Interpreter-friendly mapList for Either (traverse).
-- mapList funTerm listTerm: applies funTerm to each element, collecting results.
-- Short-circuits on first Left error.
mapList_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Term -> Either Error Term)
mapList_ = define "mapList" $
  doc "Interpreter-friendly mapList for Either (traverse)." $
  "cx" ~> "g" ~>
  "funTerm" ~> "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
  -- Fold over reversed elements so that cons builds list in original order
  -- foldl (\acc el -> bind (f el) (\y -> map (cons y) acc)) (Right []) (reverse xs)
  right $ Lists.foldl
    -- Accumulator function: acc -> el -> Either err [results]
    ("acc" ~> "el" ~>
      -- First apply funTerm to element: funTerm el
      -- Then bind the result to check if Left or Right
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termVariable $ encodedName _eithers_either)
            -- If Left: return the Left unchanged (short-circuit)
            (Core.termLambda $ Core.lambda (wrap _Name $ string "err") nothing $
              Core.termEither $ left $ Core.termVariable $ wrap _Name $ string "err"))
          -- If Right: check acc, if acc is Right then cons, else return acc's Left
          (Core.termLambda $ Core.lambda (wrap _Name $ string "y") nothing $
            Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termApplication $ Core.application
                  (Core.termVariable $ encodedName _eithers_either)
                  -- If acc is Left: return it
                  (Core.termLambda $ Core.lambda (wrap _Name $ string "accErr") nothing $
                    Core.termEither $ left $ Core.termVariable $ wrap _Name $ string "accErr"))
                -- If acc is Right: cons y onto the list using primitive
                (Core.termLambda $ Core.lambda (wrap _Name $ string "ys") nothing $
                  Core.termEither $ right $
                    Core.termApplication $ Core.application
                      (Core.termApplication $ Core.application
                        (Core.termVariable $ encodedName _lists_cons)
                        (Core.termVariable $ wrap _Name $ string "y"))
                      (Core.termVariable $ wrap _Name $ string "ys")))
              (var "acc")))
        (Core.termApplication $ Core.application (var "funTerm") (var "el")))
    -- Initial accumulator: Right []
    (Core.termEither $ right $ Core.termList $ list ([] :: [TypedTerm Term]))
    -- Reverse elements so foldl with cons builds list in original order
    (Lists.reverse $ var "elements")

-- | Interpreter-friendly mapMaybe for Either (traverse over Maybe).
-- mapMaybe funTerm maybeTerm: if Just, applies funTerm to the value.
mapMaybe_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Term -> Either Error Term)
mapMaybe_ = define "mapMaybe" $
  doc "Interpreter-friendly mapMaybe for Either (traverse over Maybe)." $
  "cx" ~> "g" ~>
  "funTerm" ~> "maybeTerm" ~>
  cases _Term (var "maybeTerm")
    (Just (ExtractCore.unexpected (string "maybe value") (ShowCore.term @@ var "maybeTerm"))) [
    _Term_maybe>>: "opt" ~>
      right $ Maybes.maybe
        -- Nothing: return Right Nothing
        (Core.termEither $ right $ Core.termMaybe nothing)
        -- Just val: apply funTerm, wrap result in Just
        ("val" ~>
          Core.termApplication $ Core.application
            (Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termVariable $ encodedName _eithers_either)
                (Core.termLambda $ Core.lambda (wrap _Name $ string "err") nothing $
                  Core.termEither $ left $ Core.termVariable $ wrap _Name $ string "err"))
              (Core.termLambda $ Core.lambda (wrap _Name $ string "y") nothing $
                Core.termEither $ right $ Core.termMaybe $ just $ Core.termVariable $ wrap _Name $ string "y"))
            (Core.termApplication $ Core.application (var "funTerm") (var "val")))
        (var "opt")]

-- | Interpreter-friendly mapSet for Either (traverse over Set).
-- mapSet funTerm setTerm: applies funTerm to each element, collecting results as a set.
-- Short-circuits on first Left error.
mapSet_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Term -> Either Error Term)
mapSet_ = define "mapSet" $
  doc "Interpreter-friendly mapSet for Either (traverse over Set)." $
  "cx" ~> "g" ~>
  "funTerm" ~> "setTerm" ~>
  "elements" <<~ (ExtractCore.set @@ var "g" @@ var "setTerm") $
  -- Convert set to list, apply mapList logic, convert back to set
  -- Fold over elements: foldl (\acc el -> either (\e -> Left e) (\y -> either (\e -> Left e) (\ys -> Right (cons y ys)) acc) (f el)) (Right []) elements
  -- Then wrap result in Set
  right $ Lists.foldl
    ("acc" ~> "el" ~>
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termVariable $ encodedName _eithers_either)
            -- If Left: return the Left unchanged (short-circuit)
            (Core.termLambda $ Core.lambda (wrap _Name $ string "err") nothing $
              Core.termEither $ left $ Core.termVariable $ wrap _Name $ string "err"))
          -- If Right: check acc, if acc is Right then cons, else return acc's Left
          (Core.termLambda $ Core.lambda (wrap _Name $ string "y") nothing $
            Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termApplication $ Core.application
                  (Core.termVariable $ encodedName _eithers_either)
                  -- If acc is Left: return it
                  (Core.termLambda $ Core.lambda (wrap _Name $ string "accErr") nothing $
                    Core.termEither $ left $ Core.termVariable $ wrap _Name $ string "accErr"))
                -- If acc is Right: insert y into the set using primitive
                (Core.termLambda $ Core.lambda (wrap _Name $ string "ys") nothing $
                  Core.termEither $ right $
                    Core.termApplication $ Core.application
                      (Core.termApplication $ Core.application
                        (Core.termVariable $ encodedName _sets_insert)
                        (Core.termVariable $ wrap _Name $ string "y"))
                      (Core.termVariable $ wrap _Name $ string "ys")))
              (var "acc")))
        (Core.termApplication $ Core.application (var "funTerm") (var "el")))
    -- Initial accumulator: Right (empty set)
    (Core.termEither $ right $ Core.termSet $ Sets.fromList (list ([] :: [TypedTerm Term])))
    -- Convert set elements to list for folding
    (Sets.toList $ var "elements")

-- | Interpreter-friendly partitionEithers for list of Either terms.
-- Splits a list of Eithers into (lefts, rights).
partitionEithers_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Either Error Term)
partitionEithers_ = define "partitionEithers" $
  doc "Interpreter-friendly partitionEithers for list of Either terms." $
  "cx" ~> "g" ~>
  "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
  right $ Lists.foldl
    ("acc" ~> "el" ~>
      "ls" <~ Pairs.first (var "acc") $
      "rs" <~ Pairs.second (var "acc") $
      cases _Term (var "el")
        (Just (var "acc")) [
        _Term_either>>: "e" ~>
          Eithers.either_
            ("val" ~> pair (Lists.concat2 (var "ls") (Lists.pure (var "val"))) (var "rs"))
            ("val" ~> pair (var "ls") (Lists.concat2 (var "rs") (Lists.pure (var "val"))))
            (var "e")])
    (pair (list ([] :: [TypedTerm Term])) (list ([] :: [TypedTerm Term])))
    (var "elements")

-- | Interpreter-friendly rights for list of Either terms.
-- Extracts all Right values from a list.
rights_ :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Either Error Term)
rights_ = define "rights" $
  doc "Interpreter-friendly rights for list of Either terms." $
  "cx" ~> "g" ~>
  "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "g" @@ var "listTerm") $
  right $ Core.termList $ Lists.foldl
    ("acc" ~> "el" ~>
      cases _Term (var "el")
        (Just (var "acc")) [
        _Term_either>>: "e" ~>
          Eithers.either_
            ("_" ~> var "acc")
            ("val" ~> Lists.concat2 (var "acc") (Lists.pure (var "val")))
            (var "e")])
    (list ([] :: [TypedTerm Term]))
    (var "elements")
