-- | Primitive declarations for the hydra.lib.eithers namespace.
{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Lib.Eithers where

import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms hiding (apply, compose, map)
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), either, foldl, map, pure)
import qualified Data.Set                    as S


ns :: ModuleName
ns = ModuleName "hydra.lib.eithers"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.eithers module.")}
  where
    -- The mapSet default-impl arg below carries an `:: ...Int...` placeholder instantiation: the
    -- generated `Hydra.Dsl.Lib.Sets` (unlike the old `Meta.Lib.Sets`) exposes the primitive's `Ord`
    -- constraint, so this polymorphic def needs a concrete type here to satisfy GHC. `Int` is arbitrary
    -- and carries no meaning — the emitted primitive is type-agnostic and fully polymorphic. See #467.
    definitions = [apply, bimap, bind, compose, either, foldList, isLeft,
                   isRight, lefts, map, mapList, mapOptional, mapSet, partition, pure,
                   rights]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

defineWithDefault :: String -> String -> TermSignature -> [String] -> TypedTerm a -> PrimitiveDefinition
defineWithDefault = primitiveWithDefaultInModule module_

-- Shared type variables
tx, ty, tz, tw :: Type
tx = Types.var "x"
ty = Types.var "y"
tz = Types.var "z"
tw = Types.var "w"

ee :: Type -> Type -> Type
ee = Types.either_

apply :: PrimitiveDefinition
apply = defineWithDefault "apply" "Applicative apply for either: combine a function under either and an argument under either."
  (sigWithParams [("ef", "the either containing the function to apply"), ("ex", "the either containing the argument")] $ TypeScheme [Name "x", Name "y", Name "z"]
    (ee tx (ty Types.~> tz) Types.~> ee tx ty Types.~> ee tx tz) Nothing)
  ["apply(ef, ex) returns right(f(x)) when ef is right(f) and ex is right(x).",
   "If either argument is a left, the result is that left; when both arguments are left, the first\
  \ (function-side) left is returned — first error wins, and ex is not consulted once ef is known to\
  \ be a left.",
   "apply(ef, ex) is bind(ef, \\f -> map(f, ex)); this defining equation is the specification, and the\
  \ default implementation.",
   "Total. Corresponds to Haskell's (<*>) :: Either a (b -> c) -> Either a b -> Either a c."]
  ("ef" ~> "ex" ~>
    Eithers.bind (var "ef") ("f" ~> Eithers.map (var "f") (var "ex")))

bimap :: PrimitiveDefinition
bimap = defineWithDefault "bimap" "Map over both sides of an either value."
  (sigWithParams [("f", "the function to apply to a Left value"), ("g", "the function to apply to a Right value"), ("e", "the either value to map over")] $ TypeScheme [Name "x", Name "y", Name "z", Name "w"]
    ((tx Types.~> tz) Types.~> (ty Types.~> tw) Types.~> ee tx ty Types.~> ee tz tw) Nothing)
  ["bimap(f, g, e) applies f to the contained value if e is a Left, or g if e is a Right; the result\
  \ retains the same Left/Right variant.",
   "Total. Corresponds to Haskell's Data.Bifunctor.bimap :: (a -> c) -> (b -> d) -> Either a b ->\
  \ Either c d."]
  ("f" ~> "g" ~> "e" ~>
    Eithers.either
      ("x" ~> left (var "f" @@ var "x"))
      ("y" ~> right (var "g" @@ var "y"))
      (var "e"))

bind :: PrimitiveDefinition
bind = defineWithDefault "bind" "Bind (flatMap) for either: if Right, apply the function; if Left, return unchanged."
  (sigWithParams [("e", "the either value to bind"), ("f", "the function to apply to a Right value")] $ TypeScheme [Name "x", Name "y", Name "z"]
    (ee tx ty Types.~> (ty Types.~> ee tx tz) Types.~> ee tx tz) Nothing)
  ["bind(e, f) is the monadic bind for either with a fixed Left type: if e is Right v, the result is\
  \ f(v); if e is Left x, the result is Left x with the Left type preserved.",
   "Used to chain computations that may fail with a common error type.",
   "Total. Corresponds to Haskell's (>>=) :: Either a b -> (b -> Either a c) -> Either a c."]
  ("e" ~> "f" ~>
    Eithers.either
      ("x" ~> left (var "x"))
      (var "f")
      (var "e"))

compose :: PrimitiveDefinition
compose = defineWithDefault "compose" "Kleisli composition for either."
  (sigWithParams [("f", "the first Kleisli arrow to apply"), ("g", "the second Kleisli arrow to apply"), ("x", "the input value")] $ TypeScheme [Name "x", Name "y", Name "z", Name "w"]
    ((tx Types.~> ee tw ty) Types.~> (ty Types.~> ee tw tz) Types.~> tx Types.~> ee tw tz) Nothing)
  ["compose(f, g, x) is bind(f(x), g); this defining equation is the specification, and the default\
  \ implementation.",
   "If either f(x) or the second stage produces a left, the result is that left.",
   "Total. Corresponds to Kleisli composition (>=>) specialised to Either with a fixed Left type."]
  ("f" ~> "g" ~> "x" ~>
    Eithers.bind (var "f" @@ var "x") (var "g"))

either :: PrimitiveDefinition
either = define "either" "Eliminate an either value by applying one of two functions."
  (sigWithParams [("f", "the function to apply to a Left value"), ("g", "the function to apply to a Right value"), ("e", "the either value to eliminate")] $ TypeScheme [Name "x", Name "y", Name "z"]
    ((tx Types.~> tz) Types.~> (ty Types.~> tz) Types.~> ee tx ty Types.~> tz) Nothing)
  ["either(f, g, e) returns f(x) if e is Left x and g(y) if e is Right y.",
   "The fundamental eliminator for the either type; every other primitive in this namespace can be\
  \ derived from it.",
   "Total. Corresponds to Haskell's either :: (a -> c) -> (b -> c) -> Either a b -> c."]

foldList :: PrimitiveDefinition
foldList = defineWithDefault "foldList" "Left-fold over a list with an Either-returning function, short-circuiting on Left."
  (sigWithParams [("f", "the Either-returning folding function"), ("acc0", "the initial accumulator value"), ("xs", "the list to fold over")] $ TypeScheme [Name "x", Name "y", Name "z"]
    ((tx Types.~> ty Types.~> ee tz tx) Types.~> tx Types.~> Types.list ty Types.~> ee tz tx) Nothing)
  ["foldList(f, acc0, xs) folds f over xs from the left, threading an accumulator of type a, where each\
  \ application may fail with Left e: it iterates while f returns Right, propagates Left on the first\
  \ failure, and returns Right (final accumulator) if all elements were processed.",
   "Corresponds to a short-circuiting variant of Haskell's foldM specialised to Either."]
  ("f" ~> "acc0" ~> "xs" ~>
    Lists.foldl
      ("acc" ~> "el" ~>
        Eithers.bind (var "acc") ("a" ~> var "f" @@ var "a" @@ var "el"))
      (right (var "acc0"))
      (var "xs"))

isLeft :: PrimitiveDefinition
isLeft = defineWithDefault "isLeft" "Check whether an either is a Left value."
  (sigWithParams [("e", "the either value to test")] $ TypeScheme [Name "x", Name "y"] (ee tx ty Types.~> Types.boolean) Nothing)
  ["True if the argument is a Left variant, false if a Right.",
   "Total. Corresponds to Haskell's Data.Either.isLeft :: Either a b -> Bool."]
  ("e" ~> Eithers.either ("_" ~> true) ("_" ~> false) (var "e"))

isRight :: PrimitiveDefinition
isRight = defineWithDefault "isRight" "Check whether an either is a Right value."
  (sigWithParams [("e", "the either value to test")] $ TypeScheme [Name "x", Name "y"] (ee tx ty Types.~> Types.boolean) Nothing)
  ["True if the argument is a Right variant, false if a Left.",
   "Total. Corresponds to Haskell's Data.Either.isRight :: Either a b -> Bool."]
  ("e" ~> Eithers.either ("_" ~> false) ("_" ~> true) (var "e"))

lefts :: PrimitiveDefinition
lefts = defineWithDefault "lefts" "Extract all Left values from a list of either values."
  (sigWithParams [("xs", "the list of either values to extract Lefts from")] $ TypeScheme [Name "x", Name "y"]
    (Types.list (ee tx ty) Types.~> Types.list tx) Nothing)
  ["lefts(xs) returns a list containing every Left value in xs, in original order, with Right values\
  \ discarded.",
   "Total. Corresponds to Haskell's Data.Either.lefts :: [Either a b] -> [a]."]
  ("xs" ~>
    Lists.foldr
      ("e" ~> "acc" ~>
        Eithers.either
          ("l" ~> Lists.cons (var "l") (var "acc" :: TypedTerm [a]))
          ("_" ~> (var "acc" :: TypedTerm [a]))
          (var "e"))
      (list ([] :: [TypedTerm a]))
      (var "xs"))

map :: PrimitiveDefinition
map = defineWithDefault "map" "Map a function over the Right side of an either (standard functor map)."
  (sigWithParams [("f", "the function to apply to a Right value"), ("e", "the either value to map over")] $ TypeScheme [Name "x", Name "y", Name "z"]
    ((tx Types.~> ty) Types.~> ee tz tx Types.~> ee tz ty) Nothing)
  ["map(f, e) returns Right (f y) if e is Right y, or Left x unchanged if e is Left x.",
   "The functor instance for either; treats the Right variant as the focus and leaves the Left variant\
  \ alone.",
   "Total. Corresponds to Haskell's fmap :: (a -> b) -> Either e a -> Either e b."]
  ("f" ~> "e" ~>
    Eithers.either
      ("x" ~> left (var "x"))
      ("y" ~> right (var "f" @@ var "y"))
      (var "e"))

mapList :: PrimitiveDefinition
mapList = defineWithDefault "mapList" "Map a function returning either over a list, collecting results or short-circuiting on Left."
  (sigWithParams [("f", "the either-returning function to apply to each element"), ("xs", "the list to map over")] $ TypeScheme [Name "x", Name "y", Name "z"]
    ((tx Types.~> ee tz ty) Types.~> Types.list tx Types.~> ee tz (Types.list ty)) Nothing)
  ["mapList(f, xs) applies f to each element of xs. If every application returns Right, the result is\
  \ Right of the list of contained values, in original order. The first application that returns Left\
  \ short-circuits the whole result to that Left.",
   "Total. Corresponds to Haskell's traverse :: (a -> Either e b) -> [a] -> Either e [b]."]
  ("f" ~> "xs" ~>
    Lists.foldr
      ("x" ~> "acc" ~>
        Eithers.bind (var "f" @@ var "x") $
          "y" ~> Eithers.map ("ys" ~> Lists.cons (var "y") (var "ys")) (var "acc"))
      (right (list ([] :: [TypedTerm b])))
      (var "xs"))

mapOptional :: PrimitiveDefinition
mapOptional = defineWithDefault "mapOptional" "Map a function returning either over an optional, or return Right none if none."
  (sigWithParams [("f", "the either-returning function to apply to the contained value"), ("m", "the optional to map over")] $ TypeScheme [Name "x", Name "y", Name "z"]
    ((tx Types.~> ee tz ty) Types.~> Types.optional tx Types.~> ee tz (Types.optional ty)) Nothing)
  ["mapOptional(f, m) returns Right none if m is none; otherwise applies f to the contained value\
  \ and returns the result with Right wrapped around given.",
   "Total. Corresponds to Haskell's traverse :: (a -> Either e b) -> Maybe a -> Either e (Maybe b)."]
  ("f" ~> "m" ~>
    Optionals.cases (var "m")
      (right nothing)
      ("x" ~> Eithers.map ("y" ~> just (var "y")) (var "f" @@ var "x")))

mapSet :: PrimitiveDefinition
mapSet = defineWithDefault "mapSet" "Map a function returning either over a set, collecting results or short-circuiting on Left."
  (sigWithParams [("f", "the either-returning function to apply to each element"), ("s", "the set to map over")] $ TypeScheme [Name "x", Name "y", Name "z"]
    ((tx Types.~> ee tz ty) Types.~> Types.set tx Types.~> ee tz (Types.set ty)) Nothing)
  ["mapSet(f, s) applies f to each element of s in unspecified order. If every application returns\
  \ Right, the result is Right of the set of contained values (deduplicated by the result type's\
  \ ordering); the first application returning Left short-circuits the whole result to that Left.",
   "Total. Corresponds to Haskell's traverse-style operation specialised to Set."]
  (("f" ~> "s" ~>
    Eithers.map
      ("ys" ~> (Sets.fromList (var "ys") :: TypedTerm (S.Set Int)))
      (Lists.foldr
        ("x" ~> "acc" ~>
          Eithers.bind (var "f" @@ var "x") $
            "y" ~> Eithers.map ("ys" ~> Lists.cons (var "y") (var "ys")) (var "acc"))
        (right (list ([] :: [TypedTerm Int])))
        (Sets.toList (var "s" :: TypedTerm (S.Set Int))))) :: TypedTerm ((Int -> Either Int Int) -> S.Set Int -> Either Int (S.Set Int)))

partition :: PrimitiveDefinition
partition = defineWithDefault "partition" "Partition a list of either values into lefts and rights."
  (sigWithParams [("xs", "the list of either values to partition")] $ TypeScheme [Name "x", Name "y"]
    (Types.list (ee tx ty) Types.~> Types.pair (Types.list tx) (Types.list ty)) Nothing)
  ["partition(xs) returns a pair (lefts, rights) where lefts contains every Left value from xs in\
  \ original order and rights contains every Right value from xs in original order.",
   "Total. Corresponds to Haskell's Data.Either.partitionEithers :: [Either a b] -> ([a], [b])."]
  ("xs" ~>
    Lists.foldr
      ("e" ~> "acc" ~>
        Eithers.either
          ("l" ~> pair (Lists.cons (var "l") (Pairs.first $ var "acc")) (Pairs.second $ var "acc"))
          ("r" ~> pair (Pairs.first $ var "acc") (Lists.cons (var "r") (Pairs.second $ var "acc")))
          (var "e"))
      (pair (list ([] :: [TypedTerm a])) (list ([] :: [TypedTerm b])))
      (var "xs"))

pure :: PrimitiveDefinition
pure = defineWithDefault "pure" "Wrap a value as a right."
  (sigWithParams [("x", "the value to wrap as a Right")] $ TypeScheme [Name "x", Name "y"] (ty Types.~> ee tx ty) Nothing)
  ["pure(x) is right(x); this defining equation is the specification, and the default implementation.",
   "This is the unit of the either monad; it exists so that code written generically over a monad can\
  \ reach the unit.",
   "Total. Corresponds to Haskell's pure / Right for Either."]
  ("x" ~> right (var "x"))

rights :: PrimitiveDefinition
rights = defineWithDefault "rights" "Extract all Right values from a list of either values."
  (sigWithParams [("xs", "the list of either values to extract Rights from")] $ TypeScheme [Name "x", Name "y"]
    (Types.list (ee tx ty) Types.~> Types.list ty) Nothing)
  ["rights(xs) returns a list containing every Right value in xs, in original order, with Left values\
  \ discarded.",
   "Total. Corresponds to Haskell's Data.Either.rights :: [Either a b] -> [b]."]
  ("xs" ~>
    Lists.foldr
      ("e" ~> "acc" ~>
        Eithers.either
          ("_" ~> (var "acc" :: TypedTerm [b]))
          ("r" ~> Lists.cons (var "r") (var "acc" :: TypedTerm [b]))
          (var "e"))
      (list ([] :: [TypedTerm b]))
      (var "xs"))
