-- | Primitive declarations for the hydra.lib.optionals namespace.
{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Lib.Optionals where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import           Hydra.Dsl.Meta.Phantoms     as Phantoms hiding (apply, cases, compose, map)
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), map, pure)


ns :: ModuleName
ns = ModuleName "hydra.lib.optionals"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.optionals module.")}
  where
    definitions = [apply, bind, cases, cat, compose, fromOptional, isGiven, isNone, map, mapOptional, pure, toList]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

defineWithDefault :: String -> String -> TermSignature -> [String] -> TypedTerm a -> PrimitiveDefinition
defineWithDefault = primitiveWithDefaultInModule module_

-- Shared type variables
tx, ty, tz :: Type
tx = Types.var "x"
ty = Types.var "y"
tz = Types.var "z"

apply :: PrimitiveDefinition
apply = defineWithDefault "apply" "Applicative apply for optionals: combine an optional function and an optional argument."
  (sig $ TypeScheme [Name "x", Name "y"]
    (Types.optional (tx Types.~> ty) Types.~> Types.optional tx Types.~> Types.optional ty)
    Nothing)
  ["apply(mf, mx) returns given(f x) when mf is given(f) and mx is given(x), and none if either is\
  \ none.",
   "The applicative apply for optionals; threads a function-in-context with a value-in-context.",
   "Total. Corresponds to Haskell's (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b."]
  ("mf" ~> "mx" ~> Optionals.bind (var "mf")
    ("f" ~> Optionals.map ("x" ~> var "f" @@ var "x") (var "mx")))

bind :: PrimitiveDefinition
bind = defineWithDefault "bind" "Monadic bind for optionals."
  (sig $ TypeScheme [Name "x", Name "y"]
    (Types.optional tx Types.~> (tx Types.~> Types.optional ty) Types.~> Types.optional ty)
    Nothing)
  ["bind(m, f) returns f(x) when m is given(x), and none when m is none.",
   "The monadic bind for optionals; used to chain computations that may be absent.",
   "Total. Corresponds to Haskell's (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b."]
  ("m" ~> "f" ~> Optionals.cases (var "m") nothing (var "f"))

-- The nothing-case value (position 1) is lazy: it is only evaluated when the optional is empty.
cases :: PrimitiveDefinition
cases = define "cases" "Case analysis on an optional, with cases-style argument order."
  (lazySig [1] $ TypeScheme [Name "x", Name "y"]
    (Types.optional tx Types.~> ty Types.~> (tx Types.~> ty) Types.~> ty)
    Nothing)
  ["cases(m, def, f) returns f(x) when m is given(x), and def when m is none.",
   "The fundamental eliminator for the optional type; every other primitive in this namespace can be\
   \ derived from it. The optional value is the first argument, matching the convention for\
   \ case-statement-like elimination.",
   "Total. Argument order is (m, def, f) rather than Haskell's maybe :: (def, f, m)."]

cat :: PrimitiveDefinition
cat = defineWithDefault "cat" "Concatenate optionals, keeping only the present values."
  (sig $ TypeScheme [Name "x"]
    (Types.list (Types.optional tx) Types.~> Types.list tx)
    Nothing)
  ["cat(xs) returns the list of contained values from given elements of xs, in original order; none\
  \ elements are discarded.",
   "Total. Corresponds to Haskell's Data.Maybe.catMaybes :: [Maybe a] -> [a]."]
  ("xs" ~> Lists.foldr
    ("m" ~> "acc" ~> Optionals.cases (var "m")
      (var "acc" :: TypedTerm [a])
      ("v" ~> Lists.cons (var "v") (var "acc")))
    (list ([] :: [TypedTerm a]))
    (var "xs"))

compose :: PrimitiveDefinition
compose = defineWithDefault "compose" "Kleisli composition for optionals."
  (sig $ TypeScheme [Name "x", Name "y", Name "z"]
    ((tx Types.~> Types.optional ty) Types.~> (ty Types.~> Types.optional tz) Types.~> tx Types.~> Types.optional tz)
    Nothing)
  ["compose(f, g, x) returns the Kleisli composition of f and g applied to x: bind(f(x), g).",
   "If either f or the second stage produces none, the result is none.",
   "Total. Corresponds to Haskell's Kleisli composition for Maybe, (>=>) :: (a -> Maybe b) -> (b ->\
   \ Maybe c) -> a -> Maybe c."]
  ("f" ~> "g" ~> "x" ~> Optionals.bind (var "f" @@ var "x") (var "g"))

-- The default value (position 0) is lazy: it is only evaluated when the optional is empty.
fromOptional :: PrimitiveDefinition
fromOptional = defineWithDefault "fromOptional" "Return the value contained in an optional, falling back to a default if absent."
  (lazySig [0] $ TypeScheme [Name "x"]
    (tx Types.~> Types.optional tx Types.~> tx)
    Nothing)
  ["fromOptional(def, m) returns x when m is given(x), and def when m is none.",
   "Total. Corresponds to Haskell's Data.Maybe.fromMaybe :: a -> Maybe a -> a."]
  ("def" ~> "m" ~> Optionals.cases (var "m") (var "def" :: TypedTerm a) ("x" ~> var "x"))

isGiven :: PrimitiveDefinition
isGiven = defineWithDefault "isGiven" "Test whether an optional is present (given)."
  (sig $ TypeScheme [Name "x"] (Types.optional tx Types.~> Types.boolean) Nothing)
  ["isGiven(m) returns true iff m is a given variant.",
   "Total. Corresponds to Haskell's Data.Maybe.isJust :: Maybe a -> Bool."]
  ("m" ~> Optionals.cases (var "m") false ("_" ~> true))

isNone :: PrimitiveDefinition
isNone = defineWithDefault "isNone" "Test whether an optional is absent (none)."
  (sig $ TypeScheme [Name "x"] (Types.optional tx Types.~> Types.boolean) Nothing)
  ["isNone(m) returns true iff m is the none variant.",
   "Total. Corresponds to Haskell's Data.Maybe.isNothing :: Maybe a -> Bool."]
  ("m" ~> Optionals.cases (var "m") true ("_" ~> false))

map :: PrimitiveDefinition
map = defineWithDefault "map" "Map a function over an optional."
  (sig $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> ty) Types.~> Types.optional tx Types.~> Types.optional ty)
    Nothing)
  ["map(f, m) returns given(f x) when m is given(x), and none when m is none.",
   "The functor instance for optionals.",
   "Total. Corresponds to Haskell's fmap :: (a -> b) -> Maybe a -> Maybe b."]
  ("f" ~> "m" ~> Optionals.cases (var "m") nothing ("x" ~> just (var "f" @@ var "x")))

mapOptional :: PrimitiveDefinition
mapOptional = defineWithDefault "mapOptional" "Map a partial function over a list, keeping only the present results."
  (sig $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> Types.optional ty) Types.~> Types.list tx Types.~> Types.list ty)
    Nothing)
  ["mapOptional(f, xs) applies f to each element of xs and returns the list of contained values from given\
  \ results in original order; none results are discarded.",
   "Total. Corresponds to Haskell's Data.Maybe.mapMaybe :: (a -> Maybe b) -> [a] -> [b]."]
  ("f" ~> "xs" ~> Optionals.cat (Lists.map (var "f") (var "xs")))

pure :: PrimitiveDefinition
pure = defineWithDefault "pure" "Wrap a value in given."
  (sig $ TypeScheme [Name "x"] (tx Types.~> Types.optional tx) Nothing)
  ["pure(x) = given(x). The applicative pure for optionals.",
   "Total. Corresponds to Haskell's pure :: a -> Maybe a / Just."]
  ("x" ~> just (var "x"))

toList :: PrimitiveDefinition
toList = defineWithDefault "toList" "Convert an optional to a list: given x maps to [x], none to []."
  (sig $ TypeScheme [Name "x"] (Types.optional tx Types.~> Types.list tx) Nothing)
  ["toList(m) returns [x] when m is given(x), and the empty list when m is none.",
   "Total. Corresponds to Haskell's Data.Maybe.maybeToList :: Maybe a -> [a]."]
  ("m" ~> Optionals.cases (var "m")
    (list ([] :: [TypedTerm a]))
    ("x" ~> list [var "x"]))
