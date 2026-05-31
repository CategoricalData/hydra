-- | Primitive declarations for the hydra.lib.maybes namespace.

module Hydra.Sources.Kernel.Lib.Maybes where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.lib.maybes"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.maybes namespace."}
  where
    definitions = [
      toPrimitive "Applicative apply for maybes: combine a maybe function and a maybe argument." applySig (Just
        "apply(mf, mx) returns Just(f x) when mf is Just(f) and mx is Just(x), and Nothing if either is\
        \ Nothing. The applicative apply for maybe; threads a function-in-context with a value-in-context.\
        \ Total. Corresponds to Haskell's (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b.") apply_,
      toPrimitive "Monadic bind for maybes." bindSig (Just
        "bind(m, f) returns f(x) when m is Just(x), and Nothing when m is Nothing. The monadic bind for\
        \ maybe; used to chain computations that may be absent. Total. Corresponds to Haskell's\
        \ (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b.") bind_,
      primNoDef "cases" "Case analysis on a maybe, with cases-style argument order." casesSig (Just
        "cases(m, def, f) returns f(x) when m is Just(x), and def when m is Nothing. Identical in behavior\
        \ to the maybe primitive but with the maybe value as the first argument (matching the convention\
        \ for case-statement-like elimination). Total. Argument order is (m, def, f) rather than\
        \ Haskell's (def, f, m)."),
      toPrimitive "Concatenate maybes, keeping only the present values." catSig (Just
        "cat(xs) returns the list of contained values from Just elements of xs, in original order;\
        \ Nothing elements are discarded. Total. Corresponds to Haskell's\
        \ Data.Maybe.catMaybes :: [Maybe a] -> [a].") cat_,
      toPrimitive "Kleisli composition for maybes." composeSig (Just
        "compose(f, g, x) returns the Kleisli composition of f and g applied to x: bind(f(x), g). If\
        \ either f or the second stage produces Nothing, the result is Nothing. Total. Corresponds to\
        \ Haskell's Kleisli composition for Maybe, (>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> a ->\
        \ Maybe c.") compose_,
      toPrimitive "Return the value contained in a maybe, falling back to a default if absent." fromMaybeSig (Just
        "fromMaybe(def, m) returns x when m is Just(x), and def when m is Nothing. Total. Corresponds to\
        \ Haskell's Data.Maybe.fromMaybe :: a -> Maybe a -> a.") fromMaybe_,
      toPrimitive "Test whether a maybe is present (Just)." isJustSig (Just
        "isJust(m) returns true iff m is a Just variant. Total. Corresponds to Haskell's\
        \ Data.Maybe.isJust :: Maybe a -> Bool.") isJust_,
      toPrimitive "Test whether a maybe is absent (Nothing)." isNothingSig (Just
        "isNothing(m) returns true iff m is the Nothing variant. Total. Corresponds to Haskell's\
        \ Data.Maybe.isNothing :: Maybe a -> Bool.") isNothing_,
      toPrimitive "Map a function over a maybe." mapSig (Just
        "map(f, m) returns Just(f x) when m is Just(x), and Nothing when m is Nothing. The functor\
        \ instance for maybe. Total. Corresponds to Haskell's fmap :: (a -> b) -> Maybe a -> Maybe b.") map_,
      toPrimitive "Map a partial function over a list, keeping only the present results." mapMaybeSig (Just
        "mapMaybe(f, xs) applies f to each element of xs and returns the list of contained values from\
        \ Just results in original order; Nothing results are discarded. Total. Corresponds to Haskell's\
        \ Data.Maybe.mapMaybe :: (a -> Maybe b) -> [a] -> [b].") mapMaybe_,
      primNoDef "maybe" "Case analysis on a maybe, applying a function if present or returning a default if absent." maybeSig (Just
        "maybe(def, f, m) returns f(x) when m is Just(x), and def when m is Nothing. The fundamental\
        \ eliminator for the maybe type; every other primitive in this namespace can be derived from it.\
        \ Total. Corresponds to Haskell's maybe :: b -> (a -> b) -> Maybe a -> b."),
      toPrimitive "Wrap a value in Just." pureSig (Just
        "pure(x) = Just(x). The applicative pure for maybe. Total. Corresponds to Haskell's\
        \ pure :: a -> Maybe a / Just.") pure_,
      toPrimitive "Convert a maybe to a list: Just x maps to [x], Nothing to []." toListSig (Just
        "toList(m) returns [x] when m is Just(x), and the empty list when m is Nothing. Total.\
        \ Corresponds to Haskell's Data.Maybe.maybeToList :: Maybe a -> [a].") toList_]
primNoDef :: String -> String -> TermSignature -> Maybe String -> Definition
primNoDef localName description s comments =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName)) comments

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

-- Build a TermSignature, marking the value parameters at the given (0-based)
-- positions as lazy (thunked by coders that distinguish strict from lazy
-- evaluation).
lazySig :: [Int] -> TypeScheme -> TermSignature
lazySig idxs ts = markLazyParams idxs (sig ts)

markLazyParams :: [Int] -> TermSignature -> TermSignature
markLazyParams idxs ts = ts {
  termSignatureParameters =
    zipWith (\i p -> if i `elem` idxs then p {parameterIsLazy = True} else p)
      [0..] (termSignatureParameters ts)}

-- Signatures.

-- apply : forall a b. Maybe (a -> b) -> Maybe a -> Maybe b
applySig :: TermSignature
applySig = sig $ TypeScheme [Name "x", Name "y"]
  (Types.optional (Types.var "x" Types.~> Types.var "y") Types.~>
   Types.optional (Types.var "x") Types.~>
   Types.optional (Types.var "y"))
  Nothing

-- bind : forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
bindSig :: TermSignature
bindSig = sig $ TypeScheme [Name "x", Name "y"]
  (Types.optional (Types.var "x") Types.~>
   (Types.var "x" Types.~> Types.optional (Types.var "y")) Types.~>
   Types.optional (Types.var "y"))
  Nothing

-- cases : forall a b. Maybe a -> b -> (a -> b) -> b
-- The nothing-case value (position 1) is lazy: it is only evaluated when the
-- optional is empty.
casesSig :: TermSignature
casesSig = lazySig [1] $ TypeScheme [Name "x", Name "y"]
  (Types.optional (Types.var "x") Types.~>
   Types.var "y" Types.~>
   (Types.var "x" Types.~> Types.var "y") Types.~>
   Types.var "y")
  Nothing

-- cat : forall a. [Maybe a] -> [a]
catSig :: TermSignature
catSig = sig $ TypeScheme [Name "x"]
  (Types.list (Types.optional (Types.var "x")) Types.~> Types.list (Types.var "x"))
  Nothing

-- compose : forall a b c. (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
composeSig :: TermSignature
composeSig = sig $ TypeScheme [Name "x", Name "y", Name "z"]
  ((Types.var "x" Types.~> Types.optional (Types.var "y")) Types.~>
   (Types.var "y" Types.~> Types.optional (Types.var "z")) Types.~>
   Types.var "x" Types.~>
   Types.optional (Types.var "z"))
  Nothing

-- fromMaybe : forall a. a -> Maybe a -> a
-- The default value (position 0) is lazy: it is only evaluated when the
-- optional is empty.
fromMaybeSig :: TermSignature
fromMaybeSig = lazySig [0] $ TypeScheme [Name "x"]
  (Types.var "x" Types.~> Types.optional (Types.var "x") Types.~> Types.var "x")
  Nothing

-- isJust / isNothing : forall a. Maybe a -> Boolean
isJustSig :: TermSignature
isJustSig = sig $ TypeScheme [Name "x"]
  (Types.optional (Types.var "x") Types.~> Types.boolean)
  Nothing

isNothingSig :: TermSignature
isNothingSig = isJustSig

-- mapMaybe : forall a b. (a -> Maybe b) -> [a] -> [b]
mapMaybeSig :: TermSignature
mapMaybeSig = sig $ TypeScheme [Name "x", Name "y"]
  ((Types.var "x" Types.~> Types.optional (Types.var "y")) Types.~>
   Types.list (Types.var "x") Types.~>
   Types.list (Types.var "y"))
  Nothing

-- map : forall a b. (a -> b) -> Maybe a -> Maybe b
mapSig :: TermSignature
mapSig = sig $ TypeScheme [Name "x", Name "y"]
  ((Types.var "x" Types.~> Types.var "y") Types.~>
   Types.optional (Types.var "x") Types.~>
   Types.optional (Types.var "y"))
  Nothing

-- maybe : forall a b. b -> (a -> b) -> Maybe a -> b
-- The default value (position 0) is lazy: it is only evaluated when the
-- optional is empty.
maybeSig :: TermSignature
maybeSig = lazySig [0] $ TypeScheme [Name "y", Name "x"]
  (Types.var "y" Types.~>
   (Types.var "x" Types.~> Types.var "y") Types.~>
   Types.optional (Types.var "x") Types.~>
   Types.var "y")
  Nothing

-- pure : forall a. a -> Maybe a
pureSig :: TermSignature
pureSig = sig $ TypeScheme [Name "x"]
  (Types.var "x" Types.~> Types.optional (Types.var "x"))
  Nothing

-- toList : forall a. Maybe a -> [a]
toListSig :: TermSignature
toListSig = sig $ TypeScheme [Name "x"]
  (Types.optional (Types.var "x") Types.~> Types.list (Types.var "x"))
  Nothing

-- Default implementations.

-- apply mf mx = bind mf (\f -> map (\x -> f x) mx)
apply_ :: TypedTermDefinition (Maybe (a -> b) -> Maybe a -> Maybe b)
apply_ = define "apply" $
  doc "Applicative apply for optionals, defined in terms of bind and map." $
  "mf" ~> "mx" ~> Maybes.bind (var "mf")
    ("f" ~> Maybes.map ("x" ~> var "f" @@ var "x") (var "mx"))

-- bind m f = maybe Nothing f m
bind_ :: TypedTermDefinition (Maybe a -> (a -> Maybe b) -> Maybe b)
bind_ = define "bind" $
  doc "Monadic bind for optionals, defined in terms of maybe." $
  "m" ~> "f" ~> Maybes.maybe nothing (var "f") (var "m")

-- cat xs = foldr (\m acc -> maybe acc (\v -> v : acc) m) [] xs
cat_ :: TypedTermDefinition ([Maybe a] -> [a])
cat_ = define "cat" $
  doc "Catenate a list of optionals, keeping only the present values." $
  "xs" ~> Lists.foldr
    ("m" ~> "acc" ~> Maybes.maybe (var "acc" :: TypedTerm [a])
      ("v" ~> Lists.cons (var "v") (var "acc"))
      (var "m"))
    (list ([] :: [TypedTerm a]))
    (var "xs")

-- compose f g x = bind (f x) g
compose_ :: TypedTermDefinition ((a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c)
compose_ = define "compose" $
  doc "Kleisli composition for optionals, defined in terms of bind." $
  "f" ~> "g" ~> "x" ~> Maybes.bind (var "f" @@ var "x") (var "g")

-- fromMaybe def m = maybe def (\x -> x) m
fromMaybe_ :: TypedTermDefinition (a -> Maybe a -> a)
fromMaybe_ = define "fromMaybe" $
  doc "Return the contained value or a default, defined in terms of maybe." $
  "def" ~> "m" ~> Maybes.maybe (var "def" :: TypedTerm a) ("x" ~> var "x") (var "m")

-- isJust m = maybe false (\_ -> true) m
isJust_ :: TypedTermDefinition (Maybe a -> Bool)
isJust_ = define "isJust" $
  doc "Test for presence, defined in terms of maybe." $
  "m" ~> Maybes.maybe false ("_" ~> true) (var "m")

-- isNothing m = maybe true (\_ -> false) m
isNothing_ :: TypedTermDefinition (Maybe a -> Bool)
isNothing_ = define "isNothing" $
  doc "Test for absence, defined in terms of maybe." $
  "m" ~> Maybes.maybe true ("_" ~> false) (var "m")

-- mapMaybe f xs = cat (Lists.map f xs)
mapMaybe_ :: TypedTermDefinition ((a -> Maybe b) -> [a] -> [b])
mapMaybe_ = define "mapMaybe" $
  doc "Map a partial function and keep only the present results, defined in terms of lists.map and cat." $
  "f" ~> "xs" ~> Maybes.cat (Lists.map (var "f") (var "xs"))

-- map f m = maybe Nothing (\x -> Just (f x)) m
map_ :: TypedTermDefinition ((a -> b) -> Maybe a -> Maybe b)
map_ = define "map" $
  doc "Map a function over an optional, defined in terms of maybe." $
  "f" ~> "m" ~> Maybes.maybe nothing ("x" ~> just (var "f" @@ var "x")) (var "m")

-- pure x = Just x
pure_ :: TypedTermDefinition (a -> Maybe a)
pure_ = define "pure" $
  doc "Wrap a value in Just." $
  "x" ~> just (var "x")

-- toList m = maybe [] (\x -> [x]) m
toList_ :: TypedTermDefinition (Maybe a -> [a])
toList_ = define "toList" $
  doc "Convert an optional to a list, defined in terms of maybe." $
  "m" ~> Maybes.maybe (list ([] :: [TypedTerm a]))
    ("x" ~> list [var "x"])
    (var "m")
