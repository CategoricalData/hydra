-- | Primitive declarations for the hydra.lib.eithers namespace.

module Hydra.Sources.Kernel.Lib.Eithers where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Set                    as S


ns :: ModuleName
ns = ModuleName "hydra.lib.eithers"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.eithers module.")}
  where
    definitions = [
      toPrimitive "Map over both sides of an either value." bimapSig [
        "bimap(f, g, e) applies f to the contained value if e is a Left, or g if e is a Right; the result\
        \ retains the same Left/Right variant.",
        "Total. Corresponds to Haskell's Data.Bifunctor.bimap :: (a -> c) -> (b -> d) -> Either a b ->\
        \ Either c d."] bimap_,
      toPrimitive "Bind (flatMap) for either: if Right, apply the function; if Left, return unchanged." bindSig [
        "bind(e, f) is the monadic bind for either with a fixed Left type: if e is Right v, the result is\
        \ f(v); if e is Left x, the result is Left x with the Left type preserved.",
        "Used to chain computations that may fail with a common error type.",
        "Total. Corresponds to Haskell's (>>=) :: Either a b -> (b -> Either a c) -> Either a c."] bind_,
      primNoDef "either" "Eliminate an either value by applying one of two functions." eitherSig [
        "either(f, g, e) returns f(x) if e is Left x and g(y) if e is Right y.",
        "The fundamental eliminator for the either type; every other primitive in this namespace can be\
        \ derived from it.",
        "Total. Corresponds to Haskell's either :: (a -> c) -> (b -> c) -> Either a b -> c."],
      toPrimitive "Left-fold over a list with an Either-returning function, short-circuiting on Left." foldlSig [
        "foldl(f, acc0, xs) folds f over xs from the left, threading an accumulator of type a, where each\
        \ application may fail with Left e: foldl iterates while f returns Right, propagates Left on the\
        \ first failure, and returns Right (final accumulator) if all elements were processed. Equivalent to\
        \ chaining bind over the list.",
        "Total in the sense that it terminates on finite inputs; the result is a Left whenever any\
        \ application of f returns one.",
        "Corresponds to a short-circuiting variant of Haskell's foldM specialised to Either."] foldl_,
      toPrimitive "Extract the Left value, or return a default." fromLeftSig [
        "fromLeft(def, e) returns the contained Left value if e is a Left, or def if e is a Right.",
        "Total. Corresponds to Haskell's Data.Either.fromLeft :: a -> Either a b -> a."] fromLeft_,
      toPrimitive "Extract the Right value, or return a default." fromRightSig [
        "fromRight(def, e) returns the contained Right value if e is a Right, or def if e is a Left.",
        "Total. Corresponds to Haskell's Data.Either.fromRight :: b -> Either a b -> b."] fromRight_,
      toPrimitive "Check whether an either is a Left value." isLeftSig [
        "True if the argument is a Left variant, false if a Right.",
        "Total. Corresponds to Haskell's Data.Either.isLeft :: Either a b -> Bool."] isLeft_,
      toPrimitive "Check whether an either is a Right value." isRightSig [
        "True if the argument is a Right variant, false if a Left.",
        "Total. Corresponds to Haskell's Data.Either.isRight :: Either a b -> Bool."] isRight_,
      toPrimitive "Extract all Left values from a list of either values." leftsSig [
        "lefts(xs) returns a list containing every Left value in xs, in original order, with Right values\
        \ discarded.",
        "Total. Corresponds to Haskell's Data.Either.lefts :: [Either a b] -> [a]."] lefts_,
      toPrimitive "Map a function over the Right side of an either (standard functor map)." mapSig [
        "map(f, e) returns Right (f y) if e is Right y, or Left x unchanged if e is Left x.",
        "The functor instance for either; treats the Right variant as the focus and leaves the Left variant\
        \ alone.",
        "Total. Corresponds to Haskell's fmap :: (a -> b) -> Either e a -> Either e b."] map_,
      toPrimitive "Map a function returning either over a list, collecting results or short-circuiting on Left." mapListSig [
        "mapList(f, xs) applies f to each element of xs. If every application returns Right, the result is\
        \ Right of the list of contained values, in original order. The first application that returns Left\
        \ short-circuits the whole result to that Left.",
        "Total. Corresponds to Haskell's traverse :: (a -> Either e b) -> [a] -> Either e [b]."] mapList_,
      toPrimitive "Map a function returning either over a maybe, or return Right Nothing if Nothing." mapMaybeSig [
        "mapMaybe(f, m) returns Right Nothing if m is Nothing; otherwise applies f to the contained value\
        \ and returns the result with Right wrapped around Just.",
        "Total. Corresponds to Haskell's traverse :: (a -> Either e b) -> Maybe a -> Either e (Maybe b)."] mapMaybe_,
      toPrimitive "Map a function returning either over a set, collecting results or short-circuiting on Left." mapSetSig [
        "mapSet(f, s) applies f to each element of s in unspecified order. If every application returns\
        \ Right, the result is Right of the set of contained values (deduplicated by the result type's\
        \ ordering); the first application returning Left short-circuits the whole result to that Left.",
        "Total. Corresponds to Haskell's traverse-style operation specialised to Set."] mapSet_,
      toPrimitive "Partition a list of either values into lefts and rights." partitionEithersSig [
        "partitionEithers(xs) returns a pair (lefts, rights) where lefts contains every Left value from xs\
        \ in original order and rights contains every Right value from xs in original order.",
        "Total. Corresponds to Haskell's Data.Either.partitionEithers :: [Either a b] -> ([a], [b])."] partitionEithers_,
      toPrimitive "Extract all Right values from a list of either values." rightsSig [
        "rights(xs) returns a list containing every Right value in xs, in original order, with Left values\
        \ discarded.",
        "Total. Corresponds to Haskell's Data.Either.rights :: [Either a b] -> [b]."] rights_]

-- Shared type variables.
tx, ty, tz, tw :: Type
tx = Types.var "x"
ty = Types.var "y"
tz = Types.var "z"
tw = Types.var "w"

ee :: Type -> Type -> Type
ee = Types.either_

primNoDef :: String -> String -> TermSignature -> [String] -> Definition
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

-- bimap : forall a b c d. (a -> c) -> (b -> d) -> Either a b -> Either c d
bimapSig :: TermSignature
bimapSig = sig $ TypeScheme [Name "x", Name "y", Name "z", Name "w"]
  ((tx Types.~> tz) Types.~>
   (ty Types.~> tw) Types.~>
   ee tx ty Types.~> ee tz tw) Nothing

-- bind : forall a b c. Either a b -> (b -> Either a c) -> Either a c
bindSig :: TermSignature
bindSig = sig $ TypeScheme [Name "x", Name "y", Name "z"]
  (ee tx ty Types.~> (ty Types.~> ee tx tz) Types.~> ee tx tz) Nothing

-- either : forall a b c. (a -> c) -> (b -> c) -> Either a b -> c
eitherSig :: TermSignature
eitherSig = sig $ TypeScheme [Name "x", Name "y", Name "z"]
  ((tx Types.~> tz) Types.~> (ty Types.~> tz) Types.~> ee tx ty Types.~> tz) Nothing

-- foldl : forall a b c. (a -> b -> Either c a) -> a -> [b] -> Either c a
foldlSig :: TermSignature
foldlSig = sig $ TypeScheme [Name "x", Name "y", Name "z"]
  ((tx Types.~> ty Types.~> ee tz tx) Types.~> tx Types.~> Types.list ty Types.~> ee tz tx) Nothing

-- fromLeft : forall a b. a -> Either a b -> a
-- The default value (position 0) is lazy: it is only evaluated when the
-- Either is a Right.
fromLeftSig :: TermSignature
fromLeftSig = lazySig [0] $ TypeScheme [Name "x", Name "y"]
  (tx Types.~> ee tx ty Types.~> tx) Nothing

-- fromRight : forall a b. b -> Either a b -> b
-- The default value (position 0) is lazy: it is only evaluated when the
-- Either is a Left.
fromRightSig :: TermSignature
fromRightSig = lazySig [0] $ TypeScheme [Name "x", Name "y"]
  (ty Types.~> ee tx ty Types.~> ty) Nothing

-- isLeft / isRight : forall a b. Either a b -> Boolean
isLeftSig :: TermSignature
isLeftSig = sig $ TypeScheme [Name "x", Name "y"]
  (ee tx ty Types.~> Types.boolean) Nothing
isRightSig :: TermSignature
isRightSig = isLeftSig

-- lefts : forall a b. [Either a b] -> [a]
leftsSig :: TermSignature
leftsSig = sig $ TypeScheme [Name "x", Name "y"]
  (Types.list (ee tx ty) Types.~> Types.list tx) Nothing

-- mapList : forall a b c. (a -> Either c b) -> [a] -> Either c [b]
mapListSig :: TermSignature
mapListSig = sig $ TypeScheme [Name "x", Name "y", Name "z"]
  ((tx Types.~> ee tz ty) Types.~> Types.list tx Types.~> ee tz (Types.list ty)) Nothing

-- mapMaybe : forall a b c. (a -> Either c b) -> Maybe a -> Either c (Maybe b)
mapMaybeSig :: TermSignature
mapMaybeSig = sig $ TypeScheme [Name "x", Name "y", Name "z"]
  ((tx Types.~> ee tz ty) Types.~> Types.optional tx Types.~> ee tz (Types.optional ty)) Nothing

-- mapSet : forall a b c. ordering y => (a -> Either c b) -> Set a -> Either c (Set b)
-- Note: in the original Libraries.hs it's [_x, _y, _z]; the ordering constraint is on the result element type
-- (Set b requires b : Ord). But Libraries.hs uses _x, _y, _z without ordering markers — so we keep them unconstrained here.
mapSetSig :: TermSignature
mapSetSig = sig $ TypeScheme [Name "x", Name "y", Name "z"]
  ((tx Types.~> ee tz ty) Types.~> Types.set tx Types.~> ee tz (Types.set ty)) Nothing

-- map : forall a b c. (a -> b) -> Either c a -> Either c b
mapSig :: TermSignature
mapSig = sig $ TypeScheme [Name "x", Name "y", Name "z"]
  ((tx Types.~> ty) Types.~> ee tz tx Types.~> ee tz ty) Nothing

-- partitionEithers : forall a b. [Either a b] -> ([a], [b])
partitionEithersSig :: TermSignature
partitionEithersSig = sig $ TypeScheme [Name "x", Name "y"]
  (Types.list (ee tx ty) Types.~> Types.pair (Types.list tx) (Types.list ty)) Nothing

-- rights : forall a b. [Either a b] -> [b]
rightsSig :: TermSignature
rightsSig = sig $ TypeScheme [Name "x", Name "y"]
  (Types.list (ee tx ty) Types.~> Types.list ty) Nothing

-- Default implementations (for the easy ones).

-- bimap f g e = either (\x -> Left (f x)) (\y -> Right (g y)) e
bimap_ :: TypedTermDefinition ((a -> c) -> (b -> d) -> Either a b -> Either c d)
bimap_ = define "bimap" $
  doc "Map over both sides of an Either, defined in terms of either." $
  "f" ~> "g" ~> "e" ~>
    Eithers.either_
      ("x" ~> left (var "f" @@ var "x"))
      ("y" ~> right (var "g" @@ var "y"))
      (var "e")

-- bind e f = either Left f e
bind_ :: TypedTermDefinition (Either a b -> (b -> Either a c) -> Either a c)
bind_ = define "bind" $
  doc "Monadic bind for Either, defined in terms of either." $
  "e" ~> "f" ~>
    Eithers.either_
      ("x" ~> left (var "x"))
      (var "f")
      (var "e")

-- fromLeft def e = either (\x -> x) (\_ -> def) e
fromLeft_ :: TypedTermDefinition (a -> Either a b -> a)
fromLeft_ = define "fromLeft" $
  doc "Extract the Left value or return a default, defined in terms of either." $
  "def" ~> "e" ~>
    Eithers.either_ ("x" ~> var "x") ("_" ~> var "def") (var "e")

-- fromRight def e = either (\_ -> def) (\x -> x) e
fromRight_ :: TypedTermDefinition (b -> Either a b -> b)
fromRight_ = define "fromRight" $
  doc "Extract the Right value or return a default, defined in terms of either." $
  "def" ~> "e" ~>
    Eithers.either_ ("_" ~> var "def") ("x" ~> var "x") (var "e")

-- isLeft e = either (\_ -> true) (\_ -> false) e
isLeft_ :: TypedTermDefinition (Either a b -> Bool)
isLeft_ = define "isLeft" $
  doc "Check whether an Either is a Left value, defined in terms of either." $
  "e" ~> Eithers.either_ ("_" ~> true) ("_" ~> false) (var "e")

-- isRight e = either (\_ -> false) (\_ -> true) e
isRight_ :: TypedTermDefinition (Either a b -> Bool)
isRight_ = define "isRight" $
  doc "Check whether an Either is a Right value, defined in terms of either." $
  "e" ~> Eithers.either_ ("_" ~> false) ("_" ~> true) (var "e")

-- map f e = either Left (\x -> Right (f x)) e
map_ :: TypedTermDefinition ((a -> b) -> Either c a -> Either c b)
map_ = define "map" $
  doc "Map a function over the Right side, defined in terms of either." $
  "f" ~> "e" ~>
    Eithers.either_
      ("x" ~> left (var "x"))
      ("y" ~> right (var "f" @@ var "y"))
      (var "e")

-- foldl f acc0 xs = foldl' (\acc el -> bind acc (\a -> f a el)) (Right acc0) xs
-- Short-circuits on Left: bind on a Left propagates without calling f.
foldl_ :: TypedTermDefinition ((a -> b -> Either c a) -> a -> [b] -> Either c a)
foldl_ = define "foldl" $
  doc "Either-short-circuiting left fold, defined in terms of bind." $
  "f" ~> "acc0" ~> "xs" ~>
    Lists.foldl
      ("acc" ~> "el" ~>
        Eithers.bind (var "acc") ("a" ~> var "f" @@ var "a" @@ var "el"))
      (right (var "acc0"))
      (var "xs")

-- lefts xs = foldr (\e acc -> either (\l -> cons l acc) (\_ -> acc) e) [] xs
lefts_ :: TypedTermDefinition ([Either a b] -> [a])
lefts_ = define "lefts" $
  doc "Extract all Left values from a list of eithers, defined in terms of either + foldr." $
  "xs" ~>
    Lists.foldr
      ("e" ~> "acc" ~>
        Eithers.either_
          ("l" ~> Lists.cons (var "l") (var "acc" :: TypedTerm [a]))
          ("_" ~> (var "acc" :: TypedTerm [a]))
          (var "e"))
      (list ([] :: [TypedTerm a]))
      (var "xs")

-- rights xs = foldr (\e acc -> either (\_ -> acc) (\r -> cons r acc) e) [] xs
rights_ :: TypedTermDefinition ([Either a b] -> [b])
rights_ = define "rights" $
  doc "Extract all Right values from a list of eithers, defined in terms of either + foldr." $
  "xs" ~>
    Lists.foldr
      ("e" ~> "acc" ~>
        Eithers.either_
          ("_" ~> (var "acc" :: TypedTerm [b]))
          ("r" ~> Lists.cons (var "r") (var "acc" :: TypedTerm [b]))
          (var "e"))
      (list ([] :: [TypedTerm b]))
      (var "xs")

-- partitionEithers xs = foldr (\e (ls, rs) -> either (\l -> (cons l ls, rs)) (\r -> (ls, cons r rs)) e) ([],[]) xs
partitionEithers_ :: TypedTermDefinition ([Either a b] -> ([a], [b]))
partitionEithers_ = define "partitionEithers" $
  doc "Partition a list of eithers into (lefts, rights), defined in terms of either + foldr." $
  "xs" ~>
    Lists.foldr
      ("e" ~> "acc" ~>
        Eithers.either_
          ("l" ~> pair (Lists.cons (var "l") (Pairs.first $ var "acc")) (Pairs.second $ var "acc"))
          ("r" ~> pair (Pairs.first $ var "acc") (Lists.cons (var "r") (Pairs.second $ var "acc")))
          (var "e"))
      (pair (list ([] :: [TypedTerm a])) (list ([] :: [TypedTerm b])))
      (var "xs")

-- mapList f xs = foldr (\x acc -> bind (f x) (\y -> map (cons y) acc)) (Right []) xs
mapList_ :: TypedTermDefinition ((a -> Either c b) -> [a] -> Either c [b])
mapList_ = define "mapList" $
  doc "Traverse a list with an Either-returning function, short-circuiting on Left." $
  "f" ~> "xs" ~>
    Lists.foldr
      ("x" ~> "acc" ~>
        Eithers.bind (var "f" @@ var "x") $
          "y" ~> Eithers.map ("ys" ~> Lists.cons (var "y") (var "ys")) (var "acc"))
      (right (list ([] :: [TypedTerm b])))
      (var "xs")

-- mapMaybe f m = maybe (Right Nothing) (\x -> map Just (f x)) m
mapMaybe_ :: TypedTermDefinition ((a -> Either c b) -> Maybe a -> Either c (Maybe b))
mapMaybe_ = define "mapMaybe" $
  doc "Traverse a Maybe with an Either-returning function." $
  "f" ~> "m" ~>
    Maybes.maybe
      (right nothing)
      ("x" ~> Eithers.map ("y" ~> just (var "y")) (var "f" @@ var "x"))
      (var "m")

-- mapSet f s = map fromList (mapList f (toList s))
mapSet_ :: TypedTermDefinition ((a -> Either c b) -> S.Set a -> Either c (S.Set b))
mapSet_ = define "mapSet" $
  doc "Traverse a Set with an Either-returning function, defined via list+set conversion." $
  "f" ~> "s" ~>
    Eithers.map
      ("ys" ~> Sets.fromList (var "ys"))
      (Lists.foldr
        ("x" ~> "acc" ~>
          Eithers.bind (var "f" @@ var "x") $
            "y" ~> Eithers.map ("ys" ~> Lists.cons (var "y") (var "ys")) (var "acc"))
        (right (list ([] :: [TypedTerm b])))
        (Sets.toList (var "s")))
