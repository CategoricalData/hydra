-- | Primitive declarations for the hydra.lib.eithers namespace.

module Hydra.Sources.Kernel.Lib.Eithers where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Set                    as S


ns :: ModuleName
ns = ModuleName "hydra.lib.eithers"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.eithers namespace."}
  where
    definitions = [
      toPrimitive "Map over both sides of an Either value." bimapSig bimap_,
      toPrimitive "Bind (flatMap) for Either: if Right, apply the function; if Left, return unchanged." bindSig bind_,
      primNoDef "either" "Eliminate an Either value by applying one of two functions." eitherSig,
      primNoDef "foldl" "Left-fold over a list with an Either-returning function, short-circuiting on Left." foldlSig,
      toPrimitive "Extract the Left value, or return a default." fromLeftSig fromLeft_,
      toPrimitive "Extract the Right value, or return a default." fromRightSig fromRight_,
      toPrimitive "Check whether an Either is a Left value." isLeftSig isLeft_,
      toPrimitive "Check whether an Either is a Right value." isRightSig isRight_,
      primNoDef "lefts" "Extract all Left values from a list of Eithers." leftsSig,
      toPrimitive "Map a function over the Right side of an Either (standard functor map)." mapSig map_,
      primNoDef "mapList" "Map a function returning Either over a list, collecting results or short-circuiting on Left." mapListSig,
      primNoDef "mapMaybe" "Map a function returning Either over a Maybe, or return Right Nothing if Nothing." mapMaybeSig,
      primNoDef "mapSet" "Map a function returning Either over a Set, collecting results or short-circuiting on Left." mapSetSig,
      primNoDef "partitionEithers" "Partition a list of Eithers into lefts and rights." partitionEithersSig,
      primNoDef "rights" "Extract all Right values from a list of Eithers." rightsSig]

-- Shared type variables.
tx, ty, tz, tw :: Type
tx = Types.var "x"
ty = Types.var "y"
tz = Types.var "z"
tw = Types.var "w"

ee :: Type -> Type -> Type
ee = Types.either_

primNoDef :: String -> String -> TermSignature -> Definition
primNoDef localName description s =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName))

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

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
fromLeftSig :: TermSignature
fromLeftSig = sig $ TypeScheme [Name "x", Name "y"]
  (tx Types.~> ee tx ty Types.~> tx) Nothing

-- fromRight : forall a b. b -> Either a b -> b
fromRightSig :: TermSignature
fromRightSig = sig $ TypeScheme [Name "x", Name "y"]
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
bimap_ :: TTermDefinition ((a -> c) -> (b -> d) -> Either a b -> Either c d)
bimap_ = define "bimap" $
  doc "Map over both sides of an Either, defined in terms of either." $
  "f" ~> "g" ~> "e" ~>
    Eithers.either_
      ("x" ~> left (var "f" @@ var "x"))
      ("y" ~> right (var "g" @@ var "y"))
      (var "e")

-- bind e f = either Left f e
bind_ :: TTermDefinition (Either a b -> (b -> Either a c) -> Either a c)
bind_ = define "bind" $
  doc "Monadic bind for Either, defined in terms of either." $
  "e" ~> "f" ~>
    Eithers.either_
      ("x" ~> left (var "x"))
      (var "f")
      (var "e")

-- fromLeft def e = either (\x -> x) (\_ -> def) e
fromLeft_ :: TTermDefinition (a -> Either a b -> a)
fromLeft_ = define "fromLeft" $
  doc "Extract the Left value or return a default, defined in terms of either." $
  "def" ~> "e" ~>
    Eithers.either_ ("x" ~> var "x") ("_" ~> var "def") (var "e")

-- fromRight def e = either (\_ -> def) (\x -> x) e
fromRight_ :: TTermDefinition (b -> Either a b -> b)
fromRight_ = define "fromRight" $
  doc "Extract the Right value or return a default, defined in terms of either." $
  "def" ~> "e" ~>
    Eithers.either_ ("_" ~> var "def") ("x" ~> var "x") (var "e")

-- isLeft e = either (\_ -> true) (\_ -> false) e
isLeft_ :: TTermDefinition (Either a b -> Bool)
isLeft_ = define "isLeft" $
  doc "Check whether an Either is a Left value, defined in terms of either." $
  "e" ~> Eithers.either_ ("_" ~> true) ("_" ~> false) (var "e")

-- isRight e = either (\_ -> false) (\_ -> true) e
isRight_ :: TTermDefinition (Either a b -> Bool)
isRight_ = define "isRight" $
  doc "Check whether an Either is a Right value, defined in terms of either." $
  "e" ~> Eithers.either_ ("_" ~> false) ("_" ~> true) (var "e")

-- map f e = either Left (\x -> Right (f x)) e
map_ :: TTermDefinition ((a -> b) -> Either c a -> Either c b)
map_ = define "map" $
  doc "Map a function over the Right side, defined in terms of either." $
  "f" ~> "e" ~>
    Eithers.either_
      ("x" ~> left (var "x"))
      ("y" ~> right (var "f" @@ var "y"))
      (var "e")
