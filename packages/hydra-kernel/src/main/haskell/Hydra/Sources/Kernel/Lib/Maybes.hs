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

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.maybes namespace."}
  where
    definitions = [
      toPrimitive "Applicative apply for optionals: combine an optional function and an optional argument." applySig Nothing apply_,
      toPrimitive "Monadic bind for optionals." bindSig Nothing bind_,
      primNoDef "cases" "Case analysis on an optional, with cases-style argument order." casesSig Nothing,
      toPrimitive "Concatenate optionals, keeping only the present values." catSig Nothing cat_,
      toPrimitive "Kleisli composition for optionals." composeSig Nothing compose_,
      toPrimitive "Return the value contained in an optional, falling back to a default if absent." fromMaybeSig Nothing fromMaybe_,
      toPrimitive "Test whether an optional is present (Just)." isJustSig Nothing isJust_,
      toPrimitive "Test whether an optional is absent (Nothing)." isNothingSig Nothing isNothing_,
      toPrimitive "Map a function over an optional." mapSig Nothing map_,
      toPrimitive "Map a partial function over a list, keeping only the present results." mapMaybeSig Nothing mapMaybe_,
      primNoDef "maybe" "Case analysis on an optional, applying a function if present or returning a default if absent." maybeSig Nothing,
      toPrimitive "Wrap a value in Just." pureSig Nothing pure_,
      toPrimitive "Convert an optional to a list: Just x maps to [x], Nothing to []." toListSig Nothing toList_]
primNoDef :: String -> String -> TermSignature -> Maybe String -> Definition
primNoDef localName description s comments =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName)) comments

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

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
casesSig :: TermSignature
casesSig = sig $ TypeScheme [Name "x", Name "y"]
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
fromMaybeSig :: TermSignature
fromMaybeSig = sig $ TypeScheme [Name "x"]
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
maybeSig :: TermSignature
maybeSig = sig $ TypeScheme [Name "y", Name "x"]
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
apply_ :: TTermDefinition (Maybe (a -> b) -> Maybe a -> Maybe b)
apply_ = define "apply" $
  doc "Applicative apply for optionals, defined in terms of bind and map." $
  "mf" ~> "mx" ~> Maybes.bind (var "mf")
    ("f" ~> Maybes.map ("x" ~> var "f" @@ var "x") (var "mx"))

-- bind m f = maybe Nothing f m
bind_ :: TTermDefinition (Maybe a -> (a -> Maybe b) -> Maybe b)
bind_ = define "bind" $
  doc "Monadic bind for optionals, defined in terms of maybe." $
  "m" ~> "f" ~> Maybes.maybe nothing (var "f") (var "m")

-- cat xs = foldr (\m acc -> maybe acc (\v -> v : acc) m) [] xs
cat_ :: TTermDefinition ([Maybe a] -> [a])
cat_ = define "cat" $
  doc "Catenate a list of optionals, keeping only the present values." $
  "xs" ~> Lists.foldr
    ("m" ~> "acc" ~> Maybes.maybe (var "acc" :: TTerm [a])
      ("v" ~> Lists.cons (var "v") (var "acc"))
      (var "m"))
    (list ([] :: [TTerm a]))
    (var "xs")

-- compose f g x = bind (f x) g
compose_ :: TTermDefinition ((a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c)
compose_ = define "compose" $
  doc "Kleisli composition for optionals, defined in terms of bind." $
  "f" ~> "g" ~> "x" ~> Maybes.bind (var "f" @@ var "x") (var "g")

-- fromMaybe def m = maybe def (\x -> x) m
fromMaybe_ :: TTermDefinition (a -> Maybe a -> a)
fromMaybe_ = define "fromMaybe" $
  doc "Return the contained value or a default, defined in terms of maybe." $
  "def" ~> "m" ~> Maybes.maybe (var "def" :: TTerm a) ("x" ~> var "x") (var "m")

-- isJust m = maybe false (\_ -> true) m
isJust_ :: TTermDefinition (Maybe a -> Bool)
isJust_ = define "isJust" $
  doc "Test for presence, defined in terms of maybe." $
  "m" ~> Maybes.maybe false ("_" ~> true) (var "m")

-- isNothing m = maybe true (\_ -> false) m
isNothing_ :: TTermDefinition (Maybe a -> Bool)
isNothing_ = define "isNothing" $
  doc "Test for absence, defined in terms of maybe." $
  "m" ~> Maybes.maybe true ("_" ~> false) (var "m")

-- mapMaybe f xs = cat (Lists.map f xs)
mapMaybe_ :: TTermDefinition ((a -> Maybe b) -> [a] -> [b])
mapMaybe_ = define "mapMaybe" $
  doc "Map a partial function and keep only the present results, defined in terms of lists.map and cat." $
  "f" ~> "xs" ~> Maybes.cat (Lists.map (var "f") (var "xs"))

-- map f m = maybe Nothing (\x -> Just (f x)) m
map_ :: TTermDefinition ((a -> b) -> Maybe a -> Maybe b)
map_ = define "map" $
  doc "Map a function over an optional, defined in terms of maybe." $
  "f" ~> "m" ~> Maybes.maybe nothing ("x" ~> just (var "f" @@ var "x")) (var "m")

-- pure x = Just x
pure_ :: TTermDefinition (a -> Maybe a)
pure_ = define "pure" $
  doc "Wrap a value in Just." $
  "x" ~> just (var "x")

-- toList m = maybe [] (\x -> [x]) m
toList_ :: TTermDefinition (Maybe a -> [a])
toList_ = define "toList" $
  doc "Convert an optional to a list, defined in terms of maybe." $
  "m" ~> Maybes.maybe (list ([] :: [TTerm a]))
    ("x" ~> list [var "x"])
    (var "m")
