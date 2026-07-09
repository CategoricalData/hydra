{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Terms.Refs where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap             as Bootstrap
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Dsl.Lib.Lists      as Lists
import qualified Hydra.Dsl.Lib.Maps       as Maps
import qualified Hydra.Dsl.Lib.Optionals  as Optionals
import qualified Hydra.Dsl.Lib.Pairs      as Pairs
import qualified Hydra.Dsl.Lib.Sets       as Sets
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S


ns :: ModuleName
ns = ModuleName "hydra.refs"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Names.ns, ModuleName "hydra.typed"] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Typed references to derived encode/decode/show functions, and structural coder builders")}
  where
   definitions = [
     toDefinition decodeRef,
     toDefinition encodeList,
     toDefinition encodeMap,
     toDefinition encodeOptional,
     toDefinition encodePair,
     toDefinition encodeRef,
     toDefinition encodeSet,
     toDefinition showRef]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- | Look up the decoder for a type given its TypedName token. The result is a term
-- reference (Term.variable) to the type's synthesized hydra.decode.<ns>.<local> binding;
-- ordinary term evaluation resolves it to the decoder function it names.
decodeRef :: TypedTermDefinition (TypedName a -> (Graph -> Term -> Either DecodingError a))
decodeRef = define "decodeRef" $
  doc "Look up the decoder function for a type given its TypedName token" $
  "tn" ~> Core.termVariable (Names.derivedBindingName @@ list [string "hydra", string "decode"] @@ boolean True @@ (unwrap _TypedName @@ var "tn"))

-- | Build an encoder for a list, given an encoder for its element type.
encodeList :: TypedTermDefinition ((a -> Term) -> [a] -> Term)
encodeList = define "encodeList" $
  doc "Build an encoder for a list, given an encoder for its element type" $
  "elemEncoder" ~> "xs" ~> Core.termList (Lists.map (var "elemEncoder") (var "xs"))

-- | Build an encoder for a map, given encoders for its key and value types.
encodeMap :: TypedTermDefinition ((Int -> Term) -> (Int -> Term) -> M.Map Int Int -> Term)
encodeMap = define "encodeMap" $
  doc "Build an encoder for a map, given encoders for its key and value types" $
  "keyEncoder" ~> "valEncoder" ~> "m" ~> Core.termMap
    (Maps.bimap (var "keyEncoder" :: TypedTerm (Int -> Term)) (var "valEncoder" :: TypedTerm (Int -> Term)) (var "m" :: TypedTerm (M.Map Int Int)))

-- | Build an encoder for an optional value, given an encoder for its element type.
encodeOptional :: TypedTermDefinition ((a -> Term) -> Maybe a -> Term)
encodeOptional = define "encodeOptional" $
  doc "Build an encoder for an optional value, given an encoder for its element type" $
  "elemEncoder" ~> "x" ~> Core.termOptional (Optionals.map (var "elemEncoder") (var "x"))

-- | Build an encoder for a pair, given encoders for its first and second components.
encodePair :: TypedTermDefinition ((a -> Term) -> (b -> Term) -> (a, b) -> Term)
encodePair = define "encodePair" $
  doc "Build an encoder for a pair, given encoders for its first and second components" $
  "firstEncoder" ~> "secondEncoder" ~> "p" ~> Core.termPair (Pairs.bimap (var "firstEncoder") (var "secondEncoder") (var "p"))

-- | Look up the encoder for a type given its TypedName token. The result is a term
-- reference (Term.variable) to the type's synthesized hydra.encode.<ns>.<local> binding;
-- ordinary term evaluation resolves it to the encoder function it names.
encodeRef :: TypedTermDefinition (TypedName a -> (a -> Term))
encodeRef = define "encodeRef" $
  doc "Look up the encoder function for a type given its TypedName token" $
  "tn" ~> Core.termVariable (Names.derivedBindingName @@ list [string "hydra", string "encode"] @@ boolean True @@ (unwrap _TypedName @@ var "tn"))

-- | Build an encoder for a set, given an encoder for its element type.
encodeSet :: TypedTermDefinition ((Int -> Term) -> S.Set Int -> Term)
encodeSet = define "encodeSet" $
  doc "Build an encoder for a set, given an encoder for its element type" $
  "elemEncoder" ~> "xs" ~> Core.termSet
    (Sets.map (var "elemEncoder" :: TypedTerm (Int -> Term)) (var "xs" :: TypedTerm (S.Set Int)))

-- | Look up the string-shower for a type given its TypedName token. The result is a
-- term reference (Term.variable) to the type's hydra.show.<ns>.<local> binding;
-- ordinary term evaluation resolves it to the shower function it names.
showRef :: TypedTermDefinition (TypedName a -> (a -> String))
showRef = define "showRef" $
  doc "Look up the string-shower function for a type given its TypedName token" $
  "tn" ~> Core.termVariable (Names.derivedBindingName @@ list [string "hydra", string "show"] @@ boolean True @@ (unwrap _TypedName @@ var "tn"))
