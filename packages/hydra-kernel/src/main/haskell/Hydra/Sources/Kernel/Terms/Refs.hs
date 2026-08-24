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
import qualified Hydra.Overlay.Haskell.Dsl.Types              as Types
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
     encodeMapDefinition,
     toDefinition encodeOptional,
     toDefinition encodePair,
     toDefinition encodeRef,
     encodeSetDefinition,
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
-- Declared with an explicit constrained TermSignature (via toPrimitive) rather than relying on
-- inference to derive the 'ordering' constraint on the key type: inference does not currently
-- propagate a callee's class constraint through a function-domain (contravariant) position, so a
-- plain TypedTermDefinition here would compile in Haskell but generate a TypeScheme with no
-- 'ordering' constraint for the other 8 hosts to consume (see #702 for the general inference fix).
-- unregistered: helper consumed by encodeMapDefinition (toPrimitive); the emitted primitive is registered as encodeMapDefinition. See #702.
encodeMap :: TypedTermDefinition ((Int -> Term) -> (Int -> Term) -> M.Map Int Int -> Term)
encodeMap = define "encodeMap" $
  doc "Build an encoder for a map, given encoders for its key and value types" $
  "keyEncoder" ~> "valEncoder" ~> "m" ~> Core.termMap
    (Maps.bimap (var "keyEncoder" :: TypedTerm (Int -> Term)) (var "valEncoder" :: TypedTerm (Int -> Term)) (var "m" :: TypedTerm (M.Map Int Int)))

encodeMapDefinition :: Definition
encodeMapDefinition = toPrimitive
  "Build an encoder for a map, given encoders for its key and value types"
  (sigWithParams
    [("keyEncoder", "the encoder for the map's key type"),
     ("valEncoder", "the encoder for the map's value type"),
     ("m", "the map to encode")] $ Types.polyConstrained
    [("k", [Name "ordering"]), ("v", [])]
    ((Types.var "k" Types.~> Types.var "hydra.core.Term") Types.~> (Types.var "v" Types.~> Types.var "hydra.core.Term") Types.~>
      Types.map (Types.var "k") (Types.var "v") Types.~> Types.var "hydra.core.Term"))
  []
  encodeMap

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
-- Declared with an explicit constrained TermSignature (via toPrimitive); see encodeMapDefinition
-- for why this is necessary rather than relying on inference (#702).
-- unregistered: helper consumed by encodeSetDefinition (toPrimitive); the emitted primitive is registered as encodeSetDefinition. See #702.
encodeSet :: TypedTermDefinition ((Int -> Term) -> S.Set Int -> Term)
encodeSet = define "encodeSet" $
  doc "Build an encoder for a set, given an encoder for its element type" $
  "elemEncoder" ~> "xs" ~> Core.termSet
    (Sets.map (var "elemEncoder" :: TypedTerm (Int -> Term)) (var "xs" :: TypedTerm (S.Set Int)))

encodeSetDefinition :: Definition
encodeSetDefinition = toPrimitive
  "Build an encoder for a set, given an encoder for its element type"
  (sigWithParams
    [("elemEncoder", "the encoder for the set's element type"),
     ("xs", "the set to encode")] $ Types.polyConstrained
    [("a", [Name "ordering"])]
    ((Types.var "a" Types.~> Types.var "hydra.core.Term") Types.~>
      Types.set (Types.var "a") Types.~> Types.var "hydra.core.Term"))
  []
  encodeSet

-- | Look up the string-shower for a type given its TypedName token. The result is a
-- term reference (Term.variable) to the type's hydra.print.<ns>.<local> binding;
-- ordinary term evaluation resolves it to the shower function it names.
showRef :: TypedTermDefinition (TypedName a -> (a -> String))
showRef = define "showRef" $
  doc "Look up the string-shower function for a type given its TypedName token" $
  "tn" ~> Core.termVariable (Names.derivedBindingName @@ list [string "hydra", string "show"] @@ boolean True @@ (unwrap _TypedName @@ var "tn"))
