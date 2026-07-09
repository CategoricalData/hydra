-- Note: this is an automatically generated file. Do not edit.

-- | Typed references to derived encode/decode/show functions, and structural coder builders

module Hydra.Refs where

import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Docs as Docs
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.File as ErrorFile
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Error.System as ErrorSystem
import qualified Hydra.Errors as Errors
import qualified Hydra.File as File
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Overlay.Haskell.Lib.Lists as Lists
import qualified Hydra.Overlay.Haskell.Lib.Maps as Maps
import qualified Hydra.Overlay.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Overlay.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Overlay.Haskell.Lib.Sets as Sets
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.System as System
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Time as Time
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S

-- | Look up the decoder function for a type given its TypedName token
decodeRef :: Typed.TypedName t0 -> Core.Term
decodeRef tn =
    Core.TermVariable (Names.derivedBindingName [
      "hydra",
      "decode"] True (Typed.unTypedName tn))

-- | Build an encoder for a list, given an encoder for its element type
encodeList :: (t0 -> Core.Term) -> [t0] -> Core.Term
encodeList elemEncoder xs = Core.TermList (Lists.map elemEncoder xs)

-- | Build an encoder for a map, given encoders for its key and value types
encodeMap :: Ord t0 => ((t0 -> Core.Term) -> (t1 -> Core.Term) -> M.Map t0 t1 -> Core.Term)
encodeMap keyEncoder valEncoder m = Core.TermMap (Maps.bimap keyEncoder valEncoder m)

-- | Build an encoder for an optional value, given an encoder for its element type
encodeOptional :: (t0 -> Core.Term) -> Maybe t0 -> Core.Term
encodeOptional elemEncoder x = Core.TermOptional (Optionals.map elemEncoder x)

-- | Build an encoder for a pair, given encoders for its first and second components
encodePair :: (t0 -> Core.Term) -> (t1 -> Core.Term) -> (t0, t1) -> Core.Term
encodePair firstEncoder secondEncoder p = Core.TermPair (Pairs.bimap firstEncoder secondEncoder p)

-- | Look up the encoder function for a type given its TypedName token
encodeRef :: Typed.TypedName t0 -> Core.Term
encodeRef tn =
    Core.TermVariable (Names.derivedBindingName [
      "hydra",
      "encode"] True (Typed.unTypedName tn))

-- | Build an encoder for a set, given an encoder for its element type
encodeSet :: Ord t0 => ((t0 -> Core.Term) -> S.Set t0 -> Core.Term)
encodeSet elemEncoder xs = Core.TermSet (Sets.map elemEncoder xs)

-- | Look up the string-shower function for a type given its TypedName token
showRef :: Typed.TypedName t0 -> Core.Term
showRef tn =
    Core.TermVariable (Names.derivedBindingName [
      "hydra",
      "show"] True (Typed.unTypedName tn))
