-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.testing

module Hydra.Encode.Testing where
import qualified Hydra.Core as Core
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.testing.Tag
tag :: Testing.Tag -> Core.Term
tag x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.testing.Tag"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Testing.unTag x))})
