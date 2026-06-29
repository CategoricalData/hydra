-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.docs

module Hydra.Encode.Docs where
import qualified Hydra.Core as Core
import qualified Hydra.Docs as Docs
import qualified Hydra.Encode.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.docs.DocSegment
docSegment :: Docs.DocSegment -> Core.Term
docSegment x =
    case x of
      Docs.DocSegmentRef v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.docs.DocSegment"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "ref"),
          Core.fieldTerm = (Packaging.entityReference v0)}})
      Docs.DocSegmentText v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.docs.DocSegment"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "text"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v0))}})
