-- Note: this is an automatically generated file. Do not edit.

-- | A model for Hydra documentation strings, including inline annotations and entity references

module Hydra.Docs where

import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | A segment of a documentation string, either a raw text fragment or an inline entity reference. Documentation strings are parsed into a sequence of DocSegments for host-specific rendering.
data DocSegment =
  -- | An inline reference to a Hydra entity
  DocSegmentRef Packaging.EntityReference |
  -- | A raw prose fragment, passed through verbatim
  DocSegmentText String
  deriving (Eq, Ord, Read, Show)

_DocSegment = Core.Name "hydra.docs.DocSegment"

_DocSegment_ref = Core.Name "ref"

_DocSegment_text = Core.Name "text"
