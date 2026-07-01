-- Note: this is an automatically generated file. Do not edit.

-- | String representations of hydra.docs types

module Hydra.Show.Docs where

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
import qualified Hydra.Overlay.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Read.Docs as ReadDocs
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

-- | Render a DefinitionReference as its fully-qualified name
definitionReference :: Packaging.DefinitionReference -> String
definitionReference x =
    case x of
      Packaging.DefinitionReferencePrimitive v0 -> Core.unName v0
      Packaging.DefinitionReferenceTerm v0 -> Core.unName v0
      Packaging.DefinitionReferenceType v0 -> Core.unName v0

-- | Render a single DocSegment back to its source string form
docSegment :: Docs.DocSegment -> String
docSegment seg = docSegmentWith entityReference seg

-- | Render a single DocSegment using a custom 'EntityReference' renderer
docSegmentWith :: (Packaging.EntityReference -> String) -> Docs.DocSegment -> String
docSegmentWith render x =
    case x of
      Docs.DocSegmentRef v0 -> render v0
      Docs.DocSegmentText v0 -> v0

-- | Render a list of DocSegments back to a plain documentation string
docSegments :: [Docs.DocSegment] -> String
docSegments segs = Strings.cat (Lists.map docSegment segs)

-- | Render a list of DocSegments using a custom 'EntityReference' renderer
docSegmentsWith :: (Packaging.EntityReference -> String) -> [Docs.DocSegment] -> String
docSegmentsWith render segs = Strings.cat (Lists.map (docSegmentWith render) segs)

-- | Render a 'EntityReference' as its doc-escape tag string (without the surrounding braces)
entityReference :: Packaging.EntityReference -> String
entityReference x =
    case x of
      Packaging.EntityReferenceDefinition v0 -> Strings.cat2 (case v0 of
        Packaging.DefinitionReferencePrimitive _ -> "primitive"
        Packaging.DefinitionReferenceTerm _ -> "term"
        Packaging.DefinitionReferenceType _ -> "type") (Strings.cat2 " " (definitionReference v0))
      Packaging.EntityReferenceModule v0 -> Strings.cat2 "module " (Packaging.unModuleName v0)
      Packaging.EntityReferencePackage v0 -> Strings.cat2 "package " (Packaging.unPackageName v0)
      Packaging.EntityReferenceTermExpr v0 -> Strings.cat2 "term-expr " v0
      Packaging.EntityReferenceTypeExpr v0 -> Strings.cat2 "type-expr " v0

-- | Parse a documentation string and re-render it, converting doc-escape tags through their canonical form
renderDocString :: String -> String
renderDocString s = renderDocStringWith entityReference s

-- | Parse a documentation string and render it using a custom 'EntityReference' renderer. Text segments are passed through; ref segments are rendered by the provided function. Unrecognized doc-escape blocks are passed through as text.
renderDocStringWith :: (Packaging.EntityReference -> String) -> String -> String
renderDocStringWith render s = docSegmentsWith render (ReadDocs.parseDocString s)
