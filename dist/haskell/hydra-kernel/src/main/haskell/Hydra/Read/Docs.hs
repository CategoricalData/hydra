-- Note: this is an automatically generated file. Do not edit.

-- | Parser for Hydra documentation strings, producing DocSegment lists

module Hydra.Read.Docs where

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
import qualified Hydra.Overlay.Haskell.Lib.Equality as Equality
import qualified Hydra.Overlay.Haskell.Lib.Lists as Lists
import qualified Hydra.Overlay.Haskell.Lib.Logic as Logic
import qualified Hydra.Overlay.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Overlay.Haskell.Lib.Strings as Strings
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

-- | Parse the content between {@...} delimiters into an EntityReference. The input is the inner content (tag and optional rhs), e.g. "type hydra.core.Lambda". Returns nothing for unrecognized tags.
parseDocAnnotation :: String -> Maybe Packaging.EntityReference
parseDocAnnotation inner =

      let parts = Strings.splitOn " " inner
          tag = Optionals.fromOptional "" (Lists.maybeHead parts)
          rhs = Strings.intercalate " " (Lists.drop 1 parts)
      in (Logic.ifElse (Equality.equal tag "primitive") (Just (Packaging.EntityReferenceDefinition (Packaging.DefinitionReferencePrimitive (Core.Name rhs)))) (Logic.ifElse (Equality.equal tag "term") (Just (Packaging.EntityReferenceDefinition (Packaging.DefinitionReferenceTerm (Core.Name rhs)))) (Logic.ifElse (Equality.equal tag "type") (Just (Packaging.EntityReferenceDefinition (Packaging.DefinitionReferenceType (Core.Name rhs)))) (Logic.ifElse (Equality.equal tag "module") (Just (Packaging.EntityReferenceModule (Packaging.ModuleName rhs))) (Logic.ifElse (Equality.equal tag "package") (Just (Packaging.EntityReferencePackage (Packaging.PackageName rhs))) (Logic.ifElse (Equality.equal tag "term-expr") (Just (Packaging.EntityReferenceTermExpr rhs)) (Logic.ifElse (Equality.equal tag "type-expr") (Just (Packaging.EntityReferenceTypeExpr rhs)) Nothing)))))))

-- | Parse a documentation string into a list of 'DocSegment's. Recognized {@tag rhs} escapes become DocSegment.ref segments (wrapping a 'EntityReference'); all other text (including unrecognized {@...} blocks) becomes DocSegment.text segments. Adjacent text fragments are not merged.
parseDocString :: String -> [Docs.DocSegment]
parseDocString s =

      let parts = Strings.splitOn "{@" s
          head_ = Optionals.fromOptional "" (Lists.maybeHead parts)
          tail_ = Lists.drop 1 parts
          toSeg =
                  \part ->
                    let subparts = Strings.splitOn "}" part
                        inner = Optionals.fromOptional "" (Lists.maybeHead subparts)
                        after = Strings.intercalate "}" (Lists.drop 1 subparts)
                        mref = parseDocAnnotation inner
                    in (Optionals.cases mref [
                      Docs.DocSegmentText (Strings.cat2 "{@" part)] (\ref -> Optionals.cat [
                      Just (Docs.DocSegmentRef ref),
                      (Logic.ifElse (Equality.equal after "") Nothing (Just (Docs.DocSegmentText after)))]))
      in (Lists.cons (Docs.DocSegmentText head_) (Lists.concat (Lists.map toSeg tail_)))
