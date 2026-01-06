-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.coders

module Hydra.Encode.Coders where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

coderDirection :: (Coders.CoderDirection -> Core.Term)
coderDirection x = case x of
  Coders.CoderDirectionEncode -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.coders.CoderDirection"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "encode"),
      Core.fieldTerm = Core.TermUnit}}))
  Coders.CoderDirectionDecode -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.coders.CoderDirection"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "decode"),
      Core.fieldTerm = Core.TermUnit}}))

languageName :: (Coders.LanguageName -> Core.Term)
languageName x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.coders.LanguageName"),
  Core.wrappedTermBody = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Coders.unLanguageName x))}))

traversalOrder :: (Coders.TraversalOrder -> Core.Term)
traversalOrder x = case x of
  Coders.TraversalOrderPre -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.coders.TraversalOrder"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "pre"),
      Core.fieldTerm = Core.TermUnit}}))
  Coders.TraversalOrderPost -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.coders.TraversalOrder"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "post"),
      Core.fieldTerm = Core.TermUnit}}))
