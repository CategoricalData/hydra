-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.json.model

module Hydra.Encode.Json.Model where

import qualified Hydra.Core as Core
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

value :: Model.Value -> Core.Term
value x =
    case x of
      Model.ValueArray v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.json.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "array"),
          Core.fieldTerm = (Core.TermList (Lists.map value v0))}})
      Model.ValueBoolean v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.json.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "boolean"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralBoolean v0))}})
      Model.ValueNull -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.json.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "null"),
          Core.fieldTerm = Core.TermUnit}})
      Model.ValueNumber v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.json.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "number"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat v0)))}})
      Model.ValueObject v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.json.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermMap (Maps.bimap (\x -> Core.TermLiteral (Core.LiteralString x)) value v0))}})
      Model.ValueString v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.json.model.Value"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v0))}})
