-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.json

module Hydra.Encode.Json where

import qualified Hydra.Core as Core
import qualified Hydra.Json as Json
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

value :: (Json.Value -> Core.Term)
value x = case x of
  Json.ValueArray v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.json.Value"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "array"),
      Core.fieldTerm = (Core.TermList (Lists.map value v1))}}))
  Json.ValueBoolean v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.json.Value"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "boolean"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralBoolean v1))}}))
  Json.ValueNull -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.json.Value"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "null"),
      Core.fieldTerm = Core.TermUnit}}))
  Json.ValueNumber v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.json.Value"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "number"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat v1)))}}))
  Json.ValueObject v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.json.Value"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "object"),
      Core.fieldTerm = (Core.TermMap (Maps.bimap (\x -> Core.TermLiteral (Core.LiteralString x)) value v1))}}))
  Json.ValueString v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.json.Value"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "string"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v1))}}))
