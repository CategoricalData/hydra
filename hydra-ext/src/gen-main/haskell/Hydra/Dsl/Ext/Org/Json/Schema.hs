-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.org.json.schema

module Hydra.Dsl.Ext.Org.Json.Schema where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Org.Json.Schema as Schema
import qualified Hydra.Json.Model as Model
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

document :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm (Maybe (M.Map Schema.Keyword Schema.Schema)) -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Schema.Document
document id definitions root =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.json.schema.Document"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Phantoms.unTTerm definitions)},
        Core.Field {
          Core.fieldName = (Core.Name "root"),
          Core.fieldTerm = (Phantoms.unTTerm root)}]}))

documentId :: Phantoms.TTerm Schema.Document -> Phantoms.TTerm (Maybe String)
documentId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.json.schema.Document"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

documentDefinitions :: Phantoms.TTerm Schema.Document -> Phantoms.TTerm (Maybe (M.Map Schema.Keyword Schema.Schema))
documentDefinitions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.json.schema.Document"),
        Core.projectionField = (Core.Name "definitions")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

documentRoot :: Phantoms.TTerm Schema.Document -> Phantoms.TTerm Schema.Schema
documentRoot x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.json.schema.Document"),
        Core.projectionField = (Core.Name "root")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

documentWithId :: Phantoms.TTerm Schema.Document -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Schema.Document
documentWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.json.schema.Document"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.json.schema.Document"),
              Core.projectionField = (Core.Name "definitions")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "root"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.json.schema.Document"),
              Core.projectionField = (Core.Name "root")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

documentWithDefinitions :: Phantoms.TTerm Schema.Document -> Phantoms.TTerm (Maybe (M.Map Schema.Keyword Schema.Schema)) -> Phantoms.TTerm Schema.Document
documentWithDefinitions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.json.schema.Document"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.json.schema.Document"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "root"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.json.schema.Document"),
              Core.projectionField = (Core.Name "root")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

documentWithRoot :: Phantoms.TTerm Schema.Document -> Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Schema.Document
documentWithRoot original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.json.schema.Document"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.json.schema.Document"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.json.schema.Document"),
              Core.projectionField = (Core.Name "definitions")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "root"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

keyword :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Keyword
keyword x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.json.schema.Keyword"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unKeyword :: Phantoms.TTerm Schema.Keyword -> Phantoms.TTerm String
unKeyword x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.json.schema.Keyword")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schema :: Phantoms.TTerm [Schema.Restriction] -> Phantoms.TTerm Schema.Schema
schema x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.json.schema.Schema"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unSchema :: Phantoms.TTerm Schema.Schema -> Phantoms.TTerm [Schema.Restriction]
unSchema x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.json.schema.Schema")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

restrictionType :: Phantoms.TTerm Schema.Type -> Phantoms.TTerm Schema.Restriction
restrictionType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.Restriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

restrictionString :: Phantoms.TTerm Schema.StringRestriction -> Phantoms.TTerm Schema.Restriction
restrictionString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.Restriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

restrictionNumber :: Phantoms.TTerm Schema.NumericRestriction -> Phantoms.TTerm Schema.Restriction
restrictionNumber x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.Restriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

restrictionArray :: Phantoms.TTerm Schema.ArrayRestriction -> Phantoms.TTerm Schema.Restriction
restrictionArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.Restriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

restrictionObject :: Phantoms.TTerm Schema.ObjectRestriction -> Phantoms.TTerm Schema.Restriction
restrictionObject x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.Restriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

restrictionMultiple :: Phantoms.TTerm Schema.MultipleRestriction -> Phantoms.TTerm Schema.Restriction
restrictionMultiple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.Restriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

restrictionReference :: Phantoms.TTerm Schema.SchemaReference -> Phantoms.TTerm Schema.Restriction
restrictionReference x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.Restriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

restrictionTitle :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Restriction
restrictionTitle x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.Restriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "title"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

restrictionDescription :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Restriction
restrictionDescription x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.Restriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "description"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeSingle :: Phantoms.TTerm Schema.TypeName -> Phantoms.TTerm Schema.Type
typeSingle x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeMultiple :: Phantoms.TTerm [Schema.TypeName] -> Phantoms.TTerm Schema.Type
typeMultiple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeNameString :: Phantoms.TTerm Schema.TypeName
typeNameString =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.TypeName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = Core.TermUnit}}))

typeNameInteger :: Phantoms.TTerm Schema.TypeName
typeNameInteger =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.TypeName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = Core.TermUnit}}))

typeNameNumber :: Phantoms.TTerm Schema.TypeName
typeNameNumber =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.TypeName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = Core.TermUnit}}))

typeNameBoolean :: Phantoms.TTerm Schema.TypeName
typeNameBoolean =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.TypeName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = Core.TermUnit}}))

typeNameNull :: Phantoms.TTerm Schema.TypeName
typeNameNull =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.TypeName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))

typeNameArray :: Phantoms.TTerm Schema.TypeName
typeNameArray =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.TypeName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = Core.TermUnit}}))

typeNameObject :: Phantoms.TTerm Schema.TypeName
typeNameObject =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.TypeName"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = Core.TermUnit}}))

stringRestrictionMinLength :: Phantoms.TTerm Int -> Phantoms.TTerm Schema.StringRestriction
stringRestrictionMinLength x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.StringRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minLength"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringRestrictionMaxLength :: Phantoms.TTerm Int -> Phantoms.TTerm Schema.StringRestriction
stringRestrictionMaxLength x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.StringRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxLength"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringRestrictionPattern :: Phantoms.TTerm Schema.RegularExpression -> Phantoms.TTerm Schema.StringRestriction
stringRestrictionPattern x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.StringRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

regularExpression :: Phantoms.TTerm String -> Phantoms.TTerm Schema.RegularExpression
regularExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.json.schema.RegularExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unRegularExpression :: Phantoms.TTerm Schema.RegularExpression -> Phantoms.TTerm String
unRegularExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.json.schema.RegularExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

numericRestrictionMinimum :: Phantoms.TTerm Schema.Limit -> Phantoms.TTerm Schema.NumericRestriction
numericRestrictionMinimum x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.NumericRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minimum"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

numericRestrictionMaximum :: Phantoms.TTerm Schema.Limit -> Phantoms.TTerm Schema.NumericRestriction
numericRestrictionMaximum x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.NumericRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maximum"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

numericRestrictionMultipleOf :: Phantoms.TTerm Int -> Phantoms.TTerm Schema.NumericRestriction
numericRestrictionMultipleOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.NumericRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multipleOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

limit :: Phantoms.TTerm Int -> Phantoms.TTerm Bool -> Phantoms.TTerm Schema.Limit
limit value exclusive =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.json.schema.Limit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "exclusive"),
          Core.fieldTerm = (Phantoms.unTTerm exclusive)}]}))

limitValue :: Phantoms.TTerm Schema.Limit -> Phantoms.TTerm Int
limitValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.json.schema.Limit"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

limitExclusive :: Phantoms.TTerm Schema.Limit -> Phantoms.TTerm Bool
limitExclusive x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.org.json.schema.Limit"),
        Core.projectionField = (Core.Name "exclusive")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

limitWithValue :: Phantoms.TTerm Schema.Limit -> Phantoms.TTerm Int -> Phantoms.TTerm Schema.Limit
limitWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.json.schema.Limit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "exclusive"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.json.schema.Limit"),
              Core.projectionField = (Core.Name "exclusive")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

limitWithExclusive :: Phantoms.TTerm Schema.Limit -> Phantoms.TTerm Bool -> Phantoms.TTerm Schema.Limit
limitWithExclusive original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.org.json.schema.Limit"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.org.json.schema.Limit"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exclusive"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

arrayRestrictionItems :: Phantoms.TTerm Schema.Items -> Phantoms.TTerm Schema.ArrayRestriction
arrayRestrictionItems x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.ArrayRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "items"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrayRestrictionAdditionalItems :: Phantoms.TTerm Schema.AdditionalItems -> Phantoms.TTerm Schema.ArrayRestriction
arrayRestrictionAdditionalItems x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.ArrayRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "additionalItems"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrayRestrictionMinItems :: Phantoms.TTerm Int -> Phantoms.TTerm Schema.ArrayRestriction
arrayRestrictionMinItems x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.ArrayRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minItems"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrayRestrictionMaxItems :: Phantoms.TTerm Int -> Phantoms.TTerm Schema.ArrayRestriction
arrayRestrictionMaxItems x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.ArrayRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxItems"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrayRestrictionUniqueItems :: Phantoms.TTerm Bool -> Phantoms.TTerm Schema.ArrayRestriction
arrayRestrictionUniqueItems x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.ArrayRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uniqueItems"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

itemsSameItems :: Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Schema.Items
itemsSameItems x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.Items"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sameItems"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

itemsVarItems :: Phantoms.TTerm [Schema.Schema] -> Phantoms.TTerm Schema.Items
itemsVarItems x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.Items"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "varItems"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

additionalItemsAny :: Phantoms.TTerm Bool -> Phantoms.TTerm Schema.AdditionalItems
additionalItemsAny x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.AdditionalItems"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "any"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

additionalItemsSchema :: Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Schema.AdditionalItems
additionalItemsSchema x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.AdditionalItems"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "schema"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectRestrictionProperties :: Phantoms.TTerm (M.Map Schema.Keyword Schema.Schema) -> Phantoms.TTerm Schema.ObjectRestriction
objectRestrictionProperties x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.ObjectRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "properties"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectRestrictionAdditionalProperties :: Phantoms.TTerm Schema.AdditionalItems -> Phantoms.TTerm Schema.ObjectRestriction
objectRestrictionAdditionalProperties x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.ObjectRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "additionalProperties"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectRestrictionRequired :: Phantoms.TTerm [Schema.Keyword] -> Phantoms.TTerm Schema.ObjectRestriction
objectRestrictionRequired x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.ObjectRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "required"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectRestrictionMinProperties :: Phantoms.TTerm Int -> Phantoms.TTerm Schema.ObjectRestriction
objectRestrictionMinProperties x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.ObjectRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minProperties"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectRestrictionMaxProperties :: Phantoms.TTerm Int -> Phantoms.TTerm Schema.ObjectRestriction
objectRestrictionMaxProperties x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.ObjectRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "maxProperties"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectRestrictionDependencies :: Phantoms.TTerm (M.Map Schema.Keyword Schema.SchemaOrArray) -> Phantoms.TTerm Schema.ObjectRestriction
objectRestrictionDependencies x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.ObjectRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dependencies"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectRestrictionPatternProperties :: Phantoms.TTerm (M.Map Schema.RegularExpression Schema.Schema) -> Phantoms.TTerm Schema.ObjectRestriction
objectRestrictionPatternProperties x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.ObjectRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "patternProperties"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

schemaOrArraySchema :: Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Schema.SchemaOrArray
schemaOrArraySchema x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.SchemaOrArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "schema"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

schemaOrArrayArray :: Phantoms.TTerm [Schema.Keyword] -> Phantoms.TTerm Schema.SchemaOrArray
schemaOrArrayArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.SchemaOrArray"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

multipleRestrictionAllOf :: Phantoms.TTerm [Schema.Schema] -> Phantoms.TTerm Schema.MultipleRestriction
multipleRestrictionAllOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.MultipleRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "allOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

multipleRestrictionAnyOf :: Phantoms.TTerm [Schema.Schema] -> Phantoms.TTerm Schema.MultipleRestriction
multipleRestrictionAnyOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.MultipleRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anyOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

multipleRestrictionOneOf :: Phantoms.TTerm [Schema.Schema] -> Phantoms.TTerm Schema.MultipleRestriction
multipleRestrictionOneOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.MultipleRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "oneOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

multipleRestrictionNot :: Phantoms.TTerm Schema.Schema -> Phantoms.TTerm Schema.MultipleRestriction
multipleRestrictionNot x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.MultipleRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

multipleRestrictionEnum :: Phantoms.TTerm [Model.Value] -> Phantoms.TTerm Schema.MultipleRestriction
multipleRestrictionEnum x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.org.json.schema.MultipleRestriction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enum"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

schemaReference :: Phantoms.TTerm String -> Phantoms.TTerm Schema.SchemaReference
schemaReference x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.json.schema.SchemaReference"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unSchemaReference :: Phantoms.TTerm Schema.SchemaReference -> Phantoms.TTerm String
unSchemaReference x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.json.schema.SchemaReference")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
