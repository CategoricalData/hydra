-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.parsing

module Hydra.Dsl.Parsing where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL constructor for hydra.parsing.ParseError
parseError :: Typed.TypedTerm String -> Typed.TypedTerm String -> Typed.TypedTerm Parsing.ParseError
parseError message remainder =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parsing.ParseError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Typed.unTypedTerm message)},
        Core.Field {
          Core.fieldName = (Core.Name "remainder"),
          Core.fieldTerm = (Typed.unTypedTerm remainder)}]}))
-- | DSL accessor for the message field of hydra.parsing.ParseError
parseErrorMessage :: Typed.TypedTerm Parsing.ParseError -> Typed.TypedTerm String
parseErrorMessage x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parsing.ParseError"),
        Core.projectionFieldName = (Core.Name "message")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the remainder field of hydra.parsing.ParseError
parseErrorRemainder :: Typed.TypedTerm Parsing.ParseError -> Typed.TypedTerm String
parseErrorRemainder x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parsing.ParseError"),
        Core.projectionFieldName = (Core.Name "remainder")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the message field of hydra.parsing.ParseError
parseErrorWithMessage :: Typed.TypedTerm Parsing.ParseError -> Typed.TypedTerm String -> Typed.TypedTerm Parsing.ParseError
parseErrorWithMessage original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parsing.ParseError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "remainder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parsing.ParseError"),
              Core.projectionFieldName = (Core.Name "remainder")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the remainder field of hydra.parsing.ParseError
parseErrorWithRemainder :: Typed.TypedTerm Parsing.ParseError -> Typed.TypedTerm String -> Typed.TypedTerm Parsing.ParseError
parseErrorWithRemainder original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parsing.ParseError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parsing.ParseError"),
              Core.projectionFieldName = (Core.Name "message")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "remainder"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the failure variant of hydra.parsing.ParseResult
parseResultFailure :: Typed.TypedTerm Parsing.ParseError -> Typed.TypedTerm (Parsing.ParseResult a)
parseResultFailure x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parsing.ParseResult"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "failure"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the success variant of hydra.parsing.ParseResult
parseResultSuccess :: Typed.TypedTerm (Parsing.ParseSuccess a) -> Typed.TypedTerm (Parsing.ParseResult a)
parseResultSuccess x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parsing.ParseResult"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "success"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.parsing.ParseSuccess
parseSuccess :: Typed.TypedTerm a -> Typed.TypedTerm String -> Typed.TypedTerm (Parsing.ParseSuccess a)
parseSuccess value remainder =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "remainder"),
          Core.fieldTerm = (Typed.unTypedTerm remainder)}]}))
-- | DSL accessor for the remainder field of hydra.parsing.ParseSuccess
parseSuccessRemainder :: Typed.TypedTerm (Parsing.ParseSuccess a) -> Typed.TypedTerm String
parseSuccessRemainder x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
        Core.projectionFieldName = (Core.Name "remainder")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.parsing.ParseSuccess
parseSuccessValue :: Typed.TypedTerm (Parsing.ParseSuccess a) -> Typed.TypedTerm a
parseSuccessValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the remainder field of hydra.parsing.ParseSuccess
parseSuccessWithRemainder :: Typed.TypedTerm (Parsing.ParseSuccess a) -> Typed.TypedTerm String -> Typed.TypedTerm (Parsing.ParseSuccess a)
parseSuccessWithRemainder original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "remainder"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the value field of hydra.parsing.ParseSuccess
parseSuccessWithValue :: Typed.TypedTerm (Parsing.ParseSuccess a) -> Typed.TypedTerm a -> Typed.TypedTerm (Parsing.ParseSuccess a)
parseSuccessWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "remainder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
              Core.projectionFieldName = (Core.Name "remainder")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.parsing.Parser wrapper
parser :: Typed.TypedTerm (String -> Parsing.ParseResult a) -> Typed.TypedTerm (Parsing.Parser a)
parser x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.parsing.Parser"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.parsing.Parser
unParser :: Typed.TypedTerm (Parsing.Parser a) -> Typed.TypedTerm (String -> Parsing.ParseResult a)
unParser x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.parsing.Parser")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
