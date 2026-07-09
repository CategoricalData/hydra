-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.parsing

module Hydra.Dsl.Parsing where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Parsing as DecodeParsing
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Encode.Parsing as EncodeParsing
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | DSL composition builder for the decoder of hydra.parsing.ParseResult
decodeParseResult :: Typed.TypedTerm (Graph.Graph -> Core.Term -> Either Errors.DecodingError a) -> Typed.TypedTerm (Graph.Graph -> Core.Term -> Either Errors.DecodingError (Parsing.ParseResult a))
decodeParseResult a =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.parsing.parseResult")),
      Core.applicationArgument = (Typed.unTypedTerm a)}))

-- | DSL composition builder for the decoder of hydra.parsing.ParseSuccess
decodeParseSuccess :: Typed.TypedTerm (Graph.Graph -> Core.Term -> Either Errors.DecodingError a) -> Typed.TypedTerm (Graph.Graph -> Core.Term -> Either Errors.DecodingError (Parsing.ParseSuccess a))
decodeParseSuccess a =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.parsing.parseSuccess")),
      Core.applicationArgument = (Typed.unTypedTerm a)}))

-- | DSL composition builder for the decoder of hydra.parsing.Parser
decodeParser :: Typed.TypedTerm (Graph.Graph -> Core.Term -> Either Errors.DecodingError a) -> Typed.TypedTerm (Graph.Graph -> Core.Term -> Either Errors.DecodingError (Parsing.Parser a))
decodeParser a =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.parsing.parser")),
      Core.applicationArgument = (Typed.unTypedTerm a)}))

-- | DSL composition builder for the encoder of hydra.parsing.ParseResult
encodeParseResult :: Typed.TypedTerm (a -> Core.Term) -> Typed.TypedTerm (Parsing.ParseResult a -> Core.Term)
encodeParseResult a =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.parsing.parseResult")),
      Core.applicationArgument = (Typed.unTypedTerm a)}))

-- | DSL composition builder for the encoder of hydra.parsing.ParseSuccess
encodeParseSuccess :: Typed.TypedTerm (a -> Core.Term) -> Typed.TypedTerm (Parsing.ParseSuccess a -> Core.Term)
encodeParseSuccess a =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.parsing.parseSuccess")),
      Core.applicationArgument = (Typed.unTypedTerm a)}))

-- | DSL composition builder for the encoder of hydra.parsing.Parser
encodeParser :: Typed.TypedTerm (a -> Core.Term) -> Typed.TypedTerm (Parsing.Parser a -> Core.Term)
encodeParser a =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.parsing.parser")),
      Core.applicationArgument = (Typed.unTypedTerm a)}))

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

-- | DSL name token for hydra.parsing.ParseError
parseErrorParseError :: Typed.TypedName Parsing.ParseError
parseErrorParseError = Typed.TypedName (Core.Name "hydra.parsing.ParseError")

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

-- | DSL name token for hydra.parsing.ParseResult
parseResultParseResult :: Typed.TypedName (Parsing.ParseResult a)
parseResultParseResult = Typed.TypedName (Core.Name "hydra.parsing.ParseResult")

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

-- | DSL name token for hydra.parsing.ParseSuccess
parseSuccessParseSuccess :: Typed.TypedName (Parsing.ParseSuccess a)
parseSuccessParseSuccess = Typed.TypedName (Core.Name "hydra.parsing.ParseSuccess")

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

-- | DSL name token for hydra.parsing.Parser
parserParser :: Typed.TypedName (Parsing.Parser a)
parserParser = Typed.TypedName (Core.Name "hydra.parsing.Parser")

-- | DSL accessor for the body of hydra.parsing.Parser
unParser :: Typed.TypedTerm (Parsing.Parser a) -> Typed.TypedTerm (String -> Parsing.ParseResult a)
unParser x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.parsing.Parser")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
