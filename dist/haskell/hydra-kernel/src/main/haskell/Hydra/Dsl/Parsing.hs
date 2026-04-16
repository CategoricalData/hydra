-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.parsing

module Hydra.Dsl.Parsing where

import qualified Hydra.Core as Core
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

parseError :: Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm Parsing.ParseError
parseError message remainder =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parsing.ParseError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTTerm message)},
        Core.Field {
          Core.fieldName = (Core.Name "remainder"),
          Core.fieldTerm = (Phantoms.unTTerm remainder)}]}))

parseErrorMessage :: Phantoms.TTerm Parsing.ParseError -> Phantoms.TTerm String
parseErrorMessage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parsing.ParseError"),
        Core.projectionField = (Core.Name "message")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parseErrorRemainder :: Phantoms.TTerm Parsing.ParseError -> Phantoms.TTerm String
parseErrorRemainder x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parsing.ParseError"),
        Core.projectionField = (Core.Name "remainder")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parseErrorWithMessage :: Phantoms.TTerm Parsing.ParseError -> Phantoms.TTerm String -> Phantoms.TTerm Parsing.ParseError
parseErrorWithMessage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parsing.ParseError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "remainder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parsing.ParseError"),
              Core.projectionField = (Core.Name "remainder")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

parseErrorWithRemainder :: Phantoms.TTerm Parsing.ParseError -> Phantoms.TTerm String -> Phantoms.TTerm Parsing.ParseError
parseErrorWithRemainder original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parsing.ParseError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parsing.ParseError"),
              Core.projectionField = (Core.Name "message")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "remainder"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

parseResultFailure :: Phantoms.TTerm Parsing.ParseError -> Phantoms.TTerm (Parsing.ParseResult a)
parseResultFailure x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parsing.ParseResult"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "failure"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

parseResultSuccess :: Phantoms.TTerm (Parsing.ParseSuccess a) -> Phantoms.TTerm (Parsing.ParseResult a)
parseResultSuccess x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parsing.ParseResult"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "success"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

parseSuccess :: Phantoms.TTerm a -> Phantoms.TTerm String -> Phantoms.TTerm (Parsing.ParseSuccess a)
parseSuccess value remainder =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "remainder"),
          Core.fieldTerm = (Phantoms.unTTerm remainder)}]}))

parseSuccessRemainder :: Phantoms.TTerm (Parsing.ParseSuccess a) -> Phantoms.TTerm String
parseSuccessRemainder x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
        Core.projectionField = (Core.Name "remainder")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parseSuccessValue :: Phantoms.TTerm (Parsing.ParseSuccess a) -> Phantoms.TTerm a
parseSuccessValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parseSuccessWithRemainder :: Phantoms.TTerm (Parsing.ParseSuccess a) -> Phantoms.TTerm String -> Phantoms.TTerm (Parsing.ParseSuccess a)
parseSuccessWithRemainder original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "remainder"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

parseSuccessWithValue :: Phantoms.TTerm (Parsing.ParseSuccess a) -> Phantoms.TTerm a -> Phantoms.TTerm (Parsing.ParseSuccess a)
parseSuccessWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "remainder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
              Core.projectionField = (Core.Name "remainder")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

parser :: Phantoms.TTerm (String -> Parsing.ParseResult a) -> Phantoms.TTerm (Parsing.Parser a)
parser x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.parsing.Parser"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unParser :: Phantoms.TTerm (Parsing.Parser a) -> Phantoms.TTerm (String -> Parsing.ParseResult a)
unParser x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.parsing.Parser")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
