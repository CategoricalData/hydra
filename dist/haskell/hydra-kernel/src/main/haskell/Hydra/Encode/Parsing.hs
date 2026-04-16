-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.parsing

module Hydra.Encode.Parsing where

import qualified Hydra.Core as Core
import qualified Hydra.Parsing as Parsing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

parseError :: Parsing.ParseError -> Core.Term
parseError x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parsing.ParseError"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "message"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Parsing.parseErrorMessage x))},
        Core.Field {
          Core.fieldName = (Core.Name "remainder"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Parsing.parseErrorRemainder x))}]})

parseResult :: (t0 -> Core.Term) -> Parsing.ParseResult t0 -> Core.Term
parseResult a x =
    case x of
      Parsing.ParseResultSuccess v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.parsing.ParseResult"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "success"),
          Core.fieldTerm = (parseSuccess a v0)}})
      Parsing.ParseResultFailure v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.parsing.ParseResult"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "failure"),
          Core.fieldTerm = (parseError v0)}})

parseSuccess :: (t0 -> Core.Term) -> Parsing.ParseSuccess t0 -> Core.Term
parseSuccess a x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (a (Parsing.parseSuccessValue x))},
        Core.Field {
          Core.fieldName = (Core.Name "remainder"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Parsing.parseSuccessRemainder x))}]})
