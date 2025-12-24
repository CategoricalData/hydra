-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.parsing

module Hydra.Encode.Parsing where

import qualified Hydra.Core as Core
import qualified Hydra.Parsing as Parsing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

parseError :: (Parsing.ParseError -> Core.Term)
parseError x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.parsing.ParseError"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "message"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Parsing.parseErrorMessage x))},
    Core.Field {
      Core.fieldName = (Core.Name "remainder"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Parsing.parseErrorRemainder x))}]}))

parseResult :: ((t0 -> Core.Term) -> Parsing.ParseResult t0 -> Core.Term)
parseResult a x = case x of
  Parsing.ParseResultSuccess v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.parsing.ParseResult"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "success"),
      Core.fieldTerm = (parseSuccess a v1)}}))
  Parsing.ParseResultFailure v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.parsing.ParseResult"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "failure"),
      Core.fieldTerm = (parseError v1)}}))

parseSuccess :: ((t0 -> Core.Term) -> Parsing.ParseSuccess t0 -> Core.Term)
parseSuccess a x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "value"),
      Core.fieldTerm = (a (Parsing.parseSuccessValue x))},
    Core.Field {
      Core.fieldName = (Core.Name "remainder"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Parsing.parseSuccessRemainder x))}]}))
