-- Note: this is an automatically generated file. Do not edit.

-- | Functions for generating term decoders from type modules

module Hydra.Decoding where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Schemas as Schemas
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Transform a type binding into a decoder binding
decodeBinding :: (Core.Binding -> Compute.Flow Graph.Graph Core.Binding)
decodeBinding b = (Flows.bind (Core_.type_ (Core.bindingTerm b)) (\typ -> Flows.pure (Core.Binding {
  Core.bindingName = (decodeBindingName (Core.bindingName b)),
  Core.bindingTerm = (decodeType typ),
  Core.bindingType = (Just (decoderTypeScheme typ))})))

-- | Generate a binding name for a decoder function from a type name
decodeBindingName :: (Core.Name -> Core.Name)
decodeBindingName n = (Logic.ifElse (Logic.not (Lists.null (Lists.tail (Strings.splitOn "." (Core.unName n))))) (Core.Name (Strings.intercalate "." (Lists.concat2 [
  "hydra",
  "decodeNew"] (Lists.concat2 (Lists.tail (Lists.init (Strings.splitOn "." (Core.unName n)))) [
  Formatting.decapitalize (Names.localNameOf n)])))) (Core.Name (Formatting.decapitalize (Names.localNameOf n))))

-- | Generate a decoder for a literal type
decodeLiteralType :: (Core.LiteralType -> Core.Term)
decodeLiteralType x = case x of
  Core.LiteralTypeBinary -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
    Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
    Core.caseStatementCases = [
      Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "v"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
              Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected binary literal"))}))))),
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "binary"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "b"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "b"))))})))}]})))),
            Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]}))))
  Core.LiteralTypeBoolean -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
    Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
    Core.caseStatementCases = [
      Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "v"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
              Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected boolean literal"))}))))),
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "boolean"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "b"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "b"))))})))}]})))),
            Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]}))))
  Core.LiteralTypeFloat v1 -> ((\x -> case x of
    Core.FloatTypeBigfloat -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
      Core.caseStatementCases = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "v"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                    "expected ",
                    "bigfloat",
                    " literal"])))}))))),
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "float"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.FloatValue"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                          "expected ",
                          "bigfloat",
                          " value"])))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "bigfloat"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "f"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "f"))))})))}]}))))}]})))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]}))))
    Core.FloatTypeFloat32 -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
      Core.caseStatementCases = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "v"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                    "expected ",
                    "float32",
                    " literal"])))}))))),
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "float"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.FloatValue"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                          "expected ",
                          "float32",
                          " value"])))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "float32"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "f"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "f"))))})))}]}))))}]})))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]}))))
    Core.FloatTypeFloat64 -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
      Core.caseStatementCases = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "v"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                    "expected ",
                    "float64",
                    " literal"])))}))))),
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "float"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.FloatValue"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                          "expected ",
                          "float64",
                          " value"])))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "float64"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "f"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "f"))))})))}]}))))}]})))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]}))))) v1)
  Core.LiteralTypeInteger v1 -> ((\x -> case x of
    Core.IntegerTypeBigint -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
      Core.caseStatementCases = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "v"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                    "expected ",
                    "bigint",
                    " literal"])))}))))),
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "integer"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                          "expected ",
                          "bigint",
                          " value"])))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "bigint"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "i"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))})))}]}))))}]})))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]}))))
    Core.IntegerTypeInt8 -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
      Core.caseStatementCases = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "v"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                    "expected ",
                    "int8",
                    " literal"])))}))))),
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "integer"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                          "expected ",
                          "int8",
                          " value"])))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "int8"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "i"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))})))}]}))))}]})))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]}))))
    Core.IntegerTypeInt16 -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
      Core.caseStatementCases = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "v"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                    "expected ",
                    "int16",
                    " literal"])))}))))),
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "integer"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                          "expected ",
                          "int16",
                          " value"])))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "int16"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "i"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))})))}]}))))}]})))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]}))))
    Core.IntegerTypeInt32 -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
      Core.caseStatementCases = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "v"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                    "expected ",
                    "int32",
                    " literal"])))}))))),
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "integer"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                          "expected ",
                          "int32",
                          " value"])))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "int32"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "i"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))})))}]}))))}]})))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]}))))
    Core.IntegerTypeInt64 -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
      Core.caseStatementCases = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "v"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                    "expected ",
                    "int64",
                    " literal"])))}))))),
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "integer"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                          "expected ",
                          "int64",
                          " value"])))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "int64"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "i"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))})))}]}))))}]})))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]}))))
    Core.IntegerTypeUint8 -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
      Core.caseStatementCases = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "v"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                    "expected ",
                    "uint8",
                    " literal"])))}))))),
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "integer"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                          "expected ",
                          "uint8",
                          " value"])))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "uint8"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "i"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))})))}]}))))}]})))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]}))))
    Core.IntegerTypeUint16 -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
      Core.caseStatementCases = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "v"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                    "expected ",
                    "uint16",
                    " literal"])))}))))),
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "integer"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                          "expected ",
                          "uint16",
                          " value"])))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "uint16"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "i"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))})))}]}))))}]})))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]}))))
    Core.IntegerTypeUint32 -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
      Core.caseStatementCases = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "v"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                    "expected ",
                    "uint32",
                    " literal"])))}))))),
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "integer"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                          "expected ",
                          "uint32",
                          " value"])))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "uint32"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "i"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))})))}]}))))}]})))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]}))))
    Core.IntegerTypeUint64 -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
      Core.caseStatementCases = [
        Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "v"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                    "expected ",
                    "uint64",
                    " literal"])))}))))),
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "integer"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                          "expected ",
                          "uint64",
                          " value"])))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "uint64"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "i"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))})))}]}))))}]})))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]}))))) v1)
  Core.LiteralTypeString -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
    Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
    Core.caseStatementCases = [
      Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "v"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
              Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected string literal"))}))))),
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "string"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "s"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "s"))))})))}]})))),
            Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]}))))

-- | Transform a type module into a decoder module
decodeModule :: (Module.Module -> Compute.Flow Graph.Graph (Maybe Module.Module))
decodeModule mod = (Flows.bind (filterTypeBindings (Module.moduleElements mod)) (\typeBindings -> Logic.ifElse (Lists.null typeBindings) (Flows.pure Nothing) (Flows.bind (Flows.mapList decodeBinding typeBindings) (\decodedBindings -> Flows.pure (Just (Module.Module {
  Module.moduleNamespace = (decodeNamespace (Module.moduleNamespace mod)),
  Module.moduleElements = decodedBindings,
  Module.moduleTermDependencies = (Lists.map decodeNamespace (Module.moduleTypeDependencies mod)),
  Module.moduleTypeDependencies = [
    Module.moduleNamespace mod],
  Module.moduleDescription = (Just (Strings.cat [
    "Term decoders for ",
    (Module.unNamespace (Module.moduleNamespace mod))]))}))))))

-- | Generate a decoder module namespace from a source module namespace
decodeNamespace :: (Module.Namespace -> Module.Namespace)
decodeNamespace ns = (Module.Namespace (Strings.cat [
  "hydra.decodeNew.",
  (Strings.intercalate "." (Lists.tail (Strings.splitOn "." (Module.unNamespace ns))))]))

-- | Generate a decoder for a record type
decodeRecordType :: (Core.RowType -> Core.Term)
decodeRecordType rt = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
  Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
  Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
    Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
      "expected record of type ",
      (Core.unName (Core.rowTypeTypeName rt))])))}))))),
  Core.caseStatementCases = [
    Core.Field {
      Core.fieldName = (Core.Name "record"),
      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "record"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))})))}]}))))

-- | Generate a decoder term for a Type
decodeType :: (Core.Type -> Core.Term)
decodeType x = case x of
  Core.TypeAnnotated v1 -> (decodeType (Core.annotatedTypeBody v1))
  Core.TypeLiteral v1 -> (decodeLiteralType v1)
  Core.TypeRecord v1 -> (decodeRecordType v1)
  Core.TypeUnion v1 -> (decodeUnionType v1)
  Core.TypeUnit -> decodeUnitType
  Core.TypeWrap v1 -> (decodeWrappedType v1)
  Core.TypeVariable v1 -> (Core.TermVariable (decodeBindingName v1))
  _ -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
    Core.lambdaParameter = (Core.Name "t"),
    Core.lambdaDomain = Nothing,
    Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unsupported type variant"))}))))})))

-- | Generate a decoder for a union type
decodeUnionType :: (Core.RowType -> Core.Term)
decodeUnionType rt =  
  let typeName = (Core.rowTypeTypeName rt)
  in  
    let toVariantPair = (\ft -> Core.TermPair (Core.TermWrap (Core.WrappedTerm {
            Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName (Core.fieldTypeName ft))))}), (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "term"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.map"))),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "t"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = typeName,
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.fieldTypeName ft),
                      Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))})))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (decodeType (Core.fieldTypeType ft)),
                Core.applicationArgument = (Core.TermVariable (Core.Name "term"))}))}))})))))
    in (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
          "expected union of type ",
          (Core.unName typeName)])))}))))),
      Core.caseStatementCases = [
        Core.Field {
          Core.fieldName = (Core.Name "union"),
          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "inj"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "tname"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                      Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
                      Core.projectionField = (Core.Name "typeName")})))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "inj"))})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "field"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                      Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
                      Core.projectionField = (Core.Name "field")})))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "inj"))})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "fname"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                      Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                      Core.projectionField = (Core.Name "name")})))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "field"))})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "fterm"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                      Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                      Core.projectionField = (Core.Name "term")})))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "field"))})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "variantMap"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.fromList"))),
                    Core.applicationArgument = (Core.TermList (Lists.map toVariantPair (Core.rowTypeFields rt)))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.maybe"))),
                    Core.applicationArgument = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                      Core.wrappedTermBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
                        Core.applicationArgument = (Core.TermList [
                          Core.TermLiteral (Core.LiteralString "no such field "),
                          Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.core.Name")))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))}),
                          Core.TermLiteral (Core.LiteralString " in union type "),
                          (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.core.Name")))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "tname"))}))])}))}))))})),
                  Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "f"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "fterm"))}))})))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.lookup"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "variantMap"))}))}))}))})))}]}))))

-- | Generate a decoder for the unit type
decodeUnitType :: Core.Term
decodeUnitType = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
  Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
  Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
    Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected a unit value"))}))))),
  Core.caseStatementCases = [
    Core.Field {
      Core.fieldName = (Core.Name "unit"),
      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "_"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermEither (Right Core.TermUnit))})))}]}))))

-- | Generate a decoder for a wrapped type
decodeWrappedType :: (Core.WrappedType -> Core.Term)
decodeWrappedType wt = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
  Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
  Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
    Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
      "expected wrapped type ",
      (Core.unName (Core.wrappedTypeTypeName wt))])))}))))),
  Core.caseStatementCases = [
    Core.Field {
      Core.fieldName = (Core.Name "wrap"),
      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "wrappedTerm"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.map"))),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "b"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.wrappedTypeTypeName wt),
                Core.wrappedTermBody = (Core.TermVariable (Core.Name "b"))}))})))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (decodeType (Core.wrappedTypeBody wt)),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
                Core.projectionField = (Core.Name "body")})))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "wrappedTerm"))}))}))}))})))}]}))))

-- | Compute the result type name for a decoder
decoderResultType :: (Core.Type -> Core.Name)
decoderResultType x = case x of
  Core.TypeAnnotated v1 -> (decoderResultType (Core.annotatedTypeBody v1))
  Core.TypeLiteral _ -> (Core.Name "hydra.core.Literal")
  Core.TypeRecord v1 -> (Core.rowTypeTypeName v1)
  Core.TypeUnion v1 -> (Core.rowTypeTypeName v1)
  Core.TypeWrap v1 -> (Core.wrappedTypeTypeName v1)
  _ -> (Core.Name "hydra.core.Term")

-- | Build type scheme for a decoder function
decoderTypeScheme :: (Core.Type -> Core.TypeScheme)
decoderTypeScheme typ = Core.TypeScheme {
  Core.typeSchemeVariables = [],
  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
    Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
      Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.util.DecodingError")),
      Core.eitherTypeRight = (Core.TypeVariable (decoderResultType typ))}))}))}

-- | Filter bindings to only decodable type definitions
filterTypeBindings :: ([Core.Binding] -> Compute.Flow Graph.Graph [Core.Binding])
filterTypeBindings bindings = (Flows.map Maybes.cat (Flows.mapList isDecodableBinding (Lists.filter Annotations.isNativeType bindings)))

-- | Check if a binding is decodable (serializable type)
isDecodableBinding :: (Core.Binding -> Compute.Flow Graph.Graph (Maybe Core.Binding))
isDecodableBinding b = (Flows.map (\serializable -> Logic.ifElse serializable (Just b) Nothing) (Schemas.isSerializableByName (Core.bindingName b)))
