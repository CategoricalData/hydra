-- Note: this is an automatically generated file. Do not edit.

-- | Functions for generating term decoders from type modules

module Hydra.Decoding where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Monads as Monads
import qualified Hydra.Names as Names
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Collect forall type variable names from a type
collectForallVariables :: (Core.Type -> [Core.Name])
collectForallVariables typ = ((\x -> case x of
  Core.TypeAnnotated v1 -> (collectForallVariables (Core.annotatedTypeBody v1))
  Core.TypeForall v1 -> (Lists.cons (Core.forallTypeParameter v1) (collectForallVariables (Core.forallTypeBody v1)))
  _ -> []) typ)

-- | Collect type variables needing Ord constraints (from Set element types)
collectOrdConstrainedVariables :: (Core.Type -> [Core.Name])
collectOrdConstrainedVariables typ = ((\x -> case x of
  Core.TypeAnnotated v1 -> (collectOrdConstrainedVariables (Core.annotatedTypeBody v1))
  Core.TypeApplication v1 -> (Lists.concat2 (collectOrdConstrainedVariables (Core.applicationTypeFunction v1)) (collectOrdConstrainedVariables (Core.applicationTypeArgument v1)))
  Core.TypeEither v1 -> (Lists.concat2 (collectOrdConstrainedVariables (Core.eitherTypeLeft v1)) (collectOrdConstrainedVariables (Core.eitherTypeRight v1)))
  Core.TypeForall v1 -> (collectOrdConstrainedVariables (Core.forallTypeBody v1))
  Core.TypeList v1 -> (collectOrdConstrainedVariables v1)
  Core.TypeMap v1 -> (Lists.concat2 (collectOrdConstrainedVariables (Core.mapTypeKeys v1)) (collectOrdConstrainedVariables (Core.mapTypeValues v1)))
  Core.TypeMaybe v1 -> (collectOrdConstrainedVariables v1)
  Core.TypePair v1 -> (Lists.concat2 (collectOrdConstrainedVariables (Core.pairTypeFirst v1)) (collectOrdConstrainedVariables (Core.pairTypeSecond v1)))
  Core.TypeRecord v1 -> (Lists.concat (Lists.map (\ft -> collectOrdConstrainedVariables (Core.fieldTypeType ft)) (Core.rowTypeFields v1)))
  Core.TypeSet v1 -> (Lists.concat2 (collectTypeVariablesFromType v1) (collectOrdConstrainedVariables v1))
  Core.TypeUnion v1 -> (Lists.concat (Lists.map (\ft -> collectOrdConstrainedVariables (Core.fieldTypeType ft)) (Core.rowTypeFields v1)))
  Core.TypeWrap v1 -> (collectOrdConstrainedVariables (Core.wrappedTypeBody v1))
  _ -> []) typ)

-- | Collect type variable names from a type (forall parameters only)
collectTypeVariables :: (Core.Type -> [Core.Name])
collectTypeVariables typ = (collectForallVariables typ)

-- | Collect all type variable names from a type expression
collectTypeVariablesFromType :: (Core.Type -> [Core.Name])
collectTypeVariablesFromType typ = ((\x -> case x of
  Core.TypeAnnotated v1 -> (collectTypeVariablesFromType (Core.annotatedTypeBody v1))
  Core.TypeApplication v1 -> (Lists.concat2 (collectTypeVariablesFromType (Core.applicationTypeFunction v1)) (collectTypeVariablesFromType (Core.applicationTypeArgument v1)))
  Core.TypeEither v1 -> (Lists.concat2 (collectTypeVariablesFromType (Core.eitherTypeLeft v1)) (collectTypeVariablesFromType (Core.eitherTypeRight v1)))
  Core.TypeForall v1 -> (collectTypeVariablesFromType (Core.forallTypeBody v1))
  Core.TypeList v1 -> (collectTypeVariablesFromType v1)
  Core.TypeMap v1 -> (Lists.concat2 (collectTypeVariablesFromType (Core.mapTypeKeys v1)) (collectTypeVariablesFromType (Core.mapTypeValues v1)))
  Core.TypeMaybe v1 -> (collectTypeVariablesFromType v1)
  Core.TypePair v1 -> (Lists.concat2 (collectTypeVariablesFromType (Core.pairTypeFirst v1)) (collectTypeVariablesFromType (Core.pairTypeSecond v1)))
  Core.TypeRecord v1 -> (Lists.concat (Lists.map (\ft -> collectTypeVariablesFromType (Core.fieldTypeType ft)) (Core.rowTypeFields v1)))
  Core.TypeSet v1 -> (collectTypeVariablesFromType v1)
  Core.TypeUnion v1 -> (Lists.concat (Lists.map (\ft -> collectTypeVariablesFromType (Core.fieldTypeType ft)) (Core.rowTypeFields v1)))
  Core.TypeVariable v1 -> [
    v1]
  Core.TypeWrap v1 -> (collectTypeVariablesFromType (Core.wrappedTypeBody v1))
  _ -> []) typ)

-- | Transform a type binding into a decoder binding
decodeBinding :: (Core.Binding -> Compute.Flow Graph.Graph Core.Binding)
decodeBinding b = (Flows.bind Monads.getState (\cx -> Flows.bind (Monads.eitherToFlow Util.unDecodingError (Core_.type_ cx (Core.bindingTerm b))) (\typ -> Flows.pure (Core.Binding {
  Core.bindingName = (decodeBindingName (Core.bindingName b)),
  Core.bindingTerm = (decodeType typ),
  Core.bindingType = (Just (decoderTypeScheme typ))}))))

-- | Generate a binding name for a decoder function from a type name
decodeBindingName :: (Core.Name -> Core.Name)
decodeBindingName n = (Logic.ifElse (Logic.not (Lists.null (Lists.tail (Strings.splitOn "." (Core.unName n))))) (Core.Name (Strings.intercalate "." (Lists.concat2 [
  "hydra",
  "decode"] (Lists.concat2 (Lists.tail (Lists.init (Strings.splitOn "." (Core.unName n)))) [
  Formatting.decapitalize (Names.localNameOf n)])))) (Core.Name (Formatting.decapitalize (Names.localNameOf n))))

-- | Generate a decoder for an Either type
decodeEitherType :: (Core.EitherType -> Core.Term)
decodeEitherType et =  
  let leftDecoder = (decodeType (Core.eitherTypeLeft et))
  in  
    let rightDecoder = (decodeType (Core.eitherTypeRight et))
    in (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.decodeEither")),
        Core.applicationArgument = leftDecoder})),
      Core.applicationArgument = rightDecoder}))

-- | Generate a decoder for a polymorphic (forall) type
decodeForallType :: (Core.ForallType -> Core.Term)
decodeForallType ft = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
  Core.lambdaParameter = (decodeBindingName (Core.forallTypeParameter ft)),
  Core.lambdaDomain = Nothing,
  Core.lambdaBody = (decodeType (Core.forallTypeBody ft))})))

-- | Generate a decoder for a list type
decodeListType :: (Core.Type -> Core.Term)
decodeListType elemType =  
  let elemDecoder = (decodeType elemType)
  in (Core.TermApplication (Core.Application {
    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.decodeList")),
    Core.applicationArgument = elemDecoder}))

-- | Generate a decoder for a literal type
decodeLiteralType :: (Core.LiteralType -> Core.Term)
decodeLiteralType lt = ((\x -> case x of
  Core.LiteralTypeBinary -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
    Core.lambdaParameter = (Core.Name "cx"),
    Core.lambdaDomain = Nothing,
    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "raw"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "err"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "stripped"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
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
                        Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]})))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
          Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))
  Core.LiteralTypeBoolean -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
    Core.lambdaParameter = (Core.Name "cx"),
    Core.lambdaDomain = Nothing,
    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "raw"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "err"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "stripped"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
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
                        Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]})))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
          Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))
  Core.LiteralTypeFloat v1 -> ((\x -> case x of
    Core.FloatTypeBigfloat -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "cx"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "raw"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "err"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "stripped"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
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
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))
    Core.FloatTypeFloat32 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "cx"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "raw"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "err"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "stripped"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
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
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))
    Core.FloatTypeFloat64 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "cx"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "raw"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "err"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "stripped"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
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
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))) v1)
  Core.LiteralTypeInteger v1 -> ((\x -> case x of
    Core.IntegerTypeBigint -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "cx"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "raw"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "err"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "stripped"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
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
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))
    Core.IntegerTypeInt8 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "cx"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "raw"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "err"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "stripped"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
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
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))
    Core.IntegerTypeInt16 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "cx"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "raw"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "err"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "stripped"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
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
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))
    Core.IntegerTypeInt32 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "cx"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "raw"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "err"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "stripped"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
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
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))
    Core.IntegerTypeInt64 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "cx"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "raw"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "err"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "stripped"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
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
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))
    Core.IntegerTypeUint8 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "cx"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "raw"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "err"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "stripped"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
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
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))
    Core.IntegerTypeUint16 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "cx"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "raw"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "err"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "stripped"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
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
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))
    Core.IntegerTypeUint32 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "cx"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "raw"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "err"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "stripped"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
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
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))
    Core.IntegerTypeUint64 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "cx"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "raw"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "err"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "stripped"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
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
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))) v1)
  Core.LiteralTypeString -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
    Core.lambdaParameter = (Core.Name "cx"),
    Core.lambdaDomain = Nothing,
    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "raw"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "err"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "stripped"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
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
                        Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]})))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
          Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))) lt)

-- | Generate a decoder for a map type
decodeMapType :: (Core.MapType -> Core.Term)
decodeMapType mt =  
  let keyDecoder = (decodeType (Core.mapTypeKeys mt))
  in  
    let valDecoder = (decodeType (Core.mapTypeValues mt))
    in (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.decodeMap")),
        Core.applicationArgument = keyDecoder})),
      Core.applicationArgument = valDecoder}))

-- | Generate a decoder for an optional type
decodeMaybeType :: (Core.Type -> Core.Term)
decodeMaybeType elemType =  
  let elemDecoder = (decodeType elemType)
  in (Core.TermApplication (Core.Application {
    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.decodeMaybe")),
    Core.applicationArgument = elemDecoder}))

-- | Transform a type module into a decoder module
decodeModule :: (Module.Module -> Compute.Flow Graph.Graph (Maybe Module.Module))
decodeModule mod = (Flows.bind (filterTypeBindings (Module.moduleElements mod)) (\typeBindings -> Logic.ifElse (Lists.null typeBindings) (Flows.pure Nothing) (Flows.bind (Flows.mapList decodeBinding typeBindings) (\decodedBindings ->  
  let decodedTypeDeps = (Lists.map decodeNamespace (Module.moduleTypeDependencies mod))
  in  
    let decodedTermDeps = (Lists.map decodeNamespace (Module.moduleTermDependencies mod))
    in  
      let allDecodedDeps = (Lists.nub (Lists.concat2 decodedTypeDeps decodedTermDeps))
      in (Flows.pure (Just (Module.Module {
        Module.moduleNamespace = (decodeNamespace (Module.moduleNamespace mod)),
        Module.moduleElements = decodedBindings,
        Module.moduleTermDependencies = (Lists.concat2 [
          Module.Namespace "hydra.extract.helpers",
          Module.Namespace "hydra.lexical",
          (Module.Namespace "hydra.rewriting")] allDecodedDeps),
        Module.moduleTypeDependencies = [
          Module.moduleNamespace mod,
          (Module.Namespace "hydra.util")],
        Module.moduleDescription = (Just (Strings.cat [
          "Term decoders for ",
          (Module.unNamespace (Module.moduleNamespace mod))]))})))))))

-- | Generate a decoder module namespace from a source module namespace
decodeNamespace :: (Module.Namespace -> Module.Namespace)
decodeNamespace ns = (Module.Namespace (Strings.cat [
  "hydra.decode.",
  (Strings.intercalate "." (Lists.tail (Strings.splitOn "." (Module.unNamespace ns))))]))

-- | Generate a decoder for a pair type
decodePairType :: (Core.PairType -> Core.Term)
decodePairType pt =  
  let firstDecoder = (decodeType (Core.pairTypeFirst pt))
  in  
    let secondDecoder = (decodeType (Core.pairTypeSecond pt))
    in (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.decodePair")),
        Core.applicationArgument = firstDecoder})),
      Core.applicationArgument = secondDecoder}))

-- | Generate a decoder for a record type
decodeRecordType :: (Core.RowType -> Core.Term)
decodeRecordType rt =  
  let typeName = (Core.rowTypeTypeName rt)
  in  
    let fieldTypes = (Core.rowTypeFields rt)
    in  
      let decodeFieldTerm = (\ft -> Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.requireField")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString (Core.unName (Core.fieldTypeName ft))))})),
                  Core.applicationArgument = (decodeType (Core.fieldTypeType ft))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))
      in  
        let localVarName = (\ft -> Core.Name (Strings.cat [
                "field_",
                (Core.unName (Core.fieldTypeName ft))]))
        in  
          let toFieldLambda = (\ft -> \body -> Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (localVarName ft),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = body})))
          in  
            let decodeBody = (Lists.foldl (\acc -> \ft -> Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.bind"))),
                      Core.applicationArgument = (decodeFieldTerm ft)})),
                    Core.applicationArgument = (toFieldLambda ft acc)})) (Core.TermEither (Right (Core.TermRecord (Core.Record {
                    Core.recordTypeName = typeName,
                    Core.recordFields = (Lists.map (\ft -> Core.Field {
                      Core.fieldName = (Core.fieldTypeName ft),
                      Core.fieldTerm = (Core.TermVariable (localVarName ft))}) fieldTypes)})))) (Lists.reverse fieldTypes))
            in (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "cx"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "raw"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "err"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                          Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "stripped"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                          Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                              "expected record of type ",
                              (Core.unName typeName)])))}))))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "record"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "record"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermLet (Core.Let {
                                  Core.letBindings = [
                                    Core.Binding {
                                      Core.bindingName = (Core.Name "fieldMap"),
                                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.toFieldMap")),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "record"))})),
                                      Core.bindingType = Nothing}],
                                  Core.letBody = decodeBody}))})))}]})))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))

-- | Generate a decoder for a set type
decodeSetType :: (Core.Type -> Core.Term)
decodeSetType elemType =  
  let elemDecoder = (decodeType elemType)
  in (Core.TermApplication (Core.Application {
    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.decodeSet")),
    Core.applicationArgument = elemDecoder}))

-- | Generate a decoder term for a Type
decodeType :: (Core.Type -> Core.Term)
decodeType typ = ((\x -> case x of
  Core.TypeAnnotated v1 -> (decodeType (Core.annotatedTypeBody v1))
  Core.TypeApplication v1 -> (Core.TermApplication (Core.Application {
    Core.applicationFunction = (decodeType (Core.applicationTypeFunction v1)),
    Core.applicationArgument = (decodeType (Core.applicationTypeArgument v1))}))
  Core.TypeEither v1 -> (decodeEitherType v1)
  Core.TypeForall v1 -> (decodeForallType v1)
  Core.TypeList v1 -> (decodeListType v1)
  Core.TypeLiteral v1 -> (decodeLiteralType v1)
  Core.TypeMap v1 -> (decodeMapType v1)
  Core.TypeMaybe v1 -> (decodeMaybeType v1)
  Core.TypePair v1 -> (decodePairType v1)
  Core.TypeRecord v1 -> (decodeRecordType v1)
  Core.TypeSet v1 -> (decodeSetType v1)
  Core.TypeUnion v1 -> (decodeUnionType v1)
  Core.TypeUnit -> decodeUnitType
  Core.TypeWrap v1 -> (decodeWrappedType v1)
  Core.TypeVariable v1 -> (Core.TermVariable (decodeBindingName v1))
  _ -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
    Core.lambdaParameter = (Core.Name "cx"),
    Core.lambdaDomain = Nothing,
    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "t"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unsupported type variant"))}))))})))})))) typ)

-- | Generate a decoder for the unit type
decodeUnitType :: Core.Term
decodeUnitType = (Core.TermVariable (Core.Name "hydra.extract.helpers.decodeUnit"))

-- | Generate a decoder for a union type
decodeUnionType :: (Core.RowType -> Core.Term)
decodeUnionType rt =  
  let typeName = (Core.rowTypeTypeName rt)
  in  
    let toVariantPair = (\ft -> Core.TermPair (Core.TermWrap (Core.WrappedTerm {
            Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName (Core.fieldTypeName ft))))}), (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "input"),
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
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (decodeType (Core.fieldTypeType ft)),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))))
    in (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "cx"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "raw"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "err"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                  Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "stripped"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
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
                              Core.applicationArgument = (Core.TermVariable (Core.Name "variantMap"))}))}))}))})))}]})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
            Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))

-- | Generate a decoder for a wrapped type
decodeWrappedType :: (Core.WrappedType -> Core.Term)
decodeWrappedType wt =  
  let bodyDecoder = (decodeType (Core.wrappedTypeBody wt))
  in (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
    Core.lambdaParameter = (Core.Name "cx"),
    Core.lambdaDomain = Nothing,
    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "raw"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "err"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "stripped"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
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
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = bodyDecoder,
                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                              Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
                              Core.projectionField = (Core.Name "body")})))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "wrappedTerm"))}))}))}))})))}]})))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
          Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))

-- | Get full result type for decoder
decoderFullResultType :: (Core.Type -> Core.Type)
decoderFullResultType typ = ((\x -> case x of
  Core.TypeAnnotated v1 -> (decoderFullResultType (Core.annotatedTypeBody v1))
  Core.TypeApplication v1 -> (Core.TypeApplication (Core.ApplicationType {
    Core.applicationTypeFunction = (decoderFullResultType (Core.applicationTypeFunction v1)),
    Core.applicationTypeArgument = (Core.applicationTypeArgument v1)}))
  Core.TypeEither v1 -> (Core.TypeEither (Core.EitherType {
    Core.eitherTypeLeft = (decoderFullResultType (Core.eitherTypeLeft v1)),
    Core.eitherTypeRight = (decoderFullResultType (Core.eitherTypeRight v1))}))
  Core.TypeForall v1 -> (Core.TypeApplication (Core.ApplicationType {
    Core.applicationTypeFunction = (decoderFullResultType (Core.forallTypeBody v1)),
    Core.applicationTypeArgument = (Core.TypeVariable (Core.forallTypeParameter v1))}))
  Core.TypeList v1 -> (Core.TypeList (decoderFullResultType v1))
  Core.TypeLiteral _ -> (Core.TypeVariable (Core.Name "hydra.core.Literal"))
  Core.TypeMap v1 -> (Core.TypeMap (Core.MapType {
    Core.mapTypeKeys = (decoderFullResultType (Core.mapTypeKeys v1)),
    Core.mapTypeValues = (decoderFullResultType (Core.mapTypeValues v1))}))
  Core.TypeMaybe v1 -> (Core.TypeMaybe (decoderFullResultType v1))
  Core.TypePair v1 -> (Core.TypePair (Core.PairType {
    Core.pairTypeFirst = (decoderFullResultType (Core.pairTypeFirst v1)),
    Core.pairTypeSecond = (decoderFullResultType (Core.pairTypeSecond v1))}))
  Core.TypeRecord v1 -> (Core.TypeVariable (Core.rowTypeTypeName v1))
  Core.TypeSet v1 -> (Core.TypeSet (decoderFullResultType v1))
  Core.TypeUnion v1 -> (Core.TypeVariable (Core.rowTypeTypeName v1))
  Core.TypeUnit -> Core.TypeUnit
  Core.TypeVariable v1 -> (Core.TypeVariable v1)
  Core.TypeWrap v1 -> (Core.TypeVariable (Core.wrappedTypeTypeName v1))
  _ -> (Core.TypeVariable (Core.Name "hydra.core.Term"))) typ)

-- | Compute the result type name for a decoder
decoderResultType :: (Core.Type -> Core.Name)
decoderResultType typ = ((\x -> case x of
  Core.TypeAnnotated v1 -> (decoderResultType (Core.annotatedTypeBody v1))
  Core.TypeApplication v1 -> (decoderResultType (Core.applicationTypeFunction v1))
  Core.TypeForall v1 -> (decoderResultType (Core.forallTypeBody v1))
  Core.TypeLiteral _ -> (Core.Name "hydra.core.Literal")
  Core.TypeRecord v1 -> (Core.rowTypeTypeName v1)
  Core.TypeUnion v1 -> (Core.rowTypeTypeName v1)
  Core.TypeWrap v1 -> (Core.wrappedTypeTypeName v1)
  _ -> (Core.Name "hydra.core.Term")) typ)

-- | Build decoder function type
decoderType :: (Core.Type -> Core.Type)
decoderType typ =  
  let resultType = (decoderFullResultType typ)
  in  
    let baseType = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
              Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.util.DecodingError")),
                Core.eitherTypeRight = resultType}))}))}))
    in (prependForallDecoders baseType typ)

-- | Build type scheme for a decoder function
decoderTypeScheme :: (Core.Type -> Core.TypeScheme)
decoderTypeScheme typ =  
  let typeVars = (collectTypeVariables typ)
  in  
    let allOrdVars = (collectOrdConstrainedVariables typ)
    in  
      let ordVars = (Lists.filter (\v -> Lists.elem v typeVars) allOrdVars)
      in  
        let constraints = (Logic.ifElse (Lists.null ordVars) Nothing (Just (Maps.fromList (Lists.map (\v -> (v, Core.TypeVariableMetadata {
                Core.typeVariableMetadataClasses = (Sets.singleton (Core.Name "ordering"))})) ordVars))))
        in Core.TypeScheme {
          Core.typeSchemeVariables = typeVars,
          Core.typeSchemeType = (decoderType typ),
          Core.typeSchemeConstraints = constraints}

-- | Filter bindings to only decodable type definitions
filterTypeBindings :: ([Core.Binding] -> Compute.Flow Graph.Graph [Core.Binding])
filterTypeBindings bindings = (Flows.map Maybes.cat (Flows.mapList isDecodableBinding (Lists.filter Annotations.isNativeType bindings)))

-- | Check if a binding is decodable (serializable type)
isDecodableBinding :: (Core.Binding -> Compute.Flow Graph.Graph (Maybe Core.Binding))
isDecodableBinding b = (Flows.map (\serializable -> Logic.ifElse serializable (Just b) Nothing) (Schemas.isSerializableByName (Core.bindingName b)))

-- | Prepend decoder types for forall parameters to base type
prependForallDecoders :: (Core.Type -> Core.Type -> Core.Type)
prependForallDecoders baseType typ = ((\x -> case x of
  Core.TypeAnnotated v1 -> (prependForallDecoders baseType (Core.annotatedTypeBody v1))
  Core.TypeForall v1 -> (Core.TypeFunction (Core.FunctionType {
    Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
        Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
          Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.util.DecodingError")),
          Core.eitherTypeRight = (Core.TypeVariable (Core.forallTypeParameter v1))}))}))})),
    Core.functionTypeCodomain = (prependForallDecoders baseType (Core.forallTypeBody v1))}))
  _ -> baseType) typ)
