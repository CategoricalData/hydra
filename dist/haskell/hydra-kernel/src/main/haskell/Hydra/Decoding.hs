-- Note: this is an automatically generated file. Do not edit.
-- | Functions for generating term decoders from type modules

module Hydra.Decoding where
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Constants as Constants
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Predicates as Predicates
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Collect forall type variable names from a type
collectForallVariables :: Core.Type -> [Core.Name]
collectForallVariables typ =
    case typ of
      Core.TypeAnnotated v0 -> collectForallVariables (Core.annotatedTypeBody v0)
      Core.TypeForall v0 -> Lists.cons (Core.forallTypeParameter v0) (collectForallVariables (Core.forallTypeBody v0))
      _ -> []
-- | Collect type variables needing Ord constraints (from Map key and Set element types)
collectOrdConstrainedVariables :: Core.Type -> [Core.Name]
collectOrdConstrainedVariables typ =
    case typ of
      Core.TypeAnnotated v0 -> collectOrdConstrainedVariables (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> Lists.concat2 (collectOrdConstrainedVariables (Core.applicationTypeFunction v0)) (collectOrdConstrainedVariables (Core.applicationTypeArgument v0))
      Core.TypeEither v0 -> Lists.concat2 (collectOrdConstrainedVariables (Core.eitherTypeLeft v0)) (collectOrdConstrainedVariables (Core.eitherTypeRight v0))
      Core.TypeForall v0 -> collectOrdConstrainedVariables (Core.forallTypeBody v0)
      Core.TypeList v0 -> collectOrdConstrainedVariables v0
      Core.TypeMap v0 -> Lists.concat [
        collectTypeVariablesFromType (Core.mapTypeKeys v0),
        (collectOrdConstrainedVariables (Core.mapTypeKeys v0)),
        (collectOrdConstrainedVariables (Core.mapTypeValues v0))]
      Core.TypeMaybe v0 -> collectOrdConstrainedVariables v0
      Core.TypePair v0 -> Lists.concat2 (collectOrdConstrainedVariables (Core.pairTypeFirst v0)) (collectOrdConstrainedVariables (Core.pairTypeSecond v0))
      Core.TypeRecord v0 -> Lists.concat (Lists.map (\ft -> collectOrdConstrainedVariables (Core.fieldTypeType ft)) v0)
      Core.TypeSet v0 -> Lists.concat2 (collectTypeVariablesFromType v0) (collectOrdConstrainedVariables v0)
      Core.TypeUnion v0 -> Lists.concat (Lists.map (\ft -> collectOrdConstrainedVariables (Core.fieldTypeType ft)) v0)
      Core.TypeWrap v0 -> collectOrdConstrainedVariables v0
      _ -> []
-- | Collect type variable names from a type (forall parameters only)
collectTypeVariables :: Core.Type -> [Core.Name]
collectTypeVariables typ = collectForallVariables typ
-- | Collect all type variable names from a type expression
collectTypeVariablesFromType :: Core.Type -> [Core.Name]
collectTypeVariablesFromType typ =
    case typ of
      Core.TypeAnnotated v0 -> collectTypeVariablesFromType (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> Lists.concat2 (collectTypeVariablesFromType (Core.applicationTypeFunction v0)) (collectTypeVariablesFromType (Core.applicationTypeArgument v0))
      Core.TypeEither v0 -> Lists.concat2 (collectTypeVariablesFromType (Core.eitherTypeLeft v0)) (collectTypeVariablesFromType (Core.eitherTypeRight v0))
      Core.TypeForall v0 -> collectTypeVariablesFromType (Core.forallTypeBody v0)
      Core.TypeList v0 -> collectTypeVariablesFromType v0
      Core.TypeMap v0 -> Lists.concat2 (collectTypeVariablesFromType (Core.mapTypeKeys v0)) (collectTypeVariablesFromType (Core.mapTypeValues v0))
      Core.TypeMaybe v0 -> collectTypeVariablesFromType v0
      Core.TypePair v0 -> Lists.concat2 (collectTypeVariablesFromType (Core.pairTypeFirst v0)) (collectTypeVariablesFromType (Core.pairTypeSecond v0))
      Core.TypeRecord v0 -> Lists.concat (Lists.map (\ft -> collectTypeVariablesFromType (Core.fieldTypeType ft)) v0)
      Core.TypeSet v0 -> collectTypeVariablesFromType v0
      Core.TypeUnion v0 -> Lists.concat (Lists.map (\ft -> collectTypeVariablesFromType (Core.fieldTypeType ft)) v0)
      Core.TypeVariable v0 -> [
        v0]
      Core.TypeWrap v0 -> collectTypeVariablesFromType v0
      _ -> []
-- | Transform a type binding into a decoder binding
decodeBinding :: t0 -> Graph.Graph -> Core.Binding -> Either Errors.DecodingError Core.Binding
decodeBinding cx graph b =
    Eithers.bind (DecodeCore.type_ graph (Core.bindingTerm b)) (\typ -> Right (Core.Binding {
      Core.bindingName = (decodeBindingName (Core.bindingName b)),
      Core.bindingTerm = (decodeTypeNamed (Core.bindingName b) typ),
      Core.bindingType = (Just (decoderTypeSchemeNamed (Core.bindingName b) typ))}))
-- | Generate a binding name for a decoder function from a type name
decodeBindingName :: Core.Name -> Core.Name
decodeBindingName n =

      let parts = Strings.splitOn "." (Core.unName n)
          localPart = Formatting.decapitalize (Names.localNameOf n)
          localResult = Core.Name localPart
      in (Maybes.maybe localResult (\nsParts -> Maybes.maybe localResult (\nsUc ->
        let tail = Pairs.second nsUc
        in (Core.Name (Strings.intercalate "." (Lists.concat2 [
          "hydra",
          "decode"] (Lists.concat2 tail [
          localPart]))))) (Lists.uncons nsParts)) (Lists.maybeInit parts))
-- | Generate a decoder for an Either type
decodeEitherType :: Core.EitherType -> Core.Term
decodeEitherType et =

      let leftDecoder = decodeType (Core.eitherTypeLeft et)
          rightDecoder = decodeType (Core.eitherTypeRight et)
      in (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.decodeEither")),
          Core.applicationArgument = leftDecoder})),
        Core.applicationArgument = rightDecoder}))
-- | Generate a decoder for a polymorphic (forall) type
decodeForallType :: Core.ForallType -> Core.Term
decodeForallType ft =
    Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (decodeBindingName (Core.forallTypeParameter ft)),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (decodeType (Core.forallTypeBody ft))})
-- | Generate a decoder for a list type
decodeListType :: Core.Type -> Core.Term
decodeListType elemType =

      let elemDecoder = decodeType elemType
      in (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.decodeList")),
        Core.applicationArgument = elemDecoder}))
-- | Generate a decoder for a literal type
decodeLiteralType :: Core.LiteralType -> Core.Term
decodeLiteralType lt =
    case lt of
      Core.LiteralTypeBinary -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "cx"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "raw"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "err"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "stripped"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                    Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "literal"),
                        Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "v"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                              Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected binary literal"))}))))),
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "binary"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "b"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "b"))))}))}]})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
      Core.LiteralTypeBoolean -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "cx"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "raw"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "err"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "stripped"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                    Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "literal"),
                        Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "v"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                              Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected boolean literal"))}))))),
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "boolean"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "b"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "b"))))}))}]})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
      Core.LiteralTypeDecimal -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "cx"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "raw"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "err"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "stripped"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                    Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "literal"),
                        Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "v"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                              Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected decimal literal"))}))))),
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "decimal"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "d"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "d"))))}))}]})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
      Core.LiteralTypeFloat v0 -> case v0 of
        Core.FloatTypeBigfloat -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "literal"),
                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                    "expected ",
                                    "bigfloat",
                                    " literal"])))}))))),
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "float"),
                                    Core.fieldTerm = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.FloatValue"),
                                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                          "expected ",
                                          "bigfloat",
                                          " value"])))}))))),
                                      Core.caseStatementCases = [
                                        Core.Field {
                                          Core.fieldName = (Core.Name "bigfloat"),
                                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "f"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "f"))))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.FloatTypeFloat32 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "literal"),
                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                    "expected ",
                                    "float32",
                                    " literal"])))}))))),
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "float"),
                                    Core.fieldTerm = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.FloatValue"),
                                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                          "expected ",
                                          "float32",
                                          " value"])))}))))),
                                      Core.caseStatementCases = [
                                        Core.Field {
                                          Core.fieldName = (Core.Name "float32"),
                                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "f"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "f"))))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.FloatTypeFloat64 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "literal"),
                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                    "expected ",
                                    "float64",
                                    " literal"])))}))))),
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "float"),
                                    Core.fieldTerm = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.FloatValue"),
                                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                          "expected ",
                                          "float64",
                                          " value"])))}))))),
                                      Core.caseStatementCases = [
                                        Core.Field {
                                          Core.fieldName = (Core.Name "float64"),
                                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "f"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "f"))))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
      Core.LiteralTypeInteger v0 -> case v0 of
        Core.IntegerTypeBigint -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "literal"),
                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                    "expected ",
                                    "bigint",
                                    " literal"])))}))))),
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "integer"),
                                    Core.fieldTerm = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                          "expected ",
                                          "bigint",
                                          " value"])))}))))),
                                      Core.caseStatementCases = [
                                        Core.Field {
                                          Core.fieldName = (Core.Name "bigint"),
                                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "i"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.IntegerTypeInt8 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "literal"),
                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                    "expected ",
                                    "int8",
                                    " literal"])))}))))),
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "integer"),
                                    Core.fieldTerm = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                          "expected ",
                                          "int8",
                                          " value"])))}))))),
                                      Core.caseStatementCases = [
                                        Core.Field {
                                          Core.fieldName = (Core.Name "int8"),
                                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "i"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.IntegerTypeInt16 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "literal"),
                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                    "expected ",
                                    "int16",
                                    " literal"])))}))))),
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "integer"),
                                    Core.fieldTerm = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                          "expected ",
                                          "int16",
                                          " value"])))}))))),
                                      Core.caseStatementCases = [
                                        Core.Field {
                                          Core.fieldName = (Core.Name "int16"),
                                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "i"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.IntegerTypeInt32 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "literal"),
                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                    "expected ",
                                    "int32",
                                    " literal"])))}))))),
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "integer"),
                                    Core.fieldTerm = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                          "expected ",
                                          "int32",
                                          " value"])))}))))),
                                      Core.caseStatementCases = [
                                        Core.Field {
                                          Core.fieldName = (Core.Name "int32"),
                                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "i"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.IntegerTypeInt64 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "literal"),
                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                    "expected ",
                                    "int64",
                                    " literal"])))}))))),
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "integer"),
                                    Core.fieldTerm = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                          "expected ",
                                          "int64",
                                          " value"])))}))))),
                                      Core.caseStatementCases = [
                                        Core.Field {
                                          Core.fieldName = (Core.Name "int64"),
                                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "i"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.IntegerTypeUint8 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "literal"),
                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                    "expected ",
                                    "uint8",
                                    " literal"])))}))))),
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "integer"),
                                    Core.fieldTerm = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                          "expected ",
                                          "uint8",
                                          " value"])))}))))),
                                      Core.caseStatementCases = [
                                        Core.Field {
                                          Core.fieldName = (Core.Name "uint8"),
                                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "i"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.IntegerTypeUint16 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "literal"),
                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                    "expected ",
                                    "uint16",
                                    " literal"])))}))))),
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "integer"),
                                    Core.fieldTerm = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                          "expected ",
                                          "uint16",
                                          " value"])))}))))),
                                      Core.caseStatementCases = [
                                        Core.Field {
                                          Core.fieldName = (Core.Name "uint16"),
                                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "i"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.IntegerTypeUint32 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "literal"),
                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                    "expected ",
                                    "uint32",
                                    " literal"])))}))))),
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "integer"),
                                    Core.fieldTerm = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                          "expected ",
                                          "uint32",
                                          " value"])))}))))),
                                      Core.caseStatementCases = [
                                        Core.Field {
                                          Core.fieldName = (Core.Name "uint32"),
                                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "i"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.IntegerTypeUint64 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "literal"),
                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                    "expected ",
                                    "uint64",
                                    " literal"])))}))))),
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "integer"),
                                    Core.fieldTerm = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                          "expected ",
                                          "uint64",
                                          " value"])))}))))),
                                      Core.caseStatementCases = [
                                        Core.Field {
                                          Core.fieldName = (Core.Name "uint64"),
                                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "i"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i"))))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
      Core.LiteralTypeString -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "cx"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "raw"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "err"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "stripped"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                    Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "literal"),
                        Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "v"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                              Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected string literal"))}))))),
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "string"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "s"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "s"))))}))}]})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
-- | Generate a decoder for a map type
decodeMapType :: Core.MapType -> Core.Term
decodeMapType mt =

      let keyDecoder = decodeType (Core.mapTypeKeys mt)
          valDecoder = decodeType (Core.mapTypeValues mt)
      in (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.decodeMap")),
          Core.applicationArgument = keyDecoder})),
        Core.applicationArgument = valDecoder}))
-- | Generate a decoder for an optional type
decodeMaybeType :: Core.Type -> Core.Term
decodeMaybeType elemType =

      let elemDecoder = decodeType elemType
      in (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.decodeMaybe")),
        Core.applicationArgument = elemDecoder}))
-- | Transform a type module into a decoder module
decodeModule :: Context.Context -> Graph.Graph -> Packaging.Module -> Either Errors.Error (Maybe Packaging.Module)
decodeModule cx graph mod =
    Eithers.bind (filterTypeBindings cx graph (Maybes.cat (Lists.map (\d -> case d of
      Packaging.DefinitionType v0 -> Just ((\name -> \typ ->
        let schemaTerm = Core.TermVariable (Core.Name "hydra.core.Type")
            dataTerm =
                    Annotations.normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
                      Core.annotatedTermBody = (EncodeCore.type_ typ),
                      Core.annotatedTermAnnotation = (Maps.fromList [
                        (Constants.key_type, schemaTerm)])}))
        in Core.Binding {
          Core.bindingName = name,
          Core.bindingTerm = dataTerm,
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
            Core.typeSchemeConstraints = Nothing}))}) (Packaging.typeDefinitionName v0) (Core.typeSchemeBody (Packaging.typeDefinitionType v0)))
      _ -> Nothing) (Packaging.moduleDefinitions mod)))) (\typeBindings -> Logic.ifElse (Lists.null typeBindings) (Right Nothing) (Eithers.bind (Eithers.mapList (\b -> Eithers.bimap (\_e -> Errors.ErrorDecoding _e) (\x -> x) (decodeBinding cx graph b)) typeBindings) (\decodedBindings ->
      let decodedTypeDeps = Lists.map decodeNamespace (Packaging.moduleTypeDependencies mod)
          decodedTermDeps = Lists.map decodeNamespace (Packaging.moduleTermDependencies mod)
          allDecodedDeps = Lists.nub (Lists.concat2 decodedTypeDeps decodedTermDeps)
      in (Right (Just (Packaging.Module {
        Packaging.moduleNamespace = (decodeNamespace (Packaging.moduleNamespace mod)),
        Packaging.moduleDefinitions = (Lists.map (\b -> Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.bindingName b),
          Packaging.termDefinitionTerm = (Core.bindingTerm b),
          Packaging.termDefinitionType = (Core.bindingType b)})) decodedBindings),
        Packaging.moduleTermDependencies = (Lists.concat2 [
          Packaging.Namespace "hydra.extract.core",
          (Packaging.Namespace "hydra.lexical"),
          (Packaging.Namespace "hydra.rewriting")] allDecodedDeps),
        Packaging.moduleTypeDependencies = [
          Packaging.moduleNamespace mod,
          (Packaging.Namespace "hydra.util")],
        Packaging.moduleDescription = (Just (Strings.cat [
          "Term decoders for ",
          (Packaging.unNamespace (Packaging.moduleNamespace mod))]))}))))))
-- | Generate a decoder module namespace from a source module namespace
decodeNamespace :: Packaging.Namespace -> Packaging.Namespace
decodeNamespace ns =

      let parts = Strings.splitOn "." (Packaging.unNamespace ns)
          fallback = Packaging.Namespace (Packaging.unNamespace ns)
      in (Maybes.maybe fallback (\uc -> Packaging.Namespace (Strings.cat [
        "hydra.decode.",
        (Strings.intercalate "." (Pairs.second uc))])) (Lists.uncons parts))
-- | Generate a decoder for a pair type
decodePairType :: Core.PairType -> Core.Term
decodePairType pt =

      let firstDecoder = decodeType (Core.pairTypeFirst pt)
          secondDecoder = decodeType (Core.pairTypeSecond pt)
      in (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.decodePair")),
          Core.applicationArgument = firstDecoder})),
        Core.applicationArgument = secondDecoder}))
-- | Generate a decoder for a record type
decodeRecordType :: [Core.FieldType] -> Core.Term
decodeRecordType rt = decodeRecordTypeImpl (Core.Name "unknown") rt
-- | Generate a decoder for a record type with a type name
decodeRecordTypeImpl :: Core.Name -> [Core.FieldType] -> Core.Term
decodeRecordTypeImpl tname rt =

      let decodeFieldTerm =
              \ft -> Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString (Core.unName (Core.fieldTypeName ft))))})),
                    Core.applicationArgument = (decodeType (Core.fieldTypeType ft))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})
          localVarName =
                  \ft -> Core.Name (Strings.cat [
                    "field_",
                    (Core.unName (Core.fieldTypeName ft))])
          toFieldLambda =
                  \ft -> \body -> Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (localVarName ft),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = body})
          decodeBody =
                  Lists.foldl (\acc -> \ft -> Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                      Core.applicationArgument = (decodeFieldTerm ft)})),
                    Core.applicationArgument = (toFieldLambda ft acc)})) (Core.TermEither (Right (Core.TermRecord (Core.Record {
                    Core.recordTypeName = tname,
                    Core.recordFields = (Lists.map (\ft -> Core.Field {
                      Core.fieldName = (Core.fieldTypeName ft),
                      Core.fieldTerm = (Core.TermVariable (localVarName ft))}) rt)})))) (Lists.reverse rt)
      in (Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "cx"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "raw"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "err"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "stripped"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                    Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected record"))}))))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "record"),
                        Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "record"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermLet (Core.Let {
                            Core.letBindings = [
                              Core.Binding {
                                Core.bindingName = (Core.Name "fieldMap"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.toFieldMap")),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "record"))})),
                                Core.bindingType = Nothing}],
                            Core.letBody = decodeBody}))}))}]})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))}))
-- | Generate a decoder for a record type with element name
decodeRecordTypeNamed :: Core.Name -> [Core.FieldType] -> Core.Term
decodeRecordTypeNamed ename rt = decodeRecordTypeImpl ename rt
-- | Generate a decoder for a set type
decodeSetType :: Core.Type -> Core.Term
decodeSetType elemType =

      let elemDecoder = decodeType elemType
      in (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.decodeSet")),
        Core.applicationArgument = elemDecoder}))
-- | Generate a decoder term for a Type
decodeType :: Core.Type -> Core.Term
decodeType typ =
    case typ of
      Core.TypeAnnotated v0 -> decodeType (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> Core.TermApplication (Core.Application {
        Core.applicationFunction = (decodeType (Core.applicationTypeFunction v0)),
        Core.applicationArgument = (decodeType (Core.applicationTypeArgument v0))})
      Core.TypeEither v0 -> decodeEitherType v0
      Core.TypeForall v0 -> decodeForallType v0
      Core.TypeList v0 -> decodeListType v0
      Core.TypeLiteral v0 -> decodeLiteralType v0
      Core.TypeMap v0 -> decodeMapType v0
      Core.TypeMaybe v0 -> decodeMaybeType v0
      Core.TypePair v0 -> decodePairType v0
      Core.TypeRecord v0 -> decodeRecordType v0
      Core.TypeSet v0 -> decodeSetType v0
      Core.TypeUnion v0 -> decodeUnionType v0
      Core.TypeUnit -> decodeUnitType
      Core.TypeVoid -> decodeUnitType
      Core.TypeWrap v0 -> decodeWrappedType v0
      Core.TypeVariable v0 -> Core.TermVariable (decodeBindingName v0)
      _ -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "cx"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "t"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unsupported type variant"))}))))}))})
-- | Generate a decoder term for a Type, with element name for nominal types
decodeTypeNamed :: Core.Name -> Core.Type -> Core.Term
decodeTypeNamed ename typ =
    case typ of
      Core.TypeAnnotated v0 -> decodeTypeNamed ename (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> Core.TermApplication (Core.Application {
        Core.applicationFunction = (decodeType (Core.applicationTypeFunction v0)),
        Core.applicationArgument = (decodeType (Core.applicationTypeArgument v0))})
      Core.TypeEither v0 -> decodeEitherType v0
      Core.TypeForall v0 -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (decodeBindingName (Core.forallTypeParameter v0)),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (decodeTypeNamed ename (Core.forallTypeBody v0))})
      Core.TypeList v0 -> decodeListType v0
      Core.TypeLiteral v0 -> decodeLiteralType v0
      Core.TypeMap v0 -> decodeMapType v0
      Core.TypeMaybe v0 -> decodeMaybeType v0
      Core.TypePair v0 -> decodePairType v0
      Core.TypeRecord v0 -> decodeRecordTypeNamed ename v0
      Core.TypeSet v0 -> decodeSetType v0
      Core.TypeUnion v0 -> decodeUnionTypeNamed ename v0
      Core.TypeUnit -> decodeUnitType
      Core.TypeVoid -> decodeUnitType
      Core.TypeWrap v0 -> decodeWrappedTypeNamed ename v0
      Core.TypeVariable v0 -> Core.TermVariable (decodeBindingName v0)
      _ -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "cx"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "t"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unsupported type variant"))}))))}))})
-- | Generate a decoder for a union type
decodeUnionType :: [Core.FieldType] -> Core.Term
decodeUnionType rt = decodeUnionTypeNamed (Core.Name "unknown") rt
-- | Generate a decoder for a union type with the given element name
decodeUnionTypeNamed :: Core.Name -> [Core.FieldType] -> Core.Term
decodeUnionTypeNamed ename rt =

      let toVariantPair =
              \ft -> Core.TermPair (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName (Core.fieldTypeName ft))))}), (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "input"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.map")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "t"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                        Core.injectionTypeName = ename,
                        Core.injectionField = Core.Field {
                          Core.fieldName = (Core.fieldTypeName ft),
                          Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (decodeType (Core.fieldTypeType ft)),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))
      in (Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "cx"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "raw"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "err"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "stripped"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                    Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected union"))}))))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "inj"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermLet (Core.Let {
                            Core.letBindings = [
                              Core.Binding {
                                Core.bindingName = (Core.Name "field"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
                                    Core.projectionField = (Core.Name "field")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "inj"))})),
                                Core.bindingType = Nothing},
                              Core.Binding {
                                Core.bindingName = (Core.Name "fname"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                                    Core.projectionField = (Core.Name "name")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "field"))})),
                                Core.bindingType = Nothing},
                              Core.Binding {
                                Core.bindingName = (Core.Name "fterm"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                                    Core.projectionField = (Core.Name "term")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "field"))})),
                                Core.bindingType = Nothing},
                              Core.Binding {
                                Core.bindingName = (Core.Name "variantMap"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.fromList")),
                                  Core.applicationArgument = (Core.TermList (Lists.map toVariantPair rt))})),
                                Core.bindingType = Nothing}],
                            Core.letBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                                  Core.applicationArgument = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                    Core.wrappedTermBody = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.cat")),
                                      Core.applicationArgument = (Core.TermList [
                                        Core.TermLiteral (Core.LiteralString "no such field "),
                                        (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.core.Name")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))})),
                                        (Core.TermLiteral (Core.LiteralString " in union"))])}))}))))})),
                                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "f"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "fterm"))}))}))})),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.lookup")),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))})),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "variantMap"))}))}))}))}))}]})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))}))
-- | Generate a decoder for the unit type
decodeUnitType :: Core.Term
decodeUnitType =
    Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "cx"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "t"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.decodeUnit")),
            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
          Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))}))})
-- | Generate a decoder for a wrapped type
decodeWrappedType :: Core.Type -> Core.Term
decodeWrappedType wt = decodeWrappedTypeNamed (Core.Name "unknown") wt
-- | Generate a decoder for a wrapped type with the given element name
decodeWrappedTypeNamed :: Core.Name -> Core.Type -> Core.Term
decodeWrappedTypeNamed ename wt =

      let bodyDecoder = decodeType wt
      in (Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "cx"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "raw"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "err"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))}))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "stripped"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                    Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected wrapped type"))}))))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "wrap"),
                        Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "wrappedTerm"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.map")),
                              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "b"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = ename,
                                  Core.wrappedTermBody = (Core.TermVariable (Core.Name "b"))}))}))})),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = bodyDecoder,
                                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermProject (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
                                  Core.projectionField = (Core.Name "body")})),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "wrappedTerm"))}))}))}))}))}]})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))}))
-- | Get full result type for decoder
decoderFullResultType :: Core.Type -> Core.Type
decoderFullResultType typ =
    case typ of
      Core.TypeAnnotated v0 -> decoderFullResultType (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = (decoderFullResultType (Core.applicationTypeFunction v0)),
        Core.applicationTypeArgument = (Core.applicationTypeArgument v0)})
      Core.TypeEither v0 -> Core.TypeEither (Core.EitherType {
        Core.eitherTypeLeft = (decoderFullResultType (Core.eitherTypeLeft v0)),
        Core.eitherTypeRight = (decoderFullResultType (Core.eitherTypeRight v0))})
      Core.TypeForall v0 -> Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = (decoderFullResultType (Core.forallTypeBody v0)),
        Core.applicationTypeArgument = (Core.TypeVariable (Core.forallTypeParameter v0))})
      Core.TypeList v0 -> Core.TypeList (decoderFullResultType v0)
      Core.TypeLiteral _ -> Core.TypeVariable (Core.Name "hydra.core.Literal")
      Core.TypeMap v0 -> Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = (decoderFullResultType (Core.mapTypeKeys v0)),
        Core.mapTypeValues = (decoderFullResultType (Core.mapTypeValues v0))})
      Core.TypeMaybe v0 -> Core.TypeMaybe (decoderFullResultType v0)
      Core.TypePair v0 -> Core.TypePair (Core.PairType {
        Core.pairTypeFirst = (decoderFullResultType (Core.pairTypeFirst v0)),
        Core.pairTypeSecond = (decoderFullResultType (Core.pairTypeSecond v0))})
      Core.TypeRecord _ -> Core.TypeVariable (Core.Name "hydra.core.Term")
      Core.TypeSet v0 -> Core.TypeSet (decoderFullResultType v0)
      Core.TypeUnion _ -> Core.TypeVariable (Core.Name "hydra.core.Term")
      Core.TypeUnit -> Core.TypeUnit
      Core.TypeVariable v0 -> Core.TypeVariable v0
      Core.TypeVoid -> Core.TypeVoid
      Core.TypeWrap _ -> Core.TypeVariable (Core.Name "hydra.core.Term")
      _ -> Core.TypeVariable (Core.Name "hydra.core.Term")
-- | Get full result type for decoder with element name
decoderFullResultTypeNamed :: Core.Name -> Core.Type -> Core.Type
decoderFullResultTypeNamed ename typ =
    case typ of
      Core.TypeAnnotated v0 -> decoderFullResultTypeNamed ename (Core.annotatedTypeBody v0)
      Core.TypeForall v0 -> Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = (decoderFullResultTypeNamed ename (Core.forallTypeBody v0)),
        Core.applicationTypeArgument = (Core.TypeVariable (Core.forallTypeParameter v0))})
      Core.TypeRecord _ -> Core.TypeVariable ename
      Core.TypeUnion _ -> Core.TypeVariable ename
      Core.TypeWrap _ -> Core.TypeVariable ename
      Core.TypeApplication v0 -> Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = (decoderFullResultType (Core.applicationTypeFunction v0)),
        Core.applicationTypeArgument = (Core.applicationTypeArgument v0)})
      Core.TypeEither v0 -> Core.TypeEither (Core.EitherType {
        Core.eitherTypeLeft = (decoderFullResultType (Core.eitherTypeLeft v0)),
        Core.eitherTypeRight = (decoderFullResultType (Core.eitherTypeRight v0))})
      Core.TypeList v0 -> Core.TypeList (decoderFullResultType v0)
      Core.TypeLiteral _ -> Core.TypeVariable (Core.Name "hydra.core.Literal")
      Core.TypeMap v0 -> Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = (decoderFullResultType (Core.mapTypeKeys v0)),
        Core.mapTypeValues = (decoderFullResultType (Core.mapTypeValues v0))})
      Core.TypeMaybe v0 -> Core.TypeMaybe (decoderFullResultType v0)
      Core.TypePair v0 -> Core.TypePair (Core.PairType {
        Core.pairTypeFirst = (decoderFullResultType (Core.pairTypeFirst v0)),
        Core.pairTypeSecond = (decoderFullResultType (Core.pairTypeSecond v0))})
      Core.TypeSet v0 -> Core.TypeSet (decoderFullResultType v0)
      Core.TypeUnit -> Core.TypeUnit
      Core.TypeVariable v0 -> Core.TypeVariable v0
      Core.TypeVoid -> Core.TypeVoid
      _ -> Core.TypeVariable (Core.Name "hydra.core.Term")
-- | Compute the result type name for a decoder
decoderResultType :: Core.Type -> Core.Name
decoderResultType typ =
    case typ of
      Core.TypeAnnotated v0 -> decoderResultType (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> decoderResultType (Core.applicationTypeFunction v0)
      Core.TypeForall v0 -> decoderResultType (Core.forallTypeBody v0)
      Core.TypeLiteral _ -> Core.Name "hydra.core.Literal"
      Core.TypeRecord _ -> Core.Name "hydra.core.Term"
      Core.TypeUnion _ -> Core.Name "hydra.core.Term"
      Core.TypeWrap _ -> Core.Name "hydra.core.Term"
      _ -> Core.Name "hydra.core.Term"
-- | Build decoder function type
decoderType :: Core.Type -> Core.Type
decoderType typ =

      let resultType = decoderFullResultType typ
          baseType =
                  Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                      Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                        Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                        Core.eitherTypeRight = resultType}))}))})
      in (prependForallDecoders baseType typ)
-- | Build decoder function type with element name
decoderTypeNamed :: Core.Name -> Core.Type -> Core.Type
decoderTypeNamed ename typ =

      let resultType = decoderFullResultTypeNamed ename typ
          baseType =
                  Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                      Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                        Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                        Core.eitherTypeRight = resultType}))}))})
      in (prependForallDecoders baseType typ)
-- | Build type scheme for a decoder function
decoderTypeScheme :: Core.Type -> Core.TypeScheme
decoderTypeScheme typ =

      let typeVars = collectTypeVariables typ
          allOrdVars = collectOrdConstrainedVariables typ
          ordVars = Lists.filter (\v -> Lists.elem v typeVars) allOrdVars
          constraints =
                  Logic.ifElse (Lists.null ordVars) Nothing (Just (Maps.fromList (Lists.map (\v -> (v, Core.TypeVariableMetadata {
                    Core.typeVariableMetadataClasses = (Sets.singleton (Core.Name "ordering"))})) ordVars)))
      in Core.TypeScheme {
        Core.typeSchemeVariables = typeVars,
        Core.typeSchemeBody = (decoderType typ),
        Core.typeSchemeConstraints = constraints}
-- | Build type scheme for a decoder function with element name
decoderTypeSchemeNamed :: Core.Name -> Core.Type -> Core.TypeScheme
decoderTypeSchemeNamed ename typ =

      let typeVars = collectTypeVariables typ
          allOrdVars = collectOrdConstrainedVariables typ
          ordVars = Lists.filter (\v -> Lists.elem v typeVars) allOrdVars
          constraints =
                  Logic.ifElse (Lists.null ordVars) Nothing (Just (Maps.fromList (Lists.map (\v -> (v, Core.TypeVariableMetadata {
                    Core.typeVariableMetadataClasses = (Sets.singleton (Core.Name "ordering"))})) ordVars)))
      in Core.TypeScheme {
        Core.typeSchemeVariables = typeVars,
        Core.typeSchemeBody = (decoderTypeNamed ename typ),
        Core.typeSchemeConstraints = constraints}
-- | Filter bindings to only decodable type definitions
filterTypeBindings :: Context.Context -> Graph.Graph -> [Core.Binding] -> Either Errors.Error [Core.Binding]
filterTypeBindings cx graph bindings =
    Eithers.map Maybes.cat (Eithers.mapList (isDecodableBinding cx graph) (Lists.filter Annotations.isNativeType bindings))
-- | Check if a binding is decodable (serializable type)
isDecodableBinding :: Context.Context -> Graph.Graph -> Core.Binding -> Either Errors.Error (Maybe Core.Binding)
isDecodableBinding cx graph b =
    Eithers.bind (Predicates.isSerializableByName cx graph (Core.bindingName b)) (\serializable -> Right (Logic.ifElse serializable (Just b) Nothing))
-- | Prepend decoder types for forall parameters to base type
prependForallDecoders :: Core.Type -> Core.Type -> Core.Type
prependForallDecoders baseType typ =
    case typ of
      Core.TypeAnnotated v0 -> prependForallDecoders baseType (Core.annotatedTypeBody v0)
      Core.TypeForall v0 -> Core.TypeFunction (Core.FunctionType {
        Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
            Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
              Core.eitherTypeRight = (Core.TypeVariable (Core.forallTypeParameter v0))}))}))})),
        Core.functionTypeCodomain = (prependForallDecoders baseType (Core.forallTypeBody v0))})
      _ -> baseType
