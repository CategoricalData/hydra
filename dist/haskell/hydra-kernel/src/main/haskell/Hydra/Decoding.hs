-- Note: this is an automatically generated file. Do not edit.
-- | Functions for generating term decoders from type modules

module Hydra.Decoding where
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Predicates as Predicates
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
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
      Core.TypeEffect v0 -> collectOrdConstrainedVariables v0
      Core.TypeEither v0 -> Lists.concat2 (collectOrdConstrainedVariables (Core.eitherTypeLeft v0)) (collectOrdConstrainedVariables (Core.eitherTypeRight v0))
      Core.TypeForall v0 -> collectOrdConstrainedVariables (Core.forallTypeBody v0)
      Core.TypeList v0 -> collectOrdConstrainedVariables v0
      Core.TypeMap v0 -> Lists.concat [
        collectTypeVariablesFromType (Core.mapTypeKeys v0),
        (collectOrdConstrainedVariables (Core.mapTypeKeys v0)),
        (collectOrdConstrainedVariables (Core.mapTypeValues v0))]
      Core.TypeOptional v0 -> collectOrdConstrainedVariables v0
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
      Core.TypeEffect v0 -> collectTypeVariablesFromType v0
      Core.TypeEither v0 -> Lists.concat2 (collectTypeVariablesFromType (Core.eitherTypeLeft v0)) (collectTypeVariablesFromType (Core.eitherTypeRight v0))
      Core.TypeForall v0 -> collectTypeVariablesFromType (Core.forallTypeBody v0)
      Core.TypeList v0 -> collectTypeVariablesFromType v0
      Core.TypeMap v0 -> Lists.concat2 (collectTypeVariablesFromType (Core.mapTypeKeys v0)) (collectTypeVariablesFromType (Core.mapTypeValues v0))
      Core.TypeOptional v0 -> collectTypeVariablesFromType v0
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
    Eithers.bind (DecodeCore.type_ graph (Core.bindingTerm b)) (\typ ->
      let rtype = decoderFullResultTypeNamed (Core.bindingName b) typ
          rawBody = decodeTypeNamed (Core.bindingName b) typ rtype
          description =
                  Strings.cat [
                    "Decoder for ",
                    (Core.unName (Core.bindingName b))]
      in (Right (Core.Binding {
        Core.bindingName = (decodeBindingName (Core.bindingName b)),
        Core.bindingTerm = (Annotations.setTermDescription (Just description) rawBody),
        Core.bindingTypeScheme = (Just (decoderTypeSchemeNamed (Core.bindingName b) typ))})))
-- | Generate a binding name for a decoder function from a type name
decodeBindingName :: Core.Name -> Core.Name
decodeBindingName n =

      let parts = Strings.splitOn "." (Core.unName n)
          localPart = Formatting.decapitalize (Names.localNameOf n)
          localResult = Core.Name localPart
      in (Optionals.cases (Lists.maybeInit parts) localResult (\nsParts -> Optionals.cases (Lists.uncons nsParts) localResult (\nsUc ->
        let tail = Pairs.second nsUc
        in (Core.Name (Strings.intercalate "." (Lists.concat2 [
          "hydra",
          "decode"] (Lists.concat2 tail [
          localPart])))))))
-- | Generate a decoder for an Either type
decodeEitherType :: Core.EitherType -> Core.Term
decodeEitherType et =

      let leftDecoder = decodeType (Core.eitherTypeLeft et)
          rightDecoder = decodeType (Core.eitherTypeRight et)
      in (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.extract.core.decodeEither")),
              Core.typeApplicationTermType = (decoderFullResultType (Core.eitherTypeLeft et))})),
            Core.typeApplicationTermType = (decoderFullResultType (Core.eitherTypeRight et))})),
          Core.applicationArgument = leftDecoder})),
        Core.applicationArgument = rightDecoder}))
-- | Generate a decoder for a polymorphic (forall) type
decodeForallType :: Core.ForallType -> Core.Term
decodeForallType ft =
    Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (decodeBindingName (Core.forallTypeParameter ft)),
      Core.lambdaDomain = (Just (Core.TypeFunction (Core.FunctionType {
        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
          Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
            Core.eitherTypeRight = (Core.TypeVariable (Core.forallTypeParameter ft))}))}))}))),
      Core.lambdaBody = (decodeType (Core.forallTypeBody ft))})
-- | Generate a decoder for a list type
decodeListType :: Core.Type -> Core.Term
decodeListType elemType =

      let elemDecoder = decodeType elemType
      in (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
          Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.extract.core.decodeList")),
          Core.typeApplicationTermType = (decoderFullResultType elemType)})),
        Core.applicationArgument = elemDecoder}))
-- | Generate a decoder for a literal type
decodeLiteralType :: Core.LiteralType -> Core.Term
decodeLiteralType lt =
    case lt of
      Core.LiteralTypeBinary -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "cx"),
        Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "raw"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                  Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                    Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeBinary)}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "err"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                  Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                    Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeBinary)}))}))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "stripped"),
                Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                    Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))})))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeBinary)}))),
                    Core.caseStatementCases = [
                      Core.CaseAlternative {
                        Core.caseAlternativeName = (Core.Name "literal"),
                        Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "v"),
                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Literal"))),
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                              Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected binary literal"))})))),
                                  Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeBinary)}))),
                              Core.caseStatementCases = [
                                Core.CaseAlternative {
                                  Core.caseAlternativeName = (Core.Name "binary"),
                                  Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "b"),
                                    Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeBinary)),
                                    Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "b")))),
                                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                      Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeBinary)}))}))}]})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
      Core.LiteralTypeBoolean -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "cx"),
        Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "raw"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                  Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                    Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeBoolean)}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "err"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                  Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                    Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeBoolean)}))}))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "stripped"),
                Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                    Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))})))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeBoolean)}))),
                    Core.caseStatementCases = [
                      Core.CaseAlternative {
                        Core.caseAlternativeName = (Core.Name "literal"),
                        Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "v"),
                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Literal"))),
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                              Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected boolean literal"))})))),
                                  Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeBoolean)}))),
                              Core.caseStatementCases = [
                                Core.CaseAlternative {
                                  Core.caseAlternativeName = (Core.Name "boolean"),
                                  Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "b"),
                                    Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeBoolean)),
                                    Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "b")))),
                                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                      Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeBoolean)}))}))}]})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
      Core.LiteralTypeDecimal -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "cx"),
        Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "raw"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                  Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                    Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeDecimal)}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "err"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                  Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                    Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeDecimal)}))}))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "stripped"),
                Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                    Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))})))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeDecimal)}))),
                    Core.caseStatementCases = [
                      Core.CaseAlternative {
                        Core.caseAlternativeName = (Core.Name "literal"),
                        Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "v"),
                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Literal"))),
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                              Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected decimal literal"))})))),
                                  Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeDecimal)}))),
                              Core.caseStatementCases = [
                                Core.CaseAlternative {
                                  Core.caseAlternativeName = (Core.Name "decimal"),
                                  Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "d"),
                                    Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeDecimal)),
                                    Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "d")))),
                                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                      Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeDecimal)}))}))}]})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
      Core.LiteralTypeFloat v0 -> case v0 of
        Core.FloatTypeFloat32 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                    Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                      Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeFloat v0))}))})),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                    Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeFloat v0))}))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))})))),
                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeFloat v0))}))),
                      Core.caseStatementCases = [
                        Core.CaseAlternative {
                          Core.caseAlternativeName = (Core.Name "literal"),
                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Literal"))),
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                    Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                        "expected ",
                                        "float32",
                                        " literal"])))})))),
                                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeFloat v0))}))),
                                Core.caseStatementCases = [
                                  Core.CaseAlternative {
                                    Core.caseAlternativeName = (Core.Name "float"),
                                    Core.caseAlternativeHandler = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.FloatValue"),
                                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                              "expected ",
                                              "float32",
                                              " value"])))})))),
                                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeFloat v0))}))),
                                      Core.caseStatementCases = [
                                        Core.CaseAlternative {
                                          Core.caseAlternativeName = (Core.Name "float32"),
                                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "f"),
                                            Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeFloat v0))),
                                            Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "f")))),
                                                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeFloat v0))}))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.FloatTypeFloat64 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                    Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                      Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeFloat v0))}))})),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                    Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeFloat v0))}))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))})))),
                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeFloat v0))}))),
                      Core.caseStatementCases = [
                        Core.CaseAlternative {
                          Core.caseAlternativeName = (Core.Name "literal"),
                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Literal"))),
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                    Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                        "expected ",
                                        "float64",
                                        " literal"])))})))),
                                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeFloat v0))}))),
                                Core.caseStatementCases = [
                                  Core.CaseAlternative {
                                    Core.caseAlternativeName = (Core.Name "float"),
                                    Core.caseAlternativeHandler = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.FloatValue"),
                                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                              "expected ",
                                              "float64",
                                              " value"])))})))),
                                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeFloat v0))}))),
                                      Core.caseStatementCases = [
                                        Core.CaseAlternative {
                                          Core.caseAlternativeName = (Core.Name "float64"),
                                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "f"),
                                            Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeFloat v0))),
                                            Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "f")))),
                                                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeFloat v0))}))}))}]}))}]})),
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
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                    Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                      Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))})),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                    Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))})))),
                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                      Core.caseStatementCases = [
                        Core.CaseAlternative {
                          Core.caseAlternativeName = (Core.Name "literal"),
                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Literal"))),
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                    Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                        "expected ",
                                        "bigint",
                                        " literal"])))})))),
                                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                                Core.caseStatementCases = [
                                  Core.CaseAlternative {
                                    Core.caseAlternativeName = (Core.Name "integer"),
                                    Core.caseAlternativeHandler = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                              "expected ",
                                              "bigint",
                                              " value"])))})))),
                                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                                      Core.caseStatementCases = [
                                        Core.CaseAlternative {
                                          Core.caseAlternativeName = (Core.Name "bigint"),
                                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "i"),
                                            Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger v0))),
                                            Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i")))),
                                                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.IntegerTypeInt8 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                    Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                      Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))})),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                    Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))})))),
                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                      Core.caseStatementCases = [
                        Core.CaseAlternative {
                          Core.caseAlternativeName = (Core.Name "literal"),
                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Literal"))),
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                    Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                        "expected ",
                                        "int8",
                                        " literal"])))})))),
                                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                                Core.caseStatementCases = [
                                  Core.CaseAlternative {
                                    Core.caseAlternativeName = (Core.Name "integer"),
                                    Core.caseAlternativeHandler = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                              "expected ",
                                              "int8",
                                              " value"])))})))),
                                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                                      Core.caseStatementCases = [
                                        Core.CaseAlternative {
                                          Core.caseAlternativeName = (Core.Name "int8"),
                                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "i"),
                                            Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger v0))),
                                            Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i")))),
                                                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.IntegerTypeInt16 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                    Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                      Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))})),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                    Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))})))),
                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                      Core.caseStatementCases = [
                        Core.CaseAlternative {
                          Core.caseAlternativeName = (Core.Name "literal"),
                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Literal"))),
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                    Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                        "expected ",
                                        "int16",
                                        " literal"])))})))),
                                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                                Core.caseStatementCases = [
                                  Core.CaseAlternative {
                                    Core.caseAlternativeName = (Core.Name "integer"),
                                    Core.caseAlternativeHandler = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                              "expected ",
                                              "int16",
                                              " value"])))})))),
                                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                                      Core.caseStatementCases = [
                                        Core.CaseAlternative {
                                          Core.caseAlternativeName = (Core.Name "int16"),
                                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "i"),
                                            Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger v0))),
                                            Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i")))),
                                                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.IntegerTypeInt32 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                    Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                      Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))})),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                    Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))})))),
                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                      Core.caseStatementCases = [
                        Core.CaseAlternative {
                          Core.caseAlternativeName = (Core.Name "literal"),
                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Literal"))),
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                    Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                        "expected ",
                                        "int32",
                                        " literal"])))})))),
                                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                                Core.caseStatementCases = [
                                  Core.CaseAlternative {
                                    Core.caseAlternativeName = (Core.Name "integer"),
                                    Core.caseAlternativeHandler = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                              "expected ",
                                              "int32",
                                              " value"])))})))),
                                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                                      Core.caseStatementCases = [
                                        Core.CaseAlternative {
                                          Core.caseAlternativeName = (Core.Name "int32"),
                                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "i"),
                                            Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger v0))),
                                            Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i")))),
                                                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.IntegerTypeInt64 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                    Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                      Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))})),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                    Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))})))),
                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                      Core.caseStatementCases = [
                        Core.CaseAlternative {
                          Core.caseAlternativeName = (Core.Name "literal"),
                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Literal"))),
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                    Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                        "expected ",
                                        "int64",
                                        " literal"])))})))),
                                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                                Core.caseStatementCases = [
                                  Core.CaseAlternative {
                                    Core.caseAlternativeName = (Core.Name "integer"),
                                    Core.caseAlternativeHandler = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                              "expected ",
                                              "int64",
                                              " value"])))})))),
                                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                                      Core.caseStatementCases = [
                                        Core.CaseAlternative {
                                          Core.caseAlternativeName = (Core.Name "int64"),
                                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "i"),
                                            Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger v0))),
                                            Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i")))),
                                                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.IntegerTypeUint8 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                    Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                      Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))})),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                    Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))})))),
                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                      Core.caseStatementCases = [
                        Core.CaseAlternative {
                          Core.caseAlternativeName = (Core.Name "literal"),
                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Literal"))),
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                    Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                        "expected ",
                                        "uint8",
                                        " literal"])))})))),
                                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                                Core.caseStatementCases = [
                                  Core.CaseAlternative {
                                    Core.caseAlternativeName = (Core.Name "integer"),
                                    Core.caseAlternativeHandler = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                              "expected ",
                                              "uint8",
                                              " value"])))})))),
                                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                                      Core.caseStatementCases = [
                                        Core.CaseAlternative {
                                          Core.caseAlternativeName = (Core.Name "uint8"),
                                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "i"),
                                            Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger v0))),
                                            Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i")))),
                                                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.IntegerTypeUint16 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                    Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                      Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))})),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                    Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))})))),
                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                      Core.caseStatementCases = [
                        Core.CaseAlternative {
                          Core.caseAlternativeName = (Core.Name "literal"),
                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Literal"))),
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                    Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                        "expected ",
                                        "uint16",
                                        " literal"])))})))),
                                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                                Core.caseStatementCases = [
                                  Core.CaseAlternative {
                                    Core.caseAlternativeName = (Core.Name "integer"),
                                    Core.caseAlternativeHandler = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                              "expected ",
                                              "uint16",
                                              " value"])))})))),
                                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                                      Core.caseStatementCases = [
                                        Core.CaseAlternative {
                                          Core.caseAlternativeName = (Core.Name "uint16"),
                                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "i"),
                                            Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger v0))),
                                            Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i")))),
                                                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.IntegerTypeUint32 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                    Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                      Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))})),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                    Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))})))),
                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                      Core.caseStatementCases = [
                        Core.CaseAlternative {
                          Core.caseAlternativeName = (Core.Name "literal"),
                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Literal"))),
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                    Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                        "expected ",
                                        "uint32",
                                        " literal"])))})))),
                                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                                Core.caseStatementCases = [
                                  Core.CaseAlternative {
                                    Core.caseAlternativeName = (Core.Name "integer"),
                                    Core.caseAlternativeHandler = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                              "expected ",
                                              "uint32",
                                              " value"])))})))),
                                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                                      Core.caseStatementCases = [
                                        Core.CaseAlternative {
                                          Core.caseAlternativeName = (Core.Name "uint32"),
                                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "i"),
                                            Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger v0))),
                                            Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i")))),
                                                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
        Core.IntegerTypeUint64 -> Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "cx"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "raw"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                    Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                      Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))})),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "err"),
                    Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                    Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "stripped"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))})))),
                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                      Core.caseStatementCases = [
                        Core.CaseAlternative {
                          Core.caseAlternativeName = (Core.Name "literal"),
                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Literal"))),
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                    Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                        "expected ",
                                        "uint64",
                                        " literal"])))})))),
                                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                                Core.caseStatementCases = [
                                  Core.CaseAlternative {
                                    Core.caseAlternativeName = (Core.Name "integer"),
                                    Core.caseAlternativeHandler = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                          Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                            Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                                              "expected ",
                                              "uint64",
                                              " value"])))})))),
                                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))),
                                      Core.caseStatementCases = [
                                        Core.CaseAlternative {
                                          Core.caseAlternativeName = (Core.Name "uint64"),
                                          Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "i"),
                                            Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger v0))),
                                            Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                                Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "i")))),
                                                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger v0))}))}))}]}))}]})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))}))}]})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))})
      Core.LiteralTypeString -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "cx"),
        Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "raw"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                  Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                    Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "err"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                  Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                    Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))}))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "stripped"),
                Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                    Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))})))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))),
                    Core.caseStatementCases = [
                      Core.CaseAlternative {
                        Core.caseAlternativeName = (Core.Name "literal"),
                        Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "v"),
                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Literal"))),
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                              Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected string literal"))})))),
                                  Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))),
                              Core.caseStatementCases = [
                                Core.CaseAlternative {
                                  Core.caseAlternativeName = (Core.Name "string"),
                                  Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "s"),
                                    Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                                    Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "s")))),
                                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                      Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))}))}]})),
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
          Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.extract.core.decodeMap")),
              Core.typeApplicationTermType = (decoderFullResultType (Core.mapTypeKeys mt))})),
            Core.typeApplicationTermType = (decoderFullResultType (Core.mapTypeValues mt))})),
          Core.applicationArgument = keyDecoder})),
        Core.applicationArgument = valDecoder}))
-- | Generate a decoder for an optional type
decodeMaybeType :: Core.Type -> Core.Term
decodeMaybeType elemType =

      let elemDecoder = decodeType elemType
      in (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
          Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.extract.core.decodeMaybe")),
          Core.typeApplicationTermType = (decoderFullResultType elemType)})),
        Core.applicationArgument = elemDecoder}))
-- | Transform a type module into a decoder module
decodeModule :: t0 -> Graph.Graph -> Packaging.Module -> Either Errors.Error (Maybe Packaging.Module)
decodeModule cx graph mod =
    Eithers.bind (filterTypeBindings cx graph (Optionals.cat (Lists.map (\d -> case d of
      Packaging.DefinitionType v0 -> Just ((\name -> \typ ->
        let schemaTerm = Core.TermVariable (Core.Name "hydra.core.Type")
            dataTerm =
                    Annotations.normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
                      Core.annotatedTermBody = (EncodeCore.type_ typ),
                      Core.annotatedTermAnnotation = (Annotations.wrapAnnotationMap (Maps.fromList [
                        (Constants.keyType, schemaTerm)]))}))
        in Core.Binding {
          Core.bindingName = name,
          Core.bindingTerm = dataTerm,
          Core.bindingTypeScheme = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
            Core.typeSchemeConstraints = Nothing}))}) (Packaging.typeDefinitionName v0) (Core.typeSchemeBody (Packaging.typeDefinitionBody v0)))
      _ -> Nothing) (Packaging.moduleDefinitions mod)))) (\typeBindings -> Logic.ifElse (Lists.null typeBindings) (Right Nothing) (Eithers.bind (Eithers.mapList (\b -> Eithers.bimap (\_e -> Errors.ErrorDecoding _e) (\x -> x) (decodeBinding cx graph b)) typeBindings) (\decodedBindings ->
      let allDecodedDeps =
              Lists.nub (Lists.map decodeModuleName (Lists.map (\dep -> Packaging.moduleDependencyModule dep) (Packaging.moduleDependencies mod)))
      in (Right (Just (Packaging.Module {
        Packaging.moduleName = (decodeModuleName (Packaging.moduleName mod)),
        Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
          Packaging.entityMetadataDescription = (Just (Strings.cat [
            "Term decoders for ",
            (Packaging.unModuleName (Packaging.moduleName mod))])),
          Packaging.entityMetadataComments = [],
          Packaging.entityMetadataSeeAlso = [],
          Packaging.entityMetadataLifecycle = Nothing})),
        Packaging.moduleDependencies = (Lists.map (\ns -> Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = ns,
          Packaging.moduleDependencyPackage = Nothing}) (Lists.concat2 [
          Packaging.ModuleName "hydra.extract.core",
          (Packaging.ModuleName "hydra.lexical"),
          (Packaging.ModuleName "hydra.rewriting"),
          (Packaging.moduleName mod),
          (Packaging.ModuleName "hydra.util")] allDecodedDeps)),
        Packaging.moduleDefinitions = (Lists.map (\b -> Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.bindingName b),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Optionals.map Scoping.typeSchemeToTermSignature (Core.bindingTypeScheme b)),
          Packaging.termDefinitionBody = (Core.bindingTerm b)})) decodedBindings)}))))))
-- | Generate a decoder module name from a source module name
decodeModuleName :: Packaging.ModuleName -> Packaging.ModuleName
decodeModuleName ns =

      let parts = Strings.splitOn "." (Packaging.unModuleName ns)
          fallback = Packaging.ModuleName (Packaging.unModuleName ns)
      in (Optionals.cases (Lists.uncons parts) fallback (\uc -> Packaging.ModuleName (Strings.cat [
        "hydra.decode.",
        (Strings.intercalate "." (Pairs.second uc))])))
-- | Generate a decoder for a pair type
decodePairType :: Core.PairType -> Core.Term
decodePairType pt =

      let firstDecoder = decodeType (Core.pairTypeFirst pt)
          secondDecoder = decodeType (Core.pairTypeSecond pt)
      in (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.extract.core.decodePair")),
              Core.typeApplicationTermType = (decoderFullResultType (Core.pairTypeFirst pt))})),
            Core.typeApplicationTermType = (decoderFullResultType (Core.pairTypeSecond pt))})),
          Core.applicationArgument = firstDecoder})),
        Core.applicationArgument = secondDecoder}))
-- | Generate a decoder for a record type
decodeRecordType :: [Core.FieldType] -> Core.Term
decodeRecordType rt = decodeRecordTypeImpl (Core.Name "unknown") rt (Core.TypeVariable (Core.Name "unknown"))
-- | Generate a decoder for a record type with a type name. rtype is the fully-applied result type (e.g. Table<v>) used for the record's body annotations. (#476)
decodeRecordTypeImpl :: Core.Name -> [Core.FieldType] -> Core.Type -> Core.Term
decodeRecordTypeImpl tname rt rtype =

      let recType = rtype
          graphType = Core.TypeVariable (Core.Name "hydra.graph.Graph")
          termType = Core.TypeVariable (Core.Name "hydra.core.Term")
          decodeFieldTerm =
                  \ft -> Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.extract.core.requireField")),
                                Core.typeApplicationTermType = graphType})),
                              Core.typeApplicationTermType = termType})),
                            Core.typeApplicationTermType = (decoderFullResultType (Core.fieldTypeType ft))})),
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
                    Core.lambdaDomain = (Just (decoderFullResultType (Core.fieldTypeType ft))),
                    Core.lambdaBody = body})
          decodeBody =
                  Lists.foldl (\acc -> \ft -> Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.bind")),
                            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                          Core.typeApplicationTermType = (decoderFullResultType (Core.fieldTypeType ft))})),
                        Core.typeApplicationTermType = recType})),
                      Core.applicationArgument = (decodeFieldTerm ft)})),
                    Core.applicationArgument = (toFieldLambda ft acc)})) (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                        Core.recordTypeName = tname,
                        Core.recordFields = (Lists.map (\ft -> Core.Field {
                          Core.fieldName = (Core.fieldTypeName ft),
                          Core.fieldTerm = (Core.TermVariable (localVarName ft))}) rt)})))),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                    Core.typeApplicationTermType = recType})) (Lists.reverse rt)
      in (Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "cx"),
        Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "raw"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                  Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                    Core.eitherTypeRight = recType}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "err"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                  Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                    Core.typeApplicationTermType = recType}))}))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "stripped"),
                Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                    Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Strings.cat [
                            "expected a record of type ",
                            (Core.unName tname)])))})))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = recType}))),
                    Core.caseStatementCases = [
                      Core.CaseAlternative {
                        Core.caseAlternativeName = (Core.Name "record"),
                        Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "record"),
                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Record"))),
                          Core.lambdaBody = (Core.TermLet (Core.Let {
                            Core.letBindings = [
                              Core.Binding {
                                Core.bindingName = (Core.Name "fieldMap"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.toFieldMap")),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "record"))})),
                                Core.bindingTypeScheme = (Just (Core.TypeScheme {
                                  Core.typeSchemeVariables = [],
                                  Core.typeSchemeBody = (Core.TypeMap (Core.MapType {
                                    Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                                    Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                                  Core.typeSchemeConstraints = Nothing}))}],
                            Core.letBody = decodeBody}))}))}]})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))}))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.stripWithDecodingError")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))}))}))
-- | Generate a decoder for a record type with element name
decodeRecordTypeNamed :: Core.Name -> [Core.FieldType] -> Core.Type -> Core.Term
decodeRecordTypeNamed ename rt rtype = decodeRecordTypeImpl ename rt rtype
-- | Generate a decoder for a set type
decodeSetType :: Core.Type -> Core.Term
decodeSetType elemType =

      let elemDecoder = decodeType elemType
      in (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
          Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.extract.core.decodeSet")),
          Core.typeApplicationTermType = (decoderFullResultType elemType)})),
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
      Core.TypeOptional v0 -> decodeMaybeType v0
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
        Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "t"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
          Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unsupported type variant"))})))),
              Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}))}))})
-- | Generate a decoder term for a Type, with element name for nominal types. rtype is the FULLY-APPLIED result type for nominal bodies (e.g. DataRow<v> for forall v. wrap...), so the body's intermediate type annotations carry the type parameters rather than a bare nominal name — otherwise Java/Scala coders emit raw types that fail to compile against the parameterized signature. (#476)
decodeTypeNamed :: Core.Name -> Core.Type -> Core.Type -> Core.Term
decodeTypeNamed ename typ rtype =
    case typ of
      Core.TypeAnnotated v0 -> decodeTypeNamed ename (Core.annotatedTypeBody v0) rtype
      Core.TypeApplication v0 -> Core.TermApplication (Core.Application {
        Core.applicationFunction = (decodeType (Core.applicationTypeFunction v0)),
        Core.applicationArgument = (decodeType (Core.applicationTypeArgument v0))})
      Core.TypeEither v0 -> decodeEitherType v0
      Core.TypeForall v0 -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (decodeBindingName (Core.forallTypeParameter v0)),
        Core.lambdaDomain = (Just (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
            Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
              Core.eitherTypeRight = (Core.TypeVariable (Core.forallTypeParameter v0))}))}))}))),
        Core.lambdaBody = (decodeTypeNamed ename (Core.forallTypeBody v0) rtype)})
      Core.TypeList v0 -> decodeListType v0
      Core.TypeLiteral v0 -> decodeLiteralType v0
      Core.TypeMap v0 -> decodeMapType v0
      Core.TypeOptional v0 -> decodeMaybeType v0
      Core.TypePair v0 -> decodePairType v0
      Core.TypeRecord v0 -> decodeRecordTypeNamed ename v0 rtype
      Core.TypeSet v0 -> decodeSetType v0
      Core.TypeUnion v0 -> decodeUnionTypeNamed ename v0 rtype
      Core.TypeUnit -> decodeUnitType
      Core.TypeVoid -> decodeUnitType
      Core.TypeWrap v0 -> decodeWrappedTypeNamed ename v0 rtype
      Core.TypeVariable v0 -> Core.TermVariable (decodeBindingName v0)
      _ -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "cx"),
        Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "t"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
          Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unsupported type variant"))})))),
              Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}))}))})
-- | Generate a decoder for a union type
decodeUnionType :: [Core.FieldType] -> Core.Term
decodeUnionType rt = decodeUnionTypeNamed (Core.Name "unknown") rt (Core.TypeVariable (Core.Name "unknown"))
-- | Generate a decoder for a union type with the given element name. rtype is the fully-applied result type (e.g. Foo<v>) used for body annotations. (#476)
decodeUnionTypeNamed :: Core.Name -> [Core.FieldType] -> Core.Type -> Core.Term
decodeUnionTypeNamed ename rt rtype =

      let unionType = rtype
          decErrType = Core.TypeVariable (Core.Name "hydra.errors.DecodingError")
          nameType = Core.TypeVariable (Core.Name "hydra.core.Name")
          variantFnType =
                  Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                    Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                      Core.eitherTypeLeft = decErrType,
                      Core.eitherTypeRight = unionType}))})
          toVariantPair =
                  \ft ->
                    let fldType = decoderFullResultType (Core.fieldTypeType ft)
                    in (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermPair (
                          Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString (Core.unName (Core.fieldTypeName ft))))}),
                          (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "input"),
                            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.map")),
                                      Core.typeApplicationTermType = fldType})),
                                    Core.typeApplicationTermType = unionType})),
                                  Core.typeApplicationTermType = decErrType})),
                                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "t"),
                                  Core.lambdaDomain = (Just fldType),
                                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                                    Core.injectionTypeName = ename,
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.fieldTypeName ft),
                                      Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))}))})),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (decodeType (Core.fieldTypeType ft)),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Name"))})),
                      Core.typeApplicationTermType = variantFnType}))
      in (Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "cx"),
        Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "raw"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                  Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                    Core.eitherTypeRight = unionType}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "err"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                  Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                    Core.typeApplicationTermType = unionType}))}))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "stripped"),
                Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                    Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected union"))})))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = unionType}))),
                    Core.caseStatementCases = [
                      Core.CaseAlternative {
                        Core.caseAlternativeName = (Core.Name "inject"),
                        Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "inj"),
                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Injection"))),
                          Core.lambdaBody = (Core.TermLet (Core.Let {
                            Core.letBindings = [
                              Core.Binding {
                                Core.bindingName = (Core.Name "field"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
                                    Core.projectionFieldName = (Core.Name "field")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "inj"))})),
                                Core.bindingTypeScheme = (Just (Core.TypeScheme {
                                  Core.typeSchemeVariables = [],
                                  Core.typeSchemeBody = (Core.TypeVariable (Core.Name "hydra.core.Field")),
                                  Core.typeSchemeConstraints = Nothing}))},
                              Core.Binding {
                                Core.bindingName = (Core.Name "fname"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                                    Core.projectionFieldName = (Core.Name "name")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "field"))})),
                                Core.bindingTypeScheme = (Just (Core.TypeScheme {
                                  Core.typeSchemeVariables = [],
                                  Core.typeSchemeBody = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                                  Core.typeSchemeConstraints = Nothing}))},
                              Core.Binding {
                                Core.bindingName = (Core.Name "fterm"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                                    Core.projectionFieldName = (Core.Name "term")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "field"))})),
                                Core.bindingTypeScheme = (Just (Core.TypeScheme {
                                  Core.typeSchemeVariables = [],
                                  Core.typeSchemeBody = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                                  Core.typeSchemeConstraints = Nothing}))},
                              Core.Binding {
                                Core.bindingName = (Core.Name "variantMap"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.maps.fromList")),
                                      Core.typeApplicationTermType = nameType})),
                                    Core.typeApplicationTermType = variantFnType})),
                                  Core.applicationArgument = (Core.TermList (Lists.map toVariantPair rt))})),
                                Core.bindingTypeScheme = (Just (Core.TypeScheme {
                                  Core.typeSchemeVariables = [],
                                  Core.typeSchemeBody = (Core.TypeMap (Core.MapType {
                                    Core.mapTypeKeys = nameType,
                                    Core.mapTypeValues = variantFnType})),
                                  Core.typeSchemeConstraints = Nothing}))}],
                            Core.letBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.optionals.cases")),
                                      Core.typeApplicationTermType = variantFnType})),
                                    Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                                      Core.eitherTypeLeft = decErrType,
                                      Core.eitherTypeRight = unionType}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                          Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.maps.lookup")),
                                          Core.typeApplicationTermType = nameType})),
                                        Core.typeApplicationTermType = variantFnType})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "variantMap"))}))})),
                                Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                    Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                      Core.wrappedTermBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.cat")),
                                        Core.applicationArgument = (Core.TermList [
                                          Core.TermLiteral (Core.LiteralString "no such field "),
                                          (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.core.Name")),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))})),
                                          (Core.TermLiteral (Core.LiteralString " in union"))])}))})))),
                                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                                  Core.typeApplicationTermType = unionType}))})),
                              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "f"),
                                Core.lambdaDomain = (Just variantFnType),
                                Core.lambdaBody = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "fterm"))}))}))}))}))}))}]})),
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
      Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
      Core.lambdaBody = (Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "t"),
        Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
        Core.lambdaBody = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.decodeUnit")),
            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
          Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))}))})
-- | Generate a decoder for a wrapped type
decodeWrappedType :: Core.Type -> Core.Term
decodeWrappedType wt = decodeWrappedTypeNamed (Core.Name "unknown") wt (Core.TypeVariable (Core.Name "unknown"))
-- | Generate a decoder for a wrapped type with the given element name. rtype is the fully-applied result type (e.g. DataRow<v>) used for body annotations. (#476)
decodeWrappedTypeNamed :: Core.Name -> Core.Type -> Core.Type -> Core.Term
decodeWrappedTypeNamed ename wt rtype =

      let bodyDecoder = decodeType wt
          bodyType = decoderFullResultType wt
      in (Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "cx"),
        Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.graph.Graph"))),
        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "raw"),
          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.either")),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                  Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                    Core.eitherTypeRight = rtype}))})),
                Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "err"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))),
                  Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err")))),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                    Core.typeApplicationTermType = rtype}))}))})),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "stripped"),
                Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.Term"))),
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                    Core.caseStatementDefault = (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected wrapped type"))})))),
                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                      Core.typeApplicationTermType = rtype}))),
                    Core.caseStatementCases = [
                      Core.CaseAlternative {
                        Core.caseAlternativeName = (Core.Name "wrap"),
                        Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "wrappedTerm"),
                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.core.WrappedTerm"))),
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                    Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "hydra.lib.eithers.map")),
                                    Core.typeApplicationTermType = bodyType})),
                                  Core.typeApplicationTermType = rtype})),
                                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError"))})),
                              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "b"),
                                Core.lambdaDomain = (Just bodyType),
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
                                  Core.projectionFieldName = (Core.Name "body")})),
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
      Core.TypeEffect v0 -> Core.TypeEffect (decoderFullResultType v0)
      Core.TypeEither v0 -> Core.TypeEither (Core.EitherType {
        Core.eitherTypeLeft = (decoderFullResultType (Core.eitherTypeLeft v0)),
        Core.eitherTypeRight = (decoderFullResultType (Core.eitherTypeRight v0))})
      Core.TypeForall v0 -> Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = (decoderFullResultType (Core.forallTypeBody v0)),
        Core.applicationTypeArgument = (Core.TypeVariable (Core.forallTypeParameter v0))})
      Core.TypeList v0 -> Core.TypeList (decoderFullResultType v0)
      Core.TypeLiteral v0 -> Core.TypeLiteral v0
      Core.TypeMap v0 -> Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = (decoderFullResultType (Core.mapTypeKeys v0)),
        Core.mapTypeValues = (decoderFullResultType (Core.mapTypeValues v0))})
      Core.TypeOptional v0 -> Core.TypeOptional (decoderFullResultType v0)
      Core.TypePair v0 -> Core.TypePair (Core.PairType {
        Core.pairTypeFirst = (decoderFullResultType (Core.pairTypeFirst v0)),
        Core.pairTypeSecond = (decoderFullResultType (Core.pairTypeSecond v0))})
      Core.TypeRecord _ -> Core.TypeVariable (Core.Name "hydra.core.Term")
      Core.TypeSet v0 -> Core.TypeSet (decoderFullResultType v0)
      Core.TypeUnion _ -> Core.TypeVariable (Core.Name "hydra.core.Term")
      Core.TypeUnit -> Core.TypeUnit
      Core.TypeVariable v0 -> Core.TypeVariable v0
      Core.TypeVoid -> Core.TypeVoid
      Core.TypeWrap v0 -> decoderFullResultType v0
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
      Core.TypeEffect v0 -> Core.TypeEffect (decoderFullResultType v0)
      Core.TypeEither v0 -> Core.TypeEither (Core.EitherType {
        Core.eitherTypeLeft = (decoderFullResultType (Core.eitherTypeLeft v0)),
        Core.eitherTypeRight = (decoderFullResultType (Core.eitherTypeRight v0))})
      Core.TypeList v0 -> Core.TypeList (decoderFullResultType v0)
      Core.TypeLiteral v0 -> Core.TypeLiteral v0
      Core.TypeMap v0 -> Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = (decoderFullResultType (Core.mapTypeKeys v0)),
        Core.mapTypeValues = (decoderFullResultType (Core.mapTypeValues v0))})
      Core.TypeOptional v0 -> Core.TypeOptional (decoderFullResultType v0)
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
                  Logic.ifElse (Lists.null ordVars) Nothing (Just (Maps.fromList (Lists.map (\v -> (
                    v,
                    Core.TypeVariableConstraints {
                      Core.typeVariableConstraintsClasses = [
                        Core.TypeClassConstraintSimple (Core.Name "ordering")]})) ordVars)))
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
                  Logic.ifElse (Lists.null ordVars) Nothing (Just (Maps.fromList (Lists.map (\v -> (
                    v,
                    Core.TypeVariableConstraints {
                      Core.typeVariableConstraintsClasses = [
                        Core.TypeClassConstraintSimple (Core.Name "ordering")]})) ordVars)))
      in Core.TypeScheme {
        Core.typeSchemeVariables = typeVars,
        Core.typeSchemeBody = (decoderTypeNamed ename typ),
        Core.typeSchemeConstraints = constraints}
-- | Filter bindings to only decodable type definitions
filterTypeBindings :: t0 -> Graph.Graph -> [Core.Binding] -> Either Errors.Error [Core.Binding]
filterTypeBindings cx graph bindings =
    Eithers.map Optionals.cat (Eithers.mapList (isDecodableBinding cx graph) (Lists.filter Annotations.isNativeType bindings))
-- | Check if a binding is decodable (serializable type)
isDecodableBinding :: t0 -> Graph.Graph -> Core.Binding -> Either Errors.Error (Maybe Core.Binding)
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
