-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.core

module Hydra.Decode.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

annotatedTerm :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.AnnotatedTerm
annotatedTerm cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "body" term fieldMap cx) (\field_body -> Eithers.bind (ExtractCore.requireField "annotation" (ExtractCore.decodeMap name term) fieldMap cx) (\field_annotation -> Right (Core.AnnotatedTerm {
          Core.annotatedTermBody = field_body,
          Core.annotatedTermAnnotation = field_annotation}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

annotatedType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.AnnotatedType
annotatedType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "body" type_ fieldMap cx) (\field_body -> Eithers.bind (ExtractCore.requireField "annotation" (ExtractCore.decodeMap name term) fieldMap cx) (\field_annotation -> Right (Core.AnnotatedType {
          Core.annotatedTypeBody = field_body,
          Core.annotatedTypeAnnotation = field_annotation}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

application :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.Application
application cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "function" term fieldMap cx) (\field_function -> Eithers.bind (ExtractCore.requireField "argument" term fieldMap cx) (\field_argument -> Right (Core.Application {
          Core.applicationFunction = field_function,
          Core.applicationArgument = field_argument}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

applicationType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.ApplicationType
applicationType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "function" type_ fieldMap cx) (\field_function -> Eithers.bind (ExtractCore.requireField "argument" type_ fieldMap cx) (\field_argument -> Right (Core.ApplicationType {
          Core.applicationTypeFunction = field_function,
          Core.applicationTypeArgument = field_argument}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

binding :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.Binding
binding cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "term" term fieldMap cx) (\field_term -> Eithers.bind (ExtractCore.requireField "type" (ExtractCore.decodeMaybe typeScheme) fieldMap cx) (\field_type -> Right (Core.Binding {
          Core.bindingName = field_name,
          Core.bindingTerm = field_term,
          Core.bindingType = field_type})))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

caseStatement :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.CaseStatement
caseStatement cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "typeName" name fieldMap cx) (\field_typeName -> Eithers.bind (ExtractCore.requireField "default" (ExtractCore.decodeMaybe term) fieldMap cx) (\field_default -> Eithers.bind (ExtractCore.requireField "cases" (ExtractCore.decodeList field) fieldMap cx) (\field_cases -> Right (Core.CaseStatement {
          Core.caseStatementTypeName = field_typeName,
          Core.caseStatementDefault = field_default,
          Core.caseStatementCases = field_cases})))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

eitherType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.EitherType
eitherType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "left" type_ fieldMap cx) (\field_left -> Eithers.bind (ExtractCore.requireField "right" type_ fieldMap cx) (\field_right -> Right (Core.EitherType {
          Core.eitherTypeLeft = field_left,
          Core.eitherTypeRight = field_right}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

field :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.Field
field cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "term" term fieldMap cx) (\field_term -> Right (Core.Field {
          Core.fieldName = field_name,
          Core.fieldTerm = field_term}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

fieldType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.FieldType
fieldType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "type" type_ fieldMap cx) (\field_type -> Right (Core.FieldType {
          Core.fieldTypeName = field_name,
          Core.fieldTypeType = field_type}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

floatType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.FloatType
floatType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "bigfloat", (\input -> Eithers.map (\t -> Core.FloatTypeBigfloat) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "float32", (\input -> Eithers.map (\t -> Core.FloatTypeFloat32) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "float64", (\input -> Eithers.map (\t -> Core.FloatTypeFloat64) (ExtractCore.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

floatValue :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.FloatValue
floatValue cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "bigfloat", (\input -> Eithers.map (\t -> Core.FloatValueBigfloat t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralFloat v2 -> case v2 of
                            Core.FloatValueBigfloat v3 -> Right v3
                            _ -> Left (Errors.DecodingError "expected bigfloat value")
                          _ -> Left (Errors.DecodingError "expected bigfloat literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "float32", (\input -> Eithers.map (\t -> Core.FloatValueFloat32 t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralFloat v2 -> case v2 of
                            Core.FloatValueFloat32 v3 -> Right v3
                            _ -> Left (Errors.DecodingError "expected float32 value")
                          _ -> Left (Errors.DecodingError "expected float32 literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "float64", (\input -> Eithers.map (\t -> Core.FloatValueFloat64 t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralFloat v2 -> case v2 of
                            Core.FloatValueFloat64 v3 -> Right v3
                            _ -> Left (Errors.DecodingError "expected float64 value")
                          _ -> Left (Errors.DecodingError "expected float64 literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input))))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

forallType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.ForallType
forallType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "parameter" name fieldMap cx) (\field_parameter -> Eithers.bind (ExtractCore.requireField "body" type_ fieldMap cx) (\field_body -> Right (Core.ForallType {
          Core.forallTypeParameter = field_parameter,
          Core.forallTypeBody = field_body}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

functionType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.FunctionType
functionType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "domain" type_ fieldMap cx) (\field_domain -> Eithers.bind (ExtractCore.requireField "codomain" type_ fieldMap cx) (\field_codomain -> Right (Core.FunctionType {
          Core.functionTypeDomain = field_domain,
          Core.functionTypeCodomain = field_codomain}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

injection :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.Injection
injection cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "typeName" name fieldMap cx) (\field_typeName -> Eithers.bind (ExtractCore.requireField "field" field fieldMap cx) (\field_field -> Right (Core.Injection {
          Core.injectionTypeName = field_typeName,
          Core.injectionField = field_field}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

integerType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.IntegerType
integerType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "bigint", (\input -> Eithers.map (\t -> Core.IntegerTypeBigint) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "int8", (\input -> Eithers.map (\t -> Core.IntegerTypeInt8) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "int16", (\input -> Eithers.map (\t -> Core.IntegerTypeInt16) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "int32", (\input -> Eithers.map (\t -> Core.IntegerTypeInt32) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "int64", (\input -> Eithers.map (\t -> Core.IntegerTypeInt64) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "uint8", (\input -> Eithers.map (\t -> Core.IntegerTypeUint8) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "uint16", (\input -> Eithers.map (\t -> Core.IntegerTypeUint16) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "uint32", (\input -> Eithers.map (\t -> Core.IntegerTypeUint32) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "uint64", (\input -> Eithers.map (\t -> Core.IntegerTypeUint64) (ExtractCore.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

integerValue :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.IntegerValue
integerValue cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "bigint", (\input -> Eithers.map (\t -> Core.IntegerValueBigint t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralInteger v2 -> case v2 of
                            Core.IntegerValueBigint v3 -> Right v3
                            _ -> Left (Errors.DecodingError "expected bigint value")
                          _ -> Left (Errors.DecodingError "expected bigint literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "int8", (\input -> Eithers.map (\t -> Core.IntegerValueInt8 t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralInteger v2 -> case v2 of
                            Core.IntegerValueInt8 v3 -> Right v3
                            _ -> Left (Errors.DecodingError "expected int8 value")
                          _ -> Left (Errors.DecodingError "expected int8 literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "int16", (\input -> Eithers.map (\t -> Core.IntegerValueInt16 t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralInteger v2 -> case v2 of
                            Core.IntegerValueInt16 v3 -> Right v3
                            _ -> Left (Errors.DecodingError "expected int16 value")
                          _ -> Left (Errors.DecodingError "expected int16 literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "int32", (\input -> Eithers.map (\t -> Core.IntegerValueInt32 t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralInteger v2 -> case v2 of
                            Core.IntegerValueInt32 v3 -> Right v3
                            _ -> Left (Errors.DecodingError "expected int32 value")
                          _ -> Left (Errors.DecodingError "expected int32 literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "int64", (\input -> Eithers.map (\t -> Core.IntegerValueInt64 t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralInteger v2 -> case v2 of
                            Core.IntegerValueInt64 v3 -> Right v3
                            _ -> Left (Errors.DecodingError "expected int64 value")
                          _ -> Left (Errors.DecodingError "expected int64 literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "uint8", (\input -> Eithers.map (\t -> Core.IntegerValueUint8 t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralInteger v2 -> case v2 of
                            Core.IntegerValueUint8 v3 -> Right v3
                            _ -> Left (Errors.DecodingError "expected uint8 value")
                          _ -> Left (Errors.DecodingError "expected uint8 literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "uint16", (\input -> Eithers.map (\t -> Core.IntegerValueUint16 t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralInteger v2 -> case v2 of
                            Core.IntegerValueUint16 v3 -> Right v3
                            _ -> Left (Errors.DecodingError "expected uint16 value")
                          _ -> Left (Errors.DecodingError "expected uint16 literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "uint32", (\input -> Eithers.map (\t -> Core.IntegerValueUint32 t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralInteger v2 -> case v2 of
                            Core.IntegerValueUint32 v3 -> Right v3
                            _ -> Left (Errors.DecodingError "expected uint32 value")
                          _ -> Left (Errors.DecodingError "expected uint32 literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "uint64", (\input -> Eithers.map (\t -> Core.IntegerValueUint64 t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralInteger v2 -> case v2 of
                            Core.IntegerValueUint64 v3 -> Right v3
                            _ -> Left (Errors.DecodingError "expected uint64 value")
                          _ -> Left (Errors.DecodingError "expected uint64 literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input))))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

lambda :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.Lambda
lambda cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "parameter" name fieldMap cx) (\field_parameter -> Eithers.bind (ExtractCore.requireField "domain" (ExtractCore.decodeMaybe type_) fieldMap cx) (\field_domain -> Eithers.bind (ExtractCore.requireField "body" term fieldMap cx) (\field_body -> Right (Core.Lambda {
          Core.lambdaParameter = field_parameter,
          Core.lambdaDomain = field_domain,
          Core.lambdaBody = field_body})))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

let_ :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.Let
let_ cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "bindings" (ExtractCore.decodeList binding) fieldMap cx) (\field_bindings -> Eithers.bind (ExtractCore.requireField "body" term fieldMap cx) (\field_body -> Right (Core.Let {
          Core.letBindings = field_bindings,
          Core.letBody = field_body}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

literal :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.Literal
literal cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "binary", (\input -> Eithers.map (\t -> Core.LiteralBinary t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralBinary v2 -> Right v2
                          _ -> Left (Errors.DecodingError "expected binary literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "boolean", (\input -> Eithers.map (\t -> Core.LiteralBoolean t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralBoolean v2 -> Right v2
                          _ -> Left (Errors.DecodingError "expected boolean literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (Core.Name "float", (\input -> Eithers.map (\t -> Core.LiteralFloat t) (floatValue cx input))),
                      (Core.Name "integer", (\input -> Eithers.map (\t -> Core.LiteralInteger t) (integerValue cx input))),
                      (Core.Name "string", (\input -> Eithers.map (\t -> Core.LiteralString t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                        Core.TermLiteral v1 -> case v1 of
                          Core.LiteralString v2 -> Right v2
                          _ -> Left (Errors.DecodingError "expected string literal")
                        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input))))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

literalType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.LiteralType
literalType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "binary", (\input -> Eithers.map (\t -> Core.LiteralTypeBinary) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "boolean", (\input -> Eithers.map (\t -> Core.LiteralTypeBoolean) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "float", (\input -> Eithers.map (\t -> Core.LiteralTypeFloat t) (floatType cx input))),
                      (Core.Name "integer", (\input -> Eithers.map (\t -> Core.LiteralTypeInteger t) (integerType cx input))),
                      (Core.Name "string", (\input -> Eithers.map (\t -> Core.LiteralTypeString) (ExtractCore.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

mapType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.MapType
mapType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "keys" type_ fieldMap cx) (\field_keys -> Eithers.bind (ExtractCore.requireField "values" type_ fieldMap cx) (\field_values -> Right (Core.MapType {
          Core.mapTypeKeys = field_keys,
          Core.mapTypeValues = field_values}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

name :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.Name
name cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Core.Name b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)

pairType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.PairType
pairType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "first" type_ fieldMap cx) (\field_first -> Eithers.bind (ExtractCore.requireField "second" type_ fieldMap cx) (\field_second -> Right (Core.PairType {
          Core.pairTypeFirst = field_first,
          Core.pairTypeSecond = field_second}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

projection :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.Projection
projection cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "typeName" name fieldMap cx) (\field_typeName -> Eithers.bind (ExtractCore.requireField "field" name fieldMap cx) (\field_field -> Right (Core.Projection {
          Core.projectionTypeName = field_typeName,
          Core.projectionField = field_field}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

record :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.Record
record cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "typeName" name fieldMap cx) (\field_typeName -> Eithers.bind (ExtractCore.requireField "fields" (ExtractCore.decodeList field) fieldMap cx) (\field_fields -> Right (Core.Record {
          Core.recordTypeName = field_typeName,
          Core.recordFields = field_fields}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

term :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.Term
term cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "annotated", (\input -> Eithers.map (\t -> Core.TermAnnotated t) (annotatedTerm cx input))),
                      (Core.Name "application", (\input -> Eithers.map (\t -> Core.TermApplication t) (application cx input))),
                      (Core.Name "cases", (\input -> Eithers.map (\t -> Core.TermCases t) (caseStatement cx input))),
                      (Core.Name "either", (\input -> Eithers.map (\t -> Core.TermEither t) (ExtractCore.decodeEither term term cx input))),
                      (Core.Name "lambda", (\input -> Eithers.map (\t -> Core.TermLambda t) (lambda cx input))),
                      (Core.Name "let", (\input -> Eithers.map (\t -> Core.TermLet t) (let_ cx input))),
                      (Core.Name "list", (\input -> Eithers.map (\t -> Core.TermList t) (ExtractCore.decodeList term cx input))),
                      (Core.Name "literal", (\input -> Eithers.map (\t -> Core.TermLiteral t) (literal cx input))),
                      (Core.Name "map", (\input -> Eithers.map (\t -> Core.TermMap t) (ExtractCore.decodeMap term term cx input))),
                      (Core.Name "maybe", (\input -> Eithers.map (\t -> Core.TermMaybe t) (ExtractCore.decodeMaybe term cx input))),
                      (Core.Name "pair", (\input -> Eithers.map (\t -> Core.TermPair t) (ExtractCore.decodePair term term cx input))),
                      (Core.Name "project", (\input -> Eithers.map (\t -> Core.TermProject t) (projection cx input))),
                      (Core.Name "record", (\input -> Eithers.map (\t -> Core.TermRecord t) (record cx input))),
                      (Core.Name "set", (\input -> Eithers.map (\t -> Core.TermSet t) (ExtractCore.decodeSet term cx input))),
                      (Core.Name "typeApplication", (\input -> Eithers.map (\t -> Core.TermTypeApplication t) (typeApplicationTerm cx input))),
                      (Core.Name "typeLambda", (\input -> Eithers.map (\t -> Core.TermTypeLambda t) (typeLambda cx input))),
                      (Core.Name "union", (\input -> Eithers.map (\t -> Core.TermUnion t) (injection cx input))),
                      (Core.Name "unit", (\input -> Eithers.map (\t -> Core.TermUnit) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "unwrap", (\input -> Eithers.map (\t -> Core.TermUnwrap t) (name cx input))),
                      (Core.Name "variable", (\input -> Eithers.map (\t -> Core.TermVariable t) (name cx input))),
                      (Core.Name "wrap", (\input -> Eithers.map (\t -> Core.TermWrap t) (wrappedTerm cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

type_ :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.Type
type_ cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "annotated", (\input -> Eithers.map (\t -> Core.TypeAnnotated t) (annotatedType cx input))),
                      (Core.Name "application", (\input -> Eithers.map (\t -> Core.TypeApplication t) (applicationType cx input))),
                      (Core.Name "either", (\input -> Eithers.map (\t -> Core.TypeEither t) (eitherType cx input))),
                      (Core.Name "forall", (\input -> Eithers.map (\t -> Core.TypeForall t) (forallType cx input))),
                      (Core.Name "function", (\input -> Eithers.map (\t -> Core.TypeFunction t) (functionType cx input))),
                      (Core.Name "list", (\input -> Eithers.map (\t -> Core.TypeList t) (type_ cx input))),
                      (Core.Name "literal", (\input -> Eithers.map (\t -> Core.TypeLiteral t) (literalType cx input))),
                      (Core.Name "map", (\input -> Eithers.map (\t -> Core.TypeMap t) (mapType cx input))),
                      (Core.Name "maybe", (\input -> Eithers.map (\t -> Core.TypeMaybe t) (type_ cx input))),
                      (Core.Name "pair", (\input -> Eithers.map (\t -> Core.TypePair t) (pairType cx input))),
                      (Core.Name "record", (\input -> Eithers.map (\t -> Core.TypeRecord t) (ExtractCore.decodeList fieldType cx input))),
                      (Core.Name "set", (\input -> Eithers.map (\t -> Core.TypeSet t) (type_ cx input))),
                      (Core.Name "union", (\input -> Eithers.map (\t -> Core.TypeUnion t) (ExtractCore.decodeList fieldType cx input))),
                      (Core.Name "unit", (\input -> Eithers.map (\t -> Core.TypeUnit) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "variable", (\input -> Eithers.map (\t -> Core.TypeVariable t) (name cx input))),
                      (Core.Name "void", (\input -> Eithers.map (\t -> Core.TypeVoid) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "wrap", (\input -> Eithers.map (\t -> Core.TypeWrap t) (type_ cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)

typeApplicationTerm :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.TypeApplicationTerm
typeApplicationTerm cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "body" term fieldMap cx) (\field_body -> Eithers.bind (ExtractCore.requireField "type" type_ fieldMap cx) (\field_type -> Right (Core.TypeApplicationTerm {
          Core.typeApplicationTermBody = field_body,
          Core.typeApplicationTermType = field_type}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

typeLambda :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.TypeLambda
typeLambda cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "parameter" name fieldMap cx) (\field_parameter -> Eithers.bind (ExtractCore.requireField "body" term fieldMap cx) (\field_body -> Right (Core.TypeLambda {
          Core.typeLambdaParameter = field_parameter,
          Core.typeLambdaBody = field_body}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

typeScheme :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.TypeScheme
typeScheme cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "variables" (ExtractCore.decodeList name) fieldMap cx) (\field_variables -> Eithers.bind (ExtractCore.requireField "type" type_ fieldMap cx) (\field_type -> Eithers.bind (ExtractCore.requireField "constraints" (ExtractCore.decodeMaybe (ExtractCore.decodeMap name typeVariableMetadata)) fieldMap cx) (\field_constraints -> Right (Core.TypeScheme {
          Core.typeSchemeVariables = field_variables,
          Core.typeSchemeType = field_type,
          Core.typeSchemeConstraints = field_constraints})))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

typeVariableMetadata :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.TypeVariableMetadata
typeVariableMetadata cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "classes" (ExtractCore.decodeSet name) fieldMap cx) (\field_classes -> Right (Core.TypeVariableMetadata {
          Core.typeVariableMetadataClasses = field_classes})))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)

wrappedTerm :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Core.WrappedTerm
wrappedTerm cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "typeName" name fieldMap cx) (\field_typeName -> Eithers.bind (ExtractCore.requireField "body" term fieldMap cx) (\field_body -> Right (Core.WrappedTerm {
          Core.wrappedTermTypeName = field_typeName,
          Core.wrappedTermBody = field_body}))))
      _ -> Left (Errors.DecodingError "expected record")) (ExtractCore.stripWithDecodingError cx raw)
