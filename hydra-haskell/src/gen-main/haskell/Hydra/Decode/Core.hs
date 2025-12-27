-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.core

module Hydra.Decode.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

annotatedTerm :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.AnnotatedTerm)
annotatedTerm cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_body -> Eithers.either (\err -> Left err) (\field_annotation -> Right (Core.AnnotatedTerm {
      Core.annotatedTermBody = field_body,
      Core.annotatedTermAnnotation = field_annotation})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "annotation",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMap v2 ->  
        let pairs = (Maps.toList v2) 
            decodePair = (\kv ->  
                    let rawKey = (Pairs.first kv) 
                        rawVal = (Pairs.second kv)
                    in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (term cx rawVal)) (name cx rawKey)))
        in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
      _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "annotation") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "body",
      " in record"]))) (\fieldTerm -> term cx fieldTerm) (Maps.lookup (Core.Name "body") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.AnnotatedTerm"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

annotatedType :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.AnnotatedType)
annotatedType cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_body -> Eithers.either (\err -> Left err) (\field_annotation -> Right (Core.AnnotatedType {
      Core.annotatedTypeBody = field_body,
      Core.annotatedTypeAnnotation = field_annotation})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "annotation",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMap v2 ->  
        let pairs = (Maps.toList v2) 
            decodePair = (\kv ->  
                    let rawKey = (Pairs.first kv) 
                        rawVal = (Pairs.second kv)
                    in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (term cx rawVal)) (name cx rawKey)))
        in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
      _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "annotation") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "body",
      " in record"]))) (\fieldTerm -> type_ cx fieldTerm) (Maps.lookup (Core.Name "body") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.AnnotatedType"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

application :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.Application)
application cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_function -> Eithers.either (\err -> Left err) (\field_argument -> Right (Core.Application {
      Core.applicationFunction = field_function,
      Core.applicationArgument = field_argument})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "argument",
      " in record"]))) (\fieldTerm -> term cx fieldTerm) (Maps.lookup (Core.Name "argument") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "function",
      " in record"]))) (\fieldTerm -> term cx fieldTerm) (Maps.lookup (Core.Name "function") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.Application"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

applicationType :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.ApplicationType)
applicationType cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_function -> Eithers.either (\err -> Left err) (\field_argument -> Right (Core.ApplicationType {
      Core.applicationTypeFunction = field_function,
      Core.applicationTypeArgument = field_argument})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "argument",
      " in record"]))) (\fieldTerm -> type_ cx fieldTerm) (Maps.lookup (Core.Name "argument") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "function",
      " in record"]))) (\fieldTerm -> type_ cx fieldTerm) (Maps.lookup (Core.Name "function") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.ApplicationType"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

binding :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.Binding)
binding cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_name -> Eithers.either (\err -> Left err) (\field_term -> Eithers.either (\err -> Left err) (\field_type -> Right (Core.Binding {
      Core.bindingName = field_name,
      Core.bindingTerm = field_term,
      Core.bindingType = field_type})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "type",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMaybe v2 -> (Eithers.mapMaybe (typeScheme cx) v2)
      _ -> (Left (Util.DecodingError "expected optional value"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "type") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "term",
      " in record"]))) (\fieldTerm -> term cx fieldTerm) (Maps.lookup (Core.Name "term") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "name",
      " in record"]))) (\fieldTerm -> name cx fieldTerm) (Maps.lookup (Core.Name "name") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.Binding"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

caseStatement :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.CaseStatement)
caseStatement cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_typeName -> Eithers.either (\err -> Left err) (\field_default -> Eithers.either (\err -> Left err) (\field_cases -> Right (Core.CaseStatement {
      Core.caseStatementTypeName = field_typeName,
      Core.caseStatementDefault = field_default,
      Core.caseStatementCases = field_cases})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "cases",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (field cx) v2)
      _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "cases") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "default",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMaybe v2 -> (Eithers.mapMaybe (term cx) v2)
      _ -> (Left (Util.DecodingError "expected optional value"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "default") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "typeName",
      " in record"]))) (\fieldTerm -> name cx fieldTerm) (Maps.lookup (Core.Name "typeName") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.CaseStatement"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

eitherType :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.EitherType)
eitherType cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_left -> Eithers.either (\err -> Left err) (\field_right -> Right (Core.EitherType {
      Core.eitherTypeLeft = field_left,
      Core.eitherTypeRight = field_right})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "right",
      " in record"]))) (\fieldTerm -> type_ cx fieldTerm) (Maps.lookup (Core.Name "right") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "left",
      " in record"]))) (\fieldTerm -> type_ cx fieldTerm) (Maps.lookup (Core.Name "left") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.EitherType"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

pairType :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.PairType)
pairType cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_first -> Eithers.either (\err -> Left err) (\field_second -> Right (Core.PairType {
      Core.pairTypeFirst = field_first,
      Core.pairTypeSecond = field_second})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "second",
      " in record"]))) (\fieldTerm -> type_ cx fieldTerm) (Maps.lookup (Core.Name "second") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "first",
      " in record"]))) (\fieldTerm -> type_ cx fieldTerm) (Maps.lookup (Core.Name "first") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.PairType"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

elimination :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.Elimination)
elimination cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "record", (\input -> Eithers.map (\t -> Core.EliminationRecord t) (projection cx input))),
                (Core.Name "union", (\input -> Eithers.map (\t -> Core.EliminationUnion t) (caseStatement cx input))),
                (Core.Name "wrap", (\input -> Eithers.map (\t -> Core.EliminationWrap t) (name cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.core.Elimination"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

field :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.Field)
field cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_name -> Eithers.either (\err -> Left err) (\field_term -> Right (Core.Field {
      Core.fieldName = field_name,
      Core.fieldTerm = field_term})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "term",
      " in record"]))) (\fieldTerm -> term cx fieldTerm) (Maps.lookup (Core.Name "term") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "name",
      " in record"]))) (\fieldTerm -> name cx fieldTerm) (Maps.lookup (Core.Name "name") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.Field"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

fieldType :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.FieldType)
fieldType cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_name -> Eithers.either (\err -> Left err) (\field_type -> Right (Core.FieldType {
      Core.fieldTypeName = field_name,
      Core.fieldTypeType = field_type})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "type",
      " in record"]))) (\fieldTerm -> type_ cx fieldTerm) (Maps.lookup (Core.Name "type") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "name",
      " in record"]))) (\fieldTerm -> name cx fieldTerm) (Maps.lookup (Core.Name "name") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.FieldType"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

floatType :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.FloatType)
floatType cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "bigfloat", (\input -> Eithers.map (\t -> Core.FloatTypeBigfloat) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "float32", (\input -> Eithers.map (\t -> Core.FloatTypeFloat32) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "float64", (\input -> Eithers.map (\t -> Core.FloatTypeFloat64) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.core.FloatType"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

floatValue :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.FloatValue)
floatValue cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "bigfloat", (\input -> Eithers.map (\t -> Core.FloatValueBigfloat t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralFloat v3 -> ((\x -> case x of
                      Core.FloatValueBigfloat v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected bigfloat value"))) v3)
                    _ -> (Left (Util.DecodingError "expected bigfloat literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "float32", (\input -> Eithers.map (\t -> Core.FloatValueFloat32 t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralFloat v3 -> ((\x -> case x of
                      Core.FloatValueFloat32 v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected float32 value"))) v3)
                    _ -> (Left (Util.DecodingError "expected float32 literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "float64", (\input -> Eithers.map (\t -> Core.FloatValueFloat64 t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralFloat v3 -> ((\x -> case x of
                      Core.FloatValueFloat64 v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected float64 value"))) v3)
                    _ -> (Left (Util.DecodingError "expected float64 literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.core.FloatValue"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

forallType :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.ForallType)
forallType cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_parameter -> Eithers.either (\err -> Left err) (\field_body -> Right (Core.ForallType {
      Core.forallTypeParameter = field_parameter,
      Core.forallTypeBody = field_body})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "body",
      " in record"]))) (\fieldTerm -> type_ cx fieldTerm) (Maps.lookup (Core.Name "body") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "parameter",
      " in record"]))) (\fieldTerm -> name cx fieldTerm) (Maps.lookup (Core.Name "parameter") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.ForallType"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

function :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.Function)
function cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "elimination", (\input -> Eithers.map (\t -> Core.FunctionElimination t) (elimination cx input))),
                (Core.Name "lambda", (\input -> Eithers.map (\t -> Core.FunctionLambda t) (lambda cx input))),
                (Core.Name "primitive", (\input -> Eithers.map (\t -> Core.FunctionPrimitive t) (name cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.core.Function"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

functionType :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.FunctionType)
functionType cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_domain -> Eithers.either (\err -> Left err) (\field_codomain -> Right (Core.FunctionType {
      Core.functionTypeDomain = field_domain,
      Core.functionTypeCodomain = field_codomain})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "codomain",
      " in record"]))) (\fieldTerm -> type_ cx fieldTerm) (Maps.lookup (Core.Name "codomain") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "domain",
      " in record"]))) (\fieldTerm -> type_ cx fieldTerm) (Maps.lookup (Core.Name "domain") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.FunctionType"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

injection :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.Injection)
injection cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_typeName -> Eithers.either (\err -> Left err) (\field_field -> Right (Core.Injection {
      Core.injectionTypeName = field_typeName,
      Core.injectionField = field_field})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "field",
      " in record"]))) (\fieldTerm -> field cx fieldTerm) (Maps.lookup (Core.Name "field") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "typeName",
      " in record"]))) (\fieldTerm -> name cx fieldTerm) (Maps.lookup (Core.Name "typeName") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.Injection"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

integerType :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.IntegerType)
integerType cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "bigint", (\input -> Eithers.map (\t -> Core.IntegerTypeBigint) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "int8", (\input -> Eithers.map (\t -> Core.IntegerTypeInt8) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "int16", (\input -> Eithers.map (\t -> Core.IntegerTypeInt16) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "int32", (\input -> Eithers.map (\t -> Core.IntegerTypeInt32) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "int64", (\input -> Eithers.map (\t -> Core.IntegerTypeInt64) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "uint8", (\input -> Eithers.map (\t -> Core.IntegerTypeUint8) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "uint16", (\input -> Eithers.map (\t -> Core.IntegerTypeUint16) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "uint32", (\input -> Eithers.map (\t -> Core.IntegerTypeUint32) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "uint64", (\input -> Eithers.map (\t -> Core.IntegerTypeUint64) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.core.IntegerType"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

integerValue :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.IntegerValue)
integerValue cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "bigint", (\input -> Eithers.map (\t -> Core.IntegerValueBigint t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralInteger v3 -> ((\x -> case x of
                      Core.IntegerValueBigint v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected bigint value"))) v3)
                    _ -> (Left (Util.DecodingError "expected bigint literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "int8", (\input -> Eithers.map (\t -> Core.IntegerValueInt8 t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralInteger v3 -> ((\x -> case x of
                      Core.IntegerValueInt8 v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected int8 value"))) v3)
                    _ -> (Left (Util.DecodingError "expected int8 literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "int16", (\input -> Eithers.map (\t -> Core.IntegerValueInt16 t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralInteger v3 -> ((\x -> case x of
                      Core.IntegerValueInt16 v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected int16 value"))) v3)
                    _ -> (Left (Util.DecodingError "expected int16 literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "int32", (\input -> Eithers.map (\t -> Core.IntegerValueInt32 t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralInteger v3 -> ((\x -> case x of
                      Core.IntegerValueInt32 v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected int32 value"))) v3)
                    _ -> (Left (Util.DecodingError "expected int32 literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "int64", (\input -> Eithers.map (\t -> Core.IntegerValueInt64 t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralInteger v3 -> ((\x -> case x of
                      Core.IntegerValueInt64 v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected int64 value"))) v3)
                    _ -> (Left (Util.DecodingError "expected int64 literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "uint8", (\input -> Eithers.map (\t -> Core.IntegerValueUint8 t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralInteger v3 -> ((\x -> case x of
                      Core.IntegerValueUint8 v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected uint8 value"))) v3)
                    _ -> (Left (Util.DecodingError "expected uint8 literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "uint16", (\input -> Eithers.map (\t -> Core.IntegerValueUint16 t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralInteger v3 -> ((\x -> case x of
                      Core.IntegerValueUint16 v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected uint16 value"))) v3)
                    _ -> (Left (Util.DecodingError "expected uint16 literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "uint32", (\input -> Eithers.map (\t -> Core.IntegerValueUint32 t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralInteger v3 -> ((\x -> case x of
                      Core.IntegerValueUint32 v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected uint32 value"))) v3)
                    _ -> (Left (Util.DecodingError "expected uint32 literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "uint64", (\input -> Eithers.map (\t -> Core.IntegerValueUint64 t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralInteger v3 -> ((\x -> case x of
                      Core.IntegerValueUint64 v4 -> (Right v4)
                      _ -> (Left (Util.DecodingError "expected uint64 value"))) v3)
                    _ -> (Left (Util.DecodingError "expected uint64 literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.core.IntegerValue"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

lambda :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.Lambda)
lambda cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_parameter -> Eithers.either (\err -> Left err) (\field_domain -> Eithers.either (\err -> Left err) (\field_body -> Right (Core.Lambda {
      Core.lambdaParameter = field_parameter,
      Core.lambdaDomain = field_domain,
      Core.lambdaBody = field_body})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "body",
      " in record"]))) (\fieldTerm -> term cx fieldTerm) (Maps.lookup (Core.Name "body") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "domain",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMaybe v2 -> (Eithers.mapMaybe (type_ cx) v2)
      _ -> (Left (Util.DecodingError "expected optional value"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "domain") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "parameter",
      " in record"]))) (\fieldTerm -> name cx fieldTerm) (Maps.lookup (Core.Name "parameter") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.Lambda"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

let_ :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.Let)
let_ cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_bindings -> Eithers.either (\err -> Left err) (\field_body -> Right (Core.Let {
      Core.letBindings = field_bindings,
      Core.letBody = field_body})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "body",
      " in record"]))) (\fieldTerm -> term cx fieldTerm) (Maps.lookup (Core.Name "body") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "bindings",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (binding cx) v2)
      _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "bindings") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.Let"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

literal :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.Literal)
literal cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "binary", (\input -> Eithers.map (\t -> Core.LiteralBinary t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralBinary v3 -> (Right v3)
                    _ -> (Left (Util.DecodingError "expected binary literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "boolean", (\input -> Eithers.map (\t -> Core.LiteralBoolean t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralBoolean v3 -> (Right v3)
                    _ -> (Left (Util.DecodingError "expected boolean literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "float", (\input -> Eithers.map (\t -> Core.LiteralFloat t) (floatValue cx input))),
                (Core.Name "integer", (\input -> Eithers.map (\t -> Core.LiteralInteger t) (integerValue cx input))),
                (Core.Name "string", (\input -> Eithers.map (\t -> Core.LiteralString t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermLiteral v2 -> ((\x -> case x of
                    Core.LiteralString v3 -> (Right v3)
                    _ -> (Left (Util.DecodingError "expected string literal"))) v2)
                  _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.core.Literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

literalType :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.LiteralType)
literalType cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "binary", (\input -> Eithers.map (\t -> Core.LiteralTypeBinary) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "boolean", (\input -> Eithers.map (\t -> Core.LiteralTypeBoolean) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "float", (\input -> Eithers.map (\t -> Core.LiteralTypeFloat t) (floatType cx input))),
                (Core.Name "integer", (\input -> Eithers.map (\t -> Core.LiteralTypeInteger t) (integerType cx input))),
                (Core.Name "string", (\input -> Eithers.map (\t -> Core.LiteralTypeString) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.core.LiteralType"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

mapType :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.MapType)
mapType cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_keys -> Eithers.either (\err -> Left err) (\field_values -> Right (Core.MapType {
      Core.mapTypeKeys = field_keys,
      Core.mapTypeValues = field_values})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "values",
      " in record"]))) (\fieldTerm -> type_ cx fieldTerm) (Maps.lookup (Core.Name "values") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "keys",
      " in record"]))) (\fieldTerm -> type_ cx fieldTerm) (Maps.lookup (Core.Name "keys") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.MapType"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

name :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.Name)
name cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Core.Name b) ((\raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
    Core.TermLiteral v2 -> ((\x -> case x of
      Core.LiteralString v3 -> (Right v3)
      _ -> (Left (Util.DecodingError "expected string literal"))) v2)
    _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v1)))
  _ -> (Left (Util.DecodingError "expected wrapped type hydra.core.Name"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

projection :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.Projection)
projection cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_typeName -> Eithers.either (\err -> Left err) (\field_field -> Right (Core.Projection {
      Core.projectionTypeName = field_typeName,
      Core.projectionField = field_field})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "field",
      " in record"]))) (\fieldTerm -> name cx fieldTerm) (Maps.lookup (Core.Name "field") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "typeName",
      " in record"]))) (\fieldTerm -> name cx fieldTerm) (Maps.lookup (Core.Name "typeName") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.Projection"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

record :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.Record)
record cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_typeName -> Eithers.either (\err -> Left err) (\field_fields -> Right (Core.Record {
      Core.recordTypeName = field_typeName,
      Core.recordFields = field_fields})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "fields",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (field cx) v2)
      _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "fields") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "typeName",
      " in record"]))) (\fieldTerm -> name cx fieldTerm) (Maps.lookup (Core.Name "typeName") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.Record"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

rowType :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.RowType)
rowType cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_typeName -> Eithers.either (\err -> Left err) (\field_fields -> Right (Core.RowType {
      Core.rowTypeTypeName = field_typeName,
      Core.rowTypeFields = field_fields})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "fields",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (fieldType cx) v2)
      _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "fields") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "typeName",
      " in record"]))) (\fieldTerm -> name cx fieldTerm) (Maps.lookup (Core.Name "typeName") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.RowType"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

term :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.Term)
term cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "annotated", (\input -> Eithers.map (\t -> Core.TermAnnotated t) (annotatedTerm cx input))),
                (Core.Name "application", (\input -> Eithers.map (\t -> Core.TermApplication t) (application cx input))),
                (Core.Name "either", (\input -> Eithers.map (\t -> Core.TermEither t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermEither v2 -> (Eithers.either (Eithers.map (\x -> Left x)) (Eithers.map (\x -> Right x)) (Eithers.bimap (term cx) (term cx) v2))
                  _ -> (Left (Util.DecodingError "expected either value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "function", (\input -> Eithers.map (\t -> Core.TermFunction t) (function cx input))),
                (Core.Name "let", (\input -> Eithers.map (\t -> Core.TermLet t) (let_ cx input))),
                (Core.Name "list", (\input -> Eithers.map (\t -> Core.TermList t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermList v2 -> (Eithers.mapList (term cx) v2)
                  _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "literal", (\input -> Eithers.map (\t -> Core.TermLiteral t) (literal cx input))),
                (Core.Name "map", (\input -> Eithers.map (\t -> Core.TermMap t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermMap v2 ->  
                    let pairs = (Maps.toList v2) 
                        decodePair = (\kv ->  
                                let rawKey = (Pairs.first kv) 
                                    rawVal = (Pairs.second kv)
                                in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (term cx rawVal)) (term cx rawKey)))
                    in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
                  _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "maybe", (\input -> Eithers.map (\t -> Core.TermMaybe t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermMaybe v2 -> (Eithers.mapMaybe (term cx) v2)
                  _ -> (Left (Util.DecodingError "expected optional value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "pair", (\input -> Eithers.map (\t -> Core.TermPair t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermPair v2 ->  
                    let rawFirst = (Pairs.first v2) 
                        rawSecond = (Pairs.second v2)
                    in (Eithers.either (\err -> Left err) (\decodedFirst -> Eithers.either (\err2 -> Left err2) (\decodedSecond -> Right (decodedFirst, decodedSecond)) (term cx rawSecond)) (term cx rawFirst))
                  _ -> (Left (Util.DecodingError "expected pair"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "record", (\input -> Eithers.map (\t -> Core.TermRecord t) (record cx input))),
                (Core.Name "set", (\input -> Eithers.map (\t -> Core.TermSet t) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermSet v2 ->  
                    let elements = (Sets.toList v2)
                    in (Eithers.either (\err -> Left err) (\decodedElems -> Right (Sets.fromList decodedElems)) (Eithers.mapList (term cx) elements))
                  _ -> (Left (Util.DecodingError "expected set"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "typeApplication", (\input -> Eithers.map (\t -> Core.TermTypeApplication t) (typeApplicationTerm cx input))),
                (Core.Name "typeLambda", (\input -> Eithers.map (\t -> Core.TermTypeLambda t) (typeLambda cx input))),
                (Core.Name "union", (\input -> Eithers.map (\t -> Core.TermUnion t) (injection cx input))),
                (Core.Name "unit", (\input -> Eithers.map (\t -> Core.TermUnit) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "variable", (\input -> Eithers.map (\t -> Core.TermVariable t) (name cx input))),
                (Core.Name "wrap", (\input -> Eithers.map (\t -> Core.TermWrap t) (wrappedTerm cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.core.Term"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

type_ :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.Type)
type_ cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
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
                (Core.Name "record", (\input -> Eithers.map (\t -> Core.TypeRecord t) (rowType cx input))),
                (Core.Name "set", (\input -> Eithers.map (\t -> Core.TypeSet t) (type_ cx input))),
                (Core.Name "union", (\input -> Eithers.map (\t -> Core.TypeUnion t) (rowType cx input))),
                (Core.Name "unit", (\input -> Eithers.map (\t -> Core.TypeUnit) (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "variable", (\input -> Eithers.map (\t -> Core.TypeVariable t) (name cx input))),
                (Core.Name "wrap", (\input -> Eithers.map (\t -> Core.TypeWrap t) (wrappedType cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.core.Type"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeApplicationTerm :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.TypeApplicationTerm)
typeApplicationTerm cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_body -> Eithers.either (\err -> Left err) (\field_type -> Right (Core.TypeApplicationTerm {
      Core.typeApplicationTermBody = field_body,
      Core.typeApplicationTermType = field_type})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "type",
      " in record"]))) (\fieldTerm -> type_ cx fieldTerm) (Maps.lookup (Core.Name "type") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "body",
      " in record"]))) (\fieldTerm -> term cx fieldTerm) (Maps.lookup (Core.Name "body") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.TypeApplicationTerm"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeLambda :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.TypeLambda)
typeLambda cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_parameter -> Eithers.either (\err -> Left err) (\field_body -> Right (Core.TypeLambda {
      Core.typeLambdaParameter = field_parameter,
      Core.typeLambdaBody = field_body})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "body",
      " in record"]))) (\fieldTerm -> term cx fieldTerm) (Maps.lookup (Core.Name "body") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "parameter",
      " in record"]))) (\fieldTerm -> name cx fieldTerm) (Maps.lookup (Core.Name "parameter") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.TypeLambda"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeScheme :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.TypeScheme)
typeScheme cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_variables -> Eithers.either (\err -> Left err) (\field_type -> Eithers.either (\err -> Left err) (\field_constraints -> Right (Core.TypeScheme {
      Core.typeSchemeVariables = field_variables,
      Core.typeSchemeType = field_type,
      Core.typeSchemeConstraints = field_constraints})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "constraints",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMaybe v2 -> (Eithers.mapMaybe (\raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
        Core.TermMap v3 ->  
          let pairs = (Maps.toList v3) 
              decodePair = (\kv ->  
                      let rawKey = (Pairs.first kv) 
                          rawVal = (Pairs.second kv)
                      in (Eithers.either (\err -> Left err) (\k2 -> Eithers.either (\err2 -> Left err2) (\v2 -> Right (k2, v2)) (typeVariableMetadata cx rawVal)) (name cx rawKey)))
          in (Eithers.either (\err -> Left err) (\decodedPairs -> Right (Maps.fromList decodedPairs)) (Eithers.mapList decodePair pairs))
        _ -> (Left (Util.DecodingError "expected map"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v2)
      _ -> (Left (Util.DecodingError "expected optional value"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "constraints") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "type",
      " in record"]))) (\fieldTerm -> type_ cx fieldTerm) (Maps.lookup (Core.Name "type") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "variables",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (name cx) v2)
      _ -> (Left (Util.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "variables") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.TypeScheme"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeVariableMetadata :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.TypeVariableMetadata)
typeVariableMetadata cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_classes -> Right (Core.TypeVariableMetadata {
      Core.typeVariableMetadataClasses = field_classes})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "classes",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermSet v2 ->  
        let elements = (Sets.toList v2)
        in (Eithers.either (\err -> Left err) (\decodedElems -> Right (Sets.fromList decodedElems)) (Eithers.mapList (name cx) elements))
      _ -> (Left (Util.DecodingError "expected set"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "classes") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.TypeVariableMetadata"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

wrappedTerm :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.WrappedTerm)
wrappedTerm cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_typeName -> Eithers.either (\err -> Left err) (\field_body -> Right (Core.WrappedTerm {
      Core.wrappedTermTypeName = field_typeName,
      Core.wrappedTermBody = field_body})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "body",
      " in record"]))) (\fieldTerm -> term cx fieldTerm) (Maps.lookup (Core.Name "body") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "typeName",
      " in record"]))) (\fieldTerm -> name cx fieldTerm) (Maps.lookup (Core.Name "typeName") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.WrappedTerm"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

wrappedType :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Core.WrappedType)
wrappedType cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\field_typeName -> Eithers.either (\err -> Left err) (\field_body -> Right (Core.WrappedType {
      Core.wrappedTypeTypeName = field_typeName,
      Core.wrappedTypeBody = field_body})) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "body",
      " in record"]))) (\fieldTerm -> type_ cx fieldTerm) (Maps.lookup (Core.Name "body") fieldMap))) (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "missing field ",
      "typeName",
      " in record"]))) (\fieldTerm -> name cx fieldTerm) (Maps.lookup (Core.Name "typeName") fieldMap)))
  _ -> (Left (Util.DecodingError "expected record of type hydra.core.WrappedType"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
