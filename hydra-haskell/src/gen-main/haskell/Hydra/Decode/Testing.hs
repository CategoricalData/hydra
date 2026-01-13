-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.testing

module Hydra.Decode.Testing where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Ast as Ast
import qualified Hydra.Decode.Coders as Coders
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Decode.Json.Model as Model
import qualified Hydra.Decode.Parsing as Parsing
import qualified Hydra.Decode.Util as Util
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model_
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Testing as Testing
import qualified Hydra.Util as Util_
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

alphaConversionTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.AlphaConversionTestCase)
alphaConversionTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "term" Core_.term fieldMap cx) (\field_term -> Eithers.bind (Helpers.requireField "oldVariable" Core_.name fieldMap cx) (\field_oldVariable -> Eithers.bind (Helpers.requireField "newVariable" Core_.name fieldMap cx) (\field_newVariable -> Eithers.bind (Helpers.requireField "result" Core_.term fieldMap cx) (\field_result -> Right (Testing.AlphaConversionTestCase {
      Testing.alphaConversionTestCaseTerm = field_term,
      Testing.alphaConversionTestCaseOldVariable = field_oldVariable,
      Testing.alphaConversionTestCaseNewVariable = field_newVariable,
      Testing.alphaConversionTestCaseResult = field_result}))))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.AlphaConversionTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

evaluationStyle :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.EvaluationStyle)
evaluationStyle cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "eager", (\input -> Eithers.map (\t -> Testing.EvaluationStyleEager) (Helpers.decodeUnit cx input))),
                (Core.Name "lazy", (\input -> Eithers.map (\t -> Testing.EvaluationStyleLazy) (Helpers.decodeUnit cx input)))])
    in (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util_.DecodingError "expected union of type hydra.testing.EvaluationStyle"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

caseConversionTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.CaseConversionTestCase)
caseConversionTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "fromConvention" Util.caseConvention fieldMap cx) (\field_fromConvention -> Eithers.bind (Helpers.requireField "toConvention" Util.caseConvention fieldMap cx) (\field_toConvention -> Eithers.bind (Helpers.requireField "fromString" (\cx -> \raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_fromString -> Eithers.bind (Helpers.requireField "toString" (\cx -> \raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_toString -> Right (Testing.CaseConversionTestCase {
      Testing.caseConversionTestCaseFromConvention = field_fromConvention,
      Testing.caseConversionTestCaseToConvention = field_toConvention,
      Testing.caseConversionTestCaseFromString = field_fromString,
      Testing.caseConversionTestCaseToString = field_toString}))))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.CaseConversionTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

delegatedEvaluationTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.DelegatedEvaluationTestCase)
delegatedEvaluationTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.term fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" Core_.term fieldMap cx) (\field_output -> Right (Testing.DelegatedEvaluationTestCase {
      Testing.delegatedEvaluationTestCaseInput = field_input,
      Testing.delegatedEvaluationTestCaseOutput = field_output}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.DelegatedEvaluationTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

etaExpansionTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.EtaExpansionTestCase)
etaExpansionTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.term fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" Core_.term fieldMap cx) (\field_output -> Right (Testing.EtaExpansionTestCase {
      Testing.etaExpansionTestCaseInput = field_input,
      Testing.etaExpansionTestCaseOutput = field_output}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.EtaExpansionTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

deannotateTermTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.DeannotateTermTestCase)
deannotateTermTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.term fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" Core_.term fieldMap cx) (\field_output -> Right (Testing.DeannotateTermTestCase {
      Testing.deannotateTermTestCaseInput = field_input,
      Testing.deannotateTermTestCaseOutput = field_output}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.DeannotateTermTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

deannotateTypeTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.DeannotateTypeTestCase)
deannotateTypeTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.type_ fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" Core_.type_ fieldMap cx) (\field_output -> Right (Testing.DeannotateTypeTestCase {
      Testing.deannotateTypeTestCaseInput = field_input,
      Testing.deannotateTypeTestCaseOutput = field_output}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.DeannotateTypeTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

flattenLetTermsTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.FlattenLetTermsTestCase)
flattenLetTermsTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.term fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" Core_.term fieldMap cx) (\field_output -> Right (Testing.FlattenLetTermsTestCase {
      Testing.flattenLetTermsTestCaseInput = field_input,
      Testing.flattenLetTermsTestCaseOutput = field_output}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.FlattenLetTermsTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

foldOperation :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.FoldOperation)
foldOperation cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "sumInt32Literals", (\input -> Eithers.map (\t -> Testing.FoldOperationSumInt32Literals) (Helpers.decodeUnit cx input))),
                (Core.Name "collectListLengths", (\input -> Eithers.map (\t -> Testing.FoldOperationCollectListLengths) (Helpers.decodeUnit cx input))),
                (Core.Name "collectLabels", (\input -> Eithers.map (\t -> Testing.FoldOperationCollectLabels) (Helpers.decodeUnit cx input)))])
    in (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util_.DecodingError "expected union of type hydra.testing.FoldOperation"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

foldOverTermTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.FoldOverTermTestCase)
foldOverTermTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.term fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "traversalOrder" Coders.traversalOrder fieldMap cx) (\field_traversalOrder -> Eithers.bind (Helpers.requireField "operation" foldOperation fieldMap cx) (\field_operation -> Eithers.bind (Helpers.requireField "output" Core_.term fieldMap cx) (\field_output -> Right (Testing.FoldOverTermTestCase {
      Testing.foldOverTermTestCaseInput = field_input,
      Testing.foldOverTermTestCaseTraversalOrder = field_traversalOrder,
      Testing.foldOverTermTestCaseOperation = field_operation,
      Testing.foldOverTermTestCaseOutput = field_output}))))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.FoldOverTermTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

freeVariablesTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.FreeVariablesTestCase)
freeVariablesTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.term fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" (Helpers.decodeSet Core_.name) fieldMap cx) (\field_output -> Right (Testing.FreeVariablesTestCase {
      Testing.freeVariablesTestCaseInput = field_input,
      Testing.freeVariablesTestCaseOutput = field_output}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.FreeVariablesTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

hoistPredicate :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.HoistPredicate)
hoistPredicate cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "caseStatements", (\input -> Eithers.map (\t -> Testing.HoistPredicateCaseStatements) (Helpers.decodeUnit cx input))),
                (Core.Name "applications", (\input -> Eithers.map (\t -> Testing.HoistPredicateApplications) (Helpers.decodeUnit cx input))),
                (Core.Name "lists", (\input -> Eithers.map (\t -> Testing.HoistPredicateLists) (Helpers.decodeUnit cx input))),
                (Core.Name "nothing", (\input -> Eithers.map (\t -> Testing.HoistPredicateNothing) (Helpers.decodeUnit cx input)))])
    in (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util_.DecodingError "expected union of type hydra.testing.HoistPredicate"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

hoistLetBindingsTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.HoistLetBindingsTestCase)
hoistLetBindingsTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.let_ fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" Core_.let_ fieldMap cx) (\field_output -> Right (Testing.HoistLetBindingsTestCase {
      Testing.hoistLetBindingsTestCaseInput = field_input,
      Testing.hoistLetBindingsTestCaseOutput = field_output}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.HoistLetBindingsTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

hoistPolymorphicLetBindingsTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.HoistPolymorphicLetBindingsTestCase)
hoistPolymorphicLetBindingsTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.let_ fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" Core_.let_ fieldMap cx) (\field_output -> Right (Testing.HoistPolymorphicLetBindingsTestCase {
      Testing.hoistPolymorphicLetBindingsTestCaseInput = field_input,
      Testing.hoistPolymorphicLetBindingsTestCaseOutput = field_output}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.HoistPolymorphicLetBindingsTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

hoistSubtermsTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.HoistSubtermsTestCase)
hoistSubtermsTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "predicate" hoistPredicate fieldMap cx) (\field_predicate -> Eithers.bind (Helpers.requireField "input" Core_.term fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" Core_.term fieldMap cx) (\field_output -> Right (Testing.HoistSubtermsTestCase {
      Testing.hoistSubtermsTestCasePredicate = field_predicate,
      Testing.hoistSubtermsTestCaseInput = field_input,
      Testing.hoistSubtermsTestCaseOutput = field_output})))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.HoistSubtermsTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

hoistCaseStatementsTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.HoistCaseStatementsTestCase)
hoistCaseStatementsTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.term fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" Core_.term fieldMap cx) (\field_output -> Right (Testing.HoistCaseStatementsTestCase {
      Testing.hoistCaseStatementsTestCaseInput = field_input,
      Testing.hoistCaseStatementsTestCaseOutput = field_output}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.HoistCaseStatementsTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

termRewriter :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TermRewriter)
termRewriter cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "replaceFooWithBar", (\input -> Eithers.map (\t -> Testing.TermRewriterReplaceFooWithBar) (Helpers.decodeUnit cx input))),
                (Core.Name "replaceInt32WithInt64", (\input -> Eithers.map (\t -> Testing.TermRewriterReplaceInt32WithInt64) (Helpers.decodeUnit cx input)))])
    in (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util_.DecodingError "expected union of type hydra.testing.TermRewriter"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

rewriteTermTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.RewriteTermTestCase)
rewriteTermTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.term fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "rewriter" termRewriter fieldMap cx) (\field_rewriter -> Eithers.bind (Helpers.requireField "output" Core_.term fieldMap cx) (\field_output -> Right (Testing.RewriteTermTestCase {
      Testing.rewriteTermTestCaseInput = field_input,
      Testing.rewriteTermTestCaseRewriter = field_rewriter,
      Testing.rewriteTermTestCaseOutput = field_output})))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.RewriteTermTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeRewriter :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TypeRewriter)
typeRewriter cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "replaceStringWithInt32", (\input -> Eithers.map (\t -> Testing.TypeRewriterReplaceStringWithInt32) (Helpers.decodeUnit cx input)))])
    in (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util_.DecodingError "expected union of type hydra.testing.TypeRewriter"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

rewriteTypeTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.RewriteTypeTestCase)
rewriteTypeTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.type_ fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "rewriter" typeRewriter fieldMap cx) (\field_rewriter -> Eithers.bind (Helpers.requireField "output" Core_.type_ fieldMap cx) (\field_output -> Right (Testing.RewriteTypeTestCase {
      Testing.rewriteTypeTestCaseInput = field_input,
      Testing.rewriteTypeTestCaseRewriter = field_rewriter,
      Testing.rewriteTypeTestCaseOutput = field_output})))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.RewriteTypeTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

evaluationTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.EvaluationTestCase)
evaluationTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "evaluationStyle" evaluationStyle fieldMap cx) (\field_evaluationStyle -> Eithers.bind (Helpers.requireField "input" Core_.term fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" Core_.term fieldMap cx) (\field_output -> Right (Testing.EvaluationTestCase {
      Testing.evaluationTestCaseEvaluationStyle = field_evaluationStyle,
      Testing.evaluationTestCaseInput = field_input,
      Testing.evaluationTestCaseOutput = field_output})))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.EvaluationTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

inferenceFailureTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.InferenceFailureTestCase)
inferenceFailureTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.term fieldMap cx) (\field_input -> Right (Testing.InferenceFailureTestCase {
      Testing.inferenceFailureTestCaseInput = field_input})))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.InferenceFailureTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

inferenceTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.InferenceTestCase)
inferenceTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.term fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" Core_.typeScheme fieldMap cx) (\field_output -> Right (Testing.InferenceTestCase {
      Testing.inferenceTestCaseInput = field_input,
      Testing.inferenceTestCaseOutput = field_output}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.InferenceTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

jsonCoderTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.JsonCoderTestCase)
jsonCoderTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "type" Core_.type_ fieldMap cx) (\field_type -> Eithers.bind (Helpers.requireField "term" Core_.term fieldMap cx) (\field_term -> Eithers.bind (Helpers.requireField "json" Model.value fieldMap cx) (\field_json -> Right (Testing.JsonCoderTestCase {
      Testing.jsonCoderTestCaseType = field_type,
      Testing.jsonCoderTestCaseTerm = field_term,
      Testing.jsonCoderTestCaseJson = field_json})))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.JsonCoderTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

jsonDecodeTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.JsonDecodeTestCase)
jsonDecodeTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "type" Core_.type_ fieldMap cx) (\field_type -> Eithers.bind (Helpers.requireField "json" Model.value fieldMap cx) (\field_json -> Eithers.bind (Helpers.requireField "expected" (Helpers.decodeEither (\cx -> \raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) Core_.term) fieldMap cx) (\field_expected -> Right (Testing.JsonDecodeTestCase {
      Testing.jsonDecodeTestCaseType = field_type,
      Testing.jsonDecodeTestCaseJson = field_json,
      Testing.jsonDecodeTestCaseExpected = field_expected})))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.JsonDecodeTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

jsonEncodeTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.JsonEncodeTestCase)
jsonEncodeTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "term" Core_.term fieldMap cx) (\field_term -> Eithers.bind (Helpers.requireField "expected" (Helpers.decodeEither (\cx -> \raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) Model.value) fieldMap cx) (\field_expected -> Right (Testing.JsonEncodeTestCase {
      Testing.jsonEncodeTestCaseTerm = field_term,
      Testing.jsonEncodeTestCaseExpected = field_expected}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.JsonEncodeTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

jsonParserTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError (Testing.ParserTestCase Model_.Value))
jsonParserTestCase = (parserTestCase Model.value)

jsonRoundtripTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.JsonRoundtripTestCase)
jsonRoundtripTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "type" Core_.type_ fieldMap cx) (\field_type -> Eithers.bind (Helpers.requireField "term" Core_.term fieldMap cx) (\field_term -> Right (Testing.JsonRoundtripTestCase {
      Testing.jsonRoundtripTestCaseType = field_type,
      Testing.jsonRoundtripTestCaseTerm = field_term}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.JsonRoundtripTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

liftLambdaAboveLetTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.LiftLambdaAboveLetTestCase)
liftLambdaAboveLetTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.term fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" Core_.term fieldMap cx) (\field_output -> Right (Testing.LiftLambdaAboveLetTestCase {
      Testing.liftLambdaAboveLetTestCaseInput = field_input,
      Testing.liftLambdaAboveLetTestCaseOutput = field_output}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.LiftLambdaAboveLetTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

jsonWriterTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError (Testing.WriterTestCase Model_.Value))
jsonWriterTestCase = (writerTestCase Model.value)

parserTestCase :: ((Graph.Graph -> Core.Term -> Either Util_.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util_.DecodingError (Testing.ParserTestCase t0))
parserTestCase a cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" (\cx -> \raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" (Parsing.parseResult a) fieldMap cx) (\field_output -> Right (Testing.ParserTestCase {
      Testing.parserTestCaseInput = field_input,
      Testing.parserTestCaseOutput = field_output}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.ParserTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

tag :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.Tag)
tag cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Testing.Tag b) ((\raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
    Core.TermLiteral v2 -> ((\x -> case x of
      Core.LiteralString v3 -> (Right v3)
      _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
    _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v1)))
  _ -> (Left (Util_.DecodingError "expected wrapped type hydra.testing.Tag"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

testCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TestCase)
testCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "alphaConversion", (\input -> Eithers.map (\t -> Testing.TestCaseAlphaConversion t) (alphaConversionTestCase cx input))),
                (Core.Name "caseConversion", (\input -> Eithers.map (\t -> Testing.TestCaseCaseConversion t) (caseConversionTestCase cx input))),
                (Core.Name "deannotateTerm", (\input -> Eithers.map (\t -> Testing.TestCaseDeannotateTerm t) (deannotateTermTestCase cx input))),
                (Core.Name "deannotateType", (\input -> Eithers.map (\t -> Testing.TestCaseDeannotateType t) (deannotateTypeTestCase cx input))),
                (Core.Name "delegatedEvaluation", (\input -> Eithers.map (\t -> Testing.TestCaseDelegatedEvaluation t) (delegatedEvaluationTestCase cx input))),
                (Core.Name "etaExpansion", (\input -> Eithers.map (\t -> Testing.TestCaseEtaExpansion t) (etaExpansionTestCase cx input))),
                (Core.Name "flattenLetTerms", (\input -> Eithers.map (\t -> Testing.TestCaseFlattenLetTerms t) (flattenLetTermsTestCase cx input))),
                (Core.Name "freeVariables", (\input -> Eithers.map (\t -> Testing.TestCaseFreeVariables t) (freeVariablesTestCase cx input))),
                (Core.Name "evaluation", (\input -> Eithers.map (\t -> Testing.TestCaseEvaluation t) (evaluationTestCase cx input))),
                (Core.Name "inference", (\input -> Eithers.map (\t -> Testing.TestCaseInference t) (inferenceTestCase cx input))),
                (Core.Name "inferenceFailure", (\input -> Eithers.map (\t -> Testing.TestCaseInferenceFailure t) (inferenceFailureTestCase cx input))),
                (Core.Name "jsonCoder", (\input -> Eithers.map (\t -> Testing.TestCaseJsonCoder t) (jsonCoderTestCase cx input))),
                (Core.Name "jsonDecode", (\input -> Eithers.map (\t -> Testing.TestCaseJsonDecode t) (jsonDecodeTestCase cx input))),
                (Core.Name "jsonEncode", (\input -> Eithers.map (\t -> Testing.TestCaseJsonEncode t) (jsonEncodeTestCase cx input))),
                (Core.Name "jsonParser", (\input -> Eithers.map (\t -> Testing.TestCaseJsonParser t) (jsonParserTestCase cx input))),
                (Core.Name "jsonRoundtrip", (\input -> Eithers.map (\t -> Testing.TestCaseJsonRoundtrip t) (jsonRoundtripTestCase cx input))),
                (Core.Name "jsonWriter", (\input -> Eithers.map (\t -> Testing.TestCaseJsonWriter t) (jsonWriterTestCase cx input))),
                (Core.Name "liftLambdaAboveLet", (\input -> Eithers.map (\t -> Testing.TestCaseLiftLambdaAboveLet t) (liftLambdaAboveLetTestCase cx input))),
                (Core.Name "serialization", (\input -> Eithers.map (\t -> Testing.TestCaseSerialization t) (serializationTestCase cx input))),
                (Core.Name "simplifyTerm", (\input -> Eithers.map (\t -> Testing.TestCaseSimplifyTerm t) (simplifyTermTestCase cx input))),
                (Core.Name "topologicalSort", (\input -> Eithers.map (\t -> Testing.TestCaseTopologicalSort t) (topologicalSortTestCase cx input))),
                (Core.Name "topologicalSortBindings", (\input -> Eithers.map (\t -> Testing.TestCaseTopologicalSortBindings t) (topologicalSortBindingsTestCase cx input))),
                (Core.Name "topologicalSortSCC", (\input -> Eithers.map (\t -> Testing.TestCaseTopologicalSortSCC t) (topologicalSortSCCTestCase cx input))),
                (Core.Name "typeChecking", (\input -> Eithers.map (\t -> Testing.TestCaseTypeChecking t) (typeCheckingTestCase cx input))),
                (Core.Name "typeCheckingFailure", (\input -> Eithers.map (\t -> Testing.TestCaseTypeCheckingFailure t) (typeCheckingFailureTestCase cx input))),
                (Core.Name "typeReduction", (\input -> Eithers.map (\t -> Testing.TestCaseTypeReduction t) (typeReductionTestCase cx input))),
                (Core.Name "normalizeTypeVariables", (\input -> Eithers.map (\t -> Testing.TestCaseNormalizeTypeVariables t) (normalizeTypeVariablesTestCase cx input))),
                (Core.Name "foldOverTerm", (\input -> Eithers.map (\t -> Testing.TestCaseFoldOverTerm t) (foldOverTermTestCase cx input))),
                (Core.Name "rewriteTerm", (\input -> Eithers.map (\t -> Testing.TestCaseRewriteTerm t) (rewriteTermTestCase cx input))),
                (Core.Name "rewriteType", (\input -> Eithers.map (\t -> Testing.TestCaseRewriteType t) (rewriteTypeTestCase cx input))),
                (Core.Name "hoistSubterms", (\input -> Eithers.map (\t -> Testing.TestCaseHoistSubterms t) (hoistSubtermsTestCase cx input))),
                (Core.Name "hoistCaseStatements", (\input -> Eithers.map (\t -> Testing.TestCaseHoistCaseStatements t) (hoistCaseStatementsTestCase cx input))),
                (Core.Name "hoistLetBindings", (\input -> Eithers.map (\t -> Testing.TestCaseHoistLetBindings t) (hoistLetBindingsTestCase cx input))),
                (Core.Name "hoistPolymorphicLetBindings", (\input -> Eithers.map (\t -> Testing.TestCaseHoistPolymorphicLetBindings t) (hoistPolymorphicLetBindingsTestCase cx input)))])
    in (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util_.DecodingError "expected union of type hydra.testing.TestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

testCaseWithMetadata :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TestCaseWithMetadata)
testCaseWithMetadata cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "name" (\cx -> \raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_name -> Eithers.bind (Helpers.requireField "case" testCase fieldMap cx) (\field_case -> Eithers.bind (Helpers.requireField "description" (Helpers.decodeMaybe (\cx -> \raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))) fieldMap cx) (\field_description -> Eithers.bind (Helpers.requireField "tags" (Helpers.decodeList tag) fieldMap cx) (\field_tags -> Right (Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = field_name,
      Testing.testCaseWithMetadataCase = field_case,
      Testing.testCaseWithMetadataDescription = field_description,
      Testing.testCaseWithMetadataTags = field_tags}))))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.TestCaseWithMetadata"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

testGroup :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TestGroup)
testGroup cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "name" (\cx -> \raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_name -> Eithers.bind (Helpers.requireField "description" (Helpers.decodeMaybe (\cx -> \raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))) fieldMap cx) (\field_description -> Eithers.bind (Helpers.requireField "subgroups" (Helpers.decodeList testGroup) fieldMap cx) (\field_subgroups -> Eithers.bind (Helpers.requireField "cases" (Helpers.decodeList testCaseWithMetadata) fieldMap cx) (\field_cases -> Right (Testing.TestGroup {
      Testing.testGroupName = field_name,
      Testing.testGroupDescription = field_description,
      Testing.testGroupSubgroups = field_subgroups,
      Testing.testGroupCases = field_cases}))))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.TestGroup"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeCheckingTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TypeCheckingTestCase)
typeCheckingTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.term fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "outputTerm" Core_.term fieldMap cx) (\field_outputTerm -> Eithers.bind (Helpers.requireField "outputType" Core_.type_ fieldMap cx) (\field_outputType -> Right (Testing.TypeCheckingTestCase {
      Testing.typeCheckingTestCaseInput = field_input,
      Testing.typeCheckingTestCaseOutputTerm = field_outputTerm,
      Testing.typeCheckingTestCaseOutputType = field_outputType})))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.TypeCheckingTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeCheckingFailureTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TypeCheckingFailureTestCase)
typeCheckingFailureTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.term fieldMap cx) (\field_input -> Right (Testing.TypeCheckingFailureTestCase {
      Testing.typeCheckingFailureTestCaseInput = field_input})))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.TypeCheckingFailureTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

topologicalSortBindingsTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TopologicalSortBindingsTestCase)
topologicalSortBindingsTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "bindings" (Helpers.decodeList (Helpers.decodePair Core_.name Core_.term)) fieldMap cx) (\field_bindings -> Eithers.bind (Helpers.requireField "expected" (Helpers.decodeList (Helpers.decodeList (Helpers.decodePair Core_.name Core_.term))) fieldMap cx) (\field_expected -> Right (Testing.TopologicalSortBindingsTestCase {
      Testing.topologicalSortBindingsTestCaseBindings = field_bindings,
      Testing.topologicalSortBindingsTestCaseExpected = field_expected}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.TopologicalSortBindingsTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

topologicalSortTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TopologicalSortTestCase)
topologicalSortTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "adjacencyList" (Helpers.decodeList (Helpers.decodePair (\cx -> \raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralInteger v3 -> ((\x -> case x of
          Core.IntegerValueInt32 v4 -> (Right v4)
          _ -> (Left (Util_.DecodingError "expected int32 value"))) v3)
        _ -> (Left (Util_.DecodingError "expected int32 literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) (Helpers.decodeList (\cx -> \raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralInteger v3 -> ((\x -> case x of
          Core.IntegerValueInt32 v4 -> (Right v4)
          _ -> (Left (Util_.DecodingError "expected int32 value"))) v3)
        _ -> (Left (Util_.DecodingError "expected int32 literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))))) fieldMap cx) (\field_adjacencyList -> Eithers.bind (Helpers.requireField "expected" (Helpers.decodeEither (Helpers.decodeList (Helpers.decodeList (\cx -> \raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralInteger v3 -> ((\x -> case x of
          Core.IntegerValueInt32 v4 -> (Right v4)
          _ -> (Left (Util_.DecodingError "expected int32 value"))) v3)
        _ -> (Left (Util_.DecodingError "expected int32 literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)))) (Helpers.decodeList (\cx -> \raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralInteger v3 -> ((\x -> case x of
          Core.IntegerValueInt32 v4 -> (Right v4)
          _ -> (Left (Util_.DecodingError "expected int32 value"))) v3)
        _ -> (Left (Util_.DecodingError "expected int32 literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)))) fieldMap cx) (\field_expected -> Right (Testing.TopologicalSortTestCase {
      Testing.topologicalSortTestCaseAdjacencyList = field_adjacencyList,
      Testing.topologicalSortTestCaseExpected = field_expected}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.TopologicalSortTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

topologicalSortSCCTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TopologicalSortSCCTestCase)
topologicalSortSCCTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "adjacencyList" (Helpers.decodeList (Helpers.decodePair (\cx -> \raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralInteger v3 -> ((\x -> case x of
          Core.IntegerValueInt32 v4 -> (Right v4)
          _ -> (Left (Util_.DecodingError "expected int32 value"))) v3)
        _ -> (Left (Util_.DecodingError "expected int32 literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) (Helpers.decodeList (\cx -> \raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralInteger v3 -> ((\x -> case x of
          Core.IntegerValueInt32 v4 -> (Right v4)
          _ -> (Left (Util_.DecodingError "expected int32 value"))) v3)
        _ -> (Left (Util_.DecodingError "expected int32 literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))))) fieldMap cx) (\field_adjacencyList -> Eithers.bind (Helpers.requireField "expected" (Helpers.decodeList (Helpers.decodeList (\cx -> \raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralInteger v3 -> ((\x -> case x of
          Core.IntegerValueInt32 v4 -> (Right v4)
          _ -> (Left (Util_.DecodingError "expected int32 value"))) v3)
        _ -> (Left (Util_.DecodingError "expected int32 literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)))) fieldMap cx) (\field_expected -> Right (Testing.TopologicalSortSCCTestCase {
      Testing.topologicalSortSCCTestCaseAdjacencyList = field_adjacencyList,
      Testing.topologicalSortSCCTestCaseExpected = field_expected}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.TopologicalSortSCCTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

serializationTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.SerializationTestCase)
serializationTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Ast.expr fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" (\cx -> \raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_output -> Right (Testing.SerializationTestCase {
      Testing.serializationTestCaseInput = field_input,
      Testing.serializationTestCaseOutput = field_output}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.SerializationTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

simplifyTermTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.SimplifyTermTestCase)
simplifyTermTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.term fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" Core_.term fieldMap cx) (\field_output -> Right (Testing.SimplifyTermTestCase {
      Testing.simplifyTermTestCaseInput = field_input,
      Testing.simplifyTermTestCaseOutput = field_output}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.SimplifyTermTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

normalizeTypeVariablesTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.NormalizeTypeVariablesTestCase)
normalizeTypeVariablesTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.term fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" Core_.term fieldMap cx) (\field_output -> Right (Testing.NormalizeTypeVariablesTestCase {
      Testing.normalizeTypeVariablesTestCaseInput = field_input,
      Testing.normalizeTypeVariablesTestCaseOutput = field_output}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.NormalizeTypeVariablesTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeReductionTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TypeReductionTestCase)
typeReductionTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" Core_.type_ fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" Core_.type_ fieldMap cx) (\field_output -> Right (Testing.TypeReductionTestCase {
      Testing.typeReductionTestCaseInput = field_input,
      Testing.typeReductionTestCaseOutput = field_output}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.TypeReductionTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

writerTestCase :: ((Graph.Graph -> Core.Term -> Either Util_.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util_.DecodingError (Testing.WriterTestCase t0))
writerTestCase a cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "input" a fieldMap cx) (\field_input -> Eithers.bind (Helpers.requireField "output" (\cx -> \raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_output -> Right (Testing.WriterTestCase {
      Testing.writerTestCaseInput = field_input,
      Testing.writerTestCaseOutput = field_output}))))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.WriterTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
