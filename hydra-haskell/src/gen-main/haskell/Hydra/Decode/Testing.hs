-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.testing

module Hydra.Decode.Testing where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Ast as Ast
import qualified Hydra.Decode.Coders as Coders
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Decode.Json as Json
import qualified Hydra.Decode.Parsing as Parsing
import qualified Hydra.Decode.Util as Util
import qualified Hydra.Graph as Graph
import qualified Hydra.Json as Json_
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Testing as Testing
import qualified Hydra.Util as Util_
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

alphaConversionTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.AlphaConversionTestCase)
alphaConversionTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\term -> Eithers.either (\err -> Left err) (\oldVariable -> Eithers.either (\err -> Left err) (\newVariable -> Eithers.either (\err -> Left err) (\result -> Right (Testing.AlphaConversionTestCase {
      Testing.alphaConversionTestCaseTerm = term,
      Testing.alphaConversionTestCaseOldVariable = oldVariable,
      Testing.alphaConversionTestCaseNewVariable = newVariable,
      Testing.alphaConversionTestCaseResult = result})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "result",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "result") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "newVariable",
      " in record"]))) (\fieldTerm -> Core_.name cx fieldTerm) (Maps.lookup (Core.Name "newVariable") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "oldVariable",
      " in record"]))) (\fieldTerm -> Core_.name cx fieldTerm) (Maps.lookup (Core.Name "oldVariable") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "term",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "term") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.AlphaConversionTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

evaluationStyle :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.EvaluationStyle)
evaluationStyle cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "eager", (\input -> Eithers.map (\t -> Testing.EvaluationStyleEager) (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util_.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "lazy", (\input -> Eithers.map (\t -> Testing.EvaluationStyleLazy) (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util_.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util_.DecodingError "expected union of type hydra.testing.EvaluationStyle"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

caseConversionTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.CaseConversionTestCase)
caseConversionTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\fromConvention -> Eithers.either (\err -> Left err) (\toConvention -> Eithers.either (\err -> Left err) (\fromString -> Eithers.either (\err -> Left err) (\toString -> Right (Testing.CaseConversionTestCase {
      Testing.caseConversionTestCaseFromConvention = fromConvention,
      Testing.caseConversionTestCaseToConvention = toConvention,
      Testing.caseConversionTestCaseFromString = fromString,
      Testing.caseConversionTestCaseToString = toString})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "toString",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "toString") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "fromString",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "fromString") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "toConvention",
      " in record"]))) (\fieldTerm -> Util.caseConvention cx fieldTerm) (Maps.lookup (Core.Name "toConvention") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "fromConvention",
      " in record"]))) (\fieldTerm -> Util.caseConvention cx fieldTerm) (Maps.lookup (Core.Name "fromConvention") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.CaseConversionTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

delegatedEvaluationTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.DelegatedEvaluationTestCase)
delegatedEvaluationTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\output -> Right (Testing.DelegatedEvaluationTestCase {
      Testing.delegatedEvaluationTestCaseInput = input,
      Testing.delegatedEvaluationTestCaseOutput = output})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "output",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "output") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.DelegatedEvaluationTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

etaExpansionTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.EtaExpansionTestCase)
etaExpansionTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\output -> Right (Testing.EtaExpansionTestCase {
      Testing.etaExpansionTestCaseInput = input,
      Testing.etaExpansionTestCaseOutput = output})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "output",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "output") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.EtaExpansionTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

deannotateTermTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.DeannotateTermTestCase)
deannotateTermTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\output -> Right (Testing.DeannotateTermTestCase {
      Testing.deannotateTermTestCaseInput = input,
      Testing.deannotateTermTestCaseOutput = output})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "output",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "output") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.DeannotateTermTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

deannotateTypeTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.DeannotateTypeTestCase)
deannotateTypeTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\output -> Right (Testing.DeannotateTypeTestCase {
      Testing.deannotateTypeTestCaseInput = input,
      Testing.deannotateTypeTestCaseOutput = output})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "output",
      " in record"]))) (\fieldTerm -> Core_.type_ cx fieldTerm) (Maps.lookup (Core.Name "output") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Core_.type_ cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.DeannotateTypeTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

flattenLetTermsTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.FlattenLetTermsTestCase)
flattenLetTermsTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\output -> Right (Testing.FlattenLetTermsTestCase {
      Testing.flattenLetTermsTestCaseInput = input,
      Testing.flattenLetTermsTestCaseOutput = output})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "output",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "output") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.FlattenLetTermsTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

foldOperation :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.FoldOperation)
foldOperation cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "sumInt32Literals", (\input -> Eithers.map (\t -> Testing.FoldOperationSumInt32Literals) (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util_.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "collectListLengths", (\input -> Eithers.map (\t -> Testing.FoldOperationCollectListLengths) (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util_.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "collectLabels", (\input -> Eithers.map (\t -> Testing.FoldOperationCollectLabels) (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util_.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util_.DecodingError "expected union of type hydra.testing.FoldOperation"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

foldOverTermTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.FoldOverTermTestCase)
foldOverTermTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\traversalOrder -> Eithers.either (\err -> Left err) (\operation -> Eithers.either (\err -> Left err) (\output -> Right (Testing.FoldOverTermTestCase {
      Testing.foldOverTermTestCaseInput = input,
      Testing.foldOverTermTestCaseTraversalOrder = traversalOrder,
      Testing.foldOverTermTestCaseOperation = operation,
      Testing.foldOverTermTestCaseOutput = output})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "output",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "output") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "operation",
      " in record"]))) (\fieldTerm -> foldOperation cx fieldTerm) (Maps.lookup (Core.Name "operation") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "traversalOrder",
      " in record"]))) (\fieldTerm -> Coders.traversalOrder cx fieldTerm) (Maps.lookup (Core.Name "traversalOrder") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.FoldOverTermTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

freeVariablesTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.FreeVariablesTestCase)
freeVariablesTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\output -> Right (Testing.FreeVariablesTestCase {
      Testing.freeVariablesTestCaseInput = input,
      Testing.freeVariablesTestCaseOutput = output})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "output",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermSet v2 ->  
        let elements = (Sets.toList v2)
        in (Eithers.either (\err -> Left err) (\decodedElems -> Right (Sets.fromList decodedElems)) (Eithers.mapList (Core_.name cx) elements))
      _ -> (Left (Util_.DecodingError "expected set"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "output") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.FreeVariablesTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

termRewriter :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TermRewriter)
termRewriter cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "replaceFooWithBar", (\input -> Eithers.map (\t -> Testing.TermRewriterReplaceFooWithBar) (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util_.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input)))),
                (Core.Name "replaceInt32WithInt64", (\input -> Eithers.map (\t -> Testing.TermRewriterReplaceInt32WithInt64) (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util_.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util_.DecodingError "expected union of type hydra.testing.TermRewriter"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

rewriteTermTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.RewriteTermTestCase)
rewriteTermTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\rewriter -> Eithers.either (\err -> Left err) (\output -> Right (Testing.RewriteTermTestCase {
      Testing.rewriteTermTestCaseInput = input,
      Testing.rewriteTermTestCaseRewriter = rewriter,
      Testing.rewriteTermTestCaseOutput = output})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "output",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "output") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "rewriter",
      " in record"]))) (\fieldTerm -> termRewriter cx fieldTerm) (Maps.lookup (Core.Name "rewriter") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.RewriteTermTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeRewriter :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TypeRewriter)
typeRewriter cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "replaceStringWithInt32", (\input -> Eithers.map (\t -> Testing.TypeRewriterReplaceStringWithInt32) (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
                  Core.TermUnit -> (Right ())
                  _ -> (Left (Util_.DecodingError "expected a unit value"))) stripped) (Lexical.stripAndDereferenceTermEither cx input))))])
    in (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util_.DecodingError "expected union of type hydra.testing.TypeRewriter"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

rewriteTypeTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.RewriteTypeTestCase)
rewriteTypeTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\rewriter -> Eithers.either (\err -> Left err) (\output -> Right (Testing.RewriteTypeTestCase {
      Testing.rewriteTypeTestCaseInput = input,
      Testing.rewriteTypeTestCaseRewriter = rewriter,
      Testing.rewriteTypeTestCaseOutput = output})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "output",
      " in record"]))) (\fieldTerm -> Core_.type_ cx fieldTerm) (Maps.lookup (Core.Name "output") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "rewriter",
      " in record"]))) (\fieldTerm -> typeRewriter cx fieldTerm) (Maps.lookup (Core.Name "rewriter") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Core_.type_ cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.RewriteTypeTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

evaluationTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.EvaluationTestCase)
evaluationTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\evaluationStyle -> Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\output -> Right (Testing.EvaluationTestCase {
      Testing.evaluationTestCaseEvaluationStyle = evaluationStyle,
      Testing.evaluationTestCaseInput = input,
      Testing.evaluationTestCaseOutput = output})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "output",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "output") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "evaluationStyle",
      " in record"]))) (\fieldTerm -> evaluationStyle cx fieldTerm) (Maps.lookup (Core.Name "evaluationStyle") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.EvaluationTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

inferenceFailureTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.InferenceFailureTestCase)
inferenceFailureTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Right (Testing.InferenceFailureTestCase {
      Testing.inferenceFailureTestCaseInput = input})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.InferenceFailureTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

inferenceTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.InferenceTestCase)
inferenceTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\output -> Right (Testing.InferenceTestCase {
      Testing.inferenceTestCaseInput = input,
      Testing.inferenceTestCaseOutput = output})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "output",
      " in record"]))) (\fieldTerm -> Core_.typeScheme cx fieldTerm) (Maps.lookup (Core.Name "output") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.InferenceTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

jsonCoderTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.JsonCoderTestCase)
jsonCoderTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\type_ -> Eithers.either (\err -> Left err) (\term -> Eithers.either (\err -> Left err) (\json -> Right (Testing.JsonCoderTestCase {
      Testing.jsonCoderTestCaseType = type_,
      Testing.jsonCoderTestCaseTerm = term,
      Testing.jsonCoderTestCaseJson = json})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "json",
      " in record"]))) (\fieldTerm -> Json.value cx fieldTerm) (Maps.lookup (Core.Name "json") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "term",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "term") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "type",
      " in record"]))) (\fieldTerm -> Core_.type_ cx fieldTerm) (Maps.lookup (Core.Name "type") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.JsonCoderTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

jsonParserTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError (Testing.ParserTestCase Json_.Value))
jsonParserTestCase = (parserTestCase Json.value)

liftLambdaAboveLetTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.LiftLambdaAboveLetTestCase)
liftLambdaAboveLetTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\output -> Right (Testing.LiftLambdaAboveLetTestCase {
      Testing.liftLambdaAboveLetTestCaseInput = input,
      Testing.liftLambdaAboveLetTestCaseOutput = output})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "output",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "output") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.LiftLambdaAboveLetTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

jsonWriterTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError (Testing.WriterTestCase Json_.Value))
jsonWriterTestCase = (writerTestCase Json.value)

parserTestCase :: ((Graph.Graph -> Core.Term -> Either Util_.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util_.DecodingError (Testing.ParserTestCase t0))
parserTestCase a cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\output -> Right (Testing.ParserTestCase {
      Testing.parserTestCaseInput = input,
      Testing.parserTestCaseOutput = output})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "output",
      " in record"]))) (\fieldTerm -> Parsing.parseResult a cx fieldTerm) (Maps.lookup (Core.Name "output") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "input") fieldMap)))
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
                (Core.Name "jsonParser", (\input -> Eithers.map (\t -> Testing.TestCaseJsonParser t) (jsonParserTestCase cx input))),
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
                (Core.Name "rewriteType", (\input -> Eithers.map (\t -> Testing.TestCaseRewriteType t) (rewriteTypeTestCase cx input)))])
    in (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util_.DecodingError "expected union of type hydra.testing.TestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

testCaseWithMetadata :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TestCaseWithMetadata)
testCaseWithMetadata cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\name -> Eithers.either (\err -> Left err) (\case_ -> Eithers.either (\err -> Left err) (\description -> Eithers.either (\err -> Left err) (\tags -> Right (Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = name,
      Testing.testCaseWithMetadataCase = case_,
      Testing.testCaseWithMetadataDescription = description,
      Testing.testCaseWithMetadataTags = tags})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "tags",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (tag cx) v2)
      _ -> (Left (Util_.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "tags") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "description",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMaybe v2 -> (Eithers.mapMaybe (\raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
        Core.TermLiteral v3 -> ((\x -> case x of
          Core.LiteralString v4 -> (Right v4)
          _ -> (Left (Util_.DecodingError "expected string literal"))) v3)
        _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v2)
      _ -> (Left (Util_.DecodingError "expected optional value"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "description") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "case",
      " in record"]))) (\fieldTerm -> testCase cx fieldTerm) (Maps.lookup (Core.Name "case") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "name",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "name") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.TestCaseWithMetadata"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

testGroup :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TestGroup)
testGroup cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\name -> Eithers.either (\err -> Left err) (\description -> Eithers.either (\err -> Left err) (\subgroups -> Eithers.either (\err -> Left err) (\cases -> Right (Testing.TestGroup {
      Testing.testGroupName = name,
      Testing.testGroupDescription = description,
      Testing.testGroupSubgroups = subgroups,
      Testing.testGroupCases = cases})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "cases",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (testCaseWithMetadata cx) v2)
      _ -> (Left (Util_.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "cases") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "subgroups",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (testGroup cx) v2)
      _ -> (Left (Util_.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "subgroups") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "description",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermMaybe v2 -> (Eithers.mapMaybe (\raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
        Core.TermLiteral v3 -> ((\x -> case x of
          Core.LiteralString v4 -> (Right v4)
          _ -> (Left (Util_.DecodingError "expected string literal"))) v3)
        _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v2)
      _ -> (Left (Util_.DecodingError "expected optional value"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "description") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "name",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "name") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.TestGroup"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeCheckingTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TypeCheckingTestCase)
typeCheckingTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\outputTerm -> Eithers.either (\err -> Left err) (\outputType -> Right (Testing.TypeCheckingTestCase {
      Testing.typeCheckingTestCaseInput = input,
      Testing.typeCheckingTestCaseOutputTerm = outputTerm,
      Testing.typeCheckingTestCaseOutputType = outputType})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "outputType",
      " in record"]))) (\fieldTerm -> Core_.type_ cx fieldTerm) (Maps.lookup (Core.Name "outputType") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "outputTerm",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "outputTerm") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.TypeCheckingTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeCheckingFailureTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TypeCheckingFailureTestCase)
typeCheckingFailureTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Right (Testing.TypeCheckingFailureTestCase {
      Testing.typeCheckingFailureTestCaseInput = input})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.TypeCheckingFailureTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

topologicalSortBindingsTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TopologicalSortBindingsTestCase)
topologicalSortBindingsTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\bindings -> Eithers.either (\err -> Left err) (\expected -> Right (Testing.TopologicalSortBindingsTestCase {
      Testing.topologicalSortBindingsTestCaseBindings = bindings,
      Testing.topologicalSortBindingsTestCaseExpected = expected})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "expected",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (\raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
        Core.TermList v3 -> (Eithers.mapList (\raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
          Core.TermPair v4 ->  
            let a = (Pairs.first v4) 
                b = (Pairs.second v4)
            in (Eithers.either (\err -> Left err) (\a2 -> Eithers.either (\err2 -> Left err2) (\b2 -> Right (a2, b2)) (Core_.term cx b)) (Core_.name cx a))
          _ -> (Left (Util_.DecodingError "expected pair"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v3)
        _ -> (Left (Util_.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v2)
      _ -> (Left (Util_.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "expected") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "bindings",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (\raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
        Core.TermPair v3 ->  
          let a = (Pairs.first v3) 
              b = (Pairs.second v3)
          in (Eithers.either (\err -> Left err) (\a2 -> Eithers.either (\err2 -> Left err2) (\b2 -> Right (a2, b2)) (Core_.term cx b)) (Core_.name cx a))
        _ -> (Left (Util_.DecodingError "expected pair"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v2)
      _ -> (Left (Util_.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "bindings") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.TopologicalSortBindingsTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

topologicalSortTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TopologicalSortTestCase)
topologicalSortTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\adjacencyList -> Eithers.either (\err -> Left err) (\expected -> Right (Testing.TopologicalSortTestCase {
      Testing.topologicalSortTestCaseAdjacencyList = adjacencyList,
      Testing.topologicalSortTestCaseExpected = expected})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "expected",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermEither v2 -> (Eithers.either (Eithers.map (\x -> Left x)) (Eithers.map (\x -> Right x)) (Eithers.bimap (\raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
        Core.TermList v3 -> (Eithers.mapList (\raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
          Core.TermList v4 -> (Eithers.mapList (\raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
            Core.TermLiteral v5 -> ((\x -> case x of
              Core.LiteralInteger v6 -> ((\x -> case x of
                Core.IntegerValueInt32 v7 -> (Right v7)
                _ -> (Left (Util_.DecodingError "expected int32 value"))) v6)
              _ -> (Left (Util_.DecodingError "expected int32 literal"))) v5)
            _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v4)
          _ -> (Left (Util_.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v3)
        _ -> (Left (Util_.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) (\raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
        Core.TermList v3 -> (Eithers.mapList (\raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
          Core.TermLiteral v4 -> ((\x -> case x of
            Core.LiteralInteger v5 -> ((\x -> case x of
              Core.IntegerValueInt32 v6 -> (Right v6)
              _ -> (Left (Util_.DecodingError "expected int32 value"))) v5)
            _ -> (Left (Util_.DecodingError "expected int32 literal"))) v4)
          _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v3)
        _ -> (Left (Util_.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v2))
      _ -> (Left (Util_.DecodingError "expected either value"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "expected") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "adjacencyList",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (\raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
        Core.TermPair v3 ->  
          let a = (Pairs.first v3) 
              b = (Pairs.second v3)
          in (Eithers.either (\err -> Left err) (\a2 -> Eithers.either (\err2 -> Left err2) (\b2 -> Right (a2, b2)) (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
            Core.TermList v4 -> (Eithers.mapList (\raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
              Core.TermLiteral v5 -> ((\x -> case x of
                Core.LiteralInteger v6 -> ((\x -> case x of
                  Core.IntegerValueInt32 v7 -> (Right v7)
                  _ -> (Left (Util_.DecodingError "expected int32 value"))) v6)
                _ -> (Left (Util_.DecodingError "expected int32 literal"))) v5)
              _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v4)
            _ -> (Left (Util_.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx b))) (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
            Core.TermLiteral v4 -> ((\x -> case x of
              Core.LiteralInteger v5 -> ((\x -> case x of
                Core.IntegerValueInt32 v6 -> (Right v6)
                _ -> (Left (Util_.DecodingError "expected int32 value"))) v5)
              _ -> (Left (Util_.DecodingError "expected int32 literal"))) v4)
            _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx a)))
        _ -> (Left (Util_.DecodingError "expected pair"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v2)
      _ -> (Left (Util_.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "adjacencyList") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.TopologicalSortTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

topologicalSortSCCTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TopologicalSortSCCTestCase)
topologicalSortSCCTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\adjacencyList -> Eithers.either (\err -> Left err) (\expected -> Right (Testing.TopologicalSortSCCTestCase {
      Testing.topologicalSortSCCTestCaseAdjacencyList = adjacencyList,
      Testing.topologicalSortSCCTestCaseExpected = expected})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "expected",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (\raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
        Core.TermList v3 -> (Eithers.mapList (\raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
          Core.TermLiteral v4 -> ((\x -> case x of
            Core.LiteralInteger v5 -> ((\x -> case x of
              Core.IntegerValueInt32 v6 -> (Right v6)
              _ -> (Left (Util_.DecodingError "expected int32 value"))) v5)
            _ -> (Left (Util_.DecodingError "expected int32 literal"))) v4)
          _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v3)
        _ -> (Left (Util_.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v2)
      _ -> (Left (Util_.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "expected") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "adjacencyList",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermList v2 -> (Eithers.mapList (\raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
        Core.TermPair v3 ->  
          let a = (Pairs.first v3) 
              b = (Pairs.second v3)
          in (Eithers.either (\err -> Left err) (\a2 -> Eithers.either (\err2 -> Left err2) (\b2 -> Right (a2, b2)) (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
            Core.TermList v4 -> (Eithers.mapList (\raw -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
              Core.TermLiteral v5 -> ((\x -> case x of
                Core.LiteralInteger v6 -> ((\x -> case x of
                  Core.IntegerValueInt32 v7 -> (Right v7)
                  _ -> (Left (Util_.DecodingError "expected int32 value"))) v6)
                _ -> (Left (Util_.DecodingError "expected int32 literal"))) v5)
              _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v4)
            _ -> (Left (Util_.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx b))) (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
            Core.TermLiteral v4 -> ((\x -> case x of
              Core.LiteralInteger v5 -> ((\x -> case x of
                Core.IntegerValueInt32 v6 -> (Right v6)
                _ -> (Left (Util_.DecodingError "expected int32 value"))) v5)
              _ -> (Left (Util_.DecodingError "expected int32 literal"))) v4)
            _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx a)))
        _ -> (Left (Util_.DecodingError "expected pair"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) v2)
      _ -> (Left (Util_.DecodingError "expected list"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "adjacencyList") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.TopologicalSortSCCTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

serializationTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.SerializationTestCase)
serializationTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\output -> Right (Testing.SerializationTestCase {
      Testing.serializationTestCaseInput = input,
      Testing.serializationTestCaseOutput = output})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "output",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "output") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Ast.expr cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.SerializationTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

simplifyTermTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.SimplifyTermTestCase)
simplifyTermTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\output -> Right (Testing.SimplifyTermTestCase {
      Testing.simplifyTermTestCaseInput = input,
      Testing.simplifyTermTestCaseOutput = output})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "output",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "output") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.SimplifyTermTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

normalizeTypeVariablesTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.NormalizeTypeVariablesTestCase)
normalizeTypeVariablesTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\output -> Right (Testing.NormalizeTypeVariablesTestCase {
      Testing.normalizeTypeVariablesTestCaseInput = input,
      Testing.normalizeTypeVariablesTestCaseOutput = output})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "output",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "output") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Core_.term cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.NormalizeTypeVariablesTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

typeReductionTestCase :: (Graph.Graph -> Core.Term -> Either Util_.DecodingError Testing.TypeReductionTestCase)
typeReductionTestCase cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\output -> Right (Testing.TypeReductionTestCase {
      Testing.typeReductionTestCaseInput = input,
      Testing.typeReductionTestCaseOutput = output})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "output",
      " in record"]))) (\fieldTerm -> Core_.type_ cx fieldTerm) (Maps.lookup (Core.Name "output") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> Core_.type_ cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.TypeReductionTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

writerTestCase :: ((Graph.Graph -> Core.Term -> Either Util_.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util_.DecodingError (Testing.WriterTestCase t0))
writerTestCase a cx raw = (Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Maps.fromList (Lists.map (\f -> (Core.fieldName f, (Core.fieldTerm f))) (Core.recordFields v1)))
    in (Eithers.either (\err -> Left err) (\input -> Eithers.either (\err -> Left err) (\output -> Right (Testing.WriterTestCase {
      Testing.writerTestCaseInput = input,
      Testing.writerTestCaseOutput = output})) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "output",
      " in record"]))) (\fieldTerm -> Eithers.either (\err -> Left (Util_.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralString v3 -> (Right v3)
        _ -> (Left (Util_.DecodingError "expected string literal"))) v2)
      _ -> (Left (Util_.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx fieldTerm)) (Maps.lookup (Core.Name "output") fieldMap))) (Maybes.maybe (Left (Util_.DecodingError (Strings.cat [
      "missing field ",
      "input",
      " in record"]))) (\fieldTerm -> a cx fieldTerm) (Maps.lookup (Core.Name "input") fieldMap)))
  _ -> (Left (Util_.DecodingError "expected record of type hydra.testing.WriterTestCase"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
