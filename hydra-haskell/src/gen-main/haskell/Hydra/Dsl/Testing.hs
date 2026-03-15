-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.testing

module Hydra.Dsl.Testing where

import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Module as Module
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Testing as Testing
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

alphaConversionTestCase :: (Core.Term -> Core.Name -> Core.Name -> Core.Term -> Testing.AlphaConversionTestCase)
alphaConversionTestCase term oldVariable newVariable result = Testing.AlphaConversionTestCase {
  Testing.alphaConversionTestCaseTerm = term,
  Testing.alphaConversionTestCaseOldVariable = oldVariable,
  Testing.alphaConversionTestCaseNewVariable = newVariable,
  Testing.alphaConversionTestCaseResult = result}

alphaConversionTestCaseTerm :: (Testing.AlphaConversionTestCase -> Core.Term)
alphaConversionTestCaseTerm = Testing.alphaConversionTestCaseTerm

alphaConversionTestCaseOldVariable :: (Testing.AlphaConversionTestCase -> Core.Name)
alphaConversionTestCaseOldVariable = Testing.alphaConversionTestCaseOldVariable

alphaConversionTestCaseNewVariable :: (Testing.AlphaConversionTestCase -> Core.Name)
alphaConversionTestCaseNewVariable = Testing.alphaConversionTestCaseNewVariable

alphaConversionTestCaseResult :: (Testing.AlphaConversionTestCase -> Core.Term)
alphaConversionTestCaseResult = Testing.alphaConversionTestCaseResult

alphaConversionTestCaseWithTerm :: (Testing.AlphaConversionTestCase -> Core.Term -> Testing.AlphaConversionTestCase)
alphaConversionTestCaseWithTerm original newVal = Testing.AlphaConversionTestCase {
  Testing.alphaConversionTestCaseTerm = newVal,
  Testing.alphaConversionTestCaseOldVariable = (Testing.alphaConversionTestCaseOldVariable original),
  Testing.alphaConversionTestCaseNewVariable = (Testing.alphaConversionTestCaseNewVariable original),
  Testing.alphaConversionTestCaseResult = (Testing.alphaConversionTestCaseResult original)}

alphaConversionTestCaseWithOldVariable :: (Testing.AlphaConversionTestCase -> Core.Name -> Testing.AlphaConversionTestCase)
alphaConversionTestCaseWithOldVariable original newVal = Testing.AlphaConversionTestCase {
  Testing.alphaConversionTestCaseTerm = (Testing.alphaConversionTestCaseTerm original),
  Testing.alphaConversionTestCaseOldVariable = newVal,
  Testing.alphaConversionTestCaseNewVariable = (Testing.alphaConversionTestCaseNewVariable original),
  Testing.alphaConversionTestCaseResult = (Testing.alphaConversionTestCaseResult original)}

alphaConversionTestCaseWithNewVariable :: (Testing.AlphaConversionTestCase -> Core.Name -> Testing.AlphaConversionTestCase)
alphaConversionTestCaseWithNewVariable original newVal = Testing.AlphaConversionTestCase {
  Testing.alphaConversionTestCaseTerm = (Testing.alphaConversionTestCaseTerm original),
  Testing.alphaConversionTestCaseOldVariable = (Testing.alphaConversionTestCaseOldVariable original),
  Testing.alphaConversionTestCaseNewVariable = newVal,
  Testing.alphaConversionTestCaseResult = (Testing.alphaConversionTestCaseResult original)}

alphaConversionTestCaseWithResult :: (Testing.AlphaConversionTestCase -> Core.Term -> Testing.AlphaConversionTestCase)
alphaConversionTestCaseWithResult original newVal = Testing.AlphaConversionTestCase {
  Testing.alphaConversionTestCaseTerm = (Testing.alphaConversionTestCaseTerm original),
  Testing.alphaConversionTestCaseOldVariable = (Testing.alphaConversionTestCaseOldVariable original),
  Testing.alphaConversionTestCaseNewVariable = (Testing.alphaConversionTestCaseNewVariable original),
  Testing.alphaConversionTestCaseResult = newVal}

evaluationStyleEager :: Testing.EvaluationStyle
evaluationStyleEager = Testing.EvaluationStyleEager

evaluationStyleLazy :: Testing.EvaluationStyle
evaluationStyleLazy = Testing.EvaluationStyleLazy

caseConversionTestCase :: (Util.CaseConvention -> Util.CaseConvention -> String -> String -> Testing.CaseConversionTestCase)
caseConversionTestCase fromConvention toConvention fromString toString = Testing.CaseConversionTestCase {
  Testing.caseConversionTestCaseFromConvention = fromConvention,
  Testing.caseConversionTestCaseToConvention = toConvention,
  Testing.caseConversionTestCaseFromString = fromString,
  Testing.caseConversionTestCaseToString = toString}

caseConversionTestCaseFromConvention :: (Testing.CaseConversionTestCase -> Util.CaseConvention)
caseConversionTestCaseFromConvention = Testing.caseConversionTestCaseFromConvention

caseConversionTestCaseToConvention :: (Testing.CaseConversionTestCase -> Util.CaseConvention)
caseConversionTestCaseToConvention = Testing.caseConversionTestCaseToConvention

caseConversionTestCaseFromString :: (Testing.CaseConversionTestCase -> String)
caseConversionTestCaseFromString = Testing.caseConversionTestCaseFromString

caseConversionTestCaseToString :: (Testing.CaseConversionTestCase -> String)
caseConversionTestCaseToString = Testing.caseConversionTestCaseToString

caseConversionTestCaseWithFromConvention :: (Testing.CaseConversionTestCase -> Util.CaseConvention -> Testing.CaseConversionTestCase)
caseConversionTestCaseWithFromConvention original newVal = Testing.CaseConversionTestCase {
  Testing.caseConversionTestCaseFromConvention = newVal,
  Testing.caseConversionTestCaseToConvention = (Testing.caseConversionTestCaseToConvention original),
  Testing.caseConversionTestCaseFromString = (Testing.caseConversionTestCaseFromString original),
  Testing.caseConversionTestCaseToString = (Testing.caseConversionTestCaseToString original)}

caseConversionTestCaseWithToConvention :: (Testing.CaseConversionTestCase -> Util.CaseConvention -> Testing.CaseConversionTestCase)
caseConversionTestCaseWithToConvention original newVal = Testing.CaseConversionTestCase {
  Testing.caseConversionTestCaseFromConvention = (Testing.caseConversionTestCaseFromConvention original),
  Testing.caseConversionTestCaseToConvention = newVal,
  Testing.caseConversionTestCaseFromString = (Testing.caseConversionTestCaseFromString original),
  Testing.caseConversionTestCaseToString = (Testing.caseConversionTestCaseToString original)}

caseConversionTestCaseWithFromString :: (Testing.CaseConversionTestCase -> String -> Testing.CaseConversionTestCase)
caseConversionTestCaseWithFromString original newVal = Testing.CaseConversionTestCase {
  Testing.caseConversionTestCaseFromConvention = (Testing.caseConversionTestCaseFromConvention original),
  Testing.caseConversionTestCaseToConvention = (Testing.caseConversionTestCaseToConvention original),
  Testing.caseConversionTestCaseFromString = newVal,
  Testing.caseConversionTestCaseToString = (Testing.caseConversionTestCaseToString original)}

caseConversionTestCaseWithToString :: (Testing.CaseConversionTestCase -> String -> Testing.CaseConversionTestCase)
caseConversionTestCaseWithToString original newVal = Testing.CaseConversionTestCase {
  Testing.caseConversionTestCaseFromConvention = (Testing.caseConversionTestCaseFromConvention original),
  Testing.caseConversionTestCaseToConvention = (Testing.caseConversionTestCaseToConvention original),
  Testing.caseConversionTestCaseFromString = (Testing.caseConversionTestCaseFromString original),
  Testing.caseConversionTestCaseToString = newVal}

delegatedEvaluationTestCase :: (Core.Term -> Core.Term -> Testing.DelegatedEvaluationTestCase)
delegatedEvaluationTestCase input output = Testing.DelegatedEvaluationTestCase {
  Testing.delegatedEvaluationTestCaseInput = input,
  Testing.delegatedEvaluationTestCaseOutput = output}

delegatedEvaluationTestCaseInput :: (Testing.DelegatedEvaluationTestCase -> Core.Term)
delegatedEvaluationTestCaseInput = Testing.delegatedEvaluationTestCaseInput

delegatedEvaluationTestCaseOutput :: (Testing.DelegatedEvaluationTestCase -> Core.Term)
delegatedEvaluationTestCaseOutput = Testing.delegatedEvaluationTestCaseOutput

delegatedEvaluationTestCaseWithInput :: (Testing.DelegatedEvaluationTestCase -> Core.Term -> Testing.DelegatedEvaluationTestCase)
delegatedEvaluationTestCaseWithInput original newVal = Testing.DelegatedEvaluationTestCase {
  Testing.delegatedEvaluationTestCaseInput = newVal,
  Testing.delegatedEvaluationTestCaseOutput = (Testing.delegatedEvaluationTestCaseOutput original)}

delegatedEvaluationTestCaseWithOutput :: (Testing.DelegatedEvaluationTestCase -> Core.Term -> Testing.DelegatedEvaluationTestCase)
delegatedEvaluationTestCaseWithOutput original newVal = Testing.DelegatedEvaluationTestCase {
  Testing.delegatedEvaluationTestCaseInput = (Testing.delegatedEvaluationTestCaseInput original),
  Testing.delegatedEvaluationTestCaseOutput = newVal}

etaExpansionTestCase :: (Core.Term -> Core.Term -> Testing.EtaExpansionTestCase)
etaExpansionTestCase input output = Testing.EtaExpansionTestCase {
  Testing.etaExpansionTestCaseInput = input,
  Testing.etaExpansionTestCaseOutput = output}

etaExpansionTestCaseInput :: (Testing.EtaExpansionTestCase -> Core.Term)
etaExpansionTestCaseInput = Testing.etaExpansionTestCaseInput

etaExpansionTestCaseOutput :: (Testing.EtaExpansionTestCase -> Core.Term)
etaExpansionTestCaseOutput = Testing.etaExpansionTestCaseOutput

etaExpansionTestCaseWithInput :: (Testing.EtaExpansionTestCase -> Core.Term -> Testing.EtaExpansionTestCase)
etaExpansionTestCaseWithInput original newVal = Testing.EtaExpansionTestCase {
  Testing.etaExpansionTestCaseInput = newVal,
  Testing.etaExpansionTestCaseOutput = (Testing.etaExpansionTestCaseOutput original)}

etaExpansionTestCaseWithOutput :: (Testing.EtaExpansionTestCase -> Core.Term -> Testing.EtaExpansionTestCase)
etaExpansionTestCaseWithOutput original newVal = Testing.EtaExpansionTestCase {
  Testing.etaExpansionTestCaseInput = (Testing.etaExpansionTestCaseInput original),
  Testing.etaExpansionTestCaseOutput = newVal}

deannotateTermTestCase :: (Core.Term -> Core.Term -> Testing.DeannotateTermTestCase)
deannotateTermTestCase input output = Testing.DeannotateTermTestCase {
  Testing.deannotateTermTestCaseInput = input,
  Testing.deannotateTermTestCaseOutput = output}

deannotateTermTestCaseInput :: (Testing.DeannotateTermTestCase -> Core.Term)
deannotateTermTestCaseInput = Testing.deannotateTermTestCaseInput

deannotateTermTestCaseOutput :: (Testing.DeannotateTermTestCase -> Core.Term)
deannotateTermTestCaseOutput = Testing.deannotateTermTestCaseOutput

deannotateTermTestCaseWithInput :: (Testing.DeannotateTermTestCase -> Core.Term -> Testing.DeannotateTermTestCase)
deannotateTermTestCaseWithInput original newVal = Testing.DeannotateTermTestCase {
  Testing.deannotateTermTestCaseInput = newVal,
  Testing.deannotateTermTestCaseOutput = (Testing.deannotateTermTestCaseOutput original)}

deannotateTermTestCaseWithOutput :: (Testing.DeannotateTermTestCase -> Core.Term -> Testing.DeannotateTermTestCase)
deannotateTermTestCaseWithOutput original newVal = Testing.DeannotateTermTestCase {
  Testing.deannotateTermTestCaseInput = (Testing.deannotateTermTestCaseInput original),
  Testing.deannotateTermTestCaseOutput = newVal}

deannotateTypeTestCase :: (Core.Type -> Core.Type -> Testing.DeannotateTypeTestCase)
deannotateTypeTestCase input output = Testing.DeannotateTypeTestCase {
  Testing.deannotateTypeTestCaseInput = input,
  Testing.deannotateTypeTestCaseOutput = output}

deannotateTypeTestCaseInput :: (Testing.DeannotateTypeTestCase -> Core.Type)
deannotateTypeTestCaseInput = Testing.deannotateTypeTestCaseInput

deannotateTypeTestCaseOutput :: (Testing.DeannotateTypeTestCase -> Core.Type)
deannotateTypeTestCaseOutput = Testing.deannotateTypeTestCaseOutput

deannotateTypeTestCaseWithInput :: (Testing.DeannotateTypeTestCase -> Core.Type -> Testing.DeannotateTypeTestCase)
deannotateTypeTestCaseWithInput original newVal = Testing.DeannotateTypeTestCase {
  Testing.deannotateTypeTestCaseInput = newVal,
  Testing.deannotateTypeTestCaseOutput = (Testing.deannotateTypeTestCaseOutput original)}

deannotateTypeTestCaseWithOutput :: (Testing.DeannotateTypeTestCase -> Core.Type -> Testing.DeannotateTypeTestCase)
deannotateTypeTestCaseWithOutput original newVal = Testing.DeannotateTypeTestCase {
  Testing.deannotateTypeTestCaseInput = (Testing.deannotateTypeTestCaseInput original),
  Testing.deannotateTypeTestCaseOutput = newVal}

flattenLetTermsTestCase :: (Core.Term -> Core.Term -> Testing.FlattenLetTermsTestCase)
flattenLetTermsTestCase input output = Testing.FlattenLetTermsTestCase {
  Testing.flattenLetTermsTestCaseInput = input,
  Testing.flattenLetTermsTestCaseOutput = output}

flattenLetTermsTestCaseInput :: (Testing.FlattenLetTermsTestCase -> Core.Term)
flattenLetTermsTestCaseInput = Testing.flattenLetTermsTestCaseInput

flattenLetTermsTestCaseOutput :: (Testing.FlattenLetTermsTestCase -> Core.Term)
flattenLetTermsTestCaseOutput = Testing.flattenLetTermsTestCaseOutput

flattenLetTermsTestCaseWithInput :: (Testing.FlattenLetTermsTestCase -> Core.Term -> Testing.FlattenLetTermsTestCase)
flattenLetTermsTestCaseWithInput original newVal = Testing.FlattenLetTermsTestCase {
  Testing.flattenLetTermsTestCaseInput = newVal,
  Testing.flattenLetTermsTestCaseOutput = (Testing.flattenLetTermsTestCaseOutput original)}

flattenLetTermsTestCaseWithOutput :: (Testing.FlattenLetTermsTestCase -> Core.Term -> Testing.FlattenLetTermsTestCase)
flattenLetTermsTestCaseWithOutput original newVal = Testing.FlattenLetTermsTestCase {
  Testing.flattenLetTermsTestCaseInput = (Testing.flattenLetTermsTestCaseInput original),
  Testing.flattenLetTermsTestCaseOutput = newVal}

foldOperationSumInt32Literals :: Testing.FoldOperation
foldOperationSumInt32Literals = Testing.FoldOperationSumInt32Literals

foldOperationCollectListLengths :: Testing.FoldOperation
foldOperationCollectListLengths = Testing.FoldOperationCollectListLengths

foldOperationCollectLabels :: Testing.FoldOperation
foldOperationCollectLabels = Testing.FoldOperationCollectLabels

foldOverTermTestCase :: (Core.Term -> Coders.TraversalOrder -> Testing.FoldOperation -> Core.Term -> Testing.FoldOverTermTestCase)
foldOverTermTestCase input traversalOrder operation output = Testing.FoldOverTermTestCase {
  Testing.foldOverTermTestCaseInput = input,
  Testing.foldOverTermTestCaseTraversalOrder = traversalOrder,
  Testing.foldOverTermTestCaseOperation = operation,
  Testing.foldOverTermTestCaseOutput = output}

foldOverTermTestCaseInput :: (Testing.FoldOverTermTestCase -> Core.Term)
foldOverTermTestCaseInput = Testing.foldOverTermTestCaseInput

foldOverTermTestCaseTraversalOrder :: (Testing.FoldOverTermTestCase -> Coders.TraversalOrder)
foldOverTermTestCaseTraversalOrder = Testing.foldOverTermTestCaseTraversalOrder

foldOverTermTestCaseOperation :: (Testing.FoldOverTermTestCase -> Testing.FoldOperation)
foldOverTermTestCaseOperation = Testing.foldOverTermTestCaseOperation

foldOverTermTestCaseOutput :: (Testing.FoldOverTermTestCase -> Core.Term)
foldOverTermTestCaseOutput = Testing.foldOverTermTestCaseOutput

foldOverTermTestCaseWithInput :: (Testing.FoldOverTermTestCase -> Core.Term -> Testing.FoldOverTermTestCase)
foldOverTermTestCaseWithInput original newVal = Testing.FoldOverTermTestCase {
  Testing.foldOverTermTestCaseInput = newVal,
  Testing.foldOverTermTestCaseTraversalOrder = (Testing.foldOverTermTestCaseTraversalOrder original),
  Testing.foldOverTermTestCaseOperation = (Testing.foldOverTermTestCaseOperation original),
  Testing.foldOverTermTestCaseOutput = (Testing.foldOverTermTestCaseOutput original)}

foldOverTermTestCaseWithTraversalOrder :: (Testing.FoldOverTermTestCase -> Coders.TraversalOrder -> Testing.FoldOverTermTestCase)
foldOverTermTestCaseWithTraversalOrder original newVal = Testing.FoldOverTermTestCase {
  Testing.foldOverTermTestCaseInput = (Testing.foldOverTermTestCaseInput original),
  Testing.foldOverTermTestCaseTraversalOrder = newVal,
  Testing.foldOverTermTestCaseOperation = (Testing.foldOverTermTestCaseOperation original),
  Testing.foldOverTermTestCaseOutput = (Testing.foldOverTermTestCaseOutput original)}

foldOverTermTestCaseWithOperation :: (Testing.FoldOverTermTestCase -> Testing.FoldOperation -> Testing.FoldOverTermTestCase)
foldOverTermTestCaseWithOperation original newVal = Testing.FoldOverTermTestCase {
  Testing.foldOverTermTestCaseInput = (Testing.foldOverTermTestCaseInput original),
  Testing.foldOverTermTestCaseTraversalOrder = (Testing.foldOverTermTestCaseTraversalOrder original),
  Testing.foldOverTermTestCaseOperation = newVal,
  Testing.foldOverTermTestCaseOutput = (Testing.foldOverTermTestCaseOutput original)}

foldOverTermTestCaseWithOutput :: (Testing.FoldOverTermTestCase -> Core.Term -> Testing.FoldOverTermTestCase)
foldOverTermTestCaseWithOutput original newVal = Testing.FoldOverTermTestCase {
  Testing.foldOverTermTestCaseInput = (Testing.foldOverTermTestCaseInput original),
  Testing.foldOverTermTestCaseTraversalOrder = (Testing.foldOverTermTestCaseTraversalOrder original),
  Testing.foldOverTermTestCaseOperation = (Testing.foldOverTermTestCaseOperation original),
  Testing.foldOverTermTestCaseOutput = newVal}

freeVariablesTestCase :: (Core.Term -> S.Set Core.Name -> Testing.FreeVariablesTestCase)
freeVariablesTestCase input output = Testing.FreeVariablesTestCase {
  Testing.freeVariablesTestCaseInput = input,
  Testing.freeVariablesTestCaseOutput = output}

freeVariablesTestCaseInput :: (Testing.FreeVariablesTestCase -> Core.Term)
freeVariablesTestCaseInput = Testing.freeVariablesTestCaseInput

freeVariablesTestCaseOutput :: (Testing.FreeVariablesTestCase -> S.Set Core.Name)
freeVariablesTestCaseOutput = Testing.freeVariablesTestCaseOutput

freeVariablesTestCaseWithInput :: (Testing.FreeVariablesTestCase -> Core.Term -> Testing.FreeVariablesTestCase)
freeVariablesTestCaseWithInput original newVal = Testing.FreeVariablesTestCase {
  Testing.freeVariablesTestCaseInput = newVal,
  Testing.freeVariablesTestCaseOutput = (Testing.freeVariablesTestCaseOutput original)}

freeVariablesTestCaseWithOutput :: (Testing.FreeVariablesTestCase -> S.Set Core.Name -> Testing.FreeVariablesTestCase)
freeVariablesTestCaseWithOutput original newVal = Testing.FreeVariablesTestCase {
  Testing.freeVariablesTestCaseInput = (Testing.freeVariablesTestCaseInput original),
  Testing.freeVariablesTestCaseOutput = newVal}

hoistPredicateCaseStatements :: Testing.HoistPredicate
hoistPredicateCaseStatements = Testing.HoistPredicateCaseStatements

hoistPredicateApplications :: Testing.HoistPredicate
hoistPredicateApplications = Testing.HoistPredicateApplications

hoistPredicateLists :: Testing.HoistPredicate
hoistPredicateLists = Testing.HoistPredicateLists

hoistPredicateNothing :: Testing.HoistPredicate
hoistPredicateNothing = Testing.HoistPredicateNothing

hoistLetBindingsTestCase :: (Core.Let -> Core.Let -> Testing.HoistLetBindingsTestCase)
hoistLetBindingsTestCase input output = Testing.HoistLetBindingsTestCase {
  Testing.hoistLetBindingsTestCaseInput = input,
  Testing.hoistLetBindingsTestCaseOutput = output}

hoistLetBindingsTestCaseInput :: (Testing.HoistLetBindingsTestCase -> Core.Let)
hoistLetBindingsTestCaseInput = Testing.hoistLetBindingsTestCaseInput

hoistLetBindingsTestCaseOutput :: (Testing.HoistLetBindingsTestCase -> Core.Let)
hoistLetBindingsTestCaseOutput = Testing.hoistLetBindingsTestCaseOutput

hoistLetBindingsTestCaseWithInput :: (Testing.HoistLetBindingsTestCase -> Core.Let -> Testing.HoistLetBindingsTestCase)
hoistLetBindingsTestCaseWithInput original newVal = Testing.HoistLetBindingsTestCase {
  Testing.hoistLetBindingsTestCaseInput = newVal,
  Testing.hoistLetBindingsTestCaseOutput = (Testing.hoistLetBindingsTestCaseOutput original)}

hoistLetBindingsTestCaseWithOutput :: (Testing.HoistLetBindingsTestCase -> Core.Let -> Testing.HoistLetBindingsTestCase)
hoistLetBindingsTestCaseWithOutput original newVal = Testing.HoistLetBindingsTestCase {
  Testing.hoistLetBindingsTestCaseInput = (Testing.hoistLetBindingsTestCaseInput original),
  Testing.hoistLetBindingsTestCaseOutput = newVal}

hoistPolymorphicLetBindingsTestCase :: (Core.Let -> Core.Let -> Testing.HoistPolymorphicLetBindingsTestCase)
hoistPolymorphicLetBindingsTestCase input output = Testing.HoistPolymorphicLetBindingsTestCase {
  Testing.hoistPolymorphicLetBindingsTestCaseInput = input,
  Testing.hoistPolymorphicLetBindingsTestCaseOutput = output}

hoistPolymorphicLetBindingsTestCaseInput :: (Testing.HoistPolymorphicLetBindingsTestCase -> Core.Let)
hoistPolymorphicLetBindingsTestCaseInput = Testing.hoistPolymorphicLetBindingsTestCaseInput

hoistPolymorphicLetBindingsTestCaseOutput :: (Testing.HoistPolymorphicLetBindingsTestCase -> Core.Let)
hoistPolymorphicLetBindingsTestCaseOutput = Testing.hoistPolymorphicLetBindingsTestCaseOutput

hoistPolymorphicLetBindingsTestCaseWithInput :: (Testing.HoistPolymorphicLetBindingsTestCase -> Core.Let -> Testing.HoistPolymorphicLetBindingsTestCase)
hoistPolymorphicLetBindingsTestCaseWithInput original newVal = Testing.HoistPolymorphicLetBindingsTestCase {
  Testing.hoistPolymorphicLetBindingsTestCaseInput = newVal,
  Testing.hoistPolymorphicLetBindingsTestCaseOutput = (Testing.hoistPolymorphicLetBindingsTestCaseOutput original)}

hoistPolymorphicLetBindingsTestCaseWithOutput :: (Testing.HoistPolymorphicLetBindingsTestCase -> Core.Let -> Testing.HoistPolymorphicLetBindingsTestCase)
hoistPolymorphicLetBindingsTestCaseWithOutput original newVal = Testing.HoistPolymorphicLetBindingsTestCase {
  Testing.hoistPolymorphicLetBindingsTestCaseInput = (Testing.hoistPolymorphicLetBindingsTestCaseInput original),
  Testing.hoistPolymorphicLetBindingsTestCaseOutput = newVal}

hoistSubtermsTestCase :: (Testing.HoistPredicate -> Core.Term -> Core.Term -> Testing.HoistSubtermsTestCase)
hoistSubtermsTestCase predicate input output = Testing.HoistSubtermsTestCase {
  Testing.hoistSubtermsTestCasePredicate = predicate,
  Testing.hoistSubtermsTestCaseInput = input,
  Testing.hoistSubtermsTestCaseOutput = output}

hoistSubtermsTestCasePredicate :: (Testing.HoistSubtermsTestCase -> Testing.HoistPredicate)
hoistSubtermsTestCasePredicate = Testing.hoistSubtermsTestCasePredicate

hoistSubtermsTestCaseInput :: (Testing.HoistSubtermsTestCase -> Core.Term)
hoistSubtermsTestCaseInput = Testing.hoistSubtermsTestCaseInput

hoistSubtermsTestCaseOutput :: (Testing.HoistSubtermsTestCase -> Core.Term)
hoistSubtermsTestCaseOutput = Testing.hoistSubtermsTestCaseOutput

hoistSubtermsTestCaseWithPredicate :: (Testing.HoistSubtermsTestCase -> Testing.HoistPredicate -> Testing.HoistSubtermsTestCase)
hoistSubtermsTestCaseWithPredicate original newVal = Testing.HoistSubtermsTestCase {
  Testing.hoistSubtermsTestCasePredicate = newVal,
  Testing.hoistSubtermsTestCaseInput = (Testing.hoistSubtermsTestCaseInput original),
  Testing.hoistSubtermsTestCaseOutput = (Testing.hoistSubtermsTestCaseOutput original)}

hoistSubtermsTestCaseWithInput :: (Testing.HoistSubtermsTestCase -> Core.Term -> Testing.HoistSubtermsTestCase)
hoistSubtermsTestCaseWithInput original newVal = Testing.HoistSubtermsTestCase {
  Testing.hoistSubtermsTestCasePredicate = (Testing.hoistSubtermsTestCasePredicate original),
  Testing.hoistSubtermsTestCaseInput = newVal,
  Testing.hoistSubtermsTestCaseOutput = (Testing.hoistSubtermsTestCaseOutput original)}

hoistSubtermsTestCaseWithOutput :: (Testing.HoistSubtermsTestCase -> Core.Term -> Testing.HoistSubtermsTestCase)
hoistSubtermsTestCaseWithOutput original newVal = Testing.HoistSubtermsTestCase {
  Testing.hoistSubtermsTestCasePredicate = (Testing.hoistSubtermsTestCasePredicate original),
  Testing.hoistSubtermsTestCaseInput = (Testing.hoistSubtermsTestCaseInput original),
  Testing.hoistSubtermsTestCaseOutput = newVal}

hoistCaseStatementsTestCase :: (Core.Term -> Core.Term -> Testing.HoistCaseStatementsTestCase)
hoistCaseStatementsTestCase input output = Testing.HoistCaseStatementsTestCase {
  Testing.hoistCaseStatementsTestCaseInput = input,
  Testing.hoistCaseStatementsTestCaseOutput = output}

hoistCaseStatementsTestCaseInput :: (Testing.HoistCaseStatementsTestCase -> Core.Term)
hoistCaseStatementsTestCaseInput = Testing.hoistCaseStatementsTestCaseInput

hoistCaseStatementsTestCaseOutput :: (Testing.HoistCaseStatementsTestCase -> Core.Term)
hoistCaseStatementsTestCaseOutput = Testing.hoistCaseStatementsTestCaseOutput

hoistCaseStatementsTestCaseWithInput :: (Testing.HoistCaseStatementsTestCase -> Core.Term -> Testing.HoistCaseStatementsTestCase)
hoistCaseStatementsTestCaseWithInput original newVal = Testing.HoistCaseStatementsTestCase {
  Testing.hoistCaseStatementsTestCaseInput = newVal,
  Testing.hoistCaseStatementsTestCaseOutput = (Testing.hoistCaseStatementsTestCaseOutput original)}

hoistCaseStatementsTestCaseWithOutput :: (Testing.HoistCaseStatementsTestCase -> Core.Term -> Testing.HoistCaseStatementsTestCase)
hoistCaseStatementsTestCaseWithOutput original newVal = Testing.HoistCaseStatementsTestCase {
  Testing.hoistCaseStatementsTestCaseInput = (Testing.hoistCaseStatementsTestCaseInput original),
  Testing.hoistCaseStatementsTestCaseOutput = newVal}

termRewriterReplaceFooWithBar :: Testing.TermRewriter
termRewriterReplaceFooWithBar = Testing.TermRewriterReplaceFooWithBar

termRewriterReplaceInt32WithInt64 :: Testing.TermRewriter
termRewriterReplaceInt32WithInt64 = Testing.TermRewriterReplaceInt32WithInt64

rewriteTermTestCase :: (Core.Term -> Testing.TermRewriter -> Core.Term -> Testing.RewriteTermTestCase)
rewriteTermTestCase input rewriter output = Testing.RewriteTermTestCase {
  Testing.rewriteTermTestCaseInput = input,
  Testing.rewriteTermTestCaseRewriter = rewriter,
  Testing.rewriteTermTestCaseOutput = output}

rewriteTermTestCaseInput :: (Testing.RewriteTermTestCase -> Core.Term)
rewriteTermTestCaseInput = Testing.rewriteTermTestCaseInput

rewriteTermTestCaseRewriter :: (Testing.RewriteTermTestCase -> Testing.TermRewriter)
rewriteTermTestCaseRewriter = Testing.rewriteTermTestCaseRewriter

rewriteTermTestCaseOutput :: (Testing.RewriteTermTestCase -> Core.Term)
rewriteTermTestCaseOutput = Testing.rewriteTermTestCaseOutput

rewriteTermTestCaseWithInput :: (Testing.RewriteTermTestCase -> Core.Term -> Testing.RewriteTermTestCase)
rewriteTermTestCaseWithInput original newVal = Testing.RewriteTermTestCase {
  Testing.rewriteTermTestCaseInput = newVal,
  Testing.rewriteTermTestCaseRewriter = (Testing.rewriteTermTestCaseRewriter original),
  Testing.rewriteTermTestCaseOutput = (Testing.rewriteTermTestCaseOutput original)}

rewriteTermTestCaseWithRewriter :: (Testing.RewriteTermTestCase -> Testing.TermRewriter -> Testing.RewriteTermTestCase)
rewriteTermTestCaseWithRewriter original newVal = Testing.RewriteTermTestCase {
  Testing.rewriteTermTestCaseInput = (Testing.rewriteTermTestCaseInput original),
  Testing.rewriteTermTestCaseRewriter = newVal,
  Testing.rewriteTermTestCaseOutput = (Testing.rewriteTermTestCaseOutput original)}

rewriteTermTestCaseWithOutput :: (Testing.RewriteTermTestCase -> Core.Term -> Testing.RewriteTermTestCase)
rewriteTermTestCaseWithOutput original newVal = Testing.RewriteTermTestCase {
  Testing.rewriteTermTestCaseInput = (Testing.rewriteTermTestCaseInput original),
  Testing.rewriteTermTestCaseRewriter = (Testing.rewriteTermTestCaseRewriter original),
  Testing.rewriteTermTestCaseOutput = newVal}

typeRewriterReplaceStringWithInt32 :: Testing.TypeRewriter
typeRewriterReplaceStringWithInt32 = Testing.TypeRewriterReplaceStringWithInt32

rewriteTypeTestCase :: (Core.Type -> Testing.TypeRewriter -> Core.Type -> Testing.RewriteTypeTestCase)
rewriteTypeTestCase input rewriter output = Testing.RewriteTypeTestCase {
  Testing.rewriteTypeTestCaseInput = input,
  Testing.rewriteTypeTestCaseRewriter = rewriter,
  Testing.rewriteTypeTestCaseOutput = output}

rewriteTypeTestCaseInput :: (Testing.RewriteTypeTestCase -> Core.Type)
rewriteTypeTestCaseInput = Testing.rewriteTypeTestCaseInput

rewriteTypeTestCaseRewriter :: (Testing.RewriteTypeTestCase -> Testing.TypeRewriter)
rewriteTypeTestCaseRewriter = Testing.rewriteTypeTestCaseRewriter

rewriteTypeTestCaseOutput :: (Testing.RewriteTypeTestCase -> Core.Type)
rewriteTypeTestCaseOutput = Testing.rewriteTypeTestCaseOutput

rewriteTypeTestCaseWithInput :: (Testing.RewriteTypeTestCase -> Core.Type -> Testing.RewriteTypeTestCase)
rewriteTypeTestCaseWithInput original newVal = Testing.RewriteTypeTestCase {
  Testing.rewriteTypeTestCaseInput = newVal,
  Testing.rewriteTypeTestCaseRewriter = (Testing.rewriteTypeTestCaseRewriter original),
  Testing.rewriteTypeTestCaseOutput = (Testing.rewriteTypeTestCaseOutput original)}

rewriteTypeTestCaseWithRewriter :: (Testing.RewriteTypeTestCase -> Testing.TypeRewriter -> Testing.RewriteTypeTestCase)
rewriteTypeTestCaseWithRewriter original newVal = Testing.RewriteTypeTestCase {
  Testing.rewriteTypeTestCaseInput = (Testing.rewriteTypeTestCaseInput original),
  Testing.rewriteTypeTestCaseRewriter = newVal,
  Testing.rewriteTypeTestCaseOutput = (Testing.rewriteTypeTestCaseOutput original)}

rewriteTypeTestCaseWithOutput :: (Testing.RewriteTypeTestCase -> Core.Type -> Testing.RewriteTypeTestCase)
rewriteTypeTestCaseWithOutput original newVal = Testing.RewriteTypeTestCase {
  Testing.rewriteTypeTestCaseInput = (Testing.rewriteTypeTestCaseInput original),
  Testing.rewriteTypeTestCaseRewriter = (Testing.rewriteTypeTestCaseRewriter original),
  Testing.rewriteTypeTestCaseOutput = newVal}

evaluationTestCase :: (Testing.EvaluationStyle -> Core.Term -> Core.Term -> Testing.EvaluationTestCase)
evaluationTestCase evaluationStyle input output = Testing.EvaluationTestCase {
  Testing.evaluationTestCaseEvaluationStyle = evaluationStyle,
  Testing.evaluationTestCaseInput = input,
  Testing.evaluationTestCaseOutput = output}

evaluationTestCaseEvaluationStyle :: (Testing.EvaluationTestCase -> Testing.EvaluationStyle)
evaluationTestCaseEvaluationStyle = Testing.evaluationTestCaseEvaluationStyle

evaluationTestCaseInput :: (Testing.EvaluationTestCase -> Core.Term)
evaluationTestCaseInput = Testing.evaluationTestCaseInput

evaluationTestCaseOutput :: (Testing.EvaluationTestCase -> Core.Term)
evaluationTestCaseOutput = Testing.evaluationTestCaseOutput

evaluationTestCaseWithEvaluationStyle :: (Testing.EvaluationTestCase -> Testing.EvaluationStyle -> Testing.EvaluationTestCase)
evaluationTestCaseWithEvaluationStyle original newVal = Testing.EvaluationTestCase {
  Testing.evaluationTestCaseEvaluationStyle = newVal,
  Testing.evaluationTestCaseInput = (Testing.evaluationTestCaseInput original),
  Testing.evaluationTestCaseOutput = (Testing.evaluationTestCaseOutput original)}

evaluationTestCaseWithInput :: (Testing.EvaluationTestCase -> Core.Term -> Testing.EvaluationTestCase)
evaluationTestCaseWithInput original newVal = Testing.EvaluationTestCase {
  Testing.evaluationTestCaseEvaluationStyle = (Testing.evaluationTestCaseEvaluationStyle original),
  Testing.evaluationTestCaseInput = newVal,
  Testing.evaluationTestCaseOutput = (Testing.evaluationTestCaseOutput original)}

evaluationTestCaseWithOutput :: (Testing.EvaluationTestCase -> Core.Term -> Testing.EvaluationTestCase)
evaluationTestCaseWithOutput original newVal = Testing.EvaluationTestCase {
  Testing.evaluationTestCaseEvaluationStyle = (Testing.evaluationTestCaseEvaluationStyle original),
  Testing.evaluationTestCaseInput = (Testing.evaluationTestCaseInput original),
  Testing.evaluationTestCaseOutput = newVal}

inferenceFailureTestCase :: (Core.Term -> Testing.InferenceFailureTestCase)
inferenceFailureTestCase input = Testing.InferenceFailureTestCase {
  Testing.inferenceFailureTestCaseInput = input}

inferenceFailureTestCaseInput :: (Testing.InferenceFailureTestCase -> Core.Term)
inferenceFailureTestCaseInput = Testing.inferenceFailureTestCaseInput

inferenceFailureTestCaseWithInput :: (t0 -> Core.Term -> Testing.InferenceFailureTestCase)
inferenceFailureTestCaseWithInput original newVal = Testing.InferenceFailureTestCase {
  Testing.inferenceFailureTestCaseInput = newVal}

inferenceTestCase :: (Core.Term -> Core.TypeScheme -> Testing.InferenceTestCase)
inferenceTestCase input output = Testing.InferenceTestCase {
  Testing.inferenceTestCaseInput = input,
  Testing.inferenceTestCaseOutput = output}

inferenceTestCaseInput :: (Testing.InferenceTestCase -> Core.Term)
inferenceTestCaseInput = Testing.inferenceTestCaseInput

inferenceTestCaseOutput :: (Testing.InferenceTestCase -> Core.TypeScheme)
inferenceTestCaseOutput = Testing.inferenceTestCaseOutput

inferenceTestCaseWithInput :: (Testing.InferenceTestCase -> Core.Term -> Testing.InferenceTestCase)
inferenceTestCaseWithInput original newVal = Testing.InferenceTestCase {
  Testing.inferenceTestCaseInput = newVal,
  Testing.inferenceTestCaseOutput = (Testing.inferenceTestCaseOutput original)}

inferenceTestCaseWithOutput :: (Testing.InferenceTestCase -> Core.TypeScheme -> Testing.InferenceTestCase)
inferenceTestCaseWithOutput original newVal = Testing.InferenceTestCase {
  Testing.inferenceTestCaseInput = (Testing.inferenceTestCaseInput original),
  Testing.inferenceTestCaseOutput = newVal}

jsonDecodeTestCase :: (Core.Type -> Model.Value -> Either String Core.Term -> Testing.JsonDecodeTestCase)
jsonDecodeTestCase type_ json expected = Testing.JsonDecodeTestCase {
  Testing.jsonDecodeTestCaseType = type_,
  Testing.jsonDecodeTestCaseJson = json,
  Testing.jsonDecodeTestCaseExpected = expected}

jsonDecodeTestCaseType :: (Testing.JsonDecodeTestCase -> Core.Type)
jsonDecodeTestCaseType = Testing.jsonDecodeTestCaseType

jsonDecodeTestCaseJson :: (Testing.JsonDecodeTestCase -> Model.Value)
jsonDecodeTestCaseJson = Testing.jsonDecodeTestCaseJson

jsonDecodeTestCaseExpected :: (Testing.JsonDecodeTestCase -> Either String Core.Term)
jsonDecodeTestCaseExpected = Testing.jsonDecodeTestCaseExpected

jsonDecodeTestCaseWithType :: (Testing.JsonDecodeTestCase -> Core.Type -> Testing.JsonDecodeTestCase)
jsonDecodeTestCaseWithType original newVal = Testing.JsonDecodeTestCase {
  Testing.jsonDecodeTestCaseType = newVal,
  Testing.jsonDecodeTestCaseJson = (Testing.jsonDecodeTestCaseJson original),
  Testing.jsonDecodeTestCaseExpected = (Testing.jsonDecodeTestCaseExpected original)}

jsonDecodeTestCaseWithJson :: (Testing.JsonDecodeTestCase -> Model.Value -> Testing.JsonDecodeTestCase)
jsonDecodeTestCaseWithJson original newVal = Testing.JsonDecodeTestCase {
  Testing.jsonDecodeTestCaseType = (Testing.jsonDecodeTestCaseType original),
  Testing.jsonDecodeTestCaseJson = newVal,
  Testing.jsonDecodeTestCaseExpected = (Testing.jsonDecodeTestCaseExpected original)}

jsonDecodeTestCaseWithExpected :: (Testing.JsonDecodeTestCase -> Either String Core.Term -> Testing.JsonDecodeTestCase)
jsonDecodeTestCaseWithExpected original newVal = Testing.JsonDecodeTestCase {
  Testing.jsonDecodeTestCaseType = (Testing.jsonDecodeTestCaseType original),
  Testing.jsonDecodeTestCaseJson = (Testing.jsonDecodeTestCaseJson original),
  Testing.jsonDecodeTestCaseExpected = newVal}

jsonEncodeTestCase :: (Core.Term -> Either String Model.Value -> Testing.JsonEncodeTestCase)
jsonEncodeTestCase term expected = Testing.JsonEncodeTestCase {
  Testing.jsonEncodeTestCaseTerm = term,
  Testing.jsonEncodeTestCaseExpected = expected}

jsonEncodeTestCaseTerm :: (Testing.JsonEncodeTestCase -> Core.Term)
jsonEncodeTestCaseTerm = Testing.jsonEncodeTestCaseTerm

jsonEncodeTestCaseExpected :: (Testing.JsonEncodeTestCase -> Either String Model.Value)
jsonEncodeTestCaseExpected = Testing.jsonEncodeTestCaseExpected

jsonEncodeTestCaseWithTerm :: (Testing.JsonEncodeTestCase -> Core.Term -> Testing.JsonEncodeTestCase)
jsonEncodeTestCaseWithTerm original newVal = Testing.JsonEncodeTestCase {
  Testing.jsonEncodeTestCaseTerm = newVal,
  Testing.jsonEncodeTestCaseExpected = (Testing.jsonEncodeTestCaseExpected original)}

jsonEncodeTestCaseWithExpected :: (Testing.JsonEncodeTestCase -> Either String Model.Value -> Testing.JsonEncodeTestCase)
jsonEncodeTestCaseWithExpected original newVal = Testing.JsonEncodeTestCase {
  Testing.jsonEncodeTestCaseTerm = (Testing.jsonEncodeTestCaseTerm original),
  Testing.jsonEncodeTestCaseExpected = newVal}

jsonRoundtripTestCase :: (Core.Type -> Core.Term -> Testing.JsonRoundtripTestCase)
jsonRoundtripTestCase type_ term = Testing.JsonRoundtripTestCase {
  Testing.jsonRoundtripTestCaseType = type_,
  Testing.jsonRoundtripTestCaseTerm = term}

jsonRoundtripTestCaseType :: (Testing.JsonRoundtripTestCase -> Core.Type)
jsonRoundtripTestCaseType = Testing.jsonRoundtripTestCaseType

jsonRoundtripTestCaseTerm :: (Testing.JsonRoundtripTestCase -> Core.Term)
jsonRoundtripTestCaseTerm = Testing.jsonRoundtripTestCaseTerm

jsonRoundtripTestCaseWithType :: (Testing.JsonRoundtripTestCase -> Core.Type -> Testing.JsonRoundtripTestCase)
jsonRoundtripTestCaseWithType original newVal = Testing.JsonRoundtripTestCase {
  Testing.jsonRoundtripTestCaseType = newVal,
  Testing.jsonRoundtripTestCaseTerm = (Testing.jsonRoundtripTestCaseTerm original)}

jsonRoundtripTestCaseWithTerm :: (Testing.JsonRoundtripTestCase -> Core.Term -> Testing.JsonRoundtripTestCase)
jsonRoundtripTestCaseWithTerm original newVal = Testing.JsonRoundtripTestCase {
  Testing.jsonRoundtripTestCaseType = (Testing.jsonRoundtripTestCaseType original),
  Testing.jsonRoundtripTestCaseTerm = newVal}

liftLambdaAboveLetTestCase :: (Core.Term -> Core.Term -> Testing.LiftLambdaAboveLetTestCase)
liftLambdaAboveLetTestCase input output = Testing.LiftLambdaAboveLetTestCase {
  Testing.liftLambdaAboveLetTestCaseInput = input,
  Testing.liftLambdaAboveLetTestCaseOutput = output}

liftLambdaAboveLetTestCaseInput :: (Testing.LiftLambdaAboveLetTestCase -> Core.Term)
liftLambdaAboveLetTestCaseInput = Testing.liftLambdaAboveLetTestCaseInput

liftLambdaAboveLetTestCaseOutput :: (Testing.LiftLambdaAboveLetTestCase -> Core.Term)
liftLambdaAboveLetTestCaseOutput = Testing.liftLambdaAboveLetTestCaseOutput

liftLambdaAboveLetTestCaseWithInput :: (Testing.LiftLambdaAboveLetTestCase -> Core.Term -> Testing.LiftLambdaAboveLetTestCase)
liftLambdaAboveLetTestCaseWithInput original newVal = Testing.LiftLambdaAboveLetTestCase {
  Testing.liftLambdaAboveLetTestCaseInput = newVal,
  Testing.liftLambdaAboveLetTestCaseOutput = (Testing.liftLambdaAboveLetTestCaseOutput original)}

liftLambdaAboveLetTestCaseWithOutput :: (Testing.LiftLambdaAboveLetTestCase -> Core.Term -> Testing.LiftLambdaAboveLetTestCase)
liftLambdaAboveLetTestCaseWithOutput original newVal = Testing.LiftLambdaAboveLetTestCase {
  Testing.liftLambdaAboveLetTestCaseInput = (Testing.liftLambdaAboveLetTestCaseInput original),
  Testing.liftLambdaAboveLetTestCaseOutput = newVal}

parserTestCase :: (String -> Parsing.ParseResult t0 -> Testing.ParserTestCase t0)
parserTestCase input output = Testing.ParserTestCase {
  Testing.parserTestCaseInput = input,
  Testing.parserTestCaseOutput = output}

parserTestCaseInput :: (Testing.ParserTestCase t0 -> String)
parserTestCaseInput = Testing.parserTestCaseInput

parserTestCaseOutput :: (Testing.ParserTestCase t0 -> Parsing.ParseResult t0)
parserTestCaseOutput = Testing.parserTestCaseOutput

parserTestCaseWithInput :: (Testing.ParserTestCase t0 -> String -> Testing.ParserTestCase t0)
parserTestCaseWithInput original newVal = Testing.ParserTestCase {
  Testing.parserTestCaseInput = newVal,
  Testing.parserTestCaseOutput = (Testing.parserTestCaseOutput original)}

parserTestCaseWithOutput :: (Testing.ParserTestCase t0 -> Parsing.ParseResult t1 -> Testing.ParserTestCase t1)
parserTestCaseWithOutput original newVal = Testing.ParserTestCase {
  Testing.parserTestCaseInput = (Testing.parserTestCaseInput original),
  Testing.parserTestCaseOutput = newVal}

tag :: (String -> Testing.Tag)
tag x = (Testing.Tag x)

unTag :: (Testing.Tag -> String)
unTag = Testing.unTag

testCodec :: (Coders.LanguageName -> Module.FileExtension -> (Core.Term -> Graph.Graph -> Either String String) -> (Core.Type -> Graph.Graph -> Either String String) -> (String -> String) -> (Module.Namespace -> String) -> String -> String -> String -> String -> (S.Set Core.Name -> [String]) -> Testing.TestCodec)
testCodec language fileExtension encodeTerm encodeType formatTestName formatModuleName testCaseTemplate testGroupTemplate moduleTemplate importTemplate findImports = Testing.TestCodec {
  Testing.testCodecLanguage = language,
  Testing.testCodecFileExtension = fileExtension,
  Testing.testCodecEncodeTerm = encodeTerm,
  Testing.testCodecEncodeType = encodeType,
  Testing.testCodecFormatTestName = formatTestName,
  Testing.testCodecFormatModuleName = formatModuleName,
  Testing.testCodecTestCaseTemplate = testCaseTemplate,
  Testing.testCodecTestGroupTemplate = testGroupTemplate,
  Testing.testCodecModuleTemplate = moduleTemplate,
  Testing.testCodecImportTemplate = importTemplate,
  Testing.testCodecFindImports = findImports}

testCodecLanguage :: (Testing.TestCodec -> Coders.LanguageName)
testCodecLanguage = Testing.testCodecLanguage

testCodecFileExtension :: (Testing.TestCodec -> Module.FileExtension)
testCodecFileExtension = Testing.testCodecFileExtension

testCodecEncodeTerm :: (Testing.TestCodec -> Core.Term -> Graph.Graph -> Either String String)
testCodecEncodeTerm = Testing.testCodecEncodeTerm

testCodecEncodeType :: (Testing.TestCodec -> Core.Type -> Graph.Graph -> Either String String)
testCodecEncodeType = Testing.testCodecEncodeType

testCodecFormatTestName :: (Testing.TestCodec -> String -> String)
testCodecFormatTestName = Testing.testCodecFormatTestName

testCodecFormatModuleName :: (Testing.TestCodec -> Module.Namespace -> String)
testCodecFormatModuleName = Testing.testCodecFormatModuleName

testCodecTestCaseTemplate :: (Testing.TestCodec -> String)
testCodecTestCaseTemplate = Testing.testCodecTestCaseTemplate

testCodecTestGroupTemplate :: (Testing.TestCodec -> String)
testCodecTestGroupTemplate = Testing.testCodecTestGroupTemplate

testCodecModuleTemplate :: (Testing.TestCodec -> String)
testCodecModuleTemplate = Testing.testCodecModuleTemplate

testCodecImportTemplate :: (Testing.TestCodec -> String)
testCodecImportTemplate = Testing.testCodecImportTemplate

testCodecFindImports :: (Testing.TestCodec -> S.Set Core.Name -> [String])
testCodecFindImports = Testing.testCodecFindImports

testCodecWithLanguage :: (Testing.TestCodec -> Coders.LanguageName -> Testing.TestCodec)
testCodecWithLanguage original newVal = Testing.TestCodec {
  Testing.testCodecLanguage = newVal,
  Testing.testCodecFileExtension = (Testing.testCodecFileExtension original),
  Testing.testCodecEncodeTerm = (Testing.testCodecEncodeTerm original),
  Testing.testCodecEncodeType = (Testing.testCodecEncodeType original),
  Testing.testCodecFormatTestName = (Testing.testCodecFormatTestName original),
  Testing.testCodecFormatModuleName = (Testing.testCodecFormatModuleName original),
  Testing.testCodecTestCaseTemplate = (Testing.testCodecTestCaseTemplate original),
  Testing.testCodecTestGroupTemplate = (Testing.testCodecTestGroupTemplate original),
  Testing.testCodecModuleTemplate = (Testing.testCodecModuleTemplate original),
  Testing.testCodecImportTemplate = (Testing.testCodecImportTemplate original),
  Testing.testCodecFindImports = (Testing.testCodecFindImports original)}

testCodecWithFileExtension :: (Testing.TestCodec -> Module.FileExtension -> Testing.TestCodec)
testCodecWithFileExtension original newVal = Testing.TestCodec {
  Testing.testCodecLanguage = (Testing.testCodecLanguage original),
  Testing.testCodecFileExtension = newVal,
  Testing.testCodecEncodeTerm = (Testing.testCodecEncodeTerm original),
  Testing.testCodecEncodeType = (Testing.testCodecEncodeType original),
  Testing.testCodecFormatTestName = (Testing.testCodecFormatTestName original),
  Testing.testCodecFormatModuleName = (Testing.testCodecFormatModuleName original),
  Testing.testCodecTestCaseTemplate = (Testing.testCodecTestCaseTemplate original),
  Testing.testCodecTestGroupTemplate = (Testing.testCodecTestGroupTemplate original),
  Testing.testCodecModuleTemplate = (Testing.testCodecModuleTemplate original),
  Testing.testCodecImportTemplate = (Testing.testCodecImportTemplate original),
  Testing.testCodecFindImports = (Testing.testCodecFindImports original)}

testCodecWithEncodeTerm :: (Testing.TestCodec -> (Core.Term -> Graph.Graph -> Either String String) -> Testing.TestCodec)
testCodecWithEncodeTerm original newVal = Testing.TestCodec {
  Testing.testCodecLanguage = (Testing.testCodecLanguage original),
  Testing.testCodecFileExtension = (Testing.testCodecFileExtension original),
  Testing.testCodecEncodeTerm = newVal,
  Testing.testCodecEncodeType = (Testing.testCodecEncodeType original),
  Testing.testCodecFormatTestName = (Testing.testCodecFormatTestName original),
  Testing.testCodecFormatModuleName = (Testing.testCodecFormatModuleName original),
  Testing.testCodecTestCaseTemplate = (Testing.testCodecTestCaseTemplate original),
  Testing.testCodecTestGroupTemplate = (Testing.testCodecTestGroupTemplate original),
  Testing.testCodecModuleTemplate = (Testing.testCodecModuleTemplate original),
  Testing.testCodecImportTemplate = (Testing.testCodecImportTemplate original),
  Testing.testCodecFindImports = (Testing.testCodecFindImports original)}

testCodecWithEncodeType :: (Testing.TestCodec -> (Core.Type -> Graph.Graph -> Either String String) -> Testing.TestCodec)
testCodecWithEncodeType original newVal = Testing.TestCodec {
  Testing.testCodecLanguage = (Testing.testCodecLanguage original),
  Testing.testCodecFileExtension = (Testing.testCodecFileExtension original),
  Testing.testCodecEncodeTerm = (Testing.testCodecEncodeTerm original),
  Testing.testCodecEncodeType = newVal,
  Testing.testCodecFormatTestName = (Testing.testCodecFormatTestName original),
  Testing.testCodecFormatModuleName = (Testing.testCodecFormatModuleName original),
  Testing.testCodecTestCaseTemplate = (Testing.testCodecTestCaseTemplate original),
  Testing.testCodecTestGroupTemplate = (Testing.testCodecTestGroupTemplate original),
  Testing.testCodecModuleTemplate = (Testing.testCodecModuleTemplate original),
  Testing.testCodecImportTemplate = (Testing.testCodecImportTemplate original),
  Testing.testCodecFindImports = (Testing.testCodecFindImports original)}

testCodecWithFormatTestName :: (Testing.TestCodec -> (String -> String) -> Testing.TestCodec)
testCodecWithFormatTestName original newVal = Testing.TestCodec {
  Testing.testCodecLanguage = (Testing.testCodecLanguage original),
  Testing.testCodecFileExtension = (Testing.testCodecFileExtension original),
  Testing.testCodecEncodeTerm = (Testing.testCodecEncodeTerm original),
  Testing.testCodecEncodeType = (Testing.testCodecEncodeType original),
  Testing.testCodecFormatTestName = newVal,
  Testing.testCodecFormatModuleName = (Testing.testCodecFormatModuleName original),
  Testing.testCodecTestCaseTemplate = (Testing.testCodecTestCaseTemplate original),
  Testing.testCodecTestGroupTemplate = (Testing.testCodecTestGroupTemplate original),
  Testing.testCodecModuleTemplate = (Testing.testCodecModuleTemplate original),
  Testing.testCodecImportTemplate = (Testing.testCodecImportTemplate original),
  Testing.testCodecFindImports = (Testing.testCodecFindImports original)}

testCodecWithFormatModuleName :: (Testing.TestCodec -> (Module.Namespace -> String) -> Testing.TestCodec)
testCodecWithFormatModuleName original newVal = Testing.TestCodec {
  Testing.testCodecLanguage = (Testing.testCodecLanguage original),
  Testing.testCodecFileExtension = (Testing.testCodecFileExtension original),
  Testing.testCodecEncodeTerm = (Testing.testCodecEncodeTerm original),
  Testing.testCodecEncodeType = (Testing.testCodecEncodeType original),
  Testing.testCodecFormatTestName = (Testing.testCodecFormatTestName original),
  Testing.testCodecFormatModuleName = newVal,
  Testing.testCodecTestCaseTemplate = (Testing.testCodecTestCaseTemplate original),
  Testing.testCodecTestGroupTemplate = (Testing.testCodecTestGroupTemplate original),
  Testing.testCodecModuleTemplate = (Testing.testCodecModuleTemplate original),
  Testing.testCodecImportTemplate = (Testing.testCodecImportTemplate original),
  Testing.testCodecFindImports = (Testing.testCodecFindImports original)}

testCodecWithTestCaseTemplate :: (Testing.TestCodec -> String -> Testing.TestCodec)
testCodecWithTestCaseTemplate original newVal = Testing.TestCodec {
  Testing.testCodecLanguage = (Testing.testCodecLanguage original),
  Testing.testCodecFileExtension = (Testing.testCodecFileExtension original),
  Testing.testCodecEncodeTerm = (Testing.testCodecEncodeTerm original),
  Testing.testCodecEncodeType = (Testing.testCodecEncodeType original),
  Testing.testCodecFormatTestName = (Testing.testCodecFormatTestName original),
  Testing.testCodecFormatModuleName = (Testing.testCodecFormatModuleName original),
  Testing.testCodecTestCaseTemplate = newVal,
  Testing.testCodecTestGroupTemplate = (Testing.testCodecTestGroupTemplate original),
  Testing.testCodecModuleTemplate = (Testing.testCodecModuleTemplate original),
  Testing.testCodecImportTemplate = (Testing.testCodecImportTemplate original),
  Testing.testCodecFindImports = (Testing.testCodecFindImports original)}

testCodecWithTestGroupTemplate :: (Testing.TestCodec -> String -> Testing.TestCodec)
testCodecWithTestGroupTemplate original newVal = Testing.TestCodec {
  Testing.testCodecLanguage = (Testing.testCodecLanguage original),
  Testing.testCodecFileExtension = (Testing.testCodecFileExtension original),
  Testing.testCodecEncodeTerm = (Testing.testCodecEncodeTerm original),
  Testing.testCodecEncodeType = (Testing.testCodecEncodeType original),
  Testing.testCodecFormatTestName = (Testing.testCodecFormatTestName original),
  Testing.testCodecFormatModuleName = (Testing.testCodecFormatModuleName original),
  Testing.testCodecTestCaseTemplate = (Testing.testCodecTestCaseTemplate original),
  Testing.testCodecTestGroupTemplate = newVal,
  Testing.testCodecModuleTemplate = (Testing.testCodecModuleTemplate original),
  Testing.testCodecImportTemplate = (Testing.testCodecImportTemplate original),
  Testing.testCodecFindImports = (Testing.testCodecFindImports original)}

testCodecWithModuleTemplate :: (Testing.TestCodec -> String -> Testing.TestCodec)
testCodecWithModuleTemplate original newVal = Testing.TestCodec {
  Testing.testCodecLanguage = (Testing.testCodecLanguage original),
  Testing.testCodecFileExtension = (Testing.testCodecFileExtension original),
  Testing.testCodecEncodeTerm = (Testing.testCodecEncodeTerm original),
  Testing.testCodecEncodeType = (Testing.testCodecEncodeType original),
  Testing.testCodecFormatTestName = (Testing.testCodecFormatTestName original),
  Testing.testCodecFormatModuleName = (Testing.testCodecFormatModuleName original),
  Testing.testCodecTestCaseTemplate = (Testing.testCodecTestCaseTemplate original),
  Testing.testCodecTestGroupTemplate = (Testing.testCodecTestGroupTemplate original),
  Testing.testCodecModuleTemplate = newVal,
  Testing.testCodecImportTemplate = (Testing.testCodecImportTemplate original),
  Testing.testCodecFindImports = (Testing.testCodecFindImports original)}

testCodecWithImportTemplate :: (Testing.TestCodec -> String -> Testing.TestCodec)
testCodecWithImportTemplate original newVal = Testing.TestCodec {
  Testing.testCodecLanguage = (Testing.testCodecLanguage original),
  Testing.testCodecFileExtension = (Testing.testCodecFileExtension original),
  Testing.testCodecEncodeTerm = (Testing.testCodecEncodeTerm original),
  Testing.testCodecEncodeType = (Testing.testCodecEncodeType original),
  Testing.testCodecFormatTestName = (Testing.testCodecFormatTestName original),
  Testing.testCodecFormatModuleName = (Testing.testCodecFormatModuleName original),
  Testing.testCodecTestCaseTemplate = (Testing.testCodecTestCaseTemplate original),
  Testing.testCodecTestGroupTemplate = (Testing.testCodecTestGroupTemplate original),
  Testing.testCodecModuleTemplate = (Testing.testCodecModuleTemplate original),
  Testing.testCodecImportTemplate = newVal,
  Testing.testCodecFindImports = (Testing.testCodecFindImports original)}

testCodecWithFindImports :: (Testing.TestCodec -> (S.Set Core.Name -> [String]) -> Testing.TestCodec)
testCodecWithFindImports original newVal = Testing.TestCodec {
  Testing.testCodecLanguage = (Testing.testCodecLanguage original),
  Testing.testCodecFileExtension = (Testing.testCodecFileExtension original),
  Testing.testCodecEncodeTerm = (Testing.testCodecEncodeTerm original),
  Testing.testCodecEncodeType = (Testing.testCodecEncodeType original),
  Testing.testCodecFormatTestName = (Testing.testCodecFormatTestName original),
  Testing.testCodecFormatModuleName = (Testing.testCodecFormatModuleName original),
  Testing.testCodecTestCaseTemplate = (Testing.testCodecTestCaseTemplate original),
  Testing.testCodecTestGroupTemplate = (Testing.testCodecTestGroupTemplate original),
  Testing.testCodecModuleTemplate = (Testing.testCodecModuleTemplate original),
  Testing.testCodecImportTemplate = (Testing.testCodecImportTemplate original),
  Testing.testCodecFindImports = newVal}

testGenerator :: ((Module.Module -> Graph.Graph -> Either String (Module.Namespaces t0)) -> (Module.Namespaces t0 -> Testing.TestCodec) -> (Module.Module -> Testing.TestGroup -> Graph.Graph -> Either String (String, String)) -> Maybe (String -> [Module.Module] -> (String, String)) -> Testing.TestGenerator t0)
testGenerator namespacesForModule createCodec generateTestFile aggregatorFile = Testing.TestGenerator {
  Testing.testGeneratorNamespacesForModule = namespacesForModule,
  Testing.testGeneratorCreateCodec = createCodec,
  Testing.testGeneratorGenerateTestFile = generateTestFile,
  Testing.testGeneratorAggregatorFile = aggregatorFile}

testGeneratorNamespacesForModule :: (Testing.TestGenerator t0 -> Module.Module -> Graph.Graph -> Either String (Module.Namespaces t0))
testGeneratorNamespacesForModule = Testing.testGeneratorNamespacesForModule

testGeneratorCreateCodec :: (Testing.TestGenerator t0 -> Module.Namespaces t0 -> Testing.TestCodec)
testGeneratorCreateCodec = Testing.testGeneratorCreateCodec

testGeneratorGenerateTestFile :: (Testing.TestGenerator t0 -> Module.Module -> Testing.TestGroup -> Graph.Graph -> Either String (String, String))
testGeneratorGenerateTestFile = Testing.testGeneratorGenerateTestFile

testGeneratorAggregatorFile :: (Testing.TestGenerator t0 -> Maybe (String -> [Module.Module] -> (String, String)))
testGeneratorAggregatorFile = Testing.testGeneratorAggregatorFile

testGeneratorWithNamespacesForModule :: (Testing.TestGenerator t0 -> (Module.Module -> Graph.Graph -> Either String (Module.Namespaces t0)) -> Testing.TestGenerator t0)
testGeneratorWithNamespacesForModule original newVal = Testing.TestGenerator {
  Testing.testGeneratorNamespacesForModule = newVal,
  Testing.testGeneratorCreateCodec = (Testing.testGeneratorCreateCodec original),
  Testing.testGeneratorGenerateTestFile = (Testing.testGeneratorGenerateTestFile original),
  Testing.testGeneratorAggregatorFile = (Testing.testGeneratorAggregatorFile original)}

testGeneratorWithCreateCodec :: (Testing.TestGenerator t0 -> (Module.Namespaces t0 -> Testing.TestCodec) -> Testing.TestGenerator t0)
testGeneratorWithCreateCodec original newVal = Testing.TestGenerator {
  Testing.testGeneratorNamespacesForModule = (Testing.testGeneratorNamespacesForModule original),
  Testing.testGeneratorCreateCodec = newVal,
  Testing.testGeneratorGenerateTestFile = (Testing.testGeneratorGenerateTestFile original),
  Testing.testGeneratorAggregatorFile = (Testing.testGeneratorAggregatorFile original)}

testGeneratorWithGenerateTestFile :: (Testing.TestGenerator t0 -> (Module.Module -> Testing.TestGroup -> Graph.Graph -> Either String (String, String)) -> Testing.TestGenerator t0)
testGeneratorWithGenerateTestFile original newVal = Testing.TestGenerator {
  Testing.testGeneratorNamespacesForModule = (Testing.testGeneratorNamespacesForModule original),
  Testing.testGeneratorCreateCodec = (Testing.testGeneratorCreateCodec original),
  Testing.testGeneratorGenerateTestFile = newVal,
  Testing.testGeneratorAggregatorFile = (Testing.testGeneratorAggregatorFile original)}

testGeneratorWithAggregatorFile :: (Testing.TestGenerator t0 -> Maybe (String -> [Module.Module] -> (String, String)) -> Testing.TestGenerator t0)
testGeneratorWithAggregatorFile original newVal = Testing.TestGenerator {
  Testing.testGeneratorNamespacesForModule = (Testing.testGeneratorNamespacesForModule original),
  Testing.testGeneratorCreateCodec = (Testing.testGeneratorCreateCodec original),
  Testing.testGeneratorGenerateTestFile = (Testing.testGeneratorGenerateTestFile original),
  Testing.testGeneratorAggregatorFile = newVal}

testCaseAlphaConversion :: (Testing.AlphaConversionTestCase -> Testing.TestCase)
testCaseAlphaConversion x = (Testing.TestCaseAlphaConversion x)

testCaseCaseConversion :: (Testing.CaseConversionTestCase -> Testing.TestCase)
testCaseCaseConversion x = (Testing.TestCaseCaseConversion x)

testCaseDeannotateTerm :: (Testing.DeannotateTermTestCase -> Testing.TestCase)
testCaseDeannotateTerm x = (Testing.TestCaseDeannotateTerm x)

testCaseDeannotateType :: (Testing.DeannotateTypeTestCase -> Testing.TestCase)
testCaseDeannotateType x = (Testing.TestCaseDeannotateType x)

testCaseDelegatedEvaluation :: (Testing.DelegatedEvaluationTestCase -> Testing.TestCase)
testCaseDelegatedEvaluation x = (Testing.TestCaseDelegatedEvaluation x)

testCaseEtaExpansion :: (Testing.EtaExpansionTestCase -> Testing.TestCase)
testCaseEtaExpansion x = (Testing.TestCaseEtaExpansion x)

testCaseFlattenLetTerms :: (Testing.FlattenLetTermsTestCase -> Testing.TestCase)
testCaseFlattenLetTerms x = (Testing.TestCaseFlattenLetTerms x)

testCaseFreeVariables :: (Testing.FreeVariablesTestCase -> Testing.TestCase)
testCaseFreeVariables x = (Testing.TestCaseFreeVariables x)

testCaseEvaluation :: (Testing.EvaluationTestCase -> Testing.TestCase)
testCaseEvaluation x = (Testing.TestCaseEvaluation x)

testCaseInference :: (Testing.InferenceTestCase -> Testing.TestCase)
testCaseInference x = (Testing.TestCaseInference x)

testCaseInferenceFailure :: (Testing.InferenceFailureTestCase -> Testing.TestCase)
testCaseInferenceFailure x = (Testing.TestCaseInferenceFailure x)

testCaseJsonDecode :: (Testing.JsonDecodeTestCase -> Testing.TestCase)
testCaseJsonDecode x = (Testing.TestCaseJsonDecode x)

testCaseJsonEncode :: (Testing.JsonEncodeTestCase -> Testing.TestCase)
testCaseJsonEncode x = (Testing.TestCaseJsonEncode x)

testCaseJsonParser :: (Testing.JsonParserTestCase -> Testing.TestCase)
testCaseJsonParser x = (Testing.TestCaseJsonParser x)

testCaseJsonRoundtrip :: (Testing.JsonRoundtripTestCase -> Testing.TestCase)
testCaseJsonRoundtrip x = (Testing.TestCaseJsonRoundtrip x)

testCaseJsonWriter :: (Testing.JsonWriterTestCase -> Testing.TestCase)
testCaseJsonWriter x = (Testing.TestCaseJsonWriter x)

testCaseLiftLambdaAboveLet :: (Testing.LiftLambdaAboveLetTestCase -> Testing.TestCase)
testCaseLiftLambdaAboveLet x = (Testing.TestCaseLiftLambdaAboveLet x)

testCaseSerialization :: (Testing.SerializationTestCase -> Testing.TestCase)
testCaseSerialization x = (Testing.TestCaseSerialization x)

testCaseSimplifyTerm :: (Testing.SimplifyTermTestCase -> Testing.TestCase)
testCaseSimplifyTerm x = (Testing.TestCaseSimplifyTerm x)

testCaseTopologicalSort :: (Testing.TopologicalSortTestCase -> Testing.TestCase)
testCaseTopologicalSort x = (Testing.TestCaseTopologicalSort x)

testCaseTopologicalSortBindings :: (Testing.TopologicalSortBindingsTestCase -> Testing.TestCase)
testCaseTopologicalSortBindings x = (Testing.TestCaseTopologicalSortBindings x)

testCaseTopologicalSortSCC :: (Testing.TopologicalSortSCCTestCase -> Testing.TestCase)
testCaseTopologicalSortSCC x = (Testing.TestCaseTopologicalSortSCC x)

testCaseTypeChecking :: (Testing.TypeCheckingTestCase -> Testing.TestCase)
testCaseTypeChecking x = (Testing.TestCaseTypeChecking x)

testCaseTypeCheckingFailure :: (Testing.TypeCheckingFailureTestCase -> Testing.TestCase)
testCaseTypeCheckingFailure x = (Testing.TestCaseTypeCheckingFailure x)

testCaseTypeReduction :: (Testing.TypeReductionTestCase -> Testing.TestCase)
testCaseTypeReduction x = (Testing.TestCaseTypeReduction x)

testCaseNormalizeTypeVariables :: (Testing.NormalizeTypeVariablesTestCase -> Testing.TestCase)
testCaseNormalizeTypeVariables x = (Testing.TestCaseNormalizeTypeVariables x)

testCaseFoldOverTerm :: (Testing.FoldOverTermTestCase -> Testing.TestCase)
testCaseFoldOverTerm x = (Testing.TestCaseFoldOverTerm x)

testCaseRewriteTerm :: (Testing.RewriteTermTestCase -> Testing.TestCase)
testCaseRewriteTerm x = (Testing.TestCaseRewriteTerm x)

testCaseRewriteType :: (Testing.RewriteTypeTestCase -> Testing.TestCase)
testCaseRewriteType x = (Testing.TestCaseRewriteType x)

testCaseHoistSubterms :: (Testing.HoistSubtermsTestCase -> Testing.TestCase)
testCaseHoistSubterms x = (Testing.TestCaseHoistSubterms x)

testCaseHoistCaseStatements :: (Testing.HoistCaseStatementsTestCase -> Testing.TestCase)
testCaseHoistCaseStatements x = (Testing.TestCaseHoistCaseStatements x)

testCaseHoistLetBindings :: (Testing.HoistLetBindingsTestCase -> Testing.TestCase)
testCaseHoistLetBindings x = (Testing.TestCaseHoistLetBindings x)

testCaseHoistPolymorphicLetBindings :: (Testing.HoistPolymorphicLetBindingsTestCase -> Testing.TestCase)
testCaseHoistPolymorphicLetBindings x = (Testing.TestCaseHoistPolymorphicLetBindings x)

testCaseSubstInType :: (Testing.SubstInTypeTestCase -> Testing.TestCase)
testCaseSubstInType x = (Testing.TestCaseSubstInType x)

testCaseVariableOccursInType :: (Testing.VariableOccursInTypeTestCase -> Testing.TestCase)
testCaseVariableOccursInType x = (Testing.TestCaseVariableOccursInType x)

testCaseUnifyTypes :: (Testing.UnifyTypesTestCase -> Testing.TestCase)
testCaseUnifyTypes x = (Testing.TestCaseUnifyTypes x)

testCaseJoinTypes :: (Testing.JoinTypesTestCase -> Testing.TestCase)
testCaseJoinTypes x = (Testing.TestCaseJoinTypes x)

testCaseUnshadowVariables :: (Testing.UnshadowVariablesTestCase -> Testing.TestCase)
testCaseUnshadowVariables x = (Testing.TestCaseUnshadowVariables x)

testCaseWithMetadata :: (String -> Testing.TestCase -> Maybe String -> [Testing.Tag] -> Testing.TestCaseWithMetadata)
testCaseWithMetadata name case_ description tags = Testing.TestCaseWithMetadata {
  Testing.testCaseWithMetadataName = name,
  Testing.testCaseWithMetadataCase = case_,
  Testing.testCaseWithMetadataDescription = description,
  Testing.testCaseWithMetadataTags = tags}

testCaseWithMetadataName :: (Testing.TestCaseWithMetadata -> String)
testCaseWithMetadataName = Testing.testCaseWithMetadataName

testCaseWithMetadataCase :: (Testing.TestCaseWithMetadata -> Testing.TestCase)
testCaseWithMetadataCase = Testing.testCaseWithMetadataCase

testCaseWithMetadataDescription :: (Testing.TestCaseWithMetadata -> Maybe String)
testCaseWithMetadataDescription = Testing.testCaseWithMetadataDescription

testCaseWithMetadataTags :: (Testing.TestCaseWithMetadata -> [Testing.Tag])
testCaseWithMetadataTags = Testing.testCaseWithMetadataTags

testCaseWithMetadataWithName :: (Testing.TestCaseWithMetadata -> String -> Testing.TestCaseWithMetadata)
testCaseWithMetadataWithName original newVal = Testing.TestCaseWithMetadata {
  Testing.testCaseWithMetadataName = newVal,
  Testing.testCaseWithMetadataCase = (Testing.testCaseWithMetadataCase original),
  Testing.testCaseWithMetadataDescription = (Testing.testCaseWithMetadataDescription original),
  Testing.testCaseWithMetadataTags = (Testing.testCaseWithMetadataTags original)}

testCaseWithMetadataWithCase :: (Testing.TestCaseWithMetadata -> Testing.TestCase -> Testing.TestCaseWithMetadata)
testCaseWithMetadataWithCase original newVal = Testing.TestCaseWithMetadata {
  Testing.testCaseWithMetadataName = (Testing.testCaseWithMetadataName original),
  Testing.testCaseWithMetadataCase = newVal,
  Testing.testCaseWithMetadataDescription = (Testing.testCaseWithMetadataDescription original),
  Testing.testCaseWithMetadataTags = (Testing.testCaseWithMetadataTags original)}

testCaseWithMetadataWithDescription :: (Testing.TestCaseWithMetadata -> Maybe String -> Testing.TestCaseWithMetadata)
testCaseWithMetadataWithDescription original newVal = Testing.TestCaseWithMetadata {
  Testing.testCaseWithMetadataName = (Testing.testCaseWithMetadataName original),
  Testing.testCaseWithMetadataCase = (Testing.testCaseWithMetadataCase original),
  Testing.testCaseWithMetadataDescription = newVal,
  Testing.testCaseWithMetadataTags = (Testing.testCaseWithMetadataTags original)}

testCaseWithMetadataWithTags :: (Testing.TestCaseWithMetadata -> [Testing.Tag] -> Testing.TestCaseWithMetadata)
testCaseWithMetadataWithTags original newVal = Testing.TestCaseWithMetadata {
  Testing.testCaseWithMetadataName = (Testing.testCaseWithMetadataName original),
  Testing.testCaseWithMetadataCase = (Testing.testCaseWithMetadataCase original),
  Testing.testCaseWithMetadataDescription = (Testing.testCaseWithMetadataDescription original),
  Testing.testCaseWithMetadataTags = newVal}

testGroup :: (String -> Maybe String -> [Testing.TestGroup] -> [Testing.TestCaseWithMetadata] -> Testing.TestGroup)
testGroup name description subgroups cases = Testing.TestGroup {
  Testing.testGroupName = name,
  Testing.testGroupDescription = description,
  Testing.testGroupSubgroups = subgroups,
  Testing.testGroupCases = cases}

testGroupName :: (Testing.TestGroup -> String)
testGroupName = Testing.testGroupName

testGroupDescription :: (Testing.TestGroup -> Maybe String)
testGroupDescription = Testing.testGroupDescription

testGroupSubgroups :: (Testing.TestGroup -> [Testing.TestGroup])
testGroupSubgroups = Testing.testGroupSubgroups

testGroupCases :: (Testing.TestGroup -> [Testing.TestCaseWithMetadata])
testGroupCases = Testing.testGroupCases

testGroupWithName :: (Testing.TestGroup -> String -> Testing.TestGroup)
testGroupWithName original newVal = Testing.TestGroup {
  Testing.testGroupName = newVal,
  Testing.testGroupDescription = (Testing.testGroupDescription original),
  Testing.testGroupSubgroups = (Testing.testGroupSubgroups original),
  Testing.testGroupCases = (Testing.testGroupCases original)}

testGroupWithDescription :: (Testing.TestGroup -> Maybe String -> Testing.TestGroup)
testGroupWithDescription original newVal = Testing.TestGroup {
  Testing.testGroupName = (Testing.testGroupName original),
  Testing.testGroupDescription = newVal,
  Testing.testGroupSubgroups = (Testing.testGroupSubgroups original),
  Testing.testGroupCases = (Testing.testGroupCases original)}

testGroupWithSubgroups :: (Testing.TestGroup -> [Testing.TestGroup] -> Testing.TestGroup)
testGroupWithSubgroups original newVal = Testing.TestGroup {
  Testing.testGroupName = (Testing.testGroupName original),
  Testing.testGroupDescription = (Testing.testGroupDescription original),
  Testing.testGroupSubgroups = newVal,
  Testing.testGroupCases = (Testing.testGroupCases original)}

testGroupWithCases :: (Testing.TestGroup -> [Testing.TestCaseWithMetadata] -> Testing.TestGroup)
testGroupWithCases original newVal = Testing.TestGroup {
  Testing.testGroupName = (Testing.testGroupName original),
  Testing.testGroupDescription = (Testing.testGroupDescription original),
  Testing.testGroupSubgroups = (Testing.testGroupSubgroups original),
  Testing.testGroupCases = newVal}

typeCheckingTestCase :: (Core.Term -> Core.Term -> Core.Type -> Testing.TypeCheckingTestCase)
typeCheckingTestCase input outputTerm outputType = Testing.TypeCheckingTestCase {
  Testing.typeCheckingTestCaseInput = input,
  Testing.typeCheckingTestCaseOutputTerm = outputTerm,
  Testing.typeCheckingTestCaseOutputType = outputType}

typeCheckingTestCaseInput :: (Testing.TypeCheckingTestCase -> Core.Term)
typeCheckingTestCaseInput = Testing.typeCheckingTestCaseInput

typeCheckingTestCaseOutputTerm :: (Testing.TypeCheckingTestCase -> Core.Term)
typeCheckingTestCaseOutputTerm = Testing.typeCheckingTestCaseOutputTerm

typeCheckingTestCaseOutputType :: (Testing.TypeCheckingTestCase -> Core.Type)
typeCheckingTestCaseOutputType = Testing.typeCheckingTestCaseOutputType

typeCheckingTestCaseWithInput :: (Testing.TypeCheckingTestCase -> Core.Term -> Testing.TypeCheckingTestCase)
typeCheckingTestCaseWithInput original newVal = Testing.TypeCheckingTestCase {
  Testing.typeCheckingTestCaseInput = newVal,
  Testing.typeCheckingTestCaseOutputTerm = (Testing.typeCheckingTestCaseOutputTerm original),
  Testing.typeCheckingTestCaseOutputType = (Testing.typeCheckingTestCaseOutputType original)}

typeCheckingTestCaseWithOutputTerm :: (Testing.TypeCheckingTestCase -> Core.Term -> Testing.TypeCheckingTestCase)
typeCheckingTestCaseWithOutputTerm original newVal = Testing.TypeCheckingTestCase {
  Testing.typeCheckingTestCaseInput = (Testing.typeCheckingTestCaseInput original),
  Testing.typeCheckingTestCaseOutputTerm = newVal,
  Testing.typeCheckingTestCaseOutputType = (Testing.typeCheckingTestCaseOutputType original)}

typeCheckingTestCaseWithOutputType :: (Testing.TypeCheckingTestCase -> Core.Type -> Testing.TypeCheckingTestCase)
typeCheckingTestCaseWithOutputType original newVal = Testing.TypeCheckingTestCase {
  Testing.typeCheckingTestCaseInput = (Testing.typeCheckingTestCaseInput original),
  Testing.typeCheckingTestCaseOutputTerm = (Testing.typeCheckingTestCaseOutputTerm original),
  Testing.typeCheckingTestCaseOutputType = newVal}

typeCheckingFailureTestCase :: (Core.Term -> Testing.TypeCheckingFailureTestCase)
typeCheckingFailureTestCase input = Testing.TypeCheckingFailureTestCase {
  Testing.typeCheckingFailureTestCaseInput = input}

typeCheckingFailureTestCaseInput :: (Testing.TypeCheckingFailureTestCase -> Core.Term)
typeCheckingFailureTestCaseInput = Testing.typeCheckingFailureTestCaseInput

typeCheckingFailureTestCaseWithInput :: (t0 -> Core.Term -> Testing.TypeCheckingFailureTestCase)
typeCheckingFailureTestCaseWithInput original newVal = Testing.TypeCheckingFailureTestCase {
  Testing.typeCheckingFailureTestCaseInput = newVal}

topologicalSortBindingsTestCase :: ([(Core.Name, Core.Term)] -> [[(Core.Name, Core.Term)]] -> Testing.TopologicalSortBindingsTestCase)
topologicalSortBindingsTestCase bindings expected = Testing.TopologicalSortBindingsTestCase {
  Testing.topologicalSortBindingsTestCaseBindings = bindings,
  Testing.topologicalSortBindingsTestCaseExpected = expected}

topologicalSortBindingsTestCaseBindings :: (Testing.TopologicalSortBindingsTestCase -> [(Core.Name, Core.Term)])
topologicalSortBindingsTestCaseBindings = Testing.topologicalSortBindingsTestCaseBindings

topologicalSortBindingsTestCaseExpected :: (Testing.TopologicalSortBindingsTestCase -> [[(Core.Name, Core.Term)]])
topologicalSortBindingsTestCaseExpected = Testing.topologicalSortBindingsTestCaseExpected

topologicalSortBindingsTestCaseWithBindings :: (Testing.TopologicalSortBindingsTestCase -> [(Core.Name, Core.Term)] -> Testing.TopologicalSortBindingsTestCase)
topologicalSortBindingsTestCaseWithBindings original newVal = Testing.TopologicalSortBindingsTestCase {
  Testing.topologicalSortBindingsTestCaseBindings = newVal,
  Testing.topologicalSortBindingsTestCaseExpected = (Testing.topologicalSortBindingsTestCaseExpected original)}

topologicalSortBindingsTestCaseWithExpected :: (Testing.TopologicalSortBindingsTestCase -> [[(Core.Name, Core.Term)]] -> Testing.TopologicalSortBindingsTestCase)
topologicalSortBindingsTestCaseWithExpected original newVal = Testing.TopologicalSortBindingsTestCase {
  Testing.topologicalSortBindingsTestCaseBindings = (Testing.topologicalSortBindingsTestCaseBindings original),
  Testing.topologicalSortBindingsTestCaseExpected = newVal}

topologicalSortTestCase :: ([(Int, [Int])] -> Either [[Int]] [Int] -> Testing.TopologicalSortTestCase)
topologicalSortTestCase adjacencyList expected = Testing.TopologicalSortTestCase {
  Testing.topologicalSortTestCaseAdjacencyList = adjacencyList,
  Testing.topologicalSortTestCaseExpected = expected}

topologicalSortTestCaseAdjacencyList :: (Testing.TopologicalSortTestCase -> [(Int, [Int])])
topologicalSortTestCaseAdjacencyList = Testing.topologicalSortTestCaseAdjacencyList

topologicalSortTestCaseExpected :: (Testing.TopologicalSortTestCase -> Either [[Int]] [Int])
topologicalSortTestCaseExpected = Testing.topologicalSortTestCaseExpected

topologicalSortTestCaseWithAdjacencyList :: (Testing.TopologicalSortTestCase -> [(Int, [Int])] -> Testing.TopologicalSortTestCase)
topologicalSortTestCaseWithAdjacencyList original newVal = Testing.TopologicalSortTestCase {
  Testing.topologicalSortTestCaseAdjacencyList = newVal,
  Testing.topologicalSortTestCaseExpected = (Testing.topologicalSortTestCaseExpected original)}

topologicalSortTestCaseWithExpected :: (Testing.TopologicalSortTestCase -> Either [[Int]] [Int] -> Testing.TopologicalSortTestCase)
topologicalSortTestCaseWithExpected original newVal = Testing.TopologicalSortTestCase {
  Testing.topologicalSortTestCaseAdjacencyList = (Testing.topologicalSortTestCaseAdjacencyList original),
  Testing.topologicalSortTestCaseExpected = newVal}

topologicalSortSCCTestCase :: ([(Int, [Int])] -> [[Int]] -> Testing.TopologicalSortSCCTestCase)
topologicalSortSCCTestCase adjacencyList expected = Testing.TopologicalSortSCCTestCase {
  Testing.topologicalSortSCCTestCaseAdjacencyList = adjacencyList,
  Testing.topologicalSortSCCTestCaseExpected = expected}

topologicalSortSCCTestCaseAdjacencyList :: (Testing.TopologicalSortSCCTestCase -> [(Int, [Int])])
topologicalSortSCCTestCaseAdjacencyList = Testing.topologicalSortSCCTestCaseAdjacencyList

topologicalSortSCCTestCaseExpected :: (Testing.TopologicalSortSCCTestCase -> [[Int]])
topologicalSortSCCTestCaseExpected = Testing.topologicalSortSCCTestCaseExpected

topologicalSortSCCTestCaseWithAdjacencyList :: (Testing.TopologicalSortSCCTestCase -> [(Int, [Int])] -> Testing.TopologicalSortSCCTestCase)
topologicalSortSCCTestCaseWithAdjacencyList original newVal = Testing.TopologicalSortSCCTestCase {
  Testing.topologicalSortSCCTestCaseAdjacencyList = newVal,
  Testing.topologicalSortSCCTestCaseExpected = (Testing.topologicalSortSCCTestCaseExpected original)}

topologicalSortSCCTestCaseWithExpected :: (Testing.TopologicalSortSCCTestCase -> [[Int]] -> Testing.TopologicalSortSCCTestCase)
topologicalSortSCCTestCaseWithExpected original newVal = Testing.TopologicalSortSCCTestCase {
  Testing.topologicalSortSCCTestCaseAdjacencyList = (Testing.topologicalSortSCCTestCaseAdjacencyList original),
  Testing.topologicalSortSCCTestCaseExpected = newVal}

serializationTestCase :: (Ast.Expr -> String -> Testing.SerializationTestCase)
serializationTestCase input output = Testing.SerializationTestCase {
  Testing.serializationTestCaseInput = input,
  Testing.serializationTestCaseOutput = output}

serializationTestCaseInput :: (Testing.SerializationTestCase -> Ast.Expr)
serializationTestCaseInput = Testing.serializationTestCaseInput

serializationTestCaseOutput :: (Testing.SerializationTestCase -> String)
serializationTestCaseOutput = Testing.serializationTestCaseOutput

serializationTestCaseWithInput :: (Testing.SerializationTestCase -> Ast.Expr -> Testing.SerializationTestCase)
serializationTestCaseWithInput original newVal = Testing.SerializationTestCase {
  Testing.serializationTestCaseInput = newVal,
  Testing.serializationTestCaseOutput = (Testing.serializationTestCaseOutput original)}

serializationTestCaseWithOutput :: (Testing.SerializationTestCase -> String -> Testing.SerializationTestCase)
serializationTestCaseWithOutput original newVal = Testing.SerializationTestCase {
  Testing.serializationTestCaseInput = (Testing.serializationTestCaseInput original),
  Testing.serializationTestCaseOutput = newVal}

simplifyTermTestCase :: (Core.Term -> Core.Term -> Testing.SimplifyTermTestCase)
simplifyTermTestCase input output = Testing.SimplifyTermTestCase {
  Testing.simplifyTermTestCaseInput = input,
  Testing.simplifyTermTestCaseOutput = output}

simplifyTermTestCaseInput :: (Testing.SimplifyTermTestCase -> Core.Term)
simplifyTermTestCaseInput = Testing.simplifyTermTestCaseInput

simplifyTermTestCaseOutput :: (Testing.SimplifyTermTestCase -> Core.Term)
simplifyTermTestCaseOutput = Testing.simplifyTermTestCaseOutput

simplifyTermTestCaseWithInput :: (Testing.SimplifyTermTestCase -> Core.Term -> Testing.SimplifyTermTestCase)
simplifyTermTestCaseWithInput original newVal = Testing.SimplifyTermTestCase {
  Testing.simplifyTermTestCaseInput = newVal,
  Testing.simplifyTermTestCaseOutput = (Testing.simplifyTermTestCaseOutput original)}

simplifyTermTestCaseWithOutput :: (Testing.SimplifyTermTestCase -> Core.Term -> Testing.SimplifyTermTestCase)
simplifyTermTestCaseWithOutput original newVal = Testing.SimplifyTermTestCase {
  Testing.simplifyTermTestCaseInput = (Testing.simplifyTermTestCaseInput original),
  Testing.simplifyTermTestCaseOutput = newVal}

normalizeTypeVariablesTestCase :: (Core.Term -> Core.Term -> Testing.NormalizeTypeVariablesTestCase)
normalizeTypeVariablesTestCase input output = Testing.NormalizeTypeVariablesTestCase {
  Testing.normalizeTypeVariablesTestCaseInput = input,
  Testing.normalizeTypeVariablesTestCaseOutput = output}

normalizeTypeVariablesTestCaseInput :: (Testing.NormalizeTypeVariablesTestCase -> Core.Term)
normalizeTypeVariablesTestCaseInput = Testing.normalizeTypeVariablesTestCaseInput

normalizeTypeVariablesTestCaseOutput :: (Testing.NormalizeTypeVariablesTestCase -> Core.Term)
normalizeTypeVariablesTestCaseOutput = Testing.normalizeTypeVariablesTestCaseOutput

normalizeTypeVariablesTestCaseWithInput :: (Testing.NormalizeTypeVariablesTestCase -> Core.Term -> Testing.NormalizeTypeVariablesTestCase)
normalizeTypeVariablesTestCaseWithInput original newVal = Testing.NormalizeTypeVariablesTestCase {
  Testing.normalizeTypeVariablesTestCaseInput = newVal,
  Testing.normalizeTypeVariablesTestCaseOutput = (Testing.normalizeTypeVariablesTestCaseOutput original)}

normalizeTypeVariablesTestCaseWithOutput :: (Testing.NormalizeTypeVariablesTestCase -> Core.Term -> Testing.NormalizeTypeVariablesTestCase)
normalizeTypeVariablesTestCaseWithOutput original newVal = Testing.NormalizeTypeVariablesTestCase {
  Testing.normalizeTypeVariablesTestCaseInput = (Testing.normalizeTypeVariablesTestCaseInput original),
  Testing.normalizeTypeVariablesTestCaseOutput = newVal}

typeReductionTestCase :: (Core.Type -> Core.Type -> Testing.TypeReductionTestCase)
typeReductionTestCase input output = Testing.TypeReductionTestCase {
  Testing.typeReductionTestCaseInput = input,
  Testing.typeReductionTestCaseOutput = output}

typeReductionTestCaseInput :: (Testing.TypeReductionTestCase -> Core.Type)
typeReductionTestCaseInput = Testing.typeReductionTestCaseInput

typeReductionTestCaseOutput :: (Testing.TypeReductionTestCase -> Core.Type)
typeReductionTestCaseOutput = Testing.typeReductionTestCaseOutput

typeReductionTestCaseWithInput :: (Testing.TypeReductionTestCase -> Core.Type -> Testing.TypeReductionTestCase)
typeReductionTestCaseWithInput original newVal = Testing.TypeReductionTestCase {
  Testing.typeReductionTestCaseInput = newVal,
  Testing.typeReductionTestCaseOutput = (Testing.typeReductionTestCaseOutput original)}

typeReductionTestCaseWithOutput :: (Testing.TypeReductionTestCase -> Core.Type -> Testing.TypeReductionTestCase)
typeReductionTestCaseWithOutput original newVal = Testing.TypeReductionTestCase {
  Testing.typeReductionTestCaseInput = (Testing.typeReductionTestCaseInput original),
  Testing.typeReductionTestCaseOutput = newVal}

writerTestCase :: (t0 -> String -> Testing.WriterTestCase t0)
writerTestCase input output = Testing.WriterTestCase {
  Testing.writerTestCaseInput = input,
  Testing.writerTestCaseOutput = output}

writerTestCaseInput :: (Testing.WriterTestCase t0 -> t0)
writerTestCaseInput = Testing.writerTestCaseInput

writerTestCaseOutput :: (Testing.WriterTestCase t0 -> String)
writerTestCaseOutput = Testing.writerTestCaseOutput

writerTestCaseWithInput :: (Testing.WriterTestCase t0 -> t1 -> Testing.WriterTestCase t1)
writerTestCaseWithInput original newVal = Testing.WriterTestCase {
  Testing.writerTestCaseInput = newVal,
  Testing.writerTestCaseOutput = (Testing.writerTestCaseOutput original)}

writerTestCaseWithOutput :: (Testing.WriterTestCase t0 -> String -> Testing.WriterTestCase t0)
writerTestCaseWithOutput original newVal = Testing.WriterTestCase {
  Testing.writerTestCaseInput = (Testing.writerTestCaseInput original),
  Testing.writerTestCaseOutput = newVal}

substInTypeTestCase :: ([(Core.Name, Core.Type)] -> Core.Type -> Core.Type -> Testing.SubstInTypeTestCase)
substInTypeTestCase substitution input output = Testing.SubstInTypeTestCase {
  Testing.substInTypeTestCaseSubstitution = substitution,
  Testing.substInTypeTestCaseInput = input,
  Testing.substInTypeTestCaseOutput = output}

substInTypeTestCaseSubstitution :: (Testing.SubstInTypeTestCase -> [(Core.Name, Core.Type)])
substInTypeTestCaseSubstitution = Testing.substInTypeTestCaseSubstitution

substInTypeTestCaseInput :: (Testing.SubstInTypeTestCase -> Core.Type)
substInTypeTestCaseInput = Testing.substInTypeTestCaseInput

substInTypeTestCaseOutput :: (Testing.SubstInTypeTestCase -> Core.Type)
substInTypeTestCaseOutput = Testing.substInTypeTestCaseOutput

substInTypeTestCaseWithSubstitution :: (Testing.SubstInTypeTestCase -> [(Core.Name, Core.Type)] -> Testing.SubstInTypeTestCase)
substInTypeTestCaseWithSubstitution original newVal = Testing.SubstInTypeTestCase {
  Testing.substInTypeTestCaseSubstitution = newVal,
  Testing.substInTypeTestCaseInput = (Testing.substInTypeTestCaseInput original),
  Testing.substInTypeTestCaseOutput = (Testing.substInTypeTestCaseOutput original)}

substInTypeTestCaseWithInput :: (Testing.SubstInTypeTestCase -> Core.Type -> Testing.SubstInTypeTestCase)
substInTypeTestCaseWithInput original newVal = Testing.SubstInTypeTestCase {
  Testing.substInTypeTestCaseSubstitution = (Testing.substInTypeTestCaseSubstitution original),
  Testing.substInTypeTestCaseInput = newVal,
  Testing.substInTypeTestCaseOutput = (Testing.substInTypeTestCaseOutput original)}

substInTypeTestCaseWithOutput :: (Testing.SubstInTypeTestCase -> Core.Type -> Testing.SubstInTypeTestCase)
substInTypeTestCaseWithOutput original newVal = Testing.SubstInTypeTestCase {
  Testing.substInTypeTestCaseSubstitution = (Testing.substInTypeTestCaseSubstitution original),
  Testing.substInTypeTestCaseInput = (Testing.substInTypeTestCaseInput original),
  Testing.substInTypeTestCaseOutput = newVal}

variableOccursInTypeTestCase :: (Core.Name -> Core.Type -> Bool -> Testing.VariableOccursInTypeTestCase)
variableOccursInTypeTestCase variable type_ expected = Testing.VariableOccursInTypeTestCase {
  Testing.variableOccursInTypeTestCaseVariable = variable,
  Testing.variableOccursInTypeTestCaseType = type_,
  Testing.variableOccursInTypeTestCaseExpected = expected}

variableOccursInTypeTestCaseVariable :: (Testing.VariableOccursInTypeTestCase -> Core.Name)
variableOccursInTypeTestCaseVariable = Testing.variableOccursInTypeTestCaseVariable

variableOccursInTypeTestCaseType :: (Testing.VariableOccursInTypeTestCase -> Core.Type)
variableOccursInTypeTestCaseType = Testing.variableOccursInTypeTestCaseType

variableOccursInTypeTestCaseExpected :: (Testing.VariableOccursInTypeTestCase -> Bool)
variableOccursInTypeTestCaseExpected = Testing.variableOccursInTypeTestCaseExpected

variableOccursInTypeTestCaseWithVariable :: (Testing.VariableOccursInTypeTestCase -> Core.Name -> Testing.VariableOccursInTypeTestCase)
variableOccursInTypeTestCaseWithVariable original newVal = Testing.VariableOccursInTypeTestCase {
  Testing.variableOccursInTypeTestCaseVariable = newVal,
  Testing.variableOccursInTypeTestCaseType = (Testing.variableOccursInTypeTestCaseType original),
  Testing.variableOccursInTypeTestCaseExpected = (Testing.variableOccursInTypeTestCaseExpected original)}

variableOccursInTypeTestCaseWithType :: (Testing.VariableOccursInTypeTestCase -> Core.Type -> Testing.VariableOccursInTypeTestCase)
variableOccursInTypeTestCaseWithType original newVal = Testing.VariableOccursInTypeTestCase {
  Testing.variableOccursInTypeTestCaseVariable = (Testing.variableOccursInTypeTestCaseVariable original),
  Testing.variableOccursInTypeTestCaseType = newVal,
  Testing.variableOccursInTypeTestCaseExpected = (Testing.variableOccursInTypeTestCaseExpected original)}

variableOccursInTypeTestCaseWithExpected :: (Testing.VariableOccursInTypeTestCase -> Bool -> Testing.VariableOccursInTypeTestCase)
variableOccursInTypeTestCaseWithExpected original newVal = Testing.VariableOccursInTypeTestCase {
  Testing.variableOccursInTypeTestCaseVariable = (Testing.variableOccursInTypeTestCaseVariable original),
  Testing.variableOccursInTypeTestCaseType = (Testing.variableOccursInTypeTestCaseType original),
  Testing.variableOccursInTypeTestCaseExpected = newVal}

unshadowVariablesTestCase :: (Core.Term -> Core.Term -> Testing.UnshadowVariablesTestCase)
unshadowVariablesTestCase input output = Testing.UnshadowVariablesTestCase {
  Testing.unshadowVariablesTestCaseInput = input,
  Testing.unshadowVariablesTestCaseOutput = output}

unshadowVariablesTestCaseInput :: (Testing.UnshadowVariablesTestCase -> Core.Term)
unshadowVariablesTestCaseInput = Testing.unshadowVariablesTestCaseInput

unshadowVariablesTestCaseOutput :: (Testing.UnshadowVariablesTestCase -> Core.Term)
unshadowVariablesTestCaseOutput = Testing.unshadowVariablesTestCaseOutput

unshadowVariablesTestCaseWithInput :: (Testing.UnshadowVariablesTestCase -> Core.Term -> Testing.UnshadowVariablesTestCase)
unshadowVariablesTestCaseWithInput original newVal = Testing.UnshadowVariablesTestCase {
  Testing.unshadowVariablesTestCaseInput = newVal,
  Testing.unshadowVariablesTestCaseOutput = (Testing.unshadowVariablesTestCaseOutput original)}

unshadowVariablesTestCaseWithOutput :: (Testing.UnshadowVariablesTestCase -> Core.Term -> Testing.UnshadowVariablesTestCase)
unshadowVariablesTestCaseWithOutput original newVal = Testing.UnshadowVariablesTestCase {
  Testing.unshadowVariablesTestCaseInput = (Testing.unshadowVariablesTestCaseInput original),
  Testing.unshadowVariablesTestCaseOutput = newVal}

unifyTypesTestCase :: ([Core.Name] -> Core.Type -> Core.Type -> Either String Typing.TypeSubst -> Testing.UnifyTypesTestCase)
unifyTypesTestCase schemaTypes left right expected = Testing.UnifyTypesTestCase {
  Testing.unifyTypesTestCaseSchemaTypes = schemaTypes,
  Testing.unifyTypesTestCaseLeft = left,
  Testing.unifyTypesTestCaseRight = right,
  Testing.unifyTypesTestCaseExpected = expected}

unifyTypesTestCaseSchemaTypes :: (Testing.UnifyTypesTestCase -> [Core.Name])
unifyTypesTestCaseSchemaTypes = Testing.unifyTypesTestCaseSchemaTypes

unifyTypesTestCaseLeft :: (Testing.UnifyTypesTestCase -> Core.Type)
unifyTypesTestCaseLeft = Testing.unifyTypesTestCaseLeft

unifyTypesTestCaseRight :: (Testing.UnifyTypesTestCase -> Core.Type)
unifyTypesTestCaseRight = Testing.unifyTypesTestCaseRight

unifyTypesTestCaseExpected :: (Testing.UnifyTypesTestCase -> Either String Typing.TypeSubst)
unifyTypesTestCaseExpected = Testing.unifyTypesTestCaseExpected

unifyTypesTestCaseWithSchemaTypes :: (Testing.UnifyTypesTestCase -> [Core.Name] -> Testing.UnifyTypesTestCase)
unifyTypesTestCaseWithSchemaTypes original newVal = Testing.UnifyTypesTestCase {
  Testing.unifyTypesTestCaseSchemaTypes = newVal,
  Testing.unifyTypesTestCaseLeft = (Testing.unifyTypesTestCaseLeft original),
  Testing.unifyTypesTestCaseRight = (Testing.unifyTypesTestCaseRight original),
  Testing.unifyTypesTestCaseExpected = (Testing.unifyTypesTestCaseExpected original)}

unifyTypesTestCaseWithLeft :: (Testing.UnifyTypesTestCase -> Core.Type -> Testing.UnifyTypesTestCase)
unifyTypesTestCaseWithLeft original newVal = Testing.UnifyTypesTestCase {
  Testing.unifyTypesTestCaseSchemaTypes = (Testing.unifyTypesTestCaseSchemaTypes original),
  Testing.unifyTypesTestCaseLeft = newVal,
  Testing.unifyTypesTestCaseRight = (Testing.unifyTypesTestCaseRight original),
  Testing.unifyTypesTestCaseExpected = (Testing.unifyTypesTestCaseExpected original)}

unifyTypesTestCaseWithRight :: (Testing.UnifyTypesTestCase -> Core.Type -> Testing.UnifyTypesTestCase)
unifyTypesTestCaseWithRight original newVal = Testing.UnifyTypesTestCase {
  Testing.unifyTypesTestCaseSchemaTypes = (Testing.unifyTypesTestCaseSchemaTypes original),
  Testing.unifyTypesTestCaseLeft = (Testing.unifyTypesTestCaseLeft original),
  Testing.unifyTypesTestCaseRight = newVal,
  Testing.unifyTypesTestCaseExpected = (Testing.unifyTypesTestCaseExpected original)}

unifyTypesTestCaseWithExpected :: (Testing.UnifyTypesTestCase -> Either String Typing.TypeSubst -> Testing.UnifyTypesTestCase)
unifyTypesTestCaseWithExpected original newVal = Testing.UnifyTypesTestCase {
  Testing.unifyTypesTestCaseSchemaTypes = (Testing.unifyTypesTestCaseSchemaTypes original),
  Testing.unifyTypesTestCaseLeft = (Testing.unifyTypesTestCaseLeft original),
  Testing.unifyTypesTestCaseRight = (Testing.unifyTypesTestCaseRight original),
  Testing.unifyTypesTestCaseExpected = newVal}

joinTypesTestCase :: (Core.Type -> Core.Type -> Either () [Typing.TypeConstraint] -> Testing.JoinTypesTestCase)
joinTypesTestCase left right expected = Testing.JoinTypesTestCase {
  Testing.joinTypesTestCaseLeft = left,
  Testing.joinTypesTestCaseRight = right,
  Testing.joinTypesTestCaseExpected = expected}

joinTypesTestCaseLeft :: (Testing.JoinTypesTestCase -> Core.Type)
joinTypesTestCaseLeft = Testing.joinTypesTestCaseLeft

joinTypesTestCaseRight :: (Testing.JoinTypesTestCase -> Core.Type)
joinTypesTestCaseRight = Testing.joinTypesTestCaseRight

joinTypesTestCaseExpected :: (Testing.JoinTypesTestCase -> Either () [Typing.TypeConstraint])
joinTypesTestCaseExpected = Testing.joinTypesTestCaseExpected

joinTypesTestCaseWithLeft :: (Testing.JoinTypesTestCase -> Core.Type -> Testing.JoinTypesTestCase)
joinTypesTestCaseWithLeft original newVal = Testing.JoinTypesTestCase {
  Testing.joinTypesTestCaseLeft = newVal,
  Testing.joinTypesTestCaseRight = (Testing.joinTypesTestCaseRight original),
  Testing.joinTypesTestCaseExpected = (Testing.joinTypesTestCaseExpected original)}

joinTypesTestCaseWithRight :: (Testing.JoinTypesTestCase -> Core.Type -> Testing.JoinTypesTestCase)
joinTypesTestCaseWithRight original newVal = Testing.JoinTypesTestCase {
  Testing.joinTypesTestCaseLeft = (Testing.joinTypesTestCaseLeft original),
  Testing.joinTypesTestCaseRight = newVal,
  Testing.joinTypesTestCaseExpected = (Testing.joinTypesTestCaseExpected original)}

joinTypesTestCaseWithExpected :: (Testing.JoinTypesTestCase -> Either () [Typing.TypeConstraint] -> Testing.JoinTypesTestCase)
joinTypesTestCaseWithExpected original newVal = Testing.JoinTypesTestCase {
  Testing.joinTypesTestCaseLeft = (Testing.joinTypesTestCaseLeft original),
  Testing.joinTypesTestCaseRight = (Testing.joinTypesTestCaseRight original),
  Testing.joinTypesTestCaseExpected = newVal}
