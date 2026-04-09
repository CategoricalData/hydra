// Note: this is an automatically generated file. Do not edit.

package testing

import (
  "hydra.dev/hydra/ast"
  "hydra.dev/hydra/coders"
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/graph"
  jsonmodel "hydra.dev/hydra/json/model"
  hmodule "hydra.dev/hydra/module"
  "hydra.dev/hydra/parsing"
  "hydra.dev/hydra/util"
)

type AlphaConversionTestCase struct {
  Term core.Term
  OldVariable core.Name
  NewVariable core.Name
  Result core.Term
}

type EvaluationStyle interface {
  isEvaluationStyle()
}

type EvaluationStyleEager struct{}

func (EvaluationStyleEager) isEvaluationStyle() {

}

type EvaluationStyleLazy struct{}

func (EvaluationStyleLazy) isEvaluationStyle() {

}

type CaseConversionTestCase struct {
  FromConvention util.CaseConvention
  ToConvention util.CaseConvention
  FromString string
  ToString string
}

type DelegatedEvaluationTestCase struct {
  Input core.Term
  Output core.Term
}

type EtaExpansionTestCase struct {
  Input core.Term
  Output core.Term
}

type DeannotateTermTestCase struct {
  Input core.Term
  Output core.Term
}

type DeannotateTypeTestCase struct {
  Input core.Type
  Output core.Type
}

type FlattenLetTermsTestCase struct {
  Input core.Term
  Output core.Term
}

type FoldOperation interface {
  isFoldOperation()
}

type FoldOperationSumInt32Literals struct{}

func (FoldOperationSumInt32Literals) isFoldOperation() {

}

type FoldOperationCollectListLengths struct{}

func (FoldOperationCollectListLengths) isFoldOperation() {

}

type FoldOperationCollectLabels struct{}

func (FoldOperationCollectLabels) isFoldOperation() {

}

type FoldOverTermTestCase struct {
  Input core.Term
  TraversalOrder coders.TraversalOrder
  Operation FoldOperation
  Output core.Term
}

type FreeVariablesTestCase struct {
  Input core.Term
  Output []any
}

type HoistPredicate interface {
  isHoistPredicate()
}

type HoistPredicateCaseStatements struct{}

func (HoistPredicateCaseStatements) isHoistPredicate() {

}

type HoistPredicateApplications struct{}

func (HoistPredicateApplications) isHoistPredicate() {

}

type HoistPredicateLists struct{}

func (HoistPredicateLists) isHoistPredicate() {

}

type HoistPredicateNothing struct{}

func (HoistPredicateNothing) isHoistPredicate() {

}

type HoistLetBindingsTestCase struct {
  Input core.Let
  Output core.Let
}

type HoistPolymorphicLetBindingsTestCase struct {
  Input core.Let
  Output core.Let
}

type HoistSubtermsTestCase struct {
  Predicate HoistPredicate
  Input core.Term
  Output core.Term
}

type HoistCaseStatementsTestCase struct {
  Input core.Term
  Output core.Term
}

type TermRewriter interface {
  isTermRewriter()
}

type TermRewriterReplaceFooWithBar struct{}

func (TermRewriterReplaceFooWithBar) isTermRewriter() {

}

type TermRewriterReplaceInt32WithInt64 struct{}

func (TermRewriterReplaceInt32WithInt64) isTermRewriter() {

}

type RewriteTermTestCase struct {
  Input core.Term
  Rewriter TermRewriter
  Output core.Term
}

type TypeRewriter interface {
  isTypeRewriter()
}

type TypeRewriterReplaceStringWithInt32 struct{}

func (TypeRewriterReplaceStringWithInt32) isTypeRewriter() {

}

type RewriteTypeTestCase struct {
  Input core.Type
  Rewriter TypeRewriter
  Output core.Type
}

type EvaluationTestCase struct {
  EvaluationStyle EvaluationStyle
  Input core.Term
  Output core.Term
}

type InferenceFailureTestCase struct {
  Input core.Term
}

type InferenceTestCase struct {
  Input core.Term
  Output core.TypeScheme
}

type JsonDecodeTestCase struct {
  Type_ core.Type
  Json jsonmodel.Value
  Expected any
}

type JsonEncodeTestCase struct {
  Term core.Term
  Expected any
}

type JsonParserTestCase = ParserTestCase[jsonmodel.Value]

type JsonRoundtripTestCase struct {
  Type_ core.Type
  Term core.Term
}

type LiftLambdaAboveLetTestCase struct {
  Input core.Term
  Output core.Term
}

type JsonWriterTestCase = WriterTestCase[jsonmodel.Value]

type ParserTestCase [A any] struct {
  Input string
  Output parsing.ParseResult[A]
}

type Tag string

type TestCodec struct {
  Language coders.LanguageName
  FileExtension hmodule.FileExtension
  EncodeTerm func(core.Term) func(graph.Graph) any
  EncodeType func(core.Type) func(graph.Graph) any
  FormatTestName func(string) string
  FormatModuleName func(hmodule.Namespace) string
  TestCaseTemplate string
  TestGroupTemplate string
  ModuleTemplate string
  ImportTemplate string
  FindImports func([]any) []any
}

type TestGenerator [A any] struct {
  NamespacesForModule func(hmodule.Module) func(graph.Graph) any
  CreateCodec func(hmodule.Namespaces[A]) TestCodec
  GenerateTestFile func(hmodule.Module) func(TestGroup) func(graph.Graph) any
  AggregatorFile any
}

type TestCase interface {
  isTestCase()
}

type TestCaseAlphaConversion struct {
  Value AlphaConversionTestCase
}

func (TestCaseAlphaConversion) isTestCase() {

}

type TestCaseCaseConversion struct {
  Value CaseConversionTestCase
}

func (TestCaseCaseConversion) isTestCase() {

}

type TestCaseDeannotateTerm struct {
  Value DeannotateTermTestCase
}

func (TestCaseDeannotateTerm) isTestCase() {

}

type TestCaseDeannotateType struct {
  Value DeannotateTypeTestCase
}

func (TestCaseDeannotateType) isTestCase() {

}

type TestCaseDelegatedEvaluation struct {
  Value DelegatedEvaluationTestCase
}

func (TestCaseDelegatedEvaluation) isTestCase() {

}

type TestCaseEtaExpansion struct {
  Value EtaExpansionTestCase
}

func (TestCaseEtaExpansion) isTestCase() {

}

type TestCaseFlattenLetTerms struct {
  Value FlattenLetTermsTestCase
}

func (TestCaseFlattenLetTerms) isTestCase() {

}

type TestCaseFreeVariables struct {
  Value FreeVariablesTestCase
}

func (TestCaseFreeVariables) isTestCase() {

}

type TestCaseEvaluation struct {
  Value EvaluationTestCase
}

func (TestCaseEvaluation) isTestCase() {

}

type TestCaseInference struct {
  Value InferenceTestCase
}

func (TestCaseInference) isTestCase() {

}

type TestCaseInferenceFailure struct {
  Value InferenceFailureTestCase
}

func (TestCaseInferenceFailure) isTestCase() {

}

type TestCaseJsonDecode struct {
  Value JsonDecodeTestCase
}

func (TestCaseJsonDecode) isTestCase() {

}

type TestCaseJsonEncode struct {
  Value JsonEncodeTestCase
}

func (TestCaseJsonEncode) isTestCase() {

}

type TestCaseJsonParser struct {
  Value JsonParserTestCase
}

func (TestCaseJsonParser) isTestCase() {

}

type TestCaseJsonRoundtrip struct {
  Value JsonRoundtripTestCase
}

func (TestCaseJsonRoundtrip) isTestCase() {

}

type TestCaseJsonWriter struct {
  Value JsonWriterTestCase
}

func (TestCaseJsonWriter) isTestCase() {

}

type TestCaseLiftLambdaAboveLet struct {
  Value LiftLambdaAboveLetTestCase
}

func (TestCaseLiftLambdaAboveLet) isTestCase() {

}

type TestCaseSerialization struct {
  Value SerializationTestCase
}

func (TestCaseSerialization) isTestCase() {

}

type TestCaseSimplifyTerm struct {
  Value SimplifyTermTestCase
}

func (TestCaseSimplifyTerm) isTestCase() {

}

type TestCaseTopologicalSort struct {
  Value TopologicalSortTestCase
}

func (TestCaseTopologicalSort) isTestCase() {

}

type TestCaseTopologicalSortBindings struct {
  Value TopologicalSortBindingsTestCase
}

func (TestCaseTopologicalSortBindings) isTestCase() {

}

type TestCaseTopologicalSortSCC struct {
  Value TopologicalSortSCCTestCase
}

func (TestCaseTopologicalSortSCC) isTestCase() {

}

type TestCaseTypeChecking struct {
  Value TypeCheckingTestCase
}

func (TestCaseTypeChecking) isTestCase() {

}

type TestCaseTypeCheckingFailure struct {
  Value TypeCheckingFailureTestCase
}

func (TestCaseTypeCheckingFailure) isTestCase() {

}

type TestCaseTypeReduction struct {
  Value TypeReductionTestCase
}

func (TestCaseTypeReduction) isTestCase() {

}

type TestCaseNormalizeTypeVariables struct {
  Value NormalizeTypeVariablesTestCase
}

func (TestCaseNormalizeTypeVariables) isTestCase() {

}

type TestCaseFoldOverTerm struct {
  Value FoldOverTermTestCase
}

func (TestCaseFoldOverTerm) isTestCase() {

}

type TestCaseRewriteTerm struct {
  Value RewriteTermTestCase
}

func (TestCaseRewriteTerm) isTestCase() {

}

type TestCaseRewriteType struct {
  Value RewriteTypeTestCase
}

func (TestCaseRewriteType) isTestCase() {

}

type TestCaseHoistSubterms struct {
  Value HoistSubtermsTestCase
}

func (TestCaseHoistSubterms) isTestCase() {

}

type TestCaseHoistCaseStatements struct {
  Value HoistCaseStatementsTestCase
}

func (TestCaseHoistCaseStatements) isTestCase() {

}

type TestCaseHoistLetBindings struct {
  Value HoistLetBindingsTestCase
}

func (TestCaseHoistLetBindings) isTestCase() {

}

type TestCaseHoistPolymorphicLetBindings struct {
  Value HoistPolymorphicLetBindingsTestCase
}

func (TestCaseHoistPolymorphicLetBindings) isTestCase() {

}

type TestCaseSubstInType struct {
  Value SubstInTypeTestCase
}

func (TestCaseSubstInType) isTestCase() {

}

type TestCaseVariableOccursInType struct {
  Value VariableOccursInTypeTestCase
}

func (TestCaseVariableOccursInType) isTestCase() {

}

type TestCaseUnifyTypes struct {
  Value UnifyTypesTestCase
}

func (TestCaseUnifyTypes) isTestCase() {

}

type TestCaseJoinTypes struct {
  Value JoinTypesTestCase
}

func (TestCaseJoinTypes) isTestCase() {

}

type TestCaseUnshadowVariables struct {
  Value UnshadowVariablesTestCase
}

func (TestCaseUnshadowVariables) isTestCase() {

}

type TestCaseWithMetadata struct {
  Name string
  Case_ TestCase
  Description any
  Tags []any
}

type TestGroup struct {
  Name string
  Description any
  Subgroups []any
  Cases []any
}

type TypeCheckingTestCase struct {
  Input core.Term
  OutputTerm core.Term
  OutputType core.Type
}

type TypeCheckingFailureTestCase struct {
  Input core.Term
}

type TopologicalSortBindingsTestCase struct {
  Bindings []any
  Expected []any
}

type TopologicalSortTestCase struct {
  AdjacencyList []any
  Expected any
}

type TopologicalSortSCCTestCase struct {
  AdjacencyList []any
  Expected []any
}

type SerializationTestCase struct {
  Input ast.Expr
  Output string
}

type SimplifyTermTestCase struct {
  Input core.Term
  Output core.Term
}

type NormalizeTypeVariablesTestCase struct {
  Input core.Term
  Output core.Term
}

type TypeReductionTestCase struct {
  Input core.Type
  Output core.Type
}

type WriterTestCase [A any] struct {
  Input A
  Output string
}

type SubstInTypeTestCase struct {
  Substitution []any
  Input core.Type
  Output core.Type
}

type VariableOccursInTypeTestCase struct {
  Variable core.Name
  Type_ core.Type
  Expected bool
}

type UnshadowVariablesTestCase struct {
  Input core.Term
  Output core.Term
}

type UnifyTypesTestCase struct {
  SchemaTypes []any
  Left core.Type
  Right core.Type
  Expected any
}

type JoinTypesTestCase struct {
  Left core.Type
  Right core.Type
  Expected any
}
