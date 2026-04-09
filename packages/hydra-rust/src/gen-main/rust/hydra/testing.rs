#![allow(unused_imports)]
#![allow(non_camel_case_types)]

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use ordered_float::OrderedFloat;
use crate::Either;
use crate::hydra::ast::*;
use crate::hydra::coders::*;
use crate::hydra::graph::*;
use crate::hydra::json::model::*;
use crate::hydra::module::*;
use crate::hydra::parsing::*;
use crate::hydra::typing::*;
use crate::hydra::util::*;
use crate::hydra::core::*;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AlphaConversionTestCase_Variant {
  pub term: crate::hydra::core::Term,
  pub old_variable: crate::hydra::core::Name,
  pub new_variable: crate::hydra::core::Name,
  pub result: crate::hydra::core::Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct AlphaConversionTestCase (pub Rc<AlphaConversionTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum EvaluationStyle_Variant {
  Eager,
  Lazy}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct EvaluationStyle (pub Rc<EvaluationStyle_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct CaseConversionTestCase_Variant {
  pub from_convention: crate::hydra::util::CaseConvention,
  pub to_convention: crate::hydra::util::CaseConvention,
  pub from_string: String,
  pub to_string: String}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct CaseConversionTestCase (pub Rc<CaseConversionTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DelegatedEvaluationTestCase_Variant {
  pub input: crate::hydra::core::Term,
  pub output: crate::hydra::core::Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DelegatedEvaluationTestCase (pub Rc<DelegatedEvaluationTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct EtaExpansionTestCase_Variant {
  pub input: crate::hydra::core::Term,
  pub output: crate::hydra::core::Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct EtaExpansionTestCase (pub Rc<EtaExpansionTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DeannotateTermTestCase_Variant {
  pub input: crate::hydra::core::Term,
  pub output: crate::hydra::core::Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DeannotateTermTestCase (pub Rc<DeannotateTermTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DeannotateTypeTestCase_Variant {
  pub input: crate::hydra::core::Type,
  pub output: crate::hydra::core::Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DeannotateTypeTestCase (pub Rc<DeannotateTypeTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FlattenLetTermsTestCase_Variant {
  pub input: crate::hydra::core::Term,
  pub output: crate::hydra::core::Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FlattenLetTermsTestCase (pub Rc<FlattenLetTermsTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum FoldOperation_Variant {
  SumInt32Literals,
  CollectListLengths,
  CollectLabels}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FoldOperation (pub Rc<FoldOperation_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FoldOverTermTestCase_Variant {
  pub input: crate::hydra::core::Term,
  pub traversal_order: crate::hydra::coders::TraversalOrder,
  pub operation: FoldOperation,
  pub output: crate::hydra::core::Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FoldOverTermTestCase (pub Rc<FoldOverTermTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FreeVariablesTestCase_Variant {
  pub input: crate::hydra::core::Term,
  pub output: BTreeSet<crate::hydra::core::Name>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FreeVariablesTestCase (pub Rc<FreeVariablesTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum HoistPredicate_Variant {
  CaseStatements,
  Applications,
  Lists,
  Nothing}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct HoistPredicate (pub Rc<HoistPredicate_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct HoistLetBindingsTestCase_Variant {
  pub input: crate::hydra::core::Let,
  pub output: crate::hydra::core::Let}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct HoistLetBindingsTestCase (pub Rc<HoistLetBindingsTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct HoistPolymorphicLetBindingsTestCase_Variant {
  pub input: crate::hydra::core::Let,
  pub output: crate::hydra::core::Let}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct HoistPolymorphicLetBindingsTestCase (pub Rc<HoistPolymorphicLetBindingsTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct HoistSubtermsTestCase_Variant {
  pub predicate: HoistPredicate,
  pub input: crate::hydra::core::Term,
  pub output: crate::hydra::core::Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct HoistSubtermsTestCase (pub Rc<HoistSubtermsTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct HoistCaseStatementsTestCase_Variant {
  pub input: crate::hydra::core::Term,
  pub output: crate::hydra::core::Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct HoistCaseStatementsTestCase (pub Rc<HoistCaseStatementsTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TermRewriter_Variant {
  ReplaceFooWithBar,
  ReplaceInt32WithInt64}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TermRewriter (pub Rc<TermRewriter_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RewriteTermTestCase_Variant {
  pub input: crate::hydra::core::Term,
  pub rewriter: TermRewriter,
  pub output: crate::hydra::core::Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RewriteTermTestCase (pub Rc<RewriteTermTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeRewriter_Variant {
  ReplaceStringWithInt32}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeRewriter (pub Rc<TypeRewriter_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RewriteTypeTestCase_Variant {
  pub input: crate::hydra::core::Type,
  pub rewriter: TypeRewriter,
  pub output: crate::hydra::core::Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RewriteTypeTestCase (pub Rc<RewriteTypeTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct EvaluationTestCase_Variant {
  pub evaluation_style: EvaluationStyle,
  pub input: crate::hydra::core::Term,
  pub output: crate::hydra::core::Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct EvaluationTestCase (pub Rc<EvaluationTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct InferenceFailureTestCase_Variant {
  pub input: crate::hydra::core::Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct InferenceFailureTestCase (pub Rc<InferenceFailureTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct InferenceTestCase_Variant {
  pub input: crate::hydra::core::Term,
  pub output: crate::hydra::core::TypeScheme}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct InferenceTestCase (pub Rc<InferenceTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct JsonDecodeTestCase_Variant {
  pub type_: crate::hydra::core::Type,
  pub json: crate::hydra::json::model::Value,
  pub expected: Either<String, crate::hydra::core::Term>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct JsonDecodeTestCase (pub Rc<JsonDecodeTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct JsonEncodeTestCase_Variant {
  pub term: crate::hydra::core::Term,
  pub expected: Either<String, crate::hydra::json::model::Value>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct JsonEncodeTestCase (pub Rc<JsonEncodeTestCase_Variant>);

pub type JsonParserTestCase = ParserTestCase ;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct JsonRoundtripTestCase_Variant {
  pub type_: crate::hydra::core::Type,
  pub term: crate::hydra::core::Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct JsonRoundtripTestCase (pub Rc<JsonRoundtripTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LiftLambdaAboveLetTestCase_Variant {
  pub input: crate::hydra::core::Term,
  pub output: crate::hydra::core::Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LiftLambdaAboveLetTestCase (pub Rc<LiftLambdaAboveLetTestCase_Variant>);

pub type JsonWriterTestCase = WriterTestCase ;

pub type ParserTestCase = (String, crate::hydra::parsing::ParseResult) ;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Tag_Variant (pub String);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Tag (pub Rc<Tag_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TestCodec_Variant {
  pub language: crate::hydra::coders::LanguageName,
  pub file_extension: crate::hydra::module::FileExtension,
  pub encode_term: Rc<dyn Fn(crate::hydra::core::Term) -> Rc<dyn Fn(crate::hydra::graph::Graph) -> Either<String, String>>>,
  pub encode_type: Rc<dyn Fn(crate::hydra::core::Type) -> Rc<dyn Fn(crate::hydra::graph::Graph) -> Either<String, String>>>,
  pub format_test_name: Rc<dyn Fn(String) -> String>,
  pub format_module_name: Rc<dyn Fn(crate::hydra::module::Namespace) -> String>,
  pub test_case_template: String,
  pub test_group_template: String,
  pub module_template: String,
  pub import_template: String,
  pub find_imports: Rc<dyn Fn(BTreeSet<crate::hydra::core::Name>) -> Vec<String>>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TestCodec (pub Rc<TestCodec_Variant>);

pub type TestGenerator = (Rc<dyn Fn(crate::hydra::module::Module) -> Rc<dyn Fn(crate::hydra::graph::Graph) -> Either<String, crate::hydra::module::Namespaces>>>, Rc<dyn Fn(crate::hydra::module::Namespaces) -> TestCodec>, Rc<dyn Fn(crate::hydra::module::Module) -> Rc<dyn Fn(TestGroup) -> Rc<dyn Fn(crate::hydra::graph::Graph) -> Either<String, (String, String)>>>>, Option<Rc<dyn Fn(String) -> Rc<dyn Fn(Vec<crate::hydra::module::Module>) -> (String, String)>>>) ;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TestCase_Variant {
  AlphaConversion(AlphaConversionTestCase),
  CaseConversion(CaseConversionTestCase),
  DeannotateTerm(DeannotateTermTestCase),
  DeannotateType(DeannotateTypeTestCase),
  DelegatedEvaluation(DelegatedEvaluationTestCase),
  EtaExpansion(EtaExpansionTestCase),
  FlattenLetTerms(FlattenLetTermsTestCase),
  FreeVariables(FreeVariablesTestCase),
  Evaluation(EvaluationTestCase),
  Inference(InferenceTestCase),
  InferenceFailure(InferenceFailureTestCase),
  JsonDecode(JsonDecodeTestCase),
  JsonEncode(JsonEncodeTestCase),
  JsonParser(JsonParserTestCase),
  JsonRoundtrip(JsonRoundtripTestCase),
  JsonWriter(JsonWriterTestCase),
  LiftLambdaAboveLet(LiftLambdaAboveLetTestCase),
  Serialization(SerializationTestCase),
  SimplifyTerm(SimplifyTermTestCase),
  TopologicalSort(TopologicalSortTestCase),
  TopologicalSortBindings(TopologicalSortBindingsTestCase),
  TopologicalSortSCC(TopologicalSortSCCTestCase),
  TypeChecking(TypeCheckingTestCase),
  TypeCheckingFailure(TypeCheckingFailureTestCase),
  TypeReduction(TypeReductionTestCase),
  NormalizeTypeVariables(NormalizeTypeVariablesTestCase),
  FoldOverTerm(FoldOverTermTestCase),
  RewriteTerm(RewriteTermTestCase),
  RewriteType(RewriteTypeTestCase),
  HoistSubterms(HoistSubtermsTestCase),
  HoistCaseStatements(HoistCaseStatementsTestCase),
  HoistLetBindings(HoistLetBindingsTestCase),
  HoistPolymorphicLetBindings(HoistPolymorphicLetBindingsTestCase),
  SubstInType(SubstInTypeTestCase),
  VariableOccursInType(VariableOccursInTypeTestCase),
  UnifyTypes(UnifyTypesTestCase),
  JoinTypes(JoinTypesTestCase),
  UnshadowVariables(UnshadowVariablesTestCase)}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TestCase (pub Rc<TestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TestCaseWithMetadata_Variant {
  pub name: String,
  pub case: TestCase,
  pub description: Option<String>,
  pub tags: Vec<Tag>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TestCaseWithMetadata (pub Rc<TestCaseWithMetadata_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TestGroup_Variant {
  pub name: String,
  pub description: Option<String>,
  pub subgroups: Vec<TestGroup>,
  pub cases: Vec<TestCaseWithMetadata>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TestGroup (pub Rc<TestGroup_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeCheckingTestCase_Variant {
  pub input: crate::hydra::core::Term,
  pub output_term: crate::hydra::core::Term,
  pub output_type: crate::hydra::core::Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeCheckingTestCase (pub Rc<TypeCheckingTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeCheckingFailureTestCase_Variant {
  pub input: crate::hydra::core::Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeCheckingFailureTestCase (pub Rc<TypeCheckingFailureTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TopologicalSortBindingsTestCase_Variant {
  pub bindings: Vec<(crate::hydra::core::Name, crate::hydra::core::Term)>,
  pub expected: Vec<Vec<(crate::hydra::core::Name, crate::hydra::core::Term)>>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TopologicalSortBindingsTestCase (pub Rc<TopologicalSortBindingsTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TopologicalSortTestCase_Variant {
  pub adjacency_list: Vec<(i32, Vec<i32>)>,
  pub expected: Either<Vec<Vec<i32>>, Vec<i32>>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TopologicalSortTestCase (pub Rc<TopologicalSortTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TopologicalSortSCCTestCase_Variant {
  pub adjacency_list: Vec<(i32, Vec<i32>)>,
  pub expected: Vec<Vec<i32>>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TopologicalSortSCCTestCase (pub Rc<TopologicalSortSCCTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SerializationTestCase_Variant {
  pub input: crate::hydra::ast::Expr,
  pub output: String}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SerializationTestCase (pub Rc<SerializationTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SimplifyTermTestCase_Variant {
  pub input: crate::hydra::core::Term,
  pub output: crate::hydra::core::Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SimplifyTermTestCase (pub Rc<SimplifyTermTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NormalizeTypeVariablesTestCase_Variant {
  pub input: crate::hydra::core::Term,
  pub output: crate::hydra::core::Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NormalizeTypeVariablesTestCase (pub Rc<NormalizeTypeVariablesTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeReductionTestCase_Variant {
  pub input: crate::hydra::core::Type,
  pub output: crate::hydra::core::Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeReductionTestCase (pub Rc<TypeReductionTestCase_Variant>);

pub type WriterTestCase = (A, String) ;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SubstInTypeTestCase_Variant {
  pub substitution: Vec<(crate::hydra::core::Name, crate::hydra::core::Type)>,
  pub input: crate::hydra::core::Type,
  pub output: crate::hydra::core::Type}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SubstInTypeTestCase (pub Rc<SubstInTypeTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct VariableOccursInTypeTestCase_Variant {
  pub variable: crate::hydra::core::Name,
  pub type_: crate::hydra::core::Type,
  pub expected: bool}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct VariableOccursInTypeTestCase (pub Rc<VariableOccursInTypeTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnshadowVariablesTestCase_Variant {
  pub input: crate::hydra::core::Term,
  pub output: crate::hydra::core::Term}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnshadowVariablesTestCase (pub Rc<UnshadowVariablesTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnifyTypesTestCase_Variant {
  pub schema_types: Vec<crate::hydra::core::Name>,
  pub left: crate::hydra::core::Type,
  pub right: crate::hydra::core::Type,
  pub expected: Either<String, crate::hydra::typing::TypeSubst>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnifyTypesTestCase (pub Rc<UnifyTypesTestCase_Variant>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct JoinTypesTestCase_Variant {
  pub left: crate::hydra::core::Type,
  pub right: crate::hydra::core::Type,
  pub expected: Either<(), Vec<crate::hydra::typing::TypeConstraint>>}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct JoinTypesTestCase (pub Rc<JoinTypesTestCase_Variant>);
