{-# LANGUAGE FlexibleContexts #-}

-- | Meta-DSL for constructing test-related terms
-- TODO: merge with Hydra.Dsl.Tests

module Hydra.Dsl.Meta.Testing where

import Hydra.Kernel
import Hydra.Ast (Expr)
import Hydra.Json.Model (Value)
import Hydra.Parsing (ParseResult)
import Hydra.Testing as Testing
import Hydra.Dsl.Meta.Phantoms as Phantoms hiding ((++))
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Dsl.Meta.Types as T

import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

type Int32 = I.Int32


tag_disabled = Tag "disabled"
tag_disabledForMinimalInference = Tag "disabledForMinimalInference"
tag_disabledForPython = Tag "disabledForPython"
tag_requiresFlowDecoding = Tag "requiresFlowDecoding"

noTags :: TTerm [Tag]
noTags = Phantoms.list ([] :: [TTerm Tag])

expectFailure :: AsTerm t Term => Int -> [Tag] -> t -> TTerm TestCaseWithMetadata
expectFailure i tags term = infFailureTest ("#" ++ show i) tags (asTerm term)

expectMono :: AsTerm t Term => Int -> [Tag] -> t -> TTerm Type -> TTerm TestCaseWithMetadata
expectMono i tags term typ = infTest ("#" ++ show i) tags (asTerm term) $ T.mono typ

expectPoly :: AsTerm t Term => Int -> [Tag] -> t -> [String] -> TTerm Type -> TTerm TestCaseWithMetadata
expectPoly i tags term params typ = infTest ("#" ++ show i) tags (asTerm term) $ T.poly params typ

groupRef = MetaTerms.varNamePhantom . bindingName

primCase :: String -> Name -> [TTerm Term] -> TTerm Term -> TTerm TestCaseWithMetadata
primCase cname primName args output = primCaseWithTags cname [] primName args output

primCaseWithTags :: String -> [Tag] -> Name -> [TTerm Term] -> TTerm Term -> TTerm TestCaseWithMetadata
primCaseWithTags cname tags primName args output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseEvaluation $ evaluationTestCase evaluationStyleEager input output)
  nothing (Phantoms.list $ tag . unTag <$> tags)
  where
    input = L.foldl (MetaTerms.@@) (MetaTerms.primitive primName) args

evalCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
evalCase cname input output = evalCaseWithTags cname [] input output

evalCaseWithTags :: String -> [Tag] -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
evalCaseWithTags cname tags input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseEvaluation $ evaluationTestCase evaluationStyleEager input output)
  nothing (Phantoms.list $ tag . unTag <$> tags)

infFailureTest :: String -> [Tag] -> TTerm Term -> TTerm TestCaseWithMetadata
infFailureTest name tags term = testCaseWithMetadata (Phantoms.string name)
  (testCaseInferenceFailure $ inferenceFailureTestCase term) nothing (Phantoms.list $ tag . unTag <$> tags)

infTest :: String -> [Tag] -> TTerm Term -> TTerm TypeScheme -> TTerm TestCaseWithMetadata
infTest name tags term ts = testCaseWithMetadata (Phantoms.string name)
  (testCaseInference $ inferenceTestCase term ts) nothing (Phantoms.list $ tag . unTag <$> tags)

isDisabled tcase = tag_disabled `L.elem` Testing.testCaseWithMetadataTags tcase
isDisabledForMinimalInference tcase = tag_disabledForMinimalInference `L.elem` Testing.testCaseWithMetadataTags tcase
isDisabledForPython tcase = tag_disabledForPython `L.elem` Testing.testCaseWithMetadataTags tcase
isRequiresFlowDecoding tcase = tag_requiresFlowDecoding `L.elem` Testing.testCaseWithMetadataTags tcase

mapTerm :: [(TTerm Term, TTerm Term)] -> TTerm Term
mapTerm pairs = TTerm $ TermUnion $ Injection _Term $ Field _Term_map $ TermMap $ M.fromList [(unTTerm k, unTTerm v) | (k, v) <- pairs]

mapTermEmpty :: TTerm (M.Map k v)
mapTermEmpty = TTerm $ TermMap M.empty

subgroup :: AsTerm t TestCaseWithMetadata => String -> [t] -> TTerm TestGroup
subgroup name cases = tgroup name Nothing [] (asTerm <$> cases)

supergroup :: AsTerm t TestGroup => String -> [t] -> TTerm TestGroup
supergroup name subgroups = tgroup name Nothing (asTerm <$> subgroups) []

tag :: String -> TTerm Tag
tag = Phantoms.wrap _Tag . Phantoms.string

tgroup :: String -> Maybe String -> [TTerm TestGroup] -> [TTerm TestCaseWithMetadata] -> TTerm TestGroup
tgroup name mdesc subgroups cases = testGroup (Phantoms.string name) (opt (Phantoms.string <$> mdesc)) (Phantoms.list subgroups) (Phantoms.list cases)

----------------------------------------

encodeGroup (TestGroup name desc groups cases) = Terms.record _TestGroup [
  Field _TestGroup_name $ Terms.string name,
  Field _TestGroup_description $ Terms.optional (Terms.string <$> desc),
  Field _TestGroup_subgroups $ Terms.list (encodeGroup <$> groups),
  Field _TestGroup_cases $ Terms.list (encodeCaseWithMetadata <$> cases)]
encodeCaseWithMetadata (TestCaseWithMetadata name tcase mdesc tags) = Terms.record _TestCaseWithMetadata [
  Field _TestCaseWithMetadata_name $ Terms.string name,
  Field _TestCaseWithMetadata_case $ encodeCase tcase,
  Field _TestCaseWithMetadata_description $ Terms.optional (Terms.string <$> mdesc),
  Field _TestCaseWithMetadata_tags $ Terms.list (Terms.string . unTag <$> tags)]
encodeCase tcase = case tcase of
  TestCaseCaseConversion ccase -> Terms.inject _TestCase _TestCase_caseConversion $ encodeCaseConversionTestCase ccase
  TestCaseEtaExpansion ecase -> Terms.inject _TestCase _TestCase_etaExpansion $ encodeEtaExpansionTestCase ecase
  TestCaseEvaluation ecase -> Terms.inject _TestCase _TestCase_evaluation $ encodeEvaluationTestCase ecase
  TestCaseInference icase -> Terms.inject _TestCase _TestCase_inference $ encodeInferenceTestCase icase
encodeCaseConvention c = Terms.injectUnit _CaseConvention $ case c of
  CaseConventionLowerSnake -> _CaseConvention_lowerSnake
  CaseConventionUpperSnake -> _CaseConvention_upperSnake
  CaseConventionCamel -> _CaseConvention_camel
  CaseConventionPascal -> _CaseConvention_pascal
encodeCaseConversionTestCase (CaseConversionTestCase fromConvention toConvention fromString toString) = Terms.record _CaseConversionTestCase [
  Field _CaseConversionTestCase_fromConvention $ encodeCaseConvention fromConvention,
  Field _CaseConversionTestCase_toConvention $ encodeCaseConvention toConvention,
  Field _CaseConversionTestCase_fromString $ Terms.string fromString,
  Field _CaseConversionTestCase_toString $ Terms.string toString]
encodeEtaExpansionTestCase (EtaExpansionTestCase input output) = Terms.record _EtaExpansionTestCase [
  Field _EtaExpansionTestCase_input $ EncodeCore.term input,
  Field _EtaExpansionTestCase_output $ EncodeCore.term output]
encodeEvaluationTestCase (EvaluationTestCase style input output) = Terms.record _EvaluationTestCase [
  Field _EvaluationTestCase_evaluationStyle $ Terms.inject _EvaluationStyle (case style of
    EvaluationStyleEager -> _EvaluationStyle_eager
    EvaluationStyleLazy -> _EvaluationStyle_lazy) Terms.unit,
  Field _EvaluationTestCase_input $ EncodeCore.term input,
  Field _EvaluationTestCase_output $ EncodeCore.term output]
encodeInferenceTestCase (InferenceTestCase input output) = Terms.record _InferenceTestCase [
  Field _InferenceTestCase_input $ EncodeCore.term input,
  Field _InferenceTestCase_output $ EncodeCore.typeScheme output]

----------------------------------------

caseConversionTestCase :: TTerm CaseConvention -> TTerm CaseConvention -> TTerm String -> TTerm String -> TTerm CaseConversionTestCase
caseConversionTestCase fromConvention toConvention fromString toString =
  Phantoms.record _CaseConversionTestCase [
    _CaseConversionTestCase_fromConvention>>: fromConvention,
    _CaseConversionTestCase_toConvention>>: toConvention,
    _CaseConversionTestCase_fromString>>: fromString,
    _CaseConversionTestCase_toString>>: toString]

encodedTestGroupToBinding :: Namespace -> String -> TTerm TestGroup -> Binding
encodedTestGroupToBinding ns lname group = Binding name (unTTerm group)
    $ Just $ TypeScheme [] typ Nothing
  where
    name = unqualifyName $ QualifiedName (Just ns) lname
    typ = TypeVariable _TestGroup

inferenceFailureTestCase :: TTerm Term -> TTerm InferenceFailureTestCase
inferenceFailureTestCase input = Phantoms.record _InferenceFailureTestCase [
  _InferenceFailureTestCase_input>>: input]

etaExpansionTestCase :: TTerm Term -> TTerm Term -> TTerm EtaExpansionTestCase
etaExpansionTestCase input output = Phantoms.record _EtaExpansionTestCase [
  _EtaExpansionTestCase_input>>: input,
  _EtaExpansionTestCase_output>>: output]

inferenceTestCase :: TTerm Term -> TTerm TypeScheme -> TTerm InferenceTestCase
inferenceTestCase input output = Phantoms.record _InferenceTestCase [
  _InferenceTestCase_input>>: input,
  _InferenceTestCase_output>>: output]

evaluationStyleEager :: TTerm EvaluationStyle
evaluationStyleEager = Phantoms.injectUnit _EvaluationStyle _EvaluationStyle_eager

evaluationStyleLazy :: TTerm EvaluationStyle
evaluationStyleLazy = Phantoms.injectUnit _EvaluationStyle _EvaluationStyle_lazy

evaluationTestCase :: TTerm EvaluationStyle -> TTerm Term -> TTerm Term -> TTerm EvaluationTestCase
evaluationTestCase style input output = Phantoms.record _EvaluationTestCase [
  _EvaluationTestCase_evaluationStyle>>: style,
  _EvaluationTestCase_input>>: input,
  _EvaluationTestCase_output>>: output]

delegatedEvaluationTestCase :: TTerm Term -> TTerm Term -> TTerm DelegatedEvaluationTestCase
delegatedEvaluationTestCase input output = Phantoms.record _DelegatedEvaluationTestCase [
  _DelegatedEvaluationTestCase_input>>: input,
  _DelegatedEvaluationTestCase_output>>: output]

testCaseCaseConversion :: TTerm CaseConversionTestCase -> TTerm TestCase
testCaseCaseConversion = inject _TestCase _TestCase_caseConversion

testCaseDelegatedEvaluation :: TTerm DelegatedEvaluationTestCase -> TTerm TestCase
testCaseDelegatedEvaluation = inject _TestCase _TestCase_delegatedEvaluation

testCaseEtaExpansion :: TTerm EtaExpansionTestCase -> TTerm TestCase
testCaseEtaExpansion = inject _TestCase _TestCase_etaExpansion

testCaseEvaluation :: TTerm EvaluationTestCase -> TTerm TestCase
testCaseEvaluation = inject _TestCase _TestCase_evaluation

testCaseInference :: TTerm InferenceTestCase -> TTerm TestCase
testCaseInference = inject _TestCase _TestCase_inference

testCaseInferenceFailure :: TTerm InferenceFailureTestCase -> TTerm TestCase
testCaseInferenceFailure = inject _TestCase _TestCase_inferenceFailure

testCaseJsonCoder :: TTerm JsonCoderTestCase -> TTerm TestCase
testCaseJsonCoder = inject _TestCase _TestCase_jsonCoder

testCaseJsonDecode :: TTerm JsonDecodeTestCase -> TTerm TestCase
testCaseJsonDecode = inject _TestCase _TestCase_jsonDecode

testCaseJsonEncode :: TTerm JsonEncodeTestCase -> TTerm TestCase
testCaseJsonEncode = inject _TestCase _TestCase_jsonEncode

testCaseJsonParser :: TTerm JsonParserTestCase -> TTerm TestCase
testCaseJsonParser = inject _TestCase _TestCase_jsonParser

testCaseJsonRoundtrip :: TTerm JsonRoundtripTestCase -> TTerm TestCase
testCaseJsonRoundtrip = inject _TestCase _TestCase_jsonRoundtrip

testCaseJsonWriter :: TTerm JsonWriterTestCase -> TTerm TestCase
testCaseJsonWriter = inject _TestCase _TestCase_jsonWriter

testCaseAlphaConversion :: TTerm AlphaConversionTestCase -> TTerm TestCase
testCaseAlphaConversion = inject _TestCase _TestCase_alphaConversion

testCaseTypeReduction :: TTerm TypeReductionTestCase -> TTerm TestCase
testCaseTypeReduction = inject _TestCase _TestCase_typeReduction

alphaConversionTestCase :: TTerm Term -> TTerm Name -> TTerm Name -> TTerm Term -> TTerm AlphaConversionTestCase
alphaConversionTestCase term oldVar newVar result = Phantoms.record _AlphaConversionTestCase [
  _AlphaConversionTestCase_term>>: term,
  _AlphaConversionTestCase_oldVariable>>: oldVar,
  _AlphaConversionTestCase_newVariable>>: newVar,
  _AlphaConversionTestCase_result>>: result]

typeReductionTestCase :: TTerm Type -> TTerm Type -> TTerm TypeReductionTestCase
typeReductionTestCase input output = Phantoms.record _TypeReductionTestCase [
  _TypeReductionTestCase_input>>: input,
  _TypeReductionTestCase_output>>: output]

-- | Convenience function for creating alpha conversion test cases
alphaCase :: String -> TTerm Term -> TTerm Name -> TTerm Name -> TTerm Term -> TTerm TestCaseWithMetadata
alphaCase cname term oldVar newVar result = testCaseWithMetadata (Phantoms.string cname)
  (testCaseAlphaConversion $ alphaConversionTestCase term oldVar newVar result)
  nothing noTags

-- | Convenience function for creating type reduction test cases
typeRedCase :: String -> TTerm Type -> TTerm Type -> TTerm TestCaseWithMetadata
typeRedCase cname input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseTypeReduction $ typeReductionTestCase input output)
  nothing noTags

writerTestCase :: TTerm a -> TTerm String -> TTerm (WriterTestCase a)
writerTestCase input output = Phantoms.record _WriterTestCase [
  _WriterTestCase_input>>: input,
  _WriterTestCase_output>>: output]

jsonWriterTestCase :: TTerm Value -> TTerm String -> TTerm JsonWriterTestCase
jsonWriterTestCase = writerTestCase

parserTestCase :: TTerm String -> TTerm (ParseResult a) -> TTerm (ParserTestCase a)
parserTestCase input output = Phantoms.record _ParserTestCase [
  _ParserTestCase_input>>: input,
  _ParserTestCase_output>>: output]

jsonParserTestCase :: TTerm String -> TTerm (ParseResult Value) -> TTerm JsonParserTestCase
jsonParserTestCase = parserTestCase

jsonCoderTestCase :: TTerm Type -> TTerm Term -> TTerm Value -> TTerm JsonCoderTestCase
jsonCoderTestCase typ term json = Phantoms.record _JsonCoderTestCase [
  _JsonCoderTestCase_type>>: typ,
  _JsonCoderTestCase_term>>: term,
  _JsonCoderTestCase_json>>: json]

-- | Convenience function for creating JSON coder test cases
coderCase :: String -> TTerm Type -> TTerm Term -> TTerm Value -> TTerm TestCaseWithMetadata
coderCase cname typ term json = testCaseWithMetadata (Phantoms.string cname)
  (testCaseJsonCoder $ jsonCoderTestCase typ term json)
  nothing noTags

----------------------------------------
-- JSON encode/decode/roundtrip test case helpers (Either-based)

jsonDecodeTestCase :: TTerm Type -> TTerm Value -> TTerm (Either String Term) -> TTerm JsonDecodeTestCase
jsonDecodeTestCase typ json expected = Phantoms.record _JsonDecodeTestCase [
  _JsonDecodeTestCase_type>>: typ,
  _JsonDecodeTestCase_json>>: json,
  _JsonDecodeTestCase_expected>>: expected]

jsonEncodeTestCase :: TTerm Term -> TTerm (Either String Value) -> TTerm JsonEncodeTestCase
jsonEncodeTestCase term expected = Phantoms.record _JsonEncodeTestCase [
  _JsonEncodeTestCase_term>>: term,
  _JsonEncodeTestCase_expected>>: expected]

jsonRoundtripTestCase :: TTerm Type -> TTerm Term -> TTerm JsonRoundtripTestCase
jsonRoundtripTestCase typ term = Phantoms.record _JsonRoundtripTestCase [
  _JsonRoundtripTestCase_type>>: typ,
  _JsonRoundtripTestCase_term>>: term]

-- | Convenience function for creating JSON decode test cases (Either-based)
jsonDecodeCase :: String -> TTerm Type -> TTerm Value -> TTerm (Either String Term) -> TTerm TestCaseWithMetadata
jsonDecodeCase cname typ json expected = testCaseWithMetadata (Phantoms.string cname)
  (testCaseJsonDecode $ jsonDecodeTestCase typ json expected)
  nothing noTags

-- | Convenience function for creating JSON encode test cases (Either-based)
jsonEncodeCase :: String -> TTerm Term -> TTerm (Either String Value) -> TTerm TestCaseWithMetadata
jsonEncodeCase cname term expected = testCaseWithMetadata (Phantoms.string cname)
  (testCaseJsonEncode $ jsonEncodeTestCase term expected)
  nothing noTags

-- | Convenience function for creating JSON round-trip test cases (Either-based)
jsonRoundtripCase :: String -> TTerm Type -> TTerm Term -> TTerm TestCaseWithMetadata
jsonRoundtripCase cname typ term = testCaseWithMetadata (Phantoms.string cname)
  (testCaseJsonRoundtrip $ jsonRoundtripTestCase typ term)
  nothing noTags

testCaseTopologicalSort :: TTerm TopologicalSortTestCase -> TTerm TestCase
testCaseTopologicalSort = inject _TestCase _TestCase_topologicalSort

testCaseTopologicalSortSCC :: TTerm TopologicalSortSCCTestCase -> TTerm TestCase
testCaseTopologicalSortSCC = inject _TestCase _TestCase_topologicalSortSCC

topologicalSortTestCase :: TTerm [(Int, [Int])] -> TTerm (Either [[Int]] [Int]) -> TTerm TopologicalSortTestCase
topologicalSortTestCase adj expected = Phantoms.record _TopologicalSortTestCase [
  _TopologicalSortTestCase_adjacencyList>>: adj,
  _TopologicalSortTestCase_expected>>: expected]

topologicalSortSCCTestCase :: TTerm [(Int, [Int])] -> TTerm [[Int]] -> TTerm TopologicalSortSCCTestCase
topologicalSortSCCTestCase adj expected = Phantoms.record _TopologicalSortSCCTestCase [
  _TopologicalSortSCCTestCase_adjacencyList>>: adj,
  _TopologicalSortSCCTestCase_expected>>: expected]

-- | Convenience function for creating topological sort test cases
sortCase :: String -> TTerm [(Int, [Int])] -> TTerm (Either [[Int]] [Int]) -> TTerm TestCaseWithMetadata
sortCase cname adj expected = testCaseWithMetadata (Phantoms.string cname)
  (testCaseTopologicalSort $ topologicalSortTestCase adj expected)
  nothing noTags

-- | Convenience function for creating topological sort SCC test cases
sortSCCCase :: String -> TTerm [(Int, [Int])] -> TTerm [[Int]] -> TTerm TestCaseWithMetadata
sortSCCCase cname adj expected = testCaseWithMetadata (Phantoms.string cname)
  (testCaseTopologicalSortSCC $ topologicalSortSCCTestCase adj expected)
  nothing noTags

testCaseWithMetadata :: TTerm String -> TTerm TestCase -> TTerm (Maybe String) -> TTerm [Tag] -> TTerm TestCaseWithMetadata
testCaseWithMetadata name tcase description tags = Phantoms.record _TestCaseWithMetadata [
  _TestCaseWithMetadata_name>>: name,
  _TestCaseWithMetadata_case>>: tcase,
  _TestCaseWithMetadata_description>>: description,
  _TestCaseWithMetadata_tags>>: tags]

testCaseWithMetadataName :: TTerm (TestCaseWithMetadata -> String)
testCaseWithMetadataName = Phantoms.project _TestCaseWithMetadata _TestCaseWithMetadata_name

testCaseWithMetadataCase :: TTerm (TestCaseWithMetadata -> TestCase)
testCaseWithMetadataCase = Phantoms.project _TestCaseWithMetadata _TestCaseWithMetadata_case

testCaseWithMetadataDescription :: TTerm (TestCaseWithMetadata -> Maybe String)
testCaseWithMetadataDescription = Phantoms.project _TestCaseWithMetadata _TestCaseWithMetadata_description

testCaseWithMetadataTags :: TTerm (TestCaseWithMetadata -> [Tag])
testCaseWithMetadataTags = Phantoms.project _TestCaseWithMetadata _TestCaseWithMetadata_tags

testGroup :: TTerm String -> TTerm (Maybe String) -> TTerm [TestGroup] -> TTerm [TestCaseWithMetadata] -> TTerm TestGroup
testGroup name description subgroups cases = Phantoms.record _TestGroup [
  _TestGroup_name>>: name,
  _TestGroup_description>>: description,
  _TestGroup_subgroups>>: subgroups,
  _TestGroup_cases>>: cases]

testGroupToBinding :: Namespace -> String -> TestGroup -> Binding
testGroupToBinding ns lname group = encodedTestGroupToBinding ns lname (TTerm $ encodeGroup group)

testCaseSerialization :: TTerm SerializationTestCase -> TTerm TestCase
testCaseSerialization = inject _TestCase _TestCase_serialization

serializationTestCase :: TTerm Expr -> TTerm String -> TTerm SerializationTestCase
serializationTestCase input output = Phantoms.record _SerializationTestCase [
  _SerializationTestCase_input>>: input,
  _SerializationTestCase_output>>: output]

-- | Convenience function for creating serialization test cases
serCase :: String -> TTerm Expr -> TTerm String -> TTerm TestCaseWithMetadata
serCase cname input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseSerialization $ serializationTestCase input output)
  nothing noTags

----------------------------------------
-- Rewriting test case helpers

testCaseFlattenLetTerms :: TTerm FlattenLetTermsTestCase -> TTerm TestCase
testCaseFlattenLetTerms = inject _TestCase _TestCase_flattenLetTerms

testCaseFreeVariables :: TTerm FreeVariablesTestCase -> TTerm TestCase
testCaseFreeVariables = inject _TestCase _TestCase_freeVariables

testCaseLiftLambdaAboveLet :: TTerm LiftLambdaAboveLetTestCase -> TTerm TestCase
testCaseLiftLambdaAboveLet = inject _TestCase _TestCase_liftLambdaAboveLet

testCaseSimplifyTerm :: TTerm SimplifyTermTestCase -> TTerm TestCase
testCaseSimplifyTerm = inject _TestCase _TestCase_simplifyTerm

flattenLetTermsTestCase :: TTerm Term -> TTerm Term -> TTerm FlattenLetTermsTestCase
flattenLetTermsTestCase input output = Phantoms.record _FlattenLetTermsTestCase [
  _FlattenLetTermsTestCase_input>>: input,
  _FlattenLetTermsTestCase_output>>: output]

freeVariablesTestCase :: TTerm Term -> TTerm (S.Set Name) -> TTerm FreeVariablesTestCase
freeVariablesTestCase input output = Phantoms.record _FreeVariablesTestCase [
  _FreeVariablesTestCase_input>>: input,
  _FreeVariablesTestCase_output>>: output]

liftLambdaAboveLetTestCase :: TTerm Term -> TTerm Term -> TTerm LiftLambdaAboveLetTestCase
liftLambdaAboveLetTestCase input output = Phantoms.record _LiftLambdaAboveLetTestCase [
  _LiftLambdaAboveLetTestCase_input>>: input,
  _LiftLambdaAboveLetTestCase_output>>: output]

simplifyTermTestCase :: TTerm Term -> TTerm Term -> TTerm SimplifyTermTestCase
simplifyTermTestCase input output = Phantoms.record _SimplifyTermTestCase [
  _SimplifyTermTestCase_input>>: input,
  _SimplifyTermTestCase_output>>: output]

-- | Convenience function for creating flatten let terms test cases
flattenCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
flattenCase cname input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseFlattenLetTerms $ flattenLetTermsTestCase input output)
  nothing noTags

-- | Convenience function for creating free variables test cases
freeVarsCase :: String -> TTerm Term -> TTerm (S.Set Name) -> TTerm TestCaseWithMetadata
freeVarsCase cname input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseFreeVariables $ freeVariablesTestCase input output)
  nothing noTags

-- | Convenience function for creating lift lambda above let test cases
liftLambdaCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
liftLambdaCase cname input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseLiftLambdaAboveLet $ liftLambdaAboveLetTestCase input output)
  nothing noTags

-- | Convenience function for creating simplify term test cases
simplifyCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
simplifyCase cname input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseSimplifyTerm $ simplifyTermTestCase input output)
  nothing noTags

----------------------------------------
-- Deannotate test case helpers

testCaseDeannotateTerm :: TTerm DeannotateTermTestCase -> TTerm TestCase
testCaseDeannotateTerm = inject _TestCase _TestCase_deannotateTerm

testCaseDeannotateType :: TTerm DeannotateTypeTestCase -> TTerm TestCase
testCaseDeannotateType = inject _TestCase _TestCase_deannotateType

deannotateTermTestCase :: TTerm Term -> TTerm Term -> TTerm DeannotateTermTestCase
deannotateTermTestCase input output = Phantoms.record _DeannotateTermTestCase [
  _DeannotateTermTestCase_input>>: input,
  _DeannotateTermTestCase_output>>: output]

deannotateTypeTestCase :: TTerm Type -> TTerm Type -> TTerm DeannotateTypeTestCase
deannotateTypeTestCase input output = Phantoms.record _DeannotateTypeTestCase [
  _DeannotateTypeTestCase_input>>: input,
  _DeannotateTypeTestCase_output>>: output]

-- | Convenience function for creating deannotate term test cases
deannotateTermCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
deannotateTermCase cname input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseDeannotateTerm $ deannotateTermTestCase input output)
  nothing noTags

-- | Convenience function for creating deannotate type test cases
deannotateTypeCase :: String -> TTerm Type -> TTerm Type -> TTerm TestCaseWithMetadata
deannotateTypeCase cname input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseDeannotateType $ deannotateTypeTestCase input output)
  nothing noTags

----------------------------------------
-- Topological sort bindings test case helpers

testCaseTopologicalSortBindings :: TTerm TopologicalSortBindingsTestCase -> TTerm TestCase
testCaseTopologicalSortBindings = inject _TestCase _TestCase_topologicalSortBindings

topologicalSortBindingsTestCase :: TTerm [(Name, Term)] -> TTerm [[(Name, Term)]] -> TTerm TopologicalSortBindingsTestCase
topologicalSortBindingsTestCase bindings expected = Phantoms.record _TopologicalSortBindingsTestCase [
  _TopologicalSortBindingsTestCase_bindings>>: bindings,
  _TopologicalSortBindingsTestCase_expected>>: expected]

-- | Convenience function for creating topological sort bindings test cases
sortBindingsCase :: String -> TTerm [(Name, Term)] -> TTerm [[(Name, Term)]] -> TTerm TestCaseWithMetadata
sortBindingsCase cname bindings expected = testCaseWithMetadata (Phantoms.string cname)
  (testCaseTopologicalSortBindings $ topologicalSortBindingsTestCase bindings expected)
  nothing noTags

----------------------------------------
-- Normalize type variables test case helpers

testCaseNormalizeTypeVariables :: TTerm NormalizeTypeVariablesTestCase -> TTerm TestCase
testCaseNormalizeTypeVariables = inject _TestCase _TestCase_normalizeTypeVariables

normalizeTypeVariablesTestCase :: TTerm Term -> TTerm Term -> TTerm NormalizeTypeVariablesTestCase
normalizeTypeVariablesTestCase input output = Phantoms.record _NormalizeTypeVariablesTestCase [
  _NormalizeTypeVariablesTestCase_input>>: input,
  _NormalizeTypeVariablesTestCase_output>>: output]

-- | Convenience function for creating normalize type variables test cases
normalizeTypeVarsCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
normalizeTypeVarsCase cname input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseNormalizeTypeVariables $ normalizeTypeVariablesTestCase input output)
  nothing noTags

----------------------------------------
-- Fold over term test case helpers

testCaseFoldOverTerm :: TTerm FoldOverTermTestCase -> TTerm TestCase
testCaseFoldOverTerm = inject _TestCase _TestCase_foldOverTerm

foldOverTermTestCase :: TTerm Term -> TTerm TraversalOrder -> TTerm FoldOperation -> TTerm Term -> TTerm FoldOverTermTestCase
foldOverTermTestCase input order op output = Phantoms.record _FoldOverTermTestCase [
  _FoldOverTermTestCase_input>>: input,
  _FoldOverTermTestCase_traversalOrder>>: order,
  _FoldOverTermTestCase_operation>>: op,
  _FoldOverTermTestCase_output>>: output]

-- Fold operation constructors
foldOpSumInt32Literals :: TTerm FoldOperation
foldOpSumInt32Literals = inject _FoldOperation _FoldOperation_sumInt32Literals $ Phantoms.unit

foldOpCollectListLengths :: TTerm FoldOperation
foldOpCollectListLengths = inject _FoldOperation _FoldOperation_collectListLengths $ Phantoms.unit

foldOpCollectLabels :: TTerm FoldOperation
foldOpCollectLabels = inject _FoldOperation _FoldOperation_collectLabels $ Phantoms.unit

-- | Convenience function for creating fold over term test cases
foldOverTermCase :: String -> TTerm Term -> TTerm TraversalOrder -> TTerm FoldOperation -> TTerm Term -> TTerm TestCaseWithMetadata
foldOverTermCase cname input order op output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseFoldOverTerm $ foldOverTermTestCase input order op output)
  nothing noTags

----------------------------------------
-- Rewrite term test case helpers

testCaseRewriteTerm :: TTerm RewriteTermTestCase -> TTerm TestCase
testCaseRewriteTerm = inject _TestCase _TestCase_rewriteTerm

rewriteTermTestCase :: TTerm Term -> TTerm TermRewriter -> TTerm Term -> TTerm RewriteTermTestCase
rewriteTermTestCase input rewriter output = Phantoms.record _RewriteTermTestCase [
  _RewriteTermTestCase_input>>: input,
  _RewriteTermTestCase_rewriter>>: rewriter,
  _RewriteTermTestCase_output>>: output]

-- Term rewriter constructors
termRewriterReplaceFooWithBar :: TTerm TermRewriter
termRewriterReplaceFooWithBar = inject _TermRewriter _TermRewriter_replaceFooWithBar $ Phantoms.unit

termRewriterReplaceInt32WithInt64 :: TTerm TermRewriter
termRewriterReplaceInt32WithInt64 = inject _TermRewriter _TermRewriter_replaceInt32WithInt64 $ Phantoms.unit

-- | Convenience function for creating rewrite term test cases (replaceFooWithBar)
rewriteTermCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
rewriteTermCase cname input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseRewriteTerm $ rewriteTermTestCase input termRewriterReplaceFooWithBar output)
  nothing noTags

----------------------------------------
-- Rewrite type test case helpers

testCaseRewriteType :: TTerm RewriteTypeTestCase -> TTerm TestCase
testCaseRewriteType = inject _TestCase _TestCase_rewriteType

rewriteTypeTestCase :: TTerm Type -> TTerm TypeRewriter -> TTerm Type -> TTerm RewriteTypeTestCase
rewriteTypeTestCase input rewriter output = Phantoms.record _RewriteTypeTestCase [
  _RewriteTypeTestCase_input>>: input,
  _RewriteTypeTestCase_rewriter>>: rewriter,
  _RewriteTypeTestCase_output>>: output]

-- Type rewriter constructors
typeRewriterReplaceStringWithInt32 :: TTerm TypeRewriter
typeRewriterReplaceStringWithInt32 = inject _TypeRewriter _TypeRewriter_replaceStringWithInt32 $ Phantoms.unit

-- | Convenience function for creating rewrite type test cases (replaceStringWithInt32)
rewriteTypeCase :: String -> TTerm Type -> TTerm Type -> TTerm TestCaseWithMetadata
rewriteTypeCase cname input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseRewriteType $ rewriteTypeTestCase input typeRewriterReplaceStringWithInt32 output)
  nothing noTags

-- | Convenience function for creating eta expansion test cases
etaCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
etaCase cname input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseEtaExpansion $ etaExpansionTestCase input output)
  nothing noTags

----------------------------------------
-- Hoist subterms test case helpers

testCaseHoistSubterms :: TTerm HoistSubtermsTestCase -> TTerm TestCase
testCaseHoistSubterms = inject _TestCase _TestCase_hoistSubterms

hoistSubtermsTestCase :: TTerm HoistPredicate -> TTerm Term -> TTerm Term -> TTerm HoistSubtermsTestCase
hoistSubtermsTestCase predicate input output = Phantoms.record _HoistSubtermsTestCase [
  _HoistSubtermsTestCase_predicate>>: predicate,
  _HoistSubtermsTestCase_input>>: input,
  _HoistSubtermsTestCase_output>>: output]

-- Hoist predicate constructors

-- | Hoist case statements (elimination unions) at non-top-level positions
hoistPredicateCaseStatements :: TTerm HoistPredicate
hoistPredicateCaseStatements = inject _HoistPredicate _HoistPredicate_caseStatements $ Phantoms.unit

-- | Hoist function applications at non-top-level positions
hoistPredicateApplications :: TTerm HoistPredicate
hoistPredicateApplications = inject _HoistPredicate _HoistPredicate_applications $ Phantoms.unit

-- | Hoist list terms at non-top-level positions
hoistPredicateLists :: TTerm HoistPredicate
hoistPredicateLists = inject _HoistPredicate _HoistPredicate_lists $ Phantoms.unit

-- | Never hoist anything (identity transformation)
hoistPredicateNothing :: TTerm HoistPredicate
hoistPredicateNothing = inject _HoistPredicate _HoistPredicate_nothing $ Phantoms.unit

-- | Convenience function for creating hoist subterms test cases
hoistCase :: String -> TTerm HoistPredicate -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
hoistCase cname predicate input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseHoistSubterms $ hoistSubtermsTestCase predicate input output)
  nothing noTags

----------------------------------------
-- Hoist case statements test case helpers

testCaseHoistCaseStatements :: TTerm HoistCaseStatementsTestCase -> TTerm TestCase
testCaseHoistCaseStatements = inject _TestCase _TestCase_hoistCaseStatements

hoistCaseStatementsTestCase :: TTerm Term -> TTerm Term -> TTerm HoistCaseStatementsTestCase
hoistCaseStatementsTestCase input output = Phantoms.record _HoistCaseStatementsTestCase [
  _HoistCaseStatementsTestCase_input>>: input,
  _HoistCaseStatementsTestCase_output>>: output]

-- | Convenience function for creating hoist case statements test cases
hoistCaseStatementsCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
hoistCaseStatementsCase cname input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseHoistCaseStatements $ hoistCaseStatementsTestCase input output)
  nothing noTags

-----------------------------------------
-- Hoist let bindings test case helpers (hoistAll=True, for Java)

testCaseHoistLetBindings :: TTerm HoistLetBindingsTestCase -> TTerm TestCase
testCaseHoistLetBindings = inject _TestCase _TestCase_hoistLetBindings

hoistLetBindingsTestCase :: TTerm Let -> TTerm Let -> TTerm HoistLetBindingsTestCase
hoistLetBindingsTestCase input output = Phantoms.record _HoistLetBindingsTestCase [
  _HoistLetBindingsTestCase_input>>: input,
  _HoistLetBindingsTestCase_output>>: output]

-- | Convenience function for creating hoist let bindings test cases
hoistLetBindingsCase :: String -> TTerm Let -> TTerm Let -> TTerm TestCaseWithMetadata
hoistLetBindingsCase cname input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseHoistLetBindings $ hoistLetBindingsTestCase input output)
  nothing noTags

-----------------------------------------
-- Hoist polymorphic let bindings test case helpers

testCaseHoistPolymorphicLetBindings :: TTerm HoistPolymorphicLetBindingsTestCase -> TTerm TestCase
testCaseHoistPolymorphicLetBindings = inject _TestCase _TestCase_hoistPolymorphicLetBindings

hoistPolymorphicLetBindingsTestCase :: TTerm Let -> TTerm Let -> TTerm HoistPolymorphicLetBindingsTestCase
hoistPolymorphicLetBindingsTestCase input output = Phantoms.record _HoistPolymorphicLetBindingsTestCase [
  _HoistPolymorphicLetBindingsTestCase_input>>: input,
  _HoistPolymorphicLetBindingsTestCase_output>>: output]

-- | Convenience function for creating hoist polymorphic let bindings test cases
hoistPolymorphicLetBindingsCase :: String -> TTerm Let -> TTerm Let -> TTerm TestCaseWithMetadata
hoistPolymorphicLetBindingsCase cname input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseHoistPolymorphicLetBindings $ hoistPolymorphicLetBindingsTestCase input output)
  nothing noTags

----------------------------------------
-- Type substitution test case helpers

testCaseSubstInType :: TTerm SubstInTypeTestCase -> TTerm TestCase
testCaseSubstInType = inject _TestCase _TestCase_substInType

substInTypeTestCase :: TTerm [(Name, Type)] -> TTerm Type -> TTerm Type -> TTerm SubstInTypeTestCase
substInTypeTestCase subst input output = Phantoms.record _SubstInTypeTestCase [
  _SubstInTypeTestCase_substitution>>: subst,
  _SubstInTypeTestCase_input>>: input,
  _SubstInTypeTestCase_output>>: output]

-- | Convenience function for creating type substitution test cases
substInTypeCase :: String -> TTerm [(Name, Type)] -> TTerm Type -> TTerm Type -> TTerm TestCaseWithMetadata
substInTypeCase cname subst input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseSubstInType $ substInTypeTestCase subst input output)
  nothing noTags

----------------------------------------
-- Variable occurs in type test case helpers

testCaseVariableOccursInType :: TTerm VariableOccursInTypeTestCase -> TTerm TestCase
testCaseVariableOccursInType = inject _TestCase _TestCase_variableOccursInType

variableOccursInTypeTestCase :: TTerm Name -> TTerm Type -> TTerm Bool -> TTerm VariableOccursInTypeTestCase
variableOccursInTypeTestCase variable typ expected = Phantoms.record _VariableOccursInTypeTestCase [
  _VariableOccursInTypeTestCase_variable>>: variable,
  _VariableOccursInTypeTestCase_type>>: typ,
  _VariableOccursInTypeTestCase_expected>>: expected]

-- | Convenience function for creating variable occurs in type test cases
variableOccursCase :: String -> TTerm Name -> TTerm Type -> TTerm Bool -> TTerm TestCaseWithMetadata
variableOccursCase cname variable typ expected = testCaseWithMetadata (Phantoms.string cname)
  (testCaseVariableOccursInType $ variableOccursInTypeTestCase variable typ expected)
  nothing noTags

----------------------------------------
-- Unify types test case helpers

testCaseUnifyTypes :: TTerm UnifyTypesTestCase -> TTerm TestCase
testCaseUnifyTypes = inject _TestCase _TestCase_unifyTypes

unifyTypesTestCase :: TTerm [Name] -> TTerm Type -> TTerm Type -> TTerm (Either String TypeSubst) -> TTerm UnifyTypesTestCase
unifyTypesTestCase schemaTypes left right expected = Phantoms.record _UnifyTypesTestCase [
  _UnifyTypesTestCase_schemaTypes>>: schemaTypes,
  _UnifyTypesTestCase_left>>: left,
  _UnifyTypesTestCase_right>>: right,
  _UnifyTypesTestCase_expected>>: expected]

-- | Convenience function for creating unify types test cases (expecting success)
-- The substitution is provided as a list of (name, type) pairs
unifyTypesCase :: String -> TTerm [Name] -> TTerm Type -> TTerm Type -> [(TTerm Name, TTerm Type)] -> TTerm TestCaseWithMetadata
unifyTypesCase cname schemaTypes left right substPairs = testCaseWithMetadata (Phantoms.string cname)
  (testCaseUnifyTypes $ unifyTypesTestCase schemaTypes left right (Phantoms.right (Phantoms.wrap _TypeSubst (Phantoms.map (M.fromList substPairs)))))
  nothing noTags

-- | Convenience function for creating unify types test cases (expecting failure)
unifyTypesFailCase :: String -> TTerm [Name] -> TTerm Type -> TTerm Type -> String -> TTerm TestCaseWithMetadata
unifyTypesFailCase cname schemaTypes left right errSubstring = testCaseWithMetadata (Phantoms.string cname)
  (testCaseUnifyTypes $ unifyTypesTestCase schemaTypes left right (Phantoms.left (Phantoms.string errSubstring)))
  nothing noTags

----------------------------------------
-- Join types test case helpers

testCaseJoinTypes :: TTerm JoinTypesTestCase -> TTerm TestCase
testCaseJoinTypes = inject _TestCase _TestCase_joinTypes

joinTypesTestCase :: TTerm Type -> TTerm Type -> TTerm (Either () [TypeConstraint]) -> TTerm JoinTypesTestCase
joinTypesTestCase left right expected = Phantoms.record _JoinTypesTestCase [
  _JoinTypesTestCase_left>>: left,
  _JoinTypesTestCase_right>>: right,
  _JoinTypesTestCase_expected>>: expected]

-- | Convenience function for creating join types test cases (expecting success)
joinTypesCase :: String -> TTerm Type -> TTerm Type -> TTerm [TypeConstraint] -> TTerm TestCaseWithMetadata
joinTypesCase cname left right constraints = testCaseWithMetadata (Phantoms.string cname)
  (testCaseJoinTypes $ joinTypesTestCase left right (Phantoms.right constraints))
  nothing noTags

-- | Convenience function for creating join types test cases (expecting failure)
joinTypesFailCase :: String -> TTerm Type -> TTerm Type -> TTerm TestCaseWithMetadata
joinTypesFailCase cname left right = testCaseWithMetadata (Phantoms.string cname)
  (testCaseJoinTypes $ joinTypesTestCase left right (Phantoms.left Phantoms.unit))
  nothing noTags
