-- TODO: merge with Hydra.Dsl.Tests
module Hydra.Dsl.Meta.Testing where

import Hydra.Kernel
import Hydra.Json (Value)
import Hydra.Parsing (ParseResult)
import Hydra.Testing as Testing
import Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Dsl.Meta.Types as T

import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M

type Int32 = I.Int32


tag_disabled = Tag "disabled"
tag_disabledForMinimalInference = Tag "disabledForMinimalInference"
tag_requiresInterp = Tag "requiresInterp"

expectFailure i tags term = infFailureTest ("#" ++ show i) tags term

expectMono i tags term typ = infTest ("#" ++ show i) tags term $ T.mono typ

expectPoly i tags term params typ = infTest ("#" ++ show i) tags term $ T.poly params typ

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
isRequiresInterp tcase = tag_requiresInterp `L.elem` Testing.testCaseWithMetadataTags tcase

mapTerm :: [(TTerm Term, TTerm Term)] -> TTerm Term
mapTerm pairs = TTerm $ TermUnion $ Injection _Term $ Field _Term_map $ TermMap $ M.fromList [(unTTerm k, unTTerm v) | (k, v) <- pairs]

mapTermEmpty :: TTerm (M.Map k v)
mapTermEmpty = TTerm $ TermMap M.empty

subgroup :: String -> [TTerm TestCaseWithMetadata] -> TTerm TestGroup
subgroup name = tgroup name Nothing []

supergroup :: String -> [TTerm TestGroup] -> TTerm TestGroup
supergroup name subgroups = tgroup name Nothing subgroups []

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
    $ Just $ TypeScheme [] typ
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

testCaseCaseConversion :: TTerm CaseConversionTestCase -> TTerm TestCase
testCaseCaseConversion = inject _TestCase _TestCase_caseConversion

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

testCaseJsonParser :: TTerm JsonParserTestCase -> TTerm TestCase
testCaseJsonParser = inject _TestCase _TestCase_jsonParser

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
  nothing (Phantoms.list [])

-- | Convenience function for creating type reduction test cases
typeRedCase :: String -> TTerm Type -> TTerm Type -> TTerm TestCaseWithMetadata
typeRedCase cname input output = testCaseWithMetadata (Phantoms.string cname)
  (testCaseTypeReduction $ typeReductionTestCase input output)
  nothing (Phantoms.list [])

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
  nothing (Phantoms.list [])

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
  nothing (Phantoms.list [])

-- | Convenience function for creating topological sort SCC test cases
sortSCCCase :: String -> TTerm [(Int, [Int])] -> TTerm [[Int]] -> TTerm TestCaseWithMetadata
sortSCCCase cname adj expected = testCaseWithMetadata (Phantoms.string cname)
  (testCaseTopologicalSortSCC $ topologicalSortSCCTestCase adj expected)
  nothing (Phantoms.list [])

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
