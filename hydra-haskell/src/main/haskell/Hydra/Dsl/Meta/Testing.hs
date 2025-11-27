-- TODO: merge with Hydra.Dsl.Tests
module Hydra.Dsl.Meta.Testing where

import Hydra.Kernel
import Hydra.Testing as Testing
import Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Dsl.Meta.Types as T

import qualified Data.List as L
import qualified Data.Map as M


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
