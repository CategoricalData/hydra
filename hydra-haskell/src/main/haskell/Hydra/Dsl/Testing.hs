-- TODO: merge with Hydra.Dsl.Tests
module Hydra.Dsl.Testing where

import Hydra.Kernel
import Hydra.Testing as Testing
import Hydra.Dsl.Phantoms as Phantoms
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Dsl.Core as Core
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.TTerms as TTerms
import qualified Hydra.Dsl.TTypes as T

import qualified Data.List as L
import qualified Data.Map as M


tag_disabled = Tag "disabled"
tag_disabledForMinimalInference = Tag "disabledForMinimalInference"

expectMono i tags term typ = infTest ("#" ++ show i) tags term $ T.mono typ

expectPoly i tags term params typ = infTest ("#" ++ show i) tags term $ T.poly params typ

groupRef = TTerms.varNamePhantom . elementName

infTest :: String -> [Tag] -> TTerm Term -> TTerm TypeScheme -> TTerm TestCaseWithMetadata
infTest name tags term ts = testCaseWithMetadata (Phantoms.string name)
  (testCaseInference $ inferenceTestCase term ts) nothing (Phantoms.list $ tag . unTag <$> tags)

isDisabled tcase = tag_disabled `L.elem` Testing.testCaseWithMetadataTags tcase
isDisabledForMinimalInference tcase = tag_disabledForMinimalInference `L.elem` Testing.testCaseWithMetadataTags tcase

-- Note: this is a cheat for an encoded map term; consider using the TTerms DSL
mapTermCheat :: [(Term, Term)] -> TTerm Term
mapTermCheat = TTerm . EncodeCore.term . Terms.map . M.fromList

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
  TestCaseCaseConversion ccase -> Terms.variant _TestCase _TestCase_caseConversion $ encodeCaseConversionTestCase ccase
  TestCaseEvaluation ecase -> Terms.variant _TestCase _TestCase_evaluation $ encodeEvaluationTestCase ecase
  TestCaseInference icase -> Terms.variant _TestCase _TestCase_inference $ encodeInferenceTestCase icase
encodeCaseConvention c = Terms.unitVariant _CaseConvention $ case c of
  CaseConventionLowerSnake -> _CaseConvention_lowerSnake
  CaseConventionUpperSnake -> _CaseConvention_upperSnake
  CaseConventionCamel -> _CaseConvention_camel
  CaseConventionPascal -> _CaseConvention_pascal
encodeCaseConversionTestCase (CaseConversionTestCase fromConvention toConvention fromString toString) = Terms.record _CaseConversionTestCase [
  Field _CaseConversionTestCase_fromConvention $ encodeCaseConvention fromConvention,
  Field _CaseConversionTestCase_toConvention $ encodeCaseConvention toConvention,
  Field _CaseConversionTestCase_fromString $ Terms.string fromString,
  Field _CaseConversionTestCase_toString $ Terms.string toString]
encodeEvaluationTestCase (EvaluationTestCase style input output) = Terms.record _EvaluationTestCase [
  Field _EvaluationTestCase_evaluationStyle $ Terms.variant _EvaluationStyle (case style of
    EvaluationStyleEager -> _EvaluationStyle_eager
    EvaluationStyleLazy -> _EvaluationStyle_lazy) Terms.unit,
  Field _EvaluationTestCase_input $ EncodeCore.term input,
  Field _EvaluationTestCase_output $ EncodeCore.term output]
encodeInferenceTestCase (InferenceTestCase input output) = Terms.record _InferenceTestCase [
  Field _InferenceTestCase_input $ EncodeCore.term input,
  Field _InferenceTestCase_output $ EncodeCore.typeScheme output]

----------------------------------------

encodedTestGroupToElement :: Namespace -> String -> TTerm TestGroup -> Element
encodedTestGroupToElement ns lname group = Element name (unTTerm group)
    $ Just $ TypeScheme [] typ
  where
    name = unqualifyName $ QualifiedName (Just ns) lname
    typ = TypeVariable _TestGroup

inferenceTestCase :: TTerm Term -> TTerm TypeScheme -> TTerm InferenceTestCase
inferenceTestCase input output = Phantoms.record _InferenceTestCase [
  _InferenceTestCase_input>>: input,
  _InferenceTestCase_output>>: output]

testCaseInference :: TTerm InferenceTestCase -> TTerm TestCase
testCaseInference = variant _TestCase _TestCase_inference

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

testGroupToElement :: Namespace -> String -> TestGroup -> Element
testGroupToElement ns lname group = encodedTestGroupToElement ns lname (TTerm $ encodeGroup group)
