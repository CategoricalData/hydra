-- Note: this is an automatically generated file. Do not edit.
-- | A model for unit testing

module Hydra.Testing where
import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | A tag for test cases
newtype Tag =
  Tag {
    unTag :: String}
  deriving (Eq, Ord, Read, Show)
_Tag = Core.Name "hydra.testing.Tag"
-- | A test case with an actual and expected string for comparison
data TestCase =
  -- | A universal test case (string comparison)
  TestCaseUniversal UniversalTestCase
_TestCase = Core.Name "hydra.testing.TestCase"
_TestCase_universal = Core.Name "universal"
-- | A test case together with metadata
data TestCaseWithMetadata =
  TestCaseWithMetadata {
    -- | A short name for the test case
    testCaseWithMetadataName :: String,
    -- | The test case itself
    testCaseWithMetadataCase :: TestCase,
    -- | An optional longer description of the test case
    testCaseWithMetadataDescription :: (Maybe String),
    -- | Zero or more tags for the test case
    testCaseWithMetadataTags :: [Tag]}
_TestCaseWithMetadata = Core.Name "hydra.testing.TestCaseWithMetadata"
_TestCaseWithMetadata_name = Core.Name "name"
_TestCaseWithMetadata_case = Core.Name "case"
_TestCaseWithMetadata_description = Core.Name "description"
_TestCaseWithMetadata_tags = Core.Name "tags"
-- | A collection of test cases with a name and optional description
data TestGroup =
  TestGroup {
    -- | A short name for the test group
    testGroupName :: String,
    -- | An optional longer description of the test group
    testGroupDescription :: (Maybe String),
    -- | Zero or more subgroups
    testGroupSubgroups :: [TestGroup],
    -- | Zero or more test cases
    testGroupCases :: [TestCaseWithMetadata]}
_TestGroup = Core.Name "hydra.testing.TestGroup"
_TestGroup_name = Core.Name "name"
_TestGroup_description = Core.Name "description"
_TestGroup_subgroups = Core.Name "subgroups"
_TestGroup_cases = Core.Name "cases"
-- | A universal test case: the actual and expected values are thunks producing strings.
data UniversalTestCase =
  UniversalTestCase {
    -- | A thunk producing the actual result string. Wrapping in a thunk defers expression evaluation until the test runner forces it, so eagerly-evaluated hosts measure expression cost inside their timing bracket rather than at test-data load time.
    universalTestCaseActual :: (() -> String),
    -- | A thunk producing the expected result string.
    universalTestCaseExpected :: (() -> String)}
_UniversalTestCase = Core.Name "hydra.testing.UniversalTestCase"
_UniversalTestCase_actual = Core.Name "actual"
_UniversalTestCase_expected = Core.Name "expected"
