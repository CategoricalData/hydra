-- | A model for unit testing

module Hydra.Testing where

import qualified Hydra.Core as Core
import Data.List
import Data.Map
import Data.Set

-- | A simple test case with an input and an expected output
data TestCase a = 
  TestCase {
    testCaseDescription :: (Maybe String),
    testCaseInput :: (Core.Term a),
    testCaseOutput :: (Core.Term a)}
  deriving (Eq, Ord, Read, Show)

_TestCase = (Core.Name "hydra/testing.TestCase")

_TestCase_description = (Core.FieldName "description")

_TestCase_input = (Core.FieldName "input")

_TestCase_output = (Core.FieldName "output")

-- | A collection of test cases with a name and optional description
data TestGroup a = 
  TestGroup {
    testGroupName :: String,
    testGroupDescription :: (Maybe String),
    testGroupSubgroups :: [TestGroup a],
    testGroupCases :: [TestCase a]}
  deriving (Eq, Ord, Read, Show)

_TestGroup = (Core.Name "hydra/testing.TestGroup")

_TestGroup_name = (Core.FieldName "name")

_TestGroup_description = (Core.FieldName "description")

_TestGroup_subgroups = (Core.FieldName "subgroups")

_TestGroup_cases = (Core.FieldName "cases")