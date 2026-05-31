-- Note: this is an automatically generated file. Do not edit.
-- | A module defining the graph used in the test suite.

module Hydra.Test.TestGraph where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Test.TestEnv as TestEnv
import qualified Hydra.Test.TestTerms as TestTerms
import qualified Hydra.Test.TestTypes as TestTypes
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
testContext :: Typing.InferenceContext
testContext = TestEnv.testContext
testGraph :: Graph.Graph
testGraph = TestEnv.testGraph testTypes testTerms
testModuleName :: Packaging.ModuleName
testModuleName = Packaging.ModuleName "testGraph"
testSchemaModuleName :: Packaging.ModuleName
testSchemaModuleName = Packaging.ModuleName "testSchemaGraph"
testTerms :: M.Map Core.Name Core.Term
testTerms = Maps.fromList [
  (Core.Name "testDataArthur", TestTerms.testDataArthur)]
testTypes :: M.Map Core.Name Core.Type
testTypes =
    Maps.fromList [
      (TestTypes.testTypeBuddyListAName, TestTypes.testTypeBuddyListA),
      (TestTypes.testTypeBuddyListBName, TestTypes.testTypeBuddyListB),
      (TestTypes.testTypeComparisonName, TestTypes.testTypeComparison),
      (TestTypes.testTypeEitherName, TestTypes.testTypeEither),
      (TestTypes.testTypeHydraLiteralTypeName, TestTypes.testTypeHydraLiteralType),
      (TestTypes.testTypeHydraTypeName, TestTypes.testTypeHydraType),
      (TestTypes.testTypeIntListName, TestTypes.testTypeIntList),
      (TestTypes.testTypeLatLonName, TestTypes.testTypeLatLon),
      (TestTypes.testTypeLatLonPolyName, TestTypes.testTypeLatLonPoly),
      (TestTypes.testTypeListName, TestTypes.testTypeList),
      (TestTypes.testTypeNumberName, TestTypes.testTypeNumber),
      (TestTypes.testTypePersonName, TestTypes.testTypePerson),
      (TestTypes.testTypePersonOrSomethingName, TestTypes.testTypePersonOrSomething),
      (TestTypes.testTypePolymorphicWrapperName, TestTypes.testTypePolymorphicWrapper),
      (TestTypes.testTypeSimpleNumberName, TestTypes.testTypeSimpleNumber),
      (TestTypes.testTypeStringAliasName, TestTypes.testTypeStringAlias),
      (TestTypes.testTypeSymmetricTripleName, TestTypes.testTypeSymmetricTriple),
      (TestTypes.testTypeTimestampName, TestTypes.testTypeTimestamp),
      (TestTypes.testTypeTripleName, TestTypes.testTypeTriple),
      (TestTypes.testTypeUnionMonomorphicName, TestTypes.testTypeUnionMonomorphic),
      (TestTypes.testTypeUnionPolymorphicRecursiveName, TestTypes.testTypeUnionPolymorphicRecursive),
      (TestTypes.testTypeUnitName, TestTypes.testTypeUnit)]
