-- Note: this is an automatically generated file. Do not edit.
-- | Transform test cases for code generation, filtering to tests that can be compiled to target languages

module Hydra.Test.Transform where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Add generation namespace prefix
addGenerationPrefix :: Packaging.ModuleName -> Packaging.ModuleName
addGenerationPrefix ns_ = Packaging.ModuleName (Strings.cat2 "generation." (Packaging.unModuleName ns_))
-- | Build a Term representing a convertCase function call
buildConvertCaseCall :: Util.CaseConvention -> Util.CaseConvention -> String -> Core.Term
buildConvertCaseCall fromConv toConv input_ =
    Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.formatting.convertCase")),
          Core.applicationArgument = (encodeCaseConvention fromConv)})),
        Core.applicationArgument = (encodeCaseConvention toConv)})),
      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString input_))})
-- | Build a Term representing a topologicalSort function call
buildTopologicalSortCall :: [(Int, [Int])] -> Core.Term
buildTopologicalSortCall adjList =
    Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.sorting.topologicalSort")),
      Core.applicationArgument = (encodeAdjacencyList adjList)})
-- | Build a Term representing a topologicalSortComponents function call
buildTopologicalSortSCCCall :: [(Int, [Int])] -> Core.Term
buildTopologicalSortSCCCall adjList =
    Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.sorting.topologicalSortComponents")),
      Core.applicationArgument = (encodeAdjacencyList adjList)})
-- | Collect all test cases from a test group (flattening hierarchy)
collectTestCases :: Testing.TestGroup -> [Testing.TestCaseWithMetadata]
collectTestCases tg =
    Lists.concat2 (Testing.testGroupCases tg) (Lists.concat (Lists.map (\sg -> collectTestCases sg) (Testing.testGroupSubgroups tg)))
-- | Encode an adjacency list as a Term
encodeAdjacencyList :: [(Int, [Int])] -> Core.Term
encodeAdjacencyList pairs =
    Core.TermList (Lists.map (\p -> Core.TermPair (encodeInt (Pairs.first p), (Core.TermList (Lists.map (\d -> encodeInt d) (Pairs.second p))))) pairs)
-- | Encode CaseConvention as a Term (unit variant)
encodeCaseConvention :: Util.CaseConvention -> Core.Term
encodeCaseConvention conv =
    Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.util.CaseConvention"),
      Core.injectionField = Core.Field {
        Core.fieldName = case conv of
          Util.CaseConventionLowerSnake -> Core.Name "lowerSnake"
          Util.CaseConventionUpperSnake -> Core.Name "upperSnake"
          Util.CaseConventionCamel -> Core.Name "camel"
          Util.CaseConventionPascal -> Core.Name "pascal",
        Core.fieldTerm = Core.TermUnit}})
-- | Encode Either [[Int]] [Int] as a Term
encodeEitherListList :: Either [[Int]] [Int] -> Core.Term
encodeEitherListList e =
    Core.TermEither (Eithers.bimap (\cycles -> encodeListList cycles) (\sorted -> encodeIntList sorted) e)
-- | Encode an Int as a Term
encodeInt :: Int -> Core.Term
encodeInt n = Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 n))
-- | Encode [Int] as a Term
encodeIntList :: [Int] -> Core.Term
encodeIntList ints = Core.TermList (Lists.map (\n -> encodeInt n) ints)
-- | Encode [[Int]] as a Term
encodeListList :: [[Int]] -> Core.Term
encodeListList lists = Core.TermList (Lists.map (\l -> encodeIntList l) lists)
-- | Transform module with generation namespace
transformModule :: Packaging.Module -> Packaging.Module
transformModule m =
    Packaging.Module {
      Packaging.moduleName = (addGenerationPrefix (Packaging.moduleName m)),
      Packaging.moduleMetadata = (Packaging.moduleMetadata m),
      Packaging.moduleDependencies = (Packaging.moduleDependencies m),
      Packaging.moduleDefinitions = (Packaging.moduleDefinitions m)}
-- | Pass through test cases unchanged
transformTestCase :: t0 -> Maybe t0
transformTestCase tcm = Just tcm
-- | Transform test group hierarchy to only include delegated evaluation tests
transformToCompiledTests :: Testing.TestGroup -> Maybe Testing.TestGroup
transformToCompiledTests tg =

      let name_ = Testing.testGroupName tg
          desc = Testing.testGroupDescription tg
          subgroups = Testing.testGroupSubgroups tg
          cases_ = Testing.testGroupCases tg
          transformedCases = Optionals.cat (Lists.map (\tc -> transformTestCase tc) cases_)
          transformedSubgroups = Optionals.cat (Lists.map (\sg -> transformToCompiledTests sg) subgroups)
      in (Logic.ifElse (Logic.and (Lists.null transformedCases) (Lists.null transformedSubgroups)) Nothing (Just (Testing.TestGroup {
        Testing.testGroupName = name_,
        Testing.testGroupDescription = desc,
        Testing.testGroupSubgroups = transformedSubgroups,
        Testing.testGroupCases = transformedCases})))
