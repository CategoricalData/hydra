-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.test.transform

module Hydra.Dsl.Test.Transform where

import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Docs as Docs
import qualified Hydra.Dsl.Ast as DslAst
import qualified Hydra.Dsl.Coders as DslCoders
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Docs as DslDocs
import qualified Hydra.Dsl.Error.Checking as ErrorChecking
import qualified Hydra.Dsl.Error.Core as DslErrorCore
import qualified Hydra.Dsl.Error.File as DslErrorFile
import qualified Hydra.Dsl.Error.Packaging as DslErrorPackaging
import qualified Hydra.Dsl.Error.System as DslErrorSystem
import qualified Hydra.Dsl.Errors as DslErrors
import qualified Hydra.Dsl.File as DslFile
import qualified Hydra.Dsl.Formatting as DslFormatting
import qualified Hydra.Dsl.Graph as DslGraph
import qualified Hydra.Dsl.Json.Model as JsonModel
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Parsing as DslParsing
import qualified Hydra.Dsl.Paths as DslPaths
import qualified Hydra.Dsl.Query as DslQuery
import qualified Hydra.Dsl.Relational as DslRelational
import qualified Hydra.Dsl.Sorting as DslSorting
import qualified Hydra.Dsl.System as DslSystem
import qualified Hydra.Dsl.Tabular as DslTabular
import qualified Hydra.Dsl.Testing as DslTesting
import qualified Hydra.Dsl.Time as DslTime
import qualified Hydra.Dsl.Topology as DslTopology
import qualified Hydra.Dsl.Typing as DslTyping
import qualified Hydra.Dsl.Util as DslUtil
import qualified Hydra.Dsl.Validation as DslValidation
import qualified Hydra.Dsl.Variants as DslVariants
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.File as ErrorFile
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Error.System as ErrorSystem
import qualified Hydra.Errors as Errors
import qualified Hydra.File as File
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Sorting as Sorting
import qualified Hydra.System as System
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Test.Transform as Transform
import qualified Hydra.Testing as Testing
import qualified Hydra.Time as Time
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | DSL reference to hydra.test.transform.addGenerationPrefix
addGenerationPrefix :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm Packaging.ModuleName
addGenerationPrefix arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.test.transform.addGenerationPrefix")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.test.transform.buildConvertCaseCall
buildConvertCaseCall :: Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm String -> Typed.TypedTerm Core.Term
buildConvertCaseCall arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.test.transform.buildConvertCaseCall")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))

-- | DSL reference to hydra.test.transform.buildTopologicalSortCall
buildTopologicalSortCall :: Typed.TypedTerm [(Int, [Int])] -> Typed.TypedTerm Core.Term
buildTopologicalSortCall arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.test.transform.buildTopologicalSortCall")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.test.transform.buildTopologicalSortSCCCall
buildTopologicalSortSCCCall :: Typed.TypedTerm [(Int, [Int])] -> Typed.TypedTerm Core.Term
buildTopologicalSortSCCCall arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.test.transform.buildTopologicalSortSCCCall")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.test.transform.collectTestCases
collectTestCases :: Typed.TypedTerm Testing.TestGroup -> Typed.TypedTerm [Testing.TestCaseWithMetadata]
collectTestCases arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.test.transform.collectTestCases")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.test.transform.encodeAdjacencyList
encodeAdjacencyList :: Typed.TypedTerm [(Int, [Int])] -> Typed.TypedTerm Core.Term
encodeAdjacencyList arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.test.transform.encodeAdjacencyList")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.test.transform.encodeCaseConvention
encodeCaseConvention :: Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Core.Term
encodeCaseConvention arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.test.transform.encodeCaseConvention")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.test.transform.encodeEitherListList
encodeEitherListList :: Typed.TypedTerm (Either [[Int]] [Int]) -> Typed.TypedTerm Core.Term
encodeEitherListList arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.test.transform.encodeEitherListList")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.test.transform.encodeInt
encodeInt :: Typed.TypedTerm Int -> Typed.TypedTerm Core.Term
encodeInt arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.test.transform.encodeInt")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.test.transform.encodeIntList
encodeIntList :: Typed.TypedTerm [Int] -> Typed.TypedTerm Core.Term
encodeIntList arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.test.transform.encodeIntList")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.test.transform.encodeListList
encodeListList :: Typed.TypedTerm [[Int]] -> Typed.TypedTerm Core.Term
encodeListList arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.test.transform.encodeListList")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.test.transform.transformModule
transformModule :: Typed.TypedTerm Packaging.Module -> Typed.TypedTerm Packaging.Module
transformModule arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.test.transform.transformModule")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.test.transform.transformTestCase
transformTestCase :: Typed.TypedTerm t0 -> Typed.TypedTerm (Maybe t0)
transformTestCase arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.test.transform.transformTestCase")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.test.transform.transformToCompiledTests
transformToCompiledTests :: Typed.TypedTerm Testing.TestGroup -> Typed.TypedTerm (Maybe Testing.TestGroup)
transformToCompiledTests arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.test.transform.transformToCompiledTests")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
