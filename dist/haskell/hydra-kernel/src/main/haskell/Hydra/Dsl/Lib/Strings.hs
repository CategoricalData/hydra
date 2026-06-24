-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.lib.strings

module Hydra.Dsl.Lib.Strings where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Ast as DslAst
import qualified Hydra.Dsl.Coders as DslCoders
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Error.Checking as ErrorChecking
import qualified Hydra.Dsl.Error.Core as DslErrorCore
import qualified Hydra.Dsl.Error.File as DslErrorFile
import qualified Hydra.Dsl.Error.Packaging as DslErrorPackaging
import qualified Hydra.Dsl.Errors as DslErrors
import qualified Hydra.Dsl.File as DslFile
import qualified Hydra.Dsl.Graph as DslGraph
import qualified Hydra.Dsl.Json.Model as JsonModel
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Parsing as DslParsing
import qualified Hydra.Dsl.Paths as DslPaths
import qualified Hydra.Dsl.Query as DslQuery
import qualified Hydra.Dsl.Relational as DslRelational
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
import qualified Hydra.Errors as Errors
import qualified Hydra.File as File
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
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
-- | DSL reference to hydra.lib.strings.cat
cat :: Typed.TypedTerm [String] -> Typed.TypedTerm String
cat arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.cat")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
-- | DSL reference to hydra.lib.strings.cat2
cat2 :: Typed.TypedTerm String -> Typed.TypedTerm String -> Typed.TypedTerm String
cat2 arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.cat2")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))
-- | DSL reference to hydra.lib.strings.fromList
fromList :: Typed.TypedTerm [Int] -> Typed.TypedTerm String
fromList arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.fromList")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
-- | DSL reference to hydra.lib.strings.intercalate
intercalate :: Typed.TypedTerm String -> Typed.TypedTerm [String] -> Typed.TypedTerm String
intercalate arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.intercalate")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))
-- | DSL reference to hydra.lib.strings.length
length :: Typed.TypedTerm String -> Typed.TypedTerm Int
length arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.length")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
-- | DSL reference to hydra.lib.strings.lines
lines :: Typed.TypedTerm String -> Typed.TypedTerm [String]
lines arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.lines")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
-- | DSL reference to hydra.lib.strings.maybeCharAt
maybeCharAt :: Typed.TypedTerm Int -> Typed.TypedTerm String -> Typed.TypedTerm (Maybe Int)
maybeCharAt arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.maybeCharAt")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))
-- | DSL reference to hydra.lib.strings.null
null :: Typed.TypedTerm String -> Typed.TypedTerm Bool
null arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.null")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
-- | DSL reference to hydra.lib.strings.splitOn
splitOn :: Typed.TypedTerm String -> Typed.TypedTerm String -> Typed.TypedTerm [String]
splitOn arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.splitOn")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))
-- | DSL reference to hydra.lib.strings.toList
toList :: Typed.TypedTerm String -> Typed.TypedTerm [Int]
toList arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.toList")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
-- | DSL reference to hydra.lib.strings.toLower
toLower :: Typed.TypedTerm String -> Typed.TypedTerm String
toLower arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.toLower")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
-- | DSL reference to hydra.lib.strings.toUpper
toUpper :: Typed.TypedTerm String -> Typed.TypedTerm String
toUpper arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.toUpper")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
-- | DSL reference to hydra.lib.strings.unlines
unlines :: Typed.TypedTerm [String] -> Typed.TypedTerm String
unlines arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.unlines")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
