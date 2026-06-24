-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.lib.optionals

module Hydra.Dsl.Lib.Optionals where
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
-- | DSL reference to hydra.lib.optionals.apply
apply :: Typed.TypedTerm (Maybe (x -> y)) -> Typed.TypedTerm (Maybe x) -> Typed.TypedTerm (Maybe y)
apply arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.apply")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))
-- | DSL reference to hydra.lib.optionals.bind
bind :: Typed.TypedTerm (Maybe x) -> Typed.TypedTerm (x -> Maybe y) -> Typed.TypedTerm (Maybe y)
bind arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.bind")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))
-- | DSL reference to hydra.lib.optionals.cases
cases :: Typed.TypedTerm (Maybe x) -> Typed.TypedTerm y -> Typed.TypedTerm (x -> y) -> Typed.TypedTerm y
cases arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.cases")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))
-- | DSL reference to hydra.lib.optionals.cat
cat :: Typed.TypedTerm [Maybe x] -> Typed.TypedTerm [x]
cat arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.cat")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
-- | DSL reference to hydra.lib.optionals.compose
compose :: Typed.TypedTerm (x -> Maybe y) -> Typed.TypedTerm (y -> Maybe z) -> Typed.TypedTerm x -> Typed.TypedTerm (Maybe z)
compose arg0 arg1 arg2 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.compose")),
          Core.applicationArgument = (Typed.unTypedTerm arg0)})),
        Core.applicationArgument = (Typed.unTypedTerm arg1)})),
      Core.applicationArgument = (Typed.unTypedTerm arg2)}))
-- | DSL reference to hydra.lib.optionals.fromOptional
fromOptional :: Typed.TypedTerm x -> Typed.TypedTerm (Maybe x) -> Typed.TypedTerm x
fromOptional arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.fromOptional")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))
-- | DSL reference to hydra.lib.optionals.isGiven
isGiven :: Typed.TypedTerm (Maybe x) -> Typed.TypedTerm Bool
isGiven arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.isGiven")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
-- | DSL reference to hydra.lib.optionals.isNone
isNone :: Typed.TypedTerm (Maybe x) -> Typed.TypedTerm Bool
isNone arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.isNone")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
-- | DSL reference to hydra.lib.optionals.map
map :: Typed.TypedTerm (x -> y) -> Typed.TypedTerm (Maybe x) -> Typed.TypedTerm (Maybe y)
map arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.map")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))
-- | DSL reference to hydra.lib.optionals.mapOptional
mapOptional :: Typed.TypedTerm (x -> Maybe y) -> Typed.TypedTerm [x] -> Typed.TypedTerm [y]
mapOptional arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.mapOptional")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))
-- | DSL reference to hydra.lib.optionals.pure
pure :: Typed.TypedTerm x -> Typed.TypedTerm (Maybe x)
pure arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.pure")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
-- | DSL reference to hydra.lib.optionals.toList
toList :: Typed.TypedTerm (Maybe x) -> Typed.TypedTerm [x]
toList arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.optionals.toList")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
