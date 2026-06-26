-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.lib.sets

module Hydra.Dsl.Lib.Sets where
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
import qualified Data.Set as S
-- | DSL reference to hydra.lib.sets.delete
delete :: Ord x => (Typed.TypedTerm x -> Typed.TypedTerm (S.Set x) -> Typed.TypedTerm (S.Set x))
delete arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.delete")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))
-- | DSL reference to hydra.lib.sets.difference
difference :: Ord x => (Typed.TypedTerm (S.Set x) -> Typed.TypedTerm (S.Set x) -> Typed.TypedTerm (S.Set x))
difference arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.difference")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))
-- | DSL reference to hydra.lib.sets.empty
empty :: Ord x => (Typed.TypedTerm (S.Set x))
empty = Typed.TypedTerm (Core.TermVariable (Core.Name "hydra.lib.sets.empty"))
-- | DSL reference to hydra.lib.sets.fromList
fromList :: Ord x => (Typed.TypedTerm [x] -> Typed.TypedTerm (S.Set x))
fromList arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.fromList")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
-- | DSL reference to hydra.lib.sets.insert
insert :: Ord x => (Typed.TypedTerm x -> Typed.TypedTerm (S.Set x) -> Typed.TypedTerm (S.Set x))
insert arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.insert")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))
-- | DSL reference to hydra.lib.sets.intersection
intersection :: Ord x => (Typed.TypedTerm (S.Set x) -> Typed.TypedTerm (S.Set x) -> Typed.TypedTerm (S.Set x))
intersection arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.intersection")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))
-- | DSL reference to hydra.lib.sets.map
map :: (Ord x, Ord y) => (Typed.TypedTerm (x -> y) -> Typed.TypedTerm (S.Set x) -> Typed.TypedTerm (S.Set y))
map arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.map")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))
-- | DSL reference to hydra.lib.sets.member
member :: Ord x => (Typed.TypedTerm x -> Typed.TypedTerm (S.Set x) -> Typed.TypedTerm Bool)
member arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.member")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))
-- | DSL reference to hydra.lib.sets.null
null :: Ord x => (Typed.TypedTerm (S.Set x) -> Typed.TypedTerm Bool)
null arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.null")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
-- | DSL reference to hydra.lib.sets.singleton
singleton :: Ord x => (Typed.TypedTerm x -> Typed.TypedTerm (S.Set x))
singleton arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.singleton")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
-- | DSL reference to hydra.lib.sets.size
size :: Ord x => (Typed.TypedTerm (S.Set x) -> Typed.TypedTerm Int)
size arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.size")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
-- | DSL reference to hydra.lib.sets.toList
toList :: Ord x => (Typed.TypedTerm (S.Set x) -> Typed.TypedTerm [x])
toList arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.toList")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
-- | DSL reference to hydra.lib.sets.union
union :: Ord x => (Typed.TypedTerm (S.Set x) -> Typed.TypedTerm (S.Set x) -> Typed.TypedTerm (S.Set x))
union arg0 arg1 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.union")),
        Core.applicationArgument = (Typed.unTypedTerm arg0)})),
      Core.applicationArgument = (Typed.unTypedTerm arg1)}))
-- | DSL reference to hydra.lib.sets.unions
unions :: Ord x => (Typed.TypedTerm [S.Set x] -> Typed.TypedTerm (S.Set x))
unions arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.unions")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
