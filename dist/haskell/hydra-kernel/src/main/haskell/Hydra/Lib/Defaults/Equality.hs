-- Note: this is an automatically generated file. Do not edit.
-- | Default term-level implementations of Equality functions for the Hydra interpreter.

module Hydra.Lib.Defaults.Equality where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
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
-- | Interpreter-friendly identity function.
identity :: t0 -> t1 -> t2 -> Either t3 t2
identity cx g x = Right x
-- | Interpreter-friendly max.
max :: t0 -> t1 -> Core.Term -> Core.Term -> Either t2 Core.Term
max cx g x y =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.gte")),
              Core.applicationArgument = x})),
            Core.applicationArgument = y}))})),
        Core.applicationArgument = x})),
      Core.applicationArgument = y}))
-- | Interpreter-friendly min.
min :: t0 -> t1 -> Core.Term -> Core.Term -> Either t2 Core.Term
min cx g x y =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.lte")),
              Core.applicationArgument = x})),
            Core.applicationArgument = y}))})),
        Core.applicationArgument = x})),
      Core.applicationArgument = y}))
