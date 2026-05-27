-- Note: this is an automatically generated file. Do not edit.
-- | Default term-level implementations of Logic functions for the Hydra interpreter.

module Hydra.Lib.Defaults.Logic where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
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
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Interpreter-friendly logical AND.
and :: t0 -> t1 -> Core.Term -> Core.Term -> Either t2 Core.Term
and cx g a b =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
          Core.applicationArgument = a})),
        Core.applicationArgument = b})),
      Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))}))
-- | Interpreter-friendly logical NOT.
not :: t0 -> t1 -> Core.Term -> Either t2 Core.Term
not cx g a =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
          Core.applicationArgument = a})),
        Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})),
      Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))}))
-- | Interpreter-friendly logical OR.
or :: t0 -> t1 -> Core.Term -> Core.Term -> Either t2 Core.Term
or cx g a b =
    Right (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
          Core.applicationArgument = a})),
        Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
      Core.applicationArgument = b}))
