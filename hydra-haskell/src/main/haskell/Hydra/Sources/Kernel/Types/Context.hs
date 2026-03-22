module Hydra.Sources.Kernel.Types.Context where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.context"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns (map toTypeDef elements) [Core.ns] [Core.ns] $
    Just "Execution context for tracing and diagnostics"
  where
    elements = [
      context,
      inContext]

context :: Binding
context = define "Context" $
  doc "An execution context for tracing and diagnostics, threaded through function calls" $
  T.record [
    "trace">:
      doc "A stack of context labels describing the current execution path" $
      T.list T.string,
    "messages">:
      doc "A log of warnings and/or info messages" $
      T.list T.string,
    "other">:
      doc "A map of string keys to arbitrary terms as values, for application-specific use" $
      T.map Core.name Core.term]

inContext :: Binding
inContext = define "InContext" $
  doc "A particular domain object (such as an error) together with an execution context" $
  T.forAll "e" $ T.record [
    "object">:
      doc "A domain object; typically an error" $
      T.var "e",
    "context">:
      doc "The execution context at the point of capture" $
      context]
