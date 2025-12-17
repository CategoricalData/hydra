
module Hydra.Sources.Eval.Lib.Tuples where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Monads as Monads


ns :: Namespace
ns = Namespace "hydra.eval.lib.tuples"

define :: String -> TTerm a -> TBinding a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [Monads.module_]
    kernelTypesModules $
    Just ("Evaluation-level implementations of Tuple functions for the Hydra interpreter.")
  where
    elements = [
      toBinding curry_,
      toBinding uncurry_]

-- | Interpreter-friendly curry for Tuple terms.
-- Transforms a function (pair a b) -> c into a -> b -> c.
-- curry f = \a -> \b -> f (a, b)
curry_ :: TBinding (Term -> Flow s Term)
curry_ = define "curry" $
  doc "Interpreter-friendly curry for Tuple terms." $
  "funTerm" ~>
  -- Build: \a -> \b -> funTerm (pair a b)
  produce $ Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "a") nothing $
    Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "b") nothing $
      Core.termApplication $ Core.application
        (var "funTerm")
        (Core.termPair $ pair
          (Core.termVariable $ wrap _Name $ string "a")
          (Core.termVariable $ wrap _Name $ string "b"))

-- | Interpreter-friendly uncurry for Tuple terms.
-- Transforms a function a -> b -> c into (pair a b) -> c.
-- uncurry f = \p -> f (fst p) (snd p)
uncurry_ :: TBinding (Term -> Flow s Term)
uncurry_ = define "uncurry" $
  doc "Interpreter-friendly uncurry for Tuple terms." $
  "funTerm" ~>
  -- Build: \p -> funTerm (fst p) (snd p)
  produce $ Core.termFunction $ Core.functionLambda $ Core.lambda (wrap _Name $ string "p") nothing $
    Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (var "funTerm")
        (Core.termApplication $ Core.application
          (Core.termFunction $ Core.functionPrimitive $ wrap _Name $ string "hydra.lib.pairs.first")
          (Core.termVariable $ wrap _Name $ string "p")))
      (Core.termApplication $ Core.application
        (Core.termFunction $ Core.functionPrimitive $ wrap _Name $ string "hydra.lib.pairs.second")
        (Core.termVariable $ wrap _Name $ string "p"))
