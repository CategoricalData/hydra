
module Hydra.Sources.Eval.Lib.Equality where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths     as Paths
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Util       as Util
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Json.Model          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
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
import qualified Hydra.Dsl.Packaging        as Packaging
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Meta.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import qualified Hydra.Dsl.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


ns :: Namespace
ns = Namespace "hydra.eval.lib.equality"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns definitions
    []
    kernelTypesNamespaces $
    Just ("Evaluation-level implementations of Equality functions for the Hydra interpreter.")
  where
    definitions = [
      toDefinition identity_,
      toDefinition max_,
      toDefinition min_]

-- | Interpreter-friendly identity function.
-- identity x = x
identity_ :: TTermDefinition (Context -> Graph -> Term -> Either Error Term)
identity_ = define "identity" $
  doc "Interpreter-friendly identity function." $
  "cx" ~> "g" ~>
  "x" ~>
  right $ var "x"

-- | Interpreter-friendly max.
-- max x y = ifElse (gte x y) x y
max_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either Error Term)
max_ = define "max" $
  doc "Interpreter-friendly max." $
  "cx" ~> "g" ~>
  "x" ~> "y" ~>
  right $ Core.termApplication $ Core.application
    (Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (Core.termVariable $ encodedName _logic_ifElse)
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termVariable $ encodedName _equality_gte)
            (var "x"))
          (var "y")))
      (var "x"))
    (var "y")

-- | Interpreter-friendly min.
-- min x y = ifElse (lte x y) x y
min_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either Error Term)
min_ = define "min" $
  doc "Interpreter-friendly min." $
  "cx" ~> "g" ~>
  "x" ~> "y" ~>
  right $ Core.termApplication $ Core.application
    (Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (Core.termVariable $ encodedName _logic_ifElse)
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termVariable $ encodedName _equality_lte)
            (var "x"))
          (var "y")))
      (var "x"))
    (var "y")
