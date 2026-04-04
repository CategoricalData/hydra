
module Hydra.Sources.Eval.Lib.Math where

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
import qualified Hydra.Dsl.Meta.Literals as MetaLiterals
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
ns = Namespace "hydra.eval.lib.math"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns definitions
    []
    kernelTypesNamespaces $
    Just ("Evaluation-level implementations of Math functions for the Hydra interpreter.")
  where
    definitions = [
      toDefinition even_,
      toDefinition odd_,
      toDefinition pred_,
      toDefinition succ_]

-- | Interpreter-friendly even.
-- even x = equal (mod x 2) 0
even_ :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) Term)
even_ = define "even" $
  doc "Interpreter-friendly even." $
  "cx" ~> "g" ~>
  "x" ~>
  right $ Core.termApplication $ Core.application
    (Core.termApplication $ Core.application
      (Core.termFunction $ Core.functionPrimitive $ encodedName _equality_equal)
      (Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termFunction $ Core.functionPrimitive $ encodedName _math_mod)
          (var "x"))
        (Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ MetaLiterals.int32 2)))
    (Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ MetaLiterals.int32 0)

-- | Interpreter-friendly odd.
-- odd x = not (even x)
odd_ :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) Term)
odd_ = define "odd" $
  doc "Interpreter-friendly odd." $
  "cx" ~> "g" ~>
  "x" ~>
  right $ Core.termApplication $ Core.application
    (Core.termFunction $ Core.functionPrimitive $ encodedName _logic_not)
    (Core.termApplication $ Core.application
      (Core.termFunction $ Core.functionPrimitive $ encodedName _math_even)
      (var "x"))

-- | Interpreter-friendly predecessor.
-- pred x = sub x 1
pred_ :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) Term)
pred_ = define "pred" $
  doc "Interpreter-friendly predecessor." $
  "cx" ~> "g" ~>
  "x" ~>
  right $ Core.termApplication $ Core.application
    (Core.termApplication $ Core.application
      (Core.termFunction $ Core.functionPrimitive $ encodedName _math_sub)
      (var "x"))
    (Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ MetaLiterals.int32 1)

-- TODO: range lo hi = ifElse (gt lo hi) [] (cons lo (range (add lo 1) hi))
--   The recursive definition causes the interpreter to loop. The eval primitive returns a term
--   containing ifElse(gt(lo,hi), [], cons(lo, range(add(lo,1), hi))), but reduction does not
--   terminate. Needs investigation of the reducer's handling of recursive primitive references.
-- range_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)

-- | Interpreter-friendly successor.
-- succ x = add x 1
succ_ :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) Term)
succ_ = define "succ" $
  doc "Interpreter-friendly successor." $
  "cx" ~> "g" ~>
  "x" ~>
  right $ Core.termApplication $ Core.application
    (Core.termApplication $ Core.application
      (Core.termFunction $ Core.functionPrimitive $ encodedName _math_add)
      (var "x"))
    (Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 $ MetaLiterals.int32 1)
