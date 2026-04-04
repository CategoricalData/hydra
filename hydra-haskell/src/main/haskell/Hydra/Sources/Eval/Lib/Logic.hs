
module Hydra.Sources.Eval.Lib.Logic where

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
ns = Namespace "hydra.eval.lib.logic"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    []
    kernelTypesNamespaces $
    Just ("Evaluation-level implementations of Logic functions for the Hydra interpreter.")
  where
    elements = [
      toDefinition and_,
      toDefinition not_,
      toDefinition or_]

-- | Interpreter-friendly logical AND.
-- and a b = ifElse a b false
and_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
and_ = define "and" $
  doc "Interpreter-friendly logical AND." $
  "cx" ~> "g" ~>
  "a" ~> "b" ~>
  right $ Core.termApplication $ Core.application
    (Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (Core.termFunction $ Core.functionPrimitive $ encodedName _logic_ifElse)
        (var "a"))
      (var "b"))
    (Core.termLiteral $ Core.literalBoolean $ MetaLiterals.boolean False)

-- | Interpreter-friendly logical NOT.
-- not a = ifElse a false true
not_ :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) Term)
not_ = define "not" $
  doc "Interpreter-friendly logical NOT." $
  "cx" ~> "g" ~>
  "a" ~>
  right $ Core.termApplication $ Core.application
    (Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (Core.termFunction $ Core.functionPrimitive $ encodedName _logic_ifElse)
        (var "a"))
      (Core.termLiteral $ Core.literalBoolean $ MetaLiterals.boolean False))
    (Core.termLiteral $ Core.literalBoolean $ MetaLiterals.boolean True)

-- | Interpreter-friendly logical OR.
-- or a b = ifElse a true b
or_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
or_ = define "or" $
  doc "Interpreter-friendly logical OR." $
  "cx" ~> "g" ~>
  "a" ~> "b" ~>
  right $ Core.termApplication $ Core.application
    (Core.termApplication $ Core.application
      (Core.termApplication $ Core.application
        (Core.termFunction $ Core.functionPrimitive $ encodedName _logic_ifElse)
        (var "a"))
      (Core.termLiteral $ Core.literalBoolean $ MetaLiterals.boolean True))
    (var "b")
