
module Hydra.Sources.Eval.Lib.Sets where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (map)
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
import           Prelude hiding ((++), map)
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore

ns :: Namespace
ns = Namespace "hydra.eval.lib.sets"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns definitions
    [ExtractCore.ns, ShowCore.ns]
    kernelTypesNamespaces $
    Just ("Evaluation-level implementations of Set functions for the Hydra interpreter.")
  where
    definitions = [
      toDefinition difference_,
      toDefinition intersection_,
      toDefinition map_,
      toDefinition union_,
      toDefinition unions_]

-- | Interpreter-friendly set difference.
-- difference s1 s2: elements in s1 that are not in s2.
difference_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
difference_ = define "difference" $
  doc "Interpreter-friendly set difference." $
  "cx" ~> "g" ~>
  "set1Term" ~> "set2Term" ~>
  "elements" <<~ (ExtractCore.set @@ var "cx" @@ var "g" @@ var "set1Term") $
  -- Build: foldl (\acc el -> ifElse (member el set2) acc (insert el acc)) empty (toList set1)
  right $ Lists.foldl
    ("acc" ~> "el" ~>
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termFunction $ Core.functionPrimitive $ encodedName _logic_ifElse)
            (Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termFunction $ Core.functionPrimitive $ encodedName _sets_member)
                (var "el"))
              (var "set2Term")))
          (var "acc"))
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termFunction $ Core.functionPrimitive $ encodedName _sets_insert)
            (var "el"))
          (var "acc")))
    (Core.termSet $ Sets.fromList (list ([] :: [TTerm Term])))
    (Sets.toList $ var "elements")

-- | Interpreter-friendly set intersection.
-- intersection s1 s2: elements in both s1 and s2.
intersection_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
intersection_ = define "intersection" $
  doc "Interpreter-friendly set intersection." $
  "cx" ~> "g" ~>
  "set1Term" ~> "set2Term" ~>
  "elements" <<~ (ExtractCore.set @@ var "cx" @@ var "g" @@ var "set1Term") $
  -- Build: foldl (\acc el -> ifElse (member el set2) (insert el acc) acc) empty (toList set1)
  right $ Lists.foldl
    ("acc" ~> "el" ~>
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termApplication $ Core.application
            (Core.termFunction $ Core.functionPrimitive $ encodedName _logic_ifElse)
            (Core.termApplication $ Core.application
              (Core.termApplication $ Core.application
                (Core.termFunction $ Core.functionPrimitive $ encodedName _sets_member)
                (var "el"))
              (var "set2Term")))
          (Core.termApplication $ Core.application
            (Core.termApplication $ Core.application
              (Core.termFunction $ Core.functionPrimitive $ encodedName _sets_insert)
              (var "el"))
            (var "acc")))
        (var "acc"))
    (Core.termSet $ Sets.fromList (list ([] :: [TTerm Term])))
    (Sets.toList $ var "elements")

-- | Interpreter-friendly map for Set terms.
-- Applies fun to each element.
map_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
map_ = define "map" $
  doc "Interpreter-friendly map for Set terms." $
  "cx" ~> "g" ~>
  "fun" ~> "setTerm" ~>
  "elements" <<~ (ExtractCore.set @@ var "cx" @@ var "g" @@ var "setTerm") $
  -- Build: fromList (map fun (toList elements))
  right $ Core.termApplication $ Core.application
    (Core.termFunction $ Core.functionPrimitive $ encodedName _sets_fromList)
    (Core.termList $ Lists.map
      ("el" ~> Core.termApplication $ Core.application (var "fun") (var "el"))
      (Sets.toList $ var "elements"))

-- | Interpreter-friendly set union.
-- union s1 s2: elements in either s1 or s2.
union_ :: TTermDefinition (Context -> Graph -> Term -> Term -> Either (InContext Error) Term)
union_ = define "union" $
  doc "Interpreter-friendly set union." $
  "cx" ~> "g" ~>
  "set1Term" ~> "set2Term" ~>
  "elements" <<~ (ExtractCore.set @@ var "cx" @@ var "g" @@ var "set1Term") $
  -- Build: foldl (\acc el -> insert el acc) set2 (toList set1)
  right $ Lists.foldl
    ("acc" ~> "el" ~>
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termFunction $ Core.functionPrimitive $ encodedName _sets_insert)
          (var "el"))
        (var "acc"))
    (var "set2Term")
    (Sets.toList $ var "elements")

-- | Interpreter-friendly unions for list of Set terms.
-- unions [s1, s2, ...]: union of all sets.
unions_ :: TTermDefinition (Context -> Graph -> Term -> Either (InContext Error) Term)
unions_ = define "unions" $
  doc "Interpreter-friendly unions for list of Set terms." $
  "cx" ~> "g" ~>
  "listTerm" ~>
  "elements" <<~ (ExtractCore.list @@ var "cx" @@ var "g" @@ var "listTerm") $
  -- Build: foldl union empty sets
  right $ Lists.foldl
    ("acc" ~> "s" ~>
      Core.termApplication $ Core.application
        (Core.termApplication $ Core.application
          (Core.termFunction $ Core.functionPrimitive $ encodedName _sets_union)
          (var "acc"))
        (var "s"))
    (Core.termSet $ Sets.fromList (list ([] :: [TTerm Term])))
    (var "elements")
