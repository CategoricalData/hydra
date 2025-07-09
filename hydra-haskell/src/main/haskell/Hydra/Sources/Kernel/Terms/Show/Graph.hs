{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Show.Graph where

-- Standard imports for term-level kernel modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations


module_ :: Module
module_ = Module (Namespace "hydra.show.graph") elements
    [Annotations.module_, ShowCore.module_]
    kernelTypesModules $
    Just "String representations of hydra.graph types"
  where
   elements = [
     el elementDef,
     el graphDef]

define :: String -> TTerm a -> TElement a
define = definitionInModule module_

elementDef :: TElement (Element -> String)
elementDef = define "element" $
  doc "Show an element as a string" $
  lambda "el" $ lets [
    "name">: unwrap _Name @@ (Graph.elementName $ var "el"),
    "t">: Graph.elementTerm $ var "el",
    "typeStr">: Optionals.maybe
      (string "")
      (lambda "ts" $ Strings.cat2 (string " : ") (ref ShowCore.typeSchemeDef @@ var "ts"))
      (Graph.elementType $ var "el")] $
    Strings.cat $ list [
      var "name",
      string " = ",
      ref ShowCore.termDef @@ var "t",
      var "typeStr"]

graphDef :: TElement (Graph -> String)
graphDef = define "graph" $
  doc "Show a graph as a string" $
  lambda "graph" $ lets [
    "elements">: Maps.elems $ Graph.graphElements $ var "graph",
    "elementStrs">: Lists.map (ref elementDef) (var "elements")] $
    Strings.cat $ list [
      string "{",
      Strings.intercalate (string ", ") (var "elementStrs"),
      string "}"]
