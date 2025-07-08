{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Show.Typing where

-- Standard Tier-2 imports
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
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations


module_ :: Module
module_ = Module (Namespace "hydra.show.typing") elements
    [Annotations.module_, ShowCore.module_]
    [KernelTypes.hydraComputeModule, KernelTypes.hydraGraphModule, KernelTypes.hydraMantleModule, KernelTypes.hydraTypingModule] $
    Just "String representations of hydra.typing types"
  where
   elements = [
     el typeConstraintDef,
     el typeSubstDef]

define :: String -> TTerm a -> TElement a
define = definitionInModule module_

typeConstraintDef :: TElement (TypeConstraint -> String)
typeConstraintDef = define "typeConstraint" $
  doc "Show a type constraint as a string" $
  lambda "tc" $ lets [
    "ltyp">: Typing.typeConstraintLeft $ var "tc",
    "rtyp">: Typing.typeConstraintRight $ var "tc"] $
    Strings.cat $ list [
      ref ShowCore.typeDef @@ var "ltyp",
      string "≡",
      ref ShowCore.typeDef @@ var "rtyp"]

typeSubstDef :: TElement (TypeSubst -> String)
typeSubstDef = define "typeSubst" $
  doc "Show a type substitution as a string" $
  lambda "ts" $ lets [
    "subst">: Typing.unTypeSubst $ var "ts",
    "pairs">: Maps.toList $ var "subst",
    "showPair">: lambda "pair" $ lets [
      "name">: unwrap _Name @@ (first $ var "pair"),
      "typ">: second $ var "pair"] $
      Strings.cat $ list [
        var "name",
        string "↦",
        ref ShowCore.typeDef @@ var "typ"],
    "pairStrs">: Lists.map (var "showPair") (var "pairs")] $
    Strings.cat $ list [
      string "{",
      Strings.intercalate (string ",") (var "pairStrs"),
      string "}"]
