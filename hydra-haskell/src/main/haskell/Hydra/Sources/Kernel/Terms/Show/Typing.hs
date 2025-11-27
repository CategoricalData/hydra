{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Show.Typing where

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

import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations


module_ :: Module
module_ = Module (Namespace "hydra.show.typing") elements
    [Annotations.module_, ShowCore.module_]
    kernelTypesModules $
    Just "String representations of hydra.typing types"
  where
   elements = [
     el typeConstraintDef,
     el typeSubstDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

typeConstraintDef :: TBinding (TypeConstraint -> String)
typeConstraintDef = define "typeConstraint" $
  doc "Show a type constraint as a string" $
  lambda "tc" $ lets [
    "ltyp">: Typing.typeConstraintLeft $ var "tc",
    "rtyp">: Typing.typeConstraintRight $ var "tc"] $
    Strings.cat $ list [
      ref ShowCore.typeDef @@ var "ltyp",
      string "≡",
      ref ShowCore.typeDef @@ var "rtyp"]

typeSubstDef :: TBinding (TypeSubst -> String)
typeSubstDef = define "typeSubst" $
  doc "Show a type substitution as a string" $
  lambda "ts" $ lets [
    "subst">: Typing.unTypeSubst $ var "ts",
    "pairs">: Maps.toList $ var "subst",
    "showPair">: lambda "pair" $ lets [
      "name">: unwrap _Name @@ (Pairs.first $ var "pair"),
      "typ">: Pairs.second $ var "pair"] $
      Strings.cat $ list [
        var "name",
        string "↦",
        ref ShowCore.typeDef @@ var "typ"],
    "pairStrs">: Lists.map (var "showPair") (var "pairs")] $
    Strings.cat $ list [
      string "{",
      Strings.intercalate (string ",") (var "pairStrs"),
      string "}"]
