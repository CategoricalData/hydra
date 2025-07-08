{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Show.Mantle where

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

import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations


module_ :: Module
module_ = Module (Namespace "hydra.show.mantle") elements
    [Annotations.module_]
    [KernelTypes.hydraComputeModule, KernelTypes.hydraGraphModule, KernelTypes.hydraMantleModule, KernelTypes.hydraTypingModule] $
    Just "String representations of hydra.mantle types"
  where
   elements = [
     el termVariantDef,
     el typeVariantDef]

define :: String -> TTerm a -> TElement a
define = definitionInModule module_

termVariantDef :: TElement (TermVariant -> String)
termVariantDef = define "termVariant" $
  doc "Show a term variant as a string" $
  match _TermVariant Nothing [
    _TermVariant_annotated>>: constant $ string "annotated",
    _TermVariant_application>>: constant $ string "application",
    _TermVariant_function>>: constant $ string "function",
    _TermVariant_let>>: constant $ string "let",
    _TermVariant_list>>: constant $ string "list",
    _TermVariant_literal>>: constant $ string "literal",
    _TermVariant_map>>: constant $ string "map",
    _TermVariant_optional>>: constant $ string "optional",
    _TermVariant_product>>: constant $ string "product",
    _TermVariant_record>>: constant $ string "record",
    _TermVariant_set>>: constant $ string "set",
    _TermVariant_sum>>: constant $ string "sum",
    _TermVariant_typeAbstraction>>: constant $ string "typeAbstraction",
    _TermVariant_typeApplication>>: constant $ string "typeApplication",
    _TermVariant_union>>: constant $ string "union",
    _TermVariant_unit>>: constant $ string "unit",
    _TermVariant_variable>>: constant $ string "variable",
    _TermVariant_wrap>>: constant $ string "wrap"]

typeVariantDef :: TElement (TypeVariant -> String)
typeVariantDef = define "typeVariant" $
  doc "Show a type variant as a string" $
  match _TypeVariant Nothing [
    _TypeVariant_annotated>>: constant $ string "annotated",
    _TypeVariant_application>>: constant $ string "application",
    _TypeVariant_forall>>: constant $ string "forall",
    _TypeVariant_function>>: constant $ string "function",
    _TypeVariant_list>>: constant $ string "list",
    _TypeVariant_literal>>: constant $ string "literal",
    _TypeVariant_map>>: constant $ string "map",
    _TypeVariant_optional>>: constant $ string "optional",
    _TypeVariant_product>>: constant $ string "product",
    _TypeVariant_record>>: constant $ string "record",
    _TypeVariant_set>>: constant $ string "set",
    _TypeVariant_sum>>: constant $ string "sum",
    _TypeVariant_union>>: constant $ string "union",
    _TypeVariant_unit>>: constant $ string "unit",
    _TypeVariant_variable>>: constant $ string "variable",
    _TypeVariant_wrap>>: constant $ string "wrap"]
