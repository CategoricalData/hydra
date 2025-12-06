{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Show.Meta where

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


module_ :: Module
module_ = Module (Namespace "hydra.show.meta") elements
    []
    kernelTypesModules $
    Just "String representations of hydra.meta types"
  where
   elements = [
     el termVariantDef,
     el typeVariantDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

termVariantDef :: TBinding (TermVariant -> String)
termVariantDef = define "termVariant" $
  doc "Show a term variant as a string" $
  match _TermVariant Nothing [
    _TermVariant_annotated>>: constant $ string "annotated",
    _TermVariant_application>>: constant $ string "application",
    _TermVariant_either>>: constant $ string "either",
    _TermVariant_function>>: constant $ string "function",
    _TermVariant_let>>: constant $ string "let",
    _TermVariant_list>>: constant $ string "list",
    _TermVariant_literal>>: constant $ string "literal",
    _TermVariant_map>>: constant $ string "map",
    _TermVariant_maybe>>: constant $ string "maybe",
    _TermVariant_pair>>: constant $ string "pair",
    _TermVariant_record>>: constant $ string "record",
    _TermVariant_set>>: constant $ string "set",
    _TermVariant_typeLambda>>: constant $ string "typeLambda",
    _TermVariant_typeApplication>>: constant $ string "typeApplication",
    _TermVariant_union>>: constant $ string "union",
    _TermVariant_unit>>: constant $ string "unit",
    _TermVariant_variable>>: constant $ string "variable",
    _TermVariant_wrap>>: constant $ string "wrap"]

typeVariantDef :: TBinding (TypeVariant -> String)
typeVariantDef = define "typeVariant" $
  doc "Show a type variant as a string" $
  match _TypeVariant Nothing [
    _TypeVariant_annotated>>: constant $ string "annotated",
    _TypeVariant_application>>: constant $ string "application",
    _TypeVariant_either>>: constant $ string "either",
    _TypeVariant_forall>>: constant $ string "forall",
    _TypeVariant_function>>: constant $ string "function",
    _TypeVariant_list>>: constant $ string "list",
    _TypeVariant_literal>>: constant $ string "literal",
    _TypeVariant_map>>: constant $ string "map",
    _TypeVariant_maybe>>: constant $ string "maybe",
    _TypeVariant_pair>>: constant $ string "pair",
    _TypeVariant_record>>: constant $ string "record",
    _TypeVariant_set>>: constant $ string "set",
    _TypeVariant_union>>: constant $ string "union",
    _TypeVariant_unit>>: constant $ string "unit",
    _TypeVariant_variable>>: constant $ string "variable",
    _TypeVariant_wrap>>: constant $ string "wrap"]
