{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Extract.Mantle where

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
import qualified Hydra.Dsl.Json          as Json
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

import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads


module_ :: Module
module_ = Module (Namespace "hydra.extract.mantle") elements
    [ExtractCore.module_, Monads.module_]
    kernelTypesModules $
    Just ("A DSL for decoding and validating Hydra terms at runtime. This module provides functions to extract typed values from Hydra terms with appropriate error handling.")
  where
   elements = [
     el comparisonDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

comparisonDef :: TBinding (Term -> Flow Graph Comparison)
comparisonDef = define "comparison" $
  doc "Extract a comparison from a term" $
  lambda "term" $
    Flows.bind (ref ExtractCore.unitVariantDef @@ Core.nameLift _Comparison @@ var "term") $
      lambda "fname" $
        Logic.ifElse (Equality.equal (Core.unName $ var "fname") (string $ unName _Comparison_equalTo))
          (Flows.pure Graph.comparisonEqualTo)
          (Logic.ifElse (Equality.equal (Core.unName $ var "fname") (string $ unName _Comparison_lessThan))
            (Flows.pure Graph.comparisonLessThan)
            (Logic.ifElse (Equality.equal (Core.unName $ var "fname") (string $ unName _Comparison_greaterThan))
              (Flows.pure Graph.comparisonGreaterThan)
              (ref Monads.unexpectedDef @@ string "comparison" @@ Core.unName (var "fname"))))
