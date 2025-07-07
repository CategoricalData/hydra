{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Extract.Mantle where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors            as Accessors
import qualified Hydra.Dsl.Ast                  as Ast
import qualified Hydra.Dsl.Coders               as Coders
import qualified Hydra.Dsl.Compute              as Compute
import qualified Hydra.Dsl.Core                 as Core
import qualified Hydra.Dsl.Graph                as Graph
import qualified Hydra.Dsl.Lib.Chars            as Chars
import qualified Hydra.Dsl.Lib.Equality         as Equality
import qualified Hydra.Dsl.Lib.Flows            as Flows
import qualified Hydra.Dsl.Lib.Lists            as Lists
import qualified Hydra.Dsl.Lib.Literals         as Literals
import qualified Hydra.Dsl.Lib.Logic            as Logic
import qualified Hydra.Dsl.Lib.Maps             as Maps
import qualified Hydra.Dsl.Lib.Math             as Math
import qualified Hydra.Dsl.Lib.Optionals        as Optionals
import           Hydra.Dsl.Phantoms             as Phantoms
import qualified Hydra.Dsl.Lib.Sets             as Sets
import           Hydra.Dsl.Lib.Strings          as Strings
import qualified Hydra.Dsl.Mantle               as Mantle
import qualified Hydra.Dsl.Module               as Module
import qualified Hydra.Dsl.TTerms               as TTerms
import qualified Hydra.Dsl.TTypes               as TTypes
import qualified Hydra.Dsl.Terms                as Terms
import qualified Hydra.Dsl.Topology             as Topology
import qualified Hydra.Dsl.Types                as Types
import qualified Hydra.Dsl.Typing               as Typing
import qualified Hydra.Sources.Tier1.All        as Tier1
import qualified Hydra.Sources.Tier1.Constants  as Constants
import qualified Hydra.Sources.Tier1.Formatting as Formatting
import qualified Hydra.Sources.Tier1.Literals   as Literals
import qualified Hydra.Sources.Tier1.Strip      as Strip
import           Prelude hiding ((++))
import qualified Data.Int                  as I
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

-- Uncomment tier-2 sources as needed
--import qualified Hydra.Sources.Tier2.Adapt.Literals as AdaptLiterals
--import qualified Hydra.Sources.Tier2.Adapt.Modules as AdaptModules
--import qualified Hydra.Sources.Tier2.Adapt.Terms as AdaptTerms
--import qualified Hydra.Sources.Tier2.Adapt.Utils as AdaptUtils
--import qualified Hydra.Sources.Tier2.Annotations as Annotations
--import qualified Hydra.Sources.Tier2.Arity as Arity
--import qualified Hydra.Sources.Tier2.Decode.Core as DecodeCore
--import qualified Hydra.Sources.Tier2.Decoding as Decoding
--import qualified Hydra.Sources.Tier2.Describe.Core as DescribeCore
--import qualified Hydra.Sources.Tier2.Describe.Mantle as DescribeMantle
--import qualified Hydra.Sources.Tier2.Encode.Core as EncodeCore
--import qualified Hydra.Sources.Tier2.Errors as Errors
import qualified Hydra.Sources.Tier2.Extract.Core as ExtractCore
--import qualified Hydra.Sources.Tier2.Extract.Mantle as ExtractMantle
--import qualified Hydra.Sources.Tier2.Extract.Mantle as ExtractMantle
--import qualified Hydra.Sources.Tier2.Grammars as Grammars
--import qualified Hydra.Sources.Tier2.Inference as Inference
--import qualified Hydra.Sources.Tier2.Languages as Languages
--import qualified Hydra.Sources.Tier2.Lexical as Lexical
import qualified Hydra.Sources.Tier2.Monads as Monads
--import qualified Hydra.Sources.Tier2.Names as Names
--import qualified Hydra.Sources.Tier2.Reduction as Reduction
--import qualified Hydra.Sources.Tier2.Rewriting as Rewriting
--import qualified Hydra.Sources.Tier2.Schemas as Schemas
--import qualified Hydra.Sources.Tier2.Serialization as Serialization
--import qualified Hydra.Sources.Tier2.Show.Accessors as ShowAccessors
--import qualified Hydra.Sources.Tier2.Show.Core as ShowCore
--import qualified Hydra.Sources.Tier2.Show.Graph as ShowGraph
--import qualified Hydra.Sources.Tier2.Show.Mantle as ShowMantle
--import qualified Hydra.Sources.Tier2.Show.Typing as ShowTyping
--import qualified Hydra.Sources.Tier2.Sorting as Sorting
--import qualified Hydra.Sources.Tier2.Substitution as Substitution
--import qualified Hydra.Sources.Tier2.Tarjan as Tarjan
--import qualified Hydra.Sources.Tier2.Templates as Templates
--import qualified Hydra.Sources.Tier2.Unification as Unification
--import qualified Hydra.Sources.Tier2.Variants as Variants


extractMantleDefinition :: String -> TTerm a -> TElement a
extractMantleDefinition = definitionInModule extractMantleModule

extractMantleModule :: Module
extractMantleModule = Module (Namespace "hydra.extract.mantle") elements
    [ExtractCore.extractCoreModule, Monads.hydraMonadsModule]
    [Tier1.hydraCodersModule, Tier1.hydraMantleModule] $
    Just ("A DSL for decoding and validating Hydra terms at runtime. This module provides functions to extract typed values from Hydra terms with appropriate error handling.")
  where
   elements = [
     el comparisonDef]

comparisonDef :: TElement (Term -> Flow Graph Comparison)
comparisonDef = extractMantleDefinition "comparison" $
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
