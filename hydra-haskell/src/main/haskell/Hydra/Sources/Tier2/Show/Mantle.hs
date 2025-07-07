{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Show.Mantle where

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
--import qualified Hydra.Sources.Tier2.Extract.Core as ExtractCore
--import qualified Hydra.Sources.Tier2.Extract.Mantle as ExtractMantle
--import qualified Hydra.Sources.Tier2.Grammars as Grammars
--import qualified Hydra.Sources.Tier2.Inference as Inference
--import qualified Hydra.Sources.Tier2.Languages as Languages
--import qualified Hydra.Sources.Tier2.Lexical as Lexical
--import qualified Hydra.Sources.Tier2.Monads as Monads
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


showMantleModule :: Module
showMantleModule = Module (Namespace "hydra.show.mantle") elements
    [Tier1.hydraStripModule]
    [Tier1.hydraComputeModule, Tier1.hydraGraphModule, Tier1.hydraMantleModule, Tier1.hydraTypingModule] $
    Just "String representations of hydra.mantle types"
  where
   elements = [
     el termVariantDef,
     el typeVariantDef]

showMantleDefinition :: String -> TTerm a -> TElement a
showMantleDefinition = definitionInModule showMantleModule

termVariantDef :: TElement (TermVariant -> String)
termVariantDef = showMantleDefinition "termVariant" $
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
typeVariantDef = showMantleDefinition "typeVariant" $
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
