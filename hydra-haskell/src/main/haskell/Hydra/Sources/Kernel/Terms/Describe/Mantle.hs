{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Describe.Mantle where

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


describeMantleModule :: Module
describeMantleModule = Module (Namespace "hydra.describe.mantle") elements
    []
    [KernelTypes.hydraCoreModule, KernelTypes.hydraMantleModule] $
    Just "Natural-language descriptions for hydra.mantle types"
  where
   elements = [
     el precisionDef]

describeMantleDefinition :: String -> TTerm a -> TElement a
describeMantleDefinition = definitionInModule describeMantleModule


precisionDef :: TElement (Precision -> String)
precisionDef = describeMantleDefinition "precision" $
  doc "Display numeric precision as a string" $
  match _Precision Nothing [
    _Precision_arbitrary>>: constant $ string "arbitrary-precision",
    _Precision_bits>>: lambda "bits" $ Literals.showInt32 (var "bits") ++ string "-bit"]
