{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Languages where

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

import qualified Hydra.Sources.Kernel.Terms.Variants as Variants


module_ :: Module
module_ = Module (Namespace "hydra.languages")
  [el hydraLanguageDef]
  [Variants.module_]
  kernelTypesModules $
  Just "Language constraints for Hydra Core"

hydraLanguageDef :: TBinding Language
hydraLanguageDef = definitionInModule module_ "hydraLanguage" $
  doc "Language constraints for Hydra Core, i.e. no constraints." $ lets [
  "eliminationVariants">: Sets.fromList $ ref Variants.eliminationVariantsDef,
  "literalVariants">: Sets.fromList $ ref Variants.literalVariantsDef,
  "floatTypes">: Sets.fromList $ ref Variants.floatTypesDef,
  "functionVariants">: Sets.fromList $ ref Variants.functionVariantsDef,
  "integerTypes">: Sets.fromList $ ref Variants.integerTypesDef,
  "termVariants">: Sets.fromList $ ref Variants.termVariantsDef,
  "typeVariants">: Sets.fromList $ ref Variants.typeVariantsDef,
  "types">: constant true] $
  Coders.language
    (Coders.languageName "hydra.core")
    (Coders.languageConstraints
      (var "eliminationVariants")
      (var "literalVariants")
      (var "floatTypes")
      (var "functionVariants")
      (var "integerTypes")
      (var "termVariants")
      (var "typeVariants")
      (var "types"))
