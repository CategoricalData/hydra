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
module_ = Module ns elements
    [Variants.module_]
    kernelTypesModules $
    Just "Language constraints for Hydra Core"
  where
    ns = Namespace "hydra.languages"
    elements = [el hydraLanguageDef]

hydraLanguageDef :: TElement Language
hydraLanguageDef = definitionInModule module_ "hydraLanguage" $
  doc "Language constraints for Hydra Core, i.e. no constraints." $
  record _Language [
    _Language_name>>: wrap _LanguageName "hydra.core",
    _Language_constraints>>: record _LanguageConstraints [
    _LanguageConstraints_eliminationVariants>>: Sets.fromList $ ref Variants.eliminationVariantsDef,
    _LanguageConstraints_literalVariants>>: Sets.fromList $ ref Variants.literalVariantsDef,
    _LanguageConstraints_floatTypes>>: Sets.fromList $ ref Variants.floatTypesDef,
    _LanguageConstraints_functionVariants>>: Sets.fromList $ ref Variants.functionVariantsDef,
    _LanguageConstraints_integerTypes>>: Sets.fromList $ ref Variants.integerTypesDef,
    _LanguageConstraints_termVariants>>: Sets.fromList $ ref Variants.termVariantsDef,
    _LanguageConstraints_typeVariants>>: Sets.fromList $ ref Variants.typeVariantsDef,
    _LanguageConstraints_types>>: constant true]]
