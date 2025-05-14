{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Annotations where

-- Standard Tier-2 imports
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y
import           Hydra.Dsl.Base            as Base
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier1.All

import Hydra.Sources.Tier1.Compute
import Hydra.Sources.Tier2.Variants
import Hydra.Sources.Libraries


hydraAnnotationsModule :: Module
hydraAnnotationsModule = Module (Namespace "hydra.annotations") elements
    [hydraVariantsModule]
    [hydraComputeModule, hydraGraphModule, hydraMantleModule] $
    Just "Utilities for reading and writing type and term annotations"
  where
   elements = [
      el getTermTypeDef,
      el requireElementTypeDef,
      el requireTermTypeDef]

annotationsDefinition :: String -> TTerm a -> TElement a
annotationsDefinition = definitionInModule hydraAnnotationsModule

getTermTypeDef :: TElement (Term -> Maybe Type)
getTermTypeDef = annotationsDefinition "getTermType" $
  doc "Get the annotated type of a given term, if any" $
  match _Term (Just nothing) [
    "annotated">: ref getTermTypeDef <.> project _AnnotatedTerm _AnnotatedTerm_subject,
    "typed">: lambda "tt" $ just (project _TypedTerm _TypedTerm_type @@ var "tt")]

requireElementTypeDef :: TElement (Element -> Flow Graph Type)
requireElementTypeDef = annotationsDefinition "requireElementType" $
  doc "Get the annotated type of a given element, or fail if it is missing" $
  lambda "el" $ lets [
    "withType">: primitive _optionals_maybe
      @@ (Flows.fail ("missing type annotation for element " ++ (unwrap _Name @@ (project _Element _Element_name @@ var "el"))))
      @@ (lambda "t" $ Flows.pure $ var "t")] $
    var "withType" @@ (ref getTermTypeDef @@ (project _Element _Element_term @@ var "el"))

requireTermTypeDef :: TElement (Term -> Flow Graph Type)
requireTermTypeDef = annotationsDefinition "requireTermType" $
  doc "Get the annotated type of a given term, or fail if it is missing" $
  lets [
    "withType">: primitive _optionals_maybe
      @@ (Flows.fail "missing type annotation")
      @@ (lambda "t" $ Flows.pure $ var "t")] $
    var "withType" <.> ref getTermTypeDef
