module Hydra.Sources.Constants where

import Hydra.Kernel
import Hydra.Sources.Core
import Hydra.Dsl.Base as Base


constantsDefinition :: String -> Datum a -> Definition a
constantsDefinition = definitionInModule hydraConstantsModule

hydraConstantsModule :: Module Kv
hydraConstantsModule = Module (Namespace "hydra/constants") elements [hydraCoreModule] $
    Just ("A module for tier-0 constants.")
  where
   elements = [
     el ignoredVariableDef,
     el placeholderNameDef,
     el maxTraceDepthDef]

ignoredVariableDef :: Definition String
ignoredVariableDef = constantsDefinition "ignoredVariable" $
  string "_"

placeholderNameDef :: Definition Name
placeholderNameDef = constantsDefinition "placeholderName" $
  doc "A placeholder name for row types as they are being constructed" $
  wrap _Name $ string "Placeholder"

maxTraceDepthDef :: Definition Int
maxTraceDepthDef = constantsDefinition "maxTraceDepth" $ int32 50
