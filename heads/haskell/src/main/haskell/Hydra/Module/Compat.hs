-- | Compatibility functions for converting between Definition and Binding.
-- These bridge the gap during the transition from Module.elements :: [Binding]
-- to Module.definitions :: [Definition].

module Hydra.Module.Compat (
  bindingToDefinition,
  definitionToBinding,
  moduleBindings,
) where

import Hydra.Core
import Hydra.Packaging
import qualified Hydra.Encode.Core as EncodeCore

import qualified Data.List as L
import qualified Data.Map as M


-- | Convert a Binding to a Definition (always produces DefinitionTerm).
bindingToDefinition :: Binding -> Definition
bindingToDefinition b = DefinitionTerm $ TermDefinition {
  termDefinitionName = bindingName b,
  termDefinitionTerm = bindingTerm b,
  termDefinitionTypeScheme = bindingTypeScheme b}

-- | Convert a Definition back to a Binding.
definitionToBinding :: Definition -> Binding
definitionToBinding d = case d of
  DefinitionTerm td -> Binding {
    bindingName = termDefinitionName td,
    bindingTerm = termDefinitionTerm td,
    bindingTypeScheme = termDefinitionTypeScheme td}
  DefinitionType td ->
    let encoded = EncodeCore.type_ (typeSchemeBody $ typeDefinitionTypeScheme td)
        annotated = TermAnnotated (AnnotatedTerm {
          annotatedTermBody = encoded,
          annotatedTermAnnotation = M.fromList [
            (Name "type", TermVariable (Name "hydra.core.Type"))]})
    in Binding {
      bindingName = typeDefinitionName td,
      bindingTerm = annotated,
      bindingTypeScheme = Just (TypeScheme [] (TypeVariable (Name "hydra.core.Type")) Nothing)}

-- | Extract all definitions from a module as Bindings.
moduleBindings :: Module -> [Binding]
moduleBindings m = L.map definitionToBinding (moduleDefinitions m)
