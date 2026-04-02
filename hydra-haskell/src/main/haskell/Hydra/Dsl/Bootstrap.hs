-- | A bootstrapping DSL, used for Hydra's inner core models

module Hydra.Dsl.Bootstrap (
  module Hydra.Dsl.AsType,
  bootstrapGraph,
  datatype,
  qualify,
  typeref,
  defineType,
  toTypeDef,
  use,
  useType,
) where

import Hydra.Util
import Hydra.Constants
import Hydra.Core
import Hydra.Dsl.AsType
import Hydra.Graph
import Hydra.Lexical
import Hydra.Annotations
import Hydra.Module
import Hydra.Sources.Libraries
import qualified Hydra.Decode.Core as Decode
import qualified Hydra.Encode.Core as Encode

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


-- | An empty graph (no elements, no schema, but with primitive functions) which is used for bootstrapping Hydra Core
bootstrapGraph :: Graph
bootstrapGraph = Graph {
  graphBoundTerms = M.empty,
  graphBoundTypes = M.empty,
  graphClassConstraints = M.empty,
  graphLambdaVariables = S.empty,
  graphMetadata = M.empty,
  graphPrimitives = M.fromList $ fmap (\p -> (primitiveName p, p)) (L.concat (libraryPrimitives <$> standardLibraries)),
  graphSchemaTypes = M.empty,
  graphTypeVariables = S.empty}

datatype :: Namespace -> String -> Type -> Binding
datatype gname lname typ = Binding {
    bindingName = elName,
    bindingTerm = normalizeTermAnnotations (TermAnnotated (AnnotatedTerm {
      annotatedTermBody = Encode.type_ typ,
      annotatedTermAnnotation = M.fromList [
        (Name "type", TermVariable (Name "hydra.core.Type"))]})),
    bindingType = Just (TypeScheme {
      typeSchemeVariables = [],
      typeSchemeType = TypeVariable (Name "hydra.core.Type"),
      typeSchemeConstraints = Nothing})}
  where
    elName = qualify gname (Name lname)

qualify :: Namespace -> Name -> Name
qualify (Namespace gname) (Name lname) = Name $ gname ++ "." ++ lname

typeref :: Namespace -> String -> Type
typeref ns = TypeVariable . qualify ns . Name

-- | New DSL helpers (Option 1 from dsl-redesign-options.md)

-- | Define a type in a namespace
defineType :: Namespace -> String -> Type -> Binding
defineType = datatype

-- | Convert a type Binding (from defineType) to a type Definition.
-- The Binding must have been created by defineType/datatype, which stores
-- the type encoded as a term. We decode it back.
toTypeDef :: Binding -> Definition
toTypeDef b = case Decode.type_ bootstrapGraph (stripAnnotations $ bindingTerm b) of
  Right typ -> DefinitionType $ TypeDefinition (bindingName b) typ
  Left err -> error $ "toTypeDef: failed to decode type from binding "
    ++ show (bindingName b) ++ ": " ++ show err
  where
    stripAnnotations (TermAnnotated (AnnotatedTerm body _)) = stripAnnotations body
    stripAnnotations t = t

-- | Reference a type by its binding
use :: Binding -> Type
use b = TypeVariable (bindingName b)

-- | Reference a type in a namespace (old style, for migration)
useType :: Namespace -> String -> Type
useType = typeref
