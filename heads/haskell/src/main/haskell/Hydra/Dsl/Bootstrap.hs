-- | A bootstrapping DSL, used for Hydra's inner core models

module Hydra.Dsl.Bootstrap (
  module Hydra.Dsl.AsType,
  bootstrapGraph,
  datatype,
  descriptionMetadata,
  qualify,
  qualifiedDep,
  typeref,
  defineType,
  unqualifiedDep,
) where

import Hydra.Util
import Hydra.Constants
import Hydra.Core
import Hydra.Dsl.AsType
import Hydra.Graph
import Hydra.Lexical
import Hydra.Annotations
import Hydra.Packaging
import Hydra.Sources.Libraries

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
  graphPrimitives = M.fromList $ fmap (\p -> (primitiveDefinitionName (primitiveDefinition p), p)) (L.concat (libraryPrimitives <$> standardLibraries)),
  graphSchemaTypes = M.empty,
  graphTypeVariables = S.empty}

-- | Define a type in a namespace, producing a TypeDefinition directly.
datatype :: ModuleName -> String -> Type -> TypeDefinition
datatype gname lname typ = TypeDefinition {
    typeDefinitionName = qualify gname (Name lname),
    typeDefinitionMetadata = Nothing,
    typeDefinitionBody = TypeScheme {
      typeSchemeVariables = [],
      typeSchemeBody = typ,
      typeSchemeConstraints = Nothing}}

-- | Define a type in a namespace
defineType :: ModuleName -> String -> Type -> TypeDefinition
defineType = datatype

-- | A module dependency qualified by the providing package, for disambiguation.
-- Not yet used, but retained for forthcoming package-qualified dependency resolution.
qualifiedDep :: PackageName -> ModuleName -> ModuleDependency
qualifiedDep pkg ns = ModuleDependency ns (Just pkg)

qualify :: ModuleName -> Name -> Name
qualify (ModuleName gname) (Name lname) = Name $ gname ++ "." ++ lname

typeref :: ModuleName -> String -> Type
typeref ns = TypeVariable . qualify ns . Name

-- | An unqualified module dependency (package omitted; resolver searches all packages in scope)
unqualifiedDep :: ModuleName -> ModuleDependency
unqualifiedDep ns = ModuleDependency ns Nothing

-- | Wrap an optional description string as entity metadata carrying only that description.
-- Used to migrate the former Module/Package `description` field into the `metadata` field
-- without data loss; a Nothing description yields no metadata.
descriptionMetadata :: Maybe String -> Maybe EntityMetadata
descriptionMetadata Nothing = Nothing
descriptionMetadata desc = Just (EntityMetadata desc [] [] Nothing)
