-- | Shared module definitions for the GenPG demo code generation.
--
-- This module defines the sales and health modules (as Hydra Modules with
-- Term-encoded schemas and mappings) that are shared by both the Python
-- and Java code generators.

module Hydra.Demos.Genpg.Modules where

import Hydra.Kernel
import Hydra.Demos.Genpg.Examples.Sales.DatabaseSchema as Sales
import Hydra.Demos.Genpg.Examples.Sales.GraphSchema as Sales
import Hydra.Demos.Genpg.Examples.Sales.Mapping as Sales
import Hydra.Demos.Genpg.Examples.Health.DatabaseSchema as Health
import Hydra.Demos.Genpg.Examples.Health.GraphSchema as Health
import Hydra.Demos.Genpg.Examples.Health.Mapping as Health
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Encode.Pg.Model as EncodePg
import qualified Hydra.Encode.Tabular as EncodeTabular


-- | The sales module containing sales demo schemas and mapping.
salesModule :: Module
salesModule = Module {
  moduleName = salesNamespace,
  moduleDefinitions = [
    -- salesDatabaseSchema: list[TableType]
    DefinitionTerm $ TermDefinition {
      termDefinitionName = Name "hydra.demos.genpg.sales.salesDatabaseSchema",
      termDefinitionTerm = TermList $ fmap EncodeTabular.tableType Sales.salesTableSchemas,
      termDefinitionSignature = Just (typeSchemeToTermSignature listTableTypeScheme)
    },
    -- salesGraphSchema: GraphSchema[Type]
    DefinitionTerm $ TermDefinition {
      termDefinitionName = Name "hydra.demos.genpg.sales.salesGraphSchema",
      termDefinitionTerm = EncodePg.graphSchema EncodeCore.type_ Sales.salesGraphSchema,
      termDefinitionSignature = Just (typeSchemeToTermSignature graphSchemaTypeScheme)
    },
    -- salesMapping: LazyGraph[Term]
    DefinitionTerm $ TermDefinition {
      termDefinitionName = Name "hydra.demos.genpg.sales.salesMapping",
      termDefinitionTerm = EncodePg.lazyGraph EncodeCore.term Sales.salesGraph,
      termDefinitionSignature = Just (typeSchemeToTermSignature lazyGraphTermScheme)
    }
  ],
  moduleDependencies = fmap (\ns -> ModuleDependency ns Nothing) [ModuleName "hydra.tabular",
    ModuleName "hydra.relational",
    ModuleName "hydra.pg.model",
    ModuleName "hydra.core", ModuleName "hydra.tabular",
    ModuleName "hydra.relational",
    ModuleName "hydra.pg.model",
    ModuleName "hydra.core"],
  moduleMetadata = (Just (EntityMetadata (Just "GenPG schemas for the sales demo") [] [] Nothing))
}


-- | ModuleName for the generated sales module
salesNamespace :: ModuleName
salesNamespace = ModuleName "hydra.demos.genpg.sales"


-- | The health module containing health demo schemas and mapping.
healthModule :: Module
healthModule = Module {
  moduleName = healthNamespace,
  moduleDefinitions = [
    -- healthDatabaseSchema: list[TableType]
    DefinitionTerm $ TermDefinition {
      termDefinitionName = Name "hydra.demos.genpg.health.healthDatabaseSchema",
      termDefinitionTerm = TermList $ fmap EncodeTabular.tableType Health.healthTableSchemas,
      termDefinitionSignature = Just (typeSchemeToTermSignature listTableTypeScheme)
    },
    -- healthGraphSchema: GraphSchema[Type]
    DefinitionTerm $ TermDefinition {
      termDefinitionName = Name "hydra.demos.genpg.health.healthGraphSchema",
      termDefinitionTerm = EncodePg.graphSchema EncodeCore.type_ Health.healthGraphSchema,
      termDefinitionSignature = Just (typeSchemeToTermSignature graphSchemaTypeScheme)
    },
    -- healthMapping: LazyGraph[Term]
    DefinitionTerm $ TermDefinition {
      termDefinitionName = Name "hydra.demos.genpg.health.healthMapping",
      termDefinitionTerm = EncodePg.lazyGraph EncodeCore.term Health.healthGraph,
      termDefinitionSignature = Just (typeSchemeToTermSignature lazyGraphTermScheme)
    }
  ],
  moduleDependencies = fmap (\ns -> ModuleDependency ns Nothing) [ModuleName "hydra.tabular",
    ModuleName "hydra.relational",
    ModuleName "hydra.pg.model",
    ModuleName "hydra.core", ModuleName "hydra.tabular",
    ModuleName "hydra.relational",
    ModuleName "hydra.pg.model",
    ModuleName "hydra.core"],
  moduleMetadata = (Just (EntityMetadata (Just "GenPG schemas for the health demo") [] [] Nothing))
}


-- | ModuleName for the generated health module
healthNamespace :: ModuleName
healthNamespace = ModuleName "hydra.demos.genpg.health"


-- | Type scheme for list of TableType (no type variables)
listTableTypeScheme :: TypeScheme
listTableTypeScheme = TypeScheme {
  typeSchemeVariables = [],
  typeSchemeBody = TypeList (TypeVariable (Name "hydra.tabular.TableType")),
  typeSchemeConstraints = Nothing
}


-- | Type scheme for GraphSchema Type (no type variables)
graphSchemaTypeScheme :: TypeScheme
graphSchemaTypeScheme = TypeScheme {
  typeSchemeVariables = [],
  typeSchemeBody = TypeApplication (ApplicationType
    (TypeVariable (Name "hydra.pg.model.GraphSchema"))
    (TypeVariable (Name "hydra.core.Type"))),
  typeSchemeConstraints = Nothing
}


-- | Type scheme for LazyGraph Term (no type variables)
lazyGraphTermScheme :: TypeScheme
lazyGraphTermScheme = TypeScheme {
  typeSchemeVariables = [],
  typeSchemeBody = TypeApplication (ApplicationType
    (TypeVariable (Name "hydra.pg.model.LazyGraph"))
    (TypeVariable (Name "hydra.core.Term"))),
  typeSchemeConstraints = Nothing
}
