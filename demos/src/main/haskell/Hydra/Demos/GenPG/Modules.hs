-- | Shared module definitions for the GenPG demo code generation.
--
-- This module defines the sales and health modules (as Hydra Modules with
-- Term-encoded schemas and mappings) that are shared by both the Python
-- and Java code generators.

module Hydra.Demos.GenPG.Modules where

import Hydra.Kernel
import Hydra.Demos.GenPG.Examples.Sales.DatabaseSchema as Sales
import Hydra.Demos.GenPG.Examples.Sales.GraphSchema as Sales
import Hydra.Demos.GenPG.Examples.Sales.Mapping as Sales
import Hydra.Demos.GenPG.Examples.Health.DatabaseSchema as Health
import Hydra.Demos.GenPG.Examples.Health.GraphSchema as Health
import Hydra.Demos.GenPG.Examples.Health.Mapping as Health
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Encode.Pg.Model as EncodePg
import qualified Hydra.Encode.Tabular as EncodeTabular


-- | The sales module containing sales demo schemas and mapping.
salesModule :: Module
salesModule = Module {
  moduleNamespace = salesNamespace,
  moduleDefinitions = [
    -- salesDatabaseSchema: list[TableType]
    DefinitionTerm $ TermDefinition {
      termDefinitionName = Name "hydra.demos.genpg.sales.salesDatabaseSchema",
      termDefinitionTerm = TermList $ fmap EncodeTabular.tableType Sales.salesTableSchemas,
      termDefinitionTypeScheme = Just listTableTypeScheme
    },
    -- salesGraphSchema: GraphSchema[Type]
    DefinitionTerm $ TermDefinition {
      termDefinitionName = Name "hydra.demos.genpg.sales.salesGraphSchema",
      termDefinitionTerm = EncodePg.graphSchema EncodeCore.type_ Sales.salesGraphSchema,
      termDefinitionTypeScheme = Just graphSchemaTypeScheme
    },
    -- salesMapping: LazyGraph[Term]
    DefinitionTerm $ TermDefinition {
      termDefinitionName = Name "hydra.demos.genpg.sales.salesMapping",
      termDefinitionTerm = EncodePg.lazyGraph EncodeCore.term Sales.salesGraph,
      termDefinitionTypeScheme = Just lazyGraphTermScheme
    }
  ],
  moduleTermDependencies = [
    Namespace "hydra.tabular",
    Namespace "hydra.relational",
    Namespace "hydra.pg.model",
    Namespace "hydra.core"
  ],
  moduleTypeDependencies = [
    Namespace "hydra.tabular",
    Namespace "hydra.relational",
    Namespace "hydra.pg.model",
    Namespace "hydra.core"
  ],
  moduleDescription = Just "GenPG schemas for the sales demo"
}


-- | Namespace for the generated sales module
salesNamespace :: Namespace
salesNamespace = Namespace "hydra.demos.genpg.sales"


-- | The health module containing health demo schemas and mapping.
healthModule :: Module
healthModule = Module {
  moduleNamespace = healthNamespace,
  moduleDefinitions = [
    -- healthDatabaseSchema: list[TableType]
    DefinitionTerm $ TermDefinition {
      termDefinitionName = Name "hydra.demos.genpg.health.healthDatabaseSchema",
      termDefinitionTerm = TermList $ fmap EncodeTabular.tableType Health.healthTableSchemas,
      termDefinitionTypeScheme = Just listTableTypeScheme
    },
    -- healthGraphSchema: GraphSchema[Type]
    DefinitionTerm $ TermDefinition {
      termDefinitionName = Name "hydra.demos.genpg.health.healthGraphSchema",
      termDefinitionTerm = EncodePg.graphSchema EncodeCore.type_ Health.healthGraphSchema,
      termDefinitionTypeScheme = Just graphSchemaTypeScheme
    },
    -- healthMapping: LazyGraph[Term]
    DefinitionTerm $ TermDefinition {
      termDefinitionName = Name "hydra.demos.genpg.health.healthMapping",
      termDefinitionTerm = EncodePg.lazyGraph EncodeCore.term Health.healthGraph,
      termDefinitionTypeScheme = Just lazyGraphTermScheme
    }
  ],
  moduleTermDependencies = [
    Namespace "hydra.tabular",
    Namespace "hydra.relational",
    Namespace "hydra.pg.model",
    Namespace "hydra.core"
  ],
  moduleTypeDependencies = [
    Namespace "hydra.tabular",
    Namespace "hydra.relational",
    Namespace "hydra.pg.model",
    Namespace "hydra.core"
  ],
  moduleDescription = Just "GenPG schemas for the health demo"
}


-- | Namespace for the generated health module
healthNamespace :: Namespace
healthNamespace = Namespace "hydra.demos.genpg.health"


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
