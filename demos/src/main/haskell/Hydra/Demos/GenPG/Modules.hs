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
  moduleDefinitions = map bindingToDefinition [
    -- salesDatabaseSchema: list[TableType]
    Binding {
      bindingName = Name "hydra.demos.genpg.sales.salesDatabaseSchema",
      bindingTerm = TermList $ fmap EncodeTabular.tableType Sales.salesTableSchemas,
      bindingTypeScheme = Just listTableTypeScheme
    },
    -- salesGraphSchema: GraphSchema[Type]
    Binding {
      bindingName = Name "hydra.demos.genpg.sales.salesGraphSchema",
      bindingTerm = EncodePg.graphSchema EncodeCore.type_ Sales.salesGraphSchema,
      bindingTypeScheme = Just graphSchemaTypeScheme
    },
    -- salesMapping: LazyGraph[Term]
    Binding {
      bindingName = Name "hydra.demos.genpg.sales.salesMapping",
      bindingTerm = EncodePg.lazyGraph EncodeCore.term Sales.salesGraph,
      bindingTypeScheme = Just lazyGraphTermScheme
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
  moduleDefinitions = map bindingToDefinition [
    -- healthDatabaseSchema: list[TableType]
    Binding {
      bindingName = Name "hydra.demos.genpg.health.healthDatabaseSchema",
      bindingTerm = TermList $ fmap EncodeTabular.tableType Health.healthTableSchemas,
      bindingTypeScheme = Just listTableTypeScheme
    },
    -- healthGraphSchema: GraphSchema[Type]
    Binding {
      bindingName = Name "hydra.demos.genpg.health.healthGraphSchema",
      bindingTerm = EncodePg.graphSchema EncodeCore.type_ Health.healthGraphSchema,
      bindingTypeScheme = Just graphSchemaTypeScheme
    },
    -- healthMapping: LazyGraph[Term]
    Binding {
      bindingName = Name "hydra.demos.genpg.health.healthMapping",
      bindingTerm = EncodePg.lazyGraph EncodeCore.term Health.healthGraph,
      bindingTypeScheme = Just lazyGraphTermScheme
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
