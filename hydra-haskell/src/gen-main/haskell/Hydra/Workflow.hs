-- | A model for Hydra transformation workflows

module Hydra.Workflow where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Module as Module
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | The specification of a Hydra schema, provided as a set of modules and a distinguished type
data HydraSchemaSpec = 
  HydraSchemaSpec {
    -- | The modules to include in the schema graph
    hydraSchemaSpecModules :: [Module.Module],
    -- | The name of the top-level type; all data which passes through the workflow will be instances of this type
    hydraSchemaSpecTypeName :: Core.Name}
  deriving (Eq, Ord, Read, Show)

_HydraSchemaSpec = (Core.Name "hydra/workflow.HydraSchemaSpec")

_HydraSchemaSpec_modules = (Core.Name "modules")

_HydraSchemaSpec_typeName = (Core.Name "typeName")

_HydraSchemaSpec_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/workflow.HydraSchemaSpec"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modules"),
      Core.fieldTypeType = (Core.TypeList Module._Module_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeName"),
      Core.fieldTypeType = Core._Name_type_}]}))

-- | The last mile of a transformation, which encodes and serializes terms to a file
data LastMile s a = 
  LastMile {
    -- | An encoder for terms to a list of output objects
    lastMileEncoder :: (Core.Type -> Compute.Flow s (Core.Term -> Graph.Graph -> Compute.Flow s [a])),
    -- | A function which serializes a list of output objects to a string representation
    lastMileSerializer :: ([a] -> Compute.Flow s String),
    -- | A file extension for the generated file(s)
    lastMileFileExtension :: String}

_LastMile = (Core.Name "hydra/workflow.LastMile")

_LastMile_encoder = (Core.Name "encoder")

_LastMile_serializer = (Core.Name "serializer")

_LastMile_fileExtension = (Core.Name "fileExtension")

_LastMile_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "s"),
  Core.lambdaTypeBody = (Core.TypeLambda (Core.LambdaType {
    Core.lambdaTypeParameter = (Core.Name "a"),
    Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
      Core.rowTypeTypeName = (Core.Name "hydra/workflow.LastMile"),
      Core.rowTypeExtends = Nothing,
      Core.rowTypeFields = [
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "encoder"),
          Core.fieldTypeType = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = Core._Type_type_,
            Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = Compute._Flow_type_,
                Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s"))})),
              Core.applicationTypeArgument = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = Core._Term_type_,
                Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = Graph._Graph_type_,
                  Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = Compute._Flow_type_,
                      Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s"))})),
                    Core.applicationTypeArgument = (Core.TypeList (Core.TypeVariable (Core.Name "a")))}))}))}))}))}))},
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "serializer"),
          Core.fieldTypeType = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "a"))),
            Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = Compute._Flow_type_,
                Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s"))})),
              Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)}))}))},
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "fileExtension"),
          Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)}]}))}))}))

-- | The specification of a schema at the source end of a workflow
data SchemaSpec = 
  -- | A native Hydra schema
  SchemaSpecHydra HydraSchemaSpec |
  -- | A schema provided as a file, available at the given file path
  SchemaSpecFile String |
  -- | A schema which will be provided within the workflow
  SchemaSpecProvided 
  deriving (Eq, Ord, Read, Show)

_SchemaSpec = (Core.Name "hydra/workflow.SchemaSpec")

_SchemaSpec_hydra = (Core.Name "hydra")

_SchemaSpec_file = (Core.Name "file")

_SchemaSpec_provided = (Core.Name "provided")

_SchemaSpec_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/workflow.SchemaSpec"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "hydra"),
      Core.fieldTypeType = _HydraSchemaSpec_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "file"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "provided"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

-- | The specification of a workflow which takes a schema specification, reads data from a directory, and writes data to another directory
data TransformWorkflow = 
  TransformWorkflow {
    -- | A descriptive name for the workflow
    transformWorkflowName :: String,
    -- | The schema specification
    transformWorkflowSchemaSpec :: SchemaSpec,
    -- | The source directory
    transformWorkflowSrcDir :: String,
    -- | The destination directory
    transformWorkflowDestDir :: String}
  deriving (Eq, Ord, Read, Show)

_TransformWorkflow = (Core.Name "hydra/workflow.TransformWorkflow")

_TransformWorkflow_name = (Core.Name "name")

_TransformWorkflow_schemaSpec = (Core.Name "schemaSpec")

_TransformWorkflow_srcDir = (Core.Name "srcDir")

_TransformWorkflow_destDir = (Core.Name "destDir")

_TransformWorkflow_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/workflow.TransformWorkflow"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "schemaSpec"),
      Core.fieldTypeType = _SchemaSpec_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "srcDir"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "destDir"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)}]}))