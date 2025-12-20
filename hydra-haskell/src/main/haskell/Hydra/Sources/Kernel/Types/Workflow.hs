module Hydra.Sources.Kernel.Types.Workflow where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Compute as Compute
import qualified Hydra.Sources.Kernel.Types.Graph as Graph
import qualified Hydra.Sources.Kernel.Types.Module as Module


ns :: Namespace
ns = Namespace "hydra.workflow"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Compute.ns, Graph.ns, Module.ns] [Core.ns] $
    Just "A model for Hydra transformation workflows"
  where
    elements = [
      hydraSchemaSpec,
      lastMile,
      schemaSpec,
      transformWorkflow]

hydraSchemaSpec :: Binding
hydraSchemaSpec = define "HydraSchemaSpec" $
  doc "The specification of a Hydra schema, provided as a set of modules and a distinguished type" $
  T.record [
    "modules">:
      doc "The modules to include in the schema graph" $
      T.list Module.module',
    "typeName">:
      doc "The name of the top-level type; all data which passes through the workflow will be instances of this type"
      Core.name]

lastMile :: Binding
lastMile = define "LastMile" $
  doc "The last mile of a transformation, which encodes and serializes terms to a file" $
  T.forAlls ["s", "a"] $ T.record [
    "encoder">:
      doc "An encoder for terms to a list of output objects" $
      Core.type_ ~> Compute.flow @@ "s"
        @@ (Core.term ~> Graph.graph ~> Compute.flow @@ "s" @@ T.list "a"),
    "serializer">:
      doc "A function which serializes a list of output objects to a string representation" $
      T.list "a" ~> Compute.flow @@ "s" @@ T.string,
    "fileExtension">:
      doc "A file extension for the generated file(s)" $
      T.string]

schemaSpec :: Binding
schemaSpec = define "SchemaSpec" $
  doc "The specification of a schema at the source end of a workflow" $
  T.union [
    "hydra">:
      doc "A native Hydra schema"
      hydraSchemaSpec,
    "file">:
      doc "A schema provided as a file, available at the given file path" $
      T.string,
    "provided">:
      doc "A schema which will be provided within the workflow" $
      T.unit]

transformWorkflow :: Binding
transformWorkflow = define "TransformWorkflow" $
  doc "The specification of a workflow which takes a schema specification, reads data from a directory, and writes data to another directory" $
  T.record [
    "name">:
      doc "A descriptive name for the workflow" $
      T.string,
    "schemaSpec">:
      doc "The schema specification"
      schemaSpec,
    "srcDir">:
      doc "The source directory" $
      T.string,
    "destDir">:
      doc "The destination directory" $
      T.string]
