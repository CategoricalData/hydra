{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Workflow where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Sources.Module
import Hydra.Sources.Compute
import Hydra.Sources.Core
import Hydra.Sources.Graph
import Hydra.Dsl.Types as Types


hydraWorkflowModule :: Module Kv
hydraWorkflowModule = Module ns elements [hydraModuleModule, hydraComputeModule, hydraGraphModule] $
    Just "A model for Hydra transformation workflows"
  where
    ns = Namespace "hydra/workflow"
    mod = nsref $ moduleNamespace hydraModuleModule
    compute = nsref $ moduleNamespace hydraComputeModule
    core = nsref $ moduleNamespace hydraCoreModule
    graph = nsref $ moduleNamespace hydraGraphModule
    wf = nsref ns
    def = datatype ns

    elements = [
      def "HydraSchemaSpec" $
        doc "The specification of a Hydra schema, provided as a set of modules and a distinguished type" $
        record [
          "modules">:
            doc "The modules to include in the schema graph" $
            list $ mod "Module" @@ compute "Kv",
          "typeName">:
            doc "The name of the top-level type; all data which passes through the workflow will be instances of this type" $
            core "Name"],

      def "LastMile" $
        doc "The last mile of a transformation, which encodes and serializes terms to a file" $
        lambda "s" $ lambda "a" $ record [
          "encoder">:
            doc "An encoder for terms to a list of output objects" $
            core "Type" @@ compute "Kv" --> compute "Flow" @@ "s"
              @@ (core "Term" @@ compute "Kv" --> graph "Graph" @@ compute "Kv" --> compute "Flow" @@ "s" @@ list "a"),
          "serializer">:
            doc "A function which serializes a list of output objects to a string representation" $
            list "a" --> compute "Flow" @@ "s" @@ string,
          "fileExtension">:
            doc "A file extension for the generated file(s)"
            string],

      def "SchemaSpec" $
        doc "The specification of a schema at the source end of a workflow" $
        union [
          "hydra">:
            doc "A native Hydra schema" $
            wf "HydraSchemaSpec",
          "file">:
            doc "A schema provided as a file, available at the given file path" $
            string,
          "provided">:
            doc "A schema which will be provided within the workflow" $
            unit],

      def "TransformWorkflow" $
        doc "The specification of a workflow which takes a schema specification, reads data from a directory, and writes data to another directory" $
        record [
          "name">:
            doc "A descriptive name for the workflow"
            string,
          "schemaSpec">:
            doc "The schema specification" $
            wf "SchemaSpec",
          "srcDir">:
            doc "The source directory"
            string,
          "destDir">:
            doc "The destination directory"
            string]]
