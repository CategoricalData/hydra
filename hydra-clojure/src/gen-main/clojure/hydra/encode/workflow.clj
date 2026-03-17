(ns hydra.encode.workflow
  (:require [hydra.core :refer :all] [hydra.encode.core :refer :all] [hydra.encode.module :refer :all] [hydra.lib.lists :refer :all] [hydra.workflow :refer :all]
))

(declare hydra_encode_workflow_hydra_schema_spec hydra_encode_workflow_schema_spec hydra_encode_workflow_transform_workflow)

(def hydra_encode_workflow_hydra_schema_spec (fn [x] (list :record (->hydra_core_record "hydra.workflow.HydraSchemaSpec" (list (->hydra_core_field "modules" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_module_module) xs))) ((fn [v] (:modules v)) x))) (->hydra_core_field "typeName" (hydra_encode_core_name ((fn [v] (:type_name v)) x))))))))

(def hydra_encode_workflow_schema_spec (fn [match_target] ((fn [match_value] (cond (= (first match_target) :hydra) ((fn [y] (list :union (->hydra_core_injection "hydra.workflow.SchemaSpec" (->hydra_core_field "hydra" (hydra_encode_workflow_hydra_schema_spec y))))) match_value) (= (first match_target) :file) ((fn [y] (list :union (->hydra_core_injection "hydra.workflow.SchemaSpec" (->hydra_core_field "file" ((fn [x] (list :literal (list :string x))) y))))) match_value) (= (first match_target) :provided) ((fn [y] (list :union (->hydra_core_injection "hydra.workflow.SchemaSpec" (->hydra_core_field "provided" ((fn [_] (list :unit nil)) y))))) match_value))) (second match_target))))

(def hydra_encode_workflow_transform_workflow (fn [x] (list :record (->hydra_core_record "hydra.workflow.TransformWorkflow" (list (->hydra_core_field "name" ((fn [x] (list :literal (list :string x))) ((fn [v] (:name v)) x))) (->hydra_core_field "schemaSpec" (hydra_encode_workflow_schema_spec ((fn [v] (:schema_spec v)) x))) (->hydra_core_field "srcDir" ((fn [x] (list :literal (list :string x))) ((fn [v] (:src_dir v)) x))) (->hydra_core_field "destDir" ((fn [x] (list :literal (list :string x))) ((fn [v] (:dest_dir v)) x))))))))
