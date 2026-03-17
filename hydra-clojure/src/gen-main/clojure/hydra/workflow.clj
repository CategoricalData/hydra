(ns hydra.workflow
  (:require [hydra.core :refer :all] [hydra.module :refer :all]
))

(declare hydra_workflow_schema_spec-variants)

(defrecord hydra_workflow_hydra_schema_spec [modules type_name])
(defn make-hydra_workflow_hydra_schema_spec [modules type_name] (->hydra_workflow_hydra_schema_spec modules type_name))

(def hydra_workflow_schema_spec-variants (list :hydra :file :provided))

(defrecord hydra_workflow_transform_workflow [name schema_spec src_dir dest_dir])
(defn make-hydra_workflow_transform_workflow [name schema_spec src_dir dest_dir] (->hydra_workflow_transform_workflow name schema_spec src_dir dest_dir))
