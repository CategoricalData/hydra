(defpackage :hydra.workflow
(:use :cl :hydra.core :hydra.module)
(:export :make-hydra_workflow_hydra_schema_spec :hydra_workflow_hydra_schema_spec? :hydra_workflow_hydra_schema_spec-modules :hydra_workflow_hydra_schema_spec-type_name :hydra_workflow_schema_spec-variants :make-hydra_workflow_transform_workflow :hydra_workflow_transform_workflow? :hydra_workflow_transform_workflow-name :hydra_workflow_transform_workflow-schema_spec :hydra_workflow_transform_workflow-src_dir :hydra_workflow_transform_workflow-dest_dir))

(in-package :hydra.workflow)

(cl:defstruct hydra_workflow_hydra_schema_spec modules type_name)

(cl:defvar hydra_workflow_schema_spec-variants (cl:list :hydra :file :provided))

(cl:defstruct hydra_workflow_transform_workflow name schema_spec src_dir dest_dir)
