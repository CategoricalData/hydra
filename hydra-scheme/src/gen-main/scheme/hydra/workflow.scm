(define-library (hydra workflow)
(export make-hydra_workflow_hydra_schema_spec hydra_workflow_hydra_schema_spec? hydra_workflow_hydra_schema_spec-modules hydra_workflow_hydra_schema_spec-type_name hydra_workflow_schema_spec-variants make-hydra_workflow_transform_workflow hydra_workflow_transform_workflow? hydra_workflow_transform_workflow-name hydra_workflow_transform_workflow-schema_spec hydra_workflow_transform_workflow-src_dir hydra_workflow_transform_workflow-dest_dir)
(import (scheme base) (hydra core) (hydra module))
(begin
(define-record-type hydra_workflow_hydra_schema_spec (make-hydra_workflow_hydra_schema_spec modules type_name) hydra_workflow_hydra_schema_spec? (modules hydra_workflow_hydra_schema_spec-modules) (type_name hydra_workflow_hydra_schema_spec-type_name))
(define hydra_workflow_schema_spec-variants (list 'hydra 'file 'provided))
(define-record-type hydra_workflow_transform_workflow (make-hydra_workflow_transform_workflow name schema_spec src_dir dest_dir) hydra_workflow_transform_workflow? (name hydra_workflow_transform_workflow-name) (schema_spec hydra_workflow_transform_workflow-schema_spec) (src_dir hydra_workflow_transform_workflow-src_dir) (dest_dir hydra_workflow_transform_workflow-dest_dir))))
