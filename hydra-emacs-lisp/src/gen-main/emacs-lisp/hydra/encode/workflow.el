(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.encode.core)

(require 'hydra.encode.module)

(require 'hydra.lib.lists)

(require 'hydra.workflow)

(defvar hydra_encode_workflow_hydra_schema_spec (lambda (x) (list :record (make-hydra_core_record "hydra.workflow.HydraSchemaSpec" (list (make-hydra_core_field "modules" ((lambda (xs) (list :list ((hydra_lib_lists_map hydra_encode_module_module) xs))) ((lambda (v) (hydra_workflow_hydra_schema_spec-modules v)) x))) (make-hydra_core_field "typeName" (hydra_encode_core_name ((lambda (v) (hydra_workflow_hydra_schema_spec-type_name v)) x))))))))

(defvar hydra_encode_workflow_schema_spec (lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :hydra) ((lambda (y) (list :union (make-hydra_core_injection "hydra.workflow.SchemaSpec" (make-hydra_core_field "hydra" (hydra_encode_workflow_hydra_schema_spec y))))) match_value)) ((equal (car match_target) :file) ((lambda (y) (list :union (make-hydra_core_injection "hydra.workflow.SchemaSpec" (make-hydra_core_field "file" ((lambda (x) (list :literal (list :string x))) y))))) match_value)) ((equal (car match_target) :provided) ((lambda (y) (list :union (make-hydra_core_injection "hydra.workflow.SchemaSpec" (make-hydra_core_field "provided" ((lambda (_) (list :unit nil)) y))))) match_value)))) (cadr match_target))))

(defvar hydra_encode_workflow_transform_workflow (lambda (x) (list :record (make-hydra_core_record "hydra.workflow.TransformWorkflow" (list (make-hydra_core_field "name" ((lambda (x) (list :literal (list :string x))) ((lambda (v) (hydra_workflow_transform_workflow-name v)) x))) (make-hydra_core_field "schemaSpec" (hydra_encode_workflow_schema_spec ((lambda (v) (hydra_workflow_transform_workflow-schema_spec v)) x))) (make-hydra_core_field "srcDir" ((lambda (x) (list :literal (list :string x))) ((lambda (v) (hydra_workflow_transform_workflow-src_dir v)) x))) (make-hydra_core_field "destDir" ((lambda (x) (list :literal (list :string x))) ((lambda (v) (hydra_workflow_transform_workflow-dest_dir v)) x))))))))

(provide 'hydra.encode.workflow)
