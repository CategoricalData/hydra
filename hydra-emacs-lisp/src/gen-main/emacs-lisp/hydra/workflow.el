(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.module)

(cl-defstruct hydra_workflow_hydra_schema_spec modules type_name)

(defvar hydra_workflow_schema_spec-variants (list :hydra :file :provided))

(cl-defstruct hydra_workflow_transform_workflow name schema_spec src_dir dest_dir)

(provide 'hydra.workflow)
