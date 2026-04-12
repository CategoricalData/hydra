(define-library (hydra ext org yaml model)
(export hydra_ext_org_yaml_model_node-variants hydra_ext_org_yaml_model_scalar-variants)
(import (scheme base))
(begin
(define hydra_ext_org_yaml_model_node-variants (list 'mapping 'scalar 'sequence))
(define hydra_ext_org_yaml_model_scalar-variants (list 'bool 'float 'int 'null 'str))))
