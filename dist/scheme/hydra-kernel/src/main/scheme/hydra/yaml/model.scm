(define-library (hydra yaml model)
(export hydra_yaml_model_node-variants hydra_yaml_model_scalar-variants)
(import (scheme base))
(begin
(define hydra_yaml_model_node-variants (list 'mapping 'scalar 'sequence))
(define hydra_yaml_model_scalar-variants (list 'bool 'decimal 'float 'int 'null 'str))))
