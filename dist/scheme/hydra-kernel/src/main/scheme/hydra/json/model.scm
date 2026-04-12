(define-library (hydra json model)
(export hydra_json_model_value-variants)
(import (scheme base))
(begin
(define hydra_json_model_value-variants (list 'array 'boolean 'null 'number 'object 'string))))
