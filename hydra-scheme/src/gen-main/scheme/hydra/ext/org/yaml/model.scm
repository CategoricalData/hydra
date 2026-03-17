(define-library (hydra ext org yaml model)
(import (scheme base) (hydra lib chars) (hydra lib eithers) (hydra lib equality) (hydra lib lists) (hydra lib literals) (hydra lib logic) (hydra lib maps) (hydra lib math) (hydra lib maybes) (hydra lib pairs) (hydra lib sets) (hydra lib strings))
(export node-variants scalar-variants)
(begin
(define Node-variants (list 'mapping 'scalar 'sequence))
(define Scalar-variants (list 'bool 'float 'int 'null 'str))))
