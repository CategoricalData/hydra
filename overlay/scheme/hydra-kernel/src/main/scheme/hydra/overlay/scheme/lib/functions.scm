(define-library (hydra overlay scheme lib functions)
  (import (scheme base))
  (export hydra_overlay_scheme_lib_functions_absurd
          hydra_overlay_scheme_lib_functions_identity)
  (begin

    ;; Eliminate a value of the uninhabited void type. Unreachable in any well-typed program.
    (define hydra_overlay_scheme_lib_functions_absurd
      (lambda (v) (error "hydra.lib.functions.absurd: void has no inhabitants")))

    ;; Return a value unchanged.
    (define hydra_overlay_scheme_lib_functions_identity
      (lambda (x) x))))
