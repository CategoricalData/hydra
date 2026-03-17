(define-library (hydra monads)
(export hydra_monads_empty_context hydra_monads_maybe_to_list)
(import (scheme base) (hydra context) (hydra lib lists) (hydra lib maps) (hydra lib maybes))
(begin
(define hydra_monads_empty_context (make-hydra_context_context (list) (list) hydra_lib_maps_empty))
(define hydra_monads_maybe_to_list (lambda (mx) (((hydra_lib_maybes_maybe (list)) hydra_lib_lists_pure) mx)))))
