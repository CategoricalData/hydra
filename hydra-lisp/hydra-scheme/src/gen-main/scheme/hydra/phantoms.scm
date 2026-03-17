(define-library (hydra phantoms)
(export make-hydra_phantoms_t_binding hydra_phantoms_t_binding? hydra_phantoms_t_binding-name hydra_phantoms_t_binding-term make-hydra_phantoms_t_term hydra_phantoms_t_term? hydra_phantoms_t_term-value)
(import (scheme base) (hydra core))
(begin
(define-record-type hydra_phantoms_t_binding (make-hydra_phantoms_t_binding name term) hydra_phantoms_t_binding? (name hydra_phantoms_t_binding-name) (term hydra_phantoms_t_binding-term))
(define-record-type hydra_phantoms_t_term (make-hydra_phantoms_t_term value) hydra_phantoms_t_term? (value hydra_phantoms_t_term-value))))
