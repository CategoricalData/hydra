(define-library (hydra encode phantoms)
(export hydra_encode_phantoms_t_term hydra_encode_phantoms_t_binding hydra_encode_phantoms_t_term_definition)
(import (scheme base) (hydra core) (hydra encode core) (hydra phantoms))
(begin
(define hydra_encode_phantoms_t_term (lambda (a) (lambda (x) (list 'wrap (make-hydra_core_wrapped_term "hydra.phantoms.TTerm" (hydra_encode_core_term ((lambda (v) v) x)))))))
(define hydra_encode_phantoms_t_binding (lambda (a) (lambda (x) (list 'record (make-hydra_core_record "hydra.phantoms.TBinding" (list (make-hydra_core_field "name" (hydra_encode_core_name ((lambda (v) (hydra_phantoms_t_binding-name v)) x))) (make-hydra_core_field "term" ((hydra_encode_phantoms_t_term a) ((lambda (v) (hydra_phantoms_t_binding-term v)) x)))))))))
(define hydra_encode_phantoms_t_term_definition (lambda (a) (lambda (x) (list 'record (make-hydra_core_record "hydra.phantoms.TTermDefinition" (list (make-hydra_core_field "name" (hydra_encode_core_name ((lambda (v) (hydra_phantoms_t_term_definition-name v)) x))) (make-hydra_core_field "term" ((hydra_encode_phantoms_t_term a) ((lambda (v) (hydra_phantoms_t_term_definition-term v)) x)))))))))))
