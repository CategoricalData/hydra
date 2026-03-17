(define-library (hydra encode classes)
(export hydra_encode_classes_type_class)
(import (scheme base) (hydra classes) (hydra core))
(begin
(define hydra_encode_classes_type_class (lambda (match_target) ((lambda (match_value) (cond ((equal? (car match_target) 'equality) ((lambda (y) (list 'union (make-hydra_core_injection "hydra.classes.TypeClass" (make-hydra_core_field "equality" ((lambda (_) (list 'unit '())) y))))) match_value)) ((equal? (car match_target) 'ordering) ((lambda (y) (list 'union (make-hydra_core_injection "hydra.classes.TypeClass" (make-hydra_core_field "ordering" ((lambda (_) (list 'unit '())) y))))) match_value)))) (cadr match_target))))))
