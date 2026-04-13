(define-library (hydra eval lib equality)
(export hydra_eval_lib_equality_identity hydra_eval_lib_equality_max hydra_eval_lib_equality_min)
(import (scheme base) (hydra core))
(begin
(define hydra_eval_lib_equality_identity (lambda (cx) (lambda (g) (lambda (x) (list 'right x)))))
(define hydra_eval_lib_equality_max (lambda (cx) (lambda (g) (lambda (x) (lambda (y) (list 'right (list 'application (make-hydra_core_application (list 'application (make-hydra_core_application (list 'application (make-hydra_core_application (list 'variable "hydra.lib.logic.ifElse") (list 'application (make-hydra_core_application (list 'application (make-hydra_core_application (list 'variable "hydra.lib.equality.gte") x)) y)))) x)) y))))))))
(define hydra_eval_lib_equality_min (lambda (cx) (lambda (g) (lambda (x) (lambda (y) (list 'right (list 'application (make-hydra_core_application (list 'application (make-hydra_core_application (list 'application (make-hydra_core_application (list 'variable "hydra.lib.logic.ifElse") (list 'application (make-hydra_core_application (list 'application (make-hydra_core_application (list 'variable "hydra.lib.equality.lte") x)) y)))) x)) y))))))))))
