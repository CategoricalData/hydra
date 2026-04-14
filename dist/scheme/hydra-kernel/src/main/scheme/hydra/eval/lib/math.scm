(define-library (hydra eval lib math)
(export hydra_eval_lib_math_even hydra_eval_lib_math_odd hydra_eval_lib_math_pred hydra_eval_lib_math_succ)
(import (scheme base) (hydra core))
(begin
(define hydra_eval_lib_math_even (lambda (cx) (lambda (g) (lambda (x) (list 'right (list 'application (make-hydra_core_application (list 'application (make-hydra_core_application (list 'variable "hydra.lib.equality.equal") (list 'application (make-hydra_core_application (list 'application (make-hydra_core_application (list 'variable "hydra.lib.math.mod") x)) (list 'literal (list 'integer (list 'int32 2))))))) (list 'literal (list 'integer (list 'int32 0))))))))))
(define hydra_eval_lib_math_odd (lambda (cx) (lambda (g) (lambda (x) (list 'right (list 'application (make-hydra_core_application (list 'variable "hydra.lib.logic.not") (list 'application (make-hydra_core_application (list 'variable "hydra.lib.math.even") x)))))))))
(define hydra_eval_lib_math_pred (lambda (cx) (lambda (g) (lambda (x) (list 'right (list 'application (make-hydra_core_application (list 'application (make-hydra_core_application (list 'variable "hydra.lib.math.sub") x)) (list 'literal (list 'integer (list 'int32 1))))))))))
(define hydra_eval_lib_math_succ (lambda (cx) (lambda (g) (lambda (x) (list 'right (list 'application (make-hydra_core_application (list 'application (make-hydra_core_application (list 'variable "hydra.lib.math.add") x)) (list 'literal (list 'integer (list 'int32 1))))))))))))
