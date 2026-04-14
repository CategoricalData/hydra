(define-library (hydra eval lib logic)
(export hydra_eval_lib_logic_and hydra_eval_lib_logic_not hydra_eval_lib_logic_or)
(import (scheme base) (hydra core))
(begin
(define hydra_eval_lib_logic_and (lambda (cx) (lambda (g) (lambda (a) (lambda (b) (list 'right (list 'application (make-hydra_core_application (list 'application (make-hydra_core_application (list 'application (make-hydra_core_application (list 'variable "hydra.lib.logic.ifElse") a)) b)) (list 'literal (list 'boolean #f))))))))))
(define hydra_eval_lib_logic_not (lambda (cx) (lambda (g) (lambda (a) (list 'right (list 'application (make-hydra_core_application (list 'application (make-hydra_core_application (list 'application (make-hydra_core_application (list 'variable "hydra.lib.logic.ifElse") a)) (list 'literal (list 'boolean #f)))) (list 'literal (list 'boolean #t)))))))))
(define hydra_eval_lib_logic_or (lambda (cx) (lambda (g) (lambda (a) (lambda (b) (list 'right (list 'application (make-hydra_core_application (list 'application (make-hydra_core_application (list 'application (make-hydra_core_application (list 'variable "hydra.lib.logic.ifElse") a)) (list 'literal (list 'boolean #t)))) b))))))))))
