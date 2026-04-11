(define-library (hydra eval lib pairs)
(export hydra_eval_lib_pairs_bimap)
(import (scheme base) (hydra context) (hydra core) (hydra error) (hydra lib pairs) (hydra lib strings) (hydra show core))
(begin
(define hydra_eval_lib_pairs_bimap (lambda (cx) (lambda (g) (lambda (first_fun) (lambda (second_fun) (lambda (pair_term) ((lambda (match_target) ((lambda (match_value) (cond ((equal? (car match_target) 'pair) ((lambda (p) (let ((fst (hydra_lib_pairs_first p))) (let ((snd (hydra_lib_pairs_second p))) (list 'right (list 'pair (list (list 'application (make-hydra_core_application first_fun fst)) (list 'application (make-hydra_core_application second_fun snd)))))))) match_value)) (else (list 'left (make-hydra_context_in_context (list 'other ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "expected ") "pair value")) " but found ")) (hydra_show_core_term pair_term))) cx))))) (cadr match_target))) pair_term)))))))))
