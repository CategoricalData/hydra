(define-library (hydra eval lib sets)
(export hydra_eval_lib_sets_map)
(import (scheme base) (hydra core) (hydra extract core) (hydra lib eithers) (hydra lib lists) (hydra lib sets))
(begin
(define hydra_eval_lib_sets_map (lambda (cx) (lambda (g) (lambda (fun) (lambda (set_term) ((hydra_lib_eithers_bind (((hydra_extract_core_set cx) g) set_term)) (lambda (elements) (list 'right (list 'application (make-hydra_core_application (list 'function (list 'primitive "hydra.lib.sets.fromList")) (list 'list ((hydra_lib_lists_map (lambda (el) (list 'application (make-hydra_core_application fun el)))) (hydra_lib_sets_to_list elements)))))))))))))))
