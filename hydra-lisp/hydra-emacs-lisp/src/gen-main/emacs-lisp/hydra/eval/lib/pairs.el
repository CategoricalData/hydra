(require 'cl-lib)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.error)

(require 'hydra.lib.pairs)

(require 'hydra.lib.strings)

(require 'hydra.show.core)

(defvar hydra_eval_lib_pairs_bimap (lambda (cx) (lambda (g) (lambda (first_fun) (lambda (second_fun) (lambda (pair_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :pair) (funcall (lambda (p) (let ((fst (hydra_lib_pairs_first p))) (let ((snd (hydra_lib_pairs_second p))) (list :right (list :pair (list (list :application (make-hydra_core_application first_fun fst)) (list :application (make-hydra_core_application second_fun snd)))))))) match_value)) (t (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "expected ") "pair value")) " but found ")) (hydra_show_core_term pair_term))) cx))))) (cadr match_target))) pair_term)))))))

(provide 'hydra.eval.lib.pairs)
