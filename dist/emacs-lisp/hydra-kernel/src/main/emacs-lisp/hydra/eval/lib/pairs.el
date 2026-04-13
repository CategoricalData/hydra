(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.errors)

(require 'hydra.lib.pairs)

(require 'hydra.show.core)

(defvar hydra_eval_lib_pairs_bimap (lambda (cx) (lambda (g) (lambda (first_fun) (lambda (second_fun) (lambda (pair_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :pair) (funcall (lambda (p) (let ((fst (hydra_lib_pairs_first p))) (let ((snd (hydra_lib_pairs_second p))) (list :right (list :pair (list (list :application (make-hydra_core_application first_fun fst)) (list :application (make-hydra_core_application second_fun snd)))))))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "pair value" (hydra_show_core_term pair_term)))))))) (cadr match_target))) pair_term)))))))

(defvar hydra_eval_lib_pairs_first (lambda (cx) (lambda (g) (lambda (pair_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :pair) (funcall (lambda (p) (list :right (hydra_lib_pairs_first p))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "pair value" (hydra_show_core_term pair_term)))))))) (cadr match_target))) pair_term)))))

(defvar hydra_eval_lib_pairs_second (lambda (cx) (lambda (g) (lambda (pair_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :pair) (funcall (lambda (p) (list :right (hydra_lib_pairs_second p))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "pair value" (hydra_show_core_term pair_term)))))))) (cadr match_target))) pair_term)))))

(provide 'hydra.eval.lib.pairs)
