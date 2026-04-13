(defpackage :hydra.eval.lib.pairs
(:use :cl :hydra.core :hydra.errors :hydra.lib.pairs :hydra.show.core)
(:export :hydra_eval_lib_pairs_bimap :hydra_eval_lib_pairs_first :hydra_eval_lib_pairs_second))

(in-package :hydra.eval.lib.pairs)

(cl:defvar hydra_eval_lib_pairs_bimap (cl:lambda (cx) (cl:lambda (g) (cl:lambda (first_fun) (cl:lambda (second_fun) (cl:lambda (pair_term) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :pair) ((cl:lambda (p) (let ((fst (hydra_lib_pairs_first p))) (let ((snd (hydra_lib_pairs_second p))) (list :right (list :pair (cl:list (list :application (make-hydra_core_application first_fun fst)) (list :application (make-hydra_core_application second_fun snd)))))))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "pair value" (hydra_show_core_term pair_term)))))))) (cadr match_target))) pair_term)))))))

(cl:defvar hydra_eval_lib_pairs_first (cl:lambda (cx) (cl:lambda (g) (cl:lambda (pair_term) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :pair) ((cl:lambda (p) (list :right (hydra_lib_pairs_first p))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "pair value" (hydra_show_core_term pair_term)))))))) (cadr match_target))) pair_term)))))

(cl:defvar hydra_eval_lib_pairs_second (cl:lambda (cx) (cl:lambda (g) (cl:lambda (pair_term) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :pair) ((cl:lambda (p) (list :right (hydra_lib_pairs_second p))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "pair value" (hydra_show_core_term pair_term)))))))) (cadr match_target))) pair_term)))))
