(defpackage :hydra.eval.lib.pairs
(:use :cl :hydra.context :hydra.core :hydra.error :hydra.lib.pairs :hydra.lib.strings :hydra.show.core)
(:export :hydra_eval_lib_pairs_bimap))

(in-package :hydra.eval.lib.pairs)

(cl:defvar hydra_eval_lib_pairs_bimap (cl:lambda (cx) (cl:lambda (g) (cl:lambda (first_fun) (cl:lambda (second_fun) (cl:lambda (pair_term) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :pair) ((cl:lambda (p) (let ((fst (hydra_lib_pairs_first p))) (let ((snd (hydra_lib_pairs_second p))) (list :right (list :pair (cl:list (list :application (make-hydra_core_application first_fun fst)) (list :application (make-hydra_core_application second_fun snd)))))))) match_value)) (t (list :left (make-hydra_context_in_context (list :other ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 "expected ") "pair value")) " but found ")) (hydra_show_core_term pair_term))) cx))))) (cadr match_target))) pair_term)))))))
