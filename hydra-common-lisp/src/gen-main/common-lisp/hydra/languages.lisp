(defpackage :hydra.languages
(:use :cl :hydra.coders :hydra.core :hydra.lib.sets :hydra.reflect)
(:export :hydra_languages_hydra_language))

(in-package :hydra.languages)

(cl:defvar hydra_languages_hydra_language (let* ((elimination_variants (hydra_lib_sets_from_list hydra_reflect_elimination_variants)) (float_types (hydra_lib_sets_from_list hydra_reflect_float_types)) (function_variants (hydra_lib_sets_from_list hydra_reflect_function_variants)) (integer_types (hydra_lib_sets_from_list hydra_reflect_integer_types)) (literal_variants (hydra_lib_sets_from_list hydra_reflect_literal_variants)) (term_variants (hydra_lib_sets_from_list hydra_reflect_term_variants)) (type_variants (hydra_lib_sets_from_list hydra_reflect_type_variants)) (types (cl:lambda (t_) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond (t cl:t))) (cadr match_target))) t_)))) (make-hydra_coders_language "hydra.core" (make-hydra_coders_language_constraints elimination_variants literal_variants float_types function_variants integer_types term_variants type_variants types))))
