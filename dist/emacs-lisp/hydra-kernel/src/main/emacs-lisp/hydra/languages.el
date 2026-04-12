(require 'cl-lib)

(require 'hydra.coders)

(require 'hydra.core)

(require 'hydra.lib.sets)

(require 'hydra.reflect)

(defvar hydra_languages_hydra_language (let* ((elimination_variants (hydra_lib_sets_from_list hydra_reflect_elimination_variants)) (float_types (hydra_lib_sets_from_list hydra_reflect_float_types)) (function_variants (hydra_lib_sets_from_list hydra_reflect_function_variants)) (integer_types (hydra_lib_sets_from_list hydra_reflect_integer_types)) (literal_variants (hydra_lib_sets_from_list hydra_reflect_literal_variants)) (term_variants (hydra_lib_sets_from_list hydra_reflect_term_variants)) (type_variants (hydra_lib_sets_from_list hydra_reflect_type_variants)) (types (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond (t t))) (cadr match_target))) t_)))) (make-hydra_coders_language "hydra.core" (make-hydra_coders_language_constraints elimination_variants literal_variants float_types function_variants integer_types term_variants type_variants types))))

(provide 'hydra.languages)
