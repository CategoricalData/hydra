(require 'cl-lib)

(require 'hydra.coders)

(require 'hydra.core)

(require 'hydra.lib.sets)

(require 'hydra.rewriting)

(require 'hydra.variants)

(defvar hydra_ext_org_json_language_json_language (let* ((elimination_variants hydra_lib_sets_empty) (float_types (hydra_lib_sets_from_list (list (list :bigfloat nil)))) (function_variants hydra_lib_sets_empty) (integer_types (hydra_lib_sets_from_list (list (list :bigint nil)))) (literal_variants (hydra_lib_sets_from_list (list (list :boolean nil) (list :float nil) (list :integer nil) (list :string nil)))) (term_variants (hydra_lib_sets_from_list (list (list :list nil) (list :literal nil) (list :map nil) (list :maybe nil) (list :record nil)))) (type_predicate (lambda (typ) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :maybe) ((lambda (inner_type) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :maybe) ((lambda (_) nil) match_value)) (t t))) (cadr match_target))) inner_type)) match_value)) (t t))) (cadr match_target))) (hydra_rewriting_deannotate_type typ)))) (type_variants (hydra_lib_sets_from_list (list (list :list nil) (list :literal nil) (list :map nil) (list :maybe nil) (list :record nil))))) (make-hydra_coders_language "hydra.ext.org.json" (make-hydra_coders_language_constraints elimination_variants literal_variants float_types function_variants integer_types term_variants type_variants type_predicate))))

(provide 'hydra.ext.org.json.language)
