(require 'cl-lib)

(require 'hydra.coders)

(require 'hydra.compute)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.error)

(require 'hydra.formatting)

(require 'hydra.lib.eithers)

(require 'hydra.lib.lists)

(require 'hydra.lib.literals)

(require 'hydra.lib.logic)

(require 'hydra.lib.maybes)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.module)

(require 'hydra.names)

(require 'hydra.reflect)

(require 'hydra.rewriting)

(require 'hydra.show.core)

(require 'hydra.util)

(require 'hydra.variants)

(defvar hydra_adapt_utils_bidirectional (lambda (f) (make-hydra_compute_coder (f (list :encode nil)) (f (list :decode nil)))))

(defvar hydra_adapt_utils_id_coder (make-hydra_compute_coder (lambda (_cx) (lambda (x) (list :right x))) (lambda (_cx) (lambda (x) (list :right x)))))

(defvar hydra_adapt_utils_choose_adapter (lambda (alts) (lambda (supported) (lambda (show) (lambda (describe) (lambda (typ) (if (supported typ) (list :right (make-hydra_compute_adapter nil typ typ hydra_adapt_utils_id_coder)) ((hydra_lib_eithers_bind (alts typ)) (lambda (raw) (let ((candidates ((hydra_lib_lists_filter (lambda (adapter) (supported ((lambda (v) (hydra_compute_adapter-target v)) adapter)))) raw))) (if (hydra_lib_lists_null candidates) (list :left (hydra_lib_strings_cat (list "no adapters found for " (describe typ) (if (hydra_lib_lists_null raw) "" (hydra_lib_strings_cat (list " (discarded " (hydra_lib_literals_show_int32 (hydra_lib_lists_length raw)) " unsupported candidate types: " ((hydra_show_core_list show) ((hydra_lib_lists_map (lambda (v) (hydra_compute_adapter-target v))) raw)) ")"))) ". Original type: " (show typ)))) (list :right (hydra_lib_lists_head candidates)))))))))))))

(defvar hydra_adapt_utils_compose_coders (lambda (c1) (lambda (c2) (make-hydra_compute_coder (lambda (cx) (lambda (a) ((hydra_lib_eithers_bind ((((lambda (v) (hydra_compute_coder-encode v)) c1) cx) a)) (lambda (b1) ((((lambda (v) (hydra_compute_coder-encode v)) c2) cx) b1))))) (lambda (cx) (lambda (c) ((hydra_lib_eithers_bind ((((lambda (v) (hydra_compute_coder-decode v)) c2) cx) c)) (lambda (b2) ((((lambda (v) (hydra_compute_coder-decode v)) c1) cx) b2)))))))))

(defvar hydra_adapt_utils_encode_decode (lambda (dir) (lambda (coder) (lambda (cx) (lambda (term) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :encode) ((lambda (_) ((((lambda (v) (hydra_compute_coder-encode v)) coder) cx) term)) match_value)) ((equal (car match_target) :decode) ((lambda (_) ((((lambda (v) (hydra_compute_coder-decode v)) coder) cx) term)) match_value)))) (cadr match_target))) dir))))))

(defvar hydra_adapt_utils_float_type_is_supported (lambda (constraints) (lambda (ft) ((hydra_lib_sets_member ft) ((lambda (v) (hydra_coders_language_constraints-float_types v)) constraints)))))

(defvar hydra_adapt_utils_id_adapter (lambda (t_) (make-hydra_compute_adapter nil t_ t_ hydra_adapt_utils_id_coder)))

(defvar hydra_adapt_utils_integer_type_is_supported (lambda (constraints) (lambda (it) ((hydra_lib_sets_member it) ((lambda (v) (hydra_coders_language_constraints-integer_types v)) constraints)))))

(defvar hydra_adapt_utils_literal_type_is_supported (lambda (constraints) (lambda (lt) (let ((is_supported (lambda (lt) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :float) ((lambda (ft) ((hydra_adapt_utils_float_type_is_supported constraints) ft)) match_value)) ((equal (car match_target) :integer) ((lambda (it) ((hydra_adapt_utils_integer_type_is_supported constraints) it)) match_value)) (t t))) (cadr match_target))) lt)))) ((hydra_lib_logic_and ((hydra_lib_sets_member (hydra_reflect_literal_type_variant lt)) ((lambda (v) (hydra_coders_language_constraints-literal_variants v)) constraints))) (is_supported lt))))))

(defvar hydra_adapt_utils_name_to_file_path (lambda (ns_conv) (lambda (local_conv) (lambda (ext) (lambda (name) (let ((qual_name (hydra_names_qualify_name name))) (let ((ns_ ((lambda (v) (hydra_module_qualified_name-namespace v)) qual_name))) (let ((local ((lambda (v) (hydra_module_qualified_name-local v)) qual_name))) (let ((ns_to_file_path (lambda (ns_) ((hydra_lib_strings_intercalate "/") ((hydra_lib_lists_map (lambda (part) (((hydra_formatting_convert_case (list :camel nil)) ns_conv) part))) ((hydra_lib_strings_split_on ".") ((lambda (v) v) ns_))))))) (let ((prefix (((hydra_lib_maybes_maybe "") (lambda (n) ((hydra_lib_strings_cat2 (ns_to_file_path n)) "/"))) ns_))) (let ((suffix (((hydra_formatting_convert_case (list :pascal nil)) local_conv) local))) (hydra_lib_strings_cat (list prefix suffix "." ((lambda (v) v) ext))))))))))))))

(defvar hydra_adapt_utils_type_is_supported (lambda (constraints) (lambda (t_) (let ((base (hydra_rewriting_deannotate_type t_))) (let ((is_variable (lambda (v) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :variable) ((lambda (_) t) match_value)) (t nil))) (cadr match_target))) v)))) (let ((is_supported_variant (lambda (v) ((hydra_lib_logic_or (is_variable v)) ((hydra_lib_sets_member v) ((lambda (v) (hydra_coders_language_constraints-type_variants v)) constraints)))))) (let ((is_supported (lambda (base) ((lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :annotated) ((lambda (at) ((hydra_adapt_utils_type_is_supported constraints) ((lambda (v) (hydra_core_annotated_type-body v)) at))) match_value)) ((equal (car match_target) :application) ((lambda (app) ((hydra_lib_logic_and ((hydra_adapt_utils_type_is_supported constraints) ((lambda (v) (hydra_core_application_type-function v)) app))) ((hydra_adapt_utils_type_is_supported constraints) ((lambda (v) (hydra_core_application_type-argument v)) app)))) match_value)) ((equal (car match_target) :either) ((lambda (et) ((hydra_lib_logic_and ((hydra_adapt_utils_type_is_supported constraints) ((lambda (v) (hydra_core_either_type-left v)) et))) ((hydra_adapt_utils_type_is_supported constraints) ((lambda (v) (hydra_core_either_type-right v)) et)))) match_value)) ((equal (car match_target) :forall) ((lambda (ft) ((hydra_adapt_utils_type_is_supported constraints) ((lambda (v) (hydra_core_forall_type-body v)) ft))) match_value)) ((equal (car match_target) :function) ((lambda (ft) ((hydra_lib_logic_and ((hydra_adapt_utils_type_is_supported constraints) ((lambda (v) (hydra_core_function_type-domain v)) ft))) ((hydra_adapt_utils_type_is_supported constraints) ((lambda (v) (hydra_core_function_type-codomain v)) ft)))) match_value)) ((equal (car match_target) :list) ((lambda (lt) ((hydra_adapt_utils_type_is_supported constraints) lt)) match_value)) ((equal (car match_target) :literal) ((lambda (at) ((hydra_adapt_utils_literal_type_is_supported constraints) at)) match_value)) ((equal (car match_target) :map) ((lambda (mt) ((hydra_lib_logic_and ((hydra_adapt_utils_type_is_supported constraints) ((lambda (v) (hydra_core_map_type-keys v)) mt))) ((hydra_adapt_utils_type_is_supported constraints) ((lambda (v) (hydra_core_map_type-values v)) mt)))) match_value)) ((equal (car match_target) :maybe) ((lambda (ot) ((hydra_adapt_utils_type_is_supported constraints) ot)) match_value)) ((equal (car match_target) :pair) ((lambda (pt) ((hydra_lib_logic_and ((hydra_adapt_utils_type_is_supported constraints) ((lambda (v) (hydra_core_pair_type-first v)) pt))) ((hydra_adapt_utils_type_is_supported constraints) ((lambda (v) (hydra_core_pair_type-second v)) pt)))) match_value)) ((equal (car match_target) :record) ((lambda (rt) (((hydra_lib_lists_foldl hydra_lib_logic_and) t) ((hydra_lib_lists_map (lambda (field) ((hydra_adapt_utils_type_is_supported constraints) ((lambda (v) (hydra_core_field_type-type v)) field)))) ((lambda (v) (hydra_core_row_type-fields v)) rt)))) match_value)) ((equal (car match_target) :set) ((lambda (st) ((hydra_adapt_utils_type_is_supported constraints) st)) match_value)) ((equal (car match_target) :union) ((lambda (rt) (((hydra_lib_lists_foldl hydra_lib_logic_and) t) ((hydra_lib_lists_map (lambda (field) ((hydra_adapt_utils_type_is_supported constraints) ((lambda (v) (hydra_core_field_type-type v)) field)))) ((lambda (v) (hydra_core_row_type-fields v)) rt)))) match_value)) ((equal (car match_target) :unit) ((lambda (_) t) match_value)) ((equal (car match_target) :wrap) ((lambda (wt) ((hydra_adapt_utils_type_is_supported constraints) ((lambda (v) (hydra_core_wrapped_type-body v)) wt))) match_value)) ((equal (car match_target) :variable) ((lambda (_) t) match_value)))) (cadr match_target))) base)))) ((hydra_lib_logic_and (((lambda (v) (hydra_coders_language_constraints-types v)) constraints) base)) ((hydra_lib_logic_and (is_supported_variant (hydra_reflect_type_variant base))) (is_supported base))))))))))

(defvar hydra_adapt_utils_unidirectional_coder (lambda (m) (make-hydra_compute_coder m (lambda (cx) (lambda (_) (list :left (make-hydra_context_in_context "inbound mapping is unsupported" cx)))))))

(provide 'hydra.adapt.utils)
