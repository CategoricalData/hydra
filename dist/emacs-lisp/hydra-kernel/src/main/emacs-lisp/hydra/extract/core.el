(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.errors)

(require 'hydra.lexical)

(require 'hydra.lib.eithers)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.literals)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.show.core)

(require 'hydra.show.errors)

(require 'hydra.strip)

(defvar hydra_extract_core_bigfloat_value (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :bigfloat) (funcall (lambda (f) (list :right f)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "bigfloat" (hydra_show_core_float v)))))))) (cadr match_target))) v)))

(defvar hydra_extract_core_float_literal (lambda (lit) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :float) (funcall (lambda (v) (list :right v)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "floating-point value" (hydra_show_core_literal lit)))))))) (cadr match_target))) lit)))

(defvar hydra_extract_core_literal (lambda (graph) (lambda (term0) (funcall (hydra_lib_eithers_bind (funcall (hydra_lexical_strip_and_dereference_term graph) term0)) (lambda (term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :literal) (funcall (lambda (lit) (list :right lit)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "literal" (hydra_show_core_term term)))))))) (cadr match_target))) term))))))

(defvar hydra_extract_core_bigfloat (lambda (graph) (lambda (t_) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_literal graph) t_)) (lambda (l) (funcall (hydra_lib_eithers_bind (hydra_extract_core_float_literal l)) (lambda (f) (hydra_extract_core_bigfloat_value f))))))))

(defvar hydra_extract_core_bigint_value (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :bigint) (funcall (lambda (i) (list :right i)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "bigint" (hydra_show_core_integer v)))))))) (cadr match_target))) v)))

(defvar hydra_extract_core_integer_literal (lambda (lit) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :integer) (funcall (lambda (v) (list :right v)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "integer value" (hydra_show_core_literal lit)))))))) (cadr match_target))) lit)))

(defvar hydra_extract_core_bigint (lambda (graph) (lambda (t_) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_literal graph) t_)) (lambda (l) (funcall (hydra_lib_eithers_bind (hydra_extract_core_integer_literal l)) (lambda (i) (hydra_extract_core_bigint_value i))))))))

(defvar hydra_extract_core_binary_literal (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :binary) (funcall (lambda (b) (list :right b)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "binary" (hydra_show_core_literal v)))))))) (cadr match_target))) v)))

(defvar hydra_extract_core_binary (lambda (graph) (lambda (t_) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_literal graph) t_)) (lambda (l) (hydra_extract_core_binary_literal l))))))

(defvar hydra_extract_core_boolean_literal (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :boolean) (funcall (lambda (b) (list :right b)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "boolean" (hydra_show_core_literal v)))))))) (cadr match_target))) v)))

(defvar hydra_extract_core_boolean (lambda (graph) (lambda (t_) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_literal graph) t_)) (lambda (l) (hydra_extract_core_boolean_literal l))))))

(defvar hydra_extract_core_cases (lambda (name) (lambda (graph) (lambda (term0) (funcall (hydra_lib_eithers_bind (funcall (hydra_lexical_strip_and_dereference_term graph) term0)) (lambda (term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :cases) (funcall (lambda (cs) (if (funcall (hydra_lib_equality_equal (funcall (lambda (v) v) (funcall (lambda (v) (hydra_core_case_statement-type_name v)) cs))) (funcall (lambda (v) v) name)) (list :right cs) (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error (funcall (hydra_lib_strings_cat2 "case statement for type ") (funcall (lambda (v) v) name)) (hydra_show_core_term term))))))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "case statement" (hydra_show_core_term term)))))))) (cadr match_target))) term)))))))

(defvar hydra_extract_core_case_field (lambda (name) (lambda (n) (lambda (graph) (lambda (term) (let ((field_name n)) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_extract_core_cases name) graph) term)) (lambda (cs) (let ((matching (funcall (hydra_lib_lists_filter (lambda (f) (funcall (hydra_lib_equality_equal (funcall (lambda (v) v) (funcall (lambda (v) (hydra_core_field-name v)) f))) (funcall (lambda (v) v) field_name)))) (funcall (lambda (v) (hydra_core_case_statement-cases v)) cs)))) (if (hydra_lib_lists_null matching) (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "matching case" "no matching case")))) (list :right (hydra_lib_lists_head matching))))))))))))

(defvar hydra_extract_core_strip_with_decoding_error (lambda (g) (lambda (term) (funcall (funcall (hydra_lib_eithers_bimap (lambda (_e) (hydra_show_errors_error _e))) (lambda (x) x)) (funcall (hydra_lexical_strip_and_dereference_term_either g) term)))))

(defvar hydra_extract_core_decode_either (lambda (left_decoder) (lambda (right_decoder) (lambda (g) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_strip_with_decoding_error g) term)) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :either) (funcall (lambda (e) (funcall (funcall (hydra_lib_eithers_either (lambda (lv) (funcall (hydra_lib_eithers_map (lambda (x) (list :left x))) (funcall (left_decoder g) lv)))) (lambda (rv) (funcall (hydra_lib_eithers_map (lambda (x) (list :right x))) (funcall (right_decoder g) rv)))) e)) match_value)) (t (list :left "expected either value")))) (cadr match_target))) stripped))))))))

(defvar hydra_extract_core_decode_list (lambda (elem_decoder) (lambda (g) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_strip_with_decoding_error g) term)) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :list) (funcall (lambda (els) (funcall (hydra_lib_eithers_map_list (elem_decoder g)) els)) match_value)) (t (list :left "expected list")))) (cadr match_target))) stripped)))))))

(defvar hydra_extract_core_decode_map (lambda (key_decoder) (lambda (val_decoder) (lambda (g) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_strip_with_decoding_error g) term)) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :map) (funcall (lambda (m) (funcall (hydra_lib_eithers_map hydra_lib_maps_from_list) (funcall (hydra_lib_eithers_map_list (lambda (kv) (funcall (hydra_lib_eithers_bind (funcall (key_decoder g) (hydra_lib_pairs_first kv))) (lambda (k) (funcall (hydra_lib_eithers_map (lambda (v) (list k v))) (funcall (val_decoder g) (hydra_lib_pairs_second kv))))))) (hydra_lib_maps_to_list m)))) match_value)) (t (list :left "expected map")))) (cadr match_target))) stripped))))))))

(defvar hydra_extract_core_decode_maybe (lambda (elem_decoder) (lambda (g) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_strip_with_decoding_error g) term)) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (opt) (funcall (hydra_lib_eithers_map_maybe (elem_decoder g)) opt)) match_value)) (t (list :left "expected optional value")))) (cadr match_target))) stripped)))))))

(defvar hydra_extract_core_decode_pair (lambda (first_decoder) (lambda (second_decoder) (lambda (g) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_strip_with_decoding_error g) term)) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :pair) (funcall (lambda (p) (funcall (hydra_lib_eithers_bind (funcall (first_decoder g) (hydra_lib_pairs_first p))) (lambda (f) (funcall (hydra_lib_eithers_map (lambda (s) (list f s))) (funcall (second_decoder g) (hydra_lib_pairs_second p)))))) match_value)) (t (list :left "expected pair")))) (cadr match_target))) stripped))))))))

(defvar hydra_extract_core_decode_set (lambda (elem_decoder) (lambda (g) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_strip_with_decoding_error g) term)) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :set) (funcall (lambda (s) (funcall (hydra_lib_eithers_map hydra_lib_sets_from_list) (funcall (hydra_lib_eithers_map_list (elem_decoder g)) (hydra_lib_sets_to_list s)))) match_value)) (t (list :left "expected set")))) (cadr match_target))) stripped)))))))

(defvar hydra_extract_core_decode_unit (lambda (g) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_strip_with_decoding_error g) term)) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :unit) (funcall (lambda (_) (list :right nil)) match_value)) (t (list :left "expected a unit value")))) (cadr match_target))) stripped))))))

(defvar hydra_extract_core_decode_wrapped (lambda (body_decoder) (lambda (g) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_strip_with_decoding_error g) term)) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :wrap) (funcall (lambda (wt) (funcall (body_decoder g) (funcall (lambda (v) (hydra_core_wrapped_term-body v)) wt))) match_value)) (t (list :left "expected wrapped value")))) (cadr match_target))) stripped)))))))

(defvar hydra_extract_core_either_term (lambda (left_fun) (lambda (right_fun) (lambda (graph) (lambda (term0) (funcall (hydra_lib_eithers_bind (funcall (hydra_lexical_strip_and_dereference_term graph) term0)) (lambda (term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :either) (funcall (lambda (et) (funcall (funcall (hydra_lib_eithers_either (lambda (l) (funcall (hydra_lib_eithers_map (lambda (x) (list :left x))) (left_fun l)))) (lambda (r) (funcall (hydra_lib_eithers_map (lambda (x) (list :right x))) (right_fun r)))) et)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "either value" (hydra_show_core_term term)))))))) (cadr match_target))) term))))))))

(defvar hydra_extract_core_either_type (lambda (typ) (let ((stripped (hydra_strip_deannotate_type typ))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :either) (funcall (lambda (et) (list :right et)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "either type" (hydra_show_core_type typ)))))))) (cadr match_target))) stripped))))

(defvar hydra_extract_core_field (lambda (fname) (lambda (mapping) (lambda (graph) (lambda (fields) (let ((matching_fields (funcall (hydra_lib_lists_filter (lambda (f) (funcall (hydra_lib_equality_equal (funcall (lambda (v) v) (funcall (lambda (v) (hydra_core_field-name v)) f))) (funcall (lambda (v) v) fname)))) fields))) (if (hydra_lib_lists_null matching_fields) (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error (funcall (hydra_lib_strings_cat2 "field ") (funcall (lambda (v) v) fname)) "no matching field")))) (if (funcall (hydra_lib_equality_equal (hydra_lib_lists_length matching_fields)) 1) (funcall (hydra_lib_eithers_bind (funcall (hydra_lexical_strip_and_dereference_term graph) (funcall (lambda (v) (hydra_core_field-term v)) (hydra_lib_lists_head matching_fields)))) (lambda (stripped) (mapping stripped))) (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "single field" (funcall (hydra_lib_strings_cat2 "multiple fields named ") (funcall (lambda (v) v) fname))))))))))))))

(defvar hydra_extract_core_float32_value (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :float32) (funcall (lambda (f) (list :right f)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "float32" (hydra_show_core_float v)))))))) (cadr match_target))) v)))

(defvar hydra_extract_core_float32 (lambda (graph) (lambda (t_) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_literal graph) t_)) (lambda (l) (funcall (hydra_lib_eithers_bind (hydra_extract_core_float_literal l)) (lambda (f) (hydra_extract_core_float32_value f))))))))

(defvar hydra_extract_core_float64_value (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :float64) (funcall (lambda (f) (list :right f)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "float64" (hydra_show_core_float v)))))))) (cadr match_target))) v)))

(defvar hydra_extract_core_float64 (lambda (graph) (lambda (t_) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_literal graph) t_)) (lambda (l) (funcall (hydra_lib_eithers_bind (hydra_extract_core_float_literal l)) (lambda (f) (hydra_extract_core_float64_value f))))))))

(defvar hydra_extract_core_float_value (lambda (graph) (lambda (t_) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_literal graph) t_)) (lambda (l) (hydra_extract_core_float_literal l))))))

(defvar hydra_extract_core_function_type (lambda (typ) (let ((stripped (hydra_strip_deannotate_type typ))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :function) (funcall (lambda (ft) (list :right ft)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "function type" (hydra_show_core_type typ)))))))) (cadr match_target))) stripped))))

(defvar hydra_extract_core_injection (lambda (expected) (lambda (graph) (lambda (term0) (funcall (hydra_lib_eithers_bind (funcall (hydra_lexical_strip_and_dereference_term graph) term0)) (lambda (term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :inject) (funcall (lambda (injection) (if (funcall (hydra_lib_equality_equal (funcall (lambda (v) v) (funcall (lambda (v) (hydra_core_injection-type_name v)) injection))) (funcall (lambda (v) v) expected)) (list :right (funcall (lambda (v) (hydra_core_injection-field v)) injection)) (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error (funcall (hydra_lib_strings_cat2 "injection of type ") (funcall (lambda (v) v) expected)) (funcall (lambda (v) v) (funcall (lambda (v) (hydra_core_injection-type_name v)) injection)))))))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "injection" (hydra_show_core_term term)))))))) (cadr match_target))) term)))))))

(defvar hydra_extract_core_int16_value (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :int16) (funcall (lambda (i) (list :right i)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "int16" (hydra_show_core_integer v)))))))) (cadr match_target))) v)))

(defvar hydra_extract_core_int16 (lambda (graph) (lambda (t_) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_literal graph) t_)) (lambda (l) (funcall (hydra_lib_eithers_bind (hydra_extract_core_integer_literal l)) (lambda (i) (hydra_extract_core_int16_value i))))))))

(defvar hydra_extract_core_int32_value (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :int32) (funcall (lambda (i) (list :right i)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "int32" (hydra_show_core_integer v)))))))) (cadr match_target))) v)))

(defvar hydra_extract_core_int32 (lambda (graph) (lambda (t_) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_literal graph) t_)) (lambda (l) (funcall (hydra_lib_eithers_bind (hydra_extract_core_integer_literal l)) (lambda (i) (hydra_extract_core_int32_value i))))))))

(defvar hydra_extract_core_int64_value (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :int64) (funcall (lambda (i) (list :right i)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "int64" (hydra_show_core_integer v)))))))) (cadr match_target))) v)))

(defvar hydra_extract_core_int64 (lambda (graph) (lambda (t_) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_literal graph) t_)) (lambda (l) (funcall (hydra_lib_eithers_bind (hydra_extract_core_integer_literal l)) (lambda (i) (hydra_extract_core_int64_value i))))))))

(defvar hydra_extract_core_int8_value (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :int8) (funcall (lambda (i) (list :right i)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "int8" (hydra_show_core_integer v)))))))) (cadr match_target))) v)))

(defvar hydra_extract_core_int8 (lambda (graph) (lambda (t_) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_literal graph) t_)) (lambda (l) (funcall (hydra_lib_eithers_bind (hydra_extract_core_integer_literal l)) (lambda (i) (hydra_extract_core_int8_value i))))))))

(defvar hydra_extract_core_integer_value (lambda (graph) (lambda (t_) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_literal graph) t_)) (lambda (l) (hydra_extract_core_integer_literal l))))))

(defvar hydra_extract_core_lambda (lambda (graph) (lambda (term0) (funcall (hydra_lib_eithers_bind (funcall (hydra_lexical_strip_and_dereference_term graph) term0)) (lambda (term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :lambda) (funcall (lambda (l) (list :right l)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "lambda" (hydra_show_core_term term)))))))) (cadr match_target))) term))))))

(defvar hydra_extract_core_lambda_body (lambda (graph) (lambda (term) (funcall (hydra_lib_eithers_map (lambda (v) (hydra_core_lambda-body v))) (funcall (hydra_extract_core_lambda graph) term)))))

(defvar hydra_extract_core_let (lambda (graph) (lambda (term0) (funcall (hydra_lib_eithers_bind (funcall (hydra_lexical_strip_and_dereference_term graph) term0)) (lambda (term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :let) (funcall (lambda (lt) (list :right lt)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "let term" (hydra_show_core_term term)))))))) (cadr match_target))) term))))))

(defvar hydra_extract_core_let_binding (lambda (n) (lambda (graph) (lambda (term) (let ((name n)) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_let graph) term)) (lambda (let_expr) (let ((matching_bindings (funcall (hydra_lib_lists_filter (lambda (b) (funcall (hydra_lib_equality_equal (funcall (lambda (v) v) (funcall (lambda (v) (hydra_core_binding-name v)) b))) (funcall (lambda (v) v) name)))) (funcall (lambda (v) (hydra_core_let-bindings v)) let_expr)))) (if (hydra_lib_lists_null matching_bindings) (list :left (list :extraction (list :no_such_binding (make-hydra_errors_no_such_binding_error name)))) (if (funcall (hydra_lib_equality_equal (hydra_lib_lists_length matching_bindings)) 1) (list :right (funcall (lambda (v) (hydra_core_binding-term v)) (hydra_lib_lists_head matching_bindings))) (list :left (list :extraction (list :multiple_bindings (make-hydra_errors_multiple_bindings_error name))))))))))))))

(defvar hydra_extract_core_list (lambda (graph) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (hydra_lexical_strip_and_dereference_term graph) term)) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :list) (funcall (lambda (l) (list :right l)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "list" (hydra_show_core_term stripped)))))))) (cadr match_target))) stripped))))))

(defvar hydra_extract_core_list_head (lambda (graph) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_list graph) term)) (lambda (l) (if (hydra_lib_lists_null l) (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "non-empty list" "empty list")))) (list :right (hydra_lib_lists_head l))))))))

(defvar hydra_extract_core_list_of (lambda (f) (lambda (graph) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_list graph) term)) (lambda (els) (funcall (hydra_lib_eithers_map_list f) els)))))))

(defvar hydra_extract_core_list_type (lambda (typ) (let ((stripped (hydra_strip_deannotate_type typ))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :list) (funcall (lambda (t_) (list :right t_)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "list type" (hydra_show_core_type typ)))))))) (cadr match_target))) stripped))))

(defvar hydra_extract_core_map (lambda (fk) (lambda (fv) (lambda (graph) (lambda (term0) (let ((pair (lambda (kv_pair) (let ((kterm (hydra_lib_pairs_first kv_pair))) (let ((vterm (hydra_lib_pairs_second kv_pair))) (funcall (hydra_lib_eithers_bind (fk kterm)) (lambda (kval) (funcall (hydra_lib_eithers_bind (fv vterm)) (lambda (vval) (list :right (list kval vval))))))))))) (funcall (hydra_lib_eithers_bind (funcall (hydra_lexical_strip_and_dereference_term graph) term0)) (lambda (term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :map) (funcall (lambda (m) (funcall (hydra_lib_eithers_map hydra_lib_maps_from_list) (funcall (hydra_lib_eithers_map_list pair) (hydra_lib_maps_to_list m)))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "map" (hydra_show_core_term term)))))))) (cadr match_target))) term)))))))))

(defvar hydra_extract_core_map_type (lambda (typ) (let ((stripped (hydra_strip_deannotate_type typ))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :map) (funcall (lambda (mt) (list :right mt)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "map type" (hydra_show_core_type typ)))))))) (cadr match_target))) stripped))))

(defvar hydra_extract_core_maybe_term (lambda (f) (lambda (graph) (lambda (term0) (funcall (hydra_lib_eithers_bind (funcall (hydra_lexical_strip_and_dereference_term graph) term0)) (lambda (term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (mt) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :right (list :nothing)))) (lambda (t_) (funcall (hydra_lib_eithers_map hydra_lib_maybes_pure) (f t_)))) mt)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "maybe value" (hydra_show_core_term term)))))))) (cadr match_target))) term)))))))

(defvar hydra_extract_core_maybe_type (lambda (typ) (let ((stripped (hydra_strip_deannotate_type typ))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (t_) (list :right t_)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "maybe type" (hydra_show_core_type typ)))))))) (cadr match_target))) stripped))))

(defvar hydra_extract_core_n_args (lambda (name) (lambda (n) (lambda (args) (if (funcall (hydra_lib_equality_equal (hydra_lib_lists_length args)) n) (list :right nil) (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error (hydra_lib_strings_cat (list (hydra_lib_literals_show_int32 n) " arguments to primitive " (hydra_lib_literals_show_string (funcall (lambda (v) v) name)))) (hydra_lib_literals_show_int32 (hydra_lib_lists_length args)))))))))))

(defvar hydra_extract_core_pair (lambda (kf) (lambda (vf) (lambda (graph) (lambda (term0) (funcall (hydra_lib_eithers_bind (funcall (hydra_lexical_strip_and_dereference_term graph) term0)) (lambda (term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :pair) (funcall (lambda (p) (funcall (hydra_lib_eithers_bind (kf (hydra_lib_pairs_first p))) (lambda (k_val) (funcall (hydra_lib_eithers_bind (vf (hydra_lib_pairs_second p))) (lambda (v_val) (list :right (list k_val v_val))))))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "pair" (hydra_show_core_term term)))))))) (cadr match_target))) term))))))))

(defvar hydra_extract_core_term_record (lambda (graph) (lambda (term0) (funcall (hydra_lib_eithers_bind (funcall (hydra_lexical_strip_and_dereference_term graph) term0)) (lambda (term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (record) (list :right record)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "record" (hydra_show_core_term term)))))))) (cadr match_target))) term))))))

(defvar hydra_extract_core_record (lambda (expected) (lambda (graph) (lambda (term0) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_term_record graph) term0)) (lambda (record) (if (funcall (hydra_lib_equality_equal (funcall (lambda (v) (hydra_core_record-type_name v)) record)) expected) (list :right (funcall (lambda (v) (hydra_core_record-fields v)) record)) (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error (funcall (hydra_lib_strings_cat2 "record of type ") (funcall (lambda (v) v) expected)) (funcall (lambda (v) v) (funcall (lambda (v) (hydra_core_record-type_name v)) record)))))))))))))

(defvar hydra_extract_core_record_type (lambda (ename) (lambda (typ) (let ((stripped (hydra_strip_deannotate_type typ))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (fields) (list :right fields)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "record type" (hydra_show_core_type typ)))))))) (cadr match_target))) stripped)))))

(defvar hydra_extract_core_require_field (lambda (field_name) (lambda (decoder) (lambda (field_map) (lambda (g) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (hydra_lib_strings_cat (list "missing field " field_name " in record"))))) (lambda (field_term) (funcall (decoder g) field_term))) (funcall (hydra_lib_maps_lookup field_name) field_map)))))))

(defvar hydra_extract_core_set (lambda (graph) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (hydra_lexical_strip_and_dereference_term graph) term)) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :set) (funcall (lambda (s) (list :right s)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "set" (hydra_show_core_term stripped)))))))) (cadr match_target))) stripped))))))

(defvar hydra_extract_core_set_of (lambda (f) (lambda (graph) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_set graph) term)) (lambda (els) (funcall (hydra_lib_eithers_map_set f) els)))))))

(defvar hydra_extract_core_set_type (lambda (typ) (let ((stripped (hydra_strip_deannotate_type typ))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :set) (funcall (lambda (t_) (list :right t_)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "set type" (hydra_show_core_type typ)))))))) (cadr match_target))) stripped))))

(defvar hydra_extract_core_string_literal (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :string) (funcall (lambda (s) (list :right s)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "string" (hydra_show_core_literal v)))))))) (cadr match_target))) v)))

(defvar hydra_extract_core_string (lambda (graph) (lambda (t_) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_literal graph) t_)) (lambda (l) (hydra_extract_core_string_literal l))))))

(defvar hydra_extract_core_to_field_map (lambda (record) (hydra_lib_maps_from_list (funcall (hydra_lib_lists_map (lambda (f) (list (funcall (lambda (v) (hydra_core_field-name v)) f) (funcall (lambda (v) (hydra_core_field-term v)) f)))) (funcall (lambda (v) (hydra_core_record-fields v)) record)))))

(defvar hydra_extract_core_uint16_value (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :uint16) (funcall (lambda (i) (list :right i)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "uint16" (hydra_show_core_integer v)))))))) (cadr match_target))) v)))

(defvar hydra_extract_core_uint16 (lambda (graph) (lambda (t_) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_literal graph) t_)) (lambda (l) (funcall (hydra_lib_eithers_bind (hydra_extract_core_integer_literal l)) (lambda (i) (hydra_extract_core_uint16_value i))))))))

(defvar hydra_extract_core_uint32_value (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :uint32) (funcall (lambda (i) (list :right i)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "uint32" (hydra_show_core_integer v)))))))) (cadr match_target))) v)))

(defvar hydra_extract_core_uint32 (lambda (graph) (lambda (t_) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_literal graph) t_)) (lambda (l) (funcall (hydra_lib_eithers_bind (hydra_extract_core_integer_literal l)) (lambda (i) (hydra_extract_core_uint32_value i))))))))

(defvar hydra_extract_core_uint64_value (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :uint64) (funcall (lambda (i) (list :right i)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "uint64" (hydra_show_core_integer v)))))))) (cadr match_target))) v)))

(defvar hydra_extract_core_uint64 (lambda (graph) (lambda (t_) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_literal graph) t_)) (lambda (l) (funcall (hydra_lib_eithers_bind (hydra_extract_core_integer_literal l)) (lambda (i) (hydra_extract_core_uint64_value i))))))))

(defvar hydra_extract_core_uint8_value (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :uint8) (funcall (lambda (i) (list :right i)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "uint8" (hydra_show_core_integer v)))))))) (cadr match_target))) v)))

(defvar hydra_extract_core_uint8 (lambda (graph) (lambda (t_) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_literal graph) t_)) (lambda (l) (funcall (hydra_lib_eithers_bind (hydra_extract_core_integer_literal l)) (lambda (i) (hydra_extract_core_uint8_value i))))))))

(defvar hydra_extract_core_union_type (lambda (ename) (lambda (typ) (let ((stripped (hydra_strip_deannotate_type typ))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :union) (funcall (lambda (fields) (list :right fields)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "union type" (hydra_show_core_type typ)))))))) (cadr match_target))) stripped)))))

(defvar hydra_extract_core_unit (lambda (term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :unit) (funcall (lambda (_) (list :right nil)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "unit" (hydra_show_core_term term)))))))) (cadr match_target))) term)))

(defvar hydra_extract_core_unit_variant (lambda (tname) (lambda (graph) (lambda (term) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_extract_core_injection tname) graph) term)) (lambda (field) (funcall (hydra_lib_eithers_bind (hydra_extract_core_unit (funcall (lambda (v) (hydra_core_field-term v)) field))) (lambda (ignored) (list :right (funcall (lambda (v) (hydra_core_field-name v)) field))))))))))

(defvar hydra_extract_core_wrap (lambda (expected) (lambda (graph) (lambda (term0) (funcall (hydra_lib_eithers_bind (funcall (hydra_lexical_strip_and_dereference_term graph) term0)) (lambda (term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :wrap) (funcall (lambda (wrapped_term) (if (funcall (hydra_lib_equality_equal (funcall (lambda (v) v) (funcall (lambda (v) (hydra_core_wrapped_term-type_name v)) wrapped_term))) (funcall (lambda (v) v) expected)) (list :right (funcall (lambda (v) (hydra_core_wrapped_term-body v)) wrapped_term)) (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error (funcall (hydra_lib_strings_cat2 "wrapper of type ") (funcall (lambda (v) v) expected)) (funcall (lambda (v) v) (funcall (lambda (v) (hydra_core_wrapped_term-type_name v)) wrapped_term)))))))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "wrap(") (funcall (lambda (v) v) expected))) ")") (hydra_show_core_term term)))))))) (cadr match_target))) term)))))))

(defvar hydra_extract_core_wrapped_type (lambda (ename) (lambda (typ) (let ((stripped (hydra_strip_deannotate_type typ))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :wrap) (funcall (lambda (inner_type) (list :right inner_type)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "wrapped type" (hydra_show_core_type typ)))))))) (cadr match_target))) stripped)))))

(provide 'hydra.extract.core)
