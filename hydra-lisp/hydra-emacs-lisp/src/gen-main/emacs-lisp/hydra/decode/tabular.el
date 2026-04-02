(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.decode.core)

(require 'hydra.decode.relational)

(require 'hydra.errors)

(require 'hydra.extract.core)

(require 'hydra.lexical)

(require 'hydra.lib.eithers)

(require 'hydra.tabular)

(defvar hydra_decode_tabular_column_type (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (record) (let ((field_map (hydra_extract_core_to_field_map record))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "name") hydra_decode_relational_column_name) field_map) cx)) (lambda (field_name) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "type") hydra_decode_core_type) field_map) cx)) (lambda (field_type) (list :right (make-hydra_tabular_column_type field_name field_type)))))))) match_value)) (t (list :left "expected record")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(defvar hydra_decode_tabular_data_row (lambda (v) (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :wrap) (funcall (lambda (wrapped_term) (funcall (hydra_lib_eithers_map (lambda (b) b)) (funcall (funcall (hydra_extract_core_decode_list (hydra_extract_core_decode_maybe v)) cx) (funcall (lambda (v) (hydra_core_wrapped_term-body v)) wrapped_term)))) match_value)) (t (list :left "expected wrapped type")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw))))))

(defvar hydra_decode_tabular_header_row (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :wrap) (funcall (lambda (wrapped_term) (funcall (hydra_lib_eithers_map (lambda (b) b)) (funcall (funcall (hydra_extract_core_decode_list (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :literal) (funcall (lambda (v) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :string) (funcall (lambda (s) (list :right s)) match_value)) (t (list :left "expected string literal")))) (cadr match_target))) v)) match_value)) (t (list :left "expected literal")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw))))) cx) (funcall (lambda (v) (hydra_core_wrapped_term-body v)) wrapped_term)))) match_value)) (t (list :left "expected wrapped type")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(defvar hydra_decode_tabular_table (lambda (v) (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (record) (let ((field_map (hydra_extract_core_to_field_map record))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "header") (hydra_extract_core_decode_maybe hydra_decode_tabular_header_row)) field_map) cx)) (lambda (field_header) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "data") (hydra_extract_core_decode_list (hydra_decode_tabular_data_row v))) field_map) cx)) (lambda (field_data) (list :right (make-hydra_tabular_table field_header field_data)))))))) match_value)) (t (list :left "expected record")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw))))))

(defvar hydra_decode_tabular_table_type (lambda (cx) (lambda (raw) (funcall (funcall (hydra_lib_eithers_either (lambda (err) (list :left err))) (lambda (stripped) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :record) (funcall (lambda (record) (let ((field_map (hydra_extract_core_to_field_map record))) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "name") hydra_decode_relational_relation_name) field_map) cx)) (lambda (field_name) (funcall (hydra_lib_eithers_bind (funcall (funcall (funcall (hydra_extract_core_require_field "columns") (hydra_extract_core_decode_list hydra_decode_tabular_column_type)) field_map) cx)) (lambda (field_columns) (list :right (make-hydra_tabular_table_type field_name field_columns)))))))) match_value)) (t (list :left "expected record")))) (cadr match_target))) stripped))) (funcall (hydra_lexical_strip_and_dereference_term_either cx) raw)))))

(provide 'hydra.decode.tabular)
