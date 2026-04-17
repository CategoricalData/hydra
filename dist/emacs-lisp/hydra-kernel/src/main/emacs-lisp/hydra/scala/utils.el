(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.formatting)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.math)

(require 'hydra.lib.maybes)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.names)

(require 'hydra.packaging)

(require 'hydra.scala.language)

(require 'hydra.scala.syntax)

(require 'hydra.strip)

(defvar hydra_scala_utils_name_of_type (lambda (cx) (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :variable) (funcall (lambda (name) (list :just name)) match_value)) ((equal (car match_target) :forall) (funcall (lambda (ft) (funcall (hydra_scala_utils_name_of_type cx) (funcall (lambda (v) (hydra_core_forall_type-body v)) ft))) match_value)) (t (list :nothing)))) (cadr match_target))) (hydra_strip_deannotate_type t_)))))

(defvar hydra_scala_utils_scala_reserved_words hydra_scala_language_scala_reserved_words)

(defvar hydra_scala_utils_scala_escape_name (lambda (s) (let* ((sanitized (hydra_lib_strings_from_list (funcall (hydra_lib_lists_map (lambda (c) (if (funcall (hydra_lib_equality_equal c) 39) 95 c))) (hydra_lib_strings_to_list s)))) (sanitized2 (if (funcall (hydra_lib_equality_equal sanitized) "_") "_x" sanitized)) (sanitized3 (if (funcall (hydra_lib_equality_equal sanitized2) "toString") "toString_" sanitized2)) (needs_backticks (funcall (hydra_lib_logic_or (funcall (hydra_lib_sets_member sanitized3) hydra_scala_utils_scala_reserved_words)) (funcall (hydra_lib_logic_and (funcall (hydra_lib_equality_gt (hydra_lib_strings_length sanitized3)) 0)) (funcall (hydra_lib_equality_equal (funcall (hydra_lib_maybes_from_maybe (lambda () 0)) (funcall (hydra_lib_strings_maybe_char_at (funcall (hydra_lib_math_sub (hydra_lib_strings_length sanitized3)) 1)) sanitized3))) 95))))) (if needs_backticks (hydra_lib_strings_cat (list "`" sanitized3 "`")) sanitized3))))

(defvar hydra_scala_utils_scala_type_name (lambda (qualify) (lambda (name) (if (funcall (hydra_lib_logic_or qualify) (funcall (hydra_lib_sets_member (hydra_names_local_name_of name)) hydra_scala_utils_scala_reserved_words)) (funcall (lambda (v) v) name) (hydra_names_local_name_of name)))))

(defvar hydra_scala_utils_qualify_union_field_name (lambda (dlft) (lambda (sname) (lambda (fname) (funcall (hydra_lib_strings_cat2 (funcall (funcall (hydra_lib_maybes_maybe (lambda () dlft)) (lambda (n) (funcall (hydra_lib_strings_cat2 (funcall (hydra_scala_utils_scala_type_name t) n)) "."))) sname)) (hydra_scala_utils_scala_escape_name (funcall (lambda (v) v) fname)))))))

(defvar hydra_scala_utils_sapply (lambda (fun) (lambda (args) (list :apply (make-hydra_scala_syntax_data_apply fun args)))))

(defvar hydra_scala_utils_sname (lambda (s) (list :ref (list :name (make-hydra_scala_syntax_data_name s)))))

(defvar hydra_scala_utils_type_to_string (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :ref) (funcall (lambda (tr) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :name) (funcall (lambda (tn) (funcall (lambda (v) (hydra_scala_syntax_type_name-value v)) tn)) match_value)) (t "Any"))) (cadr match_target))) tr)) match_value)) ((equal (car match_target) :var) (funcall (lambda (tv) (funcall (lambda (v) (hydra_scala_syntax_type_name-value v)) (funcall (lambda (v) (hydra_scala_syntax_type_var-name v)) tv))) match_value)) ((equal (car match_target) :function_type) (funcall (lambda (ft) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :function) (funcall (lambda (fn_) (let* ((params (funcall (hydra_lib_lists_map hydra_scala_utils_type_to_string) (funcall (lambda (v) (hydra_scala_syntax_type_function-params v)) fn_))) (res (hydra_scala_utils_type_to_string (funcall (lambda (v) (hydra_scala_syntax_type_function-res v)) fn_)))) (hydra_lib_strings_cat (list "(" (funcall (hydra_lib_strings_intercalate ", ") params) ") => " res)))) match_value)) (t "Any"))) (cadr match_target))) ft)) match_value)) ((equal (car match_target) :apply) (funcall (lambda (ta) (let* ((arg_strs (funcall (hydra_lib_lists_map hydra_scala_utils_type_to_string) (funcall (lambda (v) (hydra_scala_syntax_type_apply-args v)) ta))) (base (hydra_scala_utils_type_to_string (funcall (lambda (v) (hydra_scala_syntax_type_apply-tpe v)) ta)))) (hydra_lib_strings_cat (list base "[" (funcall (hydra_lib_strings_intercalate ", ") arg_strs) "]")))) match_value)) (t "Any"))) (cadr match_target))) t_)))

(defvar hydra_scala_utils_sapply_types (lambda (fun) (lambda (type_args) (let* ((type_to_str (lambda (t_) (hydra_scala_utils_type_to_string t_))) (type_strings (funcall (hydra_lib_lists_map type_to_str) type_args)) (type_arg_str (hydra_lib_strings_cat (list "[" (funcall (hydra_lib_strings_intercalate ", ") type_strings) "]")))) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :ref) (funcall (lambda (ref) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :name) (funcall (lambda (dn) (let* ((name_str (funcall (lambda (v) (hydra_scala_syntax_data_name-value v)) dn)) (raw_name (funcall (lambda (v) v) name_str))) (hydra_scala_utils_sname (funcall (hydra_lib_strings_cat2 raw_name) type_arg_str)))) match_value)) (t fun))) (cadr match_target))) ref)) match_value)) (t fun))) (cadr match_target))) fun)))))

(defvar hydra_scala_utils_sassign (lambda (lhs) (lambda (rhs) (list :assign (make-hydra_scala_syntax_data_assign lhs rhs)))))

(defvar hydra_scala_utils_slambda (lambda (v) (lambda (body) (lambda (sdom) (list :function_data (list :function (make-hydra_scala_syntax_data_function (list (make-hydra_scala_syntax_data_param (list) (list :value v) sdom (list :nothing))) body)))))))

(defvar hydra_scala_utils_sprim (lambda (name) (let* ((qname (hydra_names_qualify_name name)) (local (hydra_scala_utils_scala_escape_name (funcall (lambda (v) (hydra_packaging_qualified_name-local v)) qname))) (prefix (funcall (lambda (v) v) (funcall (hydra_lib_maybes_from_maybe (lambda () "")) (funcall (lambda (v) (hydra_packaging_qualified_name-namespace v)) qname))))) (hydra_scala_utils_sname (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 prefix) ".")) local)))))

(defvar hydra_scala_utils_stapply (lambda (t_) (lambda (args) (list :apply (make-hydra_scala_syntax_type_apply t_ args)))))

(defvar hydra_scala_utils_stapply1 (lambda (t1) (lambda (t2) (funcall (hydra_scala_utils_stapply t1) (list t2)))))

(defvar hydra_scala_utils_stapply2 (lambda (t1) (lambda (t2) (lambda (t3) (funcall (hydra_scala_utils_stapply t1) (list t2 t3))))))

(defvar hydra_scala_utils_stparam (lambda (name) (let ((v (hydra_formatting_capitalize (funcall (lambda (v) v) name)))) (make-hydra_scala_syntax_type_param (list) (list :value v) (list) (list) (list) (list)))))

(defvar hydra_scala_utils_stref (lambda (s) (list :ref (list :name (make-hydra_scala_syntax_type_name s)))))

(defvar hydra_scala_utils_svar (lambda (name) (let ((v (funcall (lambda (v) v) name))) (list :var (make-hydra_scala_syntax_pat_var (make-hydra_scala_syntax_data_name v))))))

(provide 'hydra.scala.utils)
