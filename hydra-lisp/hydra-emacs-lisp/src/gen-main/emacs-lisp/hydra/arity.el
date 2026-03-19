(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.graph)

(require 'hydra.lib.lists)

(require 'hydra.lib.math)

(defvar hydra_arity_function_arity (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :elimination) (funcall (lambda (_) 1) match_value)) ((equal (car match_target) :lambda) (funcall (lambda (arg_) (funcall (lambda (i) (funcall (hydra_lib_math_add 1) i)) (funcall (lambda (arg_) (hydra_arity_term_arity (funcall (lambda (v) (hydra_core_lambda-body v)) arg_))) arg_))) match_value)) ((equal (car match_target) :primitive) (funcall (lambda (_) 42) match_value)))) (cadr match_target))))

(defvar hydra_arity_term_arity (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :application) (funcall (lambda (arg_) (funcall (lambda (arg_) (funcall (lambda (xapp) (funcall (hydra_lib_math_sub xapp) 1)) (hydra_arity_term_arity arg_))) (funcall (lambda (v) (hydra_core_application-function v)) arg_))) match_value)) ((equal (car match_target) :function) (hydra_arity_function_arity match_value)) (t 0))) (cadr match_target))))

(defvar hydra_arity_type_arity (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :annotated) (funcall (lambda (arg_) (hydra_arity_type_arity (funcall (lambda (v) (hydra_core_annotated_type-body v)) arg_))) match_value)) ((equal (car match_target) :application) (funcall (lambda (arg_) (hydra_arity_type_arity (funcall (lambda (v) (hydra_core_application_type-function v)) arg_))) match_value)) ((equal (car match_target) :forall) (funcall (lambda (arg_) (hydra_arity_type_arity (funcall (lambda (v) (hydra_core_forall_type-body v)) arg_))) match_value)) ((equal (car match_target) :function) (funcall (lambda (f) (funcall (hydra_lib_math_add 1) (funcall (lambda (arg_) (hydra_arity_type_arity (funcall (lambda (v) (hydra_core_function_type-codomain v)) arg_))) f))) match_value)) (t 0))) (cadr match_target))))

(defvar hydra_arity_primitive_arity (lambda (arg_) (funcall (lambda (arg_) (hydra_arity_type_arity (funcall (lambda (v) (hydra_core_type_scheme-type v)) arg_))) (funcall (lambda (v) (hydra_graph_primitive-type v)) arg_))))

(defvar hydra_arity_type_scheme_arity (lambda (arg_) (hydra_arity_type_arity (funcall (lambda (v) (hydra_core_type_scheme-type v)) arg_))))

(defvar hydra_arity_uncurry_type (lambda (t_) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :annotated) (funcall (lambda (arg_) (hydra_arity_uncurry_type (funcall (lambda (v) (hydra_core_annotated_type-body v)) arg_))) match_value)) ((equal (car match_target) :application) (funcall (lambda (arg_) (hydra_arity_uncurry_type (funcall (lambda (v) (hydra_core_application_type-function v)) arg_))) match_value)) ((equal (car match_target) :forall) (funcall (lambda (arg_) (hydra_arity_uncurry_type (funcall (lambda (v) (hydra_core_forall_type-body v)) arg_))) match_value)) ((equal (car match_target) :function) (funcall (lambda (ft) (funcall (hydra_lib_lists_cons (funcall (lambda (v) (hydra_core_function_type-domain v)) ft)) (funcall (lambda (arg_) (hydra_arity_uncurry_type (funcall (lambda (v) (hydra_core_function_type-codomain v)) arg_))) ft))) match_value)) (t (list t_)))) (cadr match_target))) t_)))

(provide 'hydra.arity)
