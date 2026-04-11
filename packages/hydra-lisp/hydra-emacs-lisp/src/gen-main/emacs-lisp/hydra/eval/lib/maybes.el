(require 'cl-lib)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.error)

(require 'hydra.extract.core)

(require 'hydra.lib.eithers)

(require 'hydra.lib.lists)

(require 'hydra.lib.maybes)

(require 'hydra.lib.strings)

(require 'hydra.show.core)

(defvar hydra_eval_lib_maybes_apply (lambda (cx) (lambda (g) (lambda (fun_opt_term) (lambda (arg_opt_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (mf) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (mx) (list :right (list :maybe (funcall (hydra_lib_maybes_bind mf) (lambda (f) (funcall (hydra_lib_maybes_map (lambda (x) (list :application (make-hydra_core_application f x)))) mx)))))) match_value)) (t (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "expected ") "optional value")) " but found ")) (hydra_show_core_term arg_opt_term))) cx))))) (cadr match_target))) arg_opt_term)) match_value)) (t (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "expected ") "optional function")) " but found ")) (hydra_show_core_term fun_opt_term))) cx))))) (cadr match_target))) fun_opt_term))))))

(defvar hydra_eval_lib_maybes_bind (lambda (cx) (lambda (g) (lambda (opt_term) (lambda (fun_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (m) (list :right (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :maybe (list :nothing)))) (lambda (val) (list :application (make-hydra_core_application fun_term val)))) m))) match_value)) (t (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "expected ") "optional value")) " but found ")) (hydra_show_core_term opt_term))) cx))))) (cadr match_target))) opt_term))))))

(defvar hydra_eval_lib_maybes_cases (lambda (cx) (lambda (g) (lambda (opt_term) (lambda (default_term) (lambda (fun_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (m) (list :right (funcall (funcall (hydra_lib_maybes_maybe (lambda () default_term)) (lambda (val) (list :application (make-hydra_core_application fun_term val)))) m))) match_value)) (t (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "expected ") "optional value")) " but found ")) (hydra_show_core_term opt_term))) cx))))) (cadr match_target))) opt_term)))))))

(defvar hydra_eval_lib_maybes_compose (lambda (cx) (lambda (g) (lambda (fun_f) (lambda (fun_g) (lambda (x_term) (list :right (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :function (list :primitive "hydra.lib.maybes.bind")) (list :application (make-hydra_core_application fun_f x_term)))) fun_g)))))))))

(defvar hydra_eval_lib_maybes_map (lambda (cx) (lambda (g) (lambda (fun_term) (lambda (opt_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (m) (list :right (list :maybe (funcall (hydra_lib_maybes_map (lambda (val) (list :application (make-hydra_core_application fun_term val)))) m)))) match_value)) (t (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "expected ") "optional value")) " but found ")) (hydra_show_core_term opt_term))) cx))))) (cadr match_target))) opt_term))))))

(defvar hydra_eval_lib_maybes_map_maybe (lambda (cx) (lambda (g) (lambda (fun_term) (lambda (list_term) (funcall (hydra_lib_eithers_bind (funcall (funcall (hydra_extract_core_list cx) g) list_term)) (lambda (elements) (list :right (list :application (make-hydra_core_application (list :function (list :primitive "hydra.lib.maybes.cat")) (list :list (funcall (hydra_lib_lists_map (lambda (el) (list :application (make-hydra_core_application fun_term el)))) elements))))))))))))

(defvar hydra_eval_lib_maybes_maybe (lambda (cx) (lambda (g) (lambda (default_term) (lambda (fun_term) (lambda (opt_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (m) (list :right (funcall (funcall (hydra_lib_maybes_maybe (lambda () default_term)) (lambda (val) (list :application (make-hydra_core_application fun_term val)))) m))) match_value)) (t (list :left (make-hydra_context_in_context (list :other (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 "expected ") "optional value")) " but found ")) (hydra_show_core_term opt_term))) cx))))) (cadr match_target))) opt_term)))))))

(provide 'hydra.eval.lib.maybes)
