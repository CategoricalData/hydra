(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.errors)

(require 'hydra.extract.core)

(require 'hydra.lib.eithers)

(require 'hydra.lib.lists)

(require 'hydra.lib.maybes)

(require 'hydra.show.core)

(defvar hydra_eval_lib_maybes_apply (lambda (cx) (lambda (g) (lambda (fun_opt_term) (lambda (arg_opt_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (mf) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (mx) (list :right (list :maybe (funcall (hydra_lib_maybes_bind mf) (lambda (f) (funcall (hydra_lib_maybes_map (lambda (x) (list :application (make-hydra_core_application f x)))) mx)))))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "optional value" (hydra_show_core_term arg_opt_term)))))))) (cadr match_target))) arg_opt_term)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "optional function" (hydra_show_core_term fun_opt_term)))))))) (cadr match_target))) fun_opt_term))))))

(defvar hydra_eval_lib_maybes_bind (lambda (cx) (lambda (g) (lambda (opt_term) (lambda (fun_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (m) (list :right (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :maybe (list :nothing)))) (lambda (val) (list :application (make-hydra_core_application fun_term val)))) m))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "optional value" (hydra_show_core_term opt_term)))))))) (cadr match_target))) opt_term))))))

(defvar hydra_eval_lib_maybes_cases (lambda (cx) (lambda (g) (lambda (opt_term) (lambda (default_term) (lambda (fun_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (m) (list :right (funcall (funcall (hydra_lib_maybes_maybe (lambda () default_term)) (lambda (val) (list :application (make-hydra_core_application fun_term val)))) m))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "optional value" (hydra_show_core_term opt_term)))))))) (cadr match_target))) opt_term)))))))

(defvar hydra_eval_lib_maybes_cat (lambda (cx) (lambda (g) (lambda (list_term) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_list g) list_term)) (lambda (elements) (list :right (funcall (funcall (hydra_lib_lists_foldl (lambda (acc) (lambda (el) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (m) (funcall (funcall (hydra_lib_maybes_maybe (lambda () acc)) (lambda (v) (funcall (hydra_lib_lists_concat2 acc) (hydra_lib_lists_pure v)))) m)) match_value)) (t acc))) (cadr match_target))) el)))) (list)) elements))))))))

(defvar hydra_eval_lib_maybes_compose (lambda (cx) (lambda (g) (lambda (fun_f) (lambda (fun_g) (lambda (x_term) (list :right (list :application (make-hydra_core_application (list :application (make-hydra_core_application (list :variable "hydra.lib.maybes.bind") (list :application (make-hydra_core_application fun_f x_term)))) fun_g)))))))))

(defvar hydra_eval_lib_maybes_from_just (lambda (cx) (lambda (g) (lambda (opt_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (m) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "Just value" (hydra_show_core_term opt_term))))))) (lambda (val) (list :right val))) m)) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "optional value" (hydra_show_core_term opt_term)))))))) (cadr match_target))) opt_term)))))

(defvar hydra_eval_lib_maybes_from_maybe (lambda (cx) (lambda (g) (lambda (default_term) (lambda (opt_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (m) (list :right (funcall (funcall (hydra_lib_maybes_maybe (lambda () default_term)) (lambda (val) val)) m))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "optional value" (hydra_show_core_term opt_term)))))))) (cadr match_target))) opt_term))))))

(defvar hydra_eval_lib_maybes_is_just (lambda (cx) (lambda (g) (lambda (opt_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (m) (list :right (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :literal (list :boolean nil)))) (lambda (_) (list :literal (list :boolean t)))) m))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "optional value" (hydra_show_core_term opt_term)))))))) (cadr match_target))) opt_term)))))

(defvar hydra_eval_lib_maybes_is_nothing (lambda (cx) (lambda (g) (lambda (opt_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (m) (list :right (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list :literal (list :boolean t)))) (lambda (_) (list :literal (list :boolean nil)))) m))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "optional value" (hydra_show_core_term opt_term)))))))) (cadr match_target))) opt_term)))))

(defvar hydra_eval_lib_maybes_map (lambda (cx) (lambda (g) (lambda (fun_term) (lambda (opt_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (m) (list :right (list :maybe (funcall (hydra_lib_maybes_map (lambda (val) (list :application (make-hydra_core_application fun_term val)))) m)))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "optional value" (hydra_show_core_term opt_term)))))))) (cadr match_target))) opt_term))))))

(defvar hydra_eval_lib_maybes_map_maybe (lambda (cx) (lambda (g) (lambda (fun_term) (lambda (list_term) (funcall (hydra_lib_eithers_bind (funcall (hydra_extract_core_list g) list_term)) (lambda (elements) (list :right (list :application (make-hydra_core_application (list :variable "hydra.lib.maybes.cat") (list :list (funcall (hydra_lib_lists_map (lambda (el) (list :application (make-hydra_core_application fun_term el)))) elements))))))))))))

(defvar hydra_eval_lib_maybes_maybe (lambda (cx) (lambda (g) (lambda (default_term) (lambda (fun_term) (lambda (opt_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (m) (list :right (funcall (funcall (hydra_lib_maybes_maybe (lambda () default_term)) (lambda (val) (list :application (make-hydra_core_application fun_term val)))) m))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "optional value" (hydra_show_core_term opt_term)))))))) (cadr match_target))) opt_term)))))))

(defvar hydra_eval_lib_maybes_pure (lambda (cx) (lambda (g) (lambda (x) (list :right (list :maybe (list :just x)))))))

(defvar hydra_eval_lib_maybes_to_list (lambda (cx) (lambda (g) (lambda (opt_term) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :maybe) (funcall (lambda (m) (list :right (list :list (funcall (funcall (hydra_lib_maybes_maybe (lambda () (list))) (lambda (val) (hydra_lib_lists_pure val))) m)))) match_value)) (t (list :left (list :extraction (list :unexpected_shape (make-hydra_errors_unexpected_shape_error "optional value" (hydra_show_core_term opt_term)))))))) (cadr match_target))) opt_term)))))

(provide 'hydra.eval.lib.maybes)
