(require 'cl-lib)

(require 'hydra.coders)

(require 'hydra.core)

(defvar hydra_encode_coders_coder_direction (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :encode) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.coders.CoderDirection" (make-hydra_core_field "encode" (funcall (lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :decode) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.coders.CoderDirection" (make-hydra_core_field "decode" (funcall (lambda (_) (list :unit nil)) y))))) match_value)))) (cadr match_target))))

(defvar hydra_encode_coders_language_name (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.coders.LanguageName" (funcall (lambda (x) (list :literal (list :string x))) (funcall (lambda (v) v) x))))))

(defvar hydra_encode_coders_traversal_order (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :pre) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.coders.TraversalOrder" (make-hydra_core_field "pre" (funcall (lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :post) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.coders.TraversalOrder" (make-hydra_core_field "post" (funcall (lambda (_) (list :unit nil)) y))))) match_value)))) (cadr match_target))))

(provide 'hydra.encode.coders)
