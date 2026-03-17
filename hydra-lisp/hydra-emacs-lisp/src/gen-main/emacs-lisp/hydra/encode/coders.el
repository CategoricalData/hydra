(require 'cl-lib)

(require 'hydra.coders)

(require 'hydra.core)

(defvar hydra_encode_coders_coder_direction (lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :encode) ((lambda (y) (list :union (make-hydra_core_injection "hydra.coders.CoderDirection" (make-hydra_core_field "encode" ((lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :decode) ((lambda (y) (list :union (make-hydra_core_injection "hydra.coders.CoderDirection" (make-hydra_core_field "decode" ((lambda (_) (list :unit nil)) y))))) match_value)))) (cadr match_target))))

(defvar hydra_encode_coders_language_name (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.coders.LanguageName" ((lambda (x) (list :literal (list :string x))) ((lambda (v) v) x))))))

(defvar hydra_encode_coders_traversal_order (lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :pre) ((lambda (y) (list :union (make-hydra_core_injection "hydra.coders.TraversalOrder" (make-hydra_core_field "pre" ((lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :post) ((lambda (y) (list :union (make-hydra_core_injection "hydra.coders.TraversalOrder" (make-hydra_core_field "post" ((lambda (_) (list :unit nil)) y))))) match_value)))) (cadr match_target))))

(provide 'hydra.encode.coders)
