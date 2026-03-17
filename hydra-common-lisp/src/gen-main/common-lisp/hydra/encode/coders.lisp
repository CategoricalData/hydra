(defpackage :hydra.encode.coders
(:use :cl :hydra.coders :hydra.core)
(:export :hydra_encode_coders_coder_direction :hydra_encode_coders_language_name :hydra_encode_coders_traversal_order))

(in-package :hydra.encode.coders)

(cl:defvar hydra_encode_coders_coder_direction (cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :encode) ((cl:lambda (y) (list :union (make-hydra_core_injection "hydra.coders.CoderDirection" (make-hydra_core_field "encode" ((cl:lambda (_) (list :unit cl:nil)) y))))) match_value)) ((equal (car match_target) :decode) ((cl:lambda (y) (list :union (make-hydra_core_injection "hydra.coders.CoderDirection" (make-hydra_core_field "decode" ((cl:lambda (_) (list :unit cl:nil)) y))))) match_value)))) (cadr match_target))))

(cl:defvar hydra_encode_coders_language_name (cl:lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.coders.LanguageName" ((cl:lambda (x) (list :literal (list :string x))) ((cl:lambda (v) v) x))))))

(cl:defvar hydra_encode_coders_traversal_order (cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :pre) ((cl:lambda (y) (list :union (make-hydra_core_injection "hydra.coders.TraversalOrder" (make-hydra_core_field "pre" ((cl:lambda (_) (list :unit cl:nil)) y))))) match_value)) ((equal (car match_target) :post) ((cl:lambda (y) (list :union (make-hydra_core_injection "hydra.coders.TraversalOrder" (make-hydra_core_field "post" ((cl:lambda (_) (list :unit cl:nil)) y))))) match_value)))) (cadr match_target))))
