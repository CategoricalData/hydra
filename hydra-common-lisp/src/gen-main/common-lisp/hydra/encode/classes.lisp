(defpackage :hydra.encode.classes
(:use :cl :hydra.classes :hydra.core)
(:export :hydra_encode_classes_type_class))

(in-package :hydra.encode.classes)

(cl:defvar hydra_encode_classes_type_class (cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :equality) ((cl:lambda (y) (list :union (make-hydra_core_injection "hydra.classes.TypeClass" (make-hydra_core_field "equality" ((cl:lambda (_) (list :unit cl:nil)) y))))) match_value)) ((equal (car match_target) :ordering) ((cl:lambda (y) (list :union (make-hydra_core_injection "hydra.classes.TypeClass" (make-hydra_core_field "ordering" ((cl:lambda (_) (list :unit cl:nil)) y))))) match_value)))) (cadr match_target))))
