(require 'cl-lib)

(require 'hydra.classes)

(require 'hydra.core)

(defvar hydra_encode_classes_type_class (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :equality) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.classes.TypeClass" (make-hydra_core_field "equality" (funcall (lambda (_) (list :unit nil)) y))))) match_value)) ((equal (car match_target) :ordering) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.classes.TypeClass" (make-hydra_core_field "ordering" (funcall (lambda (_) (list :unit nil)) y))))) match_value)))) (cadr match_target))))

(provide 'hydra.encode.classes)
