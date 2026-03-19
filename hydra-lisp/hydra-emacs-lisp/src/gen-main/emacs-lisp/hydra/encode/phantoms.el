(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.encode.core)

(require 'hydra.phantoms)

(defvar hydra_encode_phantoms_t_term (lambda (a) (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.phantoms.TTerm" (hydra_encode_core_term (funcall (lambda (v) v) x)))))))

(defvar hydra_encode_phantoms_t_binding (lambda (a) (lambda (x) (list :record (make-hydra_core_record "hydra.phantoms.TBinding" (list (make-hydra_core_field "name" (hydra_encode_core_name (funcall (lambda (v) (hydra_phantoms_t_binding-name v)) x))) (make-hydra_core_field "term" (funcall (hydra_encode_phantoms_t_term a) (funcall (lambda (v) (hydra_phantoms_t_binding-term v)) x)))))))))

(provide 'hydra.encode.phantoms)
