(defpackage :hydra.encode.phantoms
(:use :cl :hydra.core :hydra.encode.core :hydra.phantoms)
(:export :hydra_encode_phantoms_t_term :hydra_encode_phantoms_t_binding :hydra_encode_phantoms_t_term_definition))

(in-package :hydra.encode.phantoms)

(cl:defvar hydra_encode_phantoms_t_term (cl:lambda (a) (cl:lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.phantoms.TTerm" (hydra_encode_core_term ((cl:lambda (v) v) x)))))))

(cl:defvar hydra_encode_phantoms_t_binding (cl:lambda (a) (cl:lambda (x) (list :record (make-hydra_core_record "hydra.phantoms.TBinding" (cl:list (make-hydra_core_field "name" (hydra_encode_core_name ((cl:lambda (v) (hydra_phantoms_t_binding-name v)) x))) (make-hydra_core_field "term" ((hydra_encode_phantoms_t_term a) ((cl:lambda (v) (hydra_phantoms_t_binding-term v)) x)))))))))

(cl:defvar hydra_encode_phantoms_t_term_definition (cl:lambda (a) (cl:lambda (x) (list :record (make-hydra_core_record "hydra.phantoms.TTermDefinition" (cl:list (make-hydra_core_field "name" (hydra_encode_core_name ((cl:lambda (v) (hydra_phantoms_t_term_definition-name v)) x))) (make-hydra_core_field "term" ((hydra_encode_phantoms_t_term a) ((cl:lambda (v) (hydra_phantoms_t_term_definition-term v)) x)))))))))
