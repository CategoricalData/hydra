(defpackage :hydra.encode.context
(:use :cl :hydra.context :hydra.core :hydra.encode.core :hydra.lib.lists :hydra.lib.maps)
(:export :hydra_encode_context_context :hydra_encode_context_in_context))

(in-package :hydra.encode.context)

(cl:defvar hydra_encode_context_context (cl:lambda (x) (list :record (make-hydra_core_record "hydra.context.Context" (cl:list (make-hydra_core_field "trace" ((cl:lambda (xs) (list :list ((hydra_lib_lists_map (cl:lambda (x2) (list :literal (list :string x2)))) xs))) ((cl:lambda (v) (hydra_context_context-trace v)) x))) (make-hydra_core_field "messages" ((cl:lambda (xs) (list :list ((hydra_lib_lists_map (cl:lambda (x2) (list :literal (list :string x2)))) xs))) ((cl:lambda (v) (hydra_context_context-messages v)) x))) (make-hydra_core_field "other" ((cl:lambda (m) (list :map (((hydra_lib_maps_bimap hydra_encode_core_name) hydra_encode_core_term) m))) ((cl:lambda (v) (hydra_context_context-other v)) x))))))))

(cl:defvar hydra_encode_context_in_context (cl:lambda (e) (cl:lambda (x) (list :record (make-hydra_core_record "hydra.context.InContext" (cl:list (make-hydra_core_field "object" (e ((cl:lambda (v) (hydra_context_in_context-object v)) x))) (make-hydra_core_field "context" (hydra_encode_context_context ((cl:lambda (v) (hydra_context_in_context-context v)) x)))))))))
