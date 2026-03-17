(require 'cl-lib)

(require 'hydra.context)

(require 'hydra.core)

(require 'hydra.encode.core)

(require 'hydra.lib.lists)

(require 'hydra.lib.maps)

(defvar hydra_encode_context_context (lambda (x) (list :record (make-hydra_core_record "hydra.context.Context" (list (make-hydra_core_field "trace" ((lambda (xs) (list :list ((hydra_lib_lists_map (lambda (x) (list :literal (list :string x)))) xs))) ((lambda (v) (hydra_context_context-trace v)) x))) (make-hydra_core_field "messages" ((lambda (xs) (list :list ((hydra_lib_lists_map (lambda (x) (list :literal (list :string x)))) xs))) ((lambda (v) (hydra_context_context-messages v)) x))) (make-hydra_core_field "other" ((lambda (m) (list :map (((hydra_lib_maps_bimap hydra_encode_core_name) hydra_encode_core_term) m))) ((lambda (v) (hydra_context_context-other v)) x))))))))

(defvar hydra_encode_context_in_context (lambda (e) (lambda (x) (list :record (make-hydra_core_record "hydra.context.InContext" (list (make-hydra_core_field "object" (e ((lambda (v) (hydra_context_in_context-object v)) x))) (make-hydra_core_field "context" (hydra_encode_context_context ((lambda (v) (hydra_context_in_context-context v)) x)))))))))

(provide 'hydra.encode.context)
