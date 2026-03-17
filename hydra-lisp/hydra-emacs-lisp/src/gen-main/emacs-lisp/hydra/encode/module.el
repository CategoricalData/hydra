(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.encode.core)

(require 'hydra.lib.lists)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.module)

(defvar hydra_encode_module_term_definition (lambda (x) (list :record (make-hydra_core_record "hydra.module.TermDefinition" (list (make-hydra_core_field "name" (hydra_encode_core_name ((lambda (v) (hydra_module_term_definition-name v)) x))) (make-hydra_core_field "term" (hydra_encode_core_term ((lambda (v) (hydra_module_term_definition-term v)) x))) (make-hydra_core_field "type" (hydra_encode_core_type_scheme ((lambda (v) (hydra_module_term_definition-type v)) x))))))))

(defvar hydra_encode_module_type_definition (lambda (x) (list :record (make-hydra_core_record "hydra.module.TypeDefinition" (list (make-hydra_core_field "name" (hydra_encode_core_name ((lambda (v) (hydra_module_type_definition-name v)) x))) (make-hydra_core_field "type" (hydra_encode_core_type ((lambda (v) (hydra_module_type_definition-type v)) x))))))))

(defvar hydra_encode_module_definition (lambda (match_target) ((lambda (match_value) (cond ((equal (car match_target) :term) ((lambda (y) (list :union (make-hydra_core_injection "hydra.module.Definition" (make-hydra_core_field "term" (hydra_encode_module_term_definition y))))) match_value)) ((equal (car match_target) :type) ((lambda (y) (list :union (make-hydra_core_injection "hydra.module.Definition" (make-hydra_core_field "type" (hydra_encode_module_type_definition y))))) match_value)))) (cadr match_target))))

(defvar hydra_encode_module_file_extension (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.module.FileExtension" ((lambda (x) (list :literal (list :string x))) ((lambda (v) v) x))))))

(defvar hydra_encode_module_namespace (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.module.Namespace" ((lambda (x) (list :literal (list :string x))) ((lambda (v) v) x))))))

(defvar hydra_encode_module_module (lambda (x) (list :record (make-hydra_core_record "hydra.module.Module" (list (make-hydra_core_field "namespace" (hydra_encode_module_namespace ((lambda (v) (hydra_module_module-namespace v)) x))) (make-hydra_core_field "elements" ((lambda (xs) (list :list ((hydra_lib_lists_map hydra_encode_core_binding) xs))) ((lambda (v) (hydra_module_module-elements v)) x))) (make-hydra_core_field "termDependencies" ((lambda (xs) (list :list ((hydra_lib_lists_map hydra_encode_module_namespace) xs))) ((lambda (v) (hydra_module_module-term_dependencies v)) x))) (make-hydra_core_field "typeDependencies" ((lambda (xs) (list :list ((hydra_lib_lists_map hydra_encode_module_namespace) xs))) ((lambda (v) (hydra_module_module-type_dependencies v)) x))) (make-hydra_core_field "description" ((lambda (opt) (list :maybe ((hydra_lib_maybes_map (lambda (x) (list :literal (list :string x)))) opt))) ((lambda (v) (hydra_module_module-description v)) x))))))))

(defvar hydra_encode_module_namespaces (lambda (n) (lambda (x) (list :record (make-hydra_core_record "hydra.module.Namespaces" (list (make-hydra_core_field "focus" ((lambda (p) (list :pair (((hydra_lib_pairs_bimap hydra_encode_module_namespace) n) p))) ((lambda (v) (hydra_module_namespaces-focus v)) x))) (make-hydra_core_field "mapping" ((lambda (m) (list :map (((hydra_lib_maps_bimap hydra_encode_module_namespace) n) m))) ((lambda (v) (hydra_module_namespaces-mapping v)) x)))))))))

(defvar hydra_encode_module_qualified_name (lambda (x) (list :record (make-hydra_core_record "hydra.module.QualifiedName" (list (make-hydra_core_field "namespace" ((lambda (opt) (list :maybe ((hydra_lib_maybes_map hydra_encode_module_namespace) opt))) ((lambda (v) (hydra_module_qualified_name-namespace v)) x))) (make-hydra_core_field "local" ((lambda (x) (list :literal (list :string x))) ((lambda (v) (hydra_module_qualified_name-local v)) x))))))))

(provide 'hydra.encode.module)
