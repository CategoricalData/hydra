(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.encode.core)

(require 'hydra.lib.lists)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.packaging)

(defvar hydra_encode_packaging_term_definition (lambda (x) (list :record (make-hydra_core_record "hydra.packaging.TermDefinition" (list (make-hydra_core_field "name" (hydra_encode_core_name (funcall (lambda (v) (hydra_packaging_term_definition-name v)) x))) (make-hydra_core_field "term" (hydra_encode_core_term (funcall (lambda (v) (hydra_packaging_term_definition-term v)) x))) (make-hydra_core_field "type" (funcall (lambda (opt) (list :maybe (funcall (hydra_lib_maybes_map hydra_encode_core_type_scheme) opt))) (funcall (lambda (v) (hydra_packaging_term_definition-type v)) x))))))))

(defvar hydra_encode_packaging_type_definition (lambda (x) (list :record (make-hydra_core_record "hydra.packaging.TypeDefinition" (list (make-hydra_core_field "name" (hydra_encode_core_name (funcall (lambda (v) (hydra_packaging_type_definition-name v)) x))) (make-hydra_core_field "type" (hydra_encode_core_type_scheme (funcall (lambda (v) (hydra_packaging_type_definition-type v)) x))))))))

(defvar hydra_encode_packaging_definition (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :term) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.packaging.Definition" (make-hydra_core_field "term" (hydra_encode_packaging_term_definition y))))) match_value)) ((equal (car match_target) :type) (funcall (lambda (y) (list :union (make-hydra_core_injection "hydra.packaging.Definition" (make-hydra_core_field "type" (hydra_encode_packaging_type_definition y))))) match_value)))) (cadr match_target))))

(defvar hydra_encode_packaging_file_extension (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.packaging.FileExtension" (funcall (lambda (x2) (list :literal (list :string x2))) (funcall (lambda (v) v) x))))))

(defvar hydra_encode_packaging_namespace (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.packaging.Namespace" (funcall (lambda (x2) (list :literal (list :string x2))) (funcall (lambda (v) v) x))))))

(defvar hydra_encode_packaging_module (lambda (x) (list :record (make-hydra_core_record "hydra.packaging.Module" (list (make-hydra_core_field "namespace" (hydra_encode_packaging_namespace (funcall (lambda (v) (hydra_packaging_module-namespace v)) x))) (make-hydra_core_field "definitions" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_packaging_definition) xs))) (funcall (lambda (v) (hydra_packaging_module-definitions v)) x))) (make-hydra_core_field "termDependencies" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_packaging_namespace) xs))) (funcall (lambda (v) (hydra_packaging_module-term_dependencies v)) x))) (make-hydra_core_field "typeDependencies" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_packaging_namespace) xs))) (funcall (lambda (v) (hydra_packaging_module-type_dependencies v)) x))) (make-hydra_core_field "description" (funcall (lambda (opt) (list :maybe (funcall (hydra_lib_maybes_map (lambda (x2) (list :literal (list :string x2)))) opt))) (funcall (lambda (v) (hydra_packaging_module-description v)) x))))))))

(defvar hydra_encode_packaging_namespaces (lambda (n) (lambda (x) (list :record (make-hydra_core_record "hydra.packaging.Namespaces" (list (make-hydra_core_field "focus" (funcall (lambda (p) (list :pair (funcall (funcall (hydra_lib_pairs_bimap hydra_encode_packaging_namespace) n) p))) (funcall (lambda (v) (hydra_packaging_namespaces-focus v)) x))) (make-hydra_core_field "mapping" (funcall (lambda (m) (list :map (funcall (funcall (hydra_lib_maps_bimap hydra_encode_packaging_namespace) n) m))) (funcall (lambda (v) (hydra_packaging_namespaces-mapping v)) x)))))))))

(defvar hydra_encode_packaging_package_name (lambda (x) (list :wrap (make-hydra_core_wrapped_term "hydra.packaging.PackageName" (funcall (lambda (x2) (list :literal (list :string x2))) (funcall (lambda (v) v) x))))))

(defvar hydra_encode_packaging_package (lambda (x) (list :record (make-hydra_core_record "hydra.packaging.Package" (list (make-hydra_core_field "name" (hydra_encode_packaging_package_name (funcall (lambda (v) (hydra_packaging_package-name v)) x))) (make-hydra_core_field "modules" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_packaging_module) xs))) (funcall (lambda (v) (hydra_packaging_package-modules v)) x))) (make-hydra_core_field "dependencies" (funcall (lambda (xs) (list :list (funcall (hydra_lib_lists_map hydra_encode_packaging_package_name) xs))) (funcall (lambda (v) (hydra_packaging_package-dependencies v)) x))) (make-hydra_core_field "description" (funcall (lambda (opt) (list :maybe (funcall (hydra_lib_maybes_map (lambda (x2) (list :literal (list :string x2)))) opt))) (funcall (lambda (v) (hydra_packaging_package-description v)) x))))))))

(defvar hydra_encode_packaging_qualified_name (lambda (x) (list :record (make-hydra_core_record "hydra.packaging.QualifiedName" (list (make-hydra_core_field "namespace" (funcall (lambda (opt) (list :maybe (funcall (hydra_lib_maybes_map hydra_encode_packaging_namespace) opt))) (funcall (lambda (v) (hydra_packaging_qualified_name-namespace v)) x))) (make-hydra_core_field "local" (funcall (lambda (x2) (list :literal (list :string x2))) (funcall (lambda (v) (hydra_packaging_qualified_name-local v)) x))))))))

(provide 'hydra.encode.packaging)
