(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.formatting)

(require 'hydra.lib.equality)

(require 'hydra.lib.lists)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.maybes)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.module)

(require 'hydra.util)

(defvar hydra_names_qualify_name (lambda (name) (let ((parts (hydra_lib_lists_reverse ((hydra_lib_strings_split_on ".") ((lambda (v) v) name))))) (if ((hydra_lib_equality_equal 1) (hydra_lib_lists_length parts)) (make-hydra_module_qualified_name (list :nothing) ((lambda (v) v) name)) (make-hydra_module_qualified_name (list :just ((hydra_lib_strings_intercalate ".") (hydra_lib_lists_reverse (hydra_lib_lists_tail parts)))) (hydra_lib_lists_head parts))))))

(defvar hydra_names_compact_name (lambda (namespaces) (lambda (name) (let* ((qual_name (hydra_names_qualify_name name)) (local ((lambda (v) (hydra_module_qualified_name-local v)) qual_name)) (mns ((lambda (v) (hydra_module_qualified_name-namespace v)) qual_name))) (((hydra_lib_maybes_maybe ((lambda (v) v) name)) (lambda (ns_) (((hydra_lib_maybes_maybe local) (lambda (pre) (hydra_lib_strings_cat (list pre ":" local)))) ((hydra_lib_maps_lookup ns_) namespaces)))) mns)))))

(defvar hydra_names_local_name_of (lambda (arg_) ((lambda (v) (hydra_module_qualified_name-local v)) (hydra_names_qualify_name arg_))))

(defvar hydra_names_namespace_of (lambda (arg_) ((lambda (v) (hydra_module_qualified_name-namespace v)) (hydra_names_qualify_name arg_))))

(defvar hydra_names_namespace_to_file_path (lambda (case_conv) (lambda (ext) (lambda (ns_) (let ((parts ((hydra_lib_lists_map ((hydra_formatting_convert_case (list :camel nil)) case_conv)) ((hydra_lib_strings_split_on ".") ((lambda (v) v) ns_))))) ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_intercalate "/") parts)) ".")) ((lambda (v) v) ext)))))))

(defvar hydra_names_qname (lambda (ns_) (lambda (name) (hydra_lib_strings_cat (list ((lambda (v) v) ns_) "." name)))))

(defvar hydra_names_unique_label (lambda (visited) (lambda (l) (if ((hydra_lib_sets_member l) visited) ((hydra_names_unique_label visited) ((hydra_lib_strings_cat2 l) "'")) l))))

(defvar hydra_names_unqualify_name (lambda (qname) (let ((prefix (((hydra_lib_maybes_maybe "") (lambda (n) ((hydra_lib_strings_cat2 ((lambda (v) v) n)) "."))) ((lambda (v) (hydra_module_qualified_name-namespace v)) qname)))) ((hydra_lib_strings_cat2 prefix) ((lambda (v) (hydra_module_qualified_name-local v)) qname)))))

(provide 'hydra.names)
