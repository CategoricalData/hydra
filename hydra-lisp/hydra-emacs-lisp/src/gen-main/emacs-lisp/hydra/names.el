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

(defvar hydra_names_qualify_name (lambda (name) (let ((parts (hydra_lib_lists_reverse (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) name))))) (if (funcall (hydra_lib_equality_equal 1) (hydra_lib_lists_length parts)) (make-hydra_module_qualified_name (list :nothing) (funcall (lambda (v) v) name)) (make-hydra_module_qualified_name (list :just (funcall (hydra_lib_strings_intercalate ".") (hydra_lib_lists_reverse (hydra_lib_lists_tail parts)))) (hydra_lib_lists_head parts))))))

(defvar hydra_names_compact_name (lambda (namespaces) (lambda (name) (let* ((qual_name (hydra_names_qualify_name name)) (local (funcall (lambda (v) (hydra_module_qualified_name-local v)) qual_name)) (mns (funcall (lambda (v) (hydra_module_qualified_name-namespace v)) qual_name))) (funcall (funcall (hydra_lib_maybes_maybe (funcall (lambda (v) v) name)) (lambda (ns_) (funcall (funcall (hydra_lib_maybes_maybe local) (lambda (pre) (hydra_lib_strings_cat (list pre ":" local)))) (funcall (hydra_lib_maps_lookup ns_) namespaces)))) mns)))))

(defvar hydra_names_local_name_of (lambda (arg_) (funcall (lambda (v) (hydra_module_qualified_name-local v)) (hydra_names_qualify_name arg_))))

(defvar hydra_names_namespace_of (lambda (arg_) (funcall (lambda (v) (hydra_module_qualified_name-namespace v)) (hydra_names_qualify_name arg_))))

(defvar hydra_names_namespace_to_file_path (lambda (case_conv) (lambda (ext) (lambda (ns_) (let ((parts (funcall (hydra_lib_lists_map (funcall (hydra_formatting_convert_case (list :camel nil)) case_conv)) (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) ns_))))) (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_intercalate "/") parts)) ".")) (funcall (lambda (v) v) ext)))))))

(defvar hydra_names_qname (lambda (ns_) (lambda (name) (hydra_lib_strings_cat (list (funcall (lambda (v) v) ns_) "." name)))))

(defvar hydra_names_unique_label (lambda (visited) (lambda (l) (if (funcall (hydra_lib_sets_member l) visited) (funcall (hydra_names_unique_label visited) (funcall (hydra_lib_strings_cat2 l) "'")) l))))

(defvar hydra_names_unqualify_name (lambda (qname) (let ((prefix (funcall (funcall (hydra_lib_maybes_maybe "") (lambda (n) (funcall (hydra_lib_strings_cat2 (funcall (lambda (v) v) n)) "."))) (funcall (lambda (v) (hydra_module_qualified_name-namespace v)) qname)))) (funcall (hydra_lib_strings_cat2 prefix) (funcall (lambda (v) (hydra_module_qualified_name-local v)) qname)))))

(provide 'hydra.names)
