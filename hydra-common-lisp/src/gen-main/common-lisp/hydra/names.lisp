(defpackage :hydra.names
(:use :cl :hydra.core :hydra.formatting :hydra.lib.equality :hydra.lib.lists :hydra.lib.logic :hydra.lib.maps :hydra.lib.maybes :hydra.lib.sets :hydra.lib.strings :hydra.module :hydra.util)
(:export :hydra_names_qualify_name :hydra_names_compact_name :hydra_names_local_name_of :hydra_names_namespace_of :hydra_names_namespace_to_file_path :hydra_names_qname :hydra_names_unique_label :hydra_names_unqualify_name))

(in-package :hydra.names)

(cl:defvar hydra_names_qualify_name (cl:lambda (name) (let ((parts (hydra_lib_lists_reverse ((hydra_lib_strings_split_on ".") ((cl:lambda (v) v) name))))) (if ((hydra_lib_equality_equal 1) (hydra_lib_lists_length parts)) (make-hydra_module_qualified_name cl:nil ((cl:lambda (v) v) name)) (make-hydra_module_qualified_name ((hydra_lib_strings_intercalate ".") (hydra_lib_lists_reverse (hydra_lib_lists_tail parts))) (hydra_lib_lists_head parts))))))

(cl:defvar hydra_names_compact_name (cl:lambda (namespaces) (cl:lambda (name) (let* ((qual_name (hydra_names_qualify_name name)) (local ((cl:lambda (v) (hydra_module_qualified_name-local v)) qual_name)) (mns ((cl:lambda (v) (hydra_module_qualified_name-namespace v)) qual_name))) (((hydra_lib_maybes_maybe ((cl:lambda (v) v) name)) (cl:lambda (ns_) (((hydra_lib_maybes_maybe local) (cl:lambda (pre) (hydra_lib_strings_cat (cl:list pre ":" local)))) ((hydra_lib_maps_lookup ns_) namespaces)))) mns)))))

(cl:defvar hydra_names_local_name_of (cl:lambda (arg_) ((cl:lambda (v) (hydra_module_qualified_name-local v)) (hydra_names_qualify_name arg_))))

(cl:defvar hydra_names_namespace_of (cl:lambda (arg_) ((cl:lambda (v) (hydra_module_qualified_name-namespace v)) (hydra_names_qualify_name arg_))))

(cl:defvar hydra_names_namespace_to_file_path (cl:lambda (case_conv) (cl:lambda (ext) (cl:lambda (ns_) (let ((parts ((hydra_lib_lists_map ((hydra_formatting_convert_case (list :camel cl:nil)) case_conv)) ((hydra_lib_strings_split_on ".") ((cl:lambda (v) v) ns_))))) ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_intercalate "/") parts)) ".")) ((cl:lambda (v) v) ext)))))))

(cl:defvar hydra_names_qname (cl:lambda (ns_) (cl:lambda (name) (hydra_lib_strings_cat (cl:list ((cl:lambda (v) v) ns_) "." name)))))

(cl:defvar hydra_names_unique_label (cl:lambda (visited) (cl:lambda (l) (if ((hydra_lib_sets_member l) visited) ((hydra_names_unique_label visited) ((hydra_lib_strings_cat2 l) "'")) l))))

(cl:defvar hydra_names_unqualify_name (cl:lambda (qname) (let ((prefix (((hydra_lib_maybes_maybe "") (cl:lambda (n) ((hydra_lib_strings_cat2 ((cl:lambda (v) v) n)) "."))) ((cl:lambda (v) (hydra_module_qualified_name-namespace v)) qname)))) ((hydra_lib_strings_cat2 prefix) ((cl:lambda (v) (hydra_module_qualified_name-local v)) qname)))))
