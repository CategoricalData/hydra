(require 'cl-lib)

(require 'hydra.annotations)

(require 'hydra.constants)

(require 'hydra.core)

(require 'hydra.formatting)

(require 'hydra.lib.lists)

(require 'hydra.lib.literals)

(require 'hydra.lib.logic)

(require 'hydra.lib.maps)

(require 'hydra.lib.math)

(require 'hydra.lib.maybes)

(require 'hydra.lib.pairs)

(require 'hydra.lib.sets)

(require 'hydra.lib.strings)

(require 'hydra.packaging)

(require 'hydra.util)

(defvar hydra_names_qualify_name (lambda (name) (let ((parts (hydra_lib_lists_reverse (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) name))))) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (make-hydra_packaging_qualified_name (list :nothing) (funcall (lambda (v) v) name)))) (lambda (uc) (let ((local_name (hydra_lib_pairs_first uc))) (let ((rest_reversed (hydra_lib_pairs_second uc))) (if (hydra_lib_lists_null rest_reversed) (make-hydra_packaging_qualified_name (list :nothing) (funcall (lambda (v) v) name)) (make-hydra_packaging_qualified_name (list :just (funcall (hydra_lib_strings_intercalate ".") (hydra_lib_lists_reverse rest_reversed))) local_name)))))) (hydra_lib_lists_uncons parts)))))

(defvar hydra_names_compact_name (lambda (namespaces) (lambda (name) (let* ((qual_name (hydra_names_qualify_name name)) (local (funcall (lambda (v) (hydra_packaging_qualified_name-local v)) qual_name)) (mns (funcall (lambda (v) (hydra_packaging_qualified_name-namespace v)) qual_name))) (funcall (funcall (hydra_lib_maybes_maybe (lambda () (funcall (lambda (v) v) name))) (lambda (ns_) (funcall (funcall (hydra_lib_maybes_maybe (lambda () local)) (lambda (pre) (hydra_lib_strings_cat (list pre ":" local)))) (funcall (hydra_lib_maps_lookup ns_) namespaces)))) mns)))))

(defvar hydra_names_normal_type_variable (lambda (i) (funcall (hydra_lib_strings_cat2 "t") (hydra_lib_literals_show_int32 i))))

(defvar hydra_names_fresh_name (lambda (cx) (let ((count (funcall (hydra_annotations_get_count hydra_constants_key_fresh_type_variable_count) cx))) (list (hydra_names_normal_type_variable count) (funcall (funcall (hydra_annotations_put_count hydra_constants_key_fresh_type_variable_count) (funcall (hydra_lib_math_add count) 1)) cx)))))

(defvar hydra_names_fresh_names (lambda (n) (lambda (cx) (let ((go_ (lambda (acc) (lambda (_) (let ((names (hydra_lib_pairs_first acc))) (let ((cx0 (hydra_lib_pairs_second acc))) (let ((result (hydra_names_fresh_name cx0))) (let ((name (hydra_lib_pairs_first result))) (let ((cx1 (hydra_lib_pairs_second result))) (list (funcall (hydra_lib_lists_concat2 names) (hydra_lib_lists_pure name)) cx1)))))))))) (funcall (funcall (hydra_lib_lists_foldl go_) (list (list) cx)) (funcall (hydra_lib_lists_replicate n) nil))))))

(defvar hydra_names_local_name_of (lambda (arg_) (funcall (lambda (v) (hydra_packaging_qualified_name-local v)) (hydra_names_qualify_name arg_))))

(defvar hydra_names_name_to_file_path (lambda (ns_conv) (lambda (local_conv) (lambda (ext) (lambda (name) (let ((qual_name (hydra_names_qualify_name name))) (let ((ns_ (funcall (lambda (v) (hydra_packaging_qualified_name-namespace v)) qual_name))) (let ((local (funcall (lambda (v) (hydra_packaging_qualified_name-local v)) qual_name))) (let ((ns_to_file_path (lambda (ns2) (funcall (hydra_lib_strings_intercalate "/") (funcall (hydra_lib_lists_map (lambda (part) (funcall (funcall (hydra_formatting_convert_case (list :camel nil)) ns_conv) part))) (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) ns2))))))) (let ((prefix (funcall (funcall (hydra_lib_maybes_maybe (lambda () "")) (lambda (n) (funcall (hydra_lib_strings_cat2 (ns_to_file_path n)) "/"))) ns_))) (let ((suffix (funcall (funcall (hydra_formatting_convert_case (list :pascal nil)) local_conv) local))) (hydra_lib_strings_cat (list prefix suffix "." (funcall (lambda (v) v) ext))))))))))))))

(defvar hydra_names_namespace_of (lambda (arg_) (funcall (lambda (v) (hydra_packaging_qualified_name-namespace v)) (hydra_names_qualify_name arg_))))

(defvar hydra_names_namespace_to_file_path (lambda (case_conv) (lambda (ext) (lambda (ns_) (let ((parts (funcall (hydra_lib_lists_map (funcall (hydra_formatting_convert_case (list :camel nil)) case_conv)) (funcall (hydra_lib_strings_split_on ".") (funcall (lambda (v) v) ns_))))) (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_cat2 (funcall (hydra_lib_strings_intercalate "/") parts)) ".")) (funcall (lambda (v) v) ext)))))))

(defvar hydra_names_qname (lambda (ns_) (lambda (name) (hydra_lib_strings_cat (list (funcall (lambda (v) v) ns_) "." name)))))

(defvar hydra_names_unique_label (lambda (visited) (lambda (l) (if (funcall (hydra_lib_sets_member l) visited) (funcall (hydra_names_unique_label visited) (funcall (hydra_lib_strings_cat2 l) "'")) l))))

(defvar hydra_names_unqualify_name (lambda (qname) (let ((prefix (funcall (funcall (hydra_lib_maybes_maybe (lambda () "")) (lambda (n) (funcall (hydra_lib_strings_cat2 (funcall (lambda (v) v) n)) "."))) (funcall (lambda (v) (hydra_packaging_qualified_name-namespace v)) qname)))) (funcall (hydra_lib_strings_cat2 prefix) (funcall (lambda (v) (hydra_packaging_qualified_name-local v)) qname)))))

(provide 'hydra.names)
