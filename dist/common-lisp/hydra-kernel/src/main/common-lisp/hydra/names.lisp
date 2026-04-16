(defpackage :hydra.names
(:use :cl :hydra.annotations :hydra.constants :hydra.core :hydra.formatting :hydra.lib.lists :hydra.lib.literals :hydra.lib.logic :hydra.lib.maps :hydra.lib.math :hydra.lib.maybes :hydra.lib.pairs :hydra.lib.sets :hydra.lib.strings :hydra.packaging :hydra.util)
(:export :hydra_names_qualify_name :hydra_names_compact_name :hydra_names_normal_type_variable :hydra_names_fresh_name :hydra_names_fresh_names :hydra_names_local_name_of :hydra_names_name_to_file_path :hydra_names_namespace_of :hydra_names_namespace_to_file_path :hydra_names_qname :hydra_names_unique_label :hydra_names_unqualify_name))

(in-package :hydra.names)

(cl:defvar hydra_names_qualify_name (cl:lambda (name) (let ((parts (hydra_lib_lists_reverse ((hydra_lib_strings_split_on ".") ((cl:lambda (v) v) name))))) (((hydra_lib_maybes_maybe (cl:lambda () (make-hydra_packaging_qualified_name (list :nothing) ((cl:lambda (v) v) name)))) (cl:lambda (uc) (let ((local_name (hydra_lib_pairs_first uc))) (let ((rest_reversed (hydra_lib_pairs_second uc))) (if (hydra_lib_lists_null rest_reversed) (make-hydra_packaging_qualified_name (list :nothing) ((cl:lambda (v) v) name)) (make-hydra_packaging_qualified_name (list :just ((hydra_lib_strings_intercalate ".") (hydra_lib_lists_reverse rest_reversed))) local_name)))))) (hydra_lib_lists_uncons parts)))))

(cl:defvar hydra_names_compact_name (cl:lambda (namespaces) (cl:lambda (name) (let* ((qual_name (hydra_names_qualify_name name)) (local ((cl:lambda (v) (hydra_packaging_qualified_name-local v)) qual_name)) (mns ((cl:lambda (v) (hydra_packaging_qualified_name-namespace v)) qual_name))) (((hydra_lib_maybes_maybe (cl:lambda () ((cl:lambda (v) v) name))) (cl:lambda (ns_) (((hydra_lib_maybes_maybe (cl:lambda () local)) (cl:lambda (pre) (hydra_lib_strings_cat (cl:list pre ":" local)))) ((hydra_lib_maps_lookup ns_) namespaces)))) mns)))))

(cl:defvar hydra_names_normal_type_variable (cl:lambda (i) ((hydra_lib_strings_cat2 "t") (hydra_lib_literals_show_int32 i))))

(cl:defvar hydra_names_fresh_name (cl:lambda (cx) (let ((count ((hydra_annotations_get_count hydra_constants_key_fresh_type_variable_count) cx))) (cl:list (hydra_names_normal_type_variable count) (((hydra_annotations_put_count hydra_constants_key_fresh_type_variable_count) ((hydra_lib_math_add count) 1)) cx)))))

(cl:defvar hydra_names_fresh_names (cl:lambda (n) (cl:lambda (cx) (let ((go_ (cl:lambda (acc) (cl:lambda (_) (let ((names (hydra_lib_pairs_first acc))) (let ((cx0 (hydra_lib_pairs_second acc))) (let ((result (hydra_names_fresh_name cx0))) (let ((name (hydra_lib_pairs_first result))) (let ((cx1 (hydra_lib_pairs_second result))) (cl:list ((hydra_lib_lists_concat2 names) (hydra_lib_lists_pure name)) cx1)))))))))) (((hydra_lib_lists_foldl go_) (cl:list (cl:list) cx)) ((hydra_lib_lists_replicate n) cl:nil))))))

(cl:defvar hydra_names_local_name_of (cl:lambda (arg_) ((cl:lambda (v) (hydra_packaging_qualified_name-local v)) (hydra_names_qualify_name arg_))))

(cl:defvar hydra_names_name_to_file_path (cl:lambda (ns_conv) (cl:lambda (local_conv) (cl:lambda (ext) (cl:lambda (name) (let ((qual_name (hydra_names_qualify_name name))) (let ((ns_ ((cl:lambda (v) (hydra_packaging_qualified_name-namespace v)) qual_name))) (let ((local ((cl:lambda (v) (hydra_packaging_qualified_name-local v)) qual_name))) (let ((ns_to_file_path (cl:lambda (ns2) ((hydra_lib_strings_intercalate "/") ((hydra_lib_lists_map (cl:lambda (part) (((hydra_formatting_convert_case (list :camel cl:nil)) ns_conv) part))) ((hydra_lib_strings_split_on ".") ((cl:lambda (v) v) ns2))))))) (let ((prefix (((hydra_lib_maybes_maybe (cl:lambda () "")) (cl:lambda (n) ((hydra_lib_strings_cat2 (ns_to_file_path n)) "/"))) ns_))) (let ((suffix (((hydra_formatting_convert_case (list :pascal cl:nil)) local_conv) local))) (hydra_lib_strings_cat (cl:list prefix suffix "." ((cl:lambda (v) v) ext))))))))))))))

(cl:defvar hydra_names_namespace_of (cl:lambda (arg_) ((cl:lambda (v) (hydra_packaging_qualified_name-namespace v)) (hydra_names_qualify_name arg_))))

(cl:defvar hydra_names_namespace_to_file_path (cl:lambda (case_conv) (cl:lambda (ext) (cl:lambda (ns_) (let ((parts ((hydra_lib_lists_map ((hydra_formatting_convert_case (list :camel cl:nil)) case_conv)) ((hydra_lib_strings_split_on ".") ((cl:lambda (v) v) ns_))))) ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_intercalate "/") parts)) ".")) ((cl:lambda (v) v) ext)))))))

(cl:defvar hydra_names_qname (cl:lambda (ns_) (cl:lambda (name) (hydra_lib_strings_cat (cl:list ((cl:lambda (v) v) ns_) "." name)))))

(cl:defvar hydra_names_unique_label (cl:lambda (visited) (cl:lambda (l) (if ((hydra_lib_sets_member l) visited) ((hydra_names_unique_label visited) ((hydra_lib_strings_cat2 l) "'")) l))))

(cl:defvar hydra_names_unqualify_name (cl:lambda (qname) (let ((prefix (((hydra_lib_maybes_maybe (cl:lambda () "")) (cl:lambda (n) ((hydra_lib_strings_cat2 ((cl:lambda (v) v) n)) "."))) ((cl:lambda (v) (hydra_packaging_qualified_name-namespace v)) qname)))) ((hydra_lib_strings_cat2 prefix) ((cl:lambda (v) (hydra_packaging_qualified_name-local v)) qname)))))
