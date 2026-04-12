(ns hydra.names
  (:require [hydra.annotations :refer :all] [hydra.constants :refer :all] [hydra.core :refer :all] [hydra.formatting :refer :all] [hydra.lib.equality :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.literals :refer :all] [hydra.lib.logic :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.math :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.pairs :refer :all] [hydra.lib.sets :refer :all] [hydra.lib.strings :refer :all] [hydra.packaging :refer :all] [hydra.util :refer :all]
))

(declare hydra_names_qualify_name hydra_names_compact_name hydra_names_normal_type_variable hydra_names_fresh_name hydra_names_fresh_names hydra_names_local_name_of hydra_names_name_to_file_path hydra_names_namespace_of hydra_names_namespace_to_file_path hydra_names_qname hydra_names_unique_label hydra_names_unqualify_name)

(def hydra_names_qualify_name (fn [name] (let [parts (hydra_lib_lists_reverse ((hydra_lib_strings_split_on ".") ((fn [v] v) name)))] (if ((hydra_lib_equality_equal 1) (hydra_lib_lists_length parts)) (->hydra_packaging_qualified_name (list :nothing) ((fn [v] v) name)) (->hydra_packaging_qualified_name (list :just ((hydra_lib_strings_intercalate ".") (hydra_lib_lists_reverse (hydra_lib_lists_tail parts)))) (hydra_lib_lists_head parts))))))

(def hydra_names_compact_name (fn [namespaces] (fn [name] (let [qual_name (hydra_names_qualify_name name) local ((fn [v] (:local v)) qual_name) mns ((fn [v] (:namespace v)) qual_name)] (((hydra_lib_maybes_maybe (fn [] ((fn [v] v) name))) (fn [ns_] (((hydra_lib_maybes_maybe (fn [] local)) (fn [pre] (hydra_lib_strings_cat (list pre ":" local)))) ((hydra_lib_maps_lookup ns_) namespaces)))) mns)))))

(def hydra_names_normal_type_variable (fn [i] ((hydra_lib_strings_cat2 "t") (hydra_lib_literals_show_int32 i))))

(def hydra_names_fresh_name (fn [cx] (let [count ((hydra_annotations_get_count hydra_constants_key_fresh_type_variable_count) cx)] (list (hydra_names_normal_type_variable count) (((hydra_annotations_put_count hydra_constants_key_fresh_type_variable_count) ((hydra_lib_math_add count) 1)) cx)))))

(def hydra_names_fresh_names (fn [n] (fn [cx] (let [go_ (fn [acc] (fn [_] (let [names (hydra_lib_pairs_first acc)] (let [cx0 (hydra_lib_pairs_second acc)] (let [result (hydra_names_fresh_name cx0)] (let [name (hydra_lib_pairs_first result)] (let [cx1 (hydra_lib_pairs_second result)] (list ((hydra_lib_lists_concat2 names) (hydra_lib_lists_pure name)) cx1))))))))] (((hydra_lib_lists_foldl go_) (list (list) cx)) ((hydra_lib_lists_replicate n) nil))))))

(def hydra_names_local_name_of (fn [arg_] ((fn [v] (:local v)) (hydra_names_qualify_name arg_))))

(def hydra_names_name_to_file_path (fn [ns_conv] (fn [local_conv] (fn [ext] (fn [name] (let [qual_name (hydra_names_qualify_name name)] (let [ns_ ((fn [v] (:namespace v)) qual_name)] (let [local ((fn [v] (:local v)) qual_name)] (let [ns_to_file_path (fn [ns2] ((hydra_lib_strings_intercalate "/") ((hydra_lib_lists_map (fn [part] (((hydra_formatting_convert_case (list :camel nil)) ns_conv) part))) ((hydra_lib_strings_split_on ".") ((fn [v] v) ns2)))))] (let [prefix (((hydra_lib_maybes_maybe (fn [] "")) (fn [n] ((hydra_lib_strings_cat2 (ns_to_file_path n)) "/"))) ns_)] (let [suffix (((hydra_formatting_convert_case (list :pascal nil)) local_conv) local)] (hydra_lib_strings_cat (list prefix suffix "." ((fn [v] v) ext))))))))))))))

(def hydra_names_namespace_of (fn [arg_] ((fn [v] (:namespace v)) (hydra_names_qualify_name arg_))))

(def hydra_names_namespace_to_file_path (fn [case_conv] (fn [ext] (fn [ns_] (let [parts ((hydra_lib_lists_map ((hydra_formatting_convert_case (list :camel nil)) case_conv)) ((hydra_lib_strings_split_on ".") ((fn [v] v) ns_)))] ((hydra_lib_strings_cat2 ((hydra_lib_strings_cat2 ((hydra_lib_strings_intercalate "/") parts)) ".")) ((fn [v] v) ext)))))))

(def hydra_names_qname (fn [ns_] (fn [name] (hydra_lib_strings_cat (list ((fn [v] v) ns_) "." name)))))

(def hydra_names_unique_label (fn [visited] (fn [l] (if ((hydra_lib_sets_member l) visited) ((hydra_names_unique_label visited) ((hydra_lib_strings_cat2 l) "'")) l))))

(def hydra_names_unqualify_name (fn [qname] (let [prefix (((hydra_lib_maybes_maybe (fn [] "")) (fn [n] ((hydra_lib_strings_cat2 ((fn [v] v) n)) "."))) ((fn [v] (:namespace v)) qname))] ((hydra_lib_strings_cat2 prefix) ((fn [v] (:local v)) qname)))))
