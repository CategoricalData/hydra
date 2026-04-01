(ns hydra.encode.module
  (:require [hydra.core :refer :all] [hydra.encode.core :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.pairs :refer :all] [hydra.module :refer :all]
))

(declare hydra_encode_module_term_definition hydra_encode_module_type_definition hydra_encode_module_definition hydra_encode_module_file_extension hydra_encode_module_namespace hydra_encode_module_module hydra_encode_module_namespaces hydra_encode_module_qualified_name)

(def hydra_encode_module_term_definition (fn [x] (list :record (->hydra_core_record "hydra.module.TermDefinition" (list (->hydra_core_field "name" (hydra_encode_core_name ((fn [v] (:name v)) x))) (->hydra_core_field "term" (hydra_encode_core_term ((fn [v] (:term v)) x))) (->hydra_core_field "type" ((fn [opt] (list :maybe ((hydra_lib_maybes_map hydra_encode_core_type_scheme) opt))) ((fn [v] (:type v)) x))))))))

(def hydra_encode_module_type_definition (fn [x] (list :record (->hydra_core_record "hydra.module.TypeDefinition" (list (->hydra_core_field "name" (hydra_encode_core_name ((fn [v] (:name v)) x))) (->hydra_core_field "type" (hydra_encode_core_type ((fn [v] (:type v)) x))))))))

(def hydra_encode_module_definition (fn [match_target] ((fn [match_value] (cond (= (first match_target) :term) ((fn [y] (list :union (->hydra_core_injection "hydra.module.Definition" (->hydra_core_field "term" (hydra_encode_module_term_definition y))))) match_value) (= (first match_target) :type) ((fn [y] (list :union (->hydra_core_injection "hydra.module.Definition" (->hydra_core_field "type" (hydra_encode_module_type_definition y))))) match_value))) (second match_target))))

(def hydra_encode_module_file_extension (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.module.FileExtension" ((fn [x2] (list :literal (list :string x2))) ((fn [v] v) x))))))

(def hydra_encode_module_namespace (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.module.Namespace" ((fn [x2] (list :literal (list :string x2))) ((fn [v] v) x))))))

(def hydra_encode_module_module (fn [x] (list :record (->hydra_core_record "hydra.module.Module" (list (->hydra_core_field "namespace" (hydra_encode_module_namespace ((fn [v] (:namespace v)) x))) (->hydra_core_field "definitions" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_module_definition) xs))) ((fn [v] (:definitions v)) x))) (->hydra_core_field "termDependencies" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_module_namespace) xs))) ((fn [v] (:term_dependencies v)) x))) (->hydra_core_field "typeDependencies" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_module_namespace) xs))) ((fn [v] (:type_dependencies v)) x))) (->hydra_core_field "description" ((fn [opt] (list :maybe ((hydra_lib_maybes_map (fn [x2] (list :literal (list :string x2)))) opt))) ((fn [v] (:description v)) x))))))))

(def hydra_encode_module_namespaces (fn [n] (fn [x] (list :record (->hydra_core_record "hydra.module.Namespaces" (list (->hydra_core_field "focus" ((fn [p] (list :pair (((hydra_lib_pairs_bimap hydra_encode_module_namespace) n) p))) ((fn [v] (:focus v)) x))) (->hydra_core_field "mapping" ((fn [m] (list :map (((hydra_lib_maps_bimap hydra_encode_module_namespace) n) m))) ((fn [v] (:mapping v)) x)))))))))

(def hydra_encode_module_qualified_name (fn [x] (list :record (->hydra_core_record "hydra.module.QualifiedName" (list (->hydra_core_field "namespace" ((fn [opt] (list :maybe ((hydra_lib_maybes_map hydra_encode_module_namespace) opt))) ((fn [v] (:namespace v)) x))) (->hydra_core_field "local" ((fn [x2] (list :literal (list :string x2))) ((fn [v] (:local v)) x))))))))
