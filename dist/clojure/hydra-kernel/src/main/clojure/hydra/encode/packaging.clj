(ns hydra.encode.packaging
  (:require [hydra.core :refer :all] [hydra.encode.core :refer :all] [hydra.lib.lists :refer :all] [hydra.lib.maps :refer :all] [hydra.lib.maybes :refer :all] [hydra.lib.pairs :refer :all] [hydra.packaging :refer :all]
))

(declare hydra_encode_packaging_term_definition hydra_encode_packaging_type_definition hydra_encode_packaging_definition hydra_encode_packaging_file_extension hydra_encode_packaging_namespace hydra_encode_packaging_module hydra_encode_packaging_namespaces hydra_encode_packaging_package_name hydra_encode_packaging_package hydra_encode_packaging_qualified_name)

(def hydra_encode_packaging_term_definition (fn [x] (list :record (->hydra_core_record "hydra.packaging.TermDefinition" (list (->hydra_core_field "name" (hydra_encode_core_name ((fn [v] (:name v)) x))) (->hydra_core_field "term" (hydra_encode_core_term ((fn [v] (:term v)) x))) (->hydra_core_field "type" ((fn [opt] (list :maybe ((hydra_lib_maybes_map hydra_encode_core_type_scheme) opt))) ((fn [v] (:type v)) x))))))))

(def hydra_encode_packaging_type_definition (fn [x] (list :record (->hydra_core_record "hydra.packaging.TypeDefinition" (list (->hydra_core_field "name" (hydra_encode_core_name ((fn [v] (:name v)) x))) (->hydra_core_field "type" (hydra_encode_core_type_scheme ((fn [v] (:type v)) x))))))))

(def hydra_encode_packaging_definition (fn [match_target] ((fn [match_value] (cond (= (first match_target) :term) ((fn [y] (list :inject (->hydra_core_injection "hydra.packaging.Definition" (->hydra_core_field "term" (hydra_encode_packaging_term_definition y))))) match_value) (= (first match_target) :type) ((fn [y] (list :inject (->hydra_core_injection "hydra.packaging.Definition" (->hydra_core_field "type" (hydra_encode_packaging_type_definition y))))) match_value))) (second match_target))))

(def hydra_encode_packaging_file_extension (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.packaging.FileExtension" ((fn [x2] (list :literal (list :string x2))) ((fn [v] v) x))))))

(def hydra_encode_packaging_namespace (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.packaging.Namespace" ((fn [x2] (list :literal (list :string x2))) ((fn [v] v) x))))))

(def hydra_encode_packaging_module (fn [x] (list :record (->hydra_core_record "hydra.packaging.Module" (list (->hydra_core_field "namespace" (hydra_encode_packaging_namespace ((fn [v] (:namespace v)) x))) (->hydra_core_field "definitions" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_packaging_definition) xs))) ((fn [v] (:definitions v)) x))) (->hydra_core_field "termDependencies" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_packaging_namespace) xs))) ((fn [v] (:term_dependencies v)) x))) (->hydra_core_field "typeDependencies" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_packaging_namespace) xs))) ((fn [v] (:type_dependencies v)) x))) (->hydra_core_field "description" ((fn [opt] (list :maybe ((hydra_lib_maybes_map (fn [x2] (list :literal (list :string x2)))) opt))) ((fn [v] (:description v)) x))))))))

(def hydra_encode_packaging_namespaces (fn [n] (fn [x] (list :record (->hydra_core_record "hydra.packaging.Namespaces" (list (->hydra_core_field "focus" ((fn [p] (list :pair (((hydra_lib_pairs_bimap hydra_encode_packaging_namespace) n) p))) ((fn [v] (:focus v)) x))) (->hydra_core_field "mapping" ((fn [m] (list :map (((hydra_lib_maps_bimap hydra_encode_packaging_namespace) n) m))) ((fn [v] (:mapping v)) x)))))))))

(def hydra_encode_packaging_package_name (fn [x] (list :wrap (->hydra_core_wrapped_term "hydra.packaging.PackageName" ((fn [x2] (list :literal (list :string x2))) ((fn [v] v) x))))))

(def hydra_encode_packaging_package (fn [x] (list :record (->hydra_core_record "hydra.packaging.Package" (list (->hydra_core_field "name" (hydra_encode_packaging_package_name ((fn [v] (:name v)) x))) (->hydra_core_field "modules" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_packaging_module) xs))) ((fn [v] (:modules v)) x))) (->hydra_core_field "dependencies" ((fn [xs] (list :list ((hydra_lib_lists_map hydra_encode_packaging_package_name) xs))) ((fn [v] (:dependencies v)) x))) (->hydra_core_field "description" ((fn [opt] (list :maybe ((hydra_lib_maybes_map (fn [x2] (list :literal (list :string x2)))) opt))) ((fn [v] (:description v)) x))))))))

(def hydra_encode_packaging_qualified_name (fn [x] (list :record (->hydra_core_record "hydra.packaging.QualifiedName" (list (->hydra_core_field "namespace" ((fn [opt] (list :maybe ((hydra_lib_maybes_map hydra_encode_packaging_namespace) opt))) ((fn [v] (:namespace v)) x))) (->hydra_core_field "local" ((fn [x2] (list :literal (list :string x2))) ((fn [v] (:local v)) x))))))))
