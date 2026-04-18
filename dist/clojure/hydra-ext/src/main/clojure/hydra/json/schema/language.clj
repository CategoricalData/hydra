(ns hydra.json.schema.language
  (:require [hydra.coders :refer :all] [hydra.core :refer :all] [hydra.lib.sets :refer :all] [hydra.reflect :refer :all] [hydra.variants :refer :all]
))

(declare hydra_json_schema_language_json_schema_language)

(def hydra_json_schema_language_json_schema_language (let [elimination_variants hydra_lib_sets_empty float_types (hydra_lib_sets_from_list (list (list :bigfloat nil))) function_variants hydra_lib_sets_empty integer_types (hydra_lib_sets_from_list (list (list :bigint nil))) literal_variants (hydra_lib_sets_from_list (list (list :boolean nil) (list :float nil) (list :integer nil) (list :string nil))) term_variants (hydra_lib_sets_from_list hydra_reflect_term_variants) type_predicate (fn [_] true) type_variants (hydra_lib_sets_from_list (list (list :annotated nil) (list :either nil) (list :list nil) (list :literal nil) (list :map nil) (list :maybe nil) (list :pair nil) (list :record nil) (list :set nil) (list :union nil) (list :variable nil) (list :wrap nil)))] (->hydra_coders_language "hydra.json.schema" (->hydra_coders_language_constraints elimination_variants literal_variants float_types function_variants integer_types term_variants type_variants type_predicate))))
