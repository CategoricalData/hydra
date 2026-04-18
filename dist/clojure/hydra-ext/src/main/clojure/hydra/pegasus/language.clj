(ns hydra.pegasus.language
  (:require [hydra.coders :refer :all] [hydra.core :refer :all] [hydra.lib.sets :refer :all] [hydra.variants :refer :all]
))

(declare hydra_pegasus_language_pdl_language)

(def hydra_pegasus_language_pdl_language (let [elimination_variants hydra_lib_sets_empty float_types (hydra_lib_sets_from_list (list (list :float32 nil) (list :float64 nil))) function_variants hydra_lib_sets_empty integer_types (hydra_lib_sets_from_list (list (list :int32 nil) (list :int64 nil))) literal_variants (hydra_lib_sets_from_list (list (list :binary nil) (list :boolean nil) (list :float nil) (list :integer nil) (list :string nil))) term_variants (hydra_lib_sets_from_list (list (list :either nil) (list :list nil) (list :literal nil) (list :map nil) (list :pair nil) (list :set nil) (list :wrap nil) (list :maybe nil) (list :record nil) (list :inject nil))) type_predicate (fn [_] true) type_variants (hydra_lib_sets_from_list (list (list :annotated nil) (list :either nil) (list :list nil) (list :literal nil) (list :map nil) (list :pair nil) (list :set nil) (list :wrap nil) (list :maybe nil) (list :record nil) (list :union nil) (list :variable nil)))] (->hydra_coders_language "hydra.pegasus.pdl" (->hydra_coders_language_constraints elimination_variants literal_variants float_types function_variants integer_types term_variants type_variants type_predicate))))
