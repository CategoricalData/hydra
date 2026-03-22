(ns hydra.encode.error.core
  (:require [hydra.core :refer :all] [hydra.encode.accessors :refer :all] [hydra.encode.core :refer :all] [hydra.encode.variants :refer :all] [hydra.error.core :refer :all]
))

(declare hydra_encode_error_core_duplicate_binding_error hydra_encode_error_core_duplicate_field_error hydra_encode_error_core_invalid_term_error hydra_encode_error_core_undefined_field_error hydra_encode_error_core_undefined_term_error hydra_encode_error_core_undefined_type_error hydra_encode_error_core_unexpected_term_variant_error hydra_encode_error_core_unexpected_type_variant_error)

(def hydra_encode_error_core_duplicate_binding_error (fn [x] (list :record (->hydra_core_record "hydra.error.core.DuplicateBindingError" (list (->hydra_core_field "location" (hydra_encode_accessors_accessor_path ((fn [v] (:location v)) x))) (->hydra_core_field "name" (hydra_encode_core_name ((fn [v] (:name v)) x))))))))

(def hydra_encode_error_core_duplicate_field_error (fn [x] (list :record (->hydra_core_record "hydra.error.core.DuplicateFieldError" (list (->hydra_core_field "location" (hydra_encode_accessors_accessor_path ((fn [v] (:location v)) x))) (->hydra_core_field "name" (hydra_encode_core_name ((fn [v] (:name v)) x))))))))

(def hydra_encode_error_core_invalid_term_error (fn [match_target] ((fn [match_value] (cond (= (first match_target) :duplicate_binding) ((fn [y] (list :union (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "duplicateBinding" (hydra_encode_error_core_duplicate_binding_error y))))) match_value) (= (first match_target) :duplicate_field) ((fn [y] (list :union (->hydra_core_injection "hydra.error.core.InvalidTermError" (->hydra_core_field "duplicateField" (hydra_encode_error_core_duplicate_field_error y))))) match_value))) (second match_target))))

(def hydra_encode_error_core_undefined_field_error (fn [x] (list :record (->hydra_core_record "hydra.error.core.UndefinedFieldError" (list (->hydra_core_field "fieldName" (hydra_encode_core_name ((fn [v] (:field_name v)) x))) (->hydra_core_field "typeName" (hydra_encode_core_name ((fn [v] (:type_name v)) x))))))))

(def hydra_encode_error_core_undefined_term_error (fn [x] (list :record (->hydra_core_record "hydra.error.core.UndefinedTermError" (list (->hydra_core_field "name" (hydra_encode_core_name ((fn [v] (:name v)) x))))))))

(def hydra_encode_error_core_undefined_type_error (fn [x] (list :record (->hydra_core_record "hydra.error.core.UndefinedTypeError" (list (->hydra_core_field "name" (hydra_encode_core_name ((fn [v] (:name v)) x))))))))

(def hydra_encode_error_core_unexpected_term_variant_error (fn [x] (list :record (->hydra_core_record "hydra.error.core.UnexpectedTermVariantError" (list (->hydra_core_field "expectedVariant" (hydra_encode_variants_term_variant ((fn [v] (:expected_variant v)) x))) (->hydra_core_field "actualTerm" (hydra_encode_core_term ((fn [v] (:actual_term v)) x))))))))

(def hydra_encode_error_core_unexpected_type_variant_error (fn [x] (list :record (->hydra_core_record "hydra.error.core.UnexpectedTypeVariantError" (list (->hydra_core_field "expectedVariant" (hydra_encode_variants_type_variant ((fn [v] (:expected_variant v)) x))) (->hydra_core_field "actualType" (hydra_encode_core_type ((fn [v] (:actual_type v)) x))))))))
