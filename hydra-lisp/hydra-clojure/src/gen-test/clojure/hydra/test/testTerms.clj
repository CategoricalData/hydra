(ns hydra.test.testTerms
  (:require [hydra.core :refer :all] [hydra.test.testTypes :refer :all]
))

(declare hydra_test_test_terms_latlon_record hydra_test_test_terms_test_data_arthur hydra_test_test_terms_test_element_arthur hydra_test_test_terms_test_element_first_name)

(def hydra_test_test_terms_latlon_record (fn [lat] (fn [lon] (list :record (->hydra_core_record hydra_test_test_types_test_type_lat_lon_name (list (->hydra_core_field "lat" (list :literal (list :float (list :float32 lat)))) (->hydra_core_field "lon" (list :literal (list :float (list :float32 lon))))))))))

(def hydra_test_test_terms_test_data_arthur (list :record (->hydra_core_record hydra_test_test_types_test_type_person_name (list (->hydra_core_field "firstName" (list :literal (list :string "Arthur"))) (->hydra_core_field "lastName" (list :literal (list :string "Dent"))) (->hydra_core_field "age" (list :literal (list :integer (list :int32 42))))))))

(def hydra_test_test_terms_test_element_arthur (->hydra_core_binding "firstName" hydra_test_test_terms_test_data_arthur (list :just (->hydra_core_type_scheme (list) (list :variable hydra_test_test_types_test_type_person_name) (list :nothing)))))

(def hydra_test_test_terms_test_element_first_name (->hydra_core_binding "firstName" (list :function (list :elimination (list :record (->hydra_core_projection hydra_test_test_types_test_type_person_name "firstName")))) (list :just (->hydra_core_type_scheme (list) (list :function (->hydra_core_function_type (list :variable hydra_test_test_types_test_type_person_name) (list :literal (list :string nil)))) (list :nothing)))))
