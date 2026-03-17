(defpackage :hydra.test.testTerms
(:use :cl :hydra.core :hydra.test.testTypes)
(:export :hydra_test_test_terms_latlon_record :hydra_test_test_terms_test_data_arthur :hydra_test_test_terms_test_element_arthur :hydra_test_test_terms_test_element_first_name))

(in-package :hydra.test.testTerms)

(cl:defvar hydra_test_test_terms_latlon_record (cl:lambda (lat) (cl:lambda (lon) (list :record (make-hydra_core_record hydra_test_test_types_test_type_lat_lon_name (cl:list (make-hydra_core_field "lat" (list :literal (list :float (list :float32 lat)))) (make-hydra_core_field "lon" (list :literal (list :float (list :float32 lon))))))))))

(cl:defvar hydra_test_test_terms_test_data_arthur (list :record (make-hydra_core_record hydra_test_test_types_test_type_person_name (cl:list (make-hydra_core_field "firstName" (list :literal (list :string "Arthur"))) (make-hydra_core_field "lastName" (list :literal (list :string "Dent"))) (make-hydra_core_field "age" (list :literal (list :integer (list :int32 42))))))))

(cl:defvar hydra_test_test_terms_test_element_arthur (make-hydra_core_binding "firstName" hydra_test_test_terms_test_data_arthur (make-hydra_core_type_scheme (cl:list) (list :variable hydra_test_test_types_test_type_person_name) cl:nil)))

(cl:defvar hydra_test_test_terms_test_element_first_name (make-hydra_core_binding "firstName" (list :function (list :elimination (list :record (make-hydra_core_projection hydra_test_test_types_test_type_person_name "firstName")))) (make-hydra_core_type_scheme (cl:list) (list :function (make-hydra_core_function_type (list :variable hydra_test_test_types_test_type_person_name) (list :literal (list :string cl:nil)))) cl:nil)))
