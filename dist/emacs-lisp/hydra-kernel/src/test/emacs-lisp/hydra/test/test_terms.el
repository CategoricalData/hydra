(require 'cl-lib)

(require 'hydra.core)

(require 'hydra.test.testTypes)

(defvar hydra_test_test_terms_latlon_record (lambda (lat) (lambda (lon) (list :record (make-hydra_core_record hydra_test_test_types_test_type_lat_lon_name (list (make-hydra_core_field "lat" (list :literal (list :float (list :float32 lat)))) (make-hydra_core_field "lon" (list :literal (list :float (list :float32 lon))))))))))

(defvar hydra_test_test_terms_test_data_arthur (list :record (make-hydra_core_record hydra_test_test_types_test_type_person_name (list (make-hydra_core_field "firstName" (list :literal (list :string "Arthur"))) (make-hydra_core_field "lastName" (list :literal (list :string "Dent"))) (make-hydra_core_field "age" (list :literal (list :integer (list :int32 42))))))))

(defvar hydra_test_test_terms_test_element_arthur (make-hydra_core_binding "firstName" hydra_test_test_terms_test_data_arthur (list :just (make-hydra_core_type_scheme (list) (list :variable hydra_test_test_types_test_type_person_name) (list :nothing)))))

(defvar hydra_test_test_terms_test_element_first_name (make-hydra_core_binding "firstName" (list :project (make-hydra_core_projection hydra_test_test_types_test_type_person_name "firstName")) (list :just (make-hydra_core_type_scheme (list) (list :function (make-hydra_core_function_type (list :variable hydra_test_test_types_test_type_person_name) (list :literal (list :string nil)))) (list :nothing)))))

(provide 'hydra.test.testTerms)
