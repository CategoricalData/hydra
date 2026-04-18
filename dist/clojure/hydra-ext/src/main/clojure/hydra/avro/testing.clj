(ns hydra.avro.testing
  (:require [hydra.avro.schema :refer :all] [hydra.core :refer :all] [hydra.json.model :refer :all]
))

(declare hydra_avro_testing_avro_test_case-variants)

(defrecord hydra_avro_testing_type_level_forward_test_case [description schema type])
(defn make-hydra_avro_testing_type_level_forward_test_case [description schema type] (->hydra_avro_testing_type_level_forward_test_case description schema type))

(defrecord hydra_avro_testing_type_level_reverse_test_case [description type schema])
(defn make-hydra_avro_testing_type_level_reverse_test_case [description type schema] (->hydra_avro_testing_type_level_reverse_test_case description type schema))

(defrecord hydra_avro_testing_type_level_round_trip_avro_test_case [description schema expected_schema])
(defn make-hydra_avro_testing_type_level_round_trip_avro_test_case [description schema expected_schema] (->hydra_avro_testing_type_level_round_trip_avro_test_case description schema expected_schema))

(defrecord hydra_avro_testing_type_level_round_trip_hydra_test_case [description type expected_type])
(defn make-hydra_avro_testing_type_level_round_trip_hydra_test_case [description type expected_type] (->hydra_avro_testing_type_level_round_trip_hydra_test_case description type expected_type))

(defrecord hydra_avro_testing_term_level_forward_test_case [description schema json term])
(defn make-hydra_avro_testing_term_level_forward_test_case [description schema json term] (->hydra_avro_testing_term_level_forward_test_case description schema json term))

(defrecord hydra_avro_testing_term_level_reverse_test_case [description schema term json])
(defn make-hydra_avro_testing_term_level_reverse_test_case [description schema term json] (->hydra_avro_testing_term_level_reverse_test_case description schema term json))

(defrecord hydra_avro_testing_term_level_round_trip_json_test_case [description schema json expected_json])
(defn make-hydra_avro_testing_term_level_round_trip_json_test_case [description schema json expected_json] (->hydra_avro_testing_term_level_round_trip_json_test_case description schema json expected_json))

(defrecord hydra_avro_testing_term_level_round_trip_term_test_case [description type term expected_term])
(defn make-hydra_avro_testing_term_level_round_trip_term_test_case [description type term expected_term] (->hydra_avro_testing_term_level_round_trip_term_test_case description type term expected_term))

(defrecord hydra_avro_testing_union_test_case [description hydra_type avro_schema term_pairs])
(defn make-hydra_avro_testing_union_test_case [description hydra_type avro_schema term_pairs] (->hydra_avro_testing_union_test_case description hydra_type avro_schema term_pairs))

(defrecord hydra_avro_testing_name_mapping_test_case [description hydra_name avro_name avro_namespace])
(defn make-hydra_avro_testing_name_mapping_test_case [description hydra_name avro_name avro_namespace] (->hydra_avro_testing_name_mapping_test_case description hydra_name avro_name avro_namespace))

(defrecord hydra_avro_testing_lossiness_test_case [description original_schema hydra_type recovered_schema is_lossy])
(defn make-hydra_avro_testing_lossiness_test_case [description original_schema hydra_type recovered_schema is_lossy] (->hydra_avro_testing_lossiness_test_case description original_schema hydra_type recovered_schema is_lossy))

(defrecord hydra_avro_testing_schema_serialization_test_case [description schema json])
(defn make-hydra_avro_testing_schema_serialization_test_case [description schema json] (->hydra_avro_testing_schema_serialization_test_case description schema json))

(def hydra_avro_testing_avro_test_case-variants (list :type_level_forward :type_level_reverse :type_level_round_trip_avro :type_level_round_trip_hydra :term_level_forward :term_level_reverse :term_level_round_trip_json :term_level_round_trip_term :union :name_mapping :lossiness :schema_serialization))
