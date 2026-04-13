(ns hydra.testing)

(declare hydra_testing_test_case-variants)

(defrecord hydra_testing_tag [value])
(defn make-hydra_testing_tag [value] (->hydra_testing_tag value))

(def hydra_testing_test_case-variants (list :universal))

(defrecord hydra_testing_test_case_with_metadata [name case description tags])
(defn make-hydra_testing_test_case_with_metadata [name case description tags] (->hydra_testing_test_case_with_metadata name case description tags))

(defrecord hydra_testing_test_group [name description subgroups cases])
(defn make-hydra_testing_test_group [name description subgroups cases] (->hydra_testing_test_group name description subgroups cases))

(defrecord hydra_testing_universal_test_case [actual expected])
(defn make-hydra_testing_universal_test_case [actual expected] (->hydra_testing_universal_test_case actual expected))
