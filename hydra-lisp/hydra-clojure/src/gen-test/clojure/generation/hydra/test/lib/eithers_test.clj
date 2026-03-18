;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.eithers primitives

(ns generation.hydra.test.lib.eithers-test
  (:require [clojure.test :refer :all]))

;; bind

(deftest test-bind-negbind-right-with-success

  (is (= (list :right 2)

         ((hydra_lib_eithers_bind (list :right "ab")) (fn [s] (if (hydra_lib_strings_null s) (list :left 0) (list :right (hydra_lib_strings_length s))))))))

(deftest test-bind-negbind-right-with-failure

  (is (= (list :left 0)

         ((hydra_lib_eithers_bind (list :right "")) (fn [s] (if (hydra_lib_strings_null s) (list :left 0) (list :right (hydra_lib_strings_length s))))))))

(deftest test-bind-negbind-left-returns-left-unchanged

  (is (= (list :left 42)

         ((hydra_lib_eithers_bind (list :left 42)) (fn [s] (if (hydra_lib_strings_null s) (list :left 0) (list :right (hydra_lib_strings_length s))))))))

;; bimap

(deftest test-bimap-negmap-left-value

  (is (= (list :left 10)

         (((hydra_lib_eithers_bimap (fn [x] ((hydra_lib_math_mul x) 2))) (fn [s] (hydra_lib_strings_length s))) (list :left 5)))))

(deftest test-bimap-negmap-right-value

  (is (= (list :right 2)

         (((hydra_lib_eithers_bimap (fn [x] ((hydra_lib_math_mul x) 2))) (fn [s] (hydra_lib_strings_length s))) (list :right "ab")))))

;; isLeft

(deftest test-isleft-negleft-value

  (is (= true

         (hydra_lib_eithers_is_left (list :left 42)))))

(deftest test-isleft-negright-value

  (is (= false

         (hydra_lib_eithers_is_left (list :right "test")))))

;; isRight

(deftest test-isright-negright-value

  (is (= true

         (hydra_lib_eithers_is_right (list :right "test")))))

(deftest test-isright-negleft-value

  (is (= false

         (hydra_lib_eithers_is_right (list :left 42)))))

;; fromLeft

(deftest test-fromleft-negextract-left

  (is (= 42

         ((hydra_lib_eithers_from_left 99) (list :left 42)))))

(deftest test-fromleft-neguse-default-for-right

  (is (= 99

         ((hydra_lib_eithers_from_left 99) (list :right "test")))))

;; fromRight

(deftest test-fromright-negextract-right

  (is (= "test"

         ((hydra_lib_eithers_from_right "default") (list :right "test")))))

(deftest test-fromright-neguse-default-for-left

  (is (= "default"

         ((hydra_lib_eithers_from_right "default") (list :left 42)))))

;; either

(deftest test-either-negapply-left-function

  (is (= 10

         (((hydra_lib_eithers_either (fn [x] ((hydra_lib_math_mul x) 2))) (fn [s] (hydra_lib_strings_length s))) (list :left 5)))))

(deftest test-either-negapply-right-function

  (is (= 2

         (((hydra_lib_eithers_either (fn [x] ((hydra_lib_math_mul x) 2))) (fn [s] (hydra_lib_strings_length s))) (list :right "ab")))))

;; lefts

(deftest test-lefts-negfilter-left-values

  (is (= (list 1 2)

         (hydra_lib_eithers_lefts (list (list :left 1) (list :right "a") (list :left 2) (list :right "b"))))))

(deftest test-lefts-negall-lefts

  (is (= (list 1 2)

         (hydra_lib_eithers_lefts (list (list :left 1) (list :left 2))))))

(deftest test-lefts-negall-rights

  (is (= (list )

         (hydra_lib_eithers_lefts (list (list :right "a") (list :right "b"))))))

(deftest test-lefts-negempty-list

  (is (= (list )

         (hydra_lib_eithers_lefts (list )))))

;; rights

(deftest test-rights-negfilter-right-values

  (is (= (list "a" "b")

         (hydra_lib_eithers_rights (list (list :left 1) (list :right "a") (list :left 2) (list :right "b"))))))

(deftest test-rights-negall-rights

  (is (= (list "a" "b")

         (hydra_lib_eithers_rights (list (list :right "a") (list :right "b"))))))

(deftest test-rights-negall-lefts

  (is (= (list )

         (hydra_lib_eithers_rights (list (list :left 1) (list :left 2))))))

(deftest test-rights-negempty-list

  (is (= (list )

         (hydra_lib_eithers_rights (list )))))

;; partitionEithers

(deftest test-partitioneithers-negpartition-mixed

  (is (= (list (list 1 2) (list "a" "b"))

         (hydra_lib_eithers_partition_eithers (list (list :left 1) (list :right "a") (list :left 2) (list :right "b"))))))

(deftest test-partitioneithers-negall-lefts

  (is (= (list (list 1 2) (list ))

         (hydra_lib_eithers_partition_eithers (list (list :left 1) (list :left 2))))))

(deftest test-partitioneithers-negall-rights

  (is (= (list (list ) (list "a" "b"))

         (hydra_lib_eithers_partition_eithers (list (list :right "a") (list :right "b"))))))

(deftest test-partitioneithers-negempty-list

  (is (= (list (list ) (list ))

         (hydra_lib_eithers_partition_eithers (list )))))

;; map

(deftest test-map-negmap-right-value

  (is (= (list :right 10)

         ((hydra_lib_eithers_map (fn [x] ((hydra_lib_math_mul x) 2))) (list :right 5)))))

(deftest test-map-negpreserve-left

  (is (= (list :left 99)

         ((hydra_lib_eithers_map (fn [x] ((hydra_lib_math_mul x) 2))) (list :left 99)))))

;; mapList

(deftest test-maplist-negall-succeed

  (is (= (list :right (list 2 4 6))

         ((hydra_lib_eithers_map_list (fn [x] (if ((hydra_lib_equality_equal x) 0) (list :left "zero") (list :right ((hydra_lib_math_mul x) 2))))) (list 1 2 3)))))

(deftest test-maplist-negfirst-fails

  (is (= (list :left "zero")

         ((hydra_lib_eithers_map_list (fn [x] (if ((hydra_lib_equality_equal x) 0) (list :left "zero") (list :right ((hydra_lib_math_mul x) 2))))) (list 1 0 3)))))

(deftest test-maplist-negempty-list

  (is (= (list :right (list ))

         ((hydra_lib_eithers_map_list (fn [x] (if ((hydra_lib_equality_equal x) 0) (list :left "zero") (list :right ((hydra_lib_math_mul x) 2))))) (list )))))

;; mapMaybe

(deftest test-mapmaybe-negjust-succeeds

  (is (= (list :right (list :just 10))

         ((hydra_lib_eithers_map_maybe (fn [x] (if ((hydra_lib_equality_equal x) 0) (list :left "zero") (list :right ((hydra_lib_math_mul x) 2))))) (list :just 5)))))

(deftest test-mapmaybe-negjust-fails

  (is (= (list :left "zero")

         ((hydra_lib_eithers_map_maybe (fn [x] (if ((hydra_lib_equality_equal x) 0) (list :left "zero") (list :right ((hydra_lib_math_mul x) 2))))) (list :just 0)))))

(deftest test-mapmaybe-negnothing

  (is (= (list :right (list :nothing))

         ((hydra_lib_eithers_map_maybe (fn [x] (if ((hydra_lib_equality_equal x) 0) (list :left "zero") (list :right ((hydra_lib_math_mul x) 2))))) (list :nothing)))))
