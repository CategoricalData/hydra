;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maybes primitives

(ns generation.hydra.test.lib.maybes-test
  (:require [clojure.test :refer :all]))

;; apply

(deftest test-apply-negboth-just

  (is (= (list :just 8)

         ((hydra_lib_maybes_apply (list :just (hydra_lib_math_add 3))) (list :just 5)))))

(deftest test-apply-negnothing-function

  (is (= (list :nothing)

         ((hydra_lib_maybes_apply (list :nothing)) (list :just 5)))))

(deftest test-apply-negnothing-value

  (is (= (list :nothing)

         ((hydra_lib_maybes_apply (list :just (hydra_lib_math_add 3))) (list :nothing)))))

;; bind

(deftest test-bind-negjust-to-just

  (is (= (list :just 10)

         ((hydra_lib_maybes_bind (list :just 5)) (fn [x] ((hydra_lib_math_mul x) 2))))))

(deftest test-bind-negnothing-to-nothing

  (is (= (list :nothing)

         ((hydra_lib_maybes_bind (list :nothing)) (fn [x] ((hydra_lib_math_mul x) 2))))))

;; cases

(deftest test-cases-negjust-applies-function

  (is (= 10

         (((hydra_lib_maybes_cases (list :just 5)) 0) (fn [x] ((hydra_lib_math_mul x) 2))))))

(deftest test-cases-negnothing-returns-default

  (is (= 99

         (((hydra_lib_maybes_cases (list :nothing)) 99) (fn [x] ((hydra_lib_math_mul x) 2))))))

;; cat

(deftest test-cat-negfilters-nothings

  (is (= (list 1 2)

         (hydra_lib_maybes_cat (list (list :just 1) (list :nothing) (list :just 2))))))

(deftest test-cat-negall-justs

  (is (= (list 1 2)

         (hydra_lib_maybes_cat (list (list :just 1) (list :just 2))))))

(deftest test-cat-negall-nothings

  (is (= (list )

         (hydra_lib_maybes_cat (list (list :nothing) (list :nothing))))))

(deftest test-cat-negempty-list

  (is (= (list )

         (hydra_lib_maybes_cat (list )))))

;; compose

(deftest test-compose-negboth-succeed

  (is (= (list :just 12)

         (((hydra_lib_maybes_compose (fn [x] (if ((hydra_lib_equality_lte x) 5) ((hydra_lib_math_add x) 1) nil))) (fn [y] (if ((hydra_lib_equality_gte y) 5) ((hydra_lib_math_mul y) 2) nil))) 5))))

(deftest test-compose-negfirst-fails

  (is (= (list :nothing)

         (((hydra_lib_maybes_compose (fn [x] (if ((hydra_lib_equality_lte x) 5) ((hydra_lib_math_add x) 1) nil))) (fn [y] (if ((hydra_lib_equality_gte y) 5) ((hydra_lib_math_mul y) 2) nil))) 10))))

(deftest test-compose-negsecond-fails

  (is (= (list :nothing)

         (((hydra_lib_maybes_compose (fn [x] (if ((hydra_lib_equality_lte x) 5) ((hydra_lib_math_add x) 1) nil))) (fn [y] (if ((hydra_lib_equality_gte y) 5) ((hydra_lib_math_mul y) 2) nil))) 3))))

;; fromJust

(deftest test-fromjust-negextract-from-just

  (is (= 42

         (hydra_lib_maybes_from_just (list :just 42)))))

;; fromMaybe

(deftest test-frommaybe-negjust-value

  (is (= 42

         ((hydra_lib_maybes_from_maybe 0) (list :just 42)))))

(deftest test-frommaybe-negnothing-with-default

  (is (= 99

         ((hydra_lib_maybes_from_maybe 99) (list :nothing)))))

;; isJust

(deftest test-isjust-negjust-value

  (is (= true

         (hydra_lib_maybes_is_just (list :just 42)))))

(deftest test-isjust-negnothing

  (is (= false

         (hydra_lib_maybes_is_just (list :nothing)))))

;; isNothing

(deftest test-isnothing-negjust-value

  (is (= false

         (hydra_lib_maybes_is_nothing (list :just 42)))))

(deftest test-isnothing-negnothing

  (is (= true

         (hydra_lib_maybes_is_nothing (list :nothing)))))

;; map

(deftest test-map-negmaps-just-value

  (is (= (list :just 10)

         ((hydra_lib_maybes_map (fn [x] ((hydra_lib_math_mul x) 2))) (list :just 5)))))

(deftest test-map-negnothing-unchanged

  (is (= (list :nothing)

         ((hydra_lib_maybes_map (fn [x] ((hydra_lib_math_mul x) 2))) (list :nothing)))))

;; mapMaybe

(deftest test-mapmaybe-negfilter-and-transform

  (is (= (list 6 8 10)

         ((hydra_lib_maybes_map_maybe (fn [x] (if ((hydra_lib_equality_gt x) 2) ((hydra_lib_math_mul x) 2) nil))) (list 1 2 3 4 5)))))

(deftest test-mapmaybe-negempty-result

  (is (= (list )

         ((hydra_lib_maybes_map_maybe (fn [x] (if ((hydra_lib_equality_gt x) 2) ((hydra_lib_math_mul x) 2) nil))) (list 1 2)))))

(deftest test-mapmaybe-negempty-input

  (is (= (list )

         ((hydra_lib_maybes_map_maybe (fn [x] (if ((hydra_lib_equality_gt x) 2) ((hydra_lib_math_mul x) 2) nil))) (list )))))

;; maybe

(deftest test-maybe-negjust-value-applies-function

  (is (= 10

         (((hydra_lib_maybes_maybe 0) (fn [x] ((hydra_lib_math_mul x) 2))) (list :just 5)))))

(deftest test-maybe-negnothing-returns-default

  (is (= 99

         (((hydra_lib_maybes_maybe 99) (fn [x] ((hydra_lib_math_mul x) 2))) (list :nothing)))))

;; pure

(deftest test-pure-negwraps-integer

  (is (= (list :just 42)

         (hydra_lib_maybes_pure 42))))

(deftest test-pure-negwraps-string

  (is (= (list :just "hello")

         (hydra_lib_maybes_pure "hello"))))

;; toList

(deftest test-tolist-negjust-value

  (is (= (list 42)

         (hydra_lib_maybes_to_list (list :just 42)))))

(deftest test-tolist-negnothing

  (is (= (list )

         (hydra_lib_maybes_to_list (list :nothing)))))
