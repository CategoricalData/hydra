;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maybes primitives

(ns generation.hydra.test.lib.maybes-test
  (:require [clojure.test :refer :all]))

;; apply

(deftest test-apply-negboth-just

  (is (= 8

         ((hydra_lib_maybes_apply (hydra_lib_math_add 3)) 5))))

(deftest test-apply-negnothing-function

  (is (= nil

         ((hydra_lib_maybes_apply nil) 5))))

(deftest test-apply-negnothing-value

  (is (= nil

         ((hydra_lib_maybes_apply (hydra_lib_math_add 3)) nil))))

;; bind

(deftest test-bind-negjust-to-just

  (is (= 10

         ((hydra_lib_maybes_bind 5) (fn [x] ((hydra_lib_math_mul x) 2))))))

(deftest test-bind-negnothing-to-nothing

  (is (= nil

         ((hydra_lib_maybes_bind nil) (fn [x] ((hydra_lib_math_mul x) 2))))))

;; cases

(deftest test-cases-negjust-applies-function

  (is (= 10

         (((hydra_lib_maybes_cases 5) 0) (fn [x] ((hydra_lib_math_mul x) 2))))))

(deftest test-cases-negnothing-returns-default

  (is (= 99

         (((hydra_lib_maybes_cases nil) 99) (fn [x] ((hydra_lib_math_mul x) 2))))))

;; cat

(deftest test-cat-negfilters-nothings

  (is (= (list 1 2)

         (hydra_lib_maybes_cat (list 1 nil 2)))))

(deftest test-cat-negall-justs

  (is (= (list 1 2)

         (hydra_lib_maybes_cat (list 1 2)))))

(deftest test-cat-negall-nothings

  (is (= (list )

         (hydra_lib_maybes_cat (list nil nil)))))

(deftest test-cat-negempty-list

  (is (= (list )

         (hydra_lib_maybes_cat (list )))))

;; compose

(deftest test-compose-negboth-succeed

  (is (= 12

         (((hydra_lib_maybes_compose (fn [x] (if ((hydra_lib_equality_lte x) 5) ((hydra_lib_math_add x) 1) nil))) (fn [y] (if ((hydra_lib_equality_gte y) 5) ((hydra_lib_math_mul y) 2) nil))) 5))))

(deftest test-compose-negfirst-fails

  (is (= nil

         (((hydra_lib_maybes_compose (fn [x] (if ((hydra_lib_equality_lte x) 5) ((hydra_lib_math_add x) 1) nil))) (fn [y] (if ((hydra_lib_equality_gte y) 5) ((hydra_lib_math_mul y) 2) nil))) 10))))

(deftest test-compose-negsecond-fails

  (is (= nil

         (((hydra_lib_maybes_compose (fn [x] (if ((hydra_lib_equality_lte x) 5) ((hydra_lib_math_add x) 1) nil))) (fn [y] (if ((hydra_lib_equality_gte y) 5) ((hydra_lib_math_mul y) 2) nil))) 3))))

;; fromJust

(deftest test-fromjust-negextract-from-just

  (is (= 42

         (hydra_lib_maybes_from_just 42))))

;; fromMaybe

(deftest test-frommaybe-negjust-value

  (is (= 42

         ((hydra_lib_maybes_from_maybe 0) 42))))

(deftest test-frommaybe-negnothing-with-default

  (is (= 99

         ((hydra_lib_maybes_from_maybe 99) nil))))

;; isJust

(deftest test-isjust-negjust-value

  (is (= true

         (hydra_lib_maybes_is_just 42))))

(deftest test-isjust-negnothing

  (is (= false

         (hydra_lib_maybes_is_just nil))))

;; isNothing

(deftest test-isnothing-negjust-value

  (is (= false

         (hydra_lib_maybes_is_nothing 42))))

(deftest test-isnothing-negnothing

  (is (= true

         (hydra_lib_maybes_is_nothing nil))))

;; map

(deftest test-map-negmaps-just-value

  (is (= 10

         ((hydra_lib_maybes_map (fn [x] ((hydra_lib_math_mul x) 2))) 5))))

(deftest test-map-negnothing-unchanged

  (is (= nil

         ((hydra_lib_maybes_map (fn [x] ((hydra_lib_math_mul x) 2))) nil))))

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

         (((hydra_lib_maybes_maybe 0) (fn [x] ((hydra_lib_math_mul x) 2))) 5))))

(deftest test-maybe-negnothing-returns-default

  (is (= 99

         (((hydra_lib_maybes_maybe 99) (fn [x] ((hydra_lib_math_mul x) 2))) nil))))

;; pure

(deftest test-pure-negwraps-integer

  (is (= 42

         (hydra_lib_maybes_pure 42))))

(deftest test-pure-negwraps-string

  (is (= "hello"

         (hydra_lib_maybes_pure "hello"))))

;; toList

(deftest test-tolist-negjust-value

  (is (= (list 42)

         (hydra_lib_maybes_to_list 42))))

(deftest test-tolist-negnothing

  (is (= (list )

         (hydra_lib_maybes_to_list nil))))
