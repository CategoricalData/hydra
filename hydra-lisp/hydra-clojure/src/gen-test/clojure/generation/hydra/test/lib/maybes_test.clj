;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maybes primitives

(ns generation.hydra.test.lib.maybes-test
  (:require [clojure.test :refer :all]))

;; apply

(deftest test-maybes-negapply-negboth-just

  (is (= (list :just 8)

         ((hydra_lib_maybes_apply (list :just (hydra_lib_math_add 3))) (list :just 5)))))

(deftest test-maybes-negapply-negnothing-function

  (is (= (list :nothing)

         ((hydra_lib_maybes_apply (list :nothing)) (list :just 5)))))

(deftest test-maybes-negapply-negnothing-value

  (is (= (list :nothing)

         ((hydra_lib_maybes_apply (list :just (hydra_lib_math_add 3))) (list :nothing)))))

;; bind

(deftest test-maybes-negbind-negjust-to-just

  (is (= (list :just 10)

         ((hydra_lib_maybes_bind (list :just 5)) (fn [x] (list :just ((hydra_lib_math_mul x) 2)))))))

(deftest test-maybes-negbind-negnothing-to-nothing

  (is (= (list :nothing)

         ((hydra_lib_maybes_bind (list :nothing)) (fn [x] (list :just ((hydra_lib_math_mul x) 2)))))))

;; cases

(deftest test-maybes-negcases-negjust-applies-function

  (is (= 10

         (((hydra_lib_maybes_cases (list :just 5)) 0) (fn [x] ((hydra_lib_math_mul x) 2))))))

(deftest test-maybes-negcases-negnothing-returns-default

  (is (= 99

         (((hydra_lib_maybes_cases (list :nothing)) 99) (fn [x] ((hydra_lib_math_mul x) 2))))))

;; cat

(deftest test-maybes-negcat-negfilters-nothings

  (is (= (list 1 2)

         (hydra_lib_maybes_cat (list (list :just 1) (list :nothing) (list :just 2))))))

(deftest test-maybes-negcat-negall-justs

  (is (= (list 1 2)

         (hydra_lib_maybes_cat (list (list :just 1) (list :just 2))))))

(deftest test-maybes-negcat-negall-nothings

  (is (= (list )

         (hydra_lib_maybes_cat (list (list :nothing) (list :nothing))))))

(deftest test-maybes-negcat-negempty-list

  (is (= (list )

         (hydra_lib_maybes_cat (list )))))

;; compose

(deftest test-maybes-negcompose-negboth-succeed

  (is (= (list :just 12)

         (((hydra_lib_maybes_compose (fn [x] (if ((hydra_lib_equality_lte x) 5) (list :just ((hydra_lib_math_add x) 1)) (list :nothing)))) (fn [y] (if ((hydra_lib_equality_gte y) 5) (list :just ((hydra_lib_math_mul y) 2)) (list :nothing)))) 5))))

(deftest test-maybes-negcompose-negfirst-fails

  (is (= (list :nothing)

         (((hydra_lib_maybes_compose (fn [x] (if ((hydra_lib_equality_lte x) 5) (list :just ((hydra_lib_math_add x) 1)) (list :nothing)))) (fn [y] (if ((hydra_lib_equality_gte y) 5) (list :just ((hydra_lib_math_mul y) 2)) (list :nothing)))) 10))))

(deftest test-maybes-negcompose-negsecond-fails

  (is (= (list :nothing)

         (((hydra_lib_maybes_compose (fn [x] (if ((hydra_lib_equality_lte x) 5) (list :just ((hydra_lib_math_add x) 1)) (list :nothing)))) (fn [y] (if ((hydra_lib_equality_gte y) 5) (list :just ((hydra_lib_math_mul y) 2)) (list :nothing)))) 3))))

;; fromJust

(deftest test-maybes-negfromjust-negextract-from-just

  (is (= 42

         (hydra_lib_maybes_from_just (list :just 42)))))

;; fromMaybe

(deftest test-maybes-negfrommaybe-negjust-value

  (is (= 42

         ((hydra_lib_maybes_from_maybe 0) (list :just 42)))))

(deftest test-maybes-negfrommaybe-negnothing-with-default

  (is (= 99

         ((hydra_lib_maybes_from_maybe 99) (list :nothing)))))

;; isJust

(deftest test-maybes-negisjust-negjust-value

  (is (= true

         (hydra_lib_maybes_is_just (list :just 42)))))

(deftest test-maybes-negisjust-negnothing

  (is (= false

         (hydra_lib_maybes_is_just (list :nothing)))))

;; isNothing

(deftest test-maybes-negisnothing-negjust-value

  (is (= false

         (hydra_lib_maybes_is_nothing (list :just 42)))))

(deftest test-maybes-negisnothing-negnothing

  (is (= true

         (hydra_lib_maybes_is_nothing (list :nothing)))))

;; map

(deftest test-maybes-negmap-negmaps-just-value

  (is (= (list :just 10)

         ((hydra_lib_maybes_map (fn [x] ((hydra_lib_math_mul x) 2))) (list :just 5)))))

(deftest test-maybes-negmap-negnothing-unchanged

  (is (= (list :nothing)

         ((hydra_lib_maybes_map (fn [x] ((hydra_lib_math_mul x) 2))) (list :nothing)))))

;; mapMaybe

(deftest test-maybes-negmapmaybe-negfilter-and-transform

  (is (= (list 6 8 10)

         ((hydra_lib_maybes_map_maybe (fn [x] (if ((hydra_lib_equality_gt x) 2) (list :just ((hydra_lib_math_mul x) 2)) (list :nothing)))) (list 1 2 3 4 5)))))

(deftest test-maybes-negmapmaybe-negempty-result

  (is (= (list )

         ((hydra_lib_maybes_map_maybe (fn [x] (if ((hydra_lib_equality_gt x) 2) (list :just ((hydra_lib_math_mul x) 2)) (list :nothing)))) (list 1 2)))))

(deftest test-maybes-negmapmaybe-negempty-input

  (is (= (list )

         ((hydra_lib_maybes_map_maybe (fn [x] (if ((hydra_lib_equality_gt x) 2) (list :just ((hydra_lib_math_mul x) 2)) (list :nothing)))) (list )))))

;; maybe

(deftest test-maybes-negmaybe-negjust-value-applies-function

  (is (= 10

         (((hydra_lib_maybes_maybe 0) (fn [x] ((hydra_lib_math_mul x) 2))) (list :just 5)))))

(deftest test-maybes-negmaybe-negnothing-returns-default

  (is (= 99

         (((hydra_lib_maybes_maybe 99) (fn [x] ((hydra_lib_math_mul x) 2))) (list :nothing)))))

;; pure

(deftest test-maybes-negpure-negwraps-integer

  (is (= (list :just 42)

         (hydra_lib_maybes_pure 42))))

(deftest test-maybes-negpure-negwraps-string

  (is (= (list :just "hello")

         (hydra_lib_maybes_pure "hello"))))

;; toList

(deftest test-maybes-negtolist-negjust-value

  (is (= (list 42)

         (hydra_lib_maybes_to_list (list :just 42)))))

(deftest test-maybes-negtolist-negnothing

  (is (= (list )

         (hydra_lib_maybes_to_list (list :nothing)))))
