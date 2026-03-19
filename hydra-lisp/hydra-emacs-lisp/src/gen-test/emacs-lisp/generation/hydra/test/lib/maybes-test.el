;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.maybes primitives

(require 'ert)

;; apply

(ert-deftest test-maybes-negapply-negboth-just ()

  (should (equal (list :just 8) (funcall (funcall hydra_lib_maybes_apply (list :just (funcall hydra_lib_math_add 3))) (list :just 5)))))

(ert-deftest test-maybes-negapply-negnothing-function ()

  (should (equal (list :nothing) (funcall (funcall hydra_lib_maybes_apply (list :nothing)) (list :just 5)))))

(ert-deftest test-maybes-negapply-negnothing-value ()

  (should (equal (list :nothing) (funcall (funcall hydra_lib_maybes_apply (list :just (funcall hydra_lib_math_add 3))) (list :nothing)))))

;; bind

(ert-deftest test-maybes-negbind-negjust-to-just ()

  (should (equal (list :just 10) (funcall (funcall hydra_lib_maybes_bind (list :just 5)) (lambda (x) (list :just (funcall (hydra_lib_math_mul x) 2)))))))

(ert-deftest test-maybes-negbind-negnothing-to-nothing ()

  (should (equal (list :nothing) (funcall (funcall hydra_lib_maybes_bind (list :nothing)) (lambda (x) (list :just (funcall (hydra_lib_math_mul x) 2)))))))

;; cases

(ert-deftest test-maybes-negcases-negjust-applies-function ()

  (should (equal 10 (funcall (funcall (funcall hydra_lib_maybes_cases (list :just 5)) 0) (lambda (x) (funcall (hydra_lib_math_mul x) 2))))))

(ert-deftest test-maybes-negcases-negnothing-returns-default ()

  (should (equal 99 (funcall (funcall (funcall hydra_lib_maybes_cases (list :nothing)) 99) (lambda (x) (funcall (hydra_lib_math_mul x) 2))))))

;; cat

(ert-deftest test-maybes-negcat-negfilters-nothings ()

  (should (equal (list 1 2) (funcall hydra_lib_maybes_cat (list (list :just 1) (list :nothing) (list :just 2))))))

(ert-deftest test-maybes-negcat-negall-justs ()

  (should (equal (list 1 2) (funcall hydra_lib_maybes_cat (list (list :just 1) (list :just 2))))))

(ert-deftest test-maybes-negcat-negall-nothings ()

  (should (equal (list ) (funcall hydra_lib_maybes_cat (list (list :nothing) (list :nothing))))))

(ert-deftest test-maybes-negcat-negempty-list ()

  (should (equal (list ) (funcall hydra_lib_maybes_cat (list )))))

;; compose

(ert-deftest test-maybes-negcompose-negboth-succeed ()

  (should (equal (list :just 12) (funcall (funcall (funcall hydra_lib_maybes_compose (lambda (x) (if (funcall (hydra_lib_equality_lte x) 5) (list :just (funcall (hydra_lib_math_add x) 1)) (list :nothing)))) (lambda (y) (if (funcall (hydra_lib_equality_gte y) 5) (list :just (funcall (hydra_lib_math_mul y) 2)) (list :nothing)))) 5))))

(ert-deftest test-maybes-negcompose-negfirst-fails ()

  (should (equal (list :nothing) (funcall (funcall (funcall hydra_lib_maybes_compose (lambda (x) (if (funcall (hydra_lib_equality_lte x) 5) (list :just (funcall (hydra_lib_math_add x) 1)) (list :nothing)))) (lambda (y) (if (funcall (hydra_lib_equality_gte y) 5) (list :just (funcall (hydra_lib_math_mul y) 2)) (list :nothing)))) 10))))

(ert-deftest test-maybes-negcompose-negsecond-fails ()

  (should (equal (list :nothing) (funcall (funcall (funcall hydra_lib_maybes_compose (lambda (x) (if (funcall (hydra_lib_equality_lte x) 5) (list :just (funcall (hydra_lib_math_add x) 1)) (list :nothing)))) (lambda (y) (if (funcall (hydra_lib_equality_gte y) 5) (list :just (funcall (hydra_lib_math_mul y) 2)) (list :nothing)))) 3))))

;; fromJust

(ert-deftest test-maybes-negfromjust-negextract-from-just ()

  (should (equal 42 (funcall hydra_lib_maybes_from_just (list :just 42)))))

;; fromMaybe

(ert-deftest test-maybes-negfrommaybe-negjust-value ()

  (should (equal 42 (funcall (funcall hydra_lib_maybes_from_maybe 0) (list :just 42)))))

(ert-deftest test-maybes-negfrommaybe-negnothing-with-default ()

  (should (equal 99 (funcall (funcall hydra_lib_maybes_from_maybe 99) (list :nothing)))))

;; isJust

(ert-deftest test-maybes-negisjust-negjust-value ()

  (should (equal t (funcall hydra_lib_maybes_is_just (list :just 42)))))

(ert-deftest test-maybes-negisjust-negnothing ()

  (should (equal nil (funcall hydra_lib_maybes_is_just (list :nothing)))))

;; isNothing

(ert-deftest test-maybes-negisnothing-negjust-value ()

  (should (equal nil (funcall hydra_lib_maybes_is_nothing (list :just 42)))))

(ert-deftest test-maybes-negisnothing-negnothing ()

  (should (equal t (funcall hydra_lib_maybes_is_nothing (list :nothing)))))

;; map

(ert-deftest test-maybes-negmap-negmaps-just-value ()

  (should (equal (list :just 10) (funcall (funcall hydra_lib_maybes_map (lambda (x) (funcall (hydra_lib_math_mul x) 2))) (list :just 5)))))

(ert-deftest test-maybes-negmap-negnothing-unchanged ()

  (should (equal (list :nothing) (funcall (funcall hydra_lib_maybes_map (lambda (x) (funcall (hydra_lib_math_mul x) 2))) (list :nothing)))))

;; mapMaybe

(ert-deftest test-maybes-negmapmaybe-negfilter-and-transform ()

  (should (equal (list 6 8 10) (funcall (funcall hydra_lib_maybes_map_maybe (lambda (x) (if (funcall (hydra_lib_equality_gt x) 2) (list :just (funcall (hydra_lib_math_mul x) 2)) (list :nothing)))) (list 1 2 3 4 5)))))

(ert-deftest test-maybes-negmapmaybe-negempty-result ()

  (should (equal (list ) (funcall (funcall hydra_lib_maybes_map_maybe (lambda (x) (if (funcall (hydra_lib_equality_gt x) 2) (list :just (funcall (hydra_lib_math_mul x) 2)) (list :nothing)))) (list 1 2)))))

(ert-deftest test-maybes-negmapmaybe-negempty-input ()

  (should (equal (list ) (funcall (funcall hydra_lib_maybes_map_maybe (lambda (x) (if (funcall (hydra_lib_equality_gt x) 2) (list :just (funcall (hydra_lib_math_mul x) 2)) (list :nothing)))) (list )))))

;; maybe

(ert-deftest test-maybes-negmaybe-negjust-value-applies-function ()

  (should (equal 10 (funcall (funcall (funcall hydra_lib_maybes_maybe 0) (lambda (x) (funcall (hydra_lib_math_mul x) 2))) (list :just 5)))))

(ert-deftest test-maybes-negmaybe-negnothing-returns-default ()

  (should (equal 99 (funcall (funcall (funcall hydra_lib_maybes_maybe 99) (lambda (x) (funcall (hydra_lib_math_mul x) 2))) (list :nothing)))))

;; pure

(ert-deftest test-maybes-negpure-negwraps-integer ()

  (should (equal (list :just 42) (funcall hydra_lib_maybes_pure 42))))

(ert-deftest test-maybes-negpure-negwraps-string ()

  (should (equal (list :just "hello") (funcall hydra_lib_maybes_pure "hello"))))

;; toList

(ert-deftest test-maybes-negtolist-negjust-value ()

  (should (equal (list 42) (funcall hydra_lib_maybes_to_list (list :just 42)))))

(ert-deftest test-maybes-negtolist-negnothing ()

  (should (equal (list ) (funcall hydra_lib_maybes_to_list (list :nothing)))))
