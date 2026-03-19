;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.eithers primitives

(require 'ert)

;; bind

(ert-deftest test-eithers-negbind-negbind-right-with-success ()

  (should (equal (list :right 2) (funcall (funcall hydra_lib_eithers_bind (list :right "ab")) (lambda (s) (if (hydra_lib_strings_null s) (list :left 0) (list :right (hydra_lib_strings_length s))))))))

(ert-deftest test-eithers-negbind-negbind-right-with-failure ()

  (should (equal (list :left 0) (funcall (funcall hydra_lib_eithers_bind (list :right "")) (lambda (s) (if (hydra_lib_strings_null s) (list :left 0) (list :right (hydra_lib_strings_length s))))))))

(ert-deftest test-eithers-negbind-negbind-left-returns-left-unchanged ()

  (should (equal (list :left 42) (funcall (funcall hydra_lib_eithers_bind (list :left 42)) (lambda (s) (if (hydra_lib_strings_null s) (list :left 0) (list :right (hydra_lib_strings_length s))))))))

;; bimap

(ert-deftest test-eithers-negbimap-negmap-left-value ()

  (should (equal (list :left 10) (funcall (funcall (funcall hydra_lib_eithers_bimap (lambda (x) (funcall (hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list :left 5)))))

(ert-deftest test-eithers-negbimap-negmap-right-value ()

  (should (equal (list :right 2) (funcall (funcall (funcall hydra_lib_eithers_bimap (lambda (x) (funcall (hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list :right "ab")))))

;; isLeft

(ert-deftest test-eithers-negisleft-negleft-value ()

  (should (equal t (funcall hydra_lib_eithers_is_left (list :left 42)))))

(ert-deftest test-eithers-negisleft-negright-value ()

  (should (equal nil (funcall hydra_lib_eithers_is_left (list :right "test")))))

;; isRight

(ert-deftest test-eithers-negisright-negright-value ()

  (should (equal t (funcall hydra_lib_eithers_is_right (list :right "test")))))

(ert-deftest test-eithers-negisright-negleft-value ()

  (should (equal nil (funcall hydra_lib_eithers_is_right (list :left 42)))))

;; fromLeft

(ert-deftest test-eithers-negfromleft-negextract-left ()

  (should (equal 42 (funcall (funcall hydra_lib_eithers_from_left 99) (list :left 42)))))

(ert-deftest test-eithers-negfromleft-neguse-default-for-right ()

  (should (equal 99 (funcall (funcall hydra_lib_eithers_from_left 99) (list :right "test")))))

;; fromRight

(ert-deftest test-eithers-negfromright-negextract-right ()

  (should (equal "test" (funcall (funcall hydra_lib_eithers_from_right "default") (list :right "test")))))

(ert-deftest test-eithers-negfromright-neguse-default-for-left ()

  (should (equal "default" (funcall (funcall hydra_lib_eithers_from_right "default") (list :left 42)))))

;; either

(ert-deftest test-eithers-negeither-negapply-left-function ()

  (should (equal 10 (funcall (funcall (funcall hydra_lib_eithers_either (lambda (x) (funcall (hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list :left 5)))))

(ert-deftest test-eithers-negeither-negapply-right-function ()

  (should (equal 2 (funcall (funcall (funcall hydra_lib_eithers_either (lambda (x) (funcall (hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list :right "ab")))))

;; lefts

(ert-deftest test-eithers-neglefts-negfilter-left-values ()

  (should (equal (list 1 2) (funcall hydra_lib_eithers_lefts (list (list :left 1) (list :right "a") (list :left 2) (list :right "b"))))))

(ert-deftest test-eithers-neglefts-negall-lefts ()

  (should (equal (list 1 2) (funcall hydra_lib_eithers_lefts (list (list :left 1) (list :left 2))))))

(ert-deftest test-eithers-neglefts-negall-rights ()

  (should (equal (list ) (funcall hydra_lib_eithers_lefts (list (list :right "a") (list :right "b"))))))

(ert-deftest test-eithers-neglefts-negempty-list ()

  (should (equal (list ) (funcall hydra_lib_eithers_lefts (list )))))

;; rights

(ert-deftest test-eithers-negrights-negfilter-right-values ()

  (should (equal (list "a" "b") (funcall hydra_lib_eithers_rights (list (list :left 1) (list :right "a") (list :left 2) (list :right "b"))))))

(ert-deftest test-eithers-negrights-negall-rights ()

  (should (equal (list "a" "b") (funcall hydra_lib_eithers_rights (list (list :right "a") (list :right "b"))))))

(ert-deftest test-eithers-negrights-negall-lefts ()

  (should (equal (list ) (funcall hydra_lib_eithers_rights (list (list :left 1) (list :left 2))))))

(ert-deftest test-eithers-negrights-negempty-list ()

  (should (equal (list ) (funcall hydra_lib_eithers_rights (list )))))

;; partitionEithers

(ert-deftest test-eithers-negpartitioneithers-negpartition-mixed ()

  (should (equal (list (list 1 2) (list "a" "b")) (funcall hydra_lib_eithers_partition_eithers (list (list :left 1) (list :right "a") (list :left 2) (list :right "b"))))))

(ert-deftest test-eithers-negpartitioneithers-negall-lefts ()

  (should (equal (list (list 1 2) (list )) (funcall hydra_lib_eithers_partition_eithers (list (list :left 1) (list :left 2))))))

(ert-deftest test-eithers-negpartitioneithers-negall-rights ()

  (should (equal (list (list ) (list "a" "b")) (funcall hydra_lib_eithers_partition_eithers (list (list :right "a") (list :right "b"))))))

(ert-deftest test-eithers-negpartitioneithers-negempty-list ()

  (should (equal (list (list ) (list )) (funcall hydra_lib_eithers_partition_eithers (list )))))

;; map

(ert-deftest test-eithers-negmap-negmap-right-value ()

  (should (equal (list :right 10) (funcall (funcall hydra_lib_eithers_map (lambda (x) (funcall (hydra_lib_math_mul x) 2))) (list :right 5)))))

(ert-deftest test-eithers-negmap-negpreserve-left ()

  (should (equal (list :left 99) (funcall (funcall hydra_lib_eithers_map (lambda (x) (funcall (hydra_lib_math_mul x) 2))) (list :left 99)))))

;; mapList

(ert-deftest test-eithers-negmaplist-negall-succeed ()

  (should (equal (list :right (list 2 4 6)) (funcall (funcall hydra_lib_eithers_map_list (lambda (x) (if (funcall (hydra_lib_equality_equal x) 0) (list :left "zero") (list :right (funcall (hydra_lib_math_mul x) 2))))) (list 1 2 3)))))

(ert-deftest test-eithers-negmaplist-negfirst-fails ()

  (should (equal (list :left "zero") (funcall (funcall hydra_lib_eithers_map_list (lambda (x) (if (funcall (hydra_lib_equality_equal x) 0) (list :left "zero") (list :right (funcall (hydra_lib_math_mul x) 2))))) (list 1 0 3)))))

(ert-deftest test-eithers-negmaplist-negempty-list ()

  (should (equal (list :right (list )) (funcall (funcall hydra_lib_eithers_map_list (lambda (x) (if (funcall (hydra_lib_equality_equal x) 0) (list :left "zero") (list :right (funcall (hydra_lib_math_mul x) 2))))) (list )))))

;; mapMaybe

(ert-deftest test-eithers-negmapmaybe-negjust-succeeds ()

  (should (equal (list :right (list :just 10)) (funcall (funcall hydra_lib_eithers_map_maybe (lambda (x) (if (funcall (hydra_lib_equality_equal x) 0) (list :left "zero") (list :right (funcall (hydra_lib_math_mul x) 2))))) (list :just 5)))))

(ert-deftest test-eithers-negmapmaybe-negjust-fails ()

  (should (equal (list :left "zero") (funcall (funcall hydra_lib_eithers_map_maybe (lambda (x) (if (funcall (hydra_lib_equality_equal x) 0) (list :left "zero") (list :right (funcall (hydra_lib_math_mul x) 2))))) (list :just 0)))))

(ert-deftest test-eithers-negmapmaybe-negnothing ()

  (should (equal (list :right (list :nothing)) (funcall (funcall hydra_lib_eithers_map_maybe (lambda (x) (if (funcall (hydra_lib_equality_equal x) 0) (list :left "zero") (list :right (funcall (hydra_lib_math_mul x) 2))))) (list :nothing)))))
