;;; Note: this is an automatically generated file. Do not edit.
;;; hydra.lib.eithers primitives

(require 'ert)

;; bind

(ert-deftest test-bind-negbind-right-with-success ()

  (should (equal (list :right 2) ((hydra_lib_eithers_bind (list :right "ab")) (lambda (s) (if (hydra_lib_strings_null s) (list :left 0) (list :right (hydra_lib_strings_length s))))))))

(ert-deftest test-bind-negbind-right-with-failure ()

  (should (equal (list :left 0) ((hydra_lib_eithers_bind (list :right "")) (lambda (s) (if (hydra_lib_strings_null s) (list :left 0) (list :right (hydra_lib_strings_length s))))))))

(ert-deftest test-bind-negbind-left-returns-left-unchanged ()

  (should (equal (list :left 42) ((hydra_lib_eithers_bind (list :left 42)) (lambda (s) (if (hydra_lib_strings_null s) (list :left 0) (list :right (hydra_lib_strings_length s))))))))

;; bimap

(ert-deftest test-bimap-negmap-left-value ()

  (should (equal (list :left 10) (((hydra_lib_eithers_bimap (lambda (x) ((hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list :left 5)))))

(ert-deftest test-bimap-negmap-right-value ()

  (should (equal (list :right 2) (((hydra_lib_eithers_bimap (lambda (x) ((hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list :right "ab")))))

;; isLeft

(ert-deftest test-isleft-negleft-value ()

  (should (equal t (hydra_lib_eithers_is_left (list :left 42)))))

(ert-deftest test-isleft-negright-value ()

  (should (equal nil (hydra_lib_eithers_is_left (list :right "test")))))

;; isRight

(ert-deftest test-isright-negright-value ()

  (should (equal t (hydra_lib_eithers_is_right (list :right "test")))))

(ert-deftest test-isright-negleft-value ()

  (should (equal nil (hydra_lib_eithers_is_right (list :left 42)))))

;; fromLeft

(ert-deftest test-fromleft-negextract-left ()

  (should (equal 42 ((hydra_lib_eithers_from_left 99) (list :left 42)))))

(ert-deftest test-fromleft-neguse-default-for-right ()

  (should (equal 99 ((hydra_lib_eithers_from_left 99) (list :right "test")))))

;; fromRight

(ert-deftest test-fromright-negextract-right ()

  (should (equal "test" ((hydra_lib_eithers_from_right "default") (list :right "test")))))

(ert-deftest test-fromright-neguse-default-for-left ()

  (should (equal "default" ((hydra_lib_eithers_from_right "default") (list :left 42)))))

;; either

(ert-deftest test-either-negapply-left-function ()

  (should (equal 10 (((hydra_lib_eithers_either (lambda (x) ((hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list :left 5)))))

(ert-deftest test-either-negapply-right-function ()

  (should (equal 2 (((hydra_lib_eithers_either (lambda (x) ((hydra_lib_math_mul x) 2))) (lambda (s) (hydra_lib_strings_length s))) (list :right "ab")))))

;; lefts

(ert-deftest test-lefts-negfilter-left-values ()

  (should (equal (list 1 2) (hydra_lib_eithers_lefts (list (list :left 1) (list :right "a") (list :left 2) (list :right "b"))))))

(ert-deftest test-lefts-negall-lefts ()

  (should (equal (list 1 2) (hydra_lib_eithers_lefts (list (list :left 1) (list :left 2))))))

(ert-deftest test-lefts-negall-rights ()

  (should (equal (list ) (hydra_lib_eithers_lefts (list (list :right "a") (list :right "b"))))))

(ert-deftest test-lefts-negempty-list ()

  (should (equal (list ) (hydra_lib_eithers_lefts (list )))))

;; rights

(ert-deftest test-rights-negfilter-right-values ()

  (should (equal (list "a" "b") (hydra_lib_eithers_rights (list (list :left 1) (list :right "a") (list :left 2) (list :right "b"))))))

(ert-deftest test-rights-negall-rights ()

  (should (equal (list "a" "b") (hydra_lib_eithers_rights (list (list :right "a") (list :right "b"))))))

(ert-deftest test-rights-negall-lefts ()

  (should (equal (list ) (hydra_lib_eithers_rights (list (list :left 1) (list :left 2))))))

(ert-deftest test-rights-negempty-list ()

  (should (equal (list ) (hydra_lib_eithers_rights (list )))))

;; partitionEithers

(ert-deftest test-partitioneithers-negpartition-mixed ()

  (should (equal (list (list 1 2) (list "a" "b")) (hydra_lib_eithers_partition_eithers (list (list :left 1) (list :right "a") (list :left 2) (list :right "b"))))))

(ert-deftest test-partitioneithers-negall-lefts ()

  (should (equal (list (list 1 2) (list )) (hydra_lib_eithers_partition_eithers (list (list :left 1) (list :left 2))))))

(ert-deftest test-partitioneithers-negall-rights ()

  (should (equal (list (list ) (list "a" "b")) (hydra_lib_eithers_partition_eithers (list (list :right "a") (list :right "b"))))))

(ert-deftest test-partitioneithers-negempty-list ()

  (should (equal (list (list ) (list )) (hydra_lib_eithers_partition_eithers (list )))))

;; map

(ert-deftest test-map-negmap-right-value ()

  (should (equal (list :right 10) ((hydra_lib_eithers_map (lambda (x) ((hydra_lib_math_mul x) 2))) (list :right 5)))))

(ert-deftest test-map-negpreserve-left ()

  (should (equal (list :left 99) ((hydra_lib_eithers_map (lambda (x) ((hydra_lib_math_mul x) 2))) (list :left 99)))))

;; mapList

(ert-deftest test-maplist-negall-succeed ()

  (should (equal (list :right (list 2 4 6)) ((hydra_lib_eithers_map_list (lambda (x) (if ((hydra_lib_equality_equal x) 0) (list :left "zero") (list :right ((hydra_lib_math_mul x) 2))))) (list 1 2 3)))))

(ert-deftest test-maplist-negfirst-fails ()

  (should (equal (list :left "zero") ((hydra_lib_eithers_map_list (lambda (x) (if ((hydra_lib_equality_equal x) 0) (list :left "zero") (list :right ((hydra_lib_math_mul x) 2))))) (list 1 0 3)))))

(ert-deftest test-maplist-negempty-list ()

  (should (equal (list :right (list )) ((hydra_lib_eithers_map_list (lambda (x) (if ((hydra_lib_equality_equal x) 0) (list :left "zero") (list :right ((hydra_lib_math_mul x) 2))))) (list )))))

;; mapMaybe

(ert-deftest test-mapmaybe-negjust-succeeds ()

  (should (equal (list :right (list :just 10)) ((hydra_lib_eithers_map_maybe (lambda (x) (if ((hydra_lib_equality_equal x) 0) (list :left "zero") (list :right ((hydra_lib_math_mul x) 2))))) (list :just 5)))))

(ert-deftest test-mapmaybe-negjust-fails ()

  (should (equal (list :left "zero") ((hydra_lib_eithers_map_maybe (lambda (x) (if ((hydra_lib_equality_equal x) 0) (list :left "zero") (list :right ((hydra_lib_math_mul x) 2))))) (list :just 0)))))

(ert-deftest test-mapmaybe-negnothing ()

  (should (equal (list :right (list :nothing)) ((hydra_lib_eithers_map_maybe (lambda (x) (if ((hydra_lib_equality_equal x) 0) (list :left "zero") (list :right ((hydra_lib_math_mul x) 2))))) (list :nothing)))))
