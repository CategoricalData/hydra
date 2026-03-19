;;; Note: this is an automatically generated file. Do not edit.
;;; hydra.lib.maybes primitives

(require 'ert)

;; apply

(ert-deftest test-apply-negboth-just ()

  (should (equal (list :just 8) ((hydra_lib_maybes_apply (list :just (hydra_lib_math_add 3))) (list :just 5)))))

(ert-deftest test-apply-negnothing-function ()

  (should (equal (list :nothing) ((hydra_lib_maybes_apply (list :nothing)) (list :just 5)))))

(ert-deftest test-apply-negnothing-value ()

  (should (equal (list :nothing) ((hydra_lib_maybes_apply (list :just (hydra_lib_math_add 3))) (list :nothing)))))

;; bind

(ert-deftest test-bind-negjust-to-just ()

  (should (equal (list :just 10) ((hydra_lib_maybes_bind (list :just 5)) (lambda (x) (list :just ((hydra_lib_math_mul x) 2)))))))

(ert-deftest test-bind-negnothing-to-nothing ()

  (should (equal (list :nothing) ((hydra_lib_maybes_bind (list :nothing)) (lambda (x) (list :just ((hydra_lib_math_mul x) 2)))))))

;; cases

(ert-deftest test-cases-negjust-applies-function ()

  (should (equal 10 (((hydra_lib_maybes_cases (list :just 5)) 0) (lambda (x) ((hydra_lib_math_mul x) 2))))))

(ert-deftest test-cases-negnothing-returns-default ()

  (should (equal 99 (((hydra_lib_maybes_cases (list :nothing)) 99) (lambda (x) ((hydra_lib_math_mul x) 2))))))

;; cat

(ert-deftest test-cat-negfilters-nothings ()

  (should (equal (list 1 2) (hydra_lib_maybes_cat (list (list :just 1) (list :nothing) (list :just 2))))))

(ert-deftest test-cat-negall-justs ()

  (should (equal (list 1 2) (hydra_lib_maybes_cat (list (list :just 1) (list :just 2))))))

(ert-deftest test-cat-negall-nothings ()

  (should (equal (list ) (hydra_lib_maybes_cat (list (list :nothing) (list :nothing))))))

(ert-deftest test-cat-negempty-list ()

  (should (equal (list ) (hydra_lib_maybes_cat (list )))))

;; compose

(ert-deftest test-compose-negboth-succeed ()

  (should (equal (list :just 12) (((hydra_lib_maybes_compose (lambda (x) (if ((hydra_lib_equality_lte x) 5) (list :just ((hydra_lib_math_add x) 1)) (list :nothing)))) (lambda (y) (if ((hydra_lib_equality_gte y) 5) (list :just ((hydra_lib_math_mul y) 2)) (list :nothing)))) 5))))

(ert-deftest test-compose-negfirst-fails ()

  (should (equal (list :nothing) (((hydra_lib_maybes_compose (lambda (x) (if ((hydra_lib_equality_lte x) 5) (list :just ((hydra_lib_math_add x) 1)) (list :nothing)))) (lambda (y) (if ((hydra_lib_equality_gte y) 5) (list :just ((hydra_lib_math_mul y) 2)) (list :nothing)))) 10))))

(ert-deftest test-compose-negsecond-fails ()

  (should (equal (list :nothing) (((hydra_lib_maybes_compose (lambda (x) (if ((hydra_lib_equality_lte x) 5) (list :just ((hydra_lib_math_add x) 1)) (list :nothing)))) (lambda (y) (if ((hydra_lib_equality_gte y) 5) (list :just ((hydra_lib_math_mul y) 2)) (list :nothing)))) 3))))

;; fromJust

(ert-deftest test-fromjust-negextract-from-just ()

  (should (equal 42 (hydra_lib_maybes_from_just (list :just 42)))))

;; fromMaybe

(ert-deftest test-frommaybe-negjust-value ()

  (should (equal 42 ((hydra_lib_maybes_from_maybe 0) (list :just 42)))))

(ert-deftest test-frommaybe-negnothing-with-default ()

  (should (equal 99 ((hydra_lib_maybes_from_maybe 99) (list :nothing)))))

;; isJust

(ert-deftest test-isjust-negjust-value ()

  (should (equal t (hydra_lib_maybes_is_just (list :just 42)))))

(ert-deftest test-isjust-negnothing ()

  (should (equal nil (hydra_lib_maybes_is_just (list :nothing)))))

;; isNothing

(ert-deftest test-isnothing-negjust-value ()

  (should (equal nil (hydra_lib_maybes_is_nothing (list :just 42)))))

(ert-deftest test-isnothing-negnothing ()

  (should (equal t (hydra_lib_maybes_is_nothing (list :nothing)))))

;; map

(ert-deftest test-map-negmaps-just-value ()

  (should (equal (list :just 10) ((hydra_lib_maybes_map (lambda (x) ((hydra_lib_math_mul x) 2))) (list :just 5)))))

(ert-deftest test-map-negnothing-unchanged ()

  (should (equal (list :nothing) ((hydra_lib_maybes_map (lambda (x) ((hydra_lib_math_mul x) 2))) (list :nothing)))))

;; mapMaybe

(ert-deftest test-mapmaybe-negfilter-and-transform ()

  (should (equal (list 6 8 10) ((hydra_lib_maybes_map_maybe (lambda (x) (if ((hydra_lib_equality_gt x) 2) (list :just ((hydra_lib_math_mul x) 2)) (list :nothing)))) (list 1 2 3 4 5)))))

(ert-deftest test-mapmaybe-negempty-result ()

  (should (equal (list ) ((hydra_lib_maybes_map_maybe (lambda (x) (if ((hydra_lib_equality_gt x) 2) (list :just ((hydra_lib_math_mul x) 2)) (list :nothing)))) (list 1 2)))))

(ert-deftest test-mapmaybe-negempty-input ()

  (should (equal (list ) ((hydra_lib_maybes_map_maybe (lambda (x) (if ((hydra_lib_equality_gt x) 2) (list :just ((hydra_lib_math_mul x) 2)) (list :nothing)))) (list )))))

;; maybe

(ert-deftest test-maybe-negjust-value-applies-function ()

  (should (equal 10 (((hydra_lib_maybes_maybe 0) (lambda (x) ((hydra_lib_math_mul x) 2))) (list :just 5)))))

(ert-deftest test-maybe-negnothing-returns-default ()

  (should (equal 99 (((hydra_lib_maybes_maybe 99) (lambda (x) ((hydra_lib_math_mul x) 2))) (list :nothing)))))

;; pure

(ert-deftest test-pure-negwraps-integer ()

  (should (equal (list :just 42) (hydra_lib_maybes_pure 42))))

(ert-deftest test-pure-negwraps-string ()

  (should (equal (list :just "hello") (hydra_lib_maybes_pure "hello"))))

;; toList

(ert-deftest test-tolist-negjust-value ()

  (should (equal (list 42) (hydra_lib_maybes_to_list (list :just 42)))))

(ert-deftest test-tolist-negnothing ()

  (should (equal (list ) (hydra_lib_maybes_to_list (list :nothing)))))
