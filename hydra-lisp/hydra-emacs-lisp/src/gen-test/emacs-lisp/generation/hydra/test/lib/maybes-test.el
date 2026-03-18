;;; Note: this is an automatically generated file. Do not edit.
;;; hydra.lib.maybes primitives

(require 'ert)

;; apply

(ert-deftest test-apply-negboth-just ()

  (should (equal 8 ((hydra_lib_maybes_apply (hydra_lib_math_add 3)) 5))))

(ert-deftest test-apply-negnothing-function ()

  (should (equal nil ((hydra_lib_maybes_apply nil) 5))))

(ert-deftest test-apply-negnothing-value ()

  (should (equal nil ((hydra_lib_maybes_apply (hydra_lib_math_add 3)) nil))))

;; bind

(ert-deftest test-bind-negjust-to-just ()

  (should (equal 10 ((hydra_lib_maybes_bind 5) (lambda (x) ((hydra_lib_math_mul x) 2))))))

(ert-deftest test-bind-negnothing-to-nothing ()

  (should (equal nil ((hydra_lib_maybes_bind nil) (lambda (x) ((hydra_lib_math_mul x) 2))))))

;; cases

(ert-deftest test-cases-negjust-applies-function ()

  (should (equal 10 (((hydra_lib_maybes_cases 5) 0) (lambda (x) ((hydra_lib_math_mul x) 2))))))

(ert-deftest test-cases-negnothing-returns-default ()

  (should (equal 99 (((hydra_lib_maybes_cases nil) 99) (lambda (x) ((hydra_lib_math_mul x) 2))))))

;; cat

(ert-deftest test-cat-negfilters-nothings ()

  (should (equal (list 1 2) (hydra_lib_maybes_cat (list 1 nil 2)))))

(ert-deftest test-cat-negall-justs ()

  (should (equal (list 1 2) (hydra_lib_maybes_cat (list 1 2)))))

(ert-deftest test-cat-negall-nothings ()

  (should (equal (list ) (hydra_lib_maybes_cat (list nil nil)))))

(ert-deftest test-cat-negempty-list ()

  (should (equal (list ) (hydra_lib_maybes_cat (list )))))

;; compose

(ert-deftest test-compose-negboth-succeed ()

  (should (equal 12 (((hydra_lib_maybes_compose (lambda (x) (if ((hydra_lib_equality_lte x) 5) ((hydra_lib_math_add x) 1) nil))) (lambda (y) (if ((hydra_lib_equality_gte y) 5) ((hydra_lib_math_mul y) 2) nil))) 5))))

(ert-deftest test-compose-negfirst-fails ()

  (should (equal nil (((hydra_lib_maybes_compose (lambda (x) (if ((hydra_lib_equality_lte x) 5) ((hydra_lib_math_add x) 1) nil))) (lambda (y) (if ((hydra_lib_equality_gte y) 5) ((hydra_lib_math_mul y) 2) nil))) 10))))

(ert-deftest test-compose-negsecond-fails ()

  (should (equal nil (((hydra_lib_maybes_compose (lambda (x) (if ((hydra_lib_equality_lte x) 5) ((hydra_lib_math_add x) 1) nil))) (lambda (y) (if ((hydra_lib_equality_gte y) 5) ((hydra_lib_math_mul y) 2) nil))) 3))))

;; fromJust

(ert-deftest test-fromjust-negextract-from-just ()

  (should (equal 42 (hydra_lib_maybes_from_just 42))))

;; fromMaybe

(ert-deftest test-frommaybe-negjust-value ()

  (should (equal 42 ((hydra_lib_maybes_from_maybe 0) 42))))

(ert-deftest test-frommaybe-negnothing-with-default ()

  (should (equal 99 ((hydra_lib_maybes_from_maybe 99) nil))))

;; isJust

(ert-deftest test-isjust-negjust-value ()

  (should (equal t (hydra_lib_maybes_is_just 42))))

(ert-deftest test-isjust-negnothing ()

  (should (equal nil (hydra_lib_maybes_is_just nil))))

;; isNothing

(ert-deftest test-isnothing-negjust-value ()

  (should (equal nil (hydra_lib_maybes_is_nothing 42))))

(ert-deftest test-isnothing-negnothing ()

  (should (equal t (hydra_lib_maybes_is_nothing nil))))

;; map

(ert-deftest test-map-negmaps-just-value ()

  (should (equal 10 ((hydra_lib_maybes_map (lambda (x) ((hydra_lib_math_mul x) 2))) 5))))

(ert-deftest test-map-negnothing-unchanged ()

  (should (equal nil ((hydra_lib_maybes_map (lambda (x) ((hydra_lib_math_mul x) 2))) nil))))

;; mapMaybe

(ert-deftest test-mapmaybe-negfilter-and-transform ()

  (should (equal (list 6 8 10) ((hydra_lib_maybes_map_maybe (lambda (x) (if ((hydra_lib_equality_gt x) 2) ((hydra_lib_math_mul x) 2) nil))) (list 1 2 3 4 5)))))

(ert-deftest test-mapmaybe-negempty-result ()

  (should (equal (list ) ((hydra_lib_maybes_map_maybe (lambda (x) (if ((hydra_lib_equality_gt x) 2) ((hydra_lib_math_mul x) 2) nil))) (list 1 2)))))

(ert-deftest test-mapmaybe-negempty-input ()

  (should (equal (list ) ((hydra_lib_maybes_map_maybe (lambda (x) (if ((hydra_lib_equality_gt x) 2) ((hydra_lib_math_mul x) 2) nil))) (list )))))

;; maybe

(ert-deftest test-maybe-negjust-value-applies-function ()

  (should (equal 10 (((hydra_lib_maybes_maybe 0) (lambda (x) ((hydra_lib_math_mul x) 2))) 5))))

(ert-deftest test-maybe-negnothing-returns-default ()

  (should (equal 99 (((hydra_lib_maybes_maybe 99) (lambda (x) ((hydra_lib_math_mul x) 2))) nil))))

;; pure

(ert-deftest test-pure-negwraps-integer ()

  (should (equal 42 (hydra_lib_maybes_pure 42))))

(ert-deftest test-pure-negwraps-string ()

  (should (equal "hello" (hydra_lib_maybes_pure "hello"))))

;; toList

(ert-deftest test-tolist-negjust-value ()

  (should (equal (list 42) (hydra_lib_maybes_to_list 42))))

(ert-deftest test-tolist-negnothing ()

  (should (equal (list ) (hydra_lib_maybes_to_list nil))))
