;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maybes primitives

;; apply

(defun test-apply-negboth-just ()

  (assert (equal (list :just 8) ((hydra_lib_maybes_apply (list :just (hydra_lib_math_add 3))) (list :just 5)))))

(defun test-apply-negnothing-function ()

  (assert (equal (list :nothing) ((hydra_lib_maybes_apply (list :nothing)) (list :just 5)))))

(defun test-apply-negnothing-value ()

  (assert (equal (list :nothing) ((hydra_lib_maybes_apply (list :just (hydra_lib_math_add 3))) (list :nothing)))))

;; bind

(defun test-bind-negjust-to-just ()

  (assert (equal (list :just 10) ((hydra_lib_maybes_bind (list :just 5)) (cl:lambda (x) ((hydra_lib_math_mul x) 2))))))

(defun test-bind-negnothing-to-nothing ()

  (assert (equal (list :nothing) ((hydra_lib_maybes_bind (list :nothing)) (cl:lambda (x) ((hydra_lib_math_mul x) 2))))))

;; cases

(defun test-cases-negjust-applies-function ()

  (assert (equal 10 (((hydra_lib_maybes_cases (list :just 5)) 0) (cl:lambda (x) ((hydra_lib_math_mul x) 2))))))

(defun test-cases-negnothing-returns-default ()

  (assert (equal 99 (((hydra_lib_maybes_cases (list :nothing)) 99) (cl:lambda (x) ((hydra_lib_math_mul x) 2))))))

;; cat

(defun test-cat-negfilters-nothings ()

  (assert (equal (list 1 2) (hydra_lib_maybes_cat (list (list :just 1) (list :nothing) (list :just 2))))))

(defun test-cat-negall-justs ()

  (assert (equal (list 1 2) (hydra_lib_maybes_cat (list (list :just 1) (list :just 2))))))

(defun test-cat-negall-nothings ()

  (assert (equal (list ) (hydra_lib_maybes_cat (list (list :nothing) (list :nothing))))))

(defun test-cat-negempty-list ()

  (assert (equal (list ) (hydra_lib_maybes_cat (list )))))

;; compose

(defun test-compose-negboth-succeed ()

  (assert (equal (list :just 12) (((hydra_lib_maybes_compose (cl:lambda (x) (if ((hydra_lib_equality_lte x) 5) ((hydra_lib_math_add x) 1) cl:nil))) (cl:lambda (y) (if ((hydra_lib_equality_gte y) 5) ((hydra_lib_math_mul y) 2) cl:nil))) 5))))

(defun test-compose-negfirst-fails ()

  (assert (equal (list :nothing) (((hydra_lib_maybes_compose (cl:lambda (x) (if ((hydra_lib_equality_lte x) 5) ((hydra_lib_math_add x) 1) cl:nil))) (cl:lambda (y) (if ((hydra_lib_equality_gte y) 5) ((hydra_lib_math_mul y) 2) cl:nil))) 10))))

(defun test-compose-negsecond-fails ()

  (assert (equal (list :nothing) (((hydra_lib_maybes_compose (cl:lambda (x) (if ((hydra_lib_equality_lte x) 5) ((hydra_lib_math_add x) 1) cl:nil))) (cl:lambda (y) (if ((hydra_lib_equality_gte y) 5) ((hydra_lib_math_mul y) 2) cl:nil))) 3))))

;; fromJust

(defun test-fromjust-negextract-from-just ()

  (assert (equal 42 (hydra_lib_maybes_from_just (list :just 42)))))

;; fromMaybe

(defun test-frommaybe-negjust-value ()

  (assert (equal 42 ((hydra_lib_maybes_from_maybe 0) (list :just 42)))))

(defun test-frommaybe-negnothing-with-default ()

  (assert (equal 99 ((hydra_lib_maybes_from_maybe 99) (list :nothing)))))

;; isJust

(defun test-isjust-negjust-value ()

  (assert (equal cl:t (hydra_lib_maybes_is_just (list :just 42)))))

(defun test-isjust-negnothing ()

  (assert (equal cl:nil (hydra_lib_maybes_is_just (list :nothing)))))

;; isNothing

(defun test-isnothing-negjust-value ()

  (assert (equal cl:nil (hydra_lib_maybes_is_nothing (list :just 42)))))

(defun test-isnothing-negnothing ()

  (assert (equal cl:t (hydra_lib_maybes_is_nothing (list :nothing)))))

;; map

(defun test-map-negmaps-just-value ()

  (assert (equal (list :just 10) ((hydra_lib_maybes_map (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (list :just 5)))))

(defun test-map-negnothing-unchanged ()

  (assert (equal (list :nothing) ((hydra_lib_maybes_map (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (list :nothing)))))

;; mapMaybe

(defun test-mapmaybe-negfilter-and-transform ()

  (assert (equal (list 6 8 10) ((hydra_lib_maybes_map_maybe (cl:lambda (x) (if ((hydra_lib_equality_gt x) 2) ((hydra_lib_math_mul x) 2) cl:nil))) (list 1 2 3 4 5)))))

(defun test-mapmaybe-negempty-result ()

  (assert (equal (list ) ((hydra_lib_maybes_map_maybe (cl:lambda (x) (if ((hydra_lib_equality_gt x) 2) ((hydra_lib_math_mul x) 2) cl:nil))) (list 1 2)))))

(defun test-mapmaybe-negempty-input ()

  (assert (equal (list ) ((hydra_lib_maybes_map_maybe (cl:lambda (x) (if ((hydra_lib_equality_gt x) 2) ((hydra_lib_math_mul x) 2) cl:nil))) (list )))))

;; maybe

(defun test-maybe-negjust-value-applies-function ()

  (assert (equal 10 (((hydra_lib_maybes_maybe 0) (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (list :just 5)))))

(defun test-maybe-negnothing-returns-default ()

  (assert (equal 99 (((hydra_lib_maybes_maybe 99) (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (list :nothing)))))

;; pure

(defun test-pure-negwraps-integer ()

  (assert (equal (list :just 42) (hydra_lib_maybes_pure 42))))

(defun test-pure-negwraps-string ()

  (assert (equal (list :just "hello") (hydra_lib_maybes_pure "hello"))))

;; toList

(defun test-tolist-negjust-value ()

  (assert (equal (list 42) (hydra_lib_maybes_to_list (list :just 42)))))

(defun test-tolist-negnothing ()

  (assert (equal (list ) (hydra_lib_maybes_to_list (list :nothing)))))
