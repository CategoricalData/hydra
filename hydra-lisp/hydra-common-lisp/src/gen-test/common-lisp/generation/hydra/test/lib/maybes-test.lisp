;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maybes primitives

;; apply

(defun test-maybes-negapply-negboth-just ()

  (assert (equal (list :just 8) ((hydra_lib_maybes_apply (list :just (hydra_lib_math_add 3))) (list :just 5)))))

(defun test-maybes-negapply-negnothing-function ()

  (assert (equal (list :nothing) ((hydra_lib_maybes_apply (list :nothing)) (list :just 5)))))

(defun test-maybes-negapply-negnothing-value ()

  (assert (equal (list :nothing) ((hydra_lib_maybes_apply (list :just (hydra_lib_math_add 3))) (list :nothing)))))

;; bind

(defun test-maybes-negbind-negjust-to-just ()

  (assert (equal (list :just 10) ((hydra_lib_maybes_bind (list :just 5)) (cl:lambda (x) (list :just ((hydra_lib_math_mul x) 2)))))))

(defun test-maybes-negbind-negnothing-to-nothing ()

  (assert (equal (list :nothing) ((hydra_lib_maybes_bind (list :nothing)) (cl:lambda (x) (list :just ((hydra_lib_math_mul x) 2)))))))

;; cases

(defun test-maybes-negcases-negjust-applies-function ()

  (assert (equal 10 (((hydra_lib_maybes_cases (list :just 5)) 0) (cl:lambda (x) ((hydra_lib_math_mul x) 2))))))

(defun test-maybes-negcases-negnothing-returns-default ()

  (assert (equal 99 (((hydra_lib_maybes_cases (list :nothing)) 99) (cl:lambda (x) ((hydra_lib_math_mul x) 2))))))

;; cat

(defun test-maybes-negcat-negfilters-nothings ()

  (assert (equal (list 1 2) (hydra_lib_maybes_cat (list (list :just 1) (list :nothing) (list :just 2))))))

(defun test-maybes-negcat-negall-justs ()

  (assert (equal (list 1 2) (hydra_lib_maybes_cat (list (list :just 1) (list :just 2))))))

(defun test-maybes-negcat-negall-nothings ()

  (assert (equal (list ) (hydra_lib_maybes_cat (list (list :nothing) (list :nothing))))))

(defun test-maybes-negcat-negempty-list ()

  (assert (equal (list ) (hydra_lib_maybes_cat (list )))))

;; compose

(defun test-maybes-negcompose-negboth-succeed ()

  (assert (equal (list :just 12) (((hydra_lib_maybes_compose (cl:lambda (x) (if ((hydra_lib_equality_lte x) 5) (list :just ((hydra_lib_math_add x) 1)) (list :nothing)))) (cl:lambda (y) (if ((hydra_lib_equality_gte y) 5) (list :just ((hydra_lib_math_mul y) 2)) (list :nothing)))) 5))))

(defun test-maybes-negcompose-negfirst-fails ()

  (assert (equal (list :nothing) (((hydra_lib_maybes_compose (cl:lambda (x) (if ((hydra_lib_equality_lte x) 5) (list :just ((hydra_lib_math_add x) 1)) (list :nothing)))) (cl:lambda (y) (if ((hydra_lib_equality_gte y) 5) (list :just ((hydra_lib_math_mul y) 2)) (list :nothing)))) 10))))

(defun test-maybes-negcompose-negsecond-fails ()

  (assert (equal (list :nothing) (((hydra_lib_maybes_compose (cl:lambda (x) (if ((hydra_lib_equality_lte x) 5) (list :just ((hydra_lib_math_add x) 1)) (list :nothing)))) (cl:lambda (y) (if ((hydra_lib_equality_gte y) 5) (list :just ((hydra_lib_math_mul y) 2)) (list :nothing)))) 3))))

;; fromJust

(defun test-maybes-negfromjust-negextract-from-just ()

  (assert (equal 42 (hydra_lib_maybes_from_just (list :just 42)))))

;; fromMaybe

(defun test-maybes-negfrommaybe-negjust-value ()

  (assert (equal 42 ((hydra_lib_maybes_from_maybe 0) (list :just 42)))))

(defun test-maybes-negfrommaybe-negnothing-with-default ()

  (assert (equal 99 ((hydra_lib_maybes_from_maybe 99) (list :nothing)))))

;; isJust

(defun test-maybes-negisjust-negjust-value ()

  (assert (equal cl:t (hydra_lib_maybes_is_just (list :just 42)))))

(defun test-maybes-negisjust-negnothing ()

  (assert (equal cl:nil (hydra_lib_maybes_is_just (list :nothing)))))

;; isNothing

(defun test-maybes-negisnothing-negjust-value ()

  (assert (equal cl:nil (hydra_lib_maybes_is_nothing (list :just 42)))))

(defun test-maybes-negisnothing-negnothing ()

  (assert (equal cl:t (hydra_lib_maybes_is_nothing (list :nothing)))))

;; map

(defun test-maybes-negmap-negmaps-just-value ()

  (assert (equal (list :just 10) ((hydra_lib_maybes_map (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (list :just 5)))))

(defun test-maybes-negmap-negnothing-unchanged ()

  (assert (equal (list :nothing) ((hydra_lib_maybes_map (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (list :nothing)))))

;; mapMaybe

(defun test-maybes-negmapmaybe-negfilter-and-transform ()

  (assert (equal (list 6 8 10) ((hydra_lib_maybes_map_maybe (cl:lambda (x) (if ((hydra_lib_equality_gt x) 2) (list :just ((hydra_lib_math_mul x) 2)) (list :nothing)))) (list 1 2 3 4 5)))))

(defun test-maybes-negmapmaybe-negempty-result ()

  (assert (equal (list ) ((hydra_lib_maybes_map_maybe (cl:lambda (x) (if ((hydra_lib_equality_gt x) 2) (list :just ((hydra_lib_math_mul x) 2)) (list :nothing)))) (list 1 2)))))

(defun test-maybes-negmapmaybe-negempty-input ()

  (assert (equal (list ) ((hydra_lib_maybes_map_maybe (cl:lambda (x) (if ((hydra_lib_equality_gt x) 2) (list :just ((hydra_lib_math_mul x) 2)) (list :nothing)))) (list )))))

;; maybe

(defun test-maybes-negmaybe-negjust-value-applies-function ()

  (assert (equal 10 (((hydra_lib_maybes_maybe 0) (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (list :just 5)))))

(defun test-maybes-negmaybe-negnothing-returns-default ()

  (assert (equal 99 (((hydra_lib_maybes_maybe 99) (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (list :nothing)))))

;; pure

(defun test-maybes-negpure-negwraps-integer ()

  (assert (equal (list :just 42) (hydra_lib_maybes_pure 42))))

(defun test-maybes-negpure-negwraps-string ()

  (assert (equal (list :just "hello") (hydra_lib_maybes_pure "hello"))))

;; toList

(defun test-maybes-negtolist-negjust-value ()

  (assert (equal (list 42) (hydra_lib_maybes_to_list (list :just 42)))))

(defun test-maybes-negtolist-negnothing ()

  (assert (equal (list ) (hydra_lib_maybes_to_list (list :nothing)))))
