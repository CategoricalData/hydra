;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.eithers primitives

;; bind

(defun test-bind-negbind-right-with-success ()

  (assert (equal (list :right 2) ((hydra_lib_eithers_bind (list :right "ab")) (cl:lambda (s) (if (hydra_lib_strings_null s) (list :left 0) (list :right (hydra_lib_strings_length s))))))))

(defun test-bind-negbind-right-with-failure ()

  (assert (equal (list :left 0) ((hydra_lib_eithers_bind (list :right "")) (cl:lambda (s) (if (hydra_lib_strings_null s) (list :left 0) (list :right (hydra_lib_strings_length s))))))))

(defun test-bind-negbind-left-returns-left-unchanged ()

  (assert (equal (list :left 42) ((hydra_lib_eithers_bind (list :left 42)) (cl:lambda (s) (if (hydra_lib_strings_null s) (list :left 0) (list :right (hydra_lib_strings_length s))))))))

;; bimap

(defun test-bimap-negmap-left-value ()

  (assert (equal (list :left 10) (((hydra_lib_eithers_bimap (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (cl:lambda (s) (hydra_lib_strings_length s))) (list :left 5)))))

(defun test-bimap-negmap-right-value ()

  (assert (equal (list :right 2) (((hydra_lib_eithers_bimap (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (cl:lambda (s) (hydra_lib_strings_length s))) (list :right "ab")))))

;; isLeft

(defun test-isleft-negleft-value ()

  (assert (equal cl:t (hydra_lib_eithers_is_left (list :left 42)))))

(defun test-isleft-negright-value ()

  (assert (equal cl:nil (hydra_lib_eithers_is_left (list :right "test")))))

;; isRight

(defun test-isright-negright-value ()

  (assert (equal cl:t (hydra_lib_eithers_is_right (list :right "test")))))

(defun test-isright-negleft-value ()

  (assert (equal cl:nil (hydra_lib_eithers_is_right (list :left 42)))))

;; fromLeft

(defun test-fromleft-negextract-left ()

  (assert (equal 42 ((hydra_lib_eithers_from_left 99) (list :left 42)))))

(defun test-fromleft-neguse-default-for-right ()

  (assert (equal 99 ((hydra_lib_eithers_from_left 99) (list :right "test")))))

;; fromRight

(defun test-fromright-negextract-right ()

  (assert (equal "test" ((hydra_lib_eithers_from_right "default") (list :right "test")))))

(defun test-fromright-neguse-default-for-left ()

  (assert (equal "default" ((hydra_lib_eithers_from_right "default") (list :left 42)))))

;; either

(defun test-either-negapply-left-function ()

  (assert (equal 10 (((hydra_lib_eithers_either (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (cl:lambda (s) (hydra_lib_strings_length s))) (list :left 5)))))

(defun test-either-negapply-right-function ()

  (assert (equal 2 (((hydra_lib_eithers_either (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (cl:lambda (s) (hydra_lib_strings_length s))) (list :right "ab")))))

;; lefts

(defun test-lefts-negfilter-left-values ()

  (assert (equal (list 1 2) (hydra_lib_eithers_lefts (list (list :left 1) (list :right "a") (list :left 2) (list :right "b"))))))

(defun test-lefts-negall-lefts ()

  (assert (equal (list 1 2) (hydra_lib_eithers_lefts (list (list :left 1) (list :left 2))))))

(defun test-lefts-negall-rights ()

  (assert (equal (list ) (hydra_lib_eithers_lefts (list (list :right "a") (list :right "b"))))))

(defun test-lefts-negempty-list ()

  (assert (equal (list ) (hydra_lib_eithers_lefts (list )))))

;; rights

(defun test-rights-negfilter-right-values ()

  (assert (equal (list "a" "b") (hydra_lib_eithers_rights (list (list :left 1) (list :right "a") (list :left 2) (list :right "b"))))))

(defun test-rights-negall-rights ()

  (assert (equal (list "a" "b") (hydra_lib_eithers_rights (list (list :right "a") (list :right "b"))))))

(defun test-rights-negall-lefts ()

  (assert (equal (list ) (hydra_lib_eithers_rights (list (list :left 1) (list :left 2))))))

(defun test-rights-negempty-list ()

  (assert (equal (list ) (hydra_lib_eithers_rights (list )))))

;; partitionEithers

(defun test-partitioneithers-negpartition-mixed ()

  (assert (equal (list (list 1 2) (list "a" "b")) (hydra_lib_eithers_partition_eithers (list (list :left 1) (list :right "a") (list :left 2) (list :right "b"))))))

(defun test-partitioneithers-negall-lefts ()

  (assert (equal (list (list 1 2) (list )) (hydra_lib_eithers_partition_eithers (list (list :left 1) (list :left 2))))))

(defun test-partitioneithers-negall-rights ()

  (assert (equal (list (list ) (list "a" "b")) (hydra_lib_eithers_partition_eithers (list (list :right "a") (list :right "b"))))))

(defun test-partitioneithers-negempty-list ()

  (assert (equal (list (list ) (list )) (hydra_lib_eithers_partition_eithers (list )))))

;; map

(defun test-map-negmap-right-value ()

  (assert (equal (list :right 10) ((hydra_lib_eithers_map (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (list :right 5)))))

(defun test-map-negpreserve-left ()

  (assert (equal (list :left 99) ((hydra_lib_eithers_map (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (list :left 99)))))

;; mapList

(defun test-maplist-negall-succeed ()

  (assert (equal (list :right (list 2 4 6)) ((hydra_lib_eithers_map_list (cl:lambda (x) (if ((hydra_lib_equality_equal x) 0) (list :left "zero") (list :right ((hydra_lib_math_mul x) 2))))) (list 1 2 3)))))

(defun test-maplist-negfirst-fails ()

  (assert (equal (list :left "zero") ((hydra_lib_eithers_map_list (cl:lambda (x) (if ((hydra_lib_equality_equal x) 0) (list :left "zero") (list :right ((hydra_lib_math_mul x) 2))))) (list 1 0 3)))))

(defun test-maplist-negempty-list ()

  (assert (equal (list :right (list )) ((hydra_lib_eithers_map_list (cl:lambda (x) (if ((hydra_lib_equality_equal x) 0) (list :left "zero") (list :right ((hydra_lib_math_mul x) 2))))) (list )))))

;; mapMaybe

(defun test-mapmaybe-negjust-succeeds ()

  (assert (equal (list :right 10) ((hydra_lib_eithers_map_maybe (cl:lambda (x) (if ((hydra_lib_equality_equal x) 0) (list :left "zero") (list :right ((hydra_lib_math_mul x) 2))))) 5))))

(defun test-mapmaybe-negjust-fails ()

  (assert (equal (list :left "zero") ((hydra_lib_eithers_map_maybe (cl:lambda (x) (if ((hydra_lib_equality_equal x) 0) (list :left "zero") (list :right ((hydra_lib_math_mul x) 2))))) 0))))

(defun test-mapmaybe-negnothing ()

  (assert (equal (list :right nil) ((hydra_lib_eithers_map_maybe (cl:lambda (x) (if ((hydra_lib_equality_equal x) 0) (list :left "zero") (list :right ((hydra_lib_math_mul x) 2))))) nil))))
