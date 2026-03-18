;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.sets primitives

;; empty

(defun test-empty-negempty-set ()

  (assert (equal () hydra_lib_sets_empty)))

;; singleton

(defun test-singleton-negsingle-element ()

  (assert (equal (list 42) (hydra_lib_sets_singleton 42))))

;; fromList

(defun test-fromlist-negcreate-from-list ()

  (assert (equal (list 1 2 3) (hydra_lib_sets_from_list (list 1 2 3)))))

(defun test-fromlist-negduplicates-removed ()

  (assert (equal (list 1 2 3) (hydra_lib_sets_from_list (list 1 2 1 3)))))

(defun test-fromlist-negempty-list ()

  (assert (equal () (hydra_lib_sets_from_list (list )))))

;; toList

(defun test-tolist-negconvert-to-list ()

  (assert (equal (list 1 2 3) (hydra_lib_sets_to_list (list 1 2 3)))))

(defun test-tolist-negunsorted-input ()

  (assert (equal (list 1 2 3) (hydra_lib_sets_to_list (list 1 2 3)))))

(defun test-tolist-negempty-set ()

  (assert (equal (list ) (hydra_lib_sets_to_list ()))))

;; insert

(defun test-insert-neginsert-new-element ()

  (assert (equal (list 1 2 3 4) ((hydra_lib_sets_insert 4) (list 1 2 3)))))

(defun test-insert-neginsert-existing-element ()

  (assert (equal (list 1 2 3) ((hydra_lib_sets_insert 2) (list 1 2 3)))))

(defun test-insert-neginsert-into-empty ()

  (assert (equal (list 1) ((hydra_lib_sets_insert 1) ()))))

;; delete

(defun test-delete-negdelete-existing ()

  (assert (equal (list 1 3) ((hydra_lib_sets_delete 2) (list 1 2 3)))))

(defun test-delete-negdelete-non-negexisting ()

  (assert (equal (list 1 2 3) ((hydra_lib_sets_delete 4) (list 1 2 3)))))

(defun test-delete-negdelete-from-empty ()

  (assert (equal () ((hydra_lib_sets_delete 1) ()))))

;; member

(defun test-member-negelement-exists ()

  (assert (equal cl:t ((hydra_lib_sets_member 2) (list 1 2 3)))))

(defun test-member-negelement-missing ()

  (assert (equal cl:nil ((hydra_lib_sets_member 4) (list 1 2 3)))))

(defun test-member-negempty-set ()

  (assert (equal cl:nil ((hydra_lib_sets_member 1) ()))))

;; size

(defun test-size-negthree-elements ()

  (assert (equal 3 (hydra_lib_sets_size (list 1 2 3)))))

(defun test-size-negsingle-element ()

  (assert (equal 1 (hydra_lib_sets_size (list 42)))))

(defun test-size-negempty-set ()

  (assert (equal 0 (hydra_lib_sets_size ()))))

;; null

(defun test-null-negempty-set ()

  (assert (equal cl:t (hydra_lib_sets_null ()))))

(defun test-null-negnon-negempty-set ()

  (assert (equal cl:nil (hydra_lib_sets_null (list 1 2)))))

;; union

(defun test-union-negunion-two-sets ()

  (assert (equal (list 1 2 3) ((hydra_lib_sets_union (list 1 2)) (list 2 3)))))

(defun test-union-negunion-with-empty ()

  (assert (equal (list 1 2) ((hydra_lib_sets_union (list 1 2)) ()))))

(defun test-union-negempty-with-non-negempty ()

  (assert (equal (list 1 2) ((hydra_lib_sets_union ()) (list 1 2)))))

;; unions

(defun test-unions-negunion-of-multiple-sets ()

  (assert (equal (list 1 2 3 4) (hydra_lib_sets_unions (list (list 1 2) (list 2 3) (list 3 4))))))

(defun test-unions-negunion-with-empty-sets ()

  (assert (equal (list 1 2 3) (hydra_lib_sets_unions (list (list 1 2) () (list 3))))))

(defun test-unions-negempty-list-of-sets ()

  (assert (equal () (hydra_lib_sets_unions (list )))))

(defun test-unions-negsingle-set ()

  (assert (equal (list 1 2 3) (hydra_lib_sets_unions (list (list 1 2 3))))))

;; intersection

(defun test-intersection-negcommon-elements ()

  (assert (equal (list 2 3) ((hydra_lib_sets_intersection (list 1 2 3)) (list 2 3 4)))))

(defun test-intersection-negno-common-elements ()

  (assert (equal () ((hydra_lib_sets_intersection (list 1 2)) (list 3 4)))))

(defun test-intersection-negintersection-with-empty ()

  (assert (equal () ((hydra_lib_sets_intersection (list 1 2)) ()))))

;; difference

(defun test-difference-negremove-elements ()

  (assert (equal (list 1 3) ((hydra_lib_sets_difference (list 1 2 3)) (list 2 4)))))

(defun test-difference-negno-overlap ()

  (assert (equal (list 1 2) ((hydra_lib_sets_difference (list 1 2)) (list 3 4)))))

(defun test-difference-negdifference-with-empty ()

  (assert (equal (list 1 2) ((hydra_lib_sets_difference (list 1 2)) ()))))

;; map

(defun test-map-negmap-function ()

  (assert (equal (list 2 4 6) ((hydra_lib_sets_map (cl:lambda (x) ((hydra_lib_math_mul x) 2))) (list 1 2 3)))))

(defun test-map-negmap-on-empty ()

  (assert (equal () ((hydra_lib_sets_map (cl:lambda (x) ((hydra_lib_math_mul x) 2))) ()))))
