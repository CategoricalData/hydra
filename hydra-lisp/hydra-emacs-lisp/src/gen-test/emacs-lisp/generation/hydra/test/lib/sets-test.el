;;; Note: this is an automatically generated file. Do not edit.
;;; hydra.lib.sets primitives

(require 'ert)

;; empty

(ert-deftest test-empty-negempty-set ()

  (should (equal () hydra_lib_sets_empty)))

;; singleton

(ert-deftest test-singleton-negsingle-element ()

  (should (equal (list 42) (hydra_lib_sets_singleton 42))))

;; fromList

(ert-deftest test-fromlist-negcreate-from-list ()

  (should (equal (list 1 2 3) (hydra_lib_sets_from_list (list 1 2 3)))))

(ert-deftest test-fromlist-negduplicates-removed ()

  (should (equal (list 1 2 3) (hydra_lib_sets_from_list (list 1 2 1 3)))))

(ert-deftest test-fromlist-negempty-list ()

  (should (equal () (hydra_lib_sets_from_list (list )))))

;; toList

(ert-deftest test-tolist-negconvert-to-list ()

  (should (equal (list 1 2 3) (hydra_lib_sets_to_list (list 1 2 3)))))

(ert-deftest test-tolist-negunsorted-input ()

  (should (equal (list 1 2 3) (hydra_lib_sets_to_list (list 1 2 3)))))

(ert-deftest test-tolist-negempty-set ()

  (should (equal (list ) (hydra_lib_sets_to_list ()))))

;; insert

(ert-deftest test-insert-neginsert-new-element ()

  (should (equal (list 1 2 3 4) ((hydra_lib_sets_insert 4) (list 1 2 3)))))

(ert-deftest test-insert-neginsert-existing-element ()

  (should (equal (list 1 2 3) ((hydra_lib_sets_insert 2) (list 1 2 3)))))

(ert-deftest test-insert-neginsert-into-empty ()

  (should (equal (list 1) ((hydra_lib_sets_insert 1) ()))))

;; delete

(ert-deftest test-delete-negdelete-existing ()

  (should (equal (list 1 3) ((hydra_lib_sets_delete 2) (list 1 2 3)))))

(ert-deftest test-delete-negdelete-non-negexisting ()

  (should (equal (list 1 2 3) ((hydra_lib_sets_delete 4) (list 1 2 3)))))

(ert-deftest test-delete-negdelete-from-empty ()

  (should (equal () ((hydra_lib_sets_delete 1) ()))))

;; member

(ert-deftest test-member-negelement-exists ()

  (should (equal t ((hydra_lib_sets_member 2) (list 1 2 3)))))

(ert-deftest test-member-negelement-missing ()

  (should (equal nil ((hydra_lib_sets_member 4) (list 1 2 3)))))

(ert-deftest test-member-negempty-set ()

  (should (equal nil ((hydra_lib_sets_member 1) ()))))

;; size

(ert-deftest test-size-negthree-elements ()

  (should (equal 3 (hydra_lib_sets_size (list 1 2 3)))))

(ert-deftest test-size-negsingle-element ()

  (should (equal 1 (hydra_lib_sets_size (list 42)))))

(ert-deftest test-size-negempty-set ()

  (should (equal 0 (hydra_lib_sets_size ()))))

;; null

(ert-deftest test-null-negempty-set ()

  (should (equal t (hydra_lib_sets_null ()))))

(ert-deftest test-null-negnon-negempty-set ()

  (should (equal nil (hydra_lib_sets_null (list 1 2)))))

;; union

(ert-deftest test-union-negunion-two-sets ()

  (should (equal (list 1 2 3) ((hydra_lib_sets_union (list 1 2)) (list 2 3)))))

(ert-deftest test-union-negunion-with-empty ()

  (should (equal (list 1 2) ((hydra_lib_sets_union (list 1 2)) ()))))

(ert-deftest test-union-negempty-with-non-negempty ()

  (should (equal (list 1 2) ((hydra_lib_sets_union ()) (list 1 2)))))

;; unions

(ert-deftest test-unions-negunion-of-multiple-sets ()

  (should (equal (list 1 2 3 4) (hydra_lib_sets_unions (list (list 1 2) (list 2 3) (list 3 4))))))

(ert-deftest test-unions-negunion-with-empty-sets ()

  (should (equal (list 1 2 3) (hydra_lib_sets_unions (list (list 1 2) () (list 3))))))

(ert-deftest test-unions-negempty-list-of-sets ()

  (should (equal () (hydra_lib_sets_unions (list )))))

(ert-deftest test-unions-negsingle-set ()

  (should (equal (list 1 2 3) (hydra_lib_sets_unions (list (list 1 2 3))))))

;; intersection

(ert-deftest test-intersection-negcommon-elements ()

  (should (equal (list 2 3) ((hydra_lib_sets_intersection (list 1 2 3)) (list 2 3 4)))))

(ert-deftest test-intersection-negno-common-elements ()

  (should (equal () ((hydra_lib_sets_intersection (list 1 2)) (list 3 4)))))

(ert-deftest test-intersection-negintersection-with-empty ()

  (should (equal () ((hydra_lib_sets_intersection (list 1 2)) ()))))

;; difference

(ert-deftest test-difference-negremove-elements ()

  (should (equal (list 1 3) ((hydra_lib_sets_difference (list 1 2 3)) (list 2 4)))))

(ert-deftest test-difference-negno-overlap ()

  (should (equal (list 1 2) ((hydra_lib_sets_difference (list 1 2)) (list 3 4)))))

(ert-deftest test-difference-negdifference-with-empty ()

  (should (equal (list 1 2) ((hydra_lib_sets_difference (list 1 2)) ()))))

;; map

(ert-deftest test-map-negmap-function ()

  (should (equal (list 2 4 6) ((hydra_lib_sets_map (lambda (x) ((hydra_lib_math_mul x) 2))) (list 1 2 3)))))

(ert-deftest test-map-negmap-on-empty ()

  (should (equal () ((hydra_lib_sets_map (lambda (x) ((hydra_lib_math_mul x) 2))) ()))))
