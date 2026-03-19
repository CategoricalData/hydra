;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.sets primitives

(require 'ert)

;; empty

(ert-deftest test-sets-negempty-negempty-set ()

  (should (equal (list) hydra_lib_sets_empty)))

;; singleton

(ert-deftest test-sets-negsingleton-negsingle-element ()

  (should (equal (list 42) (funcall hydra_lib_sets_singleton 42))))

;; fromList

(ert-deftest test-sets-negfromlist-negcreate-from-list ()

  (should (equal (list 1 2 3) (funcall hydra_lib_sets_from_list (list 1 2 3)))))

(ert-deftest test-sets-negfromlist-negduplicates-removed ()

  (should (equal (list 1 2 3) (funcall hydra_lib_sets_from_list (list 1 2 1 3)))))

(ert-deftest test-sets-negfromlist-negempty-list ()

  (should (equal (list) (funcall hydra_lib_sets_from_list (list )))))

;; toList

(ert-deftest test-sets-negtolist-negconvert-to-list ()

  (should (equal (list 1 2 3) (funcall hydra_lib_sets_to_list (list 1 2 3)))))

(ert-deftest test-sets-negtolist-negunsorted-input ()

  (should (equal (list 1 2 3) (funcall hydra_lib_sets_to_list (list 1 2 3)))))

(ert-deftest test-sets-negtolist-negempty-set ()

  (should (equal (list ) (funcall hydra_lib_sets_to_list (list)))))

;; insert

(ert-deftest test-sets-neginsert-neginsert-new-element ()

  (should (equal (list 1 2 3 4) (funcall (funcall hydra_lib_sets_insert 4) (list 1 2 3)))))

(ert-deftest test-sets-neginsert-neginsert-existing-element ()

  (should (equal (list 1 2 3) (funcall (funcall hydra_lib_sets_insert 2) (list 1 2 3)))))

(ert-deftest test-sets-neginsert-neginsert-into-empty ()

  (should (equal (list 1) (funcall (funcall hydra_lib_sets_insert 1) (list)))))

;; delete

(ert-deftest test-sets-negdelete-negdelete-existing ()

  (should (equal (list 1 3) (funcall (funcall hydra_lib_sets_delete 2) (list 1 2 3)))))

(ert-deftest test-sets-negdelete-negdelete-non-negexisting ()

  (should (equal (list 1 2 3) (funcall (funcall hydra_lib_sets_delete 4) (list 1 2 3)))))

(ert-deftest test-sets-negdelete-negdelete-from-empty ()

  (should (equal (list) (funcall (funcall hydra_lib_sets_delete 1) (list)))))

;; member

(ert-deftest test-sets-negmember-negelement-exists ()

  (should (equal t (funcall (funcall hydra_lib_sets_member 2) (list 1 2 3)))))

(ert-deftest test-sets-negmember-negelement-missing ()

  (should (equal nil (funcall (funcall hydra_lib_sets_member 4) (list 1 2 3)))))

(ert-deftest test-sets-negmember-negempty-set ()

  (should (equal nil (funcall (funcall hydra_lib_sets_member 1) (list)))))

;; size

(ert-deftest test-sets-negsize-negthree-elements ()

  (should (equal 3 (funcall hydra_lib_sets_size (list 1 2 3)))))

(ert-deftest test-sets-negsize-negsingle-element ()

  (should (equal 1 (funcall hydra_lib_sets_size (list 42)))))

(ert-deftest test-sets-negsize-negempty-set ()

  (should (equal 0 (funcall hydra_lib_sets_size (list)))))

;; null

(ert-deftest test-sets-negnull-negempty-set ()

  (should (equal t (funcall hydra_lib_sets_null (list)))))

(ert-deftest test-sets-negnull-negnon-negempty-set ()

  (should (equal nil (funcall hydra_lib_sets_null (list 1 2)))))

;; union

(ert-deftest test-sets-negunion-negunion-two-sets ()

  (should (equal (list 1 2 3) (funcall (funcall hydra_lib_sets_union (list 1 2)) (list 2 3)))))

(ert-deftest test-sets-negunion-negunion-with-empty ()

  (should (equal (list 1 2) (funcall (funcall hydra_lib_sets_union (list 1 2)) (list)))))

(ert-deftest test-sets-negunion-negempty-with-non-negempty ()

  (should (equal (list 1 2) (funcall (funcall hydra_lib_sets_union (list)) (list 1 2)))))

;; unions

(ert-deftest test-sets-negunions-negunion-of-multiple-sets ()

  (should (equal (list 1 2 3 4) (funcall hydra_lib_sets_unions (list (list 1 2) (list 2 3) (list 3 4))))))

(ert-deftest test-sets-negunions-negunion-with-empty-sets ()

  (should (equal (list 1 2 3) (funcall hydra_lib_sets_unions (list (list 1 2) (list) (list 3))))))

(ert-deftest test-sets-negunions-negempty-list-of-sets ()

  (should (equal (list) (funcall hydra_lib_sets_unions (list )))))

(ert-deftest test-sets-negunions-negsingle-set ()

  (should (equal (list 1 2 3) (funcall hydra_lib_sets_unions (list (list 1 2 3))))))

;; intersection

(ert-deftest test-sets-negintersection-negcommon-elements ()

  (should (equal (list 2 3) (funcall (funcall hydra_lib_sets_intersection (list 1 2 3)) (list 2 3 4)))))

(ert-deftest test-sets-negintersection-negno-common-elements ()

  (should (equal (list) (funcall (funcall hydra_lib_sets_intersection (list 1 2)) (list 3 4)))))

(ert-deftest test-sets-negintersection-negintersection-with-empty ()

  (should (equal (list) (funcall (funcall hydra_lib_sets_intersection (list 1 2)) (list)))))

;; difference

(ert-deftest test-sets-negdifference-negremove-elements ()

  (should (equal (list 1 3) (funcall (funcall hydra_lib_sets_difference (list 1 2 3)) (list 2 4)))))

(ert-deftest test-sets-negdifference-negno-overlap ()

  (should (equal (list 1 2) (funcall (funcall hydra_lib_sets_difference (list 1 2)) (list 3 4)))))

(ert-deftest test-sets-negdifference-negdifference-with-empty ()

  (should (equal (list 1 2) (funcall (funcall hydra_lib_sets_difference (list 1 2)) (list)))))

;; map

(ert-deftest test-sets-negmap-negmap-function ()

  (should (equal (list 2 4 6) (funcall (funcall hydra_lib_sets_map (lambda (x) (funcall (hydra_lib_math_mul x) 2))) (list 1 2 3)))))

(ert-deftest test-sets-negmap-negmap-on-empty ()

  (should (equal (list) (funcall (funcall hydra_lib_sets_map (lambda (x) (funcall (hydra_lib_math_mul x) 2))) (list)))))
