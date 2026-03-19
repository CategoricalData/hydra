;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.sets primitives

(ns generation.hydra.test.lib.sets-test
  (:require [clojure.test :refer :all]))

;; empty

(deftest test-sets-negempty-negempty-set

  (is (= (list)

         hydra_lib_sets_empty)))

;; singleton

(deftest test-sets-negsingleton-negsingle-element

  (is (= (list 42)

         (hydra_lib_sets_singleton 42))))

;; fromList

(deftest test-sets-negfromlist-negcreate-from-list

  (is (= (list 1 2 3)

         (hydra_lib_sets_from_list (list 1 2 3)))))

(deftest test-sets-negfromlist-negduplicates-removed

  (is (= (list 1 2 3)

         (hydra_lib_sets_from_list (list 1 2 1 3)))))

(deftest test-sets-negfromlist-negempty-list

  (is (= (list)

         (hydra_lib_sets_from_list (list )))))

;; toList

(deftest test-sets-negtolist-negconvert-to-list

  (is (= (list 1 2 3)

         (hydra_lib_sets_to_list (list 1 2 3)))))

(deftest test-sets-negtolist-negunsorted-input

  (is (= (list 1 2 3)

         (hydra_lib_sets_to_list (list 1 2 3)))))

(deftest test-sets-negtolist-negempty-set

  (is (= (list )

         (hydra_lib_sets_to_list (list)))))

;; insert

(deftest test-sets-neginsert-neginsert-new-element

  (is (= (list 1 2 3 4)

         ((hydra_lib_sets_insert 4) (list 1 2 3)))))

(deftest test-sets-neginsert-neginsert-existing-element

  (is (= (list 1 2 3)

         ((hydra_lib_sets_insert 2) (list 1 2 3)))))

(deftest test-sets-neginsert-neginsert-into-empty

  (is (= (list 1)

         ((hydra_lib_sets_insert 1) (list)))))

;; delete

(deftest test-sets-negdelete-negdelete-existing

  (is (= (list 1 3)

         ((hydra_lib_sets_delete 2) (list 1 2 3)))))

(deftest test-sets-negdelete-negdelete-non-negexisting

  (is (= (list 1 2 3)

         ((hydra_lib_sets_delete 4) (list 1 2 3)))))

(deftest test-sets-negdelete-negdelete-from-empty

  (is (= (list)

         ((hydra_lib_sets_delete 1) (list)))))

;; member

(deftest test-sets-negmember-negelement-exists

  (is (= true

         ((hydra_lib_sets_member 2) (list 1 2 3)))))

(deftest test-sets-negmember-negelement-missing

  (is (= false

         ((hydra_lib_sets_member 4) (list 1 2 3)))))

(deftest test-sets-negmember-negempty-set

  (is (= false

         ((hydra_lib_sets_member 1) (list)))))

;; size

(deftest test-sets-negsize-negthree-elements

  (is (= 3

         (hydra_lib_sets_size (list 1 2 3)))))

(deftest test-sets-negsize-negsingle-element

  (is (= 1

         (hydra_lib_sets_size (list 42)))))

(deftest test-sets-negsize-negempty-set

  (is (= 0

         (hydra_lib_sets_size (list)))))

;; null

(deftest test-sets-negnull-negempty-set

  (is (= true

         (hydra_lib_sets_null (list)))))

(deftest test-sets-negnull-negnon-negempty-set

  (is (= false

         (hydra_lib_sets_null (list 1 2)))))

;; union

(deftest test-sets-negunion-negunion-two-sets

  (is (= (list 1 2 3)

         ((hydra_lib_sets_union (list 1 2)) (list 2 3)))))

(deftest test-sets-negunion-negunion-with-empty

  (is (= (list 1 2)

         ((hydra_lib_sets_union (list 1 2)) (list)))))

(deftest test-sets-negunion-negempty-with-non-negempty

  (is (= (list 1 2)

         ((hydra_lib_sets_union (list)) (list 1 2)))))

;; unions

(deftest test-sets-negunions-negunion-of-multiple-sets

  (is (= (list 1 2 3 4)

         (hydra_lib_sets_unions (list (list 1 2) (list 2 3) (list 3 4))))))

(deftest test-sets-negunions-negunion-with-empty-sets

  (is (= (list 1 2 3)

         (hydra_lib_sets_unions (list (list 1 2) (list) (list 3))))))

(deftest test-sets-negunions-negempty-list-of-sets

  (is (= (list)

         (hydra_lib_sets_unions (list )))))

(deftest test-sets-negunions-negsingle-set

  (is (= (list 1 2 3)

         (hydra_lib_sets_unions (list (list 1 2 3))))))

;; intersection

(deftest test-sets-negintersection-negcommon-elements

  (is (= (list 2 3)

         ((hydra_lib_sets_intersection (list 1 2 3)) (list 2 3 4)))))

(deftest test-sets-negintersection-negno-common-elements

  (is (= (list)

         ((hydra_lib_sets_intersection (list 1 2)) (list 3 4)))))

(deftest test-sets-negintersection-negintersection-with-empty

  (is (= (list)

         ((hydra_lib_sets_intersection (list 1 2)) (list)))))

;; difference

(deftest test-sets-negdifference-negremove-elements

  (is (= (list 1 3)

         ((hydra_lib_sets_difference (list 1 2 3)) (list 2 4)))))

(deftest test-sets-negdifference-negno-overlap

  (is (= (list 1 2)

         ((hydra_lib_sets_difference (list 1 2)) (list 3 4)))))

(deftest test-sets-negdifference-negdifference-with-empty

  (is (= (list 1 2)

         ((hydra_lib_sets_difference (list 1 2)) (list)))))

;; map

(deftest test-sets-negmap-negmap-function

  (is (= (list 2 4 6)

         ((hydra_lib_sets_map (fn [x] ((hydra_lib_math_mul x) 2))) (list 1 2 3)))))

(deftest test-sets-negmap-negmap-on-empty

  (is (= (list)

         ((hydra_lib_sets_map (fn [x] ((hydra_lib_math_mul x) 2))) (list)))))
