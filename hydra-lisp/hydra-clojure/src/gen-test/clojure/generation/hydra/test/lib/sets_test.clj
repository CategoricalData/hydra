;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.sets primitives

(ns generation.hydra.test.lib.sets-test
  (:require [clojure.test :refer :all]))

;; empty

(deftest test-empty-negempty-set

  (is (= ()

         hydra_lib_sets_empty)))

;; singleton

(deftest test-singleton-negsingle-element

  (is (= (list 42)

         (hydra_lib_sets_singleton 42))))

;; fromList

(deftest test-fromlist-negcreate-from-list

  (is (= (list 1 2 3)

         (hydra_lib_sets_from_list (list 1 2 3)))))

(deftest test-fromlist-negduplicates-removed

  (is (= (list 1 2 3)

         (hydra_lib_sets_from_list (list 1 2 1 3)))))

(deftest test-fromlist-negempty-list

  (is (= ()

         (hydra_lib_sets_from_list (list )))))

;; toList

(deftest test-tolist-negconvert-to-list

  (is (= (list 1 2 3)

         (hydra_lib_sets_to_list (list 1 2 3)))))

(deftest test-tolist-negunsorted-input

  (is (= (list 1 2 3)

         (hydra_lib_sets_to_list (list 1 2 3)))))

(deftest test-tolist-negempty-set

  (is (= (list )

         (hydra_lib_sets_to_list ()))))

;; insert

(deftest test-insert-neginsert-new-element

  (is (= (list 1 2 3 4)

         ((hydra_lib_sets_insert 4) (list 1 2 3)))))

(deftest test-insert-neginsert-existing-element

  (is (= (list 1 2 3)

         ((hydra_lib_sets_insert 2) (list 1 2 3)))))

(deftest test-insert-neginsert-into-empty

  (is (= (list 1)

         ((hydra_lib_sets_insert 1) ()))))

;; delete

(deftest test-delete-negdelete-existing

  (is (= (list 1 3)

         ((hydra_lib_sets_delete 2) (list 1 2 3)))))

(deftest test-delete-negdelete-non-negexisting

  (is (= (list 1 2 3)

         ((hydra_lib_sets_delete 4) (list 1 2 3)))))

(deftest test-delete-negdelete-from-empty

  (is (= ()

         ((hydra_lib_sets_delete 1) ()))))

;; member

(deftest test-member-negelement-exists

  (is (= true

         ((hydra_lib_sets_member 2) (list 1 2 3)))))

(deftest test-member-negelement-missing

  (is (= false

         ((hydra_lib_sets_member 4) (list 1 2 3)))))

(deftest test-member-negempty-set

  (is (= false

         ((hydra_lib_sets_member 1) ()))))

;; size

(deftest test-size-negthree-elements

  (is (= 3

         (hydra_lib_sets_size (list 1 2 3)))))

(deftest test-size-negsingle-element

  (is (= 1

         (hydra_lib_sets_size (list 42)))))

(deftest test-size-negempty-set

  (is (= 0

         (hydra_lib_sets_size ()))))

;; null

(deftest test-null-negempty-set

  (is (= true

         (hydra_lib_sets_null ()))))

(deftest test-null-negnon-negempty-set

  (is (= false

         (hydra_lib_sets_null (list 1 2)))))

;; union

(deftest test-union-negunion-two-sets

  (is (= (list 1 2 3)

         ((hydra_lib_sets_union (list 1 2)) (list 2 3)))))

(deftest test-union-negunion-with-empty

  (is (= (list 1 2)

         ((hydra_lib_sets_union (list 1 2)) ()))))

(deftest test-union-negempty-with-non-negempty

  (is (= (list 1 2)

         ((hydra_lib_sets_union ()) (list 1 2)))))

;; unions

(deftest test-unions-negunion-of-multiple-sets

  (is (= (list 1 2 3 4)

         (hydra_lib_sets_unions (list (list 1 2) (list 2 3) (list 3 4))))))

(deftest test-unions-negunion-with-empty-sets

  (is (= (list 1 2 3)

         (hydra_lib_sets_unions (list (list 1 2) () (list 3))))))

(deftest test-unions-negempty-list-of-sets

  (is (= ()

         (hydra_lib_sets_unions (list )))))

(deftest test-unions-negsingle-set

  (is (= (list 1 2 3)

         (hydra_lib_sets_unions (list (list 1 2 3))))))

;; intersection

(deftest test-intersection-negcommon-elements

  (is (= (list 2 3)

         ((hydra_lib_sets_intersection (list 1 2 3)) (list 2 3 4)))))

(deftest test-intersection-negno-common-elements

  (is (= ()

         ((hydra_lib_sets_intersection (list 1 2)) (list 3 4)))))

(deftest test-intersection-negintersection-with-empty

  (is (= ()

         ((hydra_lib_sets_intersection (list 1 2)) ()))))

;; difference

(deftest test-difference-negremove-elements

  (is (= (list 1 3)

         ((hydra_lib_sets_difference (list 1 2 3)) (list 2 4)))))

(deftest test-difference-negno-overlap

  (is (= (list 1 2)

         ((hydra_lib_sets_difference (list 1 2)) (list 3 4)))))

(deftest test-difference-negdifference-with-empty

  (is (= (list 1 2)

         ((hydra_lib_sets_difference (list 1 2)) ()))))

;; map

(deftest test-map-negmap-function

  (is (= (list 2 4 6)

         ((hydra_lib_sets_map (fn [x] ((hydra_lib_math_mul x) 2))) (list 1 2 3)))))

(deftest test-map-negmap-on-empty

  (is (= ()

         ((hydra_lib_sets_map (fn [x] ((hydra_lib_math_mul x) 2))) ()))))
