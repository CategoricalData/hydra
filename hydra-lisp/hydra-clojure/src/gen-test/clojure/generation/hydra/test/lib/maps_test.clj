;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maps primitives

(ns generation.hydra.test.lib.maps-test
  (:require [clojure.test :refer :all]))

;; alter

(deftest test-maps-negalter-neginsert-new-key

  (is (= (list (list 1 "a") (list 2 "b") (list 3 "new"))

         (((hydra_lib_maps_alter (fn [opt] (list :just "new"))) 3) (list (list 1 "a") (list 2 "b"))))))

(deftest test-maps-negalter-negupdate-existing-key

  (is (= (list (list 1 "a") (list 2 "updated"))

         (((hydra_lib_maps_alter (fn [opt] (list :just "updated"))) 2) (list (list 1 "a") (list 2 "b"))))))

(deftest test-maps-negalter-negdelete-key

  (is (= (list (list 1 "a"))

         (((hydra_lib_maps_alter (fn [opt] (list :nothing))) 2) (list (list 1 "a") (list 2 "b"))))))

;; bimap

(deftest test-maps-negbimap-negtransform-both

  (is (= (list (list 2 "A") (list 4 "B"))

         (((hydra_lib_maps_bimap (fn [k] ((hydra_lib_math_mul k) 2))) (fn [v] (hydra_lib_strings_to_upper v))) (list (list 1 "a") (list 2 "b"))))))

(deftest test-maps-negbimap-negempty-map

  (is (= (list)

         (((hydra_lib_maps_bimap (fn [k] ((hydra_lib_math_mul k) 2))) (fn [v] (hydra_lib_strings_to_upper v))) (list)))))

;; elems

(deftest test-maps-negelems-negget-all-elements

  (is (= (list "a" "b")

         (hydra_lib_maps_elems (list (list 1 "a") (list 2 "b"))))))

(deftest test-maps-negelems-negunsorted-keys

  (is (= (list "a" "b" "c")

         (hydra_lib_maps_elems (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(deftest test-maps-negelems-negempty-map

  (is (= (list )

         (hydra_lib_maps_elems (list)))))

;; empty

(deftest test-maps-negempty-negempty-map

  (is (= (list)

         hydra_lib_maps_empty)))

;; filter

(deftest test-maps-negfilter-negfilter-values-starting-with-a

  (is (= (list (list 1 "a") (list 3 "ab"))

         ((hydra_lib_maps_filter (fn [v] ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) v)) 97))) (list (list 1 "a") (list 2 "b") (list 3 "ab"))))))

(deftest test-maps-negfilter-negfilter-all

  (is (= (list)

         ((hydra_lib_maps_filter (fn [v] ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) v)) 97))) (list (list 1 "b") (list 2 "c"))))))

(deftest test-maps-negfilter-negempty-map

  (is (= (list)

         ((hydra_lib_maps_filter (fn [v] ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) v)) 97))) (list)))))

;; filterWithKey

(deftest test-maps-negfilterwithkey-negfilter-by-key-1

  (is (= (list (list 2 "b") (list 3 "c"))

         ((hydra_lib_maps_filter_with_key (fn [k] (fn [v] ((hydra_lib_equality_gt k) 1)))) (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(deftest test-maps-negfilterwithkey-negfilter-all

  (is (= (list)

         ((hydra_lib_maps_filter_with_key (fn [k] (fn [v] ((hydra_lib_equality_gt k) 1)))) (list (list 1 "a"))))))

(deftest test-maps-negfilterwithkey-negempty-map

  (is (= (list)

         ((hydra_lib_maps_filter_with_key (fn [k] (fn [v] ((hydra_lib_equality_gt k) 1)))) (list)))))

;; findWithDefault

(deftest test-maps-negfindwithdefault-negfind-existing

  (is (= "b"

         (((hydra_lib_maps_find_with_default "default") 2) (list (list 1 "a") (list 2 "b"))))))

(deftest test-maps-negfindwithdefault-neguse-default

  (is (= "default"

         (((hydra_lib_maps_find_with_default "default") 3) (list (list 1 "a") (list 2 "b"))))))

;; fromList

(deftest test-maps-negfromlist-negcreate-from-pairs

  (is (= (list (list 1 "a") (list 2 "b"))

         (hydra_lib_maps_from_list (list (list 1 "a") (list 2 "b"))))))

(deftest test-maps-negfromlist-negduplicate-keys

  (is (= (list (list 1 "b"))

         (hydra_lib_maps_from_list (list (list 1 "a") (list 1 "b"))))))

(deftest test-maps-negfromlist-negempty-list

  (is (= (list)

         (hydra_lib_maps_from_list (list )))))

;; insert

(deftest test-maps-neginsert-neginsert-new-key

  (is (= (list (list 1 "a") (list 2 "b") (list 3 "c"))

         (((hydra_lib_maps_insert 3) "c") (list (list 1 "a") (list 2 "b"))))))

(deftest test-maps-neginsert-negupdate-existing

  (is (= (list (list 1 "a") (list 2 "updated"))

         (((hydra_lib_maps_insert 2) "updated") (list (list 1 "a") (list 2 "b"))))))

(deftest test-maps-neginsert-neginsert-into-empty

  (is (= (list (list 1 "x"))

         (((hydra_lib_maps_insert 1) "x") (list)))))

;; keys

(deftest test-maps-negkeys-negget-all-keys

  (is (= (list 1 2 3)

         (hydra_lib_maps_keys (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(deftest test-maps-negkeys-negunsorted-keys

  (is (= (list 1 2 3)

         (hydra_lib_maps_keys (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(deftest test-maps-negkeys-negempty-map

  (is (= (list )

         (hydra_lib_maps_keys (list)))))

;; lookup

(deftest test-maps-neglookup-negfind-existing-key

  (is (= (list :just "b")

         ((hydra_lib_maps_lookup 2) (list (list 1 "a") (list 2 "b"))))))

(deftest test-maps-neglookup-negkey-not-found

  (is (= (list :nothing)

         ((hydra_lib_maps_lookup 3) (list (list 1 "a") (list 2 "b"))))))

(deftest test-maps-neglookup-neglookup-in-empty

  (is (= (list :nothing)

         ((hydra_lib_maps_lookup 1) (list)))))

;; map

(deftest test-maps-negmap-negmap-over-values

  (is (= (list (list 1 "A") (list 2 "B"))

         ((hydra_lib_maps_map (fn [s] (hydra_lib_strings_to_upper s))) (list (list 1 "a") (list 2 "b"))))))

(deftest test-maps-negmap-negmap-empty

  (is (= (list)

         ((hydra_lib_maps_map (fn [s] (hydra_lib_strings_to_upper s))) (list)))))

;; mapKeys

(deftest test-maps-negmapkeys-negdouble-keys

  (is (= (list (list 2 "a") (list 4 "b"))

         ((hydra_lib_maps_map_keys (fn [k] ((hydra_lib_math_mul k) 2))) (list (list 1 "a") (list 2 "b"))))))

(deftest test-maps-negmapkeys-negempty-map

  (is (= (list)

         ((hydra_lib_maps_map_keys (fn [k] ((hydra_lib_math_mul k) 2))) (list)))))

;; member

(deftest test-maps-negmember-negkey-exists

  (is (= true

         ((hydra_lib_maps_member 2) (list (list 1 "a") (list 2 "b"))))))

(deftest test-maps-negmember-negkey-missing

  (is (= false

         ((hydra_lib_maps_member 3) (list (list 1 "a") (list 2 "b"))))))

(deftest test-maps-negmember-negempty-map

  (is (= false

         ((hydra_lib_maps_member 1) (list)))))

;; null

(deftest test-maps-negnull-negempty-map

  (is (= true

         (hydra_lib_maps_null (list)))))

(deftest test-maps-negnull-negnon-negempty-map

  (is (= false

         (hydra_lib_maps_null (list (list 1 "a"))))))

;; remove

(deftest test-maps-negremove-negremove-existing

  (is (= (list (list 1 "a") (list 3 "c"))

         ((hydra_lib_maps_delete 2) (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(deftest test-maps-negremove-negremove-non-negexisting

  (is (= (list (list 1 "a") (list 2 "b"))

         ((hydra_lib_maps_delete 4) (list (list 1 "a") (list 2 "b"))))))

(deftest test-maps-negremove-negremove-from-empty

  (is (= (list)

         ((hydra_lib_maps_delete 1) (list)))))

;; singleton

(deftest test-maps-negsingleton-negsingle-entry

  (is (= (list (list 42 "hello"))

         ((hydra_lib_maps_singleton 42) "hello"))))

;; size

(deftest test-maps-negsize-negthree-entries

  (is (= 3

         (hydra_lib_maps_size (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(deftest test-maps-negsize-negsingle-entry

  (is (= 1

         (hydra_lib_maps_size (list (list 42 "test"))))))

(deftest test-maps-negsize-negempty-map

  (is (= 0

         (hydra_lib_maps_size (list)))))

;; toList

(deftest test-maps-negtolist-negconvert-to-pairs

  (is (= (list (list 1 "a") (list 2 "b"))

         (hydra_lib_maps_to_list (list (list 1 "a") (list 2 "b"))))))

(deftest test-maps-negtolist-negunsorted-keys

  (is (= (list (list 1 "a") (list 2 "b") (list 3 "c"))

         (hydra_lib_maps_to_list (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(deftest test-maps-negtolist-negempty-map

  (is (= (list )

         (hydra_lib_maps_to_list (list)))))

;; union

(deftest test-maps-negunion-negunion-two-maps

  (is (= (list (list 1 "a") (list 2 "b") (list 3 "c"))

         ((hydra_lib_maps_union (list (list 1 "a") (list 2 "b"))) (list (list 2 "x") (list 3 "c"))))))

(deftest test-maps-negunion-negunion-with-empty

  (is (= (list (list 1 "a"))

         ((hydra_lib_maps_union (list (list 1 "a"))) (list)))))

(deftest test-maps-negunion-negempty-with-map

  (is (= (list (list 1 "a"))

         ((hydra_lib_maps_union (list)) (list (list 1 "a"))))))
