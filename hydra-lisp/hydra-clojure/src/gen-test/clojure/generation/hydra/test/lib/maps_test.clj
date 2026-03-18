;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maps primitives

(ns generation.hydra.test.lib.maps-test
  (:require [clojure.test :refer :all]))

;; alter

(deftest test-alter-neginsert-new-key

  (is (= (list (list 1 "a") (list 2 "b") (list 3 "new"))

         (((hydra_lib_maps_alter (fn [opt] "new")) 3) (list (list 1 "a") (list 2 "b"))))))

(deftest test-alter-negupdate-existing-key

  (is (= (list (list 1 "a") (list 2 "updated"))

         (((hydra_lib_maps_alter (fn [opt] "updated")) 2) (list (list 1 "a") (list 2 "b"))))))

(deftest test-alter-negdelete-key

  (is (= (list (list 1 "a"))

         (((hydra_lib_maps_alter (fn [opt] nil)) 2) (list (list 1 "a") (list 2 "b"))))))

;; bimap

(deftest test-bimap-negtransform-both

  (is (= (list (list 2 "A") (list 4 "B"))

         (((hydra_lib_maps_bimap (fn [k] ((hydra_lib_math_mul k) 2))) (fn [v] (hydra_lib_strings_to_upper v))) (list (list 1 "a") (list 2 "b"))))))

(deftest test-bimap-negempty-map

  (is (= ()

         (((hydra_lib_maps_bimap (fn [k] ((hydra_lib_math_mul k) 2))) (fn [v] (hydra_lib_strings_to_upper v))) ()))))

;; elems

(deftest test-elems-negget-all-elements

  (is (= (list "a" "b")

         (hydra_lib_maps_elems (list (list 1 "a") (list 2 "b"))))))

(deftest test-elems-negunsorted-keys

  (is (= (list "a" "b" "c")

         (hydra_lib_maps_elems (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(deftest test-elems-negempty-map

  (is (= (list )

         (hydra_lib_maps_elems ()))))

;; empty

(deftest test-empty-negempty-map

  (is (= ()

         hydra_lib_maps_empty)))

;; filter

(deftest test-filter-negfilter-values-starting-with-a

  (is (= (list (list 1 "a") (list 3 "ab"))

         ((hydra_lib_maps_filter (fn [v] ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) v)) 97))) (list (list 1 "a") (list 2 "b") (list 3 "ab"))))))

(deftest test-filter-negfilter-all

  (is (= ()

         ((hydra_lib_maps_filter (fn [v] ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) v)) 97))) (list (list 1 "b") (list 2 "c"))))))

(deftest test-filter-negempty-map

  (is (= ()

         ((hydra_lib_maps_filter (fn [v] ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) v)) 97))) ()))))

;; filterWithKey

(deftest test-filterwithkey-negfilter-by-key-1

  (is (= (list (list 2 "b") (list 3 "c"))

         ((hydra_lib_maps_filter_with_key (fn [k] (fn [v] ((hydra_lib_equality_gt k) 1)))) (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(deftest test-filterwithkey-negfilter-all

  (is (= ()

         ((hydra_lib_maps_filter_with_key (fn [k] (fn [v] ((hydra_lib_equality_gt k) 1)))) (list (list 1 "a"))))))

(deftest test-filterwithkey-negempty-map

  (is (= ()

         ((hydra_lib_maps_filter_with_key (fn [k] (fn [v] ((hydra_lib_equality_gt k) 1)))) ()))))

;; findWithDefault

(deftest test-findwithdefault-negfind-existing

  (is (= "b"

         (((hydra_lib_maps_find_with_default "default") 2) (list (list 1 "a") (list 2 "b"))))))

(deftest test-findwithdefault-neguse-default

  (is (= "default"

         (((hydra_lib_maps_find_with_default "default") 3) (list (list 1 "a") (list 2 "b"))))))

;; fromList

(deftest test-fromlist-negcreate-from-pairs

  (is (= (list (list 1 "a") (list 2 "b"))

         (hydra_lib_maps_from_list (list (list 1 "a") (list 2 "b"))))))

(deftest test-fromlist-negduplicate-keys

  (is (= (list (list 1 "b"))

         (hydra_lib_maps_from_list (list (list 1 "a") (list 1 "b"))))))

(deftest test-fromlist-negempty-list

  (is (= ()

         (hydra_lib_maps_from_list (list )))))

;; insert

(deftest test-insert-neginsert-new-key

  (is (= (list (list 1 "a") (list 2 "b") (list 3 "c"))

         (((hydra_lib_maps_insert 3) "c") (list (list 1 "a") (list 2 "b"))))))

(deftest test-insert-negupdate-existing

  (is (= (list (list 1 "a") (list 2 "updated"))

         (((hydra_lib_maps_insert 2) "updated") (list (list 1 "a") (list 2 "b"))))))

(deftest test-insert-neginsert-into-empty

  (is (= (list (list 1 "x"))

         (((hydra_lib_maps_insert 1) "x") ()))))

;; keys

(deftest test-keys-negget-all-keys

  (is (= (list 1 2 3)

         (hydra_lib_maps_keys (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(deftest test-keys-negunsorted-keys

  (is (= (list 1 2 3)

         (hydra_lib_maps_keys (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(deftest test-keys-negempty-map

  (is (= (list )

         (hydra_lib_maps_keys ()))))

;; lookup

(deftest test-lookup-negfind-existing-key

  (is (= "b"

         ((hydra_lib_maps_lookup 2) (list (list 1 "a") (list 2 "b"))))))

(deftest test-lookup-negkey-not-found

  (is (= nil

         ((hydra_lib_maps_lookup 3) (list (list 1 "a") (list 2 "b"))))))

(deftest test-lookup-neglookup-in-empty

  (is (= nil

         ((hydra_lib_maps_lookup 1) ()))))

;; map

(deftest test-map-negmap-over-values

  (is (= (list (list 1 "A") (list 2 "B"))

         ((hydra_lib_maps_map (fn [s] (hydra_lib_strings_to_upper s))) (list (list 1 "a") (list 2 "b"))))))

(deftest test-map-negmap-empty

  (is (= ()

         ((hydra_lib_maps_map (fn [s] (hydra_lib_strings_to_upper s))) ()))))

;; mapKeys

(deftest test-mapkeys-negdouble-keys

  (is (= (list (list 2 "a") (list 4 "b"))

         ((hydra_lib_maps_map_keys (fn [k] ((hydra_lib_math_mul k) 2))) (list (list 1 "a") (list 2 "b"))))))

(deftest test-mapkeys-negempty-map

  (is (= ()

         ((hydra_lib_maps_map_keys (fn [k] ((hydra_lib_math_mul k) 2))) ()))))

;; member

(deftest test-member-negkey-exists

  (is (= true

         ((hydra_lib_maps_member 2) (list (list 1 "a") (list 2 "b"))))))

(deftest test-member-negkey-missing

  (is (= false

         ((hydra_lib_maps_member 3) (list (list 1 "a") (list 2 "b"))))))

(deftest test-member-negempty-map

  (is (= false

         ((hydra_lib_maps_member 1) ()))))

;; null

(deftest test-null-negempty-map

  (is (= true

         (hydra_lib_maps_null ()))))

(deftest test-null-negnon-negempty-map

  (is (= false

         (hydra_lib_maps_null (list (list 1 "a"))))))

;; remove

(deftest test-remove-negremove-existing

  (is (= (list (list 1 "a") (list 3 "c"))

         ((hydra_lib_maps_delete 2) (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(deftest test-remove-negremove-non-negexisting

  (is (= (list (list 1 "a") (list 2 "b"))

         ((hydra_lib_maps_delete 4) (list (list 1 "a") (list 2 "b"))))))

(deftest test-remove-negremove-from-empty

  (is (= ()

         ((hydra_lib_maps_delete 1) ()))))

;; singleton

(deftest test-singleton-negsingle-entry

  (is (= (list (list 42 "hello"))

         ((hydra_lib_maps_singleton 42) "hello"))))

;; size

(deftest test-size-negthree-entries

  (is (= 3

         (hydra_lib_maps_size (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(deftest test-size-negsingle-entry

  (is (= 1

         (hydra_lib_maps_size (list (list 42 "test"))))))

(deftest test-size-negempty-map

  (is (= 0

         (hydra_lib_maps_size ()))))

;; toList

(deftest test-tolist-negconvert-to-pairs

  (is (= (list (list 1 "a") (list 2 "b"))

         (hydra_lib_maps_to_list (list (list 1 "a") (list 2 "b"))))))

(deftest test-tolist-negunsorted-keys

  (is (= (list (list 1 "a") (list 2 "b") (list 3 "c"))

         (hydra_lib_maps_to_list (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(deftest test-tolist-negempty-map

  (is (= (list )

         (hydra_lib_maps_to_list ()))))

;; union

(deftest test-union-negunion-two-maps

  (is (= (list (list 1 "a") (list 2 "b") (list 3 "c"))

         ((hydra_lib_maps_union (list (list 1 "a") (list 2 "b"))) (list (list 2 "x") (list 3 "c"))))))

(deftest test-union-negunion-with-empty

  (is (= (list (list 1 "a"))

         ((hydra_lib_maps_union (list (list 1 "a"))) ()))))

(deftest test-union-negempty-with-map

  (is (= (list (list 1 "a"))

         ((hydra_lib_maps_union ()) (list (list 1 "a"))))))
