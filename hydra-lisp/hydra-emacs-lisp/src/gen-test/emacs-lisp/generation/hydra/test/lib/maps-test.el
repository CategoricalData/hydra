;;; Note: this is an automatically generated file. Do not edit.
;;; hydra.lib.maps primitives

(require 'ert)

;; alter

(ert-deftest test-alter-neginsert-new-key ()

  (should (equal (list (list 1 "a") (list 2 "b") (list 3 "new")) (((hydra_lib_maps_alter (lambda (opt) "new")) 3) (list (list 1 "a") (list 2 "b"))))))

(ert-deftest test-alter-negupdate-existing-key ()

  (should (equal (list (list 1 "a") (list 2 "updated")) (((hydra_lib_maps_alter (lambda (opt) "updated")) 2) (list (list 1 "a") (list 2 "b"))))))

(ert-deftest test-alter-negdelete-key ()

  (should (equal (list (list 1 "a")) (((hydra_lib_maps_alter (lambda (opt) nil)) 2) (list (list 1 "a") (list 2 "b"))))))

;; bimap

(ert-deftest test-bimap-negtransform-both ()

  (should (equal (list (list 2 "A") (list 4 "B")) (((hydra_lib_maps_bimap (lambda (k) ((hydra_lib_math_mul k) 2))) (lambda (v) (hydra_lib_strings_to_upper v))) (list (list 1 "a") (list 2 "b"))))))

(ert-deftest test-bimap-negempty-map ()

  (should (equal () (((hydra_lib_maps_bimap (lambda (k) ((hydra_lib_math_mul k) 2))) (lambda (v) (hydra_lib_strings_to_upper v))) ()))))

;; elems

(ert-deftest test-elems-negget-all-elements ()

  (should (equal (list "a" "b") (hydra_lib_maps_elems (list (list 1 "a") (list 2 "b"))))))

(ert-deftest test-elems-negunsorted-keys ()

  (should (equal (list "a" "b" "c") (hydra_lib_maps_elems (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(ert-deftest test-elems-negempty-map ()

  (should (equal (list ) (hydra_lib_maps_elems ()))))

;; empty

(ert-deftest test-empty-negempty-map ()

  (should (equal () hydra_lib_maps_empty)))

;; filter

(ert-deftest test-filter-negfilter-values-starting-with-a ()

  (should (equal (list (list 1 "a") (list 3 "ab")) ((hydra_lib_maps_filter (lambda (v) ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) v)) 97))) (list (list 1 "a") (list 2 "b") (list 3 "ab"))))))

(ert-deftest test-filter-negfilter-all ()

  (should (equal () ((hydra_lib_maps_filter (lambda (v) ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) v)) 97))) (list (list 1 "b") (list 2 "c"))))))

(ert-deftest test-filter-negempty-map ()

  (should (equal () ((hydra_lib_maps_filter (lambda (v) ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) v)) 97))) ()))))

;; filterWithKey

(ert-deftest test-filterwithkey-negfilter-by-key-1 ()

  (should (equal (list (list 2 "b") (list 3 "c")) ((hydra_lib_maps_filter_with_key (lambda (k) (lambda (v) ((hydra_lib_equality_gt k) 1)))) (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(ert-deftest test-filterwithkey-negfilter-all ()

  (should (equal () ((hydra_lib_maps_filter_with_key (lambda (k) (lambda (v) ((hydra_lib_equality_gt k) 1)))) (list (list 1 "a"))))))

(ert-deftest test-filterwithkey-negempty-map ()

  (should (equal () ((hydra_lib_maps_filter_with_key (lambda (k) (lambda (v) ((hydra_lib_equality_gt k) 1)))) ()))))

;; findWithDefault

(ert-deftest test-findwithdefault-negfind-existing ()

  (should (equal "b" (((hydra_lib_maps_find_with_default "default") 2) (list (list 1 "a") (list 2 "b"))))))

(ert-deftest test-findwithdefault-neguse-default ()

  (should (equal "default" (((hydra_lib_maps_find_with_default "default") 3) (list (list 1 "a") (list 2 "b"))))))

;; fromList

(ert-deftest test-fromlist-negcreate-from-pairs ()

  (should (equal (list (list 1 "a") (list 2 "b")) (hydra_lib_maps_from_list (list (list 1 "a") (list 2 "b"))))))

(ert-deftest test-fromlist-negduplicate-keys ()

  (should (equal (list (list 1 "b")) (hydra_lib_maps_from_list (list (list 1 "a") (list 1 "b"))))))

(ert-deftest test-fromlist-negempty-list ()

  (should (equal () (hydra_lib_maps_from_list (list )))))

;; insert

(ert-deftest test-insert-neginsert-new-key ()

  (should (equal (list (list 1 "a") (list 2 "b") (list 3 "c")) (((hydra_lib_maps_insert 3) "c") (list (list 1 "a") (list 2 "b"))))))

(ert-deftest test-insert-negupdate-existing ()

  (should (equal (list (list 1 "a") (list 2 "updated")) (((hydra_lib_maps_insert 2) "updated") (list (list 1 "a") (list 2 "b"))))))

(ert-deftest test-insert-neginsert-into-empty ()

  (should (equal (list (list 1 "x")) (((hydra_lib_maps_insert 1) "x") ()))))

;; keys

(ert-deftest test-keys-negget-all-keys ()

  (should (equal (list 1 2 3) (hydra_lib_maps_keys (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(ert-deftest test-keys-negunsorted-keys ()

  (should (equal (list 1 2 3) (hydra_lib_maps_keys (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(ert-deftest test-keys-negempty-map ()

  (should (equal (list ) (hydra_lib_maps_keys ()))))

;; lookup

(ert-deftest test-lookup-negfind-existing-key ()

  (should (equal (list :just "b") ((hydra_lib_maps_lookup 2) (list (list 1 "a") (list 2 "b"))))))

(ert-deftest test-lookup-negkey-not-found ()

  (should (equal (list :nothing) ((hydra_lib_maps_lookup 3) (list (list 1 "a") (list 2 "b"))))))

(ert-deftest test-lookup-neglookup-in-empty ()

  (should (equal (list :nothing) ((hydra_lib_maps_lookup 1) ()))))

;; map

(ert-deftest test-map-negmap-over-values ()

  (should (equal (list (list 1 "A") (list 2 "B")) ((hydra_lib_maps_map (lambda (s) (hydra_lib_strings_to_upper s))) (list (list 1 "a") (list 2 "b"))))))

(ert-deftest test-map-negmap-empty ()

  (should (equal () ((hydra_lib_maps_map (lambda (s) (hydra_lib_strings_to_upper s))) ()))))

;; mapKeys

(ert-deftest test-mapkeys-negdouble-keys ()

  (should (equal (list (list 2 "a") (list 4 "b")) ((hydra_lib_maps_map_keys (lambda (k) ((hydra_lib_math_mul k) 2))) (list (list 1 "a") (list 2 "b"))))))

(ert-deftest test-mapkeys-negempty-map ()

  (should (equal () ((hydra_lib_maps_map_keys (lambda (k) ((hydra_lib_math_mul k) 2))) ()))))

;; member

(ert-deftest test-member-negkey-exists ()

  (should (equal t ((hydra_lib_maps_member 2) (list (list 1 "a") (list 2 "b"))))))

(ert-deftest test-member-negkey-missing ()

  (should (equal nil ((hydra_lib_maps_member 3) (list (list 1 "a") (list 2 "b"))))))

(ert-deftest test-member-negempty-map ()

  (should (equal nil ((hydra_lib_maps_member 1) ()))))

;; null

(ert-deftest test-null-negempty-map ()

  (should (equal t (hydra_lib_maps_null ()))))

(ert-deftest test-null-negnon-negempty-map ()

  (should (equal nil (hydra_lib_maps_null (list (list 1 "a"))))))

;; remove

(ert-deftest test-remove-negremove-existing ()

  (should (equal (list (list 1 "a") (list 3 "c")) ((hydra_lib_maps_delete 2) (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(ert-deftest test-remove-negremove-non-negexisting ()

  (should (equal (list (list 1 "a") (list 2 "b")) ((hydra_lib_maps_delete 4) (list (list 1 "a") (list 2 "b"))))))

(ert-deftest test-remove-negremove-from-empty ()

  (should (equal () ((hydra_lib_maps_delete 1) ()))))

;; singleton

(ert-deftest test-singleton-negsingle-entry ()

  (should (equal (list (list 42 "hello")) ((hydra_lib_maps_singleton 42) "hello"))))

;; size

(ert-deftest test-size-negthree-entries ()

  (should (equal 3 (hydra_lib_maps_size (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(ert-deftest test-size-negsingle-entry ()

  (should (equal 1 (hydra_lib_maps_size (list (list 42 "test"))))))

(ert-deftest test-size-negempty-map ()

  (should (equal 0 (hydra_lib_maps_size ()))))

;; toList

(ert-deftest test-tolist-negconvert-to-pairs ()

  (should (equal (list (list 1 "a") (list 2 "b")) (hydra_lib_maps_to_list (list (list 1 "a") (list 2 "b"))))))

(ert-deftest test-tolist-negunsorted-keys ()

  (should (equal (list (list 1 "a") (list 2 "b") (list 3 "c")) (hydra_lib_maps_to_list (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(ert-deftest test-tolist-negempty-map ()

  (should (equal (list ) (hydra_lib_maps_to_list ()))))

;; union

(ert-deftest test-union-negunion-two-maps ()

  (should (equal (list (list 1 "a") (list 2 "b") (list 3 "c")) ((hydra_lib_maps_union (list (list 1 "a") (list 2 "b"))) (list (list 2 "x") (list 3 "c"))))))

(ert-deftest test-union-negunion-with-empty ()

  (should (equal (list (list 1 "a")) ((hydra_lib_maps_union (list (list 1 "a"))) ()))))

(ert-deftest test-union-negempty-with-map ()

  (should (equal (list (list 1 "a")) ((hydra_lib_maps_union ()) (list (list 1 "a"))))))
