;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maps primitives

;; alter

(defun test-alter-neginsert-new-key ()

  (assert (equal (list (list 1 "a") (list 2 "b") (list 3 "new")) (((hydra_lib_maps_alter (cl:lambda (opt) "new")) 3) (list (list 1 "a") (list 2 "b"))))))

(defun test-alter-negupdate-existing-key ()

  (assert (equal (list (list 1 "a") (list 2 "updated")) (((hydra_lib_maps_alter (cl:lambda (opt) "updated")) 2) (list (list 1 "a") (list 2 "b"))))))

(defun test-alter-negdelete-key ()

  (assert (equal (list (list 1 "a")) (((hydra_lib_maps_alter (cl:lambda (opt) cl:nil)) 2) (list (list 1 "a") (list 2 "b"))))))

;; bimap

(defun test-bimap-negtransform-both ()

  (assert (equal (list (list 2 "A") (list 4 "B")) (((hydra_lib_maps_bimap (cl:lambda (k) ((hydra_lib_math_mul k) 2))) (cl:lambda (v) (hydra_lib_strings_to_upper v))) (list (list 1 "a") (list 2 "b"))))))

(defun test-bimap-negempty-map ()

  (assert (equal () (((hydra_lib_maps_bimap (cl:lambda (k) ((hydra_lib_math_mul k) 2))) (cl:lambda (v) (hydra_lib_strings_to_upper v))) ()))))

;; elems

(defun test-elems-negget-all-elements ()

  (assert (equal (list "a" "b") (hydra_lib_maps_elems (list (list 1 "a") (list 2 "b"))))))

(defun test-elems-negunsorted-keys ()

  (assert (equal (list "a" "b" "c") (hydra_lib_maps_elems (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(defun test-elems-negempty-map ()

  (assert (equal (list ) (hydra_lib_maps_elems ()))))

;; empty

(defun test-empty-negempty-map ()

  (assert (equal () hydra_lib_maps_empty)))

;; filter

(defun test-filter-negfilter-values-starting-with-a ()

  (assert (equal (list (list 1 "a") (list 3 "ab")) ((hydra_lib_maps_filter (cl:lambda (v) ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) v)) 97))) (list (list 1 "a") (list 2 "b") (list 3 "ab"))))))

(defun test-filter-negfilter-all ()

  (assert (equal () ((hydra_lib_maps_filter (cl:lambda (v) ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) v)) 97))) (list (list 1 "b") (list 2 "c"))))))

(defun test-filter-negempty-map ()

  (assert (equal () ((hydra_lib_maps_filter (cl:lambda (v) ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) v)) 97))) ()))))

;; filterWithKey

(defun test-filterwithkey-negfilter-by-key-1 ()

  (assert (equal (list (list 2 "b") (list 3 "c")) ((hydra_lib_maps_filter_with_key (cl:lambda (k) (cl:lambda (v) ((hydra_lib_equality_gt k) 1)))) (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(defun test-filterwithkey-negfilter-all ()

  (assert (equal () ((hydra_lib_maps_filter_with_key (cl:lambda (k) (cl:lambda (v) ((hydra_lib_equality_gt k) 1)))) (list (list 1 "a"))))))

(defun test-filterwithkey-negempty-map ()

  (assert (equal () ((hydra_lib_maps_filter_with_key (cl:lambda (k) (cl:lambda (v) ((hydra_lib_equality_gt k) 1)))) ()))))

;; findWithDefault

(defun test-findwithdefault-negfind-existing ()

  (assert (equal "b" (((hydra_lib_maps_find_with_default "default") 2) (list (list 1 "a") (list 2 "b"))))))

(defun test-findwithdefault-neguse-default ()

  (assert (equal "default" (((hydra_lib_maps_find_with_default "default") 3) (list (list 1 "a") (list 2 "b"))))))

;; fromList

(defun test-fromlist-negcreate-from-pairs ()

  (assert (equal (list (list 1 "a") (list 2 "b")) (hydra_lib_maps_from_list (list (list 1 "a") (list 2 "b"))))))

(defun test-fromlist-negduplicate-keys ()

  (assert (equal (list (list 1 "b")) (hydra_lib_maps_from_list (list (list 1 "a") (list 1 "b"))))))

(defun test-fromlist-negempty-list ()

  (assert (equal () (hydra_lib_maps_from_list (list )))))

;; insert

(defun test-insert-neginsert-new-key ()

  (assert (equal (list (list 1 "a") (list 2 "b") (list 3 "c")) (((hydra_lib_maps_insert 3) "c") (list (list 1 "a") (list 2 "b"))))))

(defun test-insert-negupdate-existing ()

  (assert (equal (list (list 1 "a") (list 2 "updated")) (((hydra_lib_maps_insert 2) "updated") (list (list 1 "a") (list 2 "b"))))))

(defun test-insert-neginsert-into-empty ()

  (assert (equal (list (list 1 "x")) (((hydra_lib_maps_insert 1) "x") ()))))

;; keys

(defun test-keys-negget-all-keys ()

  (assert (equal (list 1 2 3) (hydra_lib_maps_keys (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(defun test-keys-negunsorted-keys ()

  (assert (equal (list 1 2 3) (hydra_lib_maps_keys (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(defun test-keys-negempty-map ()

  (assert (equal (list ) (hydra_lib_maps_keys ()))))

;; lookup

(defun test-lookup-negfind-existing-key ()

  (assert (equal "b" ((hydra_lib_maps_lookup 2) (list (list 1 "a") (list 2 "b"))))))

(defun test-lookup-negkey-not-found ()

  (assert (equal nil ((hydra_lib_maps_lookup 3) (list (list 1 "a") (list 2 "b"))))))

(defun test-lookup-neglookup-in-empty ()

  (assert (equal nil ((hydra_lib_maps_lookup 1) ()))))

;; map

(defun test-map-negmap-over-values ()

  (assert (equal (list (list 1 "A") (list 2 "B")) ((hydra_lib_maps_map (cl:lambda (s) (hydra_lib_strings_to_upper s))) (list (list 1 "a") (list 2 "b"))))))

(defun test-map-negmap-empty ()

  (assert (equal () ((hydra_lib_maps_map (cl:lambda (s) (hydra_lib_strings_to_upper s))) ()))))

;; mapKeys

(defun test-mapkeys-negdouble-keys ()

  (assert (equal (list (list 2 "a") (list 4 "b")) ((hydra_lib_maps_map_keys (cl:lambda (k) ((hydra_lib_math_mul k) 2))) (list (list 1 "a") (list 2 "b"))))))

(defun test-mapkeys-negempty-map ()

  (assert (equal () ((hydra_lib_maps_map_keys (cl:lambda (k) ((hydra_lib_math_mul k) 2))) ()))))

;; member

(defun test-member-negkey-exists ()

  (assert (equal cl:t ((hydra_lib_maps_member 2) (list (list 1 "a") (list 2 "b"))))))

(defun test-member-negkey-missing ()

  (assert (equal cl:nil ((hydra_lib_maps_member 3) (list (list 1 "a") (list 2 "b"))))))

(defun test-member-negempty-map ()

  (assert (equal cl:nil ((hydra_lib_maps_member 1) ()))))

;; null

(defun test-null-negempty-map ()

  (assert (equal cl:t (hydra_lib_maps_null ()))))

(defun test-null-negnon-negempty-map ()

  (assert (equal cl:nil (hydra_lib_maps_null (list (list 1 "a"))))))

;; remove

(defun test-remove-negremove-existing ()

  (assert (equal (list (list 1 "a") (list 3 "c")) ((hydra_lib_maps_delete 2) (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(defun test-remove-negremove-non-negexisting ()

  (assert (equal (list (list 1 "a") (list 2 "b")) ((hydra_lib_maps_delete 4) (list (list 1 "a") (list 2 "b"))))))

(defun test-remove-negremove-from-empty ()

  (assert (equal () ((hydra_lib_maps_delete 1) ()))))

;; singleton

(defun test-singleton-negsingle-entry ()

  (assert (equal (list (list 42 "hello")) ((hydra_lib_maps_singleton 42) "hello"))))

;; size

(defun test-size-negthree-entries ()

  (assert (equal 3 (hydra_lib_maps_size (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(defun test-size-negsingle-entry ()

  (assert (equal 1 (hydra_lib_maps_size (list (list 42 "test"))))))

(defun test-size-negempty-map ()

  (assert (equal 0 (hydra_lib_maps_size ()))))

;; toList

(defun test-tolist-negconvert-to-pairs ()

  (assert (equal (list (list 1 "a") (list 2 "b")) (hydra_lib_maps_to_list (list (list 1 "a") (list 2 "b"))))))

(defun test-tolist-negunsorted-keys ()

  (assert (equal (list (list 1 "a") (list 2 "b") (list 3 "c")) (hydra_lib_maps_to_list (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(defun test-tolist-negempty-map ()

  (assert (equal (list ) (hydra_lib_maps_to_list ()))))

;; union

(defun test-union-negunion-two-maps ()

  (assert (equal (list (list 1 "a") (list 2 "b") (list 3 "c")) ((hydra_lib_maps_union (list (list 1 "a") (list 2 "b"))) (list (list 2 "x") (list 3 "c"))))))

(defun test-union-negunion-with-empty ()

  (assert (equal (list (list 1 "a")) ((hydra_lib_maps_union (list (list 1 "a"))) ()))))

(defun test-union-negempty-with-map ()

  (assert (equal (list (list 1 "a")) ((hydra_lib_maps_union ()) (list (list 1 "a"))))))
