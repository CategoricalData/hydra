;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maps primitives

;; alter

(defun test-maps-negalter-neginsert-new-key ()

  (assert (equal (list (cons 1 "a") (cons 2 "b") (cons 3 "new")) (((hydra_lib_maps_alter (cl:lambda (opt) (list :just "new"))) 3) (list (cons 1 "a") (cons 2 "b"))))))

(defun test-maps-negalter-negupdate-existing-key ()

  (assert (equal (list (cons 1 "a") (cons 2 "updated")) (((hydra_lib_maps_alter (cl:lambda (opt) (list :just "updated"))) 2) (list (cons 1 "a") (cons 2 "b"))))))

(defun test-maps-negalter-negdelete-key ()

  (assert (equal (list (cons 1 "a")) (((hydra_lib_maps_alter (cl:lambda (opt) (list :nothing))) 2) (list (cons 1 "a") (cons 2 "b"))))))

;; bimap

(defun test-maps-negbimap-negtransform-both ()

  (assert (equal (list (cons 2 "A") (cons 4 "B")) (((hydra_lib_maps_bimap (cl:lambda (k) ((hydra_lib_math_mul k) 2))) (cl:lambda (v) (hydra_lib_strings_to_upper v))) (list (cons 1 "a") (cons 2 "b"))))))

(defun test-maps-negbimap-negempty-map ()

  (assert (equal (list) (((hydra_lib_maps_bimap (cl:lambda (k) ((hydra_lib_math_mul k) 2))) (cl:lambda (v) (hydra_lib_strings_to_upper v))) (list)))))

;; elems

(defun test-maps-negelems-negget-all-elements ()

  (assert (equal (list "a" "b") (hydra_lib_maps_elems (list (cons 1 "a") (cons 2 "b"))))))

(defun test-maps-negelems-negunsorted-keys ()

  (assert (equal (list "a" "b" "c") (hydra_lib_maps_elems (list (cons 1 "a") (cons 2 "b") (cons 3 "c"))))))

(defun test-maps-negelems-negempty-map ()

  (assert (equal (list ) (hydra_lib_maps_elems (list)))))

;; empty

(defun test-maps-negempty-negempty-map ()

  (assert (equal (list) hydra_lib_maps_empty)))

;; filter

(defun test-maps-negfilter-negfilter-values-starting-with-a ()

  (assert (equal (list (cons 1 "a") (cons 3 "ab")) ((hydra_lib_maps_filter (cl:lambda (v) ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) v)) 97))) (list (cons 1 "a") (cons 2 "b") (cons 3 "ab"))))))

(defun test-maps-negfilter-negfilter-all ()

  (assert (equal (list) ((hydra_lib_maps_filter (cl:lambda (v) ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) v)) 97))) (list (cons 1 "b") (cons 2 "c"))))))

(defun test-maps-negfilter-negempty-map ()

  (assert (equal (list) ((hydra_lib_maps_filter (cl:lambda (v) ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) v)) 97))) (list)))))

;; filterWithKey

(defun test-maps-negfilterwithkey-negfilter-by-key-1 ()

  (assert (equal (list (cons 2 "b") (cons 3 "c")) ((hydra_lib_maps_filter_with_key (cl:lambda (k) (cl:lambda (v) ((hydra_lib_equality_gt k) 1)))) (list (cons 1 "a") (cons 2 "b") (cons 3 "c"))))))

(defun test-maps-negfilterwithkey-negfilter-all ()

  (assert (equal (list) ((hydra_lib_maps_filter_with_key (cl:lambda (k) (cl:lambda (v) ((hydra_lib_equality_gt k) 1)))) (list (cons 1 "a"))))))

(defun test-maps-negfilterwithkey-negempty-map ()

  (assert (equal (list) ((hydra_lib_maps_filter_with_key (cl:lambda (k) (cl:lambda (v) ((hydra_lib_equality_gt k) 1)))) (list)))))

;; findWithDefault

(defun test-maps-negfindwithdefault-negfind-existing ()

  (assert (equal "b" (((hydra_lib_maps_find_with_default "default") 2) (list (cons 1 "a") (cons 2 "b"))))))

(defun test-maps-negfindwithdefault-neguse-default ()

  (assert (equal "default" (((hydra_lib_maps_find_with_default "default") 3) (list (cons 1 "a") (cons 2 "b"))))))

;; fromList

(defun test-maps-negfromlist-negcreate-from-pairs ()

  (assert (equal (list (cons 1 "a") (cons 2 "b")) (hydra_lib_maps_from_list (list (list 1 "a") (list 2 "b"))))))

(defun test-maps-negfromlist-negduplicate-keys ()

  (assert (equal (list (cons 1 "b")) (hydra_lib_maps_from_list (list (list 1 "a") (list 1 "b"))))))

(defun test-maps-negfromlist-negempty-list ()

  (assert (equal (list) (hydra_lib_maps_from_list (list )))))

;; insert

(defun test-maps-neginsert-neginsert-new-key ()

  (assert (equal (list (cons 1 "a") (cons 2 "b") (cons 3 "c")) (((hydra_lib_maps_insert 3) "c") (list (cons 1 "a") (cons 2 "b"))))))

(defun test-maps-neginsert-negupdate-existing ()

  (assert (equal (list (cons 1 "a") (cons 2 "updated")) (((hydra_lib_maps_insert 2) "updated") (list (cons 1 "a") (cons 2 "b"))))))

(defun test-maps-neginsert-neginsert-into-empty ()

  (assert (equal (list (cons 1 "x")) (((hydra_lib_maps_insert 1) "x") (list)))))

;; keys

(defun test-maps-negkeys-negget-all-keys ()

  (assert (equal (list 1 2 3) (hydra_lib_maps_keys (list (cons 1 "a") (cons 2 "b") (cons 3 "c"))))))

(defun test-maps-negkeys-negunsorted-keys ()

  (assert (equal (list 1 2 3) (hydra_lib_maps_keys (list (cons 1 "a") (cons 2 "b") (cons 3 "c"))))))

(defun test-maps-negkeys-negempty-map ()

  (assert (equal (list ) (hydra_lib_maps_keys (list)))))

;; lookup

(defun test-maps-neglookup-negfind-existing-key ()

  (assert (equal (list :just "b") ((hydra_lib_maps_lookup 2) (list (cons 1 "a") (cons 2 "b"))))))

(defun test-maps-neglookup-negkey-not-found ()

  (assert (equal (list :nothing) ((hydra_lib_maps_lookup 3) (list (cons 1 "a") (cons 2 "b"))))))

(defun test-maps-neglookup-neglookup-in-empty ()

  (assert (equal (list :nothing) ((hydra_lib_maps_lookup 1) (list)))))

;; map

(defun test-maps-negmap-negmap-over-values ()

  (assert (equal (list (cons 1 "A") (cons 2 "B")) ((hydra_lib_maps_map (cl:lambda (s) (hydra_lib_strings_to_upper s))) (list (cons 1 "a") (cons 2 "b"))))))

(defun test-maps-negmap-negmap-empty ()

  (assert (equal (list) ((hydra_lib_maps_map (cl:lambda (s) (hydra_lib_strings_to_upper s))) (list)))))

;; mapKeys

(defun test-maps-negmapkeys-negdouble-keys ()

  (assert (equal (list (cons 2 "a") (cons 4 "b")) ((hydra_lib_maps_map_keys (cl:lambda (k) ((hydra_lib_math_mul k) 2))) (list (cons 1 "a") (cons 2 "b"))))))

(defun test-maps-negmapkeys-negempty-map ()

  (assert (equal (list) ((hydra_lib_maps_map_keys (cl:lambda (k) ((hydra_lib_math_mul k) 2))) (list)))))

;; member

(defun test-maps-negmember-negkey-exists ()

  (assert (equal cl:t ((hydra_lib_maps_member 2) (list (cons 1 "a") (cons 2 "b"))))))

(defun test-maps-negmember-negkey-missing ()

  (assert (equal cl:nil ((hydra_lib_maps_member 3) (list (cons 1 "a") (cons 2 "b"))))))

(defun test-maps-negmember-negempty-map ()

  (assert (equal cl:nil ((hydra_lib_maps_member 1) (list)))))

;; null

(defun test-maps-negnull-negempty-map ()

  (assert (equal cl:t (hydra_lib_maps_null (list)))))

(defun test-maps-negnull-negnon-negempty-map ()

  (assert (equal cl:nil (hydra_lib_maps_null (list (cons 1 "a"))))))

;; remove

(defun test-maps-negremove-negremove-existing ()

  (assert (equal (list (cons 1 "a") (cons 3 "c")) ((hydra_lib_maps_delete 2) (list (cons 1 "a") (cons 2 "b") (cons 3 "c"))))))

(defun test-maps-negremove-negremove-non-negexisting ()

  (assert (equal (list (cons 1 "a") (cons 2 "b")) ((hydra_lib_maps_delete 4) (list (cons 1 "a") (cons 2 "b"))))))

(defun test-maps-negremove-negremove-from-empty ()

  (assert (equal (list) ((hydra_lib_maps_delete 1) (list)))))

;; singleton

(defun test-maps-negsingleton-negsingle-entry ()

  (assert (equal (list (cons 42 "hello")) ((hydra_lib_maps_singleton 42) "hello"))))

;; size

(defun test-maps-negsize-negthree-entries ()

  (assert (equal 3 (hydra_lib_maps_size (list (cons 1 "a") (cons 2 "b") (cons 3 "c"))))))

(defun test-maps-negsize-negsingle-entry ()

  (assert (equal 1 (hydra_lib_maps_size (list (cons 42 "test"))))))

(defun test-maps-negsize-negempty-map ()

  (assert (equal 0 (hydra_lib_maps_size (list)))))

;; toList

(defun test-maps-negtolist-negconvert-to-pairs ()

  (assert (equal (list (list 1 "a") (list 2 "b")) (hydra_lib_maps_to_list (list (cons 1 "a") (cons 2 "b"))))))

(defun test-maps-negtolist-negunsorted-keys ()

  (assert (equal (list (list 1 "a") (list 2 "b") (list 3 "c")) (hydra_lib_maps_to_list (list (cons 1 "a") (cons 2 "b") (cons 3 "c"))))))

(defun test-maps-negtolist-negempty-map ()

  (assert (equal (list ) (hydra_lib_maps_to_list (list)))))

;; union

(defun test-maps-negunion-negunion-two-maps ()

  (assert (equal (list (cons 1 "a") (cons 2 "b") (cons 3 "c")) ((hydra_lib_maps_union (list (cons 1 "a") (cons 2 "b"))) (list (cons 2 "x") (cons 3 "c"))))))

(defun test-maps-negunion-negunion-with-empty ()

  (assert (equal (list (cons 1 "a")) ((hydra_lib_maps_union (list (cons 1 "a"))) (list)))))

(defun test-maps-negunion-negempty-with-map ()

  (assert (equal (list (cons 1 "a")) ((hydra_lib_maps_union (list)) (list (cons 1 "a"))))))
