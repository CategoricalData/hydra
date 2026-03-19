;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; hydra.lib.maps primitives

(require 'ert)

;; alter

(ert-deftest test-maps-negalter-neginsert-new-key ()

  (should (equal (list (cons 1 "a") (cons 2 "b") (cons 3 "new")) (funcall (funcall (funcall hydra_lib_maps_alter (lambda (opt) (list :just "new"))) 3) (list (cons 1 "a") (cons 2 "b"))))))

(ert-deftest test-maps-negalter-negupdate-existing-key ()

  (should (equal (list (cons 1 "a") (cons 2 "updated")) (funcall (funcall (funcall hydra_lib_maps_alter (lambda (opt) (list :just "updated"))) 2) (list (cons 1 "a") (cons 2 "b"))))))

(ert-deftest test-maps-negalter-negdelete-key ()

  (should (equal (list (cons 1 "a")) (funcall (funcall (funcall hydra_lib_maps_alter (lambda (opt) (list :nothing))) 2) (list (cons 1 "a") (cons 2 "b"))))))

;; bimap

(ert-deftest test-maps-negbimap-negtransform-both ()

  (should (equal (list (cons 2 "A") (cons 4 "B")) (funcall (funcall (funcall hydra_lib_maps_bimap (lambda (k) (funcall (hydra_lib_math_mul k) 2))) (lambda (v) (hydra_lib_strings_to_upper v))) (list (cons 1 "a") (cons 2 "b"))))))

(ert-deftest test-maps-negbimap-negempty-map ()

  (should (equal (list) (funcall (funcall (funcall hydra_lib_maps_bimap (lambda (k) (funcall (hydra_lib_math_mul k) 2))) (lambda (v) (hydra_lib_strings_to_upper v))) (list)))))

;; elems

(ert-deftest test-maps-negelems-negget-all-elements ()

  (should (equal (list "a" "b") (funcall hydra_lib_maps_elems (list (cons 1 "a") (cons 2 "b"))))))

(ert-deftest test-maps-negelems-negunsorted-keys ()

  (should (equal (list "a" "b" "c") (funcall hydra_lib_maps_elems (list (cons 1 "a") (cons 2 "b") (cons 3 "c"))))))

(ert-deftest test-maps-negelems-negempty-map ()

  (should (equal (list ) (funcall hydra_lib_maps_elems (list)))))

;; empty

(ert-deftest test-maps-negempty-negempty-map ()

  (should (equal (list) hydra_lib_maps_empty)))

;; filter

(ert-deftest test-maps-negfilter-negfilter-values-starting-with-a ()

  (should (equal (list (cons 1 "a") (cons 3 "ab")) (funcall (funcall hydra_lib_maps_filter (lambda (v) (funcall (hydra_lib_equality_equal (funcall (hydra_lib_strings_char_at 0) v)) 97))) (list (cons 1 "a") (cons 2 "b") (cons 3 "ab"))))))

(ert-deftest test-maps-negfilter-negfilter-all ()

  (should (equal (list) (funcall (funcall hydra_lib_maps_filter (lambda (v) (funcall (hydra_lib_equality_equal (funcall (hydra_lib_strings_char_at 0) v)) 97))) (list (cons 1 "b") (cons 2 "c"))))))

(ert-deftest test-maps-negfilter-negempty-map ()

  (should (equal (list) (funcall (funcall hydra_lib_maps_filter (lambda (v) (funcall (hydra_lib_equality_equal (funcall (hydra_lib_strings_char_at 0) v)) 97))) (list)))))

;; filterWithKey

(ert-deftest test-maps-negfilterwithkey-negfilter-by-key-1 ()

  (should (equal (list (cons 2 "b") (cons 3 "c")) (funcall (funcall hydra_lib_maps_filter_with_key (lambda (k) (lambda (v) (funcall (hydra_lib_equality_gt k) 1)))) (list (cons 1 "a") (cons 2 "b") (cons 3 "c"))))))

(ert-deftest test-maps-negfilterwithkey-negfilter-all ()

  (should (equal (list) (funcall (funcall hydra_lib_maps_filter_with_key (lambda (k) (lambda (v) (funcall (hydra_lib_equality_gt k) 1)))) (list (cons 1 "a"))))))

(ert-deftest test-maps-negfilterwithkey-negempty-map ()

  (should (equal (list) (funcall (funcall hydra_lib_maps_filter_with_key (lambda (k) (lambda (v) (funcall (hydra_lib_equality_gt k) 1)))) (list)))))

;; findWithDefault

(ert-deftest test-maps-negfindwithdefault-negfind-existing ()

  (should (equal "b" (funcall (funcall (funcall hydra_lib_maps_find_with_default "default") 2) (list (cons 1 "a") (cons 2 "b"))))))

(ert-deftest test-maps-negfindwithdefault-neguse-default ()

  (should (equal "default" (funcall (funcall (funcall hydra_lib_maps_find_with_default "default") 3) (list (cons 1 "a") (cons 2 "b"))))))

;; fromList

(ert-deftest test-maps-negfromlist-negcreate-from-pairs ()

  (should (equal (list (cons 1 "a") (cons 2 "b")) (funcall hydra_lib_maps_from_list (list (list 1 "a") (list 2 "b"))))))

(ert-deftest test-maps-negfromlist-negduplicate-keys ()

  (should (equal (list (cons 1 "b")) (funcall hydra_lib_maps_from_list (list (list 1 "a") (list 1 "b"))))))

(ert-deftest test-maps-negfromlist-negempty-list ()

  (should (equal (list) (funcall hydra_lib_maps_from_list (list )))))

;; insert

(ert-deftest test-maps-neginsert-neginsert-new-key ()

  (should (equal (list (cons 1 "a") (cons 2 "b") (cons 3 "c")) (funcall (funcall (funcall hydra_lib_maps_insert 3) "c") (list (cons 1 "a") (cons 2 "b"))))))

(ert-deftest test-maps-neginsert-negupdate-existing ()

  (should (equal (list (cons 1 "a") (cons 2 "updated")) (funcall (funcall (funcall hydra_lib_maps_insert 2) "updated") (list (cons 1 "a") (cons 2 "b"))))))

(ert-deftest test-maps-neginsert-neginsert-into-empty ()

  (should (equal (list (cons 1 "x")) (funcall (funcall (funcall hydra_lib_maps_insert 1) "x") (list)))))

;; keys

(ert-deftest test-maps-negkeys-negget-all-keys ()

  (should (equal (list 1 2 3) (funcall hydra_lib_maps_keys (list (cons 1 "a") (cons 2 "b") (cons 3 "c"))))))

(ert-deftest test-maps-negkeys-negunsorted-keys ()

  (should (equal (list 1 2 3) (funcall hydra_lib_maps_keys (list (cons 1 "a") (cons 2 "b") (cons 3 "c"))))))

(ert-deftest test-maps-negkeys-negempty-map ()

  (should (equal (list ) (funcall hydra_lib_maps_keys (list)))))

;; lookup

(ert-deftest test-maps-neglookup-negfind-existing-key ()

  (should (equal (list :just "b") (funcall (funcall hydra_lib_maps_lookup 2) (list (cons 1 "a") (cons 2 "b"))))))

(ert-deftest test-maps-neglookup-negkey-not-found ()

  (should (equal (list :nothing) (funcall (funcall hydra_lib_maps_lookup 3) (list (cons 1 "a") (cons 2 "b"))))))

(ert-deftest test-maps-neglookup-neglookup-in-empty ()

  (should (equal (list :nothing) (funcall (funcall hydra_lib_maps_lookup 1) (list)))))

;; map

(ert-deftest test-maps-negmap-negmap-over-values ()

  (should (equal (list (cons 1 "A") (cons 2 "B")) (funcall (funcall hydra_lib_maps_map (lambda (s) (hydra_lib_strings_to_upper s))) (list (cons 1 "a") (cons 2 "b"))))))

(ert-deftest test-maps-negmap-negmap-empty ()

  (should (equal (list) (funcall (funcall hydra_lib_maps_map (lambda (s) (hydra_lib_strings_to_upper s))) (list)))))

;; mapKeys

(ert-deftest test-maps-negmapkeys-negdouble-keys ()

  (should (equal (list (cons 2 "a") (cons 4 "b")) (funcall (funcall hydra_lib_maps_map_keys (lambda (k) (funcall (hydra_lib_math_mul k) 2))) (list (cons 1 "a") (cons 2 "b"))))))

(ert-deftest test-maps-negmapkeys-negempty-map ()

  (should (equal (list) (funcall (funcall hydra_lib_maps_map_keys (lambda (k) (funcall (hydra_lib_math_mul k) 2))) (list)))))

;; member

(ert-deftest test-maps-negmember-negkey-exists ()

  (should (equal t (funcall (funcall hydra_lib_maps_member 2) (list (cons 1 "a") (cons 2 "b"))))))

(ert-deftest test-maps-negmember-negkey-missing ()

  (should (equal nil (funcall (funcall hydra_lib_maps_member 3) (list (cons 1 "a") (cons 2 "b"))))))

(ert-deftest test-maps-negmember-negempty-map ()

  (should (equal nil (funcall (funcall hydra_lib_maps_member 1) (list)))))

;; null

(ert-deftest test-maps-negnull-negempty-map ()

  (should (equal t (funcall hydra_lib_maps_null (list)))))

(ert-deftest test-maps-negnull-negnon-negempty-map ()

  (should (equal nil (funcall hydra_lib_maps_null (list (cons 1 "a"))))))

;; remove

(ert-deftest test-maps-negremove-negremove-existing ()

  (should (equal (list (cons 1 "a") (cons 3 "c")) (funcall (funcall hydra_lib_maps_delete 2) (list (cons 1 "a") (cons 2 "b") (cons 3 "c"))))))

(ert-deftest test-maps-negremove-negremove-non-negexisting ()

  (should (equal (list (cons 1 "a") (cons 2 "b")) (funcall (funcall hydra_lib_maps_delete 4) (list (cons 1 "a") (cons 2 "b"))))))

(ert-deftest test-maps-negremove-negremove-from-empty ()

  (should (equal (list) (funcall (funcall hydra_lib_maps_delete 1) (list)))))

;; singleton

(ert-deftest test-maps-negsingleton-negsingle-entry ()

  (should (equal (list (cons 42 "hello")) (funcall (funcall hydra_lib_maps_singleton 42) "hello"))))

;; size

(ert-deftest test-maps-negsize-negthree-entries ()

  (should (equal 3 (funcall hydra_lib_maps_size (list (cons 1 "a") (cons 2 "b") (cons 3 "c"))))))

(ert-deftest test-maps-negsize-negsingle-entry ()

  (should (equal 1 (funcall hydra_lib_maps_size (list (cons 42 "test"))))))

(ert-deftest test-maps-negsize-negempty-map ()

  (should (equal 0 (funcall hydra_lib_maps_size (list)))))

;; toList

(ert-deftest test-maps-negtolist-negconvert-to-pairs ()

  (should (equal (list (list 1 "a") (list 2 "b")) (funcall hydra_lib_maps_to_list (list (cons 1 "a") (cons 2 "b"))))))

(ert-deftest test-maps-negtolist-negunsorted-keys ()

  (should (equal (list (list 1 "a") (list 2 "b") (list 3 "c")) (funcall hydra_lib_maps_to_list (list (cons 1 "a") (cons 2 "b") (cons 3 "c"))))))

(ert-deftest test-maps-negtolist-negempty-map ()

  (should (equal (list ) (funcall hydra_lib_maps_to_list (list)))))

;; union

(ert-deftest test-maps-negunion-negunion-two-maps ()

  (should (equal (list (cons 1 "a") (cons 2 "b") (cons 3 "c")) (funcall (funcall hydra_lib_maps_union (list (cons 1 "a") (cons 2 "b"))) (list (cons 2 "x") (cons 3 "c"))))))

(ert-deftest test-maps-negunion-negunion-with-empty ()

  (should (equal (list (cons 1 "a")) (funcall (funcall hydra_lib_maps_union (list (cons 1 "a"))) (list)))))

(ert-deftest test-maps-negunion-negempty-with-map ()

  (should (equal (list (cons 1 "a")) (funcall (funcall hydra_lib_maps_union (list)) (list (cons 1 "a"))))))
