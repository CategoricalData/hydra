;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maps primitives

(import (scheme base))

;; alter

(define (test-alter-neginsert-new-key)

  (assert (equal? (list (list 1 "a") (list 2 "b") (list 3 "new")) (((hydra_lib_maps_alter (lambda (opt) "new")) 3) (list (list 1 "a") (list 2 "b"))))))

(define (test-alter-negupdate-existing-key)

  (assert (equal? (list (list 1 "a") (list 2 "updated")) (((hydra_lib_maps_alter (lambda (opt) "updated")) 2) (list (list 1 "a") (list 2 "b"))))))

(define (test-alter-negdelete-key)

  (assert (equal? (list (list 1 "a")) (((hydra_lib_maps_alter (lambda (opt) '())) 2) (list (list 1 "a") (list 2 "b"))))))

;; bimap

(define (test-bimap-negtransform-both)

  (assert (equal? (list (list 2 "A") (list 4 "B")) (((hydra_lib_maps_bimap (lambda (k) ((hydra_lib_math_mul k) 2))) (lambda (v) (hydra_lib_strings_to_upper v))) (list (list 1 "a") (list 2 "b"))))))

(define (test-bimap-negempty-map)

  (assert (equal? () (((hydra_lib_maps_bimap (lambda (k) ((hydra_lib_math_mul k) 2))) (lambda (v) (hydra_lib_strings_to_upper v))) ()))))

;; elems

(define (test-elems-negget-all-elements)

  (assert (equal? (list "a" "b") (hydra_lib_maps_elems (list (list 1 "a") (list 2 "b"))))))

(define (test-elems-negunsorted-keys)

  (assert (equal? (list "a" "b" "c") (hydra_lib_maps_elems (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(define (test-elems-negempty-map)

  (assert (equal? (list ) (hydra_lib_maps_elems ()))))

;; empty

(define (test-empty-negempty-map)

  (assert (equal? () hydra_lib_maps_empty)))

;; filter

(define (test-filter-negfilter-values-starting-with-a)

  (assert (equal? (list (list 1 "a") (list 3 "ab")) ((hydra_lib_maps_filter (lambda (v) ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) v)) 97))) (list (list 1 "a") (list 2 "b") (list 3 "ab"))))))

(define (test-filter-negfilter-all)

  (assert (equal? () ((hydra_lib_maps_filter (lambda (v) ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) v)) 97))) (list (list 1 "b") (list 2 "c"))))))

(define (test-filter-negempty-map)

  (assert (equal? () ((hydra_lib_maps_filter (lambda (v) ((hydra_lib_equality_equal ((hydra_lib_strings_char_at 0) v)) 97))) ()))))

;; filterWithKey

(define (test-filterwithkey-negfilter-by-key-1)

  (assert (equal? (list (list 2 "b") (list 3 "c")) ((hydra_lib_maps_filter_with_key (lambda (k) (lambda (v) ((hydra_lib_equality_gt k) 1)))) (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(define (test-filterwithkey-negfilter-all)

  (assert (equal? () ((hydra_lib_maps_filter_with_key (lambda (k) (lambda (v) ((hydra_lib_equality_gt k) 1)))) (list (list 1 "a"))))))

(define (test-filterwithkey-negempty-map)

  (assert (equal? () ((hydra_lib_maps_filter_with_key (lambda (k) (lambda (v) ((hydra_lib_equality_gt k) 1)))) ()))))

;; findWithDefault

(define (test-findwithdefault-negfind-existing)

  (assert (equal? "b" (((hydra_lib_maps_find_with_default "default") 2) (list (list 1 "a") (list 2 "b"))))))

(define (test-findwithdefault-neguse-default)

  (assert (equal? "default" (((hydra_lib_maps_find_with_default "default") 3) (list (list 1 "a") (list 2 "b"))))))

;; fromList

(define (test-fromlist-negcreate-from-pairs)

  (assert (equal? (list (list 1 "a") (list 2 "b")) (hydra_lib_maps_from_list (list (list 1 "a") (list 2 "b"))))))

(define (test-fromlist-negduplicate-keys)

  (assert (equal? (list (list 1 "b")) (hydra_lib_maps_from_list (list (list 1 "a") (list 1 "b"))))))

(define (test-fromlist-negempty-list)

  (assert (equal? () (hydra_lib_maps_from_list (list )))))

;; insert

(define (test-insert-neginsert-new-key)

  (assert (equal? (list (list 1 "a") (list 2 "b") (list 3 "c")) (((hydra_lib_maps_insert 3) "c") (list (list 1 "a") (list 2 "b"))))))

(define (test-insert-negupdate-existing)

  (assert (equal? (list (list 1 "a") (list 2 "updated")) (((hydra_lib_maps_insert 2) "updated") (list (list 1 "a") (list 2 "b"))))))

(define (test-insert-neginsert-into-empty)

  (assert (equal? (list (list 1 "x")) (((hydra_lib_maps_insert 1) "x") ()))))

;; keys

(define (test-keys-negget-all-keys)

  (assert (equal? (list 1 2 3) (hydra_lib_maps_keys (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(define (test-keys-negunsorted-keys)

  (assert (equal? (list 1 2 3) (hydra_lib_maps_keys (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(define (test-keys-negempty-map)

  (assert (equal? (list ) (hydra_lib_maps_keys ()))))

;; lookup

(define (test-lookup-negfind-existing-key)

  (assert (equal? (list :just "b") ((hydra_lib_maps_lookup 2) (list (list 1 "a") (list 2 "b"))))))

(define (test-lookup-negkey-not-found)

  (assert (equal? (list :nothing) ((hydra_lib_maps_lookup 3) (list (list 1 "a") (list 2 "b"))))))

(define (test-lookup-neglookup-in-empty)

  (assert (equal? (list :nothing) ((hydra_lib_maps_lookup 1) ()))))

;; map

(define (test-map-negmap-over-values)

  (assert (equal? (list (list 1 "A") (list 2 "B")) ((hydra_lib_maps_map (lambda (s) (hydra_lib_strings_to_upper s))) (list (list 1 "a") (list 2 "b"))))))

(define (test-map-negmap-empty)

  (assert (equal? () ((hydra_lib_maps_map (lambda (s) (hydra_lib_strings_to_upper s))) ()))))

;; mapKeys

(define (test-mapkeys-negdouble-keys)

  (assert (equal? (list (list 2 "a") (list 4 "b")) ((hydra_lib_maps_map_keys (lambda (k) ((hydra_lib_math_mul k) 2))) (list (list 1 "a") (list 2 "b"))))))

(define (test-mapkeys-negempty-map)

  (assert (equal? () ((hydra_lib_maps_map_keys (lambda (k) ((hydra_lib_math_mul k) 2))) ()))))

;; member

(define (test-member-negkey-exists)

  (assert (equal? #t ((hydra_lib_maps_member 2) (list (list 1 "a") (list 2 "b"))))))

(define (test-member-negkey-missing)

  (assert (equal? #f ((hydra_lib_maps_member 3) (list (list 1 "a") (list 2 "b"))))))

(define (test-member-negempty-map)

  (assert (equal? #f ((hydra_lib_maps_member 1) ()))))

;; null

(define (test-null-negempty-map)

  (assert (equal? #t (hydra_lib_maps_null ()))))

(define (test-null-negnon-negempty-map)

  (assert (equal? #f (hydra_lib_maps_null (list (list 1 "a"))))))

;; remove

(define (test-remove-negremove-existing)

  (assert (equal? (list (list 1 "a") (list 3 "c")) ((hydra_lib_maps_delete 2) (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(define (test-remove-negremove-non-negexisting)

  (assert (equal? (list (list 1 "a") (list 2 "b")) ((hydra_lib_maps_delete 4) (list (list 1 "a") (list 2 "b"))))))

(define (test-remove-negremove-from-empty)

  (assert (equal? () ((hydra_lib_maps_delete 1) ()))))

;; singleton

(define (test-singleton-negsingle-entry)

  (assert (equal? (list (list 42 "hello")) ((hydra_lib_maps_singleton 42) "hello"))))

;; size

(define (test-size-negthree-entries)

  (assert (equal? 3 (hydra_lib_maps_size (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(define (test-size-negsingle-entry)

  (assert (equal? 1 (hydra_lib_maps_size (list (list 42 "test"))))))

(define (test-size-negempty-map)

  (assert (equal? 0 (hydra_lib_maps_size ()))))

;; toList

(define (test-tolist-negconvert-to-pairs)

  (assert (equal? (list (list 1 "a") (list 2 "b")) (hydra_lib_maps_to_list (list (list 1 "a") (list 2 "b"))))))

(define (test-tolist-negunsorted-keys)

  (assert (equal? (list (list 1 "a") (list 2 "b") (list 3 "c")) (hydra_lib_maps_to_list (list (list 1 "a") (list 2 "b") (list 3 "c"))))))

(define (test-tolist-negempty-map)

  (assert (equal? (list ) (hydra_lib_maps_to_list ()))))

;; union

(define (test-union-negunion-two-maps)

  (assert (equal? (list (list 1 "a") (list 2 "b") (list 3 "c")) ((hydra_lib_maps_union (list (list 1 "a") (list 2 "b"))) (list (list 2 "x") (list 3 "c"))))))

(define (test-union-negunion-with-empty)

  (assert (equal? (list (list 1 "a")) ((hydra_lib_maps_union (list (list 1 "a"))) ()))))

(define (test-union-negempty-with-map)

  (assert (equal? (list (list 1 "a")) ((hydra_lib_maps_union ()) (list (list 1 "a"))))))
