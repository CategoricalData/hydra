;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maps primitives

(import (scheme base))

;; alter

(define (test-maps-negalter-neginsert-new-key)

  (assert (equal? {1: "a", 2: "b", 3: "new"} {1: "a", 2: "b", 3: "new"})))

(define (test-maps-negalter-negupdate-existing-key)

  (assert (equal? {1: "a", 2: "updated"} {1: "a", 2: "updated"})))

(define (test-maps-negalter-negdelete-key)

  (assert (equal? {1: "a"} {1: "a"})))

;; bimap

(define (test-maps-negbimap-negtransform-both)

  (assert (equal? {2: "A", 4: "B"} {2: "A", 4: "B"})))

(define (test-maps-negbimap-negempty-map)

  (assert (equal? {} {})))

;; elems

(define (test-maps-negelems-negget-all-elements)

  (assert (equal? ["a", "b"] ["a", "b"])))

(define (test-maps-negelems-negunsorted-keys)

  (assert (equal? ["a", "b", "c"] ["a", "b", "c"])))

(define (test-maps-negelems-negempty-map)

  (assert (equal? [] [])))

;; empty

(define (test-maps-negempty-negempty-map)

  (assert (equal? {} {})))

;; filter

(define (test-maps-negfilter-negfilter-values-starting-with-a)

  (assert (equal? {1: "a", 3: "ab"} {1: "a", 3: "ab"})))

(define (test-maps-negfilter-negfilter-all)

  (assert (equal? {} {})))

(define (test-maps-negfilter-negempty-map)

  (assert (equal? {} {})))

;; filterWithKey

(define (test-maps-negfilterwithkey-negfilter-by-key-1)

  (assert (equal? {2: "b", 3: "c"} {2: "b", 3: "c"})))

(define (test-maps-negfilterwithkey-negfilter-all)

  (assert (equal? {} {})))

(define (test-maps-negfilterwithkey-negempty-map)

  (assert (equal? {} {})))

;; findWithDefault

(define (test-maps-negfindwithdefault-negfind-existing)

  (assert (equal? b b)))

(define (test-maps-negfindwithdefault-neguse-default)

  (assert (equal? default default)))

;; fromList

(define (test-maps-negfromlist-negcreate-from-pairs)

  (assert (equal? {1: "a", 2: "b"} {1: "a", 2: "b"})))

(define (test-maps-negfromlist-negduplicate-keys)

  (assert (equal? {1: "b"} {1: "b"})))

(define (test-maps-negfromlist-negempty-list)

  (assert (equal? {} {})))

;; insert

(define (test-maps-neginsert-neginsert-new-key)

  (assert (equal? {1: "a", 2: "b", 3: "c"} {1: "a", 2: "b", 3: "c"})))

(define (test-maps-neginsert-negupdate-existing)

  (assert (equal? {1: "a", 2: "updated"} {1: "a", 2: "updated"})))

(define (test-maps-neginsert-neginsert-into-empty)

  (assert (equal? {1: "x"} {1: "x"})))

;; keys

(define (test-maps-negkeys-negget-all-keys)

  (assert (equal? [1, 2, 3] [1, 2, 3])))

(define (test-maps-negkeys-negunsorted-keys)

  (assert (equal? [1, 2, 3] [1, 2, 3])))

(define (test-maps-negkeys-negempty-map)

  (assert (equal? [] [])))

;; lookup

(define (test-maps-neglookup-negfind-existing-key)

  (assert (equal? just("b") just("b"))))

(define (test-maps-neglookup-negkey-not-found)

  (assert (equal? nothing nothing)))

(define (test-maps-neglookup-neglookup-in-empty)

  (assert (equal? nothing nothing)))

;; map

(define (test-maps-negmap-negmap-over-values)

  (assert (equal? {1: "A", 2: "B"} {1: "A", 2: "B"})))

(define (test-maps-negmap-negmap-empty)

  (assert (equal? {} {})))

;; mapKeys

(define (test-maps-negmapkeys-negdouble-keys)

  (assert (equal? {2: "a", 4: "b"} {2: "a", 4: "b"})))

(define (test-maps-negmapkeys-negempty-map)

  (assert (equal? {} {})))

;; member

(define (test-maps-negmember-negkey-exists)

  (assert (equal? true true)))

(define (test-maps-negmember-negkey-missing)

  (assert (equal? false false)))

(define (test-maps-negmember-negempty-map)

  (assert (equal? false false)))

;; null

(define (test-maps-negnull-negempty-map)

  (assert (equal? true true)))

(define (test-maps-negnull-negnon-negempty-map)

  (assert (equal? false false)))

;; remove

(define (test-maps-negremove-negremove-existing)

  (assert (equal? {1: "a", 3: "c"} {1: "a", 3: "c"})))

(define (test-maps-negremove-negremove-non-negexisting)

  (assert (equal? {1: "a", 2: "b"} {1: "a", 2: "b"})))

(define (test-maps-negremove-negremove-from-empty)

  (assert (equal? {} {})))

;; singleton

(define (test-maps-negsingleton-negsingle-entry)

  (assert (equal? {42: "hello"} {42: "hello"})))

;; size

(define (test-maps-negsize-negthree-entries)

  (assert (equal? 3 3)))

(define (test-maps-negsize-negsingle-entry)

  (assert (equal? 1 1)))

(define (test-maps-negsize-negempty-map)

  (assert (equal? 0 0)))

;; toList

(define (test-maps-negtolist-negconvert-to-pairs)

  (assert (equal? [(1, "a"), (2, "b")] [(1, "a"), (2, "b")])))

(define (test-maps-negtolist-negunsorted-keys)

  (assert (equal? [(1, "a"), (2, "b"), (3, "c")] [(1, "a"), (2, "b"), (3, "c")])))

(define (test-maps-negtolist-negempty-map)

  (assert (equal? [] [])))

;; union

(define (test-maps-negunion-negunion-two-maps)

  (assert (equal? {1: "a", 2: "b", 3: "c"} {1: "a", 2: "b", 3: "c"})))

(define (test-maps-negunion-negunion-with-empty)

  (assert (equal? {1: "a"} {1: "a"})))

(define (test-maps-negunion-negempty-with-map)

  (assert (equal? {1: "a"} {1: "a"})))
