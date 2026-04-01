;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maps primitives

;; alter

(defun test-maps-negalter-neginsert-new-key ()

  (assert (equal {1: "a", 2: "b", 3: "new"} {1: "a", 2: "b", 3: "new"})))

(defun test-maps-negalter-negupdate-existing-key ()

  (assert (equal {1: "a", 2: "updated"} {1: "a", 2: "updated"})))

(defun test-maps-negalter-negdelete-key ()

  (assert (equal {1: "a"} {1: "a"})))

;; bimap

(defun test-maps-negbimap-negtransform-both ()

  (assert (equal {2: "A", 4: "B"} {2: "A", 4: "B"})))

(defun test-maps-negbimap-negempty-map ()

  (assert (equal {} {})))

;; elems

(defun test-maps-negelems-negget-all-elements ()

  (assert (equal ["a", "b"] ["a", "b"])))

(defun test-maps-negelems-negunsorted-keys ()

  (assert (equal ["a", "b", "c"] ["a", "b", "c"])))

(defun test-maps-negelems-negempty-map ()

  (assert (equal [] [])))

;; empty

(defun test-maps-negempty-negempty-map ()

  (assert (equal {} {})))

;; filter

(defun test-maps-negfilter-negfilter-values-starting-with-a ()

  (assert (equal {1: "a", 3: "ab"} {1: "a", 3: "ab"})))

(defun test-maps-negfilter-negfilter-all ()

  (assert (equal {} {})))

(defun test-maps-negfilter-negempty-map ()

  (assert (equal {} {})))

;; filterWithKey

(defun test-maps-negfilterwithkey-negfilter-by-key-1 ()

  (assert (equal {2: "b", 3: "c"} {2: "b", 3: "c"})))

(defun test-maps-negfilterwithkey-negfilter-all ()

  (assert (equal {} {})))

(defun test-maps-negfilterwithkey-negempty-map ()

  (assert (equal {} {})))

;; findWithDefault

(defun test-maps-negfindwithdefault-negfind-existing ()

  (assert (equal b b)))

(defun test-maps-negfindwithdefault-neguse-default ()

  (assert (equal default default)))

;; fromList

(defun test-maps-negfromlist-negcreate-from-pairs ()

  (assert (equal {1: "a", 2: "b"} {1: "a", 2: "b"})))

(defun test-maps-negfromlist-negduplicate-keys ()

  (assert (equal {1: "b"} {1: "b"})))

(defun test-maps-negfromlist-negempty-list ()

  (assert (equal {} {})))

;; insert

(defun test-maps-neginsert-neginsert-new-key ()

  (assert (equal {1: "a", 2: "b", 3: "c"} {1: "a", 2: "b", 3: "c"})))

(defun test-maps-neginsert-negupdate-existing ()

  (assert (equal {1: "a", 2: "updated"} {1: "a", 2: "updated"})))

(defun test-maps-neginsert-neginsert-into-empty ()

  (assert (equal {1: "x"} {1: "x"})))

;; keys

(defun test-maps-negkeys-negget-all-keys ()

  (assert (equal [1, 2, 3] [1, 2, 3])))

(defun test-maps-negkeys-negunsorted-keys ()

  (assert (equal [1, 2, 3] [1, 2, 3])))

(defun test-maps-negkeys-negempty-map ()

  (assert (equal [] [])))

;; lookup

(defun test-maps-neglookup-negfind-existing-key ()

  (assert (equal just("b") just("b"))))

(defun test-maps-neglookup-negkey-not-found ()

  (assert (equal nothing nothing)))

(defun test-maps-neglookup-neglookup-in-empty ()

  (assert (equal nothing nothing)))

;; map

(defun test-maps-negmap-negmap-over-values ()

  (assert (equal {1: "A", 2: "B"} {1: "A", 2: "B"})))

(defun test-maps-negmap-negmap-empty ()

  (assert (equal {} {})))

;; mapKeys

(defun test-maps-negmapkeys-negdouble-keys ()

  (assert (equal {2: "a", 4: "b"} {2: "a", 4: "b"})))

(defun test-maps-negmapkeys-negempty-map ()

  (assert (equal {} {})))

;; member

(defun test-maps-negmember-negkey-exists ()

  (assert (equal true true)))

(defun test-maps-negmember-negkey-missing ()

  (assert (equal false false)))

(defun test-maps-negmember-negempty-map ()

  (assert (equal false false)))

;; null

(defun test-maps-negnull-negempty-map ()

  (assert (equal true true)))

(defun test-maps-negnull-negnon-negempty-map ()

  (assert (equal false false)))

;; remove

(defun test-maps-negremove-negremove-existing ()

  (assert (equal {1: "a", 3: "c"} {1: "a", 3: "c"})))

(defun test-maps-negremove-negremove-non-negexisting ()

  (assert (equal {1: "a", 2: "b"} {1: "a", 2: "b"})))

(defun test-maps-negremove-negremove-from-empty ()

  (assert (equal {} {})))

;; singleton

(defun test-maps-negsingleton-negsingle-entry ()

  (assert (equal {42: "hello"} {42: "hello"})))

;; size

(defun test-maps-negsize-negthree-entries ()

  (assert (equal 3 3)))

(defun test-maps-negsize-negsingle-entry ()

  (assert (equal 1 1)))

(defun test-maps-negsize-negempty-map ()

  (assert (equal 0 0)))

;; toList

(defun test-maps-negtolist-negconvert-to-pairs ()

  (assert (equal [(1, "a"), (2, "b")] [(1, "a"), (2, "b")])))

(defun test-maps-negtolist-negunsorted-keys ()

  (assert (equal [(1, "a"), (2, "b"), (3, "c")] [(1, "a"), (2, "b"), (3, "c")])))

(defun test-maps-negtolist-negempty-map ()

  (assert (equal [] [])))

;; union

(defun test-maps-negunion-negunion-two-maps ()

  (assert (equal {1: "a", 2: "b", 3: "c"} {1: "a", 2: "b", 3: "c"})))

(defun test-maps-negunion-negunion-with-empty ()

  (assert (equal {1: "a"} {1: "a"})))

(defun test-maps-negunion-negempty-with-map ()

  (assert (equal {1: "a"} {1: "a"})))
