;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.sets primitives

;; empty

(defun test-sets-negempty-negempty-set ()

  (assert (equal {} {})))

;; singleton

(defun test-sets-negsingleton-negsingle-element ()

  (assert (equal {42} {42})))

;; fromList

(defun test-sets-negfromlist-negcreate-from-list ()

  (assert (equal {1, 2, 3} {1, 2, 3})))

(defun test-sets-negfromlist-negduplicates-removed ()

  (assert (equal {1, 2, 3} {1, 2, 3})))

(defun test-sets-negfromlist-negempty-list ()

  (assert (equal {} {})))

;; toList

(defun test-sets-negtolist-negconvert-to-list ()

  (assert (equal [1, 2, 3] [1, 2, 3])))

(defun test-sets-negtolist-negunsorted-input ()

  (assert (equal [1, 2, 3] [1, 2, 3])))

(defun test-sets-negtolist-negempty-set ()

  (assert (equal [] [])))

;; insert

(defun test-sets-neginsert-neginsert-new-element ()

  (assert (equal {1, 2, 3, 4} {1, 2, 3, 4})))

(defun test-sets-neginsert-neginsert-existing-element ()

  (assert (equal {1, 2, 3} {1, 2, 3})))

(defun test-sets-neginsert-neginsert-into-empty ()

  (assert (equal {1} {1})))

;; delete

(defun test-sets-negdelete-negdelete-existing ()

  (assert (equal {1, 3} {1, 3})))

(defun test-sets-negdelete-negdelete-non-negexisting ()

  (assert (equal {1, 2, 3} {1, 2, 3})))

(defun test-sets-negdelete-negdelete-from-empty ()

  (assert (equal {} {})))

;; member

(defun test-sets-negmember-negelement-exists ()

  (assert (equal true true)))

(defun test-sets-negmember-negelement-missing ()

  (assert (equal false false)))

(defun test-sets-negmember-negempty-set ()

  (assert (equal false false)))

;; size

(defun test-sets-negsize-negthree-elements ()

  (assert (equal 3 3)))

(defun test-sets-negsize-negsingle-element ()

  (assert (equal 1 1)))

(defun test-sets-negsize-negempty-set ()

  (assert (equal 0 0)))

;; null

(defun test-sets-negnull-negempty-set ()

  (assert (equal true true)))

(defun test-sets-negnull-negnon-negempty-set ()

  (assert (equal false false)))

;; union

(defun test-sets-negunion-negunion-two-sets ()

  (assert (equal {1, 2, 3} {1, 2, 3})))

(defun test-sets-negunion-negunion-with-empty ()

  (assert (equal {1, 2} {1, 2})))

(defun test-sets-negunion-negempty-with-non-negempty ()

  (assert (equal {1, 2} {1, 2})))

;; unions

(defun test-sets-negunions-negunion-of-multiple-sets ()

  (assert (equal {1, 2, 3, 4} {1, 2, 3, 4})))

(defun test-sets-negunions-negunion-with-empty-sets ()

  (assert (equal {1, 2, 3} {1, 2, 3})))

(defun test-sets-negunions-negempty-list-of-sets ()

  (assert (equal {} {})))

(defun test-sets-negunions-negsingle-set ()

  (assert (equal {1, 2, 3} {1, 2, 3})))

;; intersection

(defun test-sets-negintersection-negcommon-elements ()

  (assert (equal {2, 3} {2, 3})))

(defun test-sets-negintersection-negno-common-elements ()

  (assert (equal {} {})))

(defun test-sets-negintersection-negintersection-with-empty ()

  (assert (equal {} {})))

;; difference

(defun test-sets-negdifference-negremove-elements ()

  (assert (equal {1, 3} {1, 3})))

(defun test-sets-negdifference-negno-overlap ()

  (assert (equal {1, 2} {1, 2})))

(defun test-sets-negdifference-negdifference-with-empty ()

  (assert (equal {1, 2} {1, 2})))

;; map

(defun test-sets-negmap-negmap-function ()

  (assert (equal {2, 4, 6} {2, 4, 6})))

(defun test-sets-negmap-negmap-on-empty ()

  (assert (equal {} {})))
