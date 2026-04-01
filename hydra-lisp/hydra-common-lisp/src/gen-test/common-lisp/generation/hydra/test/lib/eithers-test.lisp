;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.eithers primitives

;; bind

(defun test-eithers-negbind-negbind-right-with-success ()

  (assert (equal right(2) right(2))))

(defun test-eithers-negbind-negbind-right-with-failure ()

  (assert (equal left(0) left(0))))

(defun test-eithers-negbind-negbind-left-returns-left-unchanged ()

  (assert (equal left(42) left(42))))

;; bimap

(defun test-eithers-negbimap-negmap-left-value ()

  (assert (equal left(10) left(10))))

(defun test-eithers-negbimap-negmap-right-value ()

  (assert (equal right(2) right(2))))

;; isLeft

(defun test-eithers-negisleft-negleft-value ()

  (assert (equal true true)))

(defun test-eithers-negisleft-negright-value ()

  (assert (equal false false)))

;; isRight

(defun test-eithers-negisright-negright-value ()

  (assert (equal true true)))

(defun test-eithers-negisright-negleft-value ()

  (assert (equal false false)))

;; fromLeft

(defun test-eithers-negfromleft-negextract-left ()

  (assert (equal 42 42)))

(defun test-eithers-negfromleft-neguse-default-for-right ()

  (assert (equal 99 99)))

;; fromRight

(defun test-eithers-negfromright-negextract-right ()

  (assert (equal "test" "test")))

(defun test-eithers-negfromright-neguse-default-for-left ()

  (assert (equal "default" "default")))

;; either

(defun test-eithers-negeither-negapply-left-function ()

  (assert (equal 10 10)))

(defun test-eithers-negeither-negapply-right-function ()

  (assert (equal 2 2)))

;; lefts

(defun test-eithers-neglefts-negfilter-left-values ()

  (assert (equal [1, 2] [1, 2])))

(defun test-eithers-neglefts-negall-lefts ()

  (assert (equal [1, 2] [1, 2])))

(defun test-eithers-neglefts-negall-rights ()

  (assert (equal [] [])))

(defun test-eithers-neglefts-negempty-list ()

  (assert (equal [] [])))

;; rights

(defun test-eithers-negrights-negfilter-right-values ()

  (assert (equal ["a", "b"] ["a", "b"])))

(defun test-eithers-negrights-negall-rights ()

  (assert (equal ["a", "b"] ["a", "b"])))

(defun test-eithers-negrights-negall-lefts ()

  (assert (equal [] [])))

(defun test-eithers-negrights-negempty-list ()

  (assert (equal [] [])))

;; partitionEithers

(defun test-eithers-negpartitioneithers-negpartition-mixed ()

  (assert (equal ([1, 2], ["a", "b"]) ([1, 2], ["a", "b"]))))

(defun test-eithers-negpartitioneithers-negall-lefts ()

  (assert (equal ([1, 2], []) ([1, 2], []))))

(defun test-eithers-negpartitioneithers-negall-rights ()

  (assert (equal ([], ["a", "b"]) ([], ["a", "b"]))))

(defun test-eithers-negpartitioneithers-negempty-list ()

  (assert (equal ([], []) ([], []))))

;; map

(defun test-eithers-negmap-negmap-right-value ()

  (assert (equal right(10) right(10))))

(defun test-eithers-negmap-negpreserve-left ()

  (assert (equal left(99) left(99))))

;; mapList

(defun test-eithers-negmaplist-negall-succeed ()

  (assert (equal right([2, 4, 6]) right([2, 4, 6]))))

(defun test-eithers-negmaplist-negfirst-fails ()

  (assert (equal left("zero") left("zero"))))

(defun test-eithers-negmaplist-negempty-list ()

  (assert (equal right([]) right([]))))

;; mapMaybe

(defun test-eithers-negmapmaybe-negjust-succeeds ()

  (assert (equal right(just(10)) right(just(10)))))

(defun test-eithers-negmapmaybe-negjust-fails ()

  (assert (equal left("zero") left("zero"))))

(defun test-eithers-negmapmaybe-negnothing ()

  (assert (equal right(nothing) right(nothing))))
