;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maybes primitives

;; apply

(defun test-maybes-negapply-negboth-just ()

  (assert (equal just(8) just(8))))

(defun test-maybes-negapply-negnothing-function ()

  (assert (equal nothing nothing)))

(defun test-maybes-negapply-negnothing-value ()

  (assert (equal nothing nothing)))

;; bind

(defun test-maybes-negbind-negjust-to-just ()

  (assert (equal just(10) just(10))))

(defun test-maybes-negbind-negnothing-to-nothing ()

  (assert (equal nothing nothing)))

;; cases

(defun test-maybes-negcases-negjust-applies-function ()

  (assert (equal 10 10)))

(defun test-maybes-negcases-negnothing-returns-default ()

  (assert (equal 99 99)))

;; cat

(defun test-maybes-negcat-negfilters-nothings ()

  (assert (equal [1, 2] [1, 2])))

(defun test-maybes-negcat-negall-justs ()

  (assert (equal [1, 2] [1, 2])))

(defun test-maybes-negcat-negall-nothings ()

  (assert (equal [] [])))

(defun test-maybes-negcat-negempty-list ()

  (assert (equal [] [])))

;; compose

(defun test-maybes-negcompose-negboth-succeed ()

  (assert (equal just(12) just(12))))

(defun test-maybes-negcompose-negfirst-fails ()

  (assert (equal nothing nothing)))

(defun test-maybes-negcompose-negsecond-fails ()

  (assert (equal nothing nothing)))

;; fromJust

(defun test-maybes-negfromjust-negextract-from-just ()

  (assert (equal 42 42)))

;; fromMaybe

(defun test-maybes-negfrommaybe-negjust-value ()

  (assert (equal 42 42)))

(defun test-maybes-negfrommaybe-negnothing-with-default ()

  (assert (equal 99 99)))

;; isJust

(defun test-maybes-negisjust-negjust-value ()

  (assert (equal true true)))

(defun test-maybes-negisjust-negnothing ()

  (assert (equal false false)))

;; isNothing

(defun test-maybes-negisnothing-negjust-value ()

  (assert (equal false false)))

(defun test-maybes-negisnothing-negnothing ()

  (assert (equal true true)))

;; map

(defun test-maybes-negmap-negmaps-just-value ()

  (assert (equal just(10) just(10))))

(defun test-maybes-negmap-negnothing-unchanged ()

  (assert (equal nothing nothing)))

;; mapMaybe

(defun test-maybes-negmapmaybe-negfilter-and-transform ()

  (assert (equal [6, 8, 10] [6, 8, 10])))

(defun test-maybes-negmapmaybe-negempty-result ()

  (assert (equal [] [])))

(defun test-maybes-negmapmaybe-negempty-input ()

  (assert (equal [] [])))

;; maybe

(defun test-maybes-negmaybe-negjust-value-applies-function ()

  (assert (equal 10 10)))

(defun test-maybes-negmaybe-negnothing-returns-default ()

  (assert (equal 99 99)))

;; pure

(defun test-maybes-negpure-negwraps-integer ()

  (assert (equal just(42) just(42))))

(defun test-maybes-negpure-negwraps-string ()

  (assert (equal just("hello") just("hello"))))

;; toList

(defun test-maybes-negtolist-negjust-value ()

  (assert (equal [42] [42])))

(defun test-maybes-negtolist-negnothing ()

  (assert (equal [] [])))
