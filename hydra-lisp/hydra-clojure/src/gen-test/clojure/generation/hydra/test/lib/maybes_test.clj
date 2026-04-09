;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maybes primitives

(ns test-ns
  (:require [clojure.test :refer :all]))

;; apply

(deftest test-maybes-negapply-negboth-just

  (is (= just(8)

         just(8))))

(deftest test-maybes-negapply-negnothing-function

  (is (= nothing

         nothing)))

(deftest test-maybes-negapply-negnothing-value

  (is (= nothing

         nothing)))

;; bind

(deftest test-maybes-negbind-negjust-to-just

  (is (= just(10)

         just(10))))

(deftest test-maybes-negbind-negnothing-to-nothing

  (is (= nothing

         nothing)))

;; cases

(deftest test-maybes-negcases-negjust-applies-function

  (is (= 10

         10)))

(deftest test-maybes-negcases-negnothing-returns-default

  (is (= 99

         99)))

;; cat

(deftest test-maybes-negcat-negfilters-nothings

  (is (= [1, 2]

         [1, 2])))

(deftest test-maybes-negcat-negall-justs

  (is (= [1, 2]

         [1, 2])))

(deftest test-maybes-negcat-negall-nothings

  (is (= []

         [])))

(deftest test-maybes-negcat-negempty-list

  (is (= []

         [])))

;; compose

(deftest test-maybes-negcompose-negboth-succeed

  (is (= just(12)

         just(12))))

(deftest test-maybes-negcompose-negfirst-fails

  (is (= nothing

         nothing)))

(deftest test-maybes-negcompose-negsecond-fails

  (is (= nothing

         nothing)))

;; fromJust

(deftest test-maybes-negfromjust-negextract-from-just

  (is (= 42

         42)))

;; fromMaybe

(deftest test-maybes-negfrommaybe-negjust-value

  (is (= 42

         42)))

(deftest test-maybes-negfrommaybe-negnothing-with-default

  (is (= 99

         99)))

;; isJust

(deftest test-maybes-negisjust-negjust-value

  (is (= true

         true)))

(deftest test-maybes-negisjust-negnothing

  (is (= false

         false)))

;; isNothing

(deftest test-maybes-negisnothing-negjust-value

  (is (= false

         false)))

(deftest test-maybes-negisnothing-negnothing

  (is (= true

         true)))

;; map

(deftest test-maybes-negmap-negmaps-just-value

  (is (= just(10)

         just(10))))

(deftest test-maybes-negmap-negnothing-unchanged

  (is (= nothing

         nothing)))

;; mapMaybe

(deftest test-maybes-negmapmaybe-negfilter-and-transform

  (is (= [6, 8, 10]

         [6, 8, 10])))

(deftest test-maybes-negmapmaybe-negempty-result

  (is (= []

         [])))

(deftest test-maybes-negmapmaybe-negempty-input

  (is (= []

         [])))

;; maybe

(deftest test-maybes-negmaybe-negjust-value-applies-function

  (is (= 10

         10)))

(deftest test-maybes-negmaybe-negnothing-returns-default

  (is (= 99

         99)))

;; pure

(deftest test-maybes-negpure-negwraps-integer

  (is (= just(42)

         just(42))))

(deftest test-maybes-negpure-negwraps-string

  (is (= just("hello")

         just("hello"))))

;; toList

(deftest test-maybes-negtolist-negjust-value

  (is (= [42]

         [42])))

(deftest test-maybes-negtolist-negnothing

  (is (= []

         [])))
