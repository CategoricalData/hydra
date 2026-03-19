;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; annotations

(require 'ert)

;; arbitrary annotations

(ert-deftest test-annotations-negarbitrary-annotations-negset-single-annotation--num1 ()

  (should (equal (list :annotated (make-hydra_core_annotated_term (list :literal (list :string "foo")) (list (cons "k1" (list :literal (list :integer (list :int32 42))))))) (funcall (funcall (funcall hydra_annotations_set_term_annotation "k1") (list :just (list :literal (list :integer (list :int32 42))))) (list :literal (list :string "foo"))))))

(ert-deftest test-annotations-negarbitrary-annotations-negset-single-annotation--num2 ()

  (should (equal (list :annotated (make-hydra_core_annotated_term (list :literal (list :string "bar")) (list (cons "myKey" (list :literal (list :integer (list :int32 -17))))))) (funcall (funcall (funcall hydra_annotations_set_term_annotation "myKey") (list :just (list :literal (list :integer (list :int32 -17))))) (list :literal (list :string "bar"))))))

(ert-deftest test-annotations-negarbitrary-annotations-negset-single-annotation--num3 ()

  (should (equal (list :annotated (make-hydra_core_annotated_term (list :literal (list :integer (list :int32 0))) (list (cons "x" (list :literal (list :string "hello")))))) (funcall (funcall (funcall hydra_annotations_set_term_annotation "x") (list :just (list :literal (list :string "hello")))) (list :literal (list :integer (list :int32 0)))))))

(ert-deftest test-annotations-negarbitrary-annotations-negget-existing-annotation--num1 ()

  (should (equal (list :just (list :literal (list :string "value"))) (funcall (funcall hydra_annotations_get_term_annotation "k1") (funcall (funcall (funcall hydra_annotations_set_term_annotation "k1") (list :just (list :literal (list :string "value")))) (list :literal (list :integer (list :int32 42))))))))

(ert-deftest test-annotations-negarbitrary-annotations-negget-existing-annotation--num2 ()

  (should (equal (list :just (list :literal (list :string ""))) (funcall (funcall hydra_annotations_get_term_annotation "foo") (funcall (funcall (funcall hydra_annotations_set_term_annotation "foo") (list :just (list :literal (list :string "")))) (list :literal (list :integer (list :int32 99))))))))

(ert-deftest test-annotations-negarbitrary-annotations-negget-existing-annotation--num3 ()

  (should (equal (list :just (list :literal (list :integer (list :int32 123)))) (funcall (funcall hydra_annotations_get_term_annotation "key") (funcall (funcall (funcall hydra_annotations_set_term_annotation "key") (list :just (list :literal (list :integer (list :int32 123))))) (list :literal (list :string "test")))))))

(ert-deftest test-annotations-negarbitrary-annotations-negget-missing-annotation--num1 ()

  (should (equal (list :nothing) (funcall (funcall hydra_annotations_get_term_annotation "k1") (list :literal (list :integer (list :int16 42)))))))

(ert-deftest test-annotations-negarbitrary-annotations-negget-missing-annotation--num2 ()

  (should (equal (list :nothing) (funcall (funcall hydra_annotations_get_term_annotation "nonexistent") (list :literal (list :string "hello"))))))

(ert-deftest test-annotations-negarbitrary-annotations-negget-missing-annotation--num3 ()

  (should (equal (list :nothing) (funcall (funcall hydra_annotations_get_term_annotation "k1") (funcall (funcall (funcall hydra_annotations_set_term_annotation "k2") (list :just (list :literal (list :integer (list :int32 1))))) (list :literal (list :integer (list :int32 42))))))))

(ert-deftest test-annotations-negarbitrary-annotations-negset-multiple-annotations--num1 ()

  (should (equal (list :annotated (make-hydra_core_annotated_term (list :literal (list :boolean t)) (list (cons "k1" (list :literal (list :string "first"))) (cons "k2" (list :literal (list :integer (list :int32 200))))))) (funcall (funcall (funcall hydra_annotations_set_term_annotation "k2") (list :just (list :literal (list :integer (list :int32 200))))) (funcall (funcall (funcall hydra_annotations_set_term_annotation "k1") (list :just (list :literal (list :string "first")))) (list :literal (list :boolean t)))))))

(ert-deftest test-annotations-negarbitrary-annotations-negset-multiple-annotations--num2 ()

  (should (equal (list :annotated (make-hydra_core_annotated_term (list :literal (list :string "test")) (list (cons "a" (list :literal (list :integer (list :int32 -5)))) (cons "b" (list :literal (list :integer (list :int32 0))))))) (funcall (funcall (funcall hydra_annotations_set_term_annotation "b") (list :just (list :literal (list :integer (list :int32 0))))) (funcall (funcall (funcall hydra_annotations_set_term_annotation "a") (list :just (list :literal (list :integer (list :int32 -5))))) (list :literal (list :string "test")))))))

(ert-deftest test-annotations-negarbitrary-annotations-negouter-annotation-overrides-inner--num1 ()

  (should (equal (list :annotated (make-hydra_core_annotated_term (list :literal (list :string "bar")) (list (cons "k1" (list :literal (list :string "outer")))))) (funcall (funcall (funcall hydra_annotations_set_term_annotation "k1") (list :just (list :literal (list :string "outer")))) (funcall (funcall (funcall hydra_annotations_set_term_annotation "k1") (list :just (list :literal (list :string "inner")))) (list :literal (list :string "bar")))))))

(ert-deftest test-annotations-negarbitrary-annotations-negouter-annotation-overrides-inner--num2 ()

  (should (equal (list :annotated (make-hydra_core_annotated_term (list :literal (list :integer (list :int32 42))) (list (cons "x" (list :literal (list :string "new")))))) (funcall (funcall (funcall hydra_annotations_set_term_annotation "x") (list :just (list :literal (list :string "new")))) (funcall (funcall (funcall hydra_annotations_set_term_annotation "x") (list :just (list :literal (list :string "old")))) (list :literal (list :integer (list :int32 42))))))))

(ert-deftest test-annotations-negarbitrary-annotations-negouter-annotation-overrides-inner--num3 ()

  (should (equal (list :annotated (make-hydra_core_annotated_term (list :literal (list :boolean nil)) (list (cons "key" (list :literal (list :integer (list :int32 999))))))) (funcall (funcall (funcall hydra_annotations_set_term_annotation "key") (list :just (list :literal (list :integer (list :int32 999))))) (funcall (funcall (funcall hydra_annotations_set_term_annotation "key") (list :just (list :literal (list :integer (list :int32 1))))) (list :literal (list :boolean nil)))))))

(ert-deftest test-annotations-negarbitrary-annotations-negunset-single-annotation--num1 ()

  (should (equal (list :literal (list :integer (list :int64 137))) (funcall (funcall (funcall hydra_annotations_set_term_annotation "k1") (list :nothing)) (funcall (funcall (funcall hydra_annotations_set_term_annotation "k1") (list :just (list :literal (list :string "foo")))) (list :literal (list :integer (list :int64 137))))))))

(ert-deftest test-annotations-negarbitrary-annotations-negunset-single-annotation--num2 ()

  (should (equal (list :literal (list :string "test")) (funcall (funcall (funcall hydra_annotations_set_term_annotation "x") (list :nothing)) (funcall (funcall (funcall hydra_annotations_set_term_annotation "x") (list :just (list :literal (list :integer (list :int32 42))))) (list :literal (list :string "test")))))))

(ert-deftest test-annotations-negarbitrary-annotations-negunset-one-of-multiple-annotations--num1 ()

  (should (equal (list :annotated (make-hydra_core_annotated_term (list :literal (list :integer (list :int64 137))) (list (cons "k2" (list :literal (list :integer (list :int32 200))))))) (funcall (funcall (funcall hydra_annotations_set_term_annotation "k1") (list :nothing)) (funcall (funcall (funcall hydra_annotations_set_term_annotation "k2") (list :just (list :literal (list :integer (list :int32 200))))) (funcall (funcall (funcall hydra_annotations_set_term_annotation "k1") (list :just (list :literal (list :string "first")))) (list :literal (list :integer (list :int64 137)))))))))

(ert-deftest test-annotations-negarbitrary-annotations-negunset-one-of-multiple-annotations--num2 ()

  (should (equal (list :annotated (make-hydra_core_annotated_term (list :literal (list :string "x")) (list (cons "a" (list :literal (list :integer (list :int32 1))))))) (funcall (funcall (funcall hydra_annotations_set_term_annotation "b") (list :nothing)) (funcall (funcall (funcall hydra_annotations_set_term_annotation "b") (list :just (list :literal (list :integer (list :int32 2))))) (funcall (funcall (funcall hydra_annotations_set_term_annotation "a") (list :just (list :literal (list :integer (list :int32 1))))) (list :literal (list :string "x"))))))))

;; descriptions

(ert-deftest test-annotations-negdescriptions-negset-description--num1 ()

  (should (equal (list :annotated (make-hydra_core_annotated_term (list :literal (list :string "foo")) (list (cons "description" (list :literal (list :string "my description")))))) (funcall (funcall hydra_annotations_set_term_description (list :just "my description")) (list :literal (list :string "foo"))))))

(ert-deftest test-annotations-negdescriptions-negset-description--num2 ()

  (should (equal (list :annotated (make-hydra_core_annotated_term (list :literal (list :integer (list :int32 42))) (list (cons "description" (list :literal (list :string "")))))) (funcall (funcall hydra_annotations_set_term_description (list :just "")) (list :literal (list :integer (list :int32 42)))))))

(ert-deftest test-annotations-negdescriptions-negset-description--num3 ()

  (should (equal (list :annotated (make-hydra_core_annotated_term (list :literal (list :boolean t)) (list (cons "description" (list :literal (list :string "A longer description with spaces")))))) (funcall (funcall hydra_annotations_set_term_description (list :just "A longer description with spaces")) (list :literal (list :boolean t))))))

(ert-deftest test-annotations-negdescriptions-negget-existing-description--num1 ()

  (should (equal (list :right (list :just "hello")) (funcall (funcall (funcall hydra_annotations_get_term_description hydra_lexical_empty_context) hydra_lexical_empty_graph) (funcall (funcall hydra_annotations_set_term_description (list :just "hello")) (list :literal (list :integer (list :int32 42))))))))

(ert-deftest test-annotations-negdescriptions-negget-existing-description--num2 ()

  (should (equal (list :right (list :just "")) (funcall (funcall (funcall hydra_annotations_get_term_description hydra_lexical_empty_context) hydra_lexical_empty_graph) (funcall (funcall hydra_annotations_set_term_description (list :just "")) (list :literal (list :string "test")))))))

(ert-deftest test-annotations-negdescriptions-negget-existing-description--num3 ()

  (should (equal (list :right (list :just "desc")) (funcall (funcall (funcall hydra_annotations_get_term_description hydra_lexical_empty_context) hydra_lexical_empty_graph) (funcall (funcall hydra_annotations_set_term_description (list :just "desc")) (list :literal (list :boolean nil)))))))

(ert-deftest test-annotations-negdescriptions-negget-missing-description--num1 ()

  (should (equal (list :right (list :nothing)) (funcall (funcall (funcall hydra_annotations_get_term_description hydra_lexical_empty_context) hydra_lexical_empty_graph) (list :literal (list :integer (list :int16 42)))))))

(ert-deftest test-annotations-negdescriptions-negget-missing-description--num2 ()

  (should (equal (list :right (list :nothing)) (funcall (funcall (funcall hydra_annotations_get_term_description hydra_lexical_empty_context) hydra_lexical_empty_graph) (list :literal (list :string "no description here"))))))

(ert-deftest test-annotations-negdescriptions-negget-missing-description--num3 ()

  (should (equal (list :right (list :nothing)) (funcall (funcall (funcall hydra_annotations_get_term_description hydra_lexical_empty_context) hydra_lexical_empty_graph) (list :literal (list :integer (list :int32 0)))))))

(ert-deftest test-annotations-negdescriptions-negouter-description-overrides-inner--num1 ()

  (should (equal (list :annotated (make-hydra_core_annotated_term (list :literal (list :string "bar")) (list (cons "description" (list :literal (list :string "outer")))))) (funcall (funcall hydra_annotations_set_term_description (list :just "outer")) (funcall (funcall hydra_annotations_set_term_description (list :just "inner")) (list :literal (list :string "bar")))))))

(ert-deftest test-annotations-negdescriptions-negouter-description-overrides-inner--num2 ()

  (should (equal (list :annotated (make-hydra_core_annotated_term (list :literal (list :integer (list :int32 99))) (list (cons "description" (list :literal (list :string "new")))))) (funcall (funcall hydra_annotations_set_term_description (list :just "new")) (funcall (funcall hydra_annotations_set_term_description (list :just "old")) (list :literal (list :integer (list :int32 99))))))))

(ert-deftest test-annotations-negdescriptions-negunset-description--num1 ()

  (should (equal (list :literal (list :integer (list :int64 137))) (funcall (funcall hydra_annotations_set_term_description (list :nothing)) (funcall (funcall hydra_annotations_set_term_description (list :just "desc")) (list :literal (list :integer (list :int64 137))))))))

(ert-deftest test-annotations-negdescriptions-negunset-description--num2 ()

  (should (equal (list :literal (list :string "test")) (funcall (funcall hydra_annotations_set_term_description (list :nothing)) (funcall (funcall hydra_annotations_set_term_description (list :just "to be removed")) (list :literal (list :string "test")))))))

;; layered annotations

(ert-deftest test-annotations-neglayered-annotations-negget-annotation-from-unannotated-term ()

  (should (equal (list :nothing) (funcall (funcall hydra_annotations_get_term_annotation "one") (list :literal (list :integer (list :int32 42)))))))

(ert-deftest test-annotations-neglayered-annotations-negget-annotation-from-singly-annotated-term ()

  (should (equal (list :just (list :literal (list :integer (list :int32 1)))) (funcall (funcall hydra_annotations_get_term_annotation "one") (list :annotated (make-hydra_core_annotated_term (list :literal (list :integer (list :int32 42))) (list (cons "one" (list :literal (list :integer (list :int32 1)))))))))))

(ert-deftest test-annotations-neglayered-annotations-negget-inner-annotation-from-doubly-annotated-term ()

  (should (equal (list :just (list :literal (list :integer (list :int32 1)))) (funcall (funcall hydra_annotations_get_term_annotation "one") (list :annotated (make-hydra_core_annotated_term (list :annotated (make-hydra_core_annotated_term (list :literal (list :integer (list :int32 42))) (list (cons "one" (list :literal (list :integer (list :int32 1))))))) (list (cons "two" (list :literal (list :integer (list :int32 2)))))))))))

(ert-deftest test-annotations-neglayered-annotations-negget-outer-annotation-from-doubly-annotated-term ()

  (should (equal (list :just (list :literal (list :integer (list :int32 2)))) (funcall (funcall hydra_annotations_get_term_annotation "two") (list :annotated (make-hydra_core_annotated_term (list :annotated (make-hydra_core_annotated_term (list :literal (list :integer (list :int32 42))) (list (cons "one" (list :literal (list :integer (list :int32 1))))))) (list (cons "two" (list :literal (list :integer (list :int32 2)))))))))))

(ert-deftest test-annotations-neglayered-annotations-negget-non-negoverridden-annotation-from-triply-annotated-term ()

  (should (equal (list :just (list :literal (list :integer (list :int32 2)))) (funcall (funcall hydra_annotations_get_term_annotation "two") (list :annotated (make-hydra_core_annotated_term (list :annotated (make-hydra_core_annotated_term (list :annotated (make-hydra_core_annotated_term (list :literal (list :integer (list :int32 42))) (list (cons "one" (list :literal (list :integer (list :int32 1))))))) (list (cons "two" (list :literal (list :integer (list :int32 2))))))) (list (cons "one" (list :literal (list :integer (list :int32 99)))))))))))

(ert-deftest test-annotations-neglayered-annotations-negouter-annotation-overrides-inner-in-layered-term ()

  (should (equal (list :just (list :literal (list :integer (list :int32 99)))) (funcall (funcall hydra_annotations_get_term_annotation "one") (list :annotated (make-hydra_core_annotated_term (list :annotated (make-hydra_core_annotated_term (list :annotated (make-hydra_core_annotated_term (list :literal (list :integer (list :int32 42))) (list (cons "one" (list :literal (list :integer (list :int32 1))))))) (list (cons "two" (list :literal (list :integer (list :int32 2))))))) (list (cons "one" (list :literal (list :integer (list :int32 99)))))))))))
