;; Note: this is an automatically generated file. Do not edit.
;; annotations

(import (scheme base))

;; arbitrary annotations

(define (test-arbitrary-annotations-negset-single-annotation--num1)

  (assert (equal? (list :annotated (->hydra_core_annotated_term (list :literal (list :string "foo")) (list (list "k1" (list :literal (list :integer (list :int32 42))))))) (((hydra_annotations_set_term_annotation "k1") (list :just (list :literal (list :integer (list :int32 42))))) (list :literal (list :string "foo"))))))

(define (test-arbitrary-annotations-negset-single-annotation--num2)

  (assert (equal? (list :annotated (->hydra_core_annotated_term (list :literal (list :string "bar")) (list (list "myKey" (list :literal (list :integer (list :int32 -17))))))) (((hydra_annotations_set_term_annotation "myKey") (list :just (list :literal (list :integer (list :int32 -17))))) (list :literal (list :string "bar"))))))

(define (test-arbitrary-annotations-negset-single-annotation--num3)

  (assert (equal? (list :annotated (->hydra_core_annotated_term (list :literal (list :integer (list :int32 0))) (list (list "x" (list :literal (list :string "hello")))))) (((hydra_annotations_set_term_annotation "x") (list :just (list :literal (list :string "hello")))) (list :literal (list :integer (list :int32 0)))))))

(define (test-arbitrary-annotations-negget-existing-annotation--num1)

  (assert (equal? (list :just (list :literal (list :string "value"))) ((hydra_annotations_get_term_annotation "k1") (((hydra_annotations_set_term_annotation "k1") (list :just (list :literal (list :string "value")))) (list :literal (list :integer (list :int32 42))))))))

(define (test-arbitrary-annotations-negget-existing-annotation--num2)

  (assert (equal? (list :just (list :literal (list :string ""))) ((hydra_annotations_get_term_annotation "foo") (((hydra_annotations_set_term_annotation "foo") (list :just (list :literal (list :string "")))) (list :literal (list :integer (list :int32 99))))))))

(define (test-arbitrary-annotations-negget-existing-annotation--num3)

  (assert (equal? (list :just (list :literal (list :integer (list :int32 123)))) ((hydra_annotations_get_term_annotation "key") (((hydra_annotations_set_term_annotation "key") (list :just (list :literal (list :integer (list :int32 123))))) (list :literal (list :string "test")))))))

(define (test-arbitrary-annotations-negget-missing-annotation--num1)

  (assert (equal? (list :nothing) ((hydra_annotations_get_term_annotation "k1") (list :literal (list :integer (list :int16 42)))))))

(define (test-arbitrary-annotations-negget-missing-annotation--num2)

  (assert (equal? (list :nothing) ((hydra_annotations_get_term_annotation "nonexistent") (list :literal (list :string "hello"))))))

(define (test-arbitrary-annotations-negget-missing-annotation--num3)

  (assert (equal? (list :nothing) ((hydra_annotations_get_term_annotation "k1") (((hydra_annotations_set_term_annotation "k2") (list :just (list :literal (list :integer (list :int32 1))))) (list :literal (list :integer (list :int32 42))))))))

(define (test-arbitrary-annotations-negset-multiple-annotations--num1)

  (assert (equal? (list :annotated (->hydra_core_annotated_term (list :literal (list :boolean #t)) (list (list "k1" (list :literal (list :string "first"))) (list "k2" (list :literal (list :integer (list :int32 200))))))) (((hydra_annotations_set_term_annotation "k2") (list :just (list :literal (list :integer (list :int32 200))))) (((hydra_annotations_set_term_annotation "k1") (list :just (list :literal (list :string "first")))) (list :literal (list :boolean #t)))))))

(define (test-arbitrary-annotations-negset-multiple-annotations--num2)

  (assert (equal? (list :annotated (->hydra_core_annotated_term (list :literal (list :string "test")) (list (list "a" (list :literal (list :integer (list :int32 -5)))) (list "b" (list :literal (list :integer (list :int32 0))))))) (((hydra_annotations_set_term_annotation "b") (list :just (list :literal (list :integer (list :int32 0))))) (((hydra_annotations_set_term_annotation "a") (list :just (list :literal (list :integer (list :int32 -5))))) (list :literal (list :string "test")))))))

(define (test-arbitrary-annotations-negouter-annotation-overrides-inner--num1)

  (assert (equal? (list :annotated (->hydra_core_annotated_term (list :literal (list :string "bar")) (list (list "k1" (list :literal (list :string "outer")))))) (((hydra_annotations_set_term_annotation "k1") (list :just (list :literal (list :string "outer")))) (((hydra_annotations_set_term_annotation "k1") (list :just (list :literal (list :string "inner")))) (list :literal (list :string "bar")))))))

(define (test-arbitrary-annotations-negouter-annotation-overrides-inner--num2)

  (assert (equal? (list :annotated (->hydra_core_annotated_term (list :literal (list :integer (list :int32 42))) (list (list "x" (list :literal (list :string "new")))))) (((hydra_annotations_set_term_annotation "x") (list :just (list :literal (list :string "new")))) (((hydra_annotations_set_term_annotation "x") (list :just (list :literal (list :string "old")))) (list :literal (list :integer (list :int32 42))))))))

(define (test-arbitrary-annotations-negouter-annotation-overrides-inner--num3)

  (assert (equal? (list :annotated (->hydra_core_annotated_term (list :literal (list :boolean #f)) (list (list "key" (list :literal (list :integer (list :int32 999))))))) (((hydra_annotations_set_term_annotation "key") (list :just (list :literal (list :integer (list :int32 999))))) (((hydra_annotations_set_term_annotation "key") (list :just (list :literal (list :integer (list :int32 1))))) (list :literal (list :boolean #f)))))))

(define (test-arbitrary-annotations-negunset-single-annotation--num1)

  (assert (equal? (list :literal (list :integer (list :int64 137))) (((hydra_annotations_set_term_annotation "k1") (list :nothing)) (((hydra_annotations_set_term_annotation "k1") (list :just (list :literal (list :string "foo")))) (list :literal (list :integer (list :int64 137))))))))

(define (test-arbitrary-annotations-negunset-single-annotation--num2)

  (assert (equal? (list :literal (list :string "test")) (((hydra_annotations_set_term_annotation "x") (list :nothing)) (((hydra_annotations_set_term_annotation "x") (list :just (list :literal (list :integer (list :int32 42))))) (list :literal (list :string "test")))))))

(define (test-arbitrary-annotations-negunset-one-of-multiple-annotations--num1)

  (assert (equal? (list :annotated (->hydra_core_annotated_term (list :literal (list :integer (list :int64 137))) (list (list "k2" (list :literal (list :integer (list :int32 200))))))) (((hydra_annotations_set_term_annotation "k1") (list :nothing)) (((hydra_annotations_set_term_annotation "k2") (list :just (list :literal (list :integer (list :int32 200))))) (((hydra_annotations_set_term_annotation "k1") (list :just (list :literal (list :string "first")))) (list :literal (list :integer (list :int64 137)))))))))

(define (test-arbitrary-annotations-negunset-one-of-multiple-annotations--num2)

  (assert (equal? (list :annotated (->hydra_core_annotated_term (list :literal (list :string "x")) (list (list "a" (list :literal (list :integer (list :int32 1))))))) (((hydra_annotations_set_term_annotation "b") (list :nothing)) (((hydra_annotations_set_term_annotation "b") (list :just (list :literal (list :integer (list :int32 2))))) (((hydra_annotations_set_term_annotation "a") (list :just (list :literal (list :integer (list :int32 1))))) (list :literal (list :string "x"))))))))

;; descriptions

(define (test-descriptions-negset-description--num1)

  (assert (equal? (list :annotated (->hydra_core_annotated_term (list :literal (list :string "foo")) (list (list "description" (list :literal (list :string "my description")))))) ((hydra_annotations_set_term_description (list :just "my description")) (list :literal (list :string "foo"))))))

(define (test-descriptions-negset-description--num2)

  (assert (equal? (list :annotated (->hydra_core_annotated_term (list :literal (list :integer (list :int32 42))) (list (list "description" (list :literal (list :string "")))))) ((hydra_annotations_set_term_description (list :just "")) (list :literal (list :integer (list :int32 42)))))))

(define (test-descriptions-negset-description--num3)

  (assert (equal? (list :annotated (->hydra_core_annotated_term (list :literal (list :boolean #t)) (list (list "description" (list :literal (list :string "A longer description with spaces")))))) ((hydra_annotations_set_term_description (list :just "A longer description with spaces")) (list :literal (list :boolean #t))))))

(define (test-descriptions-negget-existing-description--num1)

  (assert (equal? (list :right (list :just "hello")) (((hydra_annotations_get_term_description hydra_lexical_empty_context) hydra_lexical_empty_graph) ((hydra_annotations_set_term_description (list :just "hello")) (list :literal (list :integer (list :int32 42))))))))

(define (test-descriptions-negget-existing-description--num2)

  (assert (equal? (list :right (list :just "")) (((hydra_annotations_get_term_description hydra_lexical_empty_context) hydra_lexical_empty_graph) ((hydra_annotations_set_term_description (list :just "")) (list :literal (list :string "test")))))))

(define (test-descriptions-negget-existing-description--num3)

  (assert (equal? (list :right (list :just "desc")) (((hydra_annotations_get_term_description hydra_lexical_empty_context) hydra_lexical_empty_graph) ((hydra_annotations_set_term_description (list :just "desc")) (list :literal (list :boolean #f)))))))

(define (test-descriptions-negget-missing-description--num1)

  (assert (equal? (list :right (list :nothing)) (((hydra_annotations_get_term_description hydra_lexical_empty_context) hydra_lexical_empty_graph) (list :literal (list :integer (list :int16 42)))))))

(define (test-descriptions-negget-missing-description--num2)

  (assert (equal? (list :right (list :nothing)) (((hydra_annotations_get_term_description hydra_lexical_empty_context) hydra_lexical_empty_graph) (list :literal (list :string "no description here"))))))

(define (test-descriptions-negget-missing-description--num3)

  (assert (equal? (list :right (list :nothing)) (((hydra_annotations_get_term_description hydra_lexical_empty_context) hydra_lexical_empty_graph) (list :literal (list :integer (list :int32 0)))))))

(define (test-descriptions-negouter-description-overrides-inner--num1)

  (assert (equal? (list :annotated (->hydra_core_annotated_term (list :literal (list :string "bar")) (list (list "description" (list :literal (list :string "outer")))))) ((hydra_annotations_set_term_description (list :just "outer")) ((hydra_annotations_set_term_description (list :just "inner")) (list :literal (list :string "bar")))))))

(define (test-descriptions-negouter-description-overrides-inner--num2)

  (assert (equal? (list :annotated (->hydra_core_annotated_term (list :literal (list :integer (list :int32 99))) (list (list "description" (list :literal (list :string "new")))))) ((hydra_annotations_set_term_description (list :just "new")) ((hydra_annotations_set_term_description (list :just "old")) (list :literal (list :integer (list :int32 99))))))))

(define (test-descriptions-negunset-description--num1)

  (assert (equal? (list :literal (list :integer (list :int64 137))) ((hydra_annotations_set_term_description (list :nothing)) ((hydra_annotations_set_term_description (list :just "desc")) (list :literal (list :integer (list :int64 137))))))))

(define (test-descriptions-negunset-description--num2)

  (assert (equal? (list :literal (list :string "test")) ((hydra_annotations_set_term_description (list :nothing)) ((hydra_annotations_set_term_description (list :just "to be removed")) (list :literal (list :string "test")))))))

;; layered annotations

(define (test-layered-annotations-negget-annotation-from-unannotated-term)

  (assert (equal? (list :nothing) ((hydra_annotations_get_term_annotation "one") (list :literal (list :integer (list :int32 42)))))))

(define (test-layered-annotations-negget-annotation-from-singly-annotated-term)

  (assert (equal? (list :just (list :literal (list :integer (list :int32 1)))) ((hydra_annotations_get_term_annotation "one") (list :annotated (->hydra_core_annotated_term (list :literal (list :integer (list :int32 42))) (list (list "one" (list :literal (list :integer (list :int32 1)))))))))))

(define (test-layered-annotations-negget-inner-annotation-from-doubly-annotated-term)

  (assert (equal? (list :just (list :literal (list :integer (list :int32 1)))) ((hydra_annotations_get_term_annotation "one") (list :annotated (->hydra_core_annotated_term (list :annotated (->hydra_core_annotated_term (list :literal (list :integer (list :int32 42))) (list (list "one" (list :literal (list :integer (list :int32 1))))))) (list (list "two" (list :literal (list :integer (list :int32 2)))))))))))

(define (test-layered-annotations-negget-outer-annotation-from-doubly-annotated-term)

  (assert (equal? (list :just (list :literal (list :integer (list :int32 2)))) ((hydra_annotations_get_term_annotation "two") (list :annotated (->hydra_core_annotated_term (list :annotated (->hydra_core_annotated_term (list :literal (list :integer (list :int32 42))) (list (list "one" (list :literal (list :integer (list :int32 1))))))) (list (list "two" (list :literal (list :integer (list :int32 2)))))))))))

(define (test-layered-annotations-negget-non-negoverridden-annotation-from-triply-annotated-term)

  (assert (equal? (list :just (list :literal (list :integer (list :int32 2)))) ((hydra_annotations_get_term_annotation "two") (list :annotated (->hydra_core_annotated_term (list :annotated (->hydra_core_annotated_term (list :annotated (->hydra_core_annotated_term (list :literal (list :integer (list :int32 42))) (list (list "one" (list :literal (list :integer (list :int32 1))))))) (list (list "two" (list :literal (list :integer (list :int32 2))))))) (list (list "one" (list :literal (list :integer (list :int32 99)))))))))))

(define (test-layered-annotations-negouter-annotation-overrides-inner-in-layered-term)

  (assert (equal? (list :just (list :literal (list :integer (list :int32 99)))) ((hydra_annotations_get_term_annotation "one") (list :annotated (->hydra_core_annotated_term (list :annotated (->hydra_core_annotated_term (list :annotated (->hydra_core_annotated_term (list :literal (list :integer (list :int32 42))) (list (list "one" (list :literal (list :integer (list :int32 1))))))) (list (list "two" (list :literal (list :integer (list :int32 2))))))) (list (list "one" (list :literal (list :integer (list :int32 99)))))))))))
