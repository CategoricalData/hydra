;; Note: this is an automatically generated file. Do not edit.
;; formatting

(import (scheme base))

;; case conversion

(define (test-case-conversion-neg-num1-lower-snake-case--neg-upper-snake-case)

  (assert (equal? "A_HELLO_WORLD_42_A42_42A_B" (((hydra_formatting_convert_case (list :lower_snake '())) (list :upper_snake '())) "a_hello_world_42_a42_42a_b"))))

(define (test-case-conversion-neg-num2-lower-snake-case--neg-camelcase)

  (assert (equal? "aHelloWorld42A4242aB" (((hydra_formatting_convert_case (list :lower_snake '())) (list :camel '())) "a_hello_world_42_a42_42a_b"))))

(define (test-case-conversion-neg-num3-lower-snake-case--neg-pascalcase)

  (assert (equal? "AHelloWorld42A4242aB" (((hydra_formatting_convert_case (list :lower_snake '())) (list :pascal '())) "a_hello_world_42_a42_42a_b"))))

(define (test-case-conversion-neg-num4-lower-snake-case--neg-lower-snake-case)

  (assert (equal? "a_hello_world_42_a42_42a_b" (((hydra_formatting_convert_case (list :lower_snake '())) (list :lower_snake '())) "a_hello_world_42_a42_42a_b"))))

(define (test-case-conversion-neg-num5-upper-snake-case--neg-lower-snake-case)

  (assert (equal? "a_hello_world_42_a42_42a_b" (((hydra_formatting_convert_case (list :upper_snake '())) (list :lower_snake '())) "A_HELLO_WORLD_42_A42_42A_B"))))

(define (test-case-conversion-neg-num6-upper-snake-case--neg-camelcase)

  (assert (equal? "aHelloWorld42A4242aB" (((hydra_formatting_convert_case (list :upper_snake '())) (list :camel '())) "A_HELLO_WORLD_42_A42_42A_B"))))

(define (test-case-conversion-neg-num7-upper-snake-case--neg-pascalcase)

  (assert (equal? "AHelloWorld42A4242aB" (((hydra_formatting_convert_case (list :upper_snake '())) (list :pascal '())) "A_HELLO_WORLD_42_A42_42A_B"))))

(define (test-case-conversion-neg-num8-upper-snake-case--neg-upper-snake-case)

  (assert (equal? "A_HELLO_WORLD_42_A42_42A_B" (((hydra_formatting_convert_case (list :upper_snake '())) (list :upper_snake '())) "A_HELLO_WORLD_42_A42_42A_B"))))

(define (test-case-conversion-neg-num9-camelcase--neg-lower-snake-case)

  (assert (equal? "a_hello_world42_a4242a_b" (((hydra_formatting_convert_case (list :camel '())) (list :lower_snake '())) "aHelloWorld42A4242aB"))))

(define (test-case-conversion-neg-num10-camelcase--neg-upper-snake-case)

  (assert (equal? "A_HELLO_WORLD42_A4242A_B" (((hydra_formatting_convert_case (list :camel '())) (list :upper_snake '())) "aHelloWorld42A4242aB"))))

(define (test-case-conversion-neg-num11-camelcase--neg-pascalcase)

  (assert (equal? "AHelloWorld42A4242aB" (((hydra_formatting_convert_case (list :camel '())) (list :pascal '())) "aHelloWorld42A4242aB"))))

(define (test-case-conversion-neg-num12-camelcase--neg-camelcase)

  (assert (equal? "aHelloWorld42A4242aB" (((hydra_formatting_convert_case (list :camel '())) (list :camel '())) "aHelloWorld42A4242aB"))))

(define (test-case-conversion-neg-num13-pascalcase--neg-lower-snake-case)

  (assert (equal? "a_hello_world42_a4242a_b" (((hydra_formatting_convert_case (list :pascal '())) (list :lower_snake '())) "AHelloWorld42A4242aB"))))

(define (test-case-conversion-neg-num14-pascalcase--neg-upper-snake-case)

  (assert (equal? "A_HELLO_WORLD42_A4242A_B" (((hydra_formatting_convert_case (list :pascal '())) (list :upper_snake '())) "AHelloWorld42A4242aB"))))

(define (test-case-conversion-neg-num15-pascalcase--neg-camelcase)

  (assert (equal? "aHelloWorld42A4242aB" (((hydra_formatting_convert_case (list :pascal '())) (list :camel '())) "AHelloWorld42A4242aB"))))

(define (test-case-conversion-neg-num16-pascalcase--neg-pascalcase)

  (assert (equal? "AHelloWorld42A4242aB" (((hydra_formatting_convert_case (list :pascal '())) (list :pascal '())) "AHelloWorld42A4242aB"))))
