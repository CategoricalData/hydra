;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; formatting

(require 'ert)

;; case conversion

(ert-deftest test-formatting-negcase-conversion-neg-num1-lower-snake-case--neg-upper-snake-case ()

  (should (equal "A_HELLO_WORLD_42_A42_42A_B" (funcall (funcall (funcall hydra_formatting_convert_case (list :lower_snake nil)) (list :upper_snake nil)) "a_hello_world_42_a42_42a_b"))))

(ert-deftest test-formatting-negcase-conversion-neg-num2-lower-snake-case--neg-camelcase ()

  (should (equal "aHelloWorld42A4242aB" (funcall (funcall (funcall hydra_formatting_convert_case (list :lower_snake nil)) (list :camel nil)) "a_hello_world_42_a42_42a_b"))))

(ert-deftest test-formatting-negcase-conversion-neg-num3-lower-snake-case--neg-pascalcase ()

  (should (equal "AHelloWorld42A4242aB" (funcall (funcall (funcall hydra_formatting_convert_case (list :lower_snake nil)) (list :pascal nil)) "a_hello_world_42_a42_42a_b"))))

(ert-deftest test-formatting-negcase-conversion-neg-num4-lower-snake-case--neg-lower-snake-case ()

  (should (equal "a_hello_world_42_a42_42a_b" (funcall (funcall (funcall hydra_formatting_convert_case (list :lower_snake nil)) (list :lower_snake nil)) "a_hello_world_42_a42_42a_b"))))

(ert-deftest test-formatting-negcase-conversion-neg-num5-upper-snake-case--neg-lower-snake-case ()

  (should (equal "a_hello_world_42_a42_42a_b" (funcall (funcall (funcall hydra_formatting_convert_case (list :upper_snake nil)) (list :lower_snake nil)) "A_HELLO_WORLD_42_A42_42A_B"))))

(ert-deftest test-formatting-negcase-conversion-neg-num6-upper-snake-case--neg-camelcase ()

  (should (equal "aHelloWorld42A4242aB" (funcall (funcall (funcall hydra_formatting_convert_case (list :upper_snake nil)) (list :camel nil)) "A_HELLO_WORLD_42_A42_42A_B"))))

(ert-deftest test-formatting-negcase-conversion-neg-num7-upper-snake-case--neg-pascalcase ()

  (should (equal "AHelloWorld42A4242aB" (funcall (funcall (funcall hydra_formatting_convert_case (list :upper_snake nil)) (list :pascal nil)) "A_HELLO_WORLD_42_A42_42A_B"))))

(ert-deftest test-formatting-negcase-conversion-neg-num8-upper-snake-case--neg-upper-snake-case ()

  (should (equal "A_HELLO_WORLD_42_A42_42A_B" (funcall (funcall (funcall hydra_formatting_convert_case (list :upper_snake nil)) (list :upper_snake nil)) "A_HELLO_WORLD_42_A42_42A_B"))))

(ert-deftest test-formatting-negcase-conversion-neg-num9-camelcase--neg-lower-snake-case ()

  (should (equal "a_hello_world42_a4242a_b" (funcall (funcall (funcall hydra_formatting_convert_case (list :camel nil)) (list :lower_snake nil)) "aHelloWorld42A4242aB"))))

(ert-deftest test-formatting-negcase-conversion-neg-num10-camelcase--neg-upper-snake-case ()

  (should (equal "A_HELLO_WORLD42_A4242A_B" (funcall (funcall (funcall hydra_formatting_convert_case (list :camel nil)) (list :upper_snake nil)) "aHelloWorld42A4242aB"))))

(ert-deftest test-formatting-negcase-conversion-neg-num11-camelcase--neg-pascalcase ()

  (should (equal "AHelloWorld42A4242aB" (funcall (funcall (funcall hydra_formatting_convert_case (list :camel nil)) (list :pascal nil)) "aHelloWorld42A4242aB"))))

(ert-deftest test-formatting-negcase-conversion-neg-num12-camelcase--neg-camelcase ()

  (should (equal "aHelloWorld42A4242aB" (funcall (funcall (funcall hydra_formatting_convert_case (list :camel nil)) (list :camel nil)) "aHelloWorld42A4242aB"))))

(ert-deftest test-formatting-negcase-conversion-neg-num13-pascalcase--neg-lower-snake-case ()

  (should (equal "a_hello_world42_a4242a_b" (funcall (funcall (funcall hydra_formatting_convert_case (list :pascal nil)) (list :lower_snake nil)) "AHelloWorld42A4242aB"))))

(ert-deftest test-formatting-negcase-conversion-neg-num14-pascalcase--neg-upper-snake-case ()

  (should (equal "A_HELLO_WORLD42_A4242A_B" (funcall (funcall (funcall hydra_formatting_convert_case (list :pascal nil)) (list :upper_snake nil)) "AHelloWorld42A4242aB"))))

(ert-deftest test-formatting-negcase-conversion-neg-num15-pascalcase--neg-camelcase ()

  (should (equal "aHelloWorld42A4242aB" (funcall (funcall (funcall hydra_formatting_convert_case (list :pascal nil)) (list :camel nil)) "AHelloWorld42A4242aB"))))

(ert-deftest test-formatting-negcase-conversion-neg-num16-pascalcase--neg-pascalcase ()

  (should (equal "AHelloWorld42A4242aB" (funcall (funcall (funcall hydra_formatting_convert_case (list :pascal nil)) (list :pascal nil)) "AHelloWorld42A4242aB"))))
