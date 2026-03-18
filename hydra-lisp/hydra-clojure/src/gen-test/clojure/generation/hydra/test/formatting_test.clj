;; Note: this is an automatically generated file. Do not edit.
;; formatting

(ns generation.hydra.test.formatting-test
  (:require [clojure.test :refer :all]))

;; case conversion

(deftest test-case-conversion-neg-num1-lower-snake-case--neg-upper-snake-case

  (is (= "A_HELLO_WORLD_42_A42_42A_B"

         (((hydra_formatting_convert_case (list :lower_snake nil)) (list :upper_snake nil)) "a_hello_world_42_a42_42a_b"))))

(deftest test-case-conversion-neg-num2-lower-snake-case--neg-camelcase

  (is (= "aHelloWorld42A4242aB"

         (((hydra_formatting_convert_case (list :lower_snake nil)) (list :camel nil)) "a_hello_world_42_a42_42a_b"))))

(deftest test-case-conversion-neg-num3-lower-snake-case--neg-pascalcase

  (is (= "AHelloWorld42A4242aB"

         (((hydra_formatting_convert_case (list :lower_snake nil)) (list :pascal nil)) "a_hello_world_42_a42_42a_b"))))

(deftest test-case-conversion-neg-num4-lower-snake-case--neg-lower-snake-case

  (is (= "a_hello_world_42_a42_42a_b"

         (((hydra_formatting_convert_case (list :lower_snake nil)) (list :lower_snake nil)) "a_hello_world_42_a42_42a_b"))))

(deftest test-case-conversion-neg-num5-upper-snake-case--neg-lower-snake-case

  (is (= "a_hello_world_42_a42_42a_b"

         (((hydra_formatting_convert_case (list :upper_snake nil)) (list :lower_snake nil)) "A_HELLO_WORLD_42_A42_42A_B"))))

(deftest test-case-conversion-neg-num6-upper-snake-case--neg-camelcase

  (is (= "aHelloWorld42A4242aB"

         (((hydra_formatting_convert_case (list :upper_snake nil)) (list :camel nil)) "A_HELLO_WORLD_42_A42_42A_B"))))

(deftest test-case-conversion-neg-num7-upper-snake-case--neg-pascalcase

  (is (= "AHelloWorld42A4242aB"

         (((hydra_formatting_convert_case (list :upper_snake nil)) (list :pascal nil)) "A_HELLO_WORLD_42_A42_42A_B"))))

(deftest test-case-conversion-neg-num8-upper-snake-case--neg-upper-snake-case

  (is (= "A_HELLO_WORLD_42_A42_42A_B"

         (((hydra_formatting_convert_case (list :upper_snake nil)) (list :upper_snake nil)) "A_HELLO_WORLD_42_A42_42A_B"))))

(deftest test-case-conversion-neg-num9-camelcase--neg-lower-snake-case

  (is (= "a_hello_world42_a4242a_b"

         (((hydra_formatting_convert_case (list :camel nil)) (list :lower_snake nil)) "aHelloWorld42A4242aB"))))

(deftest test-case-conversion-neg-num10-camelcase--neg-upper-snake-case

  (is (= "A_HELLO_WORLD42_A4242A_B"

         (((hydra_formatting_convert_case (list :camel nil)) (list :upper_snake nil)) "aHelloWorld42A4242aB"))))

(deftest test-case-conversion-neg-num11-camelcase--neg-pascalcase

  (is (= "AHelloWorld42A4242aB"

         (((hydra_formatting_convert_case (list :camel nil)) (list :pascal nil)) "aHelloWorld42A4242aB"))))

(deftest test-case-conversion-neg-num12-camelcase--neg-camelcase

  (is (= "aHelloWorld42A4242aB"

         (((hydra_formatting_convert_case (list :camel nil)) (list :camel nil)) "aHelloWorld42A4242aB"))))

(deftest test-case-conversion-neg-num13-pascalcase--neg-lower-snake-case

  (is (= "a_hello_world42_a4242a_b"

         (((hydra_formatting_convert_case (list :pascal nil)) (list :lower_snake nil)) "AHelloWorld42A4242aB"))))

(deftest test-case-conversion-neg-num14-pascalcase--neg-upper-snake-case

  (is (= "A_HELLO_WORLD42_A4242A_B"

         (((hydra_formatting_convert_case (list :pascal nil)) (list :upper_snake nil)) "AHelloWorld42A4242aB"))))

(deftest test-case-conversion-neg-num15-pascalcase--neg-camelcase

  (is (= "aHelloWorld42A4242aB"

         (((hydra_formatting_convert_case (list :pascal nil)) (list :camel nil)) "AHelloWorld42A4242aB"))))

(deftest test-case-conversion-neg-num16-pascalcase--neg-pascalcase

  (is (= "AHelloWorld42A4242aB"

         (((hydra_formatting_convert_case (list :pascal nil)) (list :pascal nil)) "AHelloWorld42A4242aB"))))
