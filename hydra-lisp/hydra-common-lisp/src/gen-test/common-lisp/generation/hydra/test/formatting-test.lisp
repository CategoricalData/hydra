;; Note: this is an automatically generated file. Do not edit.
;; formatting

;; case conversion

(defun test-formatting-negcase-conversion-neg-num1-lower-snake-case--neg-upper-snake-case ()

  (assert (equal "A_HELLO_WORLD_42_A42_42A_B" (((hydra_formatting_convert_case (list :lower_snake cl:nil)) (list :upper_snake cl:nil)) "a_hello_world_42_a42_42a_b"))))

(defun test-formatting-negcase-conversion-neg-num2-lower-snake-case--neg-camelcase ()

  (assert (equal "aHelloWorld42A4242aB" (((hydra_formatting_convert_case (list :lower_snake cl:nil)) (list :camel cl:nil)) "a_hello_world_42_a42_42a_b"))))

(defun test-formatting-negcase-conversion-neg-num3-lower-snake-case--neg-pascalcase ()

  (assert (equal "AHelloWorld42A4242aB" (((hydra_formatting_convert_case (list :lower_snake cl:nil)) (list :pascal cl:nil)) "a_hello_world_42_a42_42a_b"))))

(defun test-formatting-negcase-conversion-neg-num4-lower-snake-case--neg-lower-snake-case ()

  (assert (equal "a_hello_world_42_a42_42a_b" (((hydra_formatting_convert_case (list :lower_snake cl:nil)) (list :lower_snake cl:nil)) "a_hello_world_42_a42_42a_b"))))

(defun test-formatting-negcase-conversion-neg-num5-upper-snake-case--neg-lower-snake-case ()

  (assert (equal "a_hello_world_42_a42_42a_b" (((hydra_formatting_convert_case (list :upper_snake cl:nil)) (list :lower_snake cl:nil)) "A_HELLO_WORLD_42_A42_42A_B"))))

(defun test-formatting-negcase-conversion-neg-num6-upper-snake-case--neg-camelcase ()

  (assert (equal "aHelloWorld42A4242aB" (((hydra_formatting_convert_case (list :upper_snake cl:nil)) (list :camel cl:nil)) "A_HELLO_WORLD_42_A42_42A_B"))))

(defun test-formatting-negcase-conversion-neg-num7-upper-snake-case--neg-pascalcase ()

  (assert (equal "AHelloWorld42A4242aB" (((hydra_formatting_convert_case (list :upper_snake cl:nil)) (list :pascal cl:nil)) "A_HELLO_WORLD_42_A42_42A_B"))))

(defun test-formatting-negcase-conversion-neg-num8-upper-snake-case--neg-upper-snake-case ()

  (assert (equal "A_HELLO_WORLD_42_A42_42A_B" (((hydra_formatting_convert_case (list :upper_snake cl:nil)) (list :upper_snake cl:nil)) "A_HELLO_WORLD_42_A42_42A_B"))))

(defun test-formatting-negcase-conversion-neg-num9-camelcase--neg-lower-snake-case ()

  (assert (equal "a_hello_world42_a4242a_b" (((hydra_formatting_convert_case (list :camel cl:nil)) (list :lower_snake cl:nil)) "aHelloWorld42A4242aB"))))

(defun test-formatting-negcase-conversion-neg-num10-camelcase--neg-upper-snake-case ()

  (assert (equal "A_HELLO_WORLD42_A4242A_B" (((hydra_formatting_convert_case (list :camel cl:nil)) (list :upper_snake cl:nil)) "aHelloWorld42A4242aB"))))

(defun test-formatting-negcase-conversion-neg-num11-camelcase--neg-pascalcase ()

  (assert (equal "AHelloWorld42A4242aB" (((hydra_formatting_convert_case (list :camel cl:nil)) (list :pascal cl:nil)) "aHelloWorld42A4242aB"))))

(defun test-formatting-negcase-conversion-neg-num12-camelcase--neg-camelcase ()

  (assert (equal "aHelloWorld42A4242aB" (((hydra_formatting_convert_case (list :camel cl:nil)) (list :camel cl:nil)) "aHelloWorld42A4242aB"))))

(defun test-formatting-negcase-conversion-neg-num13-pascalcase--neg-lower-snake-case ()

  (assert (equal "a_hello_world42_a4242a_b" (((hydra_formatting_convert_case (list :pascal cl:nil)) (list :lower_snake cl:nil)) "AHelloWorld42A4242aB"))))

(defun test-formatting-negcase-conversion-neg-num14-pascalcase--neg-upper-snake-case ()

  (assert (equal "A_HELLO_WORLD42_A4242A_B" (((hydra_formatting_convert_case (list :pascal cl:nil)) (list :upper_snake cl:nil)) "AHelloWorld42A4242aB"))))

(defun test-formatting-negcase-conversion-neg-num15-pascalcase--neg-camelcase ()

  (assert (equal "aHelloWorld42A4242aB" (((hydra_formatting_convert_case (list :pascal cl:nil)) (list :camel cl:nil)) "AHelloWorld42A4242aB"))))

(defun test-formatting-negcase-conversion-neg-num16-pascalcase--neg-pascalcase ()

  (assert (equal "AHelloWorld42A4242aB" (((hydra_formatting_convert_case (list :pascal cl:nil)) (list :pascal cl:nil)) "AHelloWorld42A4242aB"))))
