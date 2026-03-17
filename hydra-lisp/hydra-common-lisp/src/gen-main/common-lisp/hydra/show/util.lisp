(defpackage :hydra.show.util
(:use :cl :hydra.util)
(:export :hydra_show_util_case_convention))

(in-package :hydra.show.util)

(cl:defvar hydra_show_util_case_convention (cl:lambda (c) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :lower_snake) ((cl:lambda (_) "lower_snake_case") match_value)) ((equal (car match_target) :upper_snake) ((cl:lambda (_) "UPPER_SNAKE_CASE") match_value)) ((equal (car match_target) :camel) ((cl:lambda (_) "camelCase") match_value)) ((equal (car match_target) :pascal) ((cl:lambda (_) "PascalCase") match_value)))) (cadr match_target))) c)))
