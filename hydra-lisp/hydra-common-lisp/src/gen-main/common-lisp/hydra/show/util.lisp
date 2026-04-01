(defpackage :hydra.show.util
(:use :cl :hydra.util)
(:export :hydra_show_util_case_convention :hydra_show_util_comparison))

(in-package :hydra.show.util)

(cl:defvar hydra_show_util_case_convention (cl:lambda (c) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :lower_snake) ((cl:lambda (_) "lower_snake_case") match_value)) ((equal (car match_target) :upper_snake) ((cl:lambda (_) "UPPER_SNAKE_CASE") match_value)) ((equal (car match_target) :camel) ((cl:lambda (_) "camelCase") match_value)) ((equal (car match_target) :pascal) ((cl:lambda (_) "PascalCase") match_value)))) (cadr match_target))) c)))

(cl:defvar hydra_show_util_comparison (cl:lambda (c) ((cl:lambda (match_target) ((cl:lambda (match_value) (cond ((equal (car match_target) :less_than) ((cl:lambda (_) "lessThan") match_value)) ((equal (car match_target) :equal_to) ((cl:lambda (_) "equalTo") match_value)) ((equal (car match_target) :greater_than) ((cl:lambda (_) "greaterThan") match_value)))) (cadr match_target))) c)))
