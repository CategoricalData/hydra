(require 'cl-lib)

(require 'hydra.util)

(defvar hydra_show_util_case_convention (lambda (c) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :lower_snake) (funcall (lambda (_) "lower_snake_case") match_value)) ((equal (car match_target) :upper_snake) (funcall (lambda (_) "UPPER_SNAKE_CASE") match_value)) ((equal (car match_target) :camel) (funcall (lambda (_) "camelCase") match_value)) ((equal (car match_target) :pascal) (funcall (lambda (_) "PascalCase") match_value)))) (cadr match_target))) c)))

(defvar hydra_show_util_comparison (lambda (c) (funcall (lambda (match_target) (funcall (lambda (match_value) (cond ((equal (car match_target) :less_than) (funcall (lambda (_) "lessThan") match_value)) ((equal (car match_target) :equal_to) (funcall (lambda (_) "equalTo") match_value)) ((equal (car match_target) :greater_than) (funcall (lambda (_) "greaterThan") match_value)))) (cadr match_target))) c)))

(provide 'hydra.show.util)
