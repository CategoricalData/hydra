(define-library (hydra show util)
(export hydra_show_util_case_convention)
(import (scheme base) (hydra util))
(begin
(define hydra_show_util_case_convention (lambda (c) ((lambda (match_target) ((lambda (match_value) (cond ((equal? (car match_target) 'lower_snake) ((lambda (_) "lower_snake_case") match_value)) ((equal? (car match_target) 'upper_snake) ((lambda (_) "UPPER_SNAKE_CASE") match_value)) ((equal? (car match_target) 'camel) ((lambda (_) "camelCase") match_value)) ((equal? (car match_target) 'pascal) ((lambda (_) "PascalCase") match_value)))) (cadr match_target))) c)))))
