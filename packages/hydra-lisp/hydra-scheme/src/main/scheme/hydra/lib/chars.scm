(define-library (hydra lib chars)
  (import (scheme base)
          (scheme char))
  (export hydra_lib_chars_is_alpha_num
          hydra_lib_chars_is_lower
          hydra_lib_chars_is_space
          hydra_lib_chars_is_upper
          hydra_lib_chars_to_lower
          hydra_lib_chars_to_upper)
  (begin

    ;; Hydra represents characters as integer code points (int32).
    ;; Convert to Scheme char for char operations, and back to int for output.
    (define (int->char c) (if (char? c) c (integer->char c)))
    (define (char->int c) (if (char? c) (char->integer c) c))

    ;; Check whether a character is alphanumeric.
    (define hydra_lib_chars_is_alpha_num
      (lambda (c)
        (let ((ch (int->char c)))
          (or (char-alphabetic? ch)
              (char-numeric? ch)))))

    ;; Check whether a character is lowercase.
    (define hydra_lib_chars_is_lower
      (lambda (c)
        (char-lower-case? (int->char c))))

    ;; Check whether a character is a whitespace character.
    (define hydra_lib_chars_is_space
      (lambda (c)
        (char-whitespace? (int->char c))))

    ;; Check whether a character is uppercase.
    (define hydra_lib_chars_is_upper
      (lambda (c)
        (char-upper-case? (int->char c))))

    ;; Convert a character to lowercase.
    (define hydra_lib_chars_to_lower
      (lambda (c)
        (char->int (char-downcase (int->char c)))))

    ;; Convert a character to uppercase.
    (define hydra_lib_chars_to_upper
      (lambda (c)
        (char->int (char-upcase (int->char c)))))))
