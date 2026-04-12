;;; chars.el --- Hydra character primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Hydra represents characters as int32 codepoints.
;; These primitives convert to/from Emacs characters internally.

;; is_alpha_num :: Int32 -> Bool
(defvar hydra_lib_chars_is_alpha_num
  (lambda (c)
    "Check whether a character is alphanumeric."
    (let ((ch c))
      (and (or (and (>= ch ?a) (<= ch ?z))
               (and (>= ch ?A) (<= ch ?Z))
               (and (>= ch ?0) (<= ch ?9)))
           t))))

;; is_lower :: Int32 -> Bool
(defvar hydra_lib_chars_is_lower
  (lambda (c)
    "Check whether a character is lowercase."
    (and (>= c ?a) (<= c ?z))))

;; is_space :: Int32 -> Bool
(defvar hydra_lib_chars_is_space
  (lambda (c)
    "Check whether a character is a whitespace character."
    (and (or (= c ?\s)
             (= c ?\t)
             (= c ?\n)
             (= c ?\r)
             (= c ?\f))
         t)))

;; is_upper :: Int32 -> Bool
(defvar hydra_lib_chars_is_upper
  (lambda (c)
    "Check whether a character is uppercase."
    (and (>= c ?A) (<= c ?Z))))

;; to_lower :: Int32 -> Int32
(defvar hydra_lib_chars_to_lower
  (lambda (c)
    "Convert a character to lowercase."
    (downcase c)))

;; to_upper :: Int32 -> Int32
(defvar hydra_lib_chars_to_upper
  (lambda (c)
    "Convert a character to uppercase."
    (upcase c)))

(provide 'hydra.lib.chars)
