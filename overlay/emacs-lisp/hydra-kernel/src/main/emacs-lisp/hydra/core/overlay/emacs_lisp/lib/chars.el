;;; chars.el --- Hydra character primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Hydra represents characters as int32 codepoints.
;; These primitives convert to/from Emacs characters internally.

;; is_alpha_num :: Int32 -> Bool
(defvar hydra_overlay_emacs_lisp_lib_chars_is_alpha_num
  (lambda (c)
    "Check whether a character is alphanumeric."
    (let ((ch c))
      (and (or (and (>= ch ?a) (<= ch ?z))
               (and (>= ch ?A) (<= ch ?Z))
               (and (>= ch ?0) (<= ch ?9)))
           t))))

;; is_lower :: Int32 -> Bool
(defvar hydra_overlay_emacs_lisp_lib_chars_is_lower
  (lambda (c)
    "Check whether a character is lowercase."
    (and (>= c ?a) (<= c ?z))))

;; is_space :: Int32 -> Bool
(defvar hydra_overlay_emacs_lisp_lib_chars_is_space
  (lambda (c)
    "Check whether a character is a whitespace character."
    (and (or (= c ?\s)
             (= c ?\t)
             (= c ?\n)
             (= c ?\r)
             (= c ?\f))
         t)))

;; is_upper :: Int32 -> Bool
(defvar hydra_overlay_emacs_lisp_lib_chars_is_upper
  (lambda (c)
    "Check whether a character is uppercase."
    (and (>= c ?A) (<= c ?Z))))

;; Emacs's downcase/upcase mostly implement Unicode's SIMPLE case mapping
;; directly (a true one-to-one code-point mapping), but diverge from it for
;; the code points below. Found by diffing against Java's
;; Character.toLowerCase/toUpperCase (which expose Unicode's simple mapping
;; directly) across every code point that changes case under #782's spec.
(defvar hydra_overlay_emacs_lisp_lib_chars_simple_lower_overrides
  '((#x0130 . #x0069))) ; LATIN CAPITAL LETTER I WITH DOT ABOVE -> i (Emacs leaves it unchanged)

(defvar hydra_overlay_emacs_lisp_lib_chars_simple_upper_overrides
  ;; U+00DF LATIN SMALL LETTER SHARP S ("ß") has no single-code-point simple
  ;; uppercase mapping, but Emacs's upcase maps it to U+1E9E (capital sharp
  ;; S), which is its own distinct code point, not ß's simple mapping.
  '((#x00DF . #x00DF)))

;; to_lower :: Int32 -> Int32
(defvar hydra_overlay_emacs_lisp_lib_chars_to_lower
  (lambda (c)
    "Convert a character to lowercase."
    (let ((override (assoc c hydra_overlay_emacs_lisp_lib_chars_simple_lower_overrides)))
      (if override (cdr override) (downcase c)))))

;; to_upper :: Int32 -> Int32
(defvar hydra_overlay_emacs_lisp_lib_chars_to_upper
  (lambda (c)
    "Convert a character to uppercase."
    (let ((override (assoc c hydra_overlay_emacs_lisp_lib_chars_simple_upper_overrides)))
      (if override (cdr override) (upcase c)))))

(provide 'hydra.core.lib.chars)
