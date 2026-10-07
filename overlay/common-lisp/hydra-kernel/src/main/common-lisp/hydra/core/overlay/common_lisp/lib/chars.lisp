(in-package :cl-user)

;; Hydra represents characters as int32 codepoints.
;; These primitives convert to/from CL characters internally.

;; is_alpha_num :: Int32 -> Bool
;; Check whether a character is alphanumeric.
(defvar hydra_overlay_common_lisp_lib_chars_is_alpha_num
  (lambda (c)
    (let ((ch (code-char c)))
      (and ch (or (alpha-char-p ch) (digit-char-p ch))
           t))))

;; is_lower :: Int32 -> Bool
;; Check whether a character is lowercase.
(defvar hydra_overlay_common_lisp_lib_chars_is_lower
  (lambda (c)
    (let ((ch (code-char c)))
      (and ch (lower-case-p ch) t))))

;; is_space :: Int32 -> Bool
;; Check whether a character is a whitespace character.
(defvar hydra_overlay_common_lisp_lib_chars_is_space
  (lambda (c)
    (let ((ch (code-char c)))
      (and ch
           (or (char= ch #\Space)
               (char= ch #\Tab)
               (char= ch #\Newline)
               (char= ch #\Return)
               (char= ch #\Page))
           t))))

;; is_upper :: Int32 -> Bool
;; Check whether a character is uppercase.
(defvar hydra_overlay_common_lisp_lib_chars_is_upper
  (lambda (c)
    (let ((ch (code-char c)))
      (and ch (upper-case-p ch) t))))

;; SBCL's char-upcase/char-downcase mostly implement Unicode's SIMPLE case
;; mapping directly (a true one-to-one code-point mapping), but diverge from
;; it for the code points below -- either returning the input unchanged when
;; a real one-to-one mapping exists, or (for U+0149) mapping to a code point
;; other than the expected simple uppercase. Found by diffing against Java's
;; Character.toLowerCase/toUpperCase (which expose Unicode's simple mapping
;; directly) across every code point that changes case under #782's spec.
(defvar hydra_overlay_common_lisp_lib_chars_simple_lower_overrides
  (list (cons #x0130 #x0069))) ; LATIN CAPITAL LETTER I WITH DOT ABOVE -> i

(defvar hydra_overlay_common_lisp_lib_chars_simple_upper_overrides
  (list
    (cons #x0149 #x0149) ; LATIN SMALL LETTER N PRECEDED BY APOSTROPHE: no simple uppercase mapping.
    ;; Greek letters with iota subscript: SBCL's char-upcase leaves these
    ;; unchanged, but each has a real one-to-one simple uppercase mapping
    ;; (iota subscript -> capital iota, +8 code points).
    (cons #x1F80 #x1F88) (cons #x1F81 #x1F89) (cons #x1F82 #x1F8A) (cons #x1F83 #x1F8B)
    (cons #x1F84 #x1F8C) (cons #x1F85 #x1F8D) (cons #x1F86 #x1F8E) (cons #x1F87 #x1F8F)
    (cons #x1F90 #x1F98) (cons #x1F91 #x1F99) (cons #x1F92 #x1F9A) (cons #x1F93 #x1F9B)
    (cons #x1F94 #x1F9C) (cons #x1F95 #x1F9D) (cons #x1F96 #x1F9E) (cons #x1F97 #x1F9F)
    (cons #x1FA0 #x1FA8) (cons #x1FA1 #x1FA9) (cons #x1FA2 #x1FAA) (cons #x1FA3 #x1FAB)
    (cons #x1FA4 #x1FAC) (cons #x1FA5 #x1FAD) (cons #x1FA6 #x1FAE) (cons #x1FA7 #x1FAF)
    (cons #x1FB3 #x1FBC) (cons #x1FC3 #x1FCC) (cons #x1FF3 #x1FFC)))

;; to_lower :: Int32 -> Int32
;; Convert a character to lowercase.
(defvar hydra_overlay_common_lisp_lib_chars_to_lower
  (lambda (c)
    (let ((override (assoc c hydra_overlay_common_lisp_lib_chars_simple_lower_overrides)))
      (if override
          (cdr override)
          (char-code (char-downcase (code-char c)))))))

;; to_upper :: Int32 -> Int32
;; Convert a character to uppercase.
(defvar hydra_overlay_common_lisp_lib_chars_to_upper
  (lambda (c)
    (let ((override (assoc c hydra_overlay_common_lisp_lib_chars_simple_upper_overrides)))
      (if override
          (cdr override)
          (char-code (char-upcase (code-char c)))))))
