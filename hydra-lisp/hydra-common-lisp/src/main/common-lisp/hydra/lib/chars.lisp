(in-package :cl-user)

;; Hydra represents characters as int32 codepoints.
;; These primitives convert to/from CL characters internally.

;; is_alpha_num :: Int32 -> Bool
;; Check whether a character is alphanumeric.
(defvar hydra_lib_chars_is_alpha_num
  (lambda (c)
    (let ((ch (code-char c)))
      (and ch (or (alpha-char-p ch) (digit-char-p ch))
           t))))

;; is_lower :: Int32 -> Bool
;; Check whether a character is lowercase.
(defvar hydra_lib_chars_is_lower
  (lambda (c)
    (let ((ch (code-char c)))
      (and ch (lower-case-p ch) t))))

;; is_space :: Int32 -> Bool
;; Check whether a character is a whitespace character.
(defvar hydra_lib_chars_is_space
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
(defvar hydra_lib_chars_is_upper
  (lambda (c)
    (let ((ch (code-char c)))
      (and ch (upper-case-p ch) t))))

;; to_lower :: Int32 -> Int32
;; Convert a character to lowercase.
(defvar hydra_lib_chars_to_lower
  (lambda (c)
    (char-code (char-downcase (code-char c)))))

;; to_upper :: Int32 -> Int32
;; Convert a character to uppercase.
(defvar hydra_lib_chars_to_upper
  (lambda (c)
    (char-code (char-upcase (code-char c)))))
