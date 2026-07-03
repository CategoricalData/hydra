;;; hashing.el --- Hydra Emacs Lisp hashing primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Emacs Lisp implementations of hydra.lib.hashing primitives (#524).
;;
;; SHA-256 over raw bytes (binary). These pair with hydra.lib.files.readFile, which is
;; byte-oriented, to hash file contents. Pure and total.
;;
;; Runtime representations (Emacs Lisp target):
;;   binary/bytes  : a unibyte string (sequence of raw bytes), as produced by encode-coding-string.
;;
;; Emacs provides SHA-256 natively via `secure-hash': with BINARY non-nil it returns the raw
;; 32-byte digest as a unibyte string; otherwise it returns the lowercase hex string.

;; ---- Primitives ----

;; sha256 :: binary -> binary
(defvar hydra_overlay_emacs_lisp_lib_hashing_sha256
  (lambda (data)
    (secure-hash 'sha256 (string-as-unibyte (concat data)) nil nil t)))

;; sha256Hex :: binary -> string
(defvar hydra_overlay_emacs_lisp_lib_hashing_sha256_hex
  (lambda (data)
    (secure-hash 'sha256 (string-as-unibyte (concat data)))))

(provide 'hydra.lib.hashing)

;;; hashing.el ends here
