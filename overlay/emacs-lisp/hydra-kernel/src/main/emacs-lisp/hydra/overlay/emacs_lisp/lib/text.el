;;; text.el --- Hydra Emacs Lisp text primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Emacs Lisp implementations of hydra.lib.text primitives (#494).
;;
;; UTF-8 codec primitives bridging Hydra strings and raw bytes (binary). These pair with
;; hydra.lib.files.readFile / writeFile, which are byte-oriented.
;;
;; Runtime representations (Emacs Lisp target):
;;   Either        : (list :left v) | (list :right v)
;;   binary/bytes  : a unibyte string (sequence of raw bytes), as produced by encode-coding-string
;;                   and consumed by decode-coding-string.
;;
;; decode-coding-string with 'utf-8 is lenient: it never signals on malformed input, instead mapping
;; each undecodable raw byte to the reserved "eight-bit" code-point range #x3FFF00..#x3FFFFF (so the
;; round trip stays lossless). Strict decodeUtf8 therefore detects invalid input by scanning the
;; decoded text for any character in that range.

;; ---- Primitives ----

;; decodeUtf8 :: binary -> Either<string, string>
;; Strict UTF-8 decode: (list :right text) on success, or (list :left message) when the bytes are
;; not valid UTF-8.
(defvar hydra_overlay_emacs_lisp_lib_text_decode_utf8
  (lambda (data)
    (let* ((bytes (string-as-unibyte (concat data)))
           (text (decode-coding-string bytes 'utf-8 t))
           (invalid (cl-some (lambda (ch) (and (>= ch #x3FFF00) (<= ch #x3FFFFF)))
                             (append text nil))))
      (if (or (null text) invalid)
          (list :left "invalid UTF-8")
        (list :right text)))))

;; encodeUtf8 :: string -> binary
;; Total: every Hydra string is valid Unicode and therefore always encodes.
(defvar hydra_overlay_emacs_lisp_lib_text_encode_utf8
  (lambda (text)
    (encode-coding-string text 'utf-8 t)))

(provide 'hydra.lib.text)

;;; text.el ends here
