(in-package :cl-user)

;; Common Lisp implementations of hydra.lib.text primitives (#494).
;;
;; UTF-8 codec primitives bridging Hydra strings and raw bytes (binary). These pair with
;; hydra.lib.files.readFile / writeFile, which are byte-oriented.
;;
;; Runtime representations (Common Lisp target):
;;   Either        : (list :left v) | (list :right v)
;;   binary/bytes  : a vector of (unsigned-byte 8). Inputs are coerced, so any byte sequence works.
;;
;; Implementation: the CL host runs under SBCL (see packages/hydra-lisp/bin/run-tests.sh), whose
;; sb-ext:octets-to-string / string-to-octets perform strict UTF-8 transcoding and signal an error
;; on malformed input. A portable fallback (a hand-rolled UTF-8 decoder/encoder) is provided for
;; other implementations so this file remains loadable everywhere.

;; ---- Helpers (not primitives) ----

#-sbcl
(defun hydra-text-utf8-decode (bytes)
  "Strict UTF-8 decode of a byte sequence to a string; signals an error on malformed input.
   Portable fallback used when sb-ext is unavailable."
  (let ((bv (coerce bytes 'vector))
        (out (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (i 0))
    (let ((len (length bv)))
      (loop while (< i len) do
        (let ((b0 (aref bv i)))
          (cond
            ((< b0 #x80)
             (vector-push-extend (code-char b0) out) (incf i))
            ((= (logand b0 #xE0) #xC0)
             (when (> (+ i 2) len) (error "truncated UTF-8 sequence"))
             (let ((b1 (aref bv (+ i 1))))
               (unless (= (logand b1 #xC0) #x80) (error "invalid UTF-8 continuation byte"))
               (let ((cp (logior (ash (logand b0 #x1F) 6) (logand b1 #x3F))))
                 (when (< cp #x80) (error "overlong UTF-8 encoding"))
                 (vector-push-extend (code-char cp) out)))
             (incf i 2))
            ((= (logand b0 #xF0) #xE0)
             (when (> (+ i 3) len) (error "truncated UTF-8 sequence"))
             (let ((b1 (aref bv (+ i 1))) (b2 (aref bv (+ i 2))))
               (unless (and (= (logand b1 #xC0) #x80) (= (logand b2 #xC0) #x80))
                 (error "invalid UTF-8 continuation byte"))
               (let ((cp (logior (ash (logand b0 #x0F) 12)
                                 (ash (logand b1 #x3F) 6)
                                 (logand b2 #x3F))))
                 (when (< cp #x800) (error "overlong UTF-8 encoding"))
                 (vector-push-extend (code-char cp) out)))
             (incf i 3))
            ((= (logand b0 #xF8) #xF0)
             (when (> (+ i 4) len) (error "truncated UTF-8 sequence"))
             (let ((b1 (aref bv (+ i 1))) (b2 (aref bv (+ i 2))) (b3 (aref bv (+ i 3))))
               (unless (and (= (logand b1 #xC0) #x80) (= (logand b2 #xC0) #x80) (= (logand b3 #xC0) #x80))
                 (error "invalid UTF-8 continuation byte"))
               (let ((cp (logior (ash (logand b0 #x07) 18)
                                 (ash (logand b1 #x3F) 12)
                                 (ash (logand b2 #x3F) 6)
                                 (logand b3 #x3F))))
                 (when (< cp #x10000) (error "overlong UTF-8 encoding"))
                 (vector-push-extend (code-char cp) out)))
             (incf i 4))
            (t (error "invalid UTF-8 lead byte")))))
      (coerce out 'string))))

#-sbcl
(defun hydra-text-utf8-encode (text)
  "Encode a string as a vector of UTF-8 bytes (total). Portable fallback."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for ch across text do
      (let ((cp (char-code ch)))
        (cond
          ((< cp #x80) (vector-push-extend cp out))
          ((< cp #x800)
           (vector-push-extend (logior #xC0 (ash cp -6)) out)
           (vector-push-extend (logior #x80 (logand cp #x3F)) out))
          ((< cp #x10000)
           (vector-push-extend (logior #xE0 (ash cp -12)) out)
           (vector-push-extend (logior #x80 (logand (ash cp -6) #x3F)) out)
           (vector-push-extend (logior #x80 (logand cp #x3F)) out))
          (t
           (vector-push-extend (logior #xF0 (ash cp -18)) out)
           (vector-push-extend (logior #x80 (logand (ash cp -12) #x3F)) out)
           (vector-push-extend (logior #x80 (logand (ash cp -6) #x3F)) out)
           (vector-push-extend (logior #x80 (logand cp #x3F)) out)))))
    (coerce out '(vector (unsigned-byte 8)))))

;; ---- Primitives ----

;; decodeUtf8 :: binary -> Either<string, string>
;; Strict UTF-8 decode: (list :right text) on success, or (list :left message) when the bytes are
;; not valid UTF-8, where message is a host-provided description of the failure.
(defvar hydra_overlay_common_lisp_lib_text_decode_utf8
  (lambda (data)
    (handler-case
        (list :right
              #+sbcl (sb-ext:octets-to-string
                       (coerce data '(vector (unsigned-byte 8))) :external-format :utf-8)
              #-sbcl (hydra-text-utf8-decode data))
      (error (e)
        (let ((m (princ-to-string e)))
          (list :left (if (or (null m) (string= m "")) "invalid UTF-8" m)))))))

;; encodeUtf8 :: string -> binary
;; Total: every Hydra string is valid Unicode and therefore always encodes.
(defvar hydra_overlay_common_lisp_lib_text_encode_utf8
  (lambda (text)
    #+sbcl (sb-ext:string-to-octets text :external-format :utf-8)
    #-sbcl (hydra-text-utf8-encode text)))
