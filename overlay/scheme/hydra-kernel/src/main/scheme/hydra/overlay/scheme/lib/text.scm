(define-library (hydra overlay scheme lib text)
  (import (scheme base))
  (export hydra_lib_text_decode_utf8
          hydra_lib_text_encode_utf8)
  (begin

    ;; Scheme (R7RS) implementations of hydra.lib.text primitives (#494).
    ;;
    ;; UTF-8 codec primitives bridging Hydra strings and raw bytes (binary). These pair with
    ;; hydra.lib.files.readFile / writeFile, which are byte-oriented.
    ;;
    ;; Runtime representations (Scheme target):
    ;;   Either        : (list 'left v) | (list 'right v)
    ;;   binary/bytes  : a Scheme vector of byte ints (e.g. #(65 66)), matching the
    ;;                   hydra.lib.literals binary representation. The helpers below convert to/from
    ;;                   a bytevector for the R7RS utf8 codec (and accept a list or bytevector
    ;;                   defensively on decode).

    ;; Convert a Hydra binary value (vector of byte ints; also tolerates a list or bytevector) to a
    ;; bytevector for the R7RS utf8 codec.
    (define (binary->bytevector b)
      (cond
        ((bytevector? b) b)
        ((vector? b)
         (let ((bv (make-bytevector (vector-length b))))
           (do ((i 0 (+ i 1)))
               ((= i (vector-length b)) bv)
             (bytevector-u8-set! bv i (vector-ref b i)))))
        ((list? b)
         (let ((bv (make-bytevector (length b))))
           (let loop ((i 0) (xs b))
             (if (null? xs)
                 bv
                 (begin (bytevector-u8-set! bv i (car xs))
                        (loop (+ i 1) (cdr xs)))))))
        (else (error "binary->bytevector: unsupported binary representation" b))))

    ;; Convert a bytevector to the Hydra binary representation (a vector of byte ints).
    (define (bytevector->binary bv)
      (let ((v (make-vector (bytevector-length bv))))
        (do ((i 0 (+ i 1)))
            ((= i (bytevector-length bv)) v)
          (vector-set! v i (bytevector-u8-ref bv i)))))

    ;; decodeUtf8 :: binary -> Either<string, string>
    ;; Strict UTF-8 decode: returns (list 'right text) on success, or (list 'left message) when the
    ;; bytes are not valid UTF-8, where message is a host-provided description of the failure.
    ;; R7RS utf8->string raises on malformed input in strict implementations (e.g. guile); we guard
    ;; and report the failure as Left.
    (define hydra_lib_text_decode_utf8
      (lambda (data)
        (guard (e (#t
                   (let ((m (and (error-object? e) (error-object-message e))))
                     (list 'left (if (string? m) m "invalid UTF-8")))))
          (list 'right (utf8->string (binary->bytevector data))))))

    ;; encodeUtf8 :: string -> binary
    ;; Total: every Hydra string is valid Unicode and therefore always encodes.
    (define hydra_lib_text_encode_utf8
      (lambda (text)
        (bytevector->binary (string->utf8 text))))))
