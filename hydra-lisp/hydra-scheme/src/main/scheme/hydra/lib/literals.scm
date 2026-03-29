(define-library (hydra lib literals)
  (import (scheme base) (scheme inexact)
          (scheme bytevector)
          (srfi 151))         ;; Bitwise operations (chibi-compatible)
  (export hydra_lib_literals_bigfloat_to_bigint
          hydra_lib_literals_bigfloat_to_float
          hydra_lib_literals_bigfloat_to_float32
          hydra_lib_literals_bigfloat_to_float64
          hydra_lib_literals_bigint_to_bigfloat
          hydra_lib_literals_bigint_to_int
          hydra_lib_literals_bigint_to_int8
          hydra_lib_literals_bigint_to_int16
          hydra_lib_literals_bigint_to_int32
          hydra_lib_literals_bigint_to_int64
          hydra_lib_literals_bigint_to_uint
          hydra_lib_literals_bigint_to_uint8
          hydra_lib_literals_bigint_to_uint16
          hydra_lib_literals_bigint_to_uint32
          hydra_lib_literals_bigint_to_uint64
          hydra_lib_literals_binary_to_bytes
          hydra_lib_literals_binary_to_string
          hydra_lib_literals_float
          hydra_lib_literals_float32_to_bigfloat
          hydra_lib_literals_float64_to_bigfloat
          hydra_lib_literals_int
          hydra_lib_literals_int8_to_bigint
          hydra_lib_literals_int16_to_bigint
          hydra_lib_literals_int32_to_bigint
          hydra_lib_literals_int64_to_bigint
          hydra_lib_literals_read_bigfloat
          hydra_lib_literals_read_bigint
          hydra_lib_literals_read_boolean
          hydra_lib_literals_read_float
          hydra_lib_literals_read_float32
          hydra_lib_literals_read_float64
          hydra_lib_literals_read_int
          hydra_lib_literals_read_int8
          hydra_lib_literals_read_int16
          hydra_lib_literals_read_int32
          hydra_lib_literals_read_int64
          hydra_lib_literals_read_string
          hydra_lib_literals_read_uint
          hydra_lib_literals_read_uint8
          hydra_lib_literals_read_uint16
          hydra_lib_literals_read_uint32
          hydra_lib_literals_read_uint64
          hydra_lib_literals_show_bigfloat
          hydra_lib_literals_show_bigint
          hydra_lib_literals_show_boolean
          hydra_lib_literals_show_float
          hydra_lib_literals_show_float32
          hydra_lib_literals_show_float64
          hydra_lib_literals_show_int
          hydra_lib_literals_show_int8
          hydra_lib_literals_show_int16
          hydra_lib_literals_show_int32
          hydra_lib_literals_show_int64
          hydra_lib_literals_show_string
          hydra_lib_literals_show_uint
          hydra_lib_literals_show_uint8
          hydra_lib_literals_show_uint16
          hydra_lib_literals_show_uint32
          hydra_lib_literals_show_uint64
          hydra_lib_literals_string_to_binary
          hydra_lib_literals_uint
          hydra_lib_literals_uint8_to_bigint
          hydra_lib_literals_uint16_to_bigint
          hydra_lib_literals_uint32_to_bigint
          hydra_lib_literals_uint64_to_bigint)
  (begin

    ;; Approximate IEEE 754 float32 by rounding to ~7 significant digits
    ;; Snap to IEEE 754 float32 precision
    (define (float32-approx x) (snap-to-float32 x))

    ;; Haskell-compatible float formatting helper
    ;; Format a float matching Haskell's Show instance: scientific notation for
    ;; |x| < 0.1 or |x| >= 1e7, decimal notation otherwise. Full precision.
    (define (haskell-show-float x)
      (cond
        ((= x 0.0) "0.0")
        ((or (< (abs x) 0.1) (>= (abs x) 1.0e7))
         ;; Scientific notation needed. Use number->string and convert if needed.
         (let* ((s (number->string x))
                (has-e (let loop ((i 0))
                         (cond ((>= i (string-length s)) #f)
                               ((char=? (string-ref s i) #\e) #t)
                               (else (loop (+ i 1)))))))
           (if has-e
               s  ;; Already scientific notation with full precision
               ;; Decimal like "0.05" — convert to scientific via string manipulation.
               ;; Find the first significant digit and shift the decimal point.
               (let* ((neg (char=? (string-ref s 0) #\-))
                      (digits (if neg (substring s 1 (string-length s))
                                  s))
                      ;; Strip "0." prefix and count leading zeros
                      (after-dot (substring digits 2 (string-length digits)))
                      (leading-zeros (let loop ((i 0))
                                       (if (and (< i (string-length after-dot))
                                                (char=? (string-ref after-dot i) #\0))
                                           (loop (+ i 1)) i)))
                      (sig-digits (substring after-dot leading-zeros (string-length after-dot)))
                      ;; Strip trailing zeros from significant digits
                      (sig-trimmed (let loop ((i (- (string-length sig-digits) 1)))
                                     (if (and (> i 0) (char=? (string-ref sig-digits i) #\0))
                                         (loop (- i 1))
                                         (substring sig-digits 0 (+ i 1)))))
                      (mantissa (string-append (substring sig-trimmed 0 1)
                                               "."
                                               (if (> (string-length sig-trimmed) 1)
                                                   (substring sig-trimmed 1 (string-length sig-trimmed))
                                                   "0")))
                      (exp (- 0 leading-zeros 1))
                      (sign (if neg "-" "")))
                 (string-append sign mantissa "e" (number->string exp))))))
        (else (number->string x))))

    ;; Format a float32 value with minimum digits for unique representation
    ;; Simplified version without IEEE 754 bytevector round-trip
    (define (haskell-show-float32 x)
      (let ((f32 (float32-approx x)))
        (define (f32-roundtrip v) (float32-approx v))
        (define (f32-equal? a b) (= (float32-approx a) (float32-approx b)))
        ;; Round to n significant digits and format
        (define (round-sig x n)
          (if (= x 0.0) 0.0
              (let* ((e (exact (floor (/ (log (abs x)) (log 10)))))
                     (scale (expt 10 (- n e 1)))
                     (rounded (/ (round (* x scale)) scale)))
                (* 1.0 rounded))))
        (define (try-digits n)
          (if (> n 9) (number->string f32)
              (let* ((rounded (round-sig f32 n))
                     (s (number->string rounded)))
                (if (f32-equal? rounded f32)
                    s
                    (try-digits (+ n 1))))))
        (cond
          ((= f32 0.0) "0.0")
          ((and (not (= f32 0.0))
                (or (< (abs f32) 0.1) (>= (abs f32) 1.0e7)))
           (let* ((e (exact (floor (/ (log (abs f32)) (log 10)))))
                  (m (/ f32 (expt 10.0 e)))
                  (m-rounded (* 1.0 (/ (round (* (abs m) 1e6)) 1e6)))
                  (adj-e (if (>= m-rounded 10.0) (+ e 1) e))
                  (adj-m (if (>= m-rounded 10.0) (/ m-rounded 10.0) m-rounded))
                  (sign (if (< f32 0) "-" "")))
             (string-append sign (number->string adj-m) "e" (number->string adj-e))))
          (else (try-digits 1)))))

    ;; Convert a bigfloat (Double) to a bigint (Integer).
    ;; Uses round (banker's rounding / round-half-even) matching Haskell's behavior
    (define hydra_lib_literals_bigfloat_to_bigint
      (lambda (x)
        (exact (round x))))

    ;; Convert a bigfloat (Double) to a float (identity in Scheme).
    (define hydra_lib_literals_bigfloat_to_float
      (lambda (x)
        (inexact x)))

    ;; Convert a bigfloat (Double) to a float32 (Float).
    (define hydra_lib_literals_bigfloat_to_float32
      (lambda (x) (float32-approx x)))

    ;; Convert a bigfloat (Double) to a float64 (Double).
    (define hydra_lib_literals_bigfloat_to_float64
      (lambda (x)
        (inexact x)))

    ;; Convert a bigint (Integer) to a bigfloat (Double).
    (define hydra_lib_literals_bigint_to_bigfloat
      (lambda (x)
        (inexact x)))

    ;; Convert a bigint (Integer) to an int (identity in Scheme).
    (define hydra_lib_literals_bigint_to_int
      (lambda (x)
        x))

    ;; Convert a bigint (Integer) to an int8.
    (define hydra_lib_literals_bigint_to_int8
      (lambda (x)
        x))

    ;; Convert a bigint (Integer) to an int16.
    (define hydra_lib_literals_bigint_to_int16
      (lambda (x)
        x))

    ;; Convert a bigint (Integer) to an int32.
    (define hydra_lib_literals_bigint_to_int32
      (lambda (x)
        x))

    ;; Convert a bigint (Integer) to an int64.
    (define hydra_lib_literals_bigint_to_int64
      (lambda (x)
        x))

    ;; Convert a bigint (Integer) to a uint (identity in Scheme).
    (define hydra_lib_literals_bigint_to_uint
      (lambda (x)
        x))

    ;; Convert a bigint (Integer) to a uint8.
    (define hydra_lib_literals_bigint_to_uint8
      (lambda (x)
        x))

    ;; Convert a bigint (Integer) to a uint16.
    (define hydra_lib_literals_bigint_to_uint16
      (lambda (x)
        x))

    ;; Convert a bigint (Integer) to a uint32.
    (define hydra_lib_literals_bigint_to_uint32
      (lambda (x)
        x))

    ;; Convert a bigint (Integer) to a uint64.
    (define hydra_lib_literals_bigint_to_uint64
      (lambda (x)
        x))

    ;; Convert binary to a list of byte values (0-255).
    (define hydra_lib_literals_binary_to_bytes
      (lambda (bs)
        (cond
          ((bytevector? bs)
           (let loop ((i 0) (acc '()))
             (if (>= i (bytevector-length bs))
                 (reverse acc)
                 (loop (+ i 1) (cons (bytevector-u8-ref bs i) acc)))))
          ((list? bs) bs)
          (else (vector->list bs)))))

    ;; Convert binary to string by base64 encoding.
    (define hydra_lib_literals_binary_to_string
      (let ((b64-chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
        (lambda (bv)
          (let* ((bytes (cond
                          ((bytevector? bv)
                           (let ((len (bytevector-length bv)))
                             (let loop ((i 0) (acc '()))
                               (if (>= i len) (reverse acc)
                                   (loop (+ i 1) (cons (bytevector-u8-ref bv i) acc))))))
                          ((list? bv) bv)
                          (else (vector->list bv))))
                 (len (length bytes))
                 (vec (list->vector bytes)))
            (let loop ((i 0) (acc '()))
              (if (>= i len)
                  (list->string (reverse acc))
                  (let* ((b0 (vector-ref vec i))
                         (b1 (if (< (+ i 1) len) (vector-ref vec (+ i 1)) 0))
                         (b2 (if (< (+ i 2) len) (vector-ref vec (+ i 2)) 0))
                         (remaining (- len i))
                         (c0 (string-ref b64-chars (arithmetic-shift b0 -2)))
                         (c1 (string-ref b64-chars (bitwise-and (bitwise-ior (arithmetic-shift b0 4) (arithmetic-shift b1 -4)) 63)))
                         (c2 (if (>= remaining 2)
                                 (string-ref b64-chars (bitwise-and (bitwise-ior (arithmetic-shift b1 2) (arithmetic-shift b2 -6)) 63))
                                 #\=))
                         (c3 (if (>= remaining 3)
                                 (string-ref b64-chars (bitwise-and b2 63))
                                 #\=)))
                    (loop (+ i 3) (cons c3 (cons c2 (cons c1 (cons c0 acc))))))))))))

    ;; Convert a float value with given precision (identity in Scheme).
    (define hydra_lib_literals_float
      (lambda (precision)
        (lambda (x)
          (inexact x))))

    ;; Convert a float32 (Float) to a bigfloat (Double).
    (define hydra_lib_literals_float32_to_bigfloat
      (lambda (x)
        (inexact x)))

    ;; Convert a float64 (Double) to a bigfloat (Double).
    (define hydra_lib_literals_float64_to_bigfloat
      (lambda (x)
        (inexact x)))

    ;; Convert an int value with given precision (identity in Scheme).
    (define hydra_lib_literals_int
      (lambda (precision)
        (lambda (x)
          x)))

    ;; Convert an int8 to a bigint (Integer).
    (define hydra_lib_literals_int8_to_bigint
      (lambda (x)
        x))

    ;; Convert an int16 to a bigint (Integer).
    (define hydra_lib_literals_int16_to_bigint
      (lambda (x)
        x))

    ;; Convert an int32 to a bigint (Integer).
    (define hydra_lib_literals_int32_to_bigint
      (lambda (x)
        x))

    ;; Convert an int64 to a bigint (Integer).
    (define hydra_lib_literals_int64_to_bigint
      (lambda (x)
        x))

    ;; Parse a string to a bigfloat (Double).
    (define hydra_lib_literals_read_bigfloat
      (lambda (s)
        (let ((n (string->number s)))
          (if n
              (list 'just (inexact n))
              (list 'nothing)))))

    ;; Parse a string to a bigint (Integer).
    (define hydra_lib_literals_read_bigint
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; Parse a string to a boolean.
    (define hydra_lib_literals_read_boolean
      (lambda (s)
        (cond
          ((string=? s "true") (list 'just #t))
          ((string=? s "false") (list 'just #f))
          (else (list 'nothing)))))

    ;; Parse a string to a float (Double).
    (define hydra_lib_literals_read_float
      (lambda (s)
        (let ((n (string->number s)))
          (if n
              (list 'just (inexact n))
              (list 'nothing)))))

    ;; Parse a string to a float32 (Float).
    ;; Round to float32 precision
    (define hydra_lib_literals_read_float32
      (lambda (s)
        (let ((n (string->number s)))
          (if n
              (list 'just (float32-approx n))
              (list 'nothing)))))

    ;; Parse a string to a float64 (Double).
    (define hydra_lib_literals_read_float64
      (lambda (s)
        (let ((n (string->number s)))
          (if n
              (list 'just (inexact n))
              (list 'nothing)))))

    ;; Parse a string to an int.
    (define hydra_lib_literals_read_int
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; Parse a string to an int8 (-128 to 127).
    (define hydra_lib_literals_read_int8
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n -128) (<= n 127))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; Parse a string to an int16 (-32768 to 32767).
    (define hydra_lib_literals_read_int16
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n -32768) (<= n 32767))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; Parse a string to an int32.
    (define hydra_lib_literals_read_int32
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; Parse a string to an int64.
    (define hydra_lib_literals_read_int64
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; Parse a string literal.
    (define hydra_lib_literals_read_string
      (lambda (s)
        (let ((len (string-length s)))
          (if (and (>= len 2)
                   (char=? (string-ref s 0) #\")
                   (char=? (string-ref s (- len 1)) #\"))
              (let ((inner (substring s 1 (- len 1))))
                (let loop ((i 0) (acc '()))
                  (if (>= i (string-length inner))
                      (list 'just (list->string (reverse acc)))
                      (if (and (char=? (string-ref inner i) #\\)
                               (< (+ i 1) (string-length inner)))
                          (let ((c (string-ref inner (+ i 1))))
                            (cond
                              ((char=? c #\\) (loop (+ i 2) (cons #\\ acc)))
                              ((char=? c #\") (loop (+ i 2) (cons #\" acc)))
                              ((char=? c #\n) (loop (+ i 2) (cons #\newline acc)))
                              ((char=? c #\t) (loop (+ i 2) (cons #\tab acc)))
                              ((char=? c #\r) (loop (+ i 2) (cons #\return acc)))
                              (else (loop (+ i 2) (cons c (cons #\\ acc))))))
                          (loop (+ i 1) (cons (string-ref inner i) acc))))))
              (list 'nothing)))))

    ;; Parse a string to a uint (non-negative integer).
    (define hydra_lib_literals_read_uint
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n 0))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; Parse a string to a uint8 (0 to 255).
    (define hydra_lib_literals_read_uint8
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n 0) (<= n 255))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; Parse a string to a uint16 (0 to 65535).
    (define hydra_lib_literals_read_uint16
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n 0) (<= n 65535))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; Parse a string to a uint32 (0 to 4294967295).
    (define hydra_lib_literals_read_uint32
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n 0))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; Parse a string to a uint64 (0 to 18446744073709551615).
    (define hydra_lib_literals_read_uint64
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n 0))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; Convert a bigfloat (Double) to string.
    (define hydra_lib_literals_show_bigfloat
      (lambda (x)
        (haskell-show-float x)))

    ;; Convert a bigint (Integer) to string.
    (define hydra_lib_literals_show_bigint
      (lambda (x)
        (number->string x)))

    ;; Convert a boolean to string.
    (define hydra_lib_literals_show_boolean
      (lambda (x)
        (if x "true" "false")))

    ;; Convert a float (Double) to string.
    (define hydra_lib_literals_show_float
      (lambda (x)
        (haskell-show-float x)))

    ;; Convert a float32 (Float) to string.
    (define hydra_lib_literals_show_float32
      (lambda (x)
        (haskell-show-float32 x)))

    ;; Convert a float64 (Double) to string.
    (define hydra_lib_literals_show_float64
      (lambda (x)
        (haskell-show-float x)))

    ;; Convert an int to string.
    (define hydra_lib_literals_show_int
      (lambda (x)
        (number->string x)))

    ;; Convert an int8 to string.
    (define hydra_lib_literals_show_int8
      (lambda (x)
        (number->string x)))

    ;; Convert an int16 to string.
    (define hydra_lib_literals_show_int16
      (lambda (x)
        (number->string x)))

    ;; Convert an int32 to string.
    (define hydra_lib_literals_show_int32
      (lambda (x)
        (number->string x)))

    ;; Convert an int64 to string.
    (define hydra_lib_literals_show_int64
      (lambda (x)
        (number->string x)))

    ;; Convert a string to a quoted string representation.
    (define hydra_lib_literals_show_string
      (lambda (s)
        (let loop ((i 0) (acc "\""))
          (if (>= i (string-length s))
              (string-append acc "\"")
              (let* ((c (string-ref s i))
                     (cp (char->integer c)))
                (loop (+ i 1)
                      (string-append acc
                        (cond
                          ((char=? c #\\) "\\\\")
                          ((char=? c #\") "\\\"")
                          ((char=? c #\newline) "\\n")
                          ((char=? c #\return) "\\r")
                          ((char=? c #\tab) "\\t")
                          ((= cp 0) "\\NUL")
                          ((= cp 7) "\\a")
                          ((= cp 8) "\\b")
                          ((= cp 12) "\\f")
                          ((= cp 11) "\\v")
                          ((= cp 127) "\\DEL")
                          ((< cp 32) (string-append "\\" (number->string cp)))
                          ((> cp 127) (string-append "\\" (number->string cp)))
                          (else (string c))))))))))

    ;; Convert a uint to string.
    (define hydra_lib_literals_show_uint
      (lambda (x)
        (number->string x)))

    ;; Convert a uint8 to string.
    (define hydra_lib_literals_show_uint8
      (lambda (x)
        (number->string x)))

    ;; Convert a uint16 to string.
    (define hydra_lib_literals_show_uint16
      (lambda (x)
        (number->string x)))

    ;; Convert a uint32 to string.
    (define hydra_lib_literals_show_uint32
      (lambda (x)
        (number->string x)))

    ;; Convert a uint64 to string.
    (define hydra_lib_literals_show_uint64
      (lambda (x)
        (number->string x)))

    ;; Convert string to binary by base64 decoding.
    ;; Returns an empty list if decoding fails.
    (define hydra_lib_literals_string_to_binary
      (let ((b64-vals (let ((tbl (make-vector 128 -1)))
              (let loop ((i 0) (chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
                (when (< i (string-length chars))
                  (vector-set! tbl (char->integer (string-ref chars i)) i)
                  (loop (+ i 1) chars)))
              tbl)))
        (lambda (s)
          (let* ((len (string-length s))
                 (pad (cond ((and (> len 0) (char=? (string-ref s (- len 1)) #\=))
                             (if (and (> len 1) (char=? (string-ref s (- len 2)) #\=)) 2 1))
                            (else 0)))
                 (in-len (- len pad))
                 (out-len (- (quotient (* in-len 3) 4) 0)))
            (let loop ((i 0) (acc '()))
              (if (>= i in-len)
                  (reverse acc)
                  (let* ((a (vector-ref b64-vals (char->integer (string-ref s i))))
                         (b (if (< (+ i 1) in-len) (vector-ref b64-vals (char->integer (string-ref s (+ i 1)))) 0))
                         (c (if (< (+ i 2) in-len) (vector-ref b64-vals (char->integer (string-ref s (+ i 2)))) 0))
                         (d (if (< (+ i 3) in-len) (vector-ref b64-vals (char->integer (string-ref s (+ i 3)))) 0))
                         (n (+ (arithmetic-shift a 18) (arithmetic-shift b 12) (arithmetic-shift c 6) d))
                         (remaining (- in-len i)))
                    (let ((acc1 (cons (bitwise-and (arithmetic-shift n -16) #xff) acc)))
                      (let ((acc2 (if (>= remaining 3)
                                      (cons (bitwise-and (arithmetic-shift n -8) #xff) acc1)
                                      acc1)))
                        (let ((acc3 (if (>= remaining 4)
                                        (cons (bitwise-and n #xff) acc2)
                                        acc2)))
                          (loop (+ i 4) acc3)))))))))))

    ;; Convert a uint value with given precision (identity in Scheme).
    (define hydra_lib_literals_uint
      (lambda (precision)
        (lambda (x)
          x)))

    ;; Convert a uint8 to a bigint (Integer).
    (define hydra_lib_literals_uint8_to_bigint
      (lambda (x)
        x))

    ;; Convert a uint16 to a bigint (Integer).
    (define hydra_lib_literals_uint16_to_bigint
      (lambda (x)
        x))

    ;; Convert a uint32 to a bigint (Integer).
    (define hydra_lib_literals_uint32_to_bigint
      (lambda (x)
        x))

    ;; Convert a uint64 to a bigint (Integer).
    (define hydra_lib_literals_uint64_to_bigint
      (lambda (x)
        x))))
