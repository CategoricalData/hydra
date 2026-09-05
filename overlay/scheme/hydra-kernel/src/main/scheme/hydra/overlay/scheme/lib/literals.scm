(define-library (hydra overlay scheme lib literals)
  (import (scheme base) (scheme inexact) (scheme char)
          (scheme bytevector)
          (srfi 151))         ;; Bitwise operations (chibi-compatible)
  (export hydra_overlay_scheme_lib_literals_bigint_to_decimal
          hydra_overlay_scheme_lib_literals_bigint_to_int
          hydra_overlay_scheme_lib_literals_bigint_to_int8
          hydra_overlay_scheme_lib_literals_bigint_to_int16
          hydra_overlay_scheme_lib_literals_bigint_to_int32
          hydra_overlay_scheme_lib_literals_bigint_to_int64
          hydra_overlay_scheme_lib_literals_bigint_to_uint
          hydra_overlay_scheme_lib_literals_bigint_to_uint8
          hydra_overlay_scheme_lib_literals_bigint_to_uint16
          hydra_overlay_scheme_lib_literals_bigint_to_uint32
          hydra_overlay_scheme_lib_literals_bigint_to_uint64
          hydra_overlay_scheme_lib_literals_binary_to_base64
          hydra_overlay_scheme_lib_literals_decimal_to_bigint
          hydra_overlay_scheme_lib_literals_decimal_to_float32
          hydra_overlay_scheme_lib_literals_decimal_to_float64
          hydra_overlay_scheme_lib_literals_float
          hydra_overlay_scheme_lib_literals_float32_to_decimal
          hydra_overlay_scheme_lib_literals_float32_to_float64
          hydra_overlay_scheme_lib_literals_float64_to_decimal
          hydra_overlay_scheme_lib_literals_float64_to_float32
          hydra_overlay_scheme_lib_literals_int
          hydra_overlay_scheme_lib_literals_int8_to_bigint
          hydra_overlay_scheme_lib_literals_int16_to_bigint
          hydra_overlay_scheme_lib_literals_int32_to_bigint
          hydra_overlay_scheme_lib_literals_int64_to_bigint
          hydra_overlay_scheme_lib_literals_parse_bigint
          hydra_overlay_scheme_lib_literals_parse_decimal
          hydra_overlay_scheme_lib_literals_read_float
          hydra_overlay_scheme_lib_literals_parse_float32
          hydra_overlay_scheme_lib_literals_read_int
          hydra_overlay_scheme_lib_literals_parse_int64
          hydra_overlay_scheme_lib_literals_read_uint
          hydra_overlay_scheme_lib_literals_parse_uint32
          hydra_overlay_scheme_lib_literals_parse_uint64
          hydra_overlay_scheme_lib_literals_print_bigint
          hydra_overlay_scheme_lib_literals_print_decimal
          hydra_overlay_scheme_lib_literals_show_float
          hydra_overlay_scheme_lib_literals_print_float32
          hydra_overlay_scheme_lib_literals_print_float64
          hydra_overlay_scheme_lib_literals_show_int
          hydra_overlay_scheme_lib_literals_print_int8
          hydra_overlay_scheme_lib_literals_print_int16
          hydra_overlay_scheme_lib_literals_print_int32
          hydra_overlay_scheme_lib_literals_print_int64
          hydra_overlay_scheme_lib_literals_print_string
          hydra_overlay_scheme_lib_literals_show_uint
          hydra_overlay_scheme_lib_literals_print_uint8
          hydra_overlay_scheme_lib_literals_print_uint16
          hydra_overlay_scheme_lib_literals_print_uint32
          hydra_overlay_scheme_lib_literals_print_uint64
          hydra_overlay_scheme_lib_literals_base64_to_binary
          hydra_overlay_scheme_lib_literals_uint
          hydra_overlay_scheme_lib_literals_uint8_to_bigint
          hydra_overlay_scheme_lib_literals_uint16_to_bigint
          hydra_overlay_scheme_lib_literals_uint32_to_bigint
          hydra_overlay_scheme_lib_literals_binary_to_bytes
          hydra_overlay_scheme_lib_literals_parse_boolean
          hydra_overlay_scheme_lib_literals_parse_float64
          hydra_overlay_scheme_lib_literals_parse_int8
          hydra_overlay_scheme_lib_literals_parse_int16
          hydra_overlay_scheme_lib_literals_parse_int32
          hydra_overlay_scheme_lib_literals_parse_string
          hydra_overlay_scheme_lib_literals_parse_uint8
          hydra_overlay_scheme_lib_literals_parse_uint16
          hydra_overlay_scheme_lib_literals_print_boolean
          hydra_overlay_scheme_lib_literals_uint64_to_bigint)
  (begin

    ;; Safe abs that handles complex numbers (returns magnitude for complex, which is real)
    (define (safe-abs x)
      (if (real? x) (abs x) (magnitude x)))

    ;; Approximate IEEE 754 float32 by rounding to ~7 significant digits
    ;; Snap to IEEE 754 float32 precision
    (define (float32-approx x) (snap-to-float32 x))

    ;; A scale-preserving arbitrary-precision decimal: the numeric value is
    ;; (coefficient * 10^-scale), represented as a (coefficient . scale) cons pair of native
    ;; Scheme bignums. Mirrors java.math.BigDecimal's (unscaledValue, scale) convention, so
    ;; "1.10" is (110 . 2) and "1.1" is (11 . 1) -- distinct values per the kernel spec
    ;; (docs/specification/ordering-and-equality.md: 1.1 != 1.10). scale is always >= 0.
    (define (hydra-make-decimal coefficient scale) (cons coefficient scale))
    (define (hydra-decimal-coefficient d) (car d))
    (define (hydra-decimal-scale d) (cdr d))

    ;; bigint_to_decimal :: BigInteger -> Decimal
    ;; Exact: a bigint is a decimal with scale 0.
    (define hydra_overlay_scheme_lib_literals_bigint_to_decimal
      (lambda (x)
        (hydra-make-decimal x 0)))

    ;; bigint_to_int :: BigInteger -> Int  (identity in Scheme)
    (define hydra_overlay_scheme_lib_literals_bigint_to_int
      (lambda (x)
        x))

    ;; bigint_to_int8 :: BigInteger -> Int8
    (define hydra_overlay_scheme_lib_literals_bigint_to_int8
      (lambda (x)
        x))

    ;; bigint_to_int16 :: BigInteger -> Int16
    (define hydra_overlay_scheme_lib_literals_bigint_to_int16
      (lambda (x)
        x))

    ;; bigint_to_int32 :: BigInteger -> Int32
    (define hydra_overlay_scheme_lib_literals_bigint_to_int32
      (lambda (x)
        x))

    ;; bigint_to_int64 :: BigInteger -> Int64
    (define hydra_overlay_scheme_lib_literals_bigint_to_int64
      (lambda (x)
        x))

    ;; bigint_to_uint :: BigInteger -> Uint
    (define hydra_overlay_scheme_lib_literals_bigint_to_uint
      (lambda (x)
        x))

    ;; bigint_to_uint8 :: BigInteger -> Uint8
    (define hydra_overlay_scheme_lib_literals_bigint_to_uint8
      (lambda (x)
        x))

    ;; bigint_to_uint16 :: BigInteger -> Uint16
    (define hydra_overlay_scheme_lib_literals_bigint_to_uint16
      (lambda (x)
        x))

    ;; bigint_to_uint32 :: BigInteger -> Uint32
    (define hydra_overlay_scheme_lib_literals_bigint_to_uint32
      (lambda (x)
        x))

    ;; bigint_to_uint64 :: BigInteger -> Uint64
    (define hydra_overlay_scheme_lib_literals_bigint_to_uint64
      (lambda (x)
        x))

    ;; binary_to_base64 :: ByteString -> String (base64 encoding)
    (define hydra_overlay_scheme_lib_literals_binary_to_base64
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

    ;; Convert a Decimal to an exact Scheme rational (coefficient / 10^scale), for use as an
    ;; intermediate in float conversions (Scheme's `inexact` coerces any rational exactly-
    ;; then-rounds).
    (define (hydra-decimal-rational x)
      (let ((coefficient (hydra-decimal-coefficient x))
            (scale (hydra-decimal-scale x)))
        (if (<= scale 0)
            (* coefficient (expt 10 (- scale)))
            (/ coefficient (expt 10 scale)))))

    ;; decimal_to_bigint :: Decimal -> BigInteger
    ;; Rounds to the nearest integer, ties to even (banker's rounding) -- matches the
    ;; Python/Java reference hosts' behavior (e.g. 42.7 rounds to 43, 2.5 rounds to 2).
    ;; Scheme's own ROUND already implements round-half-to-even on exact rationals.
    (define hydra_overlay_scheme_lib_literals_decimal_to_bigint
      (lambda (x)
        (let ((coefficient (hydra-decimal-coefficient x))
              (scale (hydra-decimal-scale x)))
          (if (<= scale 0)
              (* coefficient (expt 10 (- scale)))
              (round (/ coefficient (expt 10 scale)))))))

    ;; decimal_to_float32 :: Decimal -> Float
    ;; Exact rational -> single-float precision, rounding to the nearest representable value.
    (define hydra_overlay_scheme_lib_literals_decimal_to_float32
      (lambda (x) (float32-approx (inexact (hydra-decimal-rational x)))))

    ;; decimal_to_float64 :: Decimal -> Double
    ;; Exact rational -> double-float, rounding to the nearest representable value.
    (define hydra_overlay_scheme_lib_literals_decimal_to_float64
      (lambda (x)
        (inexact (hydra-decimal-rational x))))

    ;; float :: FloatPrecision -> Double -> Double
    (define hydra_overlay_scheme_lib_literals_float
      (lambda (precision)
        (lambda (x)
          (inexact x))))

    ;; float32_to_decimal :: Float -> Decimal
    ;; Exact: every IEEE 754 float has a finite decimal expansion. Derived from the shortest
    ;; round-tripping digit string, matching printFloat32's own shortest-round-trip
    ;; convention. hydra-decimal-from-float is defined further down (near
    ;; hydra--decimal-digits-and-exponent); forward reference is fine since these are all
    ;; top-level defines loaded together.
    (define hydra_overlay_scheme_lib_literals_float32_to_decimal
      (lambda (x)
        (hydra-decimal-from-float x)))

    ;; float32_to_float64 :: Float -> Double
    ;; Scheme has a single inexact float type; widening is identity.
    (define hydra_overlay_scheme_lib_literals_float32_to_float64
      (lambda (x)
        (inexact x)))

    ;; float64_to_decimal :: Double -> Decimal
    ;; Exact: every IEEE 754 float has a finite decimal expansion. Derived from the shortest
    ;; round-tripping digit string, matching printFloat64's own shortest-round-trip convention.
    (define hydra_overlay_scheme_lib_literals_float64_to_decimal
      (lambda (x)
        (hydra-decimal-from-float x)))

    ;; float64_to_float32 :: Double -> Float
    ;; Snap to IEEE 754 single-precision (lossy narrowing).
    (define hydra_overlay_scheme_lib_literals_float64_to_float32
      (lambda (x)
        (float32-approx x)))

    ;; int :: IntPrecision -> Int -> Int
    (define hydra_overlay_scheme_lib_literals_int
      (lambda (precision)
        (lambda (x)
          x)))

    ;; int8_to_bigint :: Int8 -> BigInteger
    (define hydra_overlay_scheme_lib_literals_int8_to_bigint
      (lambda (x)
        x))

    ;; int16_to_bigint :: Int16 -> BigInteger
    (define hydra_overlay_scheme_lib_literals_int16_to_bigint
      (lambda (x)
        x))

    ;; int32_to_bigint :: Int32 -> BigInteger
    (define hydra_overlay_scheme_lib_literals_int32_to_bigint
      (lambda (x)
        x))

    ;; int64_to_bigint :: Int64 -> BigInteger
    (define hydra_overlay_scheme_lib_literals_int64_to_bigint
      (lambda (x)
        x))

    ;; Parse the JSON number grammar (docs/specification/syntax.md #2.6) into a
    ;; scale-preserving Decimal: an optional sign, integer digits, an optional fraction part,
    ;; and an optional exponent part. Scale-preserving means "1.10" and "1.1" parse to
    ;; distinct values (scale 2 vs scale 1). Returns #f on parse failure.
    (define (hydra-parse-decimal-1 s)
      (let ((len (string-length s)))
        (call-with-current-continuation
         (lambda (return)
           (let* ((i 0)
                  (neg (and (< i len) (char=? (string-ref s i) #\-))))
             (if neg (set! i (+ i 1)))
             (let ((int-start i))
               (let loop ()
                 (if (and (< i len) (char-numeric? (string-ref s i)))
                     (begin (set! i (+ i 1)) (loop))))
               (if (= i int-start) (return #f))
               (let* ((int-digits (substring s int-start i))
                      (frac-digits ""))
                 (if (and (< i len) (char=? (string-ref s i) #\.))
                     (begin
                       (set! i (+ i 1))
                       (let ((frac-start i))
                         (let loop ()
                           (if (and (< i len) (char-numeric? (string-ref s i)))
                               (begin (set! i (+ i 1)) (loop))))
                         (if (= i frac-start) (return #f))
                         (set! frac-digits (substring s frac-start i)))))
                 (let ((exp-value 0))
                   (if (and (< i len) (or (char=? (string-ref s i) #\e) (char=? (string-ref s i) #\E)))
                       (begin
                         (set! i (+ i 1))
                         (let ((exp-neg #f) (exp-start i))
                           (if (and (< i len) (or (char=? (string-ref s i) #\+) (char=? (string-ref s i) #\-)))
                               (begin
                                 (set! exp-neg (char=? (string-ref s i) #\-))
                                 (set! i (+ i 1))
                                 (set! exp-start i)))
                           (let loop ()
                             (if (and (< i len) (char-numeric? (string-ref s i)))
                                 (begin (set! i (+ i 1)) (loop))))
                           (if (= i exp-start) (return #f))
                           (let ((v (string->number (substring s exp-start i))))
                             (set! exp-value (if exp-neg (- v) v))))))
                   (if (not (= i len)) (return #f))
                   (let* ((digits (string-append int-digits frac-digits))
                          (coefficient (if (string=? digits "") 0 (string->number digits)))
                          (coefficient (if neg (- coefficient) coefficient))
                          (scale (- (string-length frac-digits) exp-value)))
                     (if (< scale 0)
                         (hydra-make-decimal (* coefficient (expt 10 (- scale))) 0)
                         (hydra-make-decimal coefficient scale)))))))))))

    ;; parse_decimal :: String -> Maybe Decimal
    (define hydra_overlay_scheme_lib_literals_parse_decimal
      (lambda (s)
        (let ((d (hydra-parse-decimal-1 s)))
          (if d (list 'given d) (list 'none)))))

    ;; parse_bigint :: String -> Maybe BigInteger
    (define hydra_overlay_scheme_lib_literals_parse_bigint
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n))
              (list 'given (exact n))
              (list 'none)))))

    ;; read_float :: String -> Maybe Double
    (define hydra_overlay_scheme_lib_literals_read_float
      (lambda (s)
        (let ((n (string->number s)))
          (if n
              (list 'given (inexact n))
              (list 'none)))))

    ;; parse_float32 :: String -> Maybe Float
    ;; Round to float32 precision
    (define hydra_overlay_scheme_lib_literals_parse_float32
      (lambda (s)
        (cond
          ((string=? s "NaN") (list 'given +nan.0))
          ((string=? s "Infinity") (list 'given +inf.0))
          ((string=? s "-Infinity") (list 'given -inf.0))
          (else (let ((n (string->number s)))
                  (if n
                      (list 'given (float32-approx n))
                      (list 'none)))))))

    ;; read_int :: String -> Maybe Int
    (define hydra_overlay_scheme_lib_literals_read_int
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n))
              (list 'given (exact n))
              (list 'none)))))

    ;; parse_int64 :: String -> Maybe Int64
    (define hydra_overlay_scheme_lib_literals_parse_int64
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n))
              (list 'given (exact n))
              (list 'none)))))

    ;; read_uint :: String -> Maybe Uint
    (define hydra_overlay_scheme_lib_literals_read_uint
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n 0))
              (list 'given (exact n))
              (list 'none)))))

    ;; parse_uint32 :: String -> Maybe Uint32
    (define hydra_overlay_scheme_lib_literals_parse_uint32
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n 0))
              (list 'given (exact n))
              (list 'none)))))

    ;; parse_uint64 :: String -> Maybe Uint64
    (define hydra_overlay_scheme_lib_literals_parse_uint64
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n 0))
              (list 'given (exact n))
              (list 'none)))))

    ;; Haskell-compatible float formatting helper
    ;; Format a float matching Haskell's Show instance: scientific notation for
    ;; |x| < 0.1 or |x| >= 1e7, decimal notation otherwise. Full precision.
    ;; Derive (significant-digit-string . adjusted-exponent) for a non-zero
    ;; real X, using the implementation's own correctly-rounded printer. The
    ;; adjusted exponent is the power of 10 of the leading significant digit
    ;; (e.g. 3 for 1234.0, -2 for 0.01), matching printDecimal's own 'a'
    ;; (Literals.hs).
    (define (hydra--decimal-digits-and-exponent x)
      (let* ((s (number->string (safe-abs x)))
             (e-idx (let loop ((i 0))
                      (cond ((>= i (string-length s)) #f)
                            ((char=? (string-ref s i) #\e) i)
                            (else (loop (+ i 1))))))
             (mtext (if e-idx (substring s 0 e-idx) s))
             (base-exp (if e-idx
                           (string->number (substring s (+ e-idx 1) (string-length s)))
                           0))
             (dot-idx (let loop ((i 0))
                        (cond ((>= i (string-length mtext)) (string-length mtext))
                              ((char=? (string-ref mtext i) #\.) i)
                              (else (loop (+ i 1))))))
             (digits-before (substring mtext 0 dot-idx))
             (digits-after (if (< dot-idx (string-length mtext))
                               (substring mtext (+ dot-idx 1) (string-length mtext))
                               ""))
             (all-digits (string-append digits-before digits-after))
             (point-pos (string-length digits-before))
             (first-nz (let loop ((i 0))
                         (cond ((>= i (string-length all-digits)) 0)
                               ((char=? (string-ref all-digits i) #\0) (loop (+ i 1)))
                               (else i))))
             (last-nz (let loop ((i (- (string-length all-digits) 1)))
                        (cond ((<= i first-nz) first-nz)
                              ((char=? (string-ref all-digits i) #\0) (loop (- i 1)))
                              (else i))))
             (sig (substring all-digits first-nz (+ last-nz 1)))
             (exp (+ base-exp (- point-pos first-nz 1))))
        (cons sig exp)))

    ;; Build a Decimal from a float's shortest round-tripping significant-digit string and
    ;; adjusted exponent (via hydra--decimal-digits-and-exponent above). E.g. digits "314" and
    ;; adjusted-exponent 0 -> the value 3.14, represented as coefficient 314, scale
    ;; (length digits - 1 - adjusted-exponent) = 2.
    (define (hydra-decimal-from-float x)
      (cond
        ((not (real? x)) (hydra-make-decimal 0 0))
        ((nan? x) (hydra-make-decimal 0 0))
        ((infinite? x) (hydra-make-decimal 0 0))
        ((= x 0.0) (hydra-make-decimal 0 0))
        (else
         (let* ((digex (hydra--decimal-digits-and-exponent x))
                (sig (car digex))
                (adjusted-exp (cdr digex))
                (coefficient (string->number sig))
                (coefficient (if (< x 0) (- coefficient) coefficient))
                (scale (- (string-length sig) 1 adjusted-exp)))
           (if (< scale 0)
               (hydra-make-decimal (* coefficient (expt 10 (- scale))) 0)
               (hydra-make-decimal coefficient scale))))))

    (define (haskell-show-float x)
      (cond
        ((not (real? x)) "NaN")  ;; complex results from out-of-domain trig
        ((not (= x x)) "NaN")
        ((or (= x +inf.0) (= x -inf.0)) (if (> x 0) "Infinity" "-Infinity"))
        ((= x 0.0) (if (eqv? x -0.0) "-0.0" "0.0"))
        ((or (< (safe-abs x) 0.1) (>= (safe-abs x) 1.0e7))
         ;; Scientific notation needed. Renormalize number->string output to
         ;; Haskell's "D.DDDeN" form via pure string manipulation (no FP
         ;; arithmetic — avoids ulp-error in log/exp that would corrupt the
         ;; mantissa).
         (let* ((s (number->string x))
                (neg (char=? (string-ref s 0) #\-))
                (body (if neg (substring s 1 (string-length s)) s))
                ;; Split body into mantissa-text and (optional) exponent N.
                ;; Guile may return "4000000001.0" (no e) or "5.0e10" (with e).
                (e-idx (let loop ((i 0))
                         (cond ((>= i (string-length body)) #f)
                               ((char=? (string-ref body i) #\e) i)
                               (else (loop (+ i 1))))))
                (mtext (if e-idx (substring body 0 e-idx) body))
                (base-exp (if e-idx
                              (string->number (substring body (+ e-idx 1) (string-length body)))
                              0))
                ;; Locate the decimal point in mtext. Should always be present
                ;; for inexact reals; fall back to end-of-string if not.
                (dot-idx (let loop ((i 0))
                           (cond ((>= i (string-length mtext)) (string-length mtext))
                                 ((char=? (string-ref mtext i) #\.) i)
                                 (else (loop (+ i 1))))))
                (digits-before (substring mtext 0 dot-idx))
                (digits-after (if (< dot-idx (string-length mtext))
                                  (substring mtext (+ dot-idx 1) (string-length mtext))
                                  ""))
                ;; Concatenated raw digit string and the implied decimal-point
                ;; position (= len(digits-before)).
                (all-digits (string-append digits-before digits-after))
                (point-pos (string-length digits-before))
                ;; Locate first nonzero digit. If the value is exactly zero
                ;; the outer cond already returned, so a nonzero digit exists.
                (first-nz (let loop ((i 0))
                            (cond ((>= i (string-length all-digits)) 0)
                                  ((char=? (string-ref all-digits i) #\0)
                                   (loop (+ i 1)))
                                  (else i))))
                ;; Trim trailing zeros from the digit stream.
                (last-nz (let loop ((i (- (string-length all-digits) 1)))
                           (cond ((<= i first-nz) first-nz)
                                 ((char=? (string-ref all-digits i) #\0)
                                  (loop (- i 1)))
                                 (else i))))
                (sig (substring all-digits first-nz (+ last-nz 1)))
                ;; Renormalized exponent: position of the first significant
                ;; digit relative to the original decimal point, plus any
                ;; exponent already encoded in the input string.
                (exp (+ base-exp (- point-pos first-nz 1)))
                (mantissa (string-append (substring sig 0 1)
                                         "."
                                         (if (> (string-length sig) 1)
                                             (substring sig 1 (string-length sig))
                                             "0")))
                (sign (if neg "-" "")))
           (string-append sign mantissa "e" (number->string exp))))
        (else (number->string x))))

    ;; Format a float32 value with minimum digits for unique representation
    ;; Simplified version without IEEE 754 bytevector round-trip
    (define (haskell-show-float32 x)
      (cond
        ((not (real? x)) "NaN")  ;; complex results from out-of-domain trig
        ((not (= x x)) "NaN")
        ((or (= x +inf.0) (= x -inf.0)) (if (> x 0) "Infinity" "-Infinity"))
        (else
      (let ((f32 (float32-approx x)))
        (define (f32-roundtrip v) (float32-approx v))
        (define (f32-equal? a b) (= (float32-approx a) (float32-approx b)))
        ;; Round to n significant digits and format
        (define (round-sig x n)
          (if (= x 0.0) 0.0
              (let* ((e (exact (floor (/ (log (safe-abs x)) (log 10)))))
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
                (or (< (safe-abs f32) 0.1) (>= (safe-abs f32) 1.0e7)))
           (let* ((e (exact (floor (/ (log (safe-abs f32)) (log 10)))))
                  (m (/ f32 (expt 10.0 e)))
                  (m-rounded (* 1.0 (/ (round (* (safe-abs m) 1e6)) 1e6)))
                  (adj-e (if (>= m-rounded 10.0) (+ e 1) e))
                  (adj-m (if (>= m-rounded 10.0) (/ m-rounded 10.0) m-rounded))
                  (sign (if (< f32 0) "-" "")))
             (string-append sign (number->string adj-m) "e" (number->string adj-e))))
          (else (try-digits 1)))))))

    ;; print_decimal :: Decimal -> String
    ;; Representation-faithful decimal rendering: per docs/specification/syntax.md #2.6, prints
    ;; in positional form when the adjusted exponent is in [-6, 21) and in exponent form
    ;; otherwise, with NO mandatory trailing ".0" on whole positional values ("42" not "42.0",
    ;; "0" not "0.0", printed per scale for zero: "0", "0.0", "0.00"), coefficient digits
    ;; (including trailing zeros) preserved exactly, and exponent form always has one digit
    ;; before the point PLUS a fractional part (a single-digit coefficient prints "1.0e-20",
    ;; not "1e-20" -- the point is a structural part of exponent form, not a coefficient digit).
    (define hydra_overlay_scheme_lib_literals_print_decimal
      (lambda (x)
        (let* ((coefficient (hydra-decimal-coefficient x))
               (scale (hydra-decimal-scale x))
               (neg (< coefficient 0))
               (digits (number->string (abs coefficient)))
               (sign (if neg "-" ""))
               (adjusted-exp (- (+ (string-length digits) -1) scale)))
          (if (and (>= adjusted-exp -6) (< adjusted-exp 21))
              ;; Positional form.
              (cond
                ((<= scale 0)
                 ;; Whole value: pad with trailing zeros (no fraction, no ".0").
                 (string-append sign digits (make-string (- scale) #\0)))
                ((< scale (string-length digits))
                 (string-append sign
                                (substring digits 0 (- (string-length digits) scale))
                                "."
                                (substring digits (- (string-length digits) scale) (string-length digits))))
                (else
                 (string-append sign "0." (make-string (- scale (string-length digits)) #\0) digits)))
              ;; Exponent form: always one digit before the point AND a fractional part.
              (let* ((lead-digit (substring digits 0 1))
                     (rest (substring digits 1 (string-length digits)))
                     (mantissa (if (> (string-length rest) 0)
                                   (string-append lead-digit "." rest)
                                   (string-append lead-digit ".0")))
                     (exp-sign (if (< adjusted-exp 0) "-" "+")))
                (string-append sign mantissa "e" exp-sign (number->string (abs adjusted-exp))))))))

    ;; print_bigint :: BigInteger -> String
    (define hydra_overlay_scheme_lib_literals_print_bigint
      (lambda (x)
        (number->string x)))

    ;; show_float :: Double -> String
    (define hydra_overlay_scheme_lib_literals_show_float
      (lambda (x)
        (haskell-show-float x)))

    ;; print_float32 :: Float -> String
    (define hydra_overlay_scheme_lib_literals_print_float32
      (lambda (x)
        (haskell-show-float32 x)))

    ;; print_float64 :: Double -> String
    (define hydra_overlay_scheme_lib_literals_print_float64
      (lambda (x)
        (haskell-show-float x)))

    ;; show_int :: Int -> String
    (define hydra_overlay_scheme_lib_literals_show_int
      (lambda (x)
        (number->string x)))

    ;; print_int8 :: Int8 -> String
    (define hydra_overlay_scheme_lib_literals_print_int8
      (lambda (x)
        (number->string x)))

    ;; print_int16 :: Int16 -> String
    (define hydra_overlay_scheme_lib_literals_print_int16
      (lambda (x)
        (number->string x)))

    ;; print_int32 :: Int32 -> String
    (define hydra_overlay_scheme_lib_literals_print_int32
      (lambda (x)
        (number->string x)))

    ;; print_int64 :: Int64 -> String
    (define hydra_overlay_scheme_lib_literals_print_int64
      (lambda (x)
        (number->string x)))

    ;; show_string :: String -> String  (Haskell-compatible quoted representation)
    (define hydra_overlay_scheme_lib_literals_print_string
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

    ;; show_uint :: Uint -> String
    (define hydra_overlay_scheme_lib_literals_show_uint
      (lambda (x)
        (number->string x)))

    ;; print_uint8 :: Uint8 -> String
    (define hydra_overlay_scheme_lib_literals_print_uint8
      (lambda (x)
        (number->string x)))

    ;; print_uint16 :: Uint16 -> String
    (define hydra_overlay_scheme_lib_literals_print_uint16
      (lambda (x)
        (number->string x)))

    ;; print_uint32 :: Uint32 -> String
    (define hydra_overlay_scheme_lib_literals_print_uint32
      (lambda (x)
        (number->string x)))

    ;; print_uint64 :: Uint64 -> String
    (define hydra_overlay_scheme_lib_literals_print_uint64
      (lambda (x)
        (number->string x)))

    ;; base64_to_binary :: String -> [Int8] (base64 decode)
    (define hydra_overlay_scheme_lib_literals_base64_to_binary
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

    ;; uint :: UintPrecision -> Uint -> Uint
    (define hydra_overlay_scheme_lib_literals_uint
      (lambda (precision)
        (lambda (x)
          x)))

    ;; uint8_to_bigint :: Uint8 -> BigInteger
    (define hydra_overlay_scheme_lib_literals_uint8_to_bigint
      (lambda (x)
        x))

    ;; uint16_to_bigint :: Uint16 -> BigInteger
    (define hydra_overlay_scheme_lib_literals_uint16_to_bigint
      (lambda (x)
        x))

    ;; uint32_to_bigint :: Uint32 -> BigInteger
    (define hydra_overlay_scheme_lib_literals_uint32_to_bigint
      (lambda (x)
        x))

    ;; binary_to_bytes :: Binary -> [Int8]
    (define hydra_overlay_scheme_lib_literals_binary_to_bytes
      (lambda (bs)
        (cond
          ((bytevector? bs)
           (let loop ((i 0) (acc '()))
             (if (>= i (bytevector-length bs))
                 (reverse acc)
                 (loop (+ i 1) (cons (bytevector-u8-ref bs i) acc)))))
          ((list? bs) bs)
          (else (vector->list bs)))))

    ;; read_boolean :: String -> Maybe Bool
    (define hydra_overlay_scheme_lib_literals_parse_boolean
      (lambda (s)
        (cond
          ((string=? s "true") (list 'given #t))
          ((string=? s "false") (list 'given #f))
          (else (list 'none)))))

    ;; parse_float64 :: String -> Maybe Float64
    (define hydra_overlay_scheme_lib_literals_parse_float64
      (lambda (s)
        (cond
          ((string=? s "NaN") (list 'given +nan.0))
          ((string=? s "Infinity") (list 'given +inf.0))
          ((string=? s "-Infinity") (list 'given -inf.0))
          (else (let ((n (string->number s)))
                  (if n
                      (list 'given (inexact n))
                      (list 'none)))))))

    ;; parse_int8 :: String -> Maybe Int8
    (define hydra_overlay_scheme_lib_literals_parse_int8
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n -128) (<= n 127))
              (list 'given (exact n))
              (list 'none)))))

    ;; parse_int16 :: String -> Maybe Int16
    (define hydra_overlay_scheme_lib_literals_parse_int16
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n -32768) (<= n 32767))
              (list 'given (exact n))
              (list 'none)))))

    ;; parse_int32 :: String -> Maybe Int32
    (define hydra_overlay_scheme_lib_literals_parse_int32
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n))
              (list 'given (exact n))
              (list 'none)))))

    ;; read_string :: String -> Maybe String
    (define hydra_overlay_scheme_lib_literals_parse_string
      (lambda (s)
        (let ((len (string-length s)))
          (if (and (>= len 2)
                   (char=? (string-ref s 0) #\")
                   (char=? (string-ref s (- len 1)) #\"))
              (let ((inner (substring s 1 (- len 1))))
                (let loop ((i 0) (acc '()))
                  (if (>= i (string-length inner))
                      (list 'given (list->string (reverse acc)))
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
              (list 'none)))))

    ;; parse_uint8 :: String -> Maybe Uint8
    (define hydra_overlay_scheme_lib_literals_parse_uint8
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n 0) (<= n 255))
              (list 'given (exact n))
              (list 'none)))))

    ;; parse_uint16 :: String -> Maybe Uint16
    (define hydra_overlay_scheme_lib_literals_parse_uint16
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n 0) (<= n 65535))
              (list 'given (exact n))
              (list 'none)))))

    ;; show_boolean :: Bool -> String
    (define hydra_overlay_scheme_lib_literals_print_boolean
      (lambda (x)
        (if x "true" "false")))

    ;; uint64_to_bigint :: Uint64 -> BigInteger
    (define hydra_overlay_scheme_lib_literals_uint64_to_bigint
      (lambda (x)
        x))))
