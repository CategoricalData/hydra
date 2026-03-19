(define-library (hydra lib literals)
  (import (scheme base) (scheme inexact)
          (scheme bytevector) ;; R7RS bytevector support + snap-to-float32
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
          hydra_lib_literals_read_float
          hydra_lib_literals_read_float32
          hydra_lib_literals_read_int
          hydra_lib_literals_read_int64
          hydra_lib_literals_read_uint
          hydra_lib_literals_read_uint32
          hydra_lib_literals_read_uint64
          hydra_lib_literals_show_bigfloat
          hydra_lib_literals_show_bigint
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
          hydra_lib_literals_binary_to_bytes
          hydra_lib_literals_read_boolean
          hydra_lib_literals_read_float64
          hydra_lib_literals_read_int8
          hydra_lib_literals_read_int16
          hydra_lib_literals_read_int32
          hydra_lib_literals_read_string
          hydra_lib_literals_read_uint8
          hydra_lib_literals_read_uint16
          hydra_lib_literals_show_boolean
          hydra_lib_literals_uint64_to_bigint)
  (begin

    ;; bigfloat_to_bigint :: Double -> BigInteger
    ;; Uses round (banker's rounding / round-half-even) matching Haskell's behavior
    (define hydra_lib_literals_bigfloat_to_bigint
      (lambda (x)
        (exact (round x))))

    ;; bigfloat_to_float :: Double -> Double  (identity in Scheme)
    (define hydra_lib_literals_bigfloat_to_float
      (lambda (x)
        (inexact x)))

    ;; Approximate IEEE 754 float32 by rounding to ~7 significant digits
    ;; Snap to IEEE 754 float32 precision
    (define (float32-approx x) (snap-to-float32 x))

    ;; bigfloat_to_float32 :: Double -> Float
    (define hydra_lib_literals_bigfloat_to_float32
      (lambda (x) (float32-approx x)))

    ;; bigfloat_to_float64 :: Double -> Double
    (define hydra_lib_literals_bigfloat_to_float64
      (lambda (x)
        (inexact x)))

    ;; bigint_to_bigfloat :: BigInteger -> Double
    (define hydra_lib_literals_bigint_to_bigfloat
      (lambda (x)
        (inexact x)))

    ;; bigint_to_int :: BigInteger -> Int  (identity in Scheme)
    (define hydra_lib_literals_bigint_to_int
      (lambda (x)
        x))

    ;; bigint_to_int8 :: BigInteger -> Int8
    (define hydra_lib_literals_bigint_to_int8
      (lambda (x)
        x))

    ;; bigint_to_int16 :: BigInteger -> Int16
    (define hydra_lib_literals_bigint_to_int16
      (lambda (x)
        x))

    ;; bigint_to_int32 :: BigInteger -> Int32
    (define hydra_lib_literals_bigint_to_int32
      (lambda (x)
        x))

    ;; bigint_to_int64 :: BigInteger -> Int64
    (define hydra_lib_literals_bigint_to_int64
      (lambda (x)
        x))

    ;; bigint_to_uint :: BigInteger -> Uint
    (define hydra_lib_literals_bigint_to_uint
      (lambda (x)
        x))

    ;; bigint_to_uint8 :: BigInteger -> Uint8
    (define hydra_lib_literals_bigint_to_uint8
      (lambda (x)
        x))

    ;; bigint_to_uint16 :: BigInteger -> Uint16
    (define hydra_lib_literals_bigint_to_uint16
      (lambda (x)
        x))

    ;; bigint_to_uint32 :: BigInteger -> Uint32
    (define hydra_lib_literals_bigint_to_uint32
      (lambda (x)
        x))

    ;; bigint_to_uint64 :: BigInteger -> Uint64
    (define hydra_lib_literals_bigint_to_uint64
      (lambda (x)
        x))

    ;; binary_to_string :: ByteString -> String (base64 encoding)
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

    ;; float :: FloatPrecision -> Double -> Double
    (define hydra_lib_literals_float
      (lambda (precision)
        (lambda (x)
          (inexact x))))

    ;; float32_to_bigfloat :: Float -> Double
    (define hydra_lib_literals_float32_to_bigfloat
      (lambda (x)
        (inexact x)))

    ;; float64_to_bigfloat :: Double -> Double
    (define hydra_lib_literals_float64_to_bigfloat
      (lambda (x)
        (inexact x)))

    ;; int :: IntPrecision -> Int -> Int
    (define hydra_lib_literals_int
      (lambda (precision)
        (lambda (x)
          x)))

    ;; int8_to_bigint :: Int8 -> BigInteger
    (define hydra_lib_literals_int8_to_bigint
      (lambda (x)
        x))

    ;; int16_to_bigint :: Int16 -> BigInteger
    (define hydra_lib_literals_int16_to_bigint
      (lambda (x)
        x))

    ;; int32_to_bigint :: Int32 -> BigInteger
    (define hydra_lib_literals_int32_to_bigint
      (lambda (x)
        x))

    ;; int64_to_bigint :: Int64 -> BigInteger
    (define hydra_lib_literals_int64_to_bigint
      (lambda (x)
        x))

    ;; read_bigfloat :: String -> Maybe Double
    (define hydra_lib_literals_read_bigfloat
      (lambda (s)
        (let ((n (string->number s)))
          (if n
              (list 'just (inexact n))
              (list 'nothing)))))

    ;; read_bigint :: String -> Maybe BigInteger
    (define hydra_lib_literals_read_bigint
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; read_float :: String -> Maybe Double
    (define hydra_lib_literals_read_float
      (lambda (s)
        (let ((n (string->number s)))
          (if n
              (list 'just (inexact n))
              (list 'nothing)))))

    ;; read_float32 :: String -> Maybe Float
    ;; Round to float32 precision
    (define hydra_lib_literals_read_float32
      (lambda (s)
        (let ((n (string->number s)))
          (if n
              (list 'just (float32-approx n))
              (list 'nothing)))))

    ;; read_int :: String -> Maybe Int
    (define hydra_lib_literals_read_int
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; read_int64 :: String -> Maybe Int64
    (define hydra_lib_literals_read_int64
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; read_uint :: String -> Maybe Uint
    (define hydra_lib_literals_read_uint
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n 0))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; read_uint32 :: String -> Maybe Uint32
    (define hydra_lib_literals_read_uint32
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n 0))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; read_uint64 :: String -> Maybe Uint64
    (define hydra_lib_literals_read_uint64
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n 0))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; Haskell-compatible float formatting helper
    (define (haskell-show-float x)
      (cond
        ((= x 0.0) "0.0")
        ((and (not (= x 0.0))
              (or (< (abs x) 0.1) (>= (abs x) 1.0e7)))
         ;; Scientific notation: Haskell uses e.g. "5.0e-2"
         (let* ((e (exact (floor (/ (log (abs x)) (log 10)))))
                (m (/ x (expt 10.0 e)))
                ;; Round mantissa to eliminate floating point noise
                (m-rounded (* 1.0 (/ (round (* (abs m) 1e14)) 1e14)))
                (adj-e (if (>= m-rounded 10.0) (+ e 1) e))
                (adj-m (if (>= m-rounded 10.0) (/ m-rounded 10.0) m-rounded))
                (sign (if (< x 0) "-" "")))
           (string-append sign (number->string adj-m) "e" (number->string adj-e))))
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

    ;; show_bigfloat :: Double -> String
    (define hydra_lib_literals_show_bigfloat
      (lambda (x)
        (haskell-show-float x)))

    ;; show_bigint :: BigInteger -> String
    (define hydra_lib_literals_show_bigint
      (lambda (x)
        (number->string x)))

    ;; show_float :: Double -> String
    (define hydra_lib_literals_show_float
      (lambda (x)
        (haskell-show-float x)))

    ;; show_float32 :: Float -> String
    (define hydra_lib_literals_show_float32
      (lambda (x)
        (haskell-show-float32 x)))

    ;; show_float64 :: Double -> String
    (define hydra_lib_literals_show_float64
      (lambda (x)
        (haskell-show-float x)))

    ;; show_int :: Int -> String
    (define hydra_lib_literals_show_int
      (lambda (x)
        (number->string x)))

    ;; show_int8 :: Int8 -> String
    (define hydra_lib_literals_show_int8
      (lambda (x)
        (number->string x)))

    ;; show_int16 :: Int16 -> String
    (define hydra_lib_literals_show_int16
      (lambda (x)
        (number->string x)))

    ;; show_int32 :: Int32 -> String
    (define hydra_lib_literals_show_int32
      (lambda (x)
        (number->string x)))

    ;; show_int64 :: Int64 -> String
    (define hydra_lib_literals_show_int64
      (lambda (x)
        (number->string x)))

    ;; show_string :: String -> String  (Haskell-compatible quoted representation)
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

    ;; show_uint :: Uint -> String
    (define hydra_lib_literals_show_uint
      (lambda (x)
        (number->string x)))

    ;; show_uint8 :: Uint8 -> String
    (define hydra_lib_literals_show_uint8
      (lambda (x)
        (number->string x)))

    ;; show_uint16 :: Uint16 -> String
    (define hydra_lib_literals_show_uint16
      (lambda (x)
        (number->string x)))

    ;; show_uint32 :: Uint32 -> String
    (define hydra_lib_literals_show_uint32
      (lambda (x)
        (number->string x)))

    ;; show_uint64 :: Uint64 -> String
    (define hydra_lib_literals_show_uint64
      (lambda (x)
        (number->string x)))

    ;; string_to_binary :: String -> [Int8] (base64 decode)
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

    ;; uint :: UintPrecision -> Uint -> Uint
    (define hydra_lib_literals_uint
      (lambda (precision)
        (lambda (x)
          x)))

    ;; uint8_to_bigint :: Uint8 -> BigInteger
    (define hydra_lib_literals_uint8_to_bigint
      (lambda (x)
        x))

    ;; uint16_to_bigint :: Uint16 -> BigInteger
    (define hydra_lib_literals_uint16_to_bigint
      (lambda (x)
        x))

    ;; uint32_to_bigint :: Uint32 -> BigInteger
    (define hydra_lib_literals_uint32_to_bigint
      (lambda (x)
        x))

    ;; binary_to_bytes :: Binary -> [Int8]
    (define hydra_lib_literals_binary_to_bytes
      (lambda (bs)
        (let loop ((i 0) (acc '()))
          (if (>= i (bytevector-length bs))
              (reverse acc)
              (loop (+ i 1) (cons (bytevector-u8-ref bs i) acc))))))

    ;; read_boolean :: String -> Maybe Bool
    (define hydra_lib_literals_read_boolean
      (lambda (s)
        (cond
          ((string=? s "true") (list 'just #t))
          ((string=? s "false") (list 'just #f))
          (else (list 'nothing)))))

    ;; read_float64 :: String -> Maybe Float64
    (define hydra_lib_literals_read_float64
      (lambda (s)
        (let ((n (string->number s)))
          (if n
              (list 'just (inexact n))
              (list 'nothing)))))

    ;; read_int8 :: String -> Maybe Int8
    (define hydra_lib_literals_read_int8
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n -128) (<= n 127))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; read_int16 :: String -> Maybe Int16
    (define hydra_lib_literals_read_int16
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n -32768) (<= n 32767))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; read_int32 :: String -> Maybe Int32
    (define hydra_lib_literals_read_int32
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; read_string :: String -> Maybe String
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

    ;; read_uint8 :: String -> Maybe Uint8
    (define hydra_lib_literals_read_uint8
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n 0) (<= n 255))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; read_uint16 :: String -> Maybe Uint16
    (define hydra_lib_literals_read_uint16
      (lambda (s)
        (let ((n (string->number s)))
          (if (and n (integer? n) (>= n 0) (<= n 65535))
              (list 'just (exact n))
              (list 'nothing)))))

    ;; show_boolean :: Bool -> String
    (define hydra_lib_literals_show_boolean
      (lambda (x)
        (if x "true" "false")))

    ;; uint64_to_bigint :: Uint64 -> BigInteger
    (define hydra_lib_literals_uint64_to_bigint
      (lambda (x)
        x))))
