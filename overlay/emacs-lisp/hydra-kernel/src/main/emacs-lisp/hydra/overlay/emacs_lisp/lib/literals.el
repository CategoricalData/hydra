;;; literals.el --- Hydra literal conversion primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; A scale-preserving arbitrary-precision decimal: the numeric value is
;; (coefficient * 10^-scale), represented as a (coefficient . scale) cons pair of native
;; Emacs Lisp bignums (Emacs 27+, GMP-backed). Mirrors java.math.BigDecimal's
;; (unscaledValue, scale) convention, so "1.10" is (110 . 2) and "1.1" is (11 . 1) --
;; distinct values per the kernel spec (docs/specification/ordering-and-equality.md:
;; 1.1 != 1.10). scale is always >= 0.
(defun hydra-make-decimal (coefficient scale) (cons coefficient scale))
(defun hydra-decimal-coefficient (d) (car d))
(defun hydra-decimal-scale (d) (cdr d))

;; bigint_to_decimal :: BigInteger -> Decimal
;; Exact: a bigint is a decimal with scale 0.
(defvar hydra_overlay_emacs_lisp_lib_literals_bigint_to_decimal
  (lambda (x)
    (hydra-make-decimal x 0)))

;; bigint_to_int :: BigInteger -> Int  (identity)
(defvar hydra_overlay_emacs_lisp_lib_literals_bigint_to_int
  (lambda (x) x))

;; bigint_to_int8 :: BigInteger -> Int8
;; Two's-complement narrowing (#745): hydra--wrap-int (math.el, loaded after this
;; file, but not called until test-run time -- a defvar'd lambda body isn't
;; evaluated at load time).
(defvar hydra_overlay_emacs_lisp_lib_literals_bigint_to_int8
  (lambda (x) (hydra--wrap-int :int8 x)))

;; bigint_to_int16 :: BigInteger -> Int16
(defvar hydra_overlay_emacs_lisp_lib_literals_bigint_to_int16
  (lambda (x) (hydra--wrap-int :int16 x)))

;; bigint_to_int32 :: BigInteger -> Int32
(defvar hydra_overlay_emacs_lisp_lib_literals_bigint_to_int32
  (lambda (x) (hydra--wrap-int :int32 x)))

;; bigint_to_int64 :: BigInteger -> Int64
(defvar hydra_overlay_emacs_lisp_lib_literals_bigint_to_int64
  (lambda (x) (hydra--wrap-int :int64 x)))

;; bigint_to_uint :: BigInteger -> Uint
(defvar hydra_overlay_emacs_lisp_lib_literals_bigint_to_uint
  (lambda (x) x))

;; bigint_to_uint8 :: BigInteger -> Uint8
(defvar hydra_overlay_emacs_lisp_lib_literals_bigint_to_uint8
  (lambda (x) x))

;; bigint_to_uint16 :: BigInteger -> Uint16
(defvar hydra_overlay_emacs_lisp_lib_literals_bigint_to_uint16
  (lambda (x) x))

;; bigint_to_uint32 :: BigInteger -> Uint32
(defvar hydra_overlay_emacs_lisp_lib_literals_bigint_to_uint32
  (lambda (x) x))

;; bigint_to_uint64 :: BigInteger -> Uint64
(defvar hydra_overlay_emacs_lisp_lib_literals_bigint_to_uint64
  (lambda (x) x))

;; binary_to_base64 :: ByteString -> String (base64 encoding)
(defvar hydra_overlay_emacs_lisp_lib_literals_binary_to_base64
  (let ((b64-chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (lambda (bv)
      (let* ((bytes (vconcat bv))
             (len (length bytes))
             (result nil))
        (let ((i 0))
          (while (< i len)
            (let* ((b0 (aref bytes i))
                   (b1 (if (< (1+ i) len) (aref bytes (1+ i)) 0))
                   (b2 (if (< (+ i 2) len) (aref bytes (+ i 2)) 0))
                   (remaining (- len i)))
              (push (aref b64-chars (ash b0 -2)) result)
              (push (aref b64-chars (logior (ash (logand b0 3) 4) (ash b1 -4))) result)
              (if (>= remaining 2)
                  (push (aref b64-chars (logior (ash (logand b1 #xF) 2) (ash b2 -6))) result)
                  (push ?= result))
              (if (>= remaining 3)
                  (push (aref b64-chars (logand b2 #x3F)) result)
                  (push ?= result)))
            (setq i (+ i 3))))
        (apply #'string (nreverse result))))))

;; Convert a Decimal to an exact numerator/denominator pair (coefficient, 10^scale), for use
;; as an intermediate in float conversions. Emacs Lisp has no native exact-rational type, so
;; float conversion divides the two bignums directly via `/` with a float argument.
(defun hydra-decimal-to-float-1 (x)
  (let ((coefficient (hydra-decimal-coefficient x))
        (scale (hydra-decimal-scale x)))
    (if (<= scale 0)
        (float (* coefficient (expt 10 (- scale))))
        (/ (float coefficient) (expt 10 scale)))))

;; decimal_to_bigint :: Decimal -> BigInteger
;; Rounds to the nearest integer, ties to even (banker's rounding) -- matches the
;; Python/Java reference hosts' behavior (e.g. 42.7 rounds to 43, 2.5 rounds to 2).
;; Emacs Lisp's own 2-argument ROUND already implements round-half-to-even on bignums.
(defvar hydra_overlay_emacs_lisp_lib_literals_decimal_to_bigint
  (lambda (x)
    (let ((coefficient (hydra-decimal-coefficient x))
          (scale (hydra-decimal-scale x)))
      (if (<= scale 0)
          (* coefficient (expt 10 (- scale)))
          (round coefficient (expt 10 scale))))))

;; decimal_to_float32 :: Decimal -> Float
;; Exact division rounded to the nearest double, then narrowed to float32 precision.
(defvar hydra_overlay_emacs_lisp_lib_literals_decimal_to_float32
  (lambda (x)
    (round-to-float32 (hydra-decimal-to-float-1 x))))

;; decimal_to_float64 :: Decimal -> Double
;; Exact division, rounding to the nearest representable double.
(defvar hydra_overlay_emacs_lisp_lib_literals_decimal_to_float64
  (lambda (x)
    (hydra-decimal-to-float-1 x)))

;; float :: FloatPrecision -> Double -> Double
(defvar hydra_overlay_emacs_lisp_lib_literals_float
  (lambda (_precision)
    (lambda (x)
      (float x))))

;; float32_to_decimal :: Float -> Decimal
;; Exact: every IEEE 754 float has a finite decimal expansion. Derived from the shortest
;; round-tripping digit string, matching printFloat32's own shortest-round-trip convention.
;; hydra-decimal-from-float is defined further down in this file (near
;; hydra--decimal-digits-and-exponent); forward reference is fine since these are all
;; top-level defvar/defun forms loaded together.
(defvar hydra_overlay_emacs_lisp_lib_literals_float32_to_decimal
  (lambda (x)
    (hydra-decimal-from-float (float x))))

;; float32_to_float64 :: Float -> Double
;; EL has a single float type; widening is identity.
(defvar hydra_overlay_emacs_lisp_lib_literals_float32_to_float64
  (lambda (x)
    (float x)))

;; float64_to_decimal :: Double -> Decimal
;; Exact: every IEEE 754 float has a finite decimal expansion. Derived from the shortest
;; round-tripping digit string, matching printFloat64's own shortest-round-trip convention.
(defvar hydra_overlay_emacs_lisp_lib_literals_float64_to_decimal
  (lambda (x)
    (hydra-decimal-from-float x)))

;; float64_to_float32 :: Double -> Float
;; EL has only one float type; approximate float32 by rounding to single-precision.
(defvar hydra_overlay_emacs_lisp_lib_literals_float64_to_float32
  (lambda (x)
    (round-to-float32 (float x))))

;; int :: IntPrecision -> Int -> Int
(defvar hydra_overlay_emacs_lisp_lib_literals_int
  (lambda (_precision)
    (lambda (x) x)))

;; int8_to_bigint :: Int8 -> BigInteger
(defvar hydra_overlay_emacs_lisp_lib_literals_int8_to_bigint
  (lambda (x) x))

;; int16_to_bigint :: Int16 -> BigInteger
(defvar hydra_overlay_emacs_lisp_lib_literals_int16_to_bigint
  (lambda (x) x))

;; int32_to_bigint :: Int32 -> BigInteger
(defvar hydra_overlay_emacs_lisp_lib_literals_int32_to_bigint
  (lambda (x) x))

;; int64_to_bigint :: Int64 -> BigInteger
(defvar hydra_overlay_emacs_lisp_lib_literals_int64_to_bigint
  (lambda (x) x))

;; Parse the JSON number grammar (docs/specification/syntax.md #2.6) into a scale-preserving
;; Decimal: an optional sign, integer digits, an optional fraction part, and an optional
;; exponent part. Scale-preserving means "1.10" and "1.1" parse to distinct values (scale 2
;; vs scale 1). Returns nil on parse failure. Emacs Lisp has no `digit-char-p`-equivalent
;; (not even in cl-lib), so digits are recognized via a manual character-range check.
(defun hydra--digit-char-p (c) (and (>= c ?0) (<= c ?9)))

(defun hydra-parse-decimal-1 (s)
  (let ((len (length s))
        (i 0)
        (neg nil))
    (catch 'hydra-parse-fail
      (when (and (< i len) (= (aref s i) ?-))
        (setq neg t)
        (setq i (1+ i)))
      (let ((int-start i))
        (while (and (< i len) (hydra--digit-char-p (aref s i)))
          (setq i (1+ i)))
        (when (= i int-start) (throw 'hydra-parse-fail nil))
        (let ((int-digits (substring s int-start i))
              (frac-digits ""))
          (when (and (< i len) (= (aref s i) ?.))
            (setq i (1+ i))
            (let ((frac-start i))
              (while (and (< i len) (hydra--digit-char-p (aref s i)))
                (setq i (1+ i)))
              (when (= i frac-start) (throw 'hydra-parse-fail nil))
              (setq frac-digits (substring s frac-start i))))
          (let ((exp-value 0))
            (when (and (< i len) (memq (aref s i) '(?e ?E)))
              (setq i (1+ i))
              (let ((exp-neg nil) (exp-start i))
                (when (and (< i len) (memq (aref s i) '(?+ ?-)))
                  (setq exp-neg (= (aref s i) ?-))
                  (setq i (1+ i))
                  (setq exp-start i))
                (while (and (< i len) (hydra--digit-char-p (aref s i)))
                  (setq i (1+ i)))
                (when (= i exp-start) (throw 'hydra-parse-fail nil))
                (let ((v (string-to-number (substring s exp-start i))))
                  (setq exp-value (if exp-neg (- v) v)))))
            (when (/= i len) (throw 'hydra-parse-fail nil))
            (let* ((digits (concat int-digits frac-digits))
                   (coefficient (if (string= digits "") 0 (string-to-number digits)))
                   (coefficient (if neg (- coefficient) coefficient))
                   (scale (- (length frac-digits) exp-value)))
              (if (< scale 0)
                  (hydra-make-decimal (* coefficient (expt 10 (- scale))) 0)
                  (hydra-make-decimal coefficient scale)))))))))

;; parse_decimal :: String -> Maybe Decimal
(defvar hydra_overlay_emacs_lisp_lib_literals_parse_decimal
  (lambda (s)
    (let ((d (hydra-parse-decimal-1 s)))
      (if d (list :given d) (list :none)))))

;; parse_bigint :: String -> Maybe BigInteger
;; Uses read-from-string to handle arbitrarily large integers (Emacs 27+ bignum support)
(defvar hydra_overlay_emacs_lisp_lib_literals_parse_bigint
  (lambda (s)
    (condition-case nil
        (if (string-match-p "^-?[0-9]+$" s)
            (let ((n (car (read-from-string s))))
              (if (integerp n)
                  (list :given n)
                (list :none)))
          (list :none))
      (error (list :none)))))

;; read_float :: String -> Maybe Double
(defvar hydra_overlay_emacs_lisp_lib_literals_read_float
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (numberp n) (or (not (= n 0)) (string= s "0") (string= s "0.0")))
              (list :given (float n))
              (list :none)))
      (error (list :none)))))

;; parse_float32 :: String -> Maybe Float
(defvar hydra_overlay_emacs_lisp_lib_literals_parse_float32
  (lambda (s)
    (cond
      ((string= s "NaN") (list :given 0.0e+NaN))
      ((string= s "Infinity") (list :given 1.0e+INF))
      ((string= s "-Infinity") (list :given -1.0e+INF))
      (t (condition-case nil
             (let ((n (string-to-number s)))
               (if (and (numberp n) (or (not (= n 0)) (string= s "0") (string= s "0.0") (string= s "-0") (string= s "-0.0")))
                   (list :given (round-to-float32 (float n)))
                   (list :none)))
           (error (list :none)))))))

;; read_int :: String -> Maybe Int
(defvar hydra_overlay_emacs_lisp_lib_literals_read_int
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (string= (number-to-string n) s))
              (list :given n)
              (list :none)))
      (error (list :none)))))

;; parse_int64 :: String -> Maybe Int64
(defvar hydra_overlay_emacs_lisp_lib_literals_parse_int64
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (string= (number-to-string n) s))
              (list :given n)
              (list :none)))
      (error (list :none)))))

;; read_uint :: String -> Maybe Uint
(defvar hydra_overlay_emacs_lisp_lib_literals_read_uint
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (>= n 0) (string= (number-to-string n) s))
              (list :given n)
              (list :none)))
      (error (list :none)))))

;; parse_uint32 :: String -> Maybe Uint32
(defvar hydra_overlay_emacs_lisp_lib_literals_parse_uint32
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (>= n 0) (<= n 4294967295) (string= (number-to-string n) s))
              (list :given n)
              (list :none)))
      (error (list :none)))))

;; parse_uint64 :: String -> Maybe Uint64
(defvar hydra_overlay_emacs_lisp_lib_literals_parse_uint64
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (>= n 0) (<= n 18446744073709551615) (string= (number-to-string n) s))
              (list :given n)
              (list :none)))
      (error (list :none)))))

;; Helper for Haskell-compatible float show
(defun hydra--literals-infinitep (x)
  "Return non-nil if X is a positive or negative infinity."
  (and (numberp x)
       (not (isnan x))
       (or (> x 1.7976931348623157e308)
           (< x -1.7976931348623157e308))))

(defun haskell-show-float (x)
  "Format a double-float in Haskell's show style."
  (cond
    ((isnan x) "NaN")
    ((hydra--literals-infinitep x) (if (> x 0) "Infinity" "-Infinity"))
    ((= x 0.0) (if (< (copysign 1.0 x) 0) "-0.0" "0.0"))
    ((and (/= x 0.0)
          (or (< (abs x) 0.1) (>= (abs x) 1.0e7)))
     ;; Scientific notation
     (let* ((exp-val (floor (log (abs x) 10.0)))
            (mantissa (/ x (expt 10.0 exp-val)))
            (adj-exp (if (>= (abs mantissa) 10.0) (1+ exp-val) exp-val))
            (adj-mantissa (if (>= (abs mantissa) 10.0) (/ mantissa 10.0) mantissa)))
       (format "%s%se%d"
               (if (< adj-mantissa 0) "-" "")
               (haskell-show-float-simple (abs adj-mantissa))
               adj-exp)))
    (t (haskell-show-float-simple x))))

(defun haskell-show-float-simple (x)
  "Format a float without scientific notation, ensuring decimal point."
  (let ((s (format "%s" x)))
    ;; Ensure there's a decimal point
    (if (cl-search "." s)
        s
        (concat s ".0"))))

(defun hydra--decimal-digits-and-exponent (x)
  "Derive (significant-digit-string . adjusted-exponent) for a non-zero
float X, using the implementation's own correctly-rounded printer (avoids
the ulp error a log/exp derivation can introduce). The adjusted exponent
is the power of 10 of the leading significant digit (e.g. 3 for 1234.0,
-2 for 0.01), matching printDecimal's own `a` (Literals.hs)."
  (let* ((s (format "%s" (abs x)))
         (epos (cl-position ?e s))
         (mtext (if epos (substring s 0 epos) s))
         (base-exp (if epos (string-to-number (substring s (1+ epos))) 0))
         (dot (or (cl-position ?. mtext) (length mtext)))
         (digits-before (substring mtext 0 dot))
         (digits-after (if (< dot (length mtext)) (substring mtext (1+ dot)) ""))
         (all-digits (concat digits-before digits-after))
         (point-pos (length digits-before))
         (first-nz (or (cl-position-if (lambda (c) (/= c ?0)) all-digits) 0))
         (last-nz (let ((i (1- (length all-digits))))
                     (while (and (> i first-nz) (= (aref all-digits i) ?0))
                       (setq i (1- i)))
                     i))
         (sig (substring all-digits first-nz (1+ last-nz)))
         (e (+ base-exp (- point-pos first-nz 1))))
    (cons sig e)))

;; Build a Decimal from a float's shortest round-tripping significant-digit string and
;; adjusted exponent (via hydra--decimal-digits-and-exponent above). E.g. digits "314" and
;; adjusted-exponent 0 -> the value 3.14, represented as coefficient 314, scale
;; (length digits - 1 - adjusted-exponent) = 2.
(defun hydra-decimal-from-float (x)
  (cond
    ((not (numberp x)) (hydra-make-decimal 0 0))
    ((hydra--literals-infinitep x) (hydra-make-decimal 0 0))
    ((/= x x) (hydra-make-decimal 0 0)) ;; NaN is the only value not equal to itself
    ((= x 0.0) (hydra-make-decimal 0 0))
    (t (let* ((digex (hydra--decimal-digits-and-exponent x))
              (sig (car digex))
              (adjusted-exp (cdr digex))
              (coefficient (string-to-number sig))
              (coefficient (if (< x 0) (- coefficient) coefficient))
              (scale (- (length sig) 1 adjusted-exp)))
         (if (< scale 0)
             (hydra-make-decimal (* coefficient (expt 10 (- scale))) 0)
             (hydra-make-decimal coefficient scale))))))

;; print_decimal :: Decimal -> String
;; Representation-faithful decimal rendering: per docs/specification/syntax.md #2.6, prints
;; in positional form when the adjusted exponent is in [-6, 21) and in exponent form
;; otherwise, with NO mandatory trailing ".0" on whole positional values ("42" not "42.0",
;; "0" not "0.0", printed per scale for zero: "0", "0.0", "0.00"), coefficient digits
;; (including trailing zeros) preserved exactly, and exponent form always has one digit
;; before the point PLUS a fractional part (a single-digit coefficient prints "1.0e-20", not
;; "1e-20" -- the point is a structural part of exponent form, not a coefficient digit).
(defvar hydra_overlay_emacs_lisp_lib_literals_print_decimal
  (lambda (x)
    (let* ((coefficient (hydra-decimal-coefficient x))
           (scale (hydra-decimal-scale x))
           (neg (< coefficient 0))
           (digits (number-to-string (abs coefficient)))
           (sign (if neg "-" ""))
           (adjusted-exp (- (+ (length digits) -1) scale)))
      (if (and (>= adjusted-exp -6) (< adjusted-exp 21))
          ;; Positional form.
          (cond
            ((<= scale 0)
             ;; Whole value: pad with trailing zeros (no fraction, no ".0").
             (concat sign digits (make-string (- scale) ?0)))
            ((< scale (length digits))
             (format "%s%s.%s" sign
                     (substring digits 0 (- (length digits) scale))
                     (substring digits (- (length digits) scale))))
            (t (format "%s0.%s%s" sign (make-string (- scale (length digits)) ?0) digits)))
          ;; Exponent form: always one digit before the point AND a fractional part.
          (let* ((lead-digit (substring digits 0 1))
                 (rest (substring digits 1))
                 (mantissa (if (> (length rest) 0)
                               (format "%s.%s" lead-digit rest)
                               (format "%s.0" lead-digit)))
                 (exp-sign (if (< adjusted-exp 0) "-" "+")))
            (format "%s%se%s%d" sign mantissa exp-sign (abs adjusted-exp)))))))

;; print_bigint :: BigInteger -> String
(defvar hydra_overlay_emacs_lisp_lib_literals_print_bigint
  (lambda (x)
    (number-to-string x)))

;; show_float :: Double -> String
(defvar hydra_overlay_emacs_lisp_lib_literals_show_float
  (lambda (x)
    (haskell-show-float (float x))))

(defun round-to-float32 (x)
  "Snap a double to IEEE 754 float32 precision (24-bit mantissa)."
  (cond ((isnan x) x)
        ((hydra--literals-infinitep x) x)
        ((= x 0.0) 0.0)
        (t (let* ((sign (if (< x 0) -1.0 1.0))
                  (ax (abs x))
                  (e (floor (log ax 2.0)))
                  (scale (expt 2.0 (- 23 e)))
                  (mantissa (round (* ax scale))))
             (* sign (/ mantissa scale))))))

(defun haskell-show-float32 (x)
  "Format a float32 value with minimum digits for unique representation."
  (cond
   ((isnan x) "NaN")
   ((hydra--literals-infinitep x) (if (> x 0) "Infinity" "-Infinity"))
   (t
  (let ((f32 (round-to-float32 (float x))))
    (cond
      ((= f32 0.0) "0.0")
      ((and (/= f32 0.0)
            (or (< (abs f32) 0.1) (>= (abs f32) 1.0e7)))
       ;; Scientific notation
       (let* ((exp-val (floor (log (abs f32) 10.0)))
              (mantissa (/ f32 (expt 10.0 exp-val)))
              (adj-exp (if (>= (abs mantissa) 10.0) (1+ exp-val) exp-val))
              (adj-mantissa (if (>= (abs mantissa) 10.0) (/ mantissa 10.0) mantissa))
              (sign (if (< f32 0) "-" "")))
         ;; Find minimum digits for mantissa
         (cl-loop for n from 1 to 9
                  for rounded = (/ (round (* (abs adj-mantissa) (expt 10.0 (1- n)))) (expt 10.0 (1- n)))
                  when (= (round-to-float32 (* rounded (expt 10.0 adj-exp)))
                          (round-to-float32 (* (abs adj-mantissa) (expt 10.0 adj-exp))))
                  return (format "%s%se%d" sign (haskell-show-float-simple (* 1.0 rounded)) adj-exp)
                  finally return (format "%s%se%d" sign (haskell-show-float-simple (abs adj-mantissa)) adj-exp))))
      (t
       ;; Normal range: find minimum digits
       (cl-loop for n from 1 to 9
                for factor = (expt 10.0 n)
                for rounded = (/ (round (* f32 factor)) factor)
                when (= (round-to-float32 rounded) (round-to-float32 f32))
                return (haskell-show-float-simple (* 1.0 rounded))
                finally return (haskell-show-float-simple f32))))))))

;; print_float32 :: Float -> String
(defvar hydra_overlay_emacs_lisp_lib_literals_print_float32
  (lambda (x)
    (haskell-show-float32 x)))

;; print_float64 :: Double -> String
(defvar hydra_overlay_emacs_lisp_lib_literals_print_float64
  (lambda (x)
    (haskell-show-float (float x))))

;; show_int :: Int -> String
(defvar hydra_overlay_emacs_lisp_lib_literals_show_int
  (lambda (x)
    (number-to-string x)))

;; print_int8 :: Int8 -> String
(defvar hydra_overlay_emacs_lisp_lib_literals_print_int8
  (lambda (x)
    (number-to-string x)))

;; print_int16 :: Int16 -> String
(defvar hydra_overlay_emacs_lisp_lib_literals_print_int16
  (lambda (x)
    (number-to-string x)))

;; print_int32 :: Int32 -> String
(defvar hydra_overlay_emacs_lisp_lib_literals_print_int32
  (lambda (x)
    (number-to-string x)))

;; print_int64 :: Int64 -> String
(defvar hydra_overlay_emacs_lisp_lib_literals_print_int64
  (lambda (x)
    (number-to-string x)))

;; print_string :: String -> String  (Haskell-compatible quoted representation)
(defvar hydra_overlay_emacs_lisp_lib_literals_print_string
  (lambda (s)
    (let* ((ms (if (multibyte-string-p s) s (decode-coding-string s 'utf-8-unix)))
           (acc (list ?\")))
      (dotimes (idx (length ms))
        (let* ((c (aref ms idx))
               (code c))
          (cond
            ((= c ?\\) (push ?\\ acc) (push ?\\ acc))
            ((= c ?\") (push ?\\ acc) (push ?\" acc))
            ((= c ?\n) (push ?\\ acc) (push ?n acc))
            ((= c ?\r) (push ?\\ acc) (push ?r acc))
            ((= c ?\t) (push ?\\ acc) (push ?t acc))
            ;; Haskell control char names
            ((= code 0) (dolist (ch (append "\\NUL" nil)) (push ch acc)))
            ((= code 7) (push ?\\ acc) (push ?a acc))
            ((= code 8) (push ?\\ acc) (push ?b acc))
            ((= code 11) (push ?\\ acc) (push ?v acc))
            ((= code 12) (push ?\\ acc) (push ?f acc))
            ((= code 127) (dolist (ch (append "\\DEL" nil)) (push ch acc)))
            ;; Non-ASCII: use Haskell decimal escape
            ((> code 127)
             (push ?\\ acc)
             (dolist (ch (append (number-to-string code) nil))
               (push ch acc)))
            ;; Other control chars (1-6, 14-31): use decimal escape
            ((< code 32)
             (push ?\\ acc)
             (dolist (ch (append (number-to-string code) nil))
               (push ch acc)))
            (t (push c acc)))))
      (push ?\" acc)
      (apply #'string (nreverse acc)))))

;; show_uint :: Uint -> String
(defvar hydra_overlay_emacs_lisp_lib_literals_show_uint
  (lambda (x)
    (number-to-string x)))

;; print_uint8 :: Uint8 -> String
(defvar hydra_overlay_emacs_lisp_lib_literals_print_uint8
  (lambda (x)
    (number-to-string x)))

;; print_uint16 :: Uint16 -> String
(defvar hydra_overlay_emacs_lisp_lib_literals_print_uint16
  (lambda (x)
    (number-to-string x)))

;; print_uint32 :: Uint32 -> String
(defvar hydra_overlay_emacs_lisp_lib_literals_print_uint32
  (lambda (x)
    (number-to-string x)))

;; print_uint64 :: Uint64 -> String
(defvar hydra_overlay_emacs_lisp_lib_literals_print_uint64
  (lambda (x)
    (number-to-string x)))

;; binary_to_bytes :: Binary -> [Int8]
(defvar hydra_overlay_emacs_lisp_lib_literals_binary_to_bytes
  (lambda (bs)
    (mapcar (lambda (b) (logand b #xFF)) bs)))

;; parse_boolean :: String -> Maybe Bool
(defvar hydra_overlay_emacs_lisp_lib_literals_parse_boolean
  (lambda (s)
    (cond
      ((string= s "true") (list :given t))
      ((string= s "false") (list :given nil))
      (t (list :none)))))

;; read_string :: String -> Maybe String
;; Haskell semantics: reads a quoted string literal, returns Nothing for unquoted
(defvar hydra_overlay_emacs_lisp_lib_literals_parse_string
  (lambda (s)
    (if (and (>= (length s) 2)
             (= (aref s 0) ?\")
             (= (aref s (1- (length s))) ?\"))
        (let* ((inner (substring s 1 (1- (length s))))
               (result nil)
               (i 0)
               (len (length inner)))
          (while (< i len)
            (if (and (= (aref inner i) ?\\) (< (1+ i) len))
                (let ((c (aref inner (1+ i))))
                  (cond
                    ((= c ?\\) (push ?\\ result))
                    ((= c ?\") (push ?\" result))
                    ((= c ?n) (push ?\n result))
                    ((= c ?t) (push ?\t result))
                    ((= c ?r) (push ?\r result))
                    (t (push ?\\ result)
                       (push c result)))
                  (setq i (+ i 2)))
                (progn
                  (push (aref inner i) result)
                  (setq i (1+ i)))))
          (list :given (apply #'string (nreverse result))))
        (list :none))))

;; print_boolean :: Bool -> String
(defvar hydra_overlay_emacs_lisp_lib_literals_print_boolean
  (lambda (x)
    (if x "true" "false")))

;; base64_to_binary :: String -> ByteString (base64 decoding)
(defvar hydra_overlay_emacs_lisp_lib_literals_base64_to_binary
  (let ((b64-decode (make-vector 128 -1)))
    (let ((i 0))
      (dolist (c (append "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" nil))
        (aset b64-decode c i)
        (setq i (1+ i))))
    (lambda (s)
      (let* ((len (length s))
             (pad (cond ((and (> len 0) (= (aref s (1- len)) ?=))
                         (if (and (> len 1) (= (aref s (- len 2)) ?=)) 2 1))
                        (t 0)))
             (in-len (- len pad))
             (result nil)
             (i 0))
        (while (< i in-len)
          (let* ((a (aref b64-decode (aref s i)))
                 (b (if (< (1+ i) in-len) (aref b64-decode (aref s (1+ i))) 0))
                 (c (if (< (+ i 2) in-len) (aref b64-decode (aref s (+ i 2))) 0))
                 (d (if (< (+ i 3) in-len) (aref b64-decode (aref s (+ i 3))) 0)))
            (push (logior (ash a 2) (ash b -4)) result)
            (when (< (+ i 2) in-len)
              (push (logand (logior (ash b 4) (ash c -2)) #xFF) result))
            (when (< (+ i 3) in-len)
              (push (logand (logior (ash c 6) d) #xFF) result)))
          (setq i (+ i 4)))
        (nreverse result)))))

;; uint :: UintPrecision -> Uint -> Uint
(defvar hydra_overlay_emacs_lisp_lib_literals_uint
  (lambda (_precision)
    (lambda (x) x)))

;; uint8_to_bigint :: Uint8 -> BigInteger
(defvar hydra_overlay_emacs_lisp_lib_literals_uint8_to_bigint
  (lambda (x) x))

;; uint16_to_bigint :: Uint16 -> BigInteger
(defvar hydra_overlay_emacs_lisp_lib_literals_uint16_to_bigint
  (lambda (x) x))

;; uint32_to_bigint :: Uint32 -> BigInteger
(defvar hydra_overlay_emacs_lisp_lib_literals_uint32_to_bigint
  (lambda (x) x))

;; uint64_to_bigint :: Uint64 -> BigInteger
(defvar hydra_overlay_emacs_lisp_lib_literals_uint64_to_bigint
  (lambda (x) x))

;; parse_float64 :: String -> Maybe Float64
(defvar hydra_overlay_emacs_lisp_lib_literals_parse_float64
  (lambda (s)
    (cond
      ((string= s "NaN") (list :given 0.0e+NaN))
      ((string= s "Infinity") (list :given 1.0e+INF))
      ((string= s "-Infinity") (list :given -1.0e+INF))
      (t (condition-case nil
             (let ((n (string-to-number s)))
               (if (and (numberp n) (or (not (= n 0)) (string= s "0") (string= s "0.0") (string= s "-0") (string= s "-0.0")))
                   (list :given (float n))
                   (list :none)))
           (error (list :none)))))))

;; parse_int8 :: String -> Maybe Int8
(defvar hydra_overlay_emacs_lisp_lib_literals_parse_int8
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (>= n -128) (<= n 127) (string= (number-to-string n) s))
              (list :given n)
              (list :none)))
      (error (list :none)))))

;; parse_int16 :: String -> Maybe Int16
(defvar hydra_overlay_emacs_lisp_lib_literals_parse_int16
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (>= n -32768) (<= n 32767) (string= (number-to-string n) s))
              (list :given n)
              (list :none)))
      (error (list :none)))))

;; parse_int32 :: String -> Maybe Int32
(defvar hydra_overlay_emacs_lisp_lib_literals_parse_int32
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (string= (number-to-string n) s))
              (list :given n)
              (list :none)))
      (error (list :none)))))

;; parse_uint8 :: String -> Maybe Uint8
(defvar hydra_overlay_emacs_lisp_lib_literals_parse_uint8
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (>= n 0) (<= n 255) (string= (number-to-string n) s))
              (list :given n)
              (list :none)))
      (error (list :none)))))

;; parse_uint16 :: String -> Maybe Uint16
(defvar hydra_overlay_emacs_lisp_lib_literals_parse_uint16
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (>= n 0) (<= n 65535) (string= (number-to-string n) s))
              (list :given n)
              (list :none)))
      (error (list :none)))))

(provide 'hydra.lib.literals)
