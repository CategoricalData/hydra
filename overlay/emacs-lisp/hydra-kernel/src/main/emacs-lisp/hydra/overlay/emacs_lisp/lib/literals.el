;;; literals.el --- Hydra literal conversion primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; bigint_to_decimal :: BigInteger -> Decimal
;; Emacs Lisp has no native decimal; adapter fallback uses float.
(defvar hydra_overlay_emacs_lisp_lib_literals_bigint_to_decimal
  (lambda (x)
    (float x)))

;; bigint_to_int :: BigInteger -> Int  (identity)
(defvar hydra_overlay_emacs_lisp_lib_literals_bigint_to_int
  (lambda (x) x))

;; bigint_to_int8 :: BigInteger -> Int8
(defvar hydra_overlay_emacs_lisp_lib_literals_bigint_to_int8
  (lambda (x) x))

;; bigint_to_int16 :: BigInteger -> Int16
(defvar hydra_overlay_emacs_lisp_lib_literals_bigint_to_int16
  (lambda (x) x))

;; bigint_to_int32 :: BigInteger -> Int32
(defvar hydra_overlay_emacs_lisp_lib_literals_bigint_to_int32
  (lambda (x) x))

;; bigint_to_int64 :: BigInteger -> Int64
(defvar hydra_overlay_emacs_lisp_lib_literals_bigint_to_int64
  (lambda (x) x))

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

;; decimal_to_bigint :: Decimal -> BigInteger
;; Emacs Lisp has no native decimal; input is a float.
(defvar hydra_overlay_emacs_lisp_lib_literals_decimal_to_bigint
  (lambda (x)
    (round x)))

;; decimal_to_float32 :: Decimal -> Float
(defvar hydra_overlay_emacs_lisp_lib_literals_decimal_to_float32
  (lambda (x)
    (round-to-float32 (float x))))

;; decimal_to_float64 :: Decimal -> Double
(defvar hydra_overlay_emacs_lisp_lib_literals_decimal_to_float64
  (lambda (x)
    (float x)))

;; float :: FloatPrecision -> Double -> Double
(defvar hydra_overlay_emacs_lisp_lib_literals_float
  (lambda (_precision)
    (lambda (x)
      (float x))))

;; float32_to_decimal :: Float -> Decimal
(defvar hydra_overlay_emacs_lisp_lib_literals_float32_to_decimal
  (lambda (x)
    (float x)))

;; float32_to_float64 :: Float -> Double
;; EL has a single float type; widening is identity.
(defvar hydra_overlay_emacs_lisp_lib_literals_float32_to_float64
  (lambda (x)
    (float x)))

;; float64_to_decimal :: Double -> Decimal
(defvar hydra_overlay_emacs_lisp_lib_literals_float64_to_decimal
  (lambda (x)
    (float x)))

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

;; parse_decimal :: String -> Maybe Decimal
;; Emacs Lisp has no native decimal; fallback uses float.
(defvar hydra_overlay_emacs_lisp_lib_literals_parse_decimal
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (numberp n) (not (= n 0)) (not (string= s "0")))
              (list :given (float n))
              (if (string= s "0")
                  (list :given 0.0)
                  (list :none))))
      (error (list :none)))))

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
          (if (and (integerp n) (>= n 0) (string= (number-to-string n) s))
              (list :given n)
              (list :none)))
      (error (list :none)))))

;; parse_uint64 :: String -> Maybe Uint64
(defvar hydra_overlay_emacs_lisp_lib_literals_parse_uint64
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (>= n 0) (string= (number-to-string n) s))
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

;; print_decimal :: Decimal -> String
;; Emacs Lisp has no native decimal; formatted as float. Unlike a float
;; literal -- which reuses Double's own show threshold (scientific below
;; 0.1 or at/above 1e7) -- printDecimal has its own, wider positional range
;; (adjusted exponent -6 <= a < 21; overlay/haskell/.../Literals.hs), so
;; 0.01/0.001 print plainly ("0.01") rather than in scientific form. A
;; whole value also prints without a trailing ".0" (e.g. "42", not "42.0"),
;; since decimals track scale and a float coerced from an integral source
;; has scale 0.
(defvar hydra_overlay_emacs_lisp_lib_literals_print_decimal
  (lambda (x)
    (let ((d (float x)))
      (cond
        ((isnan d) "NaN")
        ((hydra--literals-infinitep d) (if (> d 0) "Infinity" "-Infinity"))
        ((= d 0.0) "0")
        ((and (= d (ftruncate d)) (< (abs d) 1.0e18))
         (format "%d" (truncate d)))
        (t
         (let* ((digex (hydra--decimal-digits-and-exponent d))
                (sig (car digex))
                (e (cdr digex))
                (sign (if (< d 0) "-" "")))
           (if (and (>= e -6) (< e 21))
               (if (>= e 0)
                   (if (< e (1- (length sig)))
                       (format "%s%s.%s" sign (substring sig 0 (1+ e)) (substring sig (1+ e)))
                       (format "%s%s%s" sign sig (make-string (- (1+ e) (length sig)) ?0)))
                   (format "%s0.%s%s" sign (make-string (- -1 e) ?0) sig))
               (haskell-show-float d))))))))

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
