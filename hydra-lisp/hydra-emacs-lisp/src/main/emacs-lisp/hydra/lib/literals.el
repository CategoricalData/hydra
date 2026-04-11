;;; literals.el --- Hydra literal conversion primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; bigfloat_to_bigint :: Double -> BigInteger
(defvar hydra_lib_literals_bigfloat_to_bigint
  (lambda (x)
    (round x)))

;; bigfloat_to_float :: Double -> Double  (identity)
(defvar hydra_lib_literals_bigfloat_to_float
  (lambda (x)
    (float x)))

;; bigfloat_to_float32 :: Double -> Float
;; EL has only one float type; approximate float32 by rounding to 7 sig digits
(defvar hydra_lib_literals_bigfloat_to_float32
  (lambda (x)
    (round-to-float32 (float x))))

;; bigfloat_to_float64 :: Double -> Double
(defvar hydra_lib_literals_bigfloat_to_float64
  (lambda (x)
    (float x)))

;; bigint_to_bigfloat :: BigInteger -> Double
(defvar hydra_lib_literals_bigint_to_bigfloat
  (lambda (x)
    (float x)))

;; bigint_to_int :: BigInteger -> Int  (identity)
(defvar hydra_lib_literals_bigint_to_int
  (lambda (x) x))

;; bigint_to_int8 :: BigInteger -> Int8
(defvar hydra_lib_literals_bigint_to_int8
  (lambda (x) x))

;; bigint_to_int16 :: BigInteger -> Int16
(defvar hydra_lib_literals_bigint_to_int16
  (lambda (x) x))

;; bigint_to_int32 :: BigInteger -> Int32
(defvar hydra_lib_literals_bigint_to_int32
  (lambda (x) x))

;; bigint_to_int64 :: BigInteger -> Int64
(defvar hydra_lib_literals_bigint_to_int64
  (lambda (x) x))

;; bigint_to_uint :: BigInteger -> Uint
(defvar hydra_lib_literals_bigint_to_uint
  (lambda (x) x))

;; bigint_to_uint8 :: BigInteger -> Uint8
(defvar hydra_lib_literals_bigint_to_uint8
  (lambda (x) x))

;; bigint_to_uint16 :: BigInteger -> Uint16
(defvar hydra_lib_literals_bigint_to_uint16
  (lambda (x) x))

;; bigint_to_uint32 :: BigInteger -> Uint32
(defvar hydra_lib_literals_bigint_to_uint32
  (lambda (x) x))

;; bigint_to_uint64 :: BigInteger -> Uint64
(defvar hydra_lib_literals_bigint_to_uint64
  (lambda (x) x))

;; binary_to_string :: ByteString -> String (base64 encoding)
(defvar hydra_lib_literals_binary_to_string
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

;; float :: FloatPrecision -> Double -> Double
(defvar hydra_lib_literals_float
  (lambda (_precision)
    (lambda (x)
      (float x))))

;; float32_to_bigfloat :: Float -> Double
(defvar hydra_lib_literals_float32_to_bigfloat
  (lambda (x)
    (float x)))

;; float64_to_bigfloat :: Double -> Double
(defvar hydra_lib_literals_float64_to_bigfloat
  (lambda (x)
    (float x)))

;; int :: IntPrecision -> Int -> Int
(defvar hydra_lib_literals_int
  (lambda (_precision)
    (lambda (x) x)))

;; int8_to_bigint :: Int8 -> BigInteger
(defvar hydra_lib_literals_int8_to_bigint
  (lambda (x) x))

;; int16_to_bigint :: Int16 -> BigInteger
(defvar hydra_lib_literals_int16_to_bigint
  (lambda (x) x))

;; int32_to_bigint :: Int32 -> BigInteger
(defvar hydra_lib_literals_int32_to_bigint
  (lambda (x) x))

;; int64_to_bigint :: Int64 -> BigInteger
(defvar hydra_lib_literals_int64_to_bigint
  (lambda (x) x))

;; read_bigfloat :: String -> Maybe Double
(defvar hydra_lib_literals_read_bigfloat
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (numberp n) (not (= n 0)) (not (string= s "0")))
              (list :just (float n))
              (if (string= s "0")
                  (list :just 0.0)
                  (list :nothing))))
      (error (list :nothing)))))

;; read_bigint :: String -> Maybe BigInteger
;; Uses read-from-string to handle arbitrarily large integers (Emacs 27+ bignum support)
(defvar hydra_lib_literals_read_bigint
  (lambda (s)
    (condition-case nil
        (if (string-match-p "^-?[0-9]+$" s)
            (let ((n (car (read-from-string s))))
              (if (integerp n)
                  (list :just n)
                (list :nothing)))
          (list :nothing))
      (error (list :nothing)))))

;; read_float :: String -> Maybe Double
(defvar hydra_lib_literals_read_float
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (numberp n) (or (not (= n 0)) (string= s "0") (string= s "0.0")))
              (list :just (float n))
              (list :nothing)))
      (error (list :nothing)))))

;; read_float32 :: String -> Maybe Float
(defvar hydra_lib_literals_read_float32
  (lambda (s)
    (cond
      ((string= s "NaN") (list :just 0.0e+NaN))
      ((string= s "Infinity") (list :just 1.0e+INF))
      ((string= s "-Infinity") (list :just -1.0e+INF))
      (t (condition-case nil
             (let ((n (string-to-number s)))
               (if (and (numberp n) (or (not (= n 0)) (string= s "0") (string= s "0.0")))
                   (list :just (round-to-float32 (float n)))
                   (list :nothing)))
           (error (list :nothing)))))))

;; read_int :: String -> Maybe Int
(defvar hydra_lib_literals_read_int
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (string= (number-to-string n) s))
              (list :just n)
              (list :nothing)))
      (error (list :nothing)))))

;; read_int64 :: String -> Maybe Int64
(defvar hydra_lib_literals_read_int64
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (string= (number-to-string n) s))
              (list :just n)
              (list :nothing)))
      (error (list :nothing)))))

;; read_uint :: String -> Maybe Uint
(defvar hydra_lib_literals_read_uint
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (>= n 0) (string= (number-to-string n) s))
              (list :just n)
              (list :nothing)))
      (error (list :nothing)))))

;; read_uint32 :: String -> Maybe Uint32
(defvar hydra_lib_literals_read_uint32
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (>= n 0) (string= (number-to-string n) s))
              (list :just n)
              (list :nothing)))
      (error (list :nothing)))))

;; read_uint64 :: String -> Maybe Uint64
(defvar hydra_lib_literals_read_uint64
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (>= n 0) (string= (number-to-string n) s))
              (list :just n)
              (list :nothing)))
      (error (list :nothing)))))

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

;; show_bigfloat :: Double -> String
(defvar hydra_lib_literals_show_bigfloat
  (lambda (x)
    (haskell-show-float (float x))))

;; show_bigint :: BigInteger -> String
(defvar hydra_lib_literals_show_bigint
  (lambda (x)
    (number-to-string x)))

;; show_float :: Double -> String
(defvar hydra_lib_literals_show_float
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

;; show_float32 :: Float -> String
(defvar hydra_lib_literals_show_float32
  (lambda (x)
    (haskell-show-float32 x)))

;; show_float64 :: Double -> String
(defvar hydra_lib_literals_show_float64
  (lambda (x)
    (haskell-show-float (float x))))

;; show_int :: Int -> String
(defvar hydra_lib_literals_show_int
  (lambda (x)
    (number-to-string x)))

;; show_int8 :: Int8 -> String
(defvar hydra_lib_literals_show_int8
  (lambda (x)
    (number-to-string x)))

;; show_int16 :: Int16 -> String
(defvar hydra_lib_literals_show_int16
  (lambda (x)
    (number-to-string x)))

;; show_int32 :: Int32 -> String
(defvar hydra_lib_literals_show_int32
  (lambda (x)
    (number-to-string x)))

;; show_int64 :: Int64 -> String
(defvar hydra_lib_literals_show_int64
  (lambda (x)
    (number-to-string x)))

;; show_string :: String -> String  (Haskell-compatible quoted representation)
(defvar hydra_lib_literals_show_string
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
(defvar hydra_lib_literals_show_uint
  (lambda (x)
    (number-to-string x)))

;; show_uint8 :: Uint8 -> String
(defvar hydra_lib_literals_show_uint8
  (lambda (x)
    (number-to-string x)))

;; show_uint16 :: Uint16 -> String
(defvar hydra_lib_literals_show_uint16
  (lambda (x)
    (number-to-string x)))

;; show_uint32 :: Uint32 -> String
(defvar hydra_lib_literals_show_uint32
  (lambda (x)
    (number-to-string x)))

;; show_uint64 :: Uint64 -> String
(defvar hydra_lib_literals_show_uint64
  (lambda (x)
    (number-to-string x)))

;; binary_to_bytes :: Binary -> [Int8]
(defvar hydra_lib_literals_binary_to_bytes
  (lambda (bs)
    (mapcar (lambda (b) (logand b #xFF)) bs)))

;; read_boolean :: String -> Maybe Bool
(defvar hydra_lib_literals_read_boolean
  (lambda (s)
    (cond
      ((string= s "true") (list :just t))
      ((string= s "false") (list :just nil))
      (t (list :nothing)))))

;; read_string :: String -> Maybe String
;; Haskell semantics: reads a quoted string literal, returns Nothing for unquoted
(defvar hydra_lib_literals_read_string
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
          (list :just (apply #'string (nreverse result))))
        (list :nothing))))

;; show_boolean :: Bool -> String
(defvar hydra_lib_literals_show_boolean
  (lambda (x)
    (if x "true" "false")))

;; string_to_binary :: String -> ByteString (base64 decoding)
(defvar hydra_lib_literals_string_to_binary
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
(defvar hydra_lib_literals_uint
  (lambda (_precision)
    (lambda (x) x)))

;; uint8_to_bigint :: Uint8 -> BigInteger
(defvar hydra_lib_literals_uint8_to_bigint
  (lambda (x) x))

;; uint16_to_bigint :: Uint16 -> BigInteger
(defvar hydra_lib_literals_uint16_to_bigint
  (lambda (x) x))

;; uint32_to_bigint :: Uint32 -> BigInteger
(defvar hydra_lib_literals_uint32_to_bigint
  (lambda (x) x))

;; uint64_to_bigint :: Uint64 -> BigInteger
(defvar hydra_lib_literals_uint64_to_bigint
  (lambda (x) x))

;; read_float64 :: String -> Maybe Float64
(defvar hydra_lib_literals_read_float64
  (lambda (s)
    (cond
      ((string= s "NaN") (list :just 0.0e+NaN))
      ((string= s "Infinity") (list :just 1.0e+INF))
      ((string= s "-Infinity") (list :just -1.0e+INF))
      (t (condition-case nil
             (let ((n (string-to-number s)))
               (if (and (numberp n) (or (not (= n 0)) (string= s "0") (string= s "0.0")))
                   (list :just (float n))
                   (list :nothing)))
           (error (list :nothing)))))))

;; read_int8 :: String -> Maybe Int8
(defvar hydra_lib_literals_read_int8
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (>= n -128) (<= n 127) (string= (number-to-string n) s))
              (list :just n)
              (list :nothing)))
      (error (list :nothing)))))

;; read_int16 :: String -> Maybe Int16
(defvar hydra_lib_literals_read_int16
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (>= n -32768) (<= n 32767) (string= (number-to-string n) s))
              (list :just n)
              (list :nothing)))
      (error (list :nothing)))))

;; read_int32 :: String -> Maybe Int32
(defvar hydra_lib_literals_read_int32
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (string= (number-to-string n) s))
              (list :just n)
              (list :nothing)))
      (error (list :nothing)))))

;; read_uint8 :: String -> Maybe Uint8
(defvar hydra_lib_literals_read_uint8
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (>= n 0) (<= n 255) (string= (number-to-string n) s))
              (list :just n)
              (list :nothing)))
      (error (list :nothing)))))

;; read_uint16 :: String -> Maybe Uint16
(defvar hydra_lib_literals_read_uint16
  (lambda (s)
    (condition-case nil
        (let ((n (string-to-number s)))
          (if (and (integerp n) (>= n 0) (<= n 65535) (string= (number-to-string n) s))
              (list :just n)
              (list :nothing)))
      (error (list :nothing)))))

(provide 'hydra.lib.literals)
