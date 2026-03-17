(in-package :cl-user)

;; bigfloat_to_bigint :: Double -> BigInteger
(defvar hydra_lib_literals_bigfloat_to_bigint
  (lambda (x)
    (round x)))

;; bigfloat_to_float :: Double -> Double  (identity)
(defvar hydra_lib_literals_bigfloat_to_float
  (lambda (x)
    (float x 1.0d0)))

;; bigfloat_to_float32 :: Double -> Float
;; Convert to single-float precision, then back to double for representation
(defvar hydra_lib_literals_bigfloat_to_float32
  (lambda (x)
    (float (float x 1.0f0) 1.0d0)))

;; bigfloat_to_float64 :: Double -> Double
(defvar hydra_lib_literals_bigfloat_to_float64
  (lambda (x)
    (float x 1.0d0)))

;; bigint_to_bigfloat :: BigInteger -> Double
(defvar hydra_lib_literals_bigint_to_bigfloat
  (lambda (x)
    (float x 1.0d0)))

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
      (let* ((bytes (coerce bv 'vector))
             (len (length bytes))
             (result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
        (loop for i from 0 below len by 3
              do (let* ((b0 (aref bytes i))
                        (b1 (if (< (1+ i) len) (aref bytes (1+ i)) 0))
                        (b2 (if (< (+ i 2) len) (aref bytes (+ i 2)) 0))
                        (remaining (- len i)))
                   (vector-push-extend (char b64-chars (ash b0 -2)) result)
                   (vector-push-extend (char b64-chars (logior (ash (logand b0 3) 4) (ash b1 -4))) result)
                   (if (>= remaining 2)
                       (vector-push-extend (char b64-chars (logior (ash (logand b1 #xF) 2) (ash b2 -6))) result)
                       (vector-push-extend #\= result))
                   (if (>= remaining 3)
                       (vector-push-extend (char b64-chars (logand b2 #x3F)) result)
                       (vector-push-extend #\= result))))
        (coerce result 'string)))))

;; float :: FloatPrecision -> Double -> Double
(defvar hydra_lib_literals_float
  (lambda (precision)
    (declare (ignore precision))
    (lambda (x)
      (float x 1.0d0))))

;; float32_to_bigfloat :: Float -> Double
(defvar hydra_lib_literals_float32_to_bigfloat
  (lambda (x)
    (float x 1.0d0)))

;; float64_to_bigfloat :: Double -> Double
(defvar hydra_lib_literals_float64_to_bigfloat
  (lambda (x)
    (float x 1.0d0)))

;; int :: IntPrecision -> Int -> Int
(defvar hydra_lib_literals_int
  (lambda (precision)
    (declare (ignore precision))
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
    (let* ((*read-default-float-format* 'double-float)
           (n (ignore-errors (read-from-string s))))
      (if (and n (numberp n))
          (list :just (float n 1.0d0))
          (list :nothing)))))

;; read_bigint :: String -> Maybe BigInteger
(defvar hydra_lib_literals_read_bigint
  (lambda (s)
    (let ((n (ignore-errors (parse-integer s :junk-allowed nil))))
      (if n
          (list :just n)
          (list :nothing)))))

;; read_float :: String -> Maybe Double
(defvar hydra_lib_literals_read_float
  (lambda (s)
    (let ((n (ignore-errors (read-from-string s))))
      (if (and n (numberp n))
          (list :just (float n 1.0d0))
          (list :nothing)))))

;; read_float32 :: String -> Maybe Float
(defvar hydra_lib_literals_read_float32
  (lambda (s)
    (let ((n (ignore-errors (read-from-string s))))
      (if (and n (numberp n))
          (list :just (float (float n 1.0f0) 1.0d0))
          (list :nothing)))))

;; read_int :: String -> Maybe Int
(defvar hydra_lib_literals_read_int
  (lambda (s)
    (let ((n (ignore-errors (parse-integer s :junk-allowed nil))))
      (if n
          (list :just n)
          (list :nothing)))))

;; read_int64 :: String -> Maybe Int64
(defvar hydra_lib_literals_read_int64
  (lambda (s)
    (let ((n (ignore-errors (parse-integer s :junk-allowed nil))))
      (if n
          (list :just n)
          (list :nothing)))))

;; read_uint :: String -> Maybe Uint
(defvar hydra_lib_literals_read_uint
  (lambda (s)
    (let ((n (ignore-errors (parse-integer s :junk-allowed nil))))
      (if (and n (>= n 0))
          (list :just n)
          (list :nothing)))))

;; read_uint32 :: String -> Maybe Uint32
(defvar hydra_lib_literals_read_uint32
  (lambda (s)
    (let ((n (ignore-errors (parse-integer s :junk-allowed nil))))
      (if (and n (>= n 0))
          (list :just n)
          (list :nothing)))))

;; read_uint64 :: String -> Maybe Uint64
(defvar hydra_lib_literals_read_uint64
  (lambda (s)
    (let ((n (ignore-errors (parse-integer s :junk-allowed nil))))
      (if (and n (>= n 0))
          (list :just n)
          (list :nothing)))))

;; Helper to strip CL float type suffixes (d0, f0, etc.) from write-to-string output
(defun strip-float-suffix (s)
  "Remove CL float type suffix (d0, f0, s0, l0) from a string."
  (let ((len (length s)))
    (cond
      ((and (>= len 2)
            (member (char s (- len 2)) '(#\d #\f #\s #\l #\D #\F #\S #\L))
            (char= (char s (- len 1)) #\0))
       (subseq s 0 (- len 2)))
      (t s))))

;; Helper for Haskell-compatible float show
(defun haskell-show-float (x)
  "Format a double-float in Haskell's show style."
  (cond
    ((= x 0.0d0) "0.0")
    ((and (/= x 0.0d0)
          (or (< (abs x) 0.1d0) (>= (abs x) 1.0d7)))
     ;; Scientific notation: Haskell uses e.g. "5.0e-2"
     (let* ((exp (floor (log (abs x) 10.0d0)))
            (mantissa (/ x (expt 10.0d0 exp)))
            ;; Adjust if mantissa rounds to 10
            (adj-exp (if (>= (abs mantissa) 10.0d0) (1+ exp) exp))
            (adj-mantissa (if (>= (abs mantissa) 10.0d0) (/ mantissa 10.0d0) mantissa)))
       (format nil "~A~Ae~A"
               (if (< adj-mantissa 0) "-" "")
               (strip-float-suffix (write-to-string (abs adj-mantissa)))
               adj-exp)))
    (t (strip-float-suffix (write-to-string x)))))

(defun haskell-show-float-single (x)
  "Format a single-float in Haskell's show style."
  (cond
    ((= x 0.0f0) "0.0")
    ((and (/= x 0.0f0)
          (or (< (abs x) 0.1f0) (>= (abs x) 1.0f7)))
     ;; Scientific notation
     (let* ((dx (float x 1.0d0))
            (exp (floor (log (abs dx) 10.0d0)))
            (mantissa (/ dx (expt 10.0d0 exp)))
            (adj-exp (if (>= (abs mantissa) 10.0d0) (1+ exp) exp))
            (adj-mantissa (if (>= (abs mantissa) 10.0d0) (/ mantissa 10.0d0) mantissa))
            ;; Round mantissa to single-float precision
            (sf-mantissa (float (float (abs adj-mantissa) 1.0f0) 1.0d0))
            (s (write-to-string sf-mantissa)))
       (format nil "~A~Ae~A"
               (if (< adj-mantissa 0) "-" "")
               (strip-float-suffix (write-to-string sf-mantissa)) adj-exp)))
    (t (strip-float-suffix (write-to-string x)))))

;; show_bigfloat :: Double -> String
(defvar hydra_lib_literals_show_bigfloat
  (lambda (x)
    (haskell-show-float (float x 1.0d0))))

;; show_bigint :: BigInteger -> String
(defvar hydra_lib_literals_show_bigint
  (lambda (x)
    (write-to-string x)))

;; show_float :: Double -> String
(defvar hydra_lib_literals_show_float
  (lambda (x)
    (haskell-show-float (float x 1.0d0))))

;; show_float32 :: Float -> String
;; Show a float32 value in Haskell show style.
;; The input is a double representing a single-float value.
;; Convert to single-float first for correct precision display.
(defvar hydra_lib_literals_show_float32
  (lambda (x)
    (haskell-show-float-single (float x 1.0f0))))

;; show_float64 :: Double -> String
(defvar hydra_lib_literals_show_float64
  (lambda (x)
    (haskell-show-float (float x 1.0d0))))

;; show_int :: Int -> String
(defvar hydra_lib_literals_show_int
  (lambda (x)
    (write-to-string x)))

;; show_int8 :: Int8 -> String
(defvar hydra_lib_literals_show_int8
  (lambda (x)
    (write-to-string x)))

;; show_int16 :: Int16 -> String
(defvar hydra_lib_literals_show_int16
  (lambda (x)
    (write-to-string x)))

;; show_int32 :: Int32 -> String
(defvar hydra_lib_literals_show_int32
  (lambda (x)
    (write-to-string x)))

;; show_int64 :: Int64 -> String
(defvar hydra_lib_literals_show_int64
  (lambda (x)
    (write-to-string x)))

;; show_string :: String -> String  (Haskell-compatible quoted representation)
(defvar hydra_lib_literals_show_string
  (lambda (s)
    (let ((acc (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
      (vector-push-extend #\" acc)
      (loop for c across s
            do (let ((code (char-code c)))
                 (cond
                   ((char= c #\\) (vector-push-extend #\\ acc) (vector-push-extend #\\ acc))
                   ((char= c #\") (vector-push-extend #\\ acc) (vector-push-extend #\" acc))
                   ((char= c #\Newline) (vector-push-extend #\\ acc) (vector-push-extend #\n acc))
                   ((char= c #\Return) (vector-push-extend #\\ acc) (vector-push-extend #\r acc))
                   ((char= c #\Tab) (vector-push-extend #\\ acc) (vector-push-extend #\t acc))
                   ;; Haskell control char names
                   ((= code 0) (dolist (ch (coerce "\\NUL" 'list)) (vector-push-extend ch acc)))
                   ((= code 7) (dolist (ch (coerce "\\a" 'list)) (vector-push-extend ch acc)))
                   ((= code 8) (dolist (ch (coerce "\\b" 'list)) (vector-push-extend ch acc)))
                   ((= code 11) (dolist (ch (coerce "\\v" 'list)) (vector-push-extend ch acc)))
                   ((= code 12) (dolist (ch (coerce "\\f" 'list)) (vector-push-extend ch acc)))
                   ((= code 127) (dolist (ch (coerce "\\DEL" 'list)) (vector-push-extend ch acc)))
                   ;; Non-ASCII: use Haskell decimal escape
                   ((> code 127)
                    (vector-push-extend #\\ acc)
                    (dolist (ch (coerce (write-to-string code) 'list))
                      (vector-push-extend ch acc)))
                   ;; Other control chars (1-6, 14-31): use decimal escape
                   ((< code 32)
                    (vector-push-extend #\\ acc)
                    (dolist (ch (coerce (write-to-string code) 'list))
                      (vector-push-extend ch acc)))
                   (t (vector-push-extend c acc)))))
      (vector-push-extend #\" acc)
      (coerce acc 'string))))

;; show_uint :: Uint -> String
(defvar hydra_lib_literals_show_uint
  (lambda (x)
    (write-to-string x)))

;; show_uint8 :: Uint8 -> String
(defvar hydra_lib_literals_show_uint8
  (lambda (x)
    (write-to-string x)))

;; show_uint16 :: Uint16 -> String
(defvar hydra_lib_literals_show_uint16
  (lambda (x)
    (write-to-string x)))

;; show_uint32 :: Uint32 -> String
(defvar hydra_lib_literals_show_uint32
  (lambda (x)
    (write-to-string x)))

;; show_uint64 :: Uint64 -> String
(defvar hydra_lib_literals_show_uint64
  (lambda (x)
    (write-to-string x)))

;; binary_to_bytes :: Binary -> [Int8]
(defvar hydra_lib_literals_binary_to_bytes
  (lambda (bs)
    (map 'list (lambda (b) (logand b #xFF)) bs)))

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
             (char= (char s 0) #\")
             (char= (char s (1- (length s))) #\"))
        (let* ((inner (subseq s 1 (1- (length s))))
               (result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
               (i 0)
               (len (length inner)))
          (loop while (< i len)
                do (if (and (char= (char inner i) #\\) (< (1+ i) len))
                       (let ((c (char inner (1+ i))))
                         (cond
                           ((char= c #\\) (vector-push-extend #\\ result))
                           ((char= c #\") (vector-push-extend #\" result))
                           ((char= c #\n) (vector-push-extend #\Newline result))
                           ((char= c #\t) (vector-push-extend #\Tab result))
                           ((char= c #\r) (vector-push-extend #\Return result))
                           (t (vector-push-extend #\\ result)
                              (vector-push-extend c result)))
                         (incf i 2))
                       (progn
                         (vector-push-extend (char inner i) result)
                         (incf i))))
          (list :just (coerce result 'string)))
        (list :nothing))))

;; show_boolean :: Bool -> String
(defvar hydra_lib_literals_show_boolean
  (lambda (x)
    (if x "true" "false")))

;; string_to_binary :: String -> ByteString (base64 decoding)
(defvar hydra_lib_literals_string_to_binary
  (let ((b64-decode (make-array 128 :initial-element -1)))
    (loop for c across "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
          for i from 0 do (setf (aref b64-decode (char-code c)) i))
    (lambda (s)
      ;; Strip padding
      (let* ((len (length s))
             (pad (cond ((and (> len 0) (char= (char s (1- len)) #\=))
                         (if (and (> len 1) (char= (char s (- len 2)) #\=)) 2 1))
                        (t 0)))
             (in-len (- len pad))
             (result nil))
        (loop for i from 0 below in-len by 4
              do (let* ((a (aref b64-decode (char-code (char s i))))
                        (b (if (< (1+ i) in-len) (aref b64-decode (char-code (char s (1+ i)))) 0))
                        (c (if (< (+ i 2) in-len) (aref b64-decode (char-code (char s (+ i 2)))) 0))
                        (d (if (< (+ i 3) in-len) (aref b64-decode (char-code (char s (+ i 3)))) 0)))
                   (push (logior (ash a 2) (ash b -4)) result)
                   (when (< (+ i 2) in-len)
                     (push (logand (logior (ash b 4) (ash c -2)) #xFF) result))
                   (when (< (+ i 3) in-len)
                     (push (logand (logior (ash c 6) d) #xFF) result))))
        (nreverse result)))))

;; uint :: UintPrecision -> Uint -> Uint
(defvar hydra_lib_literals_uint
  (lambda (precision)
    (declare (ignore precision))
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
    (handler-case (let ((*read-default-float-format* 'double-float))
                    (list :just (coerce (read-from-string s) 'double-float)))
      (error () (list :nothing)))))

;; read_int8 :: String -> Maybe Int8
(defvar hydra_lib_literals_read_int8
  (lambda (s)
    (handler-case
        (let ((n (parse-integer s)))
          (if (and (>= n -128) (<= n 127)) (list :just n) (list :nothing)))
      (error () (list :nothing)))))

;; read_int16 :: String -> Maybe Int16
(defvar hydra_lib_literals_read_int16
  (lambda (s)
    (handler-case
        (let ((n (parse-integer s)))
          (if (and (>= n -32768) (<= n 32767)) (list :just n) (list :nothing)))
      (error () (list :nothing)))))

;; read_int32 :: String -> Maybe Int32
(defvar hydra_lib_literals_read_int32
  (lambda (s)
    (handler-case (list :just (parse-integer s))
      (error () (list :nothing)))))

;; read_uint8 :: String -> Maybe Uint8
(defvar hydra_lib_literals_read_uint8
  (lambda (s)
    (handler-case
        (let ((n (parse-integer s)))
          (if (and (>= n 0) (<= n 255)) (list :just n) (list :nothing)))
      (error () (list :nothing)))))

;; read_uint16 :: String -> Maybe Uint16
(defvar hydra_lib_literals_read_uint16
  (lambda (s)
    (handler-case
        (let ((n (parse-integer s)))
          (if (and (>= n 0) (<= n 65535)) (list :just n) (list :nothing)))
      (error () (list :nothing)))))
