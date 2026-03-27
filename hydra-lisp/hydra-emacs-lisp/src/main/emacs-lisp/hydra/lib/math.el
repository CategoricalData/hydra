;;; math.el --- Hydra math primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; abs :: Int -> Int
(defvar hydra_lib_math_abs
  (lambda (n)
    "Return the absolute value."
    (abs n)))

;; acos :: Double -> Double
(defvar hydra_lib_math_acos
  (lambda (x)
    "Return the arc cosine of x in radians."
    (acos (float x))))

;; acosh :: Double -> Double
;; acosh(x) = ln(x + sqrt(x^2 - 1))
(defvar hydra_lib_math_acosh
  (lambda (x)
    "Return the inverse hyperbolic cosine of x."
    (let ((fx (float x)))
      (log (+ fx (sqrt (- (* fx fx) 1.0)))))))

;; add :: Int -> Int -> Int
(defvar hydra_lib_math_add
  (lambda (a)
    "Add two numbers."
    (lambda (b)
      (+ a b))))

;; asin :: Double -> Double
(defvar hydra_lib_math_asin
  (lambda (x)
    "Return the arc sine of x in radians."
    (asin (float x))))

;; asinh :: Double -> Double
;; asinh(x) = ln(x + sqrt(x^2 + 1))
(defvar hydra_lib_math_asinh
  (lambda (x)
    "Return the inverse hyperbolic sine of x."
    (let ((fx (float x)))
      (log (+ fx (sqrt (+ (* fx fx) 1.0)))))))

;; atan :: Double -> Double
(defvar hydra_lib_math_atan
  (lambda (x)
    "Return the arc tangent of x in radians."
    (atan (float x))))

;; atan2 :: Double -> Double -> Double
(defvar hydra_lib_math_atan2
  (lambda (y)
    "Return the arc tangent of y/x in radians, using signs to determine quadrant."
    (lambda (x)
      (atan (float y) (float x)))))

;; atanh :: Double -> Double
;; atanh(x) = 0.5 * ln((1+x)/(1-x))
(defvar hydra_lib_math_atanh
  (lambda (x)
    "Return the inverse hyperbolic tangent of x."
    (let ((fx (float x)))
      (* 0.5 (log (/ (+ 1.0 fx) (- 1.0 fx)))))))

;; ceiling :: Double -> BigInt
(defvar hydra_lib_math_ceiling
  (lambda (x)
    "Return the ceiling of x as an integer."
    (ceiling x)))

;; cos :: Double -> Double
(defvar hydra_lib_math_cos
  (lambda (x)
    "Return the cosine of x radians."
    (cos (float x))))

;; cosh :: Double -> Double
;; cosh(x) = (e^x + e^(-x)) / 2
(defvar hydra_lib_math_cosh
  (lambda (x)
    "Return the hyperbolic cosine of x."
    (let ((fx (float x)))
      (/ (+ (exp fx) (exp (- fx))) 2.0))))

;; div :: Int -> Int -> Int  (floor division)
(defvar hydra_lib_math_div
  (lambda (a)
    "Divide two integers using integer division."
    (lambda (b)
      (floor a b))))

;; e :: Double
(defvar hydra_lib_math_e (exp 1.0)
  "Euler's number (e approximately 2.71828).")

;; even :: Int -> Bool
(defvar hydra_lib_math_even
  (lambda (n)
    "Check if an integer is even."
    (cl-evenp n)))

;; exp :: Double -> Double
(defvar hydra_lib_math_exp
  (lambda (x)
    "Return e raised to the power x."
    (exp (float x))))

;; floor :: Double -> BigInt
(defvar hydra_lib_math_floor
  (lambda (x)
    "Return the floor of x as an integer."
    (floor x)))

;; log :: Double -> Double
(defvar hydra_lib_math_log
  (lambda (x)
    "Return the natural logarithm of x."
    (log (float x))))

;; logBase :: Double -> Double -> Double
(defvar hydra_lib_math_logBase
  (lambda (base)
    "Return the logarithm of x to the given base."
    (lambda (x)
      (/ (log (float x)) (log (float base))))))

;; log_base alias
(defvar hydra_lib_math_log_base hydra_lib_math_logBase)

;; max :: Int -> Int -> Int
(defvar hydra_lib_math_max
  (lambda (a)
    "Return the maximum of two values."
    (lambda (b)
      (max a b))))

;; min :: Int -> Int -> Int
(defvar hydra_lib_math_min
  (lambda (a)
    "Return the minimum of two values."
    (lambda (b)
      (min a b))))

;; mod :: Int -> Int -> Int  (floor mod)
(defvar hydra_lib_math_mod
  (lambda (a)
    "Mathematical modulo."
    (lambda (b)
      (mod a b))))

;; mul :: Int -> Int -> Int
(defvar hydra_lib_math_mul
  (lambda (a)
    "Multiply two numbers."
    (lambda (b)
      (* a b))))

;; negate :: Int -> Int
(defvar hydra_lib_math_negate
  (lambda (a)
    "Negate a number."
    (- a)))

;; odd :: Int -> Bool
(defvar hydra_lib_math_odd
  (lambda (n)
    "Check if an integer is odd."
    (cl-oddp n)))

;; pi :: Double
(defvar hydra_lib_math_pi float-pi
  "Pi (approximately 3.14159).")

;; pow :: Double -> Double -> Double
(defvar hydra_lib_math_pow
  (lambda (base)
    "Return x raised to the power y."
    (lambda (exp-val)
      (expt (float base) (float exp-val)))))

;; pred :: Int -> Int
(defvar hydra_lib_math_pred
  (lambda (n)
    "Return the predecessor (x - 1)."
    (1- n)))

;; range :: Int -> Int -> [Int]  (inclusive both ends)
(defvar hydra_lib_math_range
  (lambda (start)
    "Generate a range of values from start to end (inclusive)."
    (lambda (end)
      (let ((acc nil))
        (let ((i start))
          (while (<= i end)
            (push i acc)
            (setq i (1+ i))))
        (nreverse acc)))))

;; rem :: Int -> Int -> Int  (truncating remainder)
(defvar hydra_lib_math_rem
  (lambda (a)
    "Integer remainder."
    (lambda (b)
      (% a b))))

;; round :: Double -> BigInt
(defvar hydra_lib_math_round
  (lambda (x)
    "Return x rounded to the nearest integer."
    (round x)))

;; roundFloat64 :: Int -> Double -> Double
(defvar hydra_lib_math_round_float64
  (lambda (n)
    "Round a float64 to n significant digits."
    (lambda (x)
      (if (= x 0.0) 0.0
          (let* ((fx (float x))
                 (factor (expt 10.0 (- n 1 (floor (log (abs fx) 10))))))
            (/ (fround (* fx factor)) factor))))))

;; roundBigfloat :: Int -> Double -> Double  (alias for roundFloat64)
(defvar hydra_lib_math_round_bigfloat hydra_lib_math_round_float64
  "Round a bigfloat to n significant digits.")

;; roundFloat32 :: Int -> Float -> Float
;; Rounds to N significant digits, then snaps through IEEE float32
(defun snap-to-float32 (x)
  "Snap a double to IEEE 754 float32 precision (24-bit mantissa)."
  (if (= x 0.0) 0.0
    (let* ((sign (if (< x 0) -1.0 1.0))
           (ax (abs x))
           (e (floor (log ax 2.0)))
           (scale (expt 2.0 (- 23 e)))
           (mantissa (round (* ax scale))))
      (* sign (/ mantissa scale)))))
(defvar hydra_lib_math_round_float32
  (lambda (n)
    "Round a float32 to n significant digits."
    (lambda (x)
      (snap-to-float32 (funcall (funcall hydra_lib_math_round_float64 n) x)))))

;; signum :: Int -> Int
(defvar hydra_lib_math_signum
  (lambda (n)
    "Return the sign of a number (-1, 0, or 1)."
    (cond ((> n 0) 1) ((< n 0) -1) (t 0))))

;; sin :: Double -> Double
(defvar hydra_lib_math_sin
  (lambda (x)
    "Return the sine of x radians."
    (sin (float x))))

;; sinh :: Double -> Double
;; sinh(x) = (e^x - e^(-x)) / 2
(defvar hydra_lib_math_sinh
  (lambda (x)
    "Return the hyperbolic sine of x."
    (let ((fx (float x)))
      (/ (- (exp fx) (exp (- fx))) 2.0))))

;; sqrt :: Double -> Double
(defvar hydra_lib_math_sqrt
  (lambda (x)
    "Return the square root of x."
    (sqrt (float x))))

;; sub :: Int -> Int -> Int
(defvar hydra_lib_math_sub
  (lambda (a)
    "Subtract two numbers."
    (lambda (b)
      (- a b))))

;; succ :: Int -> Int
(defvar hydra_lib_math_succ
  (lambda (n)
    "Return the successor (x + 1)."
    (1+ n)))

;; tan :: Double -> Double
(defvar hydra_lib_math_tan
  (lambda (x)
    "Return the tangent of x radians."
    (tan (float x))))

;; tanh :: Double -> Double
;; tanh(x) = sinh(x) / cosh(x) = (e^x - e^(-x)) / (e^x + e^(-x))
(defvar hydra_lib_math_tanh
  (lambda (x)
    "Return the hyperbolic tangent of x."
    (let* ((fx (float x))
           (ep (exp fx))
           (en (exp (- fx))))
      (/ (- ep en) (+ ep en)))))

;; truncate :: Double -> BigInt
(defvar hydra_lib_math_truncate
  (lambda (x)
    "Return x truncated to an integer (towards zero)."
    (truncate x)))

(provide 'hydra.lib.math)
