(in-package :cl-user)

;; abs :: Int -> Int
;; Return the absolute value.
(defvar hydra_lib_math_abs
  (lambda (n) (abs n)))

;; acos :: Double -> Double
;; Return the arc cosine of x in radians.
(defvar hydra_lib_math_acos
  (lambda (x) (acos (float x 1.0d0))))

;; acosh :: Double -> Double
;; Return the inverse hyperbolic cosine of x.
(defvar hydra_lib_math_acosh
  (lambda (x) (acosh (float x 1.0d0))))

;; add :: Int -> Int -> Int
;; Add two numbers.
(defvar hydra_lib_math_add
  (lambda (a)
    (lambda (b)
      (+ a b))))

;; asin :: Double -> Double
;; Return the arc sine of x in radians.
(defvar hydra_lib_math_asin
  (lambda (x) (asin (float x 1.0d0))))

;; asinh :: Double -> Double
;; Return the inverse hyperbolic sine of x.
(defvar hydra_lib_math_asinh
  (lambda (x) (asinh (float x 1.0d0))))

;; atan :: Double -> Double
;; Return the arc tangent of x in radians.
(defvar hydra_lib_math_atan
  (lambda (x) (atan (float x 1.0d0))))

;; atan2 :: Double -> Double -> Double
;; Return the arc tangent of y/x in radians, using signs to determine quadrant.
(defvar hydra_lib_math_atan2
  (lambda (y)
    (lambda (x)
      (atan (float y 1.0d0) (float x 1.0d0)))))

;; atanh :: Double -> Double
;; Return the inverse hyperbolic tangent of x.
(defvar hydra_lib_math_atanh
  (lambda (x) (atanh (float x 1.0d0))))

;; ceiling :: Double -> BigInt
;; Return the ceiling of x as an integer.
(defvar hydra_lib_math_ceiling
  (lambda (x) (ceiling x)))

;; cos :: Double -> Double
;; Return the cosine of x radians.
(defvar hydra_lib_math_cos
  (lambda (x) (cos (float x 1.0d0))))

;; cosh :: Double -> Double
;; Return the hyperbolic cosine of x.
(defvar hydra_lib_math_cosh
  (lambda (x) (cosh (float x 1.0d0))))

;; div :: Int -> Int -> Int
;; Divide two integers using integer division.
(defvar hydra_lib_math_div
  (lambda (a)
    (lambda (b)
      (floor a b))))

;; e :: Double
;; Euler's number (e ~ 2.71828).
(defvar hydra_lib_math_e (exp 1.0d0))

;; even :: Int -> Bool
;; Check if an integer is even.
(defvar hydra_lib_math_even
  (lambda (n) (evenp n)))

;; exp :: Double -> Double
;; Return e raised to the power x.
(defvar hydra_lib_math_exp
  (lambda (x) (exp (float x 1.0d0))))

;; floor :: Double -> BigInt
;; Return the floor of x as an integer.
(defvar hydra_lib_math_floor
  (lambda (x) (floor x)))

;; log :: Double -> Double
;; Return the natural logarithm of x.
(defvar hydra_lib_math_log
  (lambda (x) (log (float x 1.0d0))))

;; logBase :: Double -> Double -> Double
;; Return the logarithm of x to the given base.
(defvar hydra_lib_math_logBase
  (lambda (base)
    (lambda (x)
      (/ (log (float x 1.0d0)) (log (float base 1.0d0))))))

;; log_base alias
(defvar hydra_lib_math_log_base hydra_lib_math_logBase)

;; max :: Int -> Int -> Int
;; Return the maximum of two values.
(defvar hydra_lib_math_max
  (lambda (a)
    (lambda (b)
      (max a b))))

;; min :: Int -> Int -> Int
;; Return the minimum of two values.
(defvar hydra_lib_math_min
  (lambda (a)
    (lambda (b)
      (min a b))))

;; mod :: Int -> Int -> Int
;; Mathematical modulo.
(defvar hydra_lib_math_mod
  (lambda (a)
    (lambda (b)
      (mod a b))))

;; mul :: Int -> Int -> Int
;; Multiply two numbers.
(defvar hydra_lib_math_mul
  (lambda (a)
    (lambda (b)
      (* a b))))

;; negate :: Int -> Int
;; Negate a number.
(defvar hydra_lib_math_negate
  (lambda (a)
    (- a)))

;; odd :: Int -> Bool
;; Check if an integer is odd.
(defvar hydra_lib_math_odd
  (lambda (n) (oddp n)))

;; pi :: Double
;; Pi (pi ~ 3.14159).
(defvar hydra_lib_math_pi (float pi 1.0d0))

;; pow :: Double -> Double -> Double
;; Return x raised to the power y.
(defvar hydra_lib_math_pow
  (lambda (base)
    (lambda (exp-val)
      (expt (float base 1.0d0) (float exp-val 1.0d0)))))

;; pred :: Int -> Int
;; Return the predecessor (x - 1).
(defvar hydra_lib_math_pred
  (lambda (n) (1- n)))

;; range :: Int -> Int -> [Int]
;; Generate a range of values from start to end (inclusive).
(defvar hydra_lib_math_range
  (lambda (start)
    (lambda (end)
      (loop for i from start to end collect i))))

;; rem :: Int -> Int -> Int
;; Integer remainder.
(defvar hydra_lib_math_rem
  (lambda (a)
    (lambda (b)
      (rem a b))))

;; round :: Double -> BigInt
;; Return x rounded to the nearest integer.
(defvar hydra_lib_math_round
  (lambda (x) (round x)))

;; roundFloat32 :: Int -> Float -> Float
;; Round a float32 to n significant digits.
;; Round to n significant digits, then snap to IEEE 754 float32 precision.
(defvar hydra_lib_math_round_float32
  (lambda (n)
    (lambda (x)
      (if (= x 0.0) 0.0d0
          (let* ((fx (float x 1.0d0))
                 (factor (expt 10.0d0 (- n 1 (floor (log (abs fx) 10)))))
                 (rounded (float (/ (round (* fx factor)) factor) 1.0d0)))
            ;; Snap to float32 precision by round-tripping through single-float
            (float (float rounded 1.0f0) 1.0d0))))))

;; roundFloat64 :: Int -> Double -> Double
;; Round a float64 to n significant digits.
(defvar hydra_lib_math_round_float64
  (lambda (n)
    (lambda (x)
      (if (= x 0.0d0) 0.0d0
          (let* ((fx (float x 1.0d0))
                 (factor (expt 10.0d0 (- n 1 (floor (log (abs fx) 10))))))
            (/ (round (* fx factor)) factor))))))

;; roundBigfloat :: Int -> Double -> Double (alias for roundFloat64)
;; Round a bigfloat to n significant digits.
(defvar hydra_lib_math_round_bigfloat hydra_lib_math_round_float64)

;; signum :: Int -> Int
;; Return the sign of a number (-1, 0, or 1).
(defvar hydra_lib_math_signum
  (lambda (n) (signum n)))

;; sin :: Double -> Double
;; Return the sine of x radians.
(defvar hydra_lib_math_sin
  (lambda (x) (sin (float x 1.0d0))))

;; sinh :: Double -> Double
;; Return the hyperbolic sine of x.
(defvar hydra_lib_math_sinh
  (lambda (x) (sinh (float x 1.0d0))))

;; sqrt :: Double -> Double
;; Return the square root of x.
(defvar hydra_lib_math_sqrt
  (lambda (x) (sqrt (float x 1.0d0))))

;; sub :: Int -> Int -> Int
;; Subtract two numbers.
(defvar hydra_lib_math_sub
  (lambda (a)
    (lambda (b)
      (- a b))))

;; succ :: Int -> Int
;; Return the successor (x + 1).
(defvar hydra_lib_math_succ
  (lambda (n) (1+ n)))

;; tan :: Double -> Double
;; Return the tangent of x radians.
(defvar hydra_lib_math_tan
  (lambda (x) (tan (float x 1.0d0))))

;; tanh :: Double -> Double
;; Return the hyperbolic tangent of x.
(defvar hydra_lib_math_tanh
  (lambda (x) (tanh (float x 1.0d0))))

;; truncate :: Double -> BigInt
;; Return x truncated to an integer (towards zero).
(defvar hydra_lib_math_truncate
  (lambda (x) (truncate x)))
