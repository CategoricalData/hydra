(in-package :cl-user)

;; abs :: Int -> Int
(defvar hydra_lib_math_abs
  (lambda (n) (abs n)))

;; acos :: Double -> Double
(defvar hydra_lib_math_acos
  (lambda (x) (acos (float x 1.0d0))))

;; acosh :: Double -> Double
(defvar hydra_lib_math_acosh
  (lambda (x) (acosh (float x 1.0d0))))

;; add :: Int -> Int -> Int
(defvar hydra_lib_math_add
  (lambda (a)
    (lambda (b)
      (+ a b))))

;; asin :: Double -> Double
(defvar hydra_lib_math_asin
  (lambda (x) (asin (float x 1.0d0))))

;; asinh :: Double -> Double
(defvar hydra_lib_math_asinh
  (lambda (x) (asinh (float x 1.0d0))))

;; atan :: Double -> Double
(defvar hydra_lib_math_atan
  (lambda (x) (atan (float x 1.0d0))))

;; atan2 :: Double -> Double -> Double
(defvar hydra_lib_math_atan2
  (lambda (y)
    (lambda (x)
      (atan (float y 1.0d0) (float x 1.0d0)))))

;; atanh :: Double -> Double
(defvar hydra_lib_math_atanh
  (lambda (x) (atanh (float x 1.0d0))))

;; ceiling :: Double -> BigInt
(defvar hydra_lib_math_ceiling
  (lambda (x) (ceiling x)))

;; cos :: Double -> Double
(defvar hydra_lib_math_cos
  (lambda (x) (cos (float x 1.0d0))))

;; cosh :: Double -> Double
(defvar hydra_lib_math_cosh
  (lambda (x) (cosh (float x 1.0d0))))

;; div :: Int -> Int -> Int  (floor division)
(defvar hydra_lib_math_div
  (lambda (a)
    (lambda (b)
      (floor a b))))

;; e :: Double
(defvar hydra_lib_math_e (exp 1.0d0))

;; even :: Int -> Bool
(defvar hydra_lib_math_even
  (lambda (n) (evenp n)))

;; exp :: Double -> Double
(defvar hydra_lib_math_exp
  (lambda (x) (exp (float x 1.0d0))))

;; floor :: Double -> BigInt
(defvar hydra_lib_math_floor
  (lambda (x) (floor x)))

;; log :: Double -> Double
(defvar hydra_lib_math_log
  (lambda (x) (log (float x 1.0d0))))

;; logBase :: Double -> Double -> Double
(defvar hydra_lib_math_logBase
  (lambda (base)
    (lambda (x)
      (/ (log (float x 1.0d0)) (log (float base 1.0d0))))))

;; log_base alias
(defvar hydra_lib_math_log_base hydra_lib_math_logBase)

;; max :: Int -> Int -> Int
(defvar hydra_lib_math_max
  (lambda (a)
    (lambda (b)
      (max a b))))

;; maybe_div :: Int -> Int -> Maybe Int
(defvar hydra_lib_math_maybe_div
  (lambda (a)
    (lambda (b)
      (if (= b 0)
          (list :nothing)
          (list :just (floor a b))))))

;; maybe_mod :: Int -> Int -> Maybe Int
(defvar hydra_lib_math_maybe_mod
  (lambda (a)
    (lambda (b)
      (if (= b 0)
          (list :nothing)
          (list :just (mod a b))))))

;; maybe_pred :: Int -> Maybe Int
(defvar hydra_lib_math_maybe_pred
  (lambda (n)
    (if (= n -2147483648)
        (list :nothing)
        (list :just (1- n)))))

;; maybe_rem :: Int -> Int -> Maybe Int
(defvar hydra_lib_math_maybe_rem
  (lambda (a)
    (lambda (b)
      (if (= b 0)
          (list :nothing)
          (list :just (rem a b))))))

;; maybe_succ :: Int -> Maybe Int
(defvar hydra_lib_math_maybe_succ
  (lambda (n)
    (if (= n 2147483647)
        (list :nothing)
        (list :just (1+ n)))))

;; min :: Int -> Int -> Int
(defvar hydra_lib_math_min
  (lambda (a)
    (lambda (b)
      (min a b))))

;; mod :: Int -> Int -> Int  (floor mod)
(defvar hydra_lib_math_mod
  (lambda (a)
    (lambda (b)
      (mod a b))))

;; mul :: Int -> Int -> Int
(defvar hydra_lib_math_mul
  (lambda (a)
    (lambda (b)
      (* a b))))

;; negate :: Int -> Int
(defvar hydra_lib_math_negate
  (lambda (a)
    (- a)))

;; odd :: Int -> Bool
(defvar hydra_lib_math_odd
  (lambda (n) (oddp n)))

;; pi :: Double
(defvar hydra_lib_math_pi (float pi 1.0d0))

;; pow :: Double -> Double -> Double
(defvar hydra_lib_math_pow
  (lambda (base)
    (lambda (exp-val)
      (expt (float base 1.0d0) (float exp-val 1.0d0)))))

;; pred :: Int -> Int
(defvar hydra_lib_math_pred
  (lambda (n) (1- n)))

;; range :: Int -> Int -> [Int]  (inclusive both ends)
(defvar hydra_lib_math_range
  (lambda (start)
    (lambda (end)
      (loop for i from start to end collect i))))

;; rem :: Int -> Int -> Int  (truncating remainder)
(defvar hydra_lib_math_rem
  (lambda (a)
    (lambda (b)
      (rem a b))))

;; round :: Double -> BigInt
(defvar hydra_lib_math_round
  (lambda (x) (round x)))

;; signum :: Int -> Int
(defvar hydra_lib_math_signum
  (lambda (n) (signum n)))

;; sin :: Double -> Double
(defvar hydra_lib_math_sin
  (lambda (x) (sin (float x 1.0d0))))

;; sinh :: Double -> Double
(defvar hydra_lib_math_sinh
  (lambda (x) (sinh (float x 1.0d0))))

;; sqrt :: Double -> Double
(defvar hydra_lib_math_sqrt
  (lambda (x) (sqrt (float x 1.0d0))))

;; sub :: Int -> Int -> Int
(defvar hydra_lib_math_sub
  (lambda (a)
    (lambda (b)
      (- a b))))

;; succ :: Int -> Int
(defvar hydra_lib_math_succ
  (lambda (n) (1+ n)))

;; tan :: Double -> Double
(defvar hydra_lib_math_tan
  (lambda (x) (tan (float x 1.0d0))))

;; tanh :: Double -> Double
(defvar hydra_lib_math_tanh
  (lambda (x) (tanh (float x 1.0d0))))

;; truncate :: Double -> BigInt
(defvar hydra_lib_math_truncate
  (lambda (x) (truncate x)))

;; roundFloat64 :: Int -> Double -> Double
(defvar hydra_lib_math_round_float64
  (lambda (n)
    (lambda (x)
      (if (= x 0.0d0) 0.0d0
          (let* ((fx (float x 1.0d0))
                 (factor (expt 10.0d0 (- n 1 (floor (log (abs fx) 10))))))
            (/ (round (* fx factor)) factor))))))

;; roundFloat32 :: Int -> Float -> Float
;; Round to n significant digits, then snap to IEEE 754 float32 precision
(defvar hydra_lib_math_round_float32
  (lambda (n)
    (lambda (x)
      (if (= x 0.0) 0.0d0
          (let* ((fx (float x 1.0d0))
                 (factor (expt 10.0d0 (- n 1 (floor (log (abs fx) 10)))))
                 (rounded (float (/ (round (* fx factor)) factor) 1.0d0)))
            ;; Snap to float32 precision by round-tripping through single-float
            (float (float rounded 1.0f0) 1.0d0))))))

;; roundBigfloat :: Int -> Double -> Double  (alias for roundFloat64)
(defvar hydra_lib_math_round_bigfloat hydra_lib_math_round_float64)
