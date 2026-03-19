;;; math.el --- Hydra math primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; abs :: Int -> Int
(defvar hydra_lib_math_abs
  (lambda (n) (abs n)))

;; acos :: Double -> Double
(defvar hydra_lib_math_acos
  (lambda (x) (acos (float x))))

;; acosh :: Double -> Double
;; acosh(x) = ln(x + sqrt(x^2 - 1))
(defvar hydra_lib_math_acosh
  (lambda (x)
    (let ((fx (float x)))
      (log (+ fx (sqrt (- (* fx fx) 1.0)))))))

;; add :: Int -> Int -> Int
(defvar hydra_lib_math_add
  (lambda (a)
    (lambda (b)
      (+ a b))))

;; asin :: Double -> Double
(defvar hydra_lib_math_asin
  (lambda (x) (asin (float x))))

;; asinh :: Double -> Double
;; asinh(x) = ln(x + sqrt(x^2 + 1))
(defvar hydra_lib_math_asinh
  (lambda (x)
    (let ((fx (float x)))
      (log (+ fx (sqrt (+ (* fx fx) 1.0)))))))

;; atan :: Double -> Double
(defvar hydra_lib_math_atan
  (lambda (x) (atan (float x))))

;; atan2 :: Double -> Double -> Double
(defvar hydra_lib_math_atan2
  (lambda (y)
    (lambda (x)
      (atan (float y) (float x)))))

;; atanh :: Double -> Double
;; atanh(x) = 0.5 * ln((1+x)/(1-x))
(defvar hydra_lib_math_atanh
  (lambda (x)
    (let ((fx (float x)))
      (* 0.5 (log (/ (+ 1.0 fx) (- 1.0 fx)))))))

;; ceiling :: Double -> BigInt
(defvar hydra_lib_math_ceiling
  (lambda (x) (ceiling x)))

;; cos :: Double -> Double
(defvar hydra_lib_math_cos
  (lambda (x) (cos (float x))))

;; cosh :: Double -> Double
;; cosh(x) = (e^x + e^(-x)) / 2
(defvar hydra_lib_math_cosh
  (lambda (x)
    (let ((fx (float x)))
      (/ (+ (exp fx) (exp (- fx))) 2.0))))

;; div :: Int -> Int -> Int  (floor division)
(defvar hydra_lib_math_div
  (lambda (a)
    (lambda (b)
      (floor a b))))

;; e :: Double
(defvar hydra_lib_math_e (exp 1.0))

;; even :: Int -> Bool
(defvar hydra_lib_math_even
  (lambda (n) (cl-evenp n)))

;; exp :: Double -> Double
(defvar hydra_lib_math_exp
  (lambda (x) (exp (float x))))

;; floor :: Double -> BigInt
(defvar hydra_lib_math_floor
  (lambda (x) (floor x)))

;; log :: Double -> Double
(defvar hydra_lib_math_log
  (lambda (x) (log (float x))))

;; logBase :: Double -> Double -> Double
(defvar hydra_lib_math_logBase
  (lambda (base)
    (lambda (x)
      (/ (log (float x)) (log (float base))))))

;; log_base alias
(defvar hydra_lib_math_log_base hydra_lib_math_logBase)

;; max :: Int -> Int -> Int
(defvar hydra_lib_math_max
  (lambda (a)
    (lambda (b)
      (max a b))))

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
  (lambda (n) (cl-oddp n)))

;; pi :: Double
(defvar hydra_lib_math_pi float-pi)

;; pow :: Double -> Double -> Double
(defvar hydra_lib_math_pow
  (lambda (base)
    (lambda (exp-val)
      (expt (float base) (float exp-val)))))

;; pred :: Int -> Int
(defvar hydra_lib_math_pred
  (lambda (n) (1- n)))

;; range :: Int -> Int -> [Int]  (inclusive both ends)
(defvar hydra_lib_math_range
  (lambda (start)
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
    (lambda (b)
      (% a b))))

;; round :: Double -> BigInt
(defvar hydra_lib_math_round
  (lambda (x) (round x)))

;; signum :: Int -> Int
(defvar hydra_lib_math_signum
  (lambda (n)
    (cond ((> n 0) 1) ((< n 0) -1) (t 0))))

;; sin :: Double -> Double
(defvar hydra_lib_math_sin
  (lambda (x) (sin (float x))))

;; sinh :: Double -> Double
;; sinh(x) = (e^x - e^(-x)) / 2
(defvar hydra_lib_math_sinh
  (lambda (x)
    (let ((fx (float x)))
      (/ (- (exp fx) (exp (- fx))) 2.0))))

;; sqrt :: Double -> Double
(defvar hydra_lib_math_sqrt
  (lambda (x) (sqrt (float x))))

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
  (lambda (x) (tan (float x))))

;; tanh :: Double -> Double
;; tanh(x) = sinh(x) / cosh(x) = (e^x - e^(-x)) / (e^x + e^(-x))
(defvar hydra_lib_math_tanh
  (lambda (x)
    (let* ((fx (float x))
           (ep (exp fx))
           (en (exp (- fx))))
      (/ (- ep en) (+ ep en)))))

;; truncate :: Double -> BigInt
(defvar hydra_lib_math_truncate
  (lambda (x) (truncate x)))

;; roundFloat64 :: Int -> Double -> Double
(defvar hydra_lib_math_round_float64
  (lambda (n)
    (lambda (x)
      (if (= x 0.0) 0.0
          (let* ((fx (float x))
                 (factor (expt 10.0 (- n 1 (floor (log (abs fx) 10))))))
            (/ (fround (* fx factor)) factor))))))

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
    (lambda (x)
      (snap-to-float32 (funcall (funcall hydra_lib_math_round_float64 n) x)))))

;; roundBigfloat :: Int -> Double -> Double  (alias for roundFloat64)
(defvar hydra_lib_math_round_bigfloat hydra_lib_math_round_float64)

(provide 'hydra.lib.math)
