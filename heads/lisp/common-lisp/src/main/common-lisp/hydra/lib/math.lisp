(in-package :cl-user)

;; Common Lisp's trig/log functions return complex numbers for out-of-domain
;; real inputs; Hydra's semantics (per Haskell / IEEE 754) require NaN/±Inf.
;; These helpers provide NaN/Inf constants and predicates, plus guards
;; against out-of-domain inputs.
(defvar +hydra-pos-inf+
  (handler-case (/ 1.0d0 0.0d0)
    (error () #+sbcl sb-ext:double-float-positive-infinity #-sbcl 1.0d308)))
(defvar +hydra-neg-inf+ (- +hydra-pos-inf+))
(defvar +hydra-nan+
  (handler-case (- +hydra-pos-inf+ +hydra-pos-inf+)
    (error () 0.0d0)))

(declaim (inline hydra-nan-p hydra-inf-p))
(defun hydra-nan-p (x)
  ;; NaN is the only value not equal to itself.
  (and (numberp x) (realp x) (not (= x x))))
(defun hydra-inf-p (x)
  ;; Infinity is the only nonzero value equal to twice itself.
  (and (numberp x) (realp x) (= x x) (not (= x 0)) (= x (* 2 x))))

;; abs :: Int -> Int
(defvar hydra_lib_math_abs
  (lambda (n) (abs n)))

;; acos :: Double -> Double  (domain [-1, 1]; out-of-domain -> NaN)
(defvar hydra_lib_math_acos
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) +hydra-nan+)
            ((or (< fx -1.0d0) (> fx 1.0d0)) +hydra-nan+)
            (t (acos fx))))))

;; acosh :: Double -> Double  (domain [1, +inf); out-of-domain -> NaN)
(defvar hydra_lib_math_acosh
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) +hydra-nan+)
            ((< fx 1.0d0) +hydra-nan+)
            ((and (hydra-inf-p fx) (> fx 0)) +hydra-pos-inf+)
            (t (acosh fx))))))

;; add :: Int -> Int -> Int
(defvar hydra_lib_math_add
  (lambda (a)
    (lambda (b)
      (+ a b))))

;; addFloat64 :: Double -> Double -> Double
(defvar hydra_lib_math_add_float64
  (lambda (a)
    (lambda (b)
      (+ (float a 1.0d0) (float b 1.0d0)))))

;; asin :: Double -> Double  (domain [-1, 1]; out-of-domain -> NaN)
(defvar hydra_lib_math_asin
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) +hydra-nan+)
            ((or (< fx -1.0d0) (> fx 1.0d0)) +hydra-nan+)
            (t (asin fx))))))

;; asinh :: Double -> Double  (unrestricted domain)
(defvar hydra_lib_math_asinh
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) +hydra-nan+)
            ((hydra-inf-p fx) fx)
            (t (asinh fx))))))

;; atan :: Double -> Double
(defvar hydra_lib_math_atan
  (lambda (x) (atan (float x 1.0d0))))

;; atan2 :: Double -> Double -> Double
;; Match Haskell: atan2 returns NaN when both arguments are infinite.
(defvar hydra_lib_math_atan2
  (lambda (y)
    (lambda (x)
      (let ((fy (float y 1.0d0)) (fx (float x 1.0d0)))
        (if (and (hydra-inf-p fy) (hydra-inf-p fx))
            +hydra-nan+
            (atan fy fx))))))

;; atanh :: Double -> Double  (domain (-1, 1); boundary -> ±Inf; |x|>1 -> NaN)
(defvar hydra_lib_math_atanh
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) +hydra-nan+)
            ((< fx -1.0d0) +hydra-nan+)
            ((> fx 1.0d0) +hydra-nan+)
            ((= fx 1.0d0) +hydra-pos-inf+)
            ((= fx -1.0d0) +hydra-neg-inf+)
            (t (atanh fx))))))

;; ceiling :: Double -> Double
;; DIVERGENCE FROM HASKELL: Hydra returns a float, not an integer, so that
;; NaN/Inf propagate naturally per IEEE 754.
(defvar hydra_lib_math_ceiling
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) fx)
            ((hydra-inf-p fx) fx)
            (t (float (ceiling fx) 1.0d0))))))

;; cos :: Double -> Double
(defvar hydra_lib_math_cos
  (lambda (x) (cos (float x 1.0d0))))

;; cosh :: Double -> Double
(defvar hydra_lib_math_cosh
  (lambda (x) (cosh (float x 1.0d0))))

;; e :: Double
(defvar hydra_lib_math_e (exp 1.0d0))

;; even :: Int -> Bool
(defvar hydra_lib_math_even
  (lambda (n) (evenp n)))

;; exp :: Double -> Double
(defvar hydra_lib_math_exp
  (lambda (x) (exp (float x 1.0d0))))

;; floor :: Double -> Double
;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
(defvar hydra_lib_math_floor
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) fx)
            ((hydra-inf-p fx) fx)
            (t (float (floor fx) 1.0d0))))))

;; log :: Double -> Double  (domain (0, +inf); x=0 -> -Inf; x<0 -> NaN)
(defvar hydra_lib_math_log
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) +hydra-nan+)
            ((< fx 0.0d0) +hydra-nan+)
            ((= fx 0.0d0) +hydra-neg-inf+)
            ((and (hydra-inf-p fx) (< fx 0)) +hydra-nan+)
            ((and (hydra-inf-p fx) (> fx 0)) +hydra-pos-inf+)
            (t (log fx))))))

;; logBase :: Double -> Double -> Double
;; Defined via the guarded log, so NaN/Inf compose correctly.
(defvar hydra_lib_math_logBase
  (lambda (base)
    (lambda (x)
      (/ (funcall hydra_lib_math_log x) (funcall hydra_lib_math_log base)))))

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

;; mul :: Int -> Int -> Int
(defvar hydra_lib_math_mul
  (lambda (a)
    (lambda (b)
      (* a b))))

;; mulFloat64 :: Double -> Double -> Double
(defvar hydra_lib_math_mul_float64
  (lambda (a)
    (lambda (b)
      (* (float a 1.0d0) (float b 1.0d0)))))

;; negate :: Int -> Int
(defvar hydra_lib_math_negate
  (lambda (a)
    (- a)))

;; negateFloat64 :: Double -> Double
(defvar hydra_lib_math_negate_float64
  (lambda (a)
    (- (float a 1.0d0))))

;; odd :: Int -> Bool
(defvar hydra_lib_math_odd
  (lambda (n) (oddp n)))

;; pi :: Double
(defvar hydra_lib_math_pi (float pi 1.0d0))

;; pow :: Double -> Double -> Double
(defvar hydra_lib_math_pow
  (lambda (base)
    (lambda (exp-val)
      (let ((b (float base 1.0d0)) (e (float exp-val 1.0d0)))
        (cond
          ((and (zerop b) (zerop e)) 1.0d0)
          ((and (zerop b) (< e 0.0d0)) +hydra-pos-inf+)
          (t
           (handler-case
             (let ((r (expt b e)))
               (if (complexp r) +hydra-nan+ r))
             (arithmetic-error () +hydra-nan+))))))))

;; range :: Int -> Int -> [Int]  (inclusive both ends)
(defvar hydra_lib_math_range
  (lambda (start)
    (lambda (end)
      (loop for i from start to end collect i))))

;; round :: Double -> Double
;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
(defvar hydra_lib_math_round
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) fx)
            ((hydra-inf-p fx) fx)
            (t (float (round fx) 1.0d0))))))

;; signum :: Int -> Int
(defvar hydra_lib_math_signum
  (lambda (n) (signum n)))

;; sin :: Double -> Double
(defvar hydra_lib_math_sin
  (lambda (x) (sin (float x 1.0d0))))

;; sinh :: Double -> Double
(defvar hydra_lib_math_sinh
  (lambda (x) (sinh (float x 1.0d0))))

;; sqrt :: Double -> Double  (domain [0, +inf); x<0 -> NaN)
(defvar hydra_lib_math_sqrt
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) +hydra-nan+)
            ((< fx 0.0d0) +hydra-nan+)
            ((and (hydra-inf-p fx) (< fx 0)) +hydra-nan+)
            (t (sqrt fx))))))

;; sub :: Int -> Int -> Int
(defvar hydra_lib_math_sub
  (lambda (a)
    (lambda (b)
      (- a b))))

;; subFloat64 :: Double -> Double -> Double
(defvar hydra_lib_math_sub_float64
  (lambda (a)
    (lambda (b)
      (- (float a 1.0d0) (float b 1.0d0)))))


;; tan :: Double -> Double
(defvar hydra_lib_math_tan
  (lambda (x) (tan (float x 1.0d0))))

;; tanh :: Double -> Double
(defvar hydra_lib_math_tanh
  (lambda (x) (tanh (float x 1.0d0))))

;; truncate :: Double -> Double
;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
(defvar hydra_lib_math_truncate
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) fx)
            ((hydra-inf-p fx) fx)
            (t (float (truncate fx) 1.0d0))))))

;; roundFloat64 :: Int -> Double -> Double
;; Returns NaN/Inf inputs unchanged (no rounding is possible).
(defvar hydra_lib_math_round_float64
  (lambda (n)
    (lambda (x)
      (let ((fx (float x 1.0d0)))
        (cond ((hydra-nan-p fx) fx)
              ((hydra-inf-p fx) fx)
              ((= fx 0.0d0) 0.0d0)
              (t (let ((factor (expt 10.0d0 (- n 1 (floor (log (abs fx) 10))))))
                   (/ (round (* fx factor)) factor))))))))

;; roundFloat32 :: Int -> Float -> Float
;; Round to n significant digits, then snap to IEEE 754 float32 precision.
;; Returns NaN/Inf inputs unchanged (no rounding is possible).
(defvar hydra_lib_math_round_float32
  (lambda (n)
    (lambda (x)
      (let ((fx (float x 1.0d0)))
        (cond ((hydra-nan-p fx) fx)
              ((hydra-inf-p fx) fx)
              ((= fx 0.0d0) 0.0d0)
              (t (let* ((factor (expt 10.0d0 (- n 1 (floor (log (abs fx) 10)))))
                        (rounded (float (/ (round (* fx factor)) factor) 1.0d0)))
                   ;; Snap to float32 precision by round-tripping through single-float
                   (float (float rounded 1.0f0) 1.0d0))))))))

;; roundBigfloat :: Int -> Double -> Double  (alias for roundFloat64)
(defvar hydra_lib_math_round_bigfloat hydra_lib_math_round_float64)
