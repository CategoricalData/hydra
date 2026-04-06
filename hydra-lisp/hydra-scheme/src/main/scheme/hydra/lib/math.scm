(define-library (hydra lib math)
  (import (scheme base)
          (scheme inexact)
          (scheme bytevector))
  (export hydra_lib_math_abs
          hydra_lib_math_acos
          hydra_lib_math_acosh
          hydra_lib_math_add
          hydra_lib_math_asin
          hydra_lib_math_asinh
          hydra_lib_math_atan
          hydra_lib_math_atan2
          hydra_lib_math_atanh
          hydra_lib_math_ceiling
          hydra_lib_math_cos
          hydra_lib_math_cosh
          hydra_lib_math_div
          hydra_lib_math_e
          hydra_lib_math_even
          hydra_lib_math_exp
          hydra_lib_math_floor
          hydra_lib_math_log
          hydra_lib_math_logBase
          hydra_lib_math_log_base
          hydra_lib_math_max
          hydra_lib_math_maybe_div
          hydra_lib_math_maybe_mod
          hydra_lib_math_maybe_pred
          hydra_lib_math_maybe_rem
          hydra_lib_math_maybe_succ
          hydra_lib_math_min
          hydra_lib_math_mod
          hydra_lib_math_mul
          hydra_lib_math_negate
          hydra_lib_math_odd
          hydra_lib_math_pi
          hydra_lib_math_pow
          hydra_lib_math_pred
          hydra_lib_math_range
          hydra_lib_math_rem
          hydra_lib_math_round
          hydra_lib_math_round_bigfloat
          hydra_lib_math_round_float32
          hydra_lib_math_round_float64
          hydra_lib_math_signum
          hydra_lib_math_sin
          hydra_lib_math_sinh
          hydra_lib_math_sqrt
          hydra_lib_math_sub
          hydra_lib_math_succ
          hydra_lib_math_tan
          hydra_lib_math_tanh
          hydra_lib_math_truncate)
  (begin

    ;; Hyperbolic functions (not in R7RS, defined inline)
    (define (sinh x) (/ (- (exp x) (exp (- x))) 2))
    (define (cosh x) (/ (+ (exp x) (exp (- x))) 2))
    (define (tanh x) (/ (sinh x) (cosh x)))
    (define (asinh x) (log (+ x (sqrt (+ (* x x) 1)))))
    (define (acosh x) (log (+ x (sqrt (- (* x x) 1)))))
    (define (atanh x) (/ (log (/ (+ 1 x) (- 1 x))) 2))

    ;; Scheme's trig/log functions can return complex numbers for out-of-domain
    ;; real inputs. Hydra's semantics (per Haskell / IEEE 754) require NaN or
    ;; ±Inf in such cases. These helpers guard against out-of-domain inputs
    ;; and return the appropriate real result.

    ;; abs :: Int -> Int
    (define hydra_lib_math_abs
      (lambda (n) (abs n)))

    ;; acos :: Double -> Double  (domain [-1, 1]; out-of-domain -> NaN)
    (define hydra_lib_math_acos
      (lambda (x)
        (if (or (nan? x) (< x -1.0) (> x 1.0))
            +nan.0
            (acos x))))

    ;; acosh :: Double -> Double  (domain [1, +inf); out-of-domain -> NaN)
    (define hydra_lib_math_acosh
      (lambda (x)
        (cond ((nan? x) +nan.0)
              ((< x 1.0) +nan.0)
              ((= x +inf.0) +inf.0)
              (else (acosh x)))))

    ;; add :: Int -> Int -> Int
    (define hydra_lib_math_add
      (lambda (a)
        (lambda (b)
          (+ a b))))

    ;; asin :: Double -> Double  (domain [-1, 1]; out-of-domain -> NaN)
    (define hydra_lib_math_asin
      (lambda (x)
        (if (or (nan? x) (< x -1.0) (> x 1.0))
            +nan.0
            (asin x))))

    ;; asinh :: Double -> Double  (unrestricted domain)
    (define hydra_lib_math_asinh
      (lambda (x)
        (cond ((nan? x) +nan.0)
              ((= x +inf.0) +inf.0)
              ((= x -inf.0) -inf.0)
              (else (asinh x)))))

    ;; atan :: Double -> Double  (unrestricted domain)
    (define hydra_lib_math_atan
      (lambda (x) (atan x)))

    ;; atan2 :: Double -> Double -> Double
    ;; Match Haskell: atan2 returns NaN when both arguments are infinite
    ;; (Scheme's two-arg atan returns ±pi/4 or ±3pi/4 in these cases).
    (define hydra_lib_math_atan2
      (lambda (y)
        (lambda (x)
          (if (and (infinite? y) (infinite? x))
              +nan.0
              (atan y x)))))

    ;; atanh :: Double -> Double  (domain (-1, 1); boundary -> ±Inf; |x|>1 -> NaN)
    (define hydra_lib_math_atanh
      (lambda (x)
        (cond ((nan? x) +nan.0)
              ((< x -1.0) +nan.0)
              ((> x 1.0) +nan.0)
              ((= x 1.0) +inf.0)
              ((= x -1.0) -inf.0)
              (else (atanh x)))))

    ;; ceiling :: Double -> Double
    ;; DIVERGENCE FROM HASKELL: Hydra returns a float, not an integer, so that
    ;; NaN/Inf propagate naturally per IEEE 754.
    (define hydra_lib_math_ceiling
      (lambda (x)
        (if (or (nan? x) (infinite? x))
            x
            (inexact (ceiling x)))))

    ;; cos :: Double -> Double
    (define hydra_lib_math_cos
      (lambda (x) (cos x)))

    ;; cosh :: Double -> Double
    (define hydra_lib_math_cosh
      (lambda (x) (cosh x)))

    ;; div :: Int -> Int -> Int  (floor division)
    (define hydra_lib_math_div
      (lambda (a)
        (lambda (b)
          (floor-quotient a b))))

    ;; e :: Double
    (define hydra_lib_math_e (exp 1))

    ;; even :: Int -> Bool
    (define hydra_lib_math_even
      (lambda (n) (even? n)))

    ;; exp :: Double -> Double
    (define hydra_lib_math_exp
      (lambda (x) (exp x)))

    ;; floor :: Double -> Double
    ;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
    (define hydra_lib_math_floor
      (lambda (x)
        (if (or (nan? x) (infinite? x))
            x
            (inexact (floor x)))))

    ;; log :: Double -> Double  (domain (0, +inf); x=0 -> -Inf; x<0 -> NaN)
    (define hydra_lib_math_log
      (lambda (x)
        (cond ((nan? x) +nan.0)
              ((< x 0.0) +nan.0)
              ((= x 0.0) -inf.0)
              ((= x -inf.0) +nan.0)
              (else (log x)))))

    ;; logBase :: Double -> Double -> Double
    ;; Defined via the guarded log, so NaN/Inf compose correctly.
    (define hydra_lib_math_logBase
      (lambda (base)
        (lambda (x)
          (/ (hydra_lib_math_log x) (hydra_lib_math_log base)))))

    (define hydra_lib_math_log_base hydra_lib_math_logBase)

    ;; max :: Int -> Int -> Int
    (define hydra_lib_math_max
      (lambda (a)
        (lambda (b)
          (max a b))))

    ;; maybe_div :: Int -> Int -> Maybe Int
    (define hydra_lib_math_maybe_div
      (lambda (a)
        (lambda (b)
          (if (= b 0)
              (list 'nothing)
              (list 'just (floor-quotient a b))))))

    ;; maybe_mod :: Int -> Int -> Maybe Int
    (define hydra_lib_math_maybe_mod
      (lambda (a)
        (lambda (b)
          (if (= b 0)
              (list 'nothing)
              (list 'just (floor-remainder a b))))))

    ;; maybe_pred :: Int -> Maybe Int
    (define hydra_lib_math_maybe_pred
      (lambda (n)
        (if (= n -2147483648)
            (list 'nothing)
            (list 'just (- n 1)))))

    ;; maybe_rem :: Int -> Int -> Maybe Int
    (define hydra_lib_math_maybe_rem
      (lambda (a)
        (lambda (b)
          (if (= b 0)
              (list 'nothing)
              (list 'just (truncate-remainder a b))))))

    ;; maybe_succ :: Int -> Maybe Int
    (define hydra_lib_math_maybe_succ
      (lambda (n)
        (if (= n 2147483647)
            (list 'nothing)
            (list 'just (+ n 1)))))

    ;; min :: Int -> Int -> Int
    (define hydra_lib_math_min
      (lambda (a)
        (lambda (b)
          (min a b))))

    ;; mod :: Int -> Int -> Int  (floor mod)
    (define hydra_lib_math_mod
      (lambda (a)
        (lambda (b)
          (floor-remainder a b))))

    ;; mul :: Int -> Int -> Int
    (define hydra_lib_math_mul
      (lambda (a)
        (lambda (b)
          (* a b))))

    ;; negate :: Int -> Int
    (define hydra_lib_math_negate
      (lambda (a)
        (- a)))

    ;; odd :: Int -> Bool
    (define hydra_lib_math_odd
      (lambda (n) (odd? n)))

    ;; pi :: Double
    (define hydra_lib_math_pi (* 4 (atan 1)))

    ;; pow :: Double -> Double -> Double
    ;; Scheme's expt can return complex numbers (e.g. negative base with
    ;; fractional exponent); Haskell's (**) returns NaN in such cases.
    ;; pow :: Double -> Double -> Double
    ;; Match Haskell's (**): 0^negative = Inf, complex results -> NaN
    (define hydra_lib_math_pow
      (lambda (base)
        (lambda (exp_)
          (let ((b (* 1.0 base)) (e (* 1.0 exp_)))
            (cond
              ;; 0^negative = Infinity (Guile returns NaN)
              ((and (= b 0.0) (< e 0.0)) +inf.0)
              (else
                (let ((result (expt b e)))
                  (if (real? result)
                      result
                      +nan.0))))))))

    ;; pred :: Int -> Int
    (define hydra_lib_math_pred
      (lambda (n) (- n 1)))

    ;; range :: Int -> Int -> [Int]  (inclusive both ends)
    (define hydra_lib_math_range
      (lambda (start)
        (lambda (end)
          (let loop ((i start) (acc '()))
            (if (> i end)
                (reverse acc)
                (loop (+ i 1) (cons i acc)))))))

    ;; rem :: Int -> Int -> Int  (truncating remainder)
    (define hydra_lib_math_rem
      (lambda (a)
        (lambda (b)
          (truncate-remainder a b))))

    ;; round :: Double -> Double
    ;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
    (define hydra_lib_math_round
      (lambda (x)
        (if (or (nan? x) (infinite? x))
            x
            (inexact (round x)))))

    ;; roundFloat64 :: Int -> Double -> Double
    ;; Returns NaN/Inf inputs unchanged (no rounding is possible).
    (define hydra_lib_math_round_float64
      (lambda (n)
        (lambda (x)
          (cond ((or (nan? x) (infinite? x)) x)
                ((= x 0.0) 0.0)
                (else
                 (let ((factor (expt 10.0 (- n 1 (exact (floor (/ (log (abs x)) (log 10))))))))
                   (/ (inexact (round (* x factor))) factor)))))))

    ;; roundFloat32 :: Int -> Float -> Float
    ;; Rounds to N significant digits, then snaps through IEEE float32
    (define hydra_lib_math_round_float32
      (lambda (n)
        (lambda (x)
          (snap-to-float32 ((hydra_lib_math_round_float64 n) x)))))

    ;; roundBigfloat :: Int -> Double -> Double  (alias for roundFloat64)
    (define hydra_lib_math_round_bigfloat hydra_lib_math_round_float64)

    ;; signum :: Int -> Int
    (define hydra_lib_math_signum
      (lambda (n) (cond ((positive? n) 1) ((negative? n) -1) (else 0))))

    ;; sin :: Double -> Double
    (define hydra_lib_math_sin
      (lambda (x) (sin x)))

    ;; sinh :: Double -> Double
    (define hydra_lib_math_sinh
      (lambda (x) (sinh x)))

    ;; sqrt :: Double -> Double  (domain [0, +inf); x<0 -> NaN)
    (define hydra_lib_math_sqrt
      (lambda (x)
        (cond ((nan? x) +nan.0)
              ((< x 0.0) +nan.0)
              ((= x -inf.0) +nan.0)
              (else (sqrt x)))))

    ;; sub :: Int -> Int -> Int
    (define hydra_lib_math_sub
      (lambda (a)
        (lambda (b)
          (- a b))))

    ;; succ :: Int -> Int
    (define hydra_lib_math_succ
      (lambda (n) (+ n 1)))

    ;; tan :: Double -> Double
    (define hydra_lib_math_tan
      (lambda (x) (tan x)))

    ;; tanh :: Double -> Double
    ;; The custom tanh (sinh/cosh) returns NaN for ±Inf because Inf/Inf = NaN.
    ;; Haskell's tanh returns ±1.0 at the infinities.
    (define hydra_lib_math_tanh
      (lambda (x)
        (cond ((nan? x) +nan.0)
              ((= x +inf.0) 1.0)
              ((= x -inf.0) -1.0)
              (else (tanh x)))))

    ;; truncate :: Double -> Double
    ;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
    (define hydra_lib_math_truncate
      (lambda (x)
        (if (or (nan? x) (infinite? x))
            x
            (inexact (truncate x)))))))
