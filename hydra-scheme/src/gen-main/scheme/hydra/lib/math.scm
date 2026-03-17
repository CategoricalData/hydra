(define-library (hydra lib math)
  (import (scheme base)
          (scheme inexact))
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

    ;; abs :: Int -> Int
    (define hydra_lib_math_abs
      (lambda (n) (abs n)))

    ;; acos :: Double -> Double
    (define hydra_lib_math_acos
      (lambda (x) (acos x)))

    ;; acosh :: Double -> Double
    (define hydra_lib_math_acosh
      (lambda (x) (acosh x)))

    ;; add :: Int -> Int -> Int
    (define hydra_lib_math_add
      (lambda (a)
        (lambda (b)
          (+ a b))))

    ;; asin :: Double -> Double
    (define hydra_lib_math_asin
      (lambda (x) (asin x)))

    ;; asinh :: Double -> Double
    (define hydra_lib_math_asinh
      (lambda (x) (asinh x)))

    ;; atan :: Double -> Double
    (define hydra_lib_math_atan
      (lambda (x) (atan x)))

    ;; atan2 :: Double -> Double -> Double
    (define hydra_lib_math_atan2
      (lambda (y)
        (lambda (x)
          (atan y x))))

    ;; atanh :: Double -> Double
    (define hydra_lib_math_atanh
      (lambda (x) (atanh x)))

    ;; ceiling :: Double -> BigInt
    (define hydra_lib_math_ceiling
      (lambda (x) (exact (ceiling x))))

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

    ;; floor :: Double -> BigInt
    (define hydra_lib_math_floor
      (lambda (x) (exact (floor x))))

    ;; log :: Double -> Double
    (define hydra_lib_math_log
      (lambda (x) (log x)))

    ;; logBase :: Double -> Double -> Double
    (define hydra_lib_math_logBase
      (lambda (base)
        (lambda (x)
          (/ (log x) (log base)))))

    (define hydra_lib_math_log_base hydra_lib_math_logBase)

    ;; max :: Int -> Int -> Int
    (define hydra_lib_math_max
      (lambda (a)
        (lambda (b)
          (max a b))))

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
    (define hydra_lib_math_pow
      (lambda (base)
        (lambda (exp_)
          (expt base exp_))))

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

    ;; round :: Double -> BigInt
    (define hydra_lib_math_round
      (lambda (x) (exact (round x))))

    ;; signum :: Int -> Int
    (define hydra_lib_math_signum
      (lambda (n) (cond ((positive? n) 1) ((negative? n) -1) (else 0))))

    ;; sin :: Double -> Double
    (define hydra_lib_math_sin
      (lambda (x) (sin x)))

    ;; sinh :: Double -> Double
    (define hydra_lib_math_sinh
      (lambda (x) (sinh x)))

    ;; sqrt :: Double -> Double
    (define hydra_lib_math_sqrt
      (lambda (x) (sqrt x)))

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
    (define hydra_lib_math_tanh
      (lambda (x) (tanh x)))

    ;; truncate :: Double -> BigInt
    (define hydra_lib_math_truncate
      (lambda (x) (exact (truncate x))))))
