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

    ;; Return the absolute value.
    (define hydra_lib_math_abs
      (lambda (n) (abs n)))

    ;; Return the arc cosine of x in radians.
    (define hydra_lib_math_acos
      (lambda (x) (acos x)))

    ;; Return the inverse hyperbolic cosine of x.
    (define hydra_lib_math_acosh
      (lambda (x) (acosh x)))

    ;; Add two numbers.
    (define hydra_lib_math_add
      (lambda (a)
        (lambda (b)
          (+ a b))))

    ;; Return the arc sine of x in radians.
    (define hydra_lib_math_asin
      (lambda (x) (asin x)))

    ;; Return the inverse hyperbolic sine of x.
    (define hydra_lib_math_asinh
      (lambda (x) (asinh x)))

    ;; Return the arc tangent of x in radians.
    (define hydra_lib_math_atan
      (lambda (x) (atan x)))

    ;; Return the arc tangent of y/x in radians, using signs to determine quadrant.
    (define hydra_lib_math_atan2
      (lambda (y)
        (lambda (x)
          (atan y x))))

    ;; Return the inverse hyperbolic tangent of x.
    (define hydra_lib_math_atanh
      (lambda (x) (atanh x)))

    ;; Return the ceiling of x as an integer.
    (define hydra_lib_math_ceiling
      (lambda (x) (exact (ceiling x))))

    ;; Return the cosine of x radians.
    (define hydra_lib_math_cos
      (lambda (x) (cos x)))

    ;; Return the hyperbolic cosine of x.
    (define hydra_lib_math_cosh
      (lambda (x) (cosh x)))

    ;; Divide two integers using integer division.
    (define hydra_lib_math_div
      (lambda (a)
        (lambda (b)
          (floor-quotient a b))))

    ;; Euler's number (e ~ 2.71828).
    (define hydra_lib_math_e (exp 1))

    ;; Check if an integer is even.
    (define hydra_lib_math_even
      (lambda (n) (even? n)))

    ;; Return e raised to the power x.
    (define hydra_lib_math_exp
      (lambda (x) (exp x)))

    ;; Return the floor of x as an integer.
    (define hydra_lib_math_floor
      (lambda (x) (exact (floor x))))

    ;; Return the natural logarithm of x.
    (define hydra_lib_math_log
      (lambda (x) (log x)))

    ;; Return the logarithm of x to the given base.
    (define hydra_lib_math_logBase
      (lambda (base)
        (lambda (x)
          (/ (log x) (log base)))))

    (define hydra_lib_math_log_base hydra_lib_math_logBase)

    ;; Return the maximum of two values.
    (define hydra_lib_math_max
      (lambda (a)
        (lambda (b)
          (max a b))))

    ;; Return the minimum of two values.
    (define hydra_lib_math_min
      (lambda (a)
        (lambda (b)
          (min a b))))

    ;; Mathematical modulo.
    (define hydra_lib_math_mod
      (lambda (a)
        (lambda (b)
          (floor-remainder a b))))

    ;; Multiply two numbers.
    (define hydra_lib_math_mul
      (lambda (a)
        (lambda (b)
          (* a b))))

    ;; Negate a number.
    (define hydra_lib_math_negate
      (lambda (a)
        (- a)))

    ;; Check if an integer is odd.
    (define hydra_lib_math_odd
      (lambda (n) (odd? n)))

    ;; Pi (~ 3.14159).
    (define hydra_lib_math_pi (* 4 (atan 1)))

    ;; Return x raised to the power y.
    (define hydra_lib_math_pow
      (lambda (base)
        (lambda (exp_)
          (expt base exp_))))

    ;; Return the predecessor (x - 1).
    (define hydra_lib_math_pred
      (lambda (n) (- n 1)))

    ;; Generate a range of values from start to end (inclusive).
    (define hydra_lib_math_range
      (lambda (start)
        (lambda (end)
          (let loop ((i start) (acc '()))
            (if (> i end)
                (reverse acc)
                (loop (+ i 1) (cons i acc)))))))

    ;; Integer remainder.
    (define hydra_lib_math_rem
      (lambda (a)
        (lambda (b)
          (truncate-remainder a b))))

    ;; Return x rounded to the nearest integer.
    (define hydra_lib_math_round
      (lambda (x) (exact (round x))))

    ;; Round a bigfloat to n significant digits.
    (define hydra_lib_math_round_bigfloat hydra_lib_math_round_float64)

    ;; Round a float32 to n significant digits.
    ;; Rounds to N significant digits, then snaps through IEEE float32
    (define hydra_lib_math_round_float32
      (lambda (n)
        (lambda (x)
          (snap-to-float32 ((hydra_lib_math_round_float64 n) x)))))

    ;; Round a float64 to n significant digits.
    (define hydra_lib_math_round_float64
      (lambda (n)
        (lambda (x)
          (if (= x 0.0)
              0.0
              (let ((factor (expt 10.0 (- n 1 (exact (floor (/ (log (abs x)) (log 10))))))))
                (/ (inexact (round (* x factor))) factor))))))

    ;; Return the sign of a number (-1, 0, or 1).
    (define hydra_lib_math_signum
      (lambda (n) (cond ((positive? n) 1) ((negative? n) -1) (else 0))))

    ;; Return the sine of x radians.
    (define hydra_lib_math_sin
      (lambda (x) (sin x)))

    ;; Return the hyperbolic sine of x.
    (define hydra_lib_math_sinh
      (lambda (x) (sinh x)))

    ;; Return the square root of x.
    (define hydra_lib_math_sqrt
      (lambda (x) (sqrt x)))

    ;; Subtract two numbers.
    (define hydra_lib_math_sub
      (lambda (a)
        (lambda (b)
          (- a b))))

    ;; Return the successor (x + 1).
    (define hydra_lib_math_succ
      (lambda (n) (+ n 1)))

    ;; Return the tangent of x radians.
    (define hydra_lib_math_tan
      (lambda (x) (tan x)))

    ;; Return the hyperbolic tangent of x.
    (define hydra_lib_math_tanh
      (lambda (x) (tanh x)))

    ;; Return x truncated to an integer (towards zero).
    (define hydra_lib_math_truncate
      (lambda (x) (exact (truncate x))))))
