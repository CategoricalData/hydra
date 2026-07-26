(define-library (hydra overlay scheme lib math)
  (import (scheme base)
          (scheme inexact)
          (scheme bytevector))
  (export hydra_lib_math_abs
          hydra_lib_math_acos
          hydra_lib_math_acosh
          hydra_lib_math_add
          hydra_lib_math_add_float64
          hydra_lib_math_asin
          hydra_lib_math_asinh
          hydra_lib_math_atan
          hydra_lib_math_atan2
          hydra_lib_math_atanh
          hydra_lib_math_ceiling
          hydra_lib_math_cos
          hydra_lib_math_cosh
          hydra_lib_math_e
          hydra_lib_math_even
          hydra_lib_math_exp
          hydra_lib_math_floor
          hydra_lib_math_log
          hydra_lib_math_logBase
          hydra_lib_math_log_base
          hydra_lib_math_div
          hydra_lib_math_mod
          hydra_lib_math_rem
          hydra_lib_math_mul
          hydra_lib_math_mul_float64
          hydra_lib_math_negate
          hydra_lib_math_negate_float64
          hydra_lib_math_odd
          hydra_lib_math_pi
          hydra_lib_math_pow
          hydra_lib_math_range
          hydra_lib_math_round
          hydra_lib_math_round_float32
          hydra_lib_math_round_float64
          hydra_lib_math_signum
          hydra_lib_math_sin
          hydra_lib_math_sinh
          hydra_lib_math_sqrt
          hydra_lib_math_sub
          hydra_lib_math_sub_float64
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

    ;; Constraint-polymorphic ('numeric') dispatch for add/sub/mul/negate.
    ;;
    ;; These vars serve two distinct call contracts, both real:
    ;;
    ;;  1. The interpreter path: registered as hydra.lib.math.add etc. via prim2/prim1 with a
    ;;     'numeric' class constraint and identity (Term) coders (tc-variable) in libraries.scm's
    ;;     register-math, so on that path the compute fn receives raw Terms -- list-encoded, e.g.
    ;;     (literal (integer (int32 42))) -- and must dispatch on the operand's literal variant.
    ;;  2. The generated-kernel-code path: this SAME binding is called directly by Hydra's own
    ;;     self-hosted Scheme source (e.g. hydra.names, hydra.lexical) as ordinary arithmetic over
    ;;     raw native numbers -- no Term involved at all.
    ;;
    ;; This mirrors the Haskell host's numericBinary/numericUnary (Hydra.Overlay.Haskell.Lib.Math)
    ;; and Java's NumericDispatch, except Java splits the two contracts onto two differently-typed
    ;; methods on one class; Scheme has one flat function namespace, so both contracts are
    ;; dispatched here by argument shape instead: a Term literal is a pair (a list), a native
    ;; number is not. No typeclass mechanism is consulted at runtime -- the host has none. Type
    ;; inference guarantees both operands of a binary op share one numeric type, so the Term-path
    ;; dispatch keys on the first operand and requires the second to match; a mismatch or a
    ;; non-numeric operand is an internal invariant violation and fails loudly.
    ;;
    ;; Fixed-width integer variants are narrowed back to the source width (two's-complement
    ;; wraparound, mirroring Java's NumericDispatch.rewrapInteger); bigint is arbitrary precision.
    ;; The native-value path needs no narrowing: Scheme's own numeric tower already gives the
    ;; right per-type behavior for the native types in play.

    (define (int-width-bits tag)
      (cond ((eq? tag 'int8) 8) ((eq? tag 'int16) 16) ((eq? tag 'int32) 32) ((eq? tag 'int64) 64)
            ((eq? tag 'uint8) 8) ((eq? tag 'uint16) 16) ((eq? tag 'uint32) 32) ((eq? tag 'uint64) 64)
            (else #f)))

    (define (unsigned-int-width? tag)
      (or (eq? tag 'uint8) (eq? tag 'uint16) (eq? tag 'uint32) (eq? tag 'uint64)))

    (define (wrap-int width-tag r)
      (if (eq? width-tag 'bigint)
          r
          (let* ((bits (int-width-bits width-tag))
                 (m (expt 2 bits)))
            (if (unsigned-int-width? width-tag)
                (modulo r m)
                (let ((w (modulo r m)))
                  (if (>= w (/ m 2)) (- w m) w))))))

    (define (numeric-literal op-name t)
      (if (eq? (car t) 'literal)
          (cadr t)
          (error (string-append "hydra.lib.math." op-name ": expected a numeric literal term"))))

    (define (numeric-binary-term op-name int-op float-op x y)
      (let* ((lx (numeric-literal op-name x))
             (ly (numeric-literal op-name y))
             (lx-kind (car lx))
             (ly-kind (car ly)))
        (if (not (eq? lx-kind ly-kind))
            (error (string-append "hydra.lib.math." op-name ": operands are not the same numeric kind")))
        (cond
          ((eq? lx-kind 'integer)
           (let* ((vx (cadr lx)) (vy (cadr ly)) (vx-tag (car vx)) (vy-tag (car vy)))
             (if (not (eq? vx-tag vy-tag))
                 (error (string-append "hydra.lib.math." op-name ": integer operands differ in precision")))
             (list 'literal (list 'integer (list vx-tag (wrap-int vx-tag (int-op (cadr vx) (cadr vy))))))))
          ((eq? lx-kind 'float)
           (let* ((vx (cadr lx)) (vy (cadr ly)) (vx-tag (car vx)) (vy-tag (car vy)))
             (if (not (eq? vx-tag vy-tag))
                 (error (string-append "hydra.lib.math." op-name ": float operands differ in precision")))
             (list 'literal (list 'float (list vx-tag (float-op (cadr vx) (cadr vy)))))))
          (else (error (string-append "hydra.lib.math." op-name ": operand is not numeric"))))))

    (define (numeric-unary-term op-name int-op float-op x)
      (let* ((lx (numeric-literal op-name x))
             (lx-kind (car lx)))
        (cond
          ((eq? lx-kind 'integer)
           (let* ((vx (cadr lx)) (vx-tag (car vx)))
             (list 'literal (list 'integer (list vx-tag (wrap-int vx-tag (int-op (cadr vx))))))))
          ((eq? lx-kind 'float)
           (let* ((vx (cadr lx)) (vx-tag (car vx)))
             (list 'literal (list 'float (list vx-tag (float-op (cadr vx)))))))
          (else (error (string-append "hydra.lib.math." op-name ": operand is not numeric"))))))

    (define (numeric-binary op-name int-op float-op)
      (lambda (x)
        (lambda (y)
          (if (pair? x)
              (numeric-binary-term op-name int-op float-op x y)
              (int-op x y)))))

    (define (numeric-unary op-name int-op float-op)
      (lambda (x)
        (if (pair? x)
            (numeric-unary-term op-name int-op float-op x)
            (int-op x))))

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

    ;; add :: numeric x => x -> x -> x
    (define hydra_lib_math_add
      (numeric-binary "add" + +))

    ;; addFloat64 :: Double -> Double -> Double
    (define hydra_lib_math_add_float64
      (lambda (a)
        (lambda (b)
          (+ (* 1.0 a) (* 1.0 b)))))

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

    ;; maybe_div :: Int -> Int -> Maybe Int
    (define hydra_lib_math_div
      (lambda (a)
        (lambda (b)
          (if (= b 0)
              (list 'none)
              (list 'given (floor-quotient a b))))))

    ;; maybe_mod :: Int -> Int -> Maybe Int
    (define hydra_lib_math_mod
      (lambda (a)
        (lambda (b)
          (if (= b 0)
              (list 'none)
              (list 'given (floor-remainder a b))))))

    ;; maybe_rem :: Int -> Int -> Maybe Int
    (define hydra_lib_math_rem
      (lambda (a)
        (lambda (b)
          (if (= b 0)
              (list 'none)
              (list 'given (truncate-remainder a b))))))

    ;; mul :: numeric x => x -> x -> x
    (define hydra_lib_math_mul
      (numeric-binary "mul" * *))

    ;; mulFloat64 :: Double -> Double -> Double
    (define hydra_lib_math_mul_float64
      (lambda (a)
        (lambda (b)
          (* (* 1.0 a) (* 1.0 b)))))

    ;; negate :: numeric x => x -> x
    (define hydra_lib_math_negate
      (numeric-unary "negate" - -))

    ;; negateFloat64 :: Double -> Double
    (define hydra_lib_math_negate_float64
      (lambda (a)
        (- (* 1.0 a))))

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

    ;; range :: Int -> Int -> [Int]  (inclusive both ends)
    (define hydra_lib_math_range
      (lambda (start)
        (lambda (end)
          (let loop ((i start) (acc '()))
            (if (> i end)
                (reverse acc)
                (loop (+ i 1) (cons i acc)))))))

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

    ;; sub :: numeric x => x -> x -> x
    (define hydra_lib_math_sub
      (numeric-binary "sub" - -))

    ;; subFloat64 :: Double -> Double -> Double
    (define hydra_lib_math_sub_float64
      (lambda (a)
        (lambda (b)
          (- (* 1.0 a) (* 1.0 b)))))

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
