(define-library (hydra overlay scheme lib math)
  (import (scheme base)
          (scheme inexact)
          (scheme bytevector))
  (export hydra_overlay_scheme_lib_math_abs
          hydra_overlay_scheme_lib_math_acos
          hydra_overlay_scheme_lib_math_acosh
          hydra_overlay_scheme_lib_math_add
          hydra_overlay_scheme_lib_math_add_float64
          hydra_overlay_scheme_lib_math_asin
          hydra_overlay_scheme_lib_math_asinh
          hydra_overlay_scheme_lib_math_atan
          hydra_overlay_scheme_lib_math_atan2
          hydra_overlay_scheme_lib_math_atanh
          hydra_overlay_scheme_lib_math_ceiling
          hydra_overlay_scheme_lib_math_cos
          hydra_overlay_scheme_lib_math_cosh
          hydra_overlay_scheme_lib_math_e
          hydra_overlay_scheme_lib_math_even
          hydra_overlay_scheme_lib_math_exp
          hydra_overlay_scheme_lib_math_floor
          hydra_overlay_scheme_lib_math_log
          hydra_overlay_scheme_lib_math_logBase
          hydra_overlay_scheme_lib_math_log_base
          hydra_overlay_scheme_lib_math_div
          hydra_overlay_scheme_lib_math_divide
          hydra_overlay_scheme_lib_math_mod
          hydra_overlay_scheme_lib_math_rem
          hydra_overlay_scheme_lib_math_mul
          hydra_overlay_scheme_lib_math_mul_float64
          hydra_overlay_scheme_lib_math_negate
          hydra_overlay_scheme_lib_math_negate_float64
          hydra_overlay_scheme_lib_math_odd
          hydra_overlay_scheme_lib_math_pi
          hydra_overlay_scheme_lib_math_pow
          hydra_overlay_scheme_lib_math_range
          hydra_overlay_scheme_lib_math_round
          hydra_overlay_scheme_lib_math_round_float32
          hydra_overlay_scheme_lib_math_round_float64
          hydra_overlay_scheme_lib_math_signum
          hydra_overlay_scheme_lib_math_sin
          hydra_overlay_scheme_lib_math_sinh
          hydra_overlay_scheme_lib_math_sqrt
          hydra_overlay_scheme_lib_math_sub
          hydra_overlay_scheme_lib_math_sub_float64
          hydra_overlay_scheme_lib_math_tan
          hydra_overlay_scheme_lib_math_tanh
          hydra_overlay_scheme_lib_math_truncate)
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
             (let ((r (float-op (cadr vx) (cadr vy))))
               (list 'literal (list 'float (list vx-tag (if (eq? vx-tag 'float32) (snap-to-float32 r) r)))))))
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
             (let ((r (float-op (cadr vx))))
               (list 'literal (list 'float (list vx-tag (if (eq? vx-tag 'float32) (snap-to-float32 r) r)))))))
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

    ;; --- Constraint-polymorphic ('integral') dispatch for div/mod/rem/even/odd ---
    ;;
    ;; div/mod are floor-based (sign follows the divisor); rem is truncated (sign follows the
    ;; dividend) -- mirroring the Haskell/Java/Python/Scala/TypeScript/Clojure hosts' div/mod vs rem
    ;; split. The int-op is one of R7RS's floor-quotient/floor-remainder/truncate-remainder, which
    ;; already give exactly these semantics. All three guard the zero-divisor case (returning 'none)
    ;; before computing. The (minBound, -1) boundary needs an explicit wrap-to-minBound on div only;
    ;; mod/rem have no overflow there. Both call contracts (Term-path vs native-value path) are
    ;; supported, same dispatch-by-shape convention as numeric-binary/numeric-unary above.

    (define (integral-literal op-name t)
      (if (and (eq? (car t) 'literal) (eq? (car (cadr t)) 'integer))
          (cadr (cadr t))
          (error (string-append "hydra.lib.math." op-name ": expected an integer literal term"))))

    (define (integral-binary-term op-name int-op wrap-min-boundary-on-div x y)
      (let* ((vx (integral-literal op-name x))
             (vy (integral-literal op-name y))
             (vx-tag (car vx)) (vy-tag (car vy)))
        (if (not (eq? vx-tag vy-tag))
            (error (string-append "hydra.lib.math." op-name ": integer operands differ in precision")))
        (let ((a (cadr vx)) (b (cadr vy)))
          (if (= b 0)
              (list 'none)
              (let* ((signed (int-width-bits vx-tag))
                     (r (if (and wrap-min-boundary-on-div signed)
                            (let ((min-bound (- (expt 2 (- (int-width-bits vx-tag) 1)))))
                              (if (and (= a min-bound) (= b -1)) min-bound (int-op a b)))
                            (int-op a b))))
                (list 'given (list 'literal (list 'integer (list vx-tag (wrap-int vx-tag r))))))))))

    (define (integral-binary-native op-name int-op wrap-min-boundary-on-div a b)
      (if (= b 0)
          (list 'none)
          (list 'given (int-op a b))))

    (define (integral-binary op-name int-op wrap-min-boundary-on-div)
      (lambda (x)
        (lambda (y)
          (if (pair? x)
              (integral-binary-term op-name int-op wrap-min-boundary-on-div x y)
              (integral-binary-native op-name int-op wrap-min-boundary-on-div x y)))))

    (define (even-or-odd op-name want-even)
      (lambda (x)
        (let ((n (if (pair? x) (cadr (integral-literal op-name x)) x)))
          (eq? (even? n) want-even))))

    ;; --- Constraint-polymorphic ('fractional') dispatch for divide ---
    ;;
    ;; float32/float64 only, IEEE-total (division by zero yields ±Infinity/NaN, not an exception --
    ;; Scheme's own inexact `/` already gives these sentinels for free, e.g. (/ 1.0 0.0) => +inf.0).

    (define (divide-term x y)
      (let* ((lx (numeric-literal "divide" x))
             (ly (numeric-literal "divide" y)))
        (if (or (not (eq? (car lx) 'float)) (not (eq? (car ly) 'float)))
            (error "hydra.lib.math.divide: operands are not the same fractional kind"))
        (let* ((vx (cadr lx)) (vy (cadr ly)) (vx-tag (car vx)) (vy-tag (car vy)))
          (if (not (eq? vx-tag vy-tag))
              (error "hydra.lib.math.divide: float operands differ in precision"))
          (let ((r (/ (* 1.0 (cadr vx)) (* 1.0 (cadr vy)))))
            (list 'literal (list 'float (list vx-tag (if (eq? vx-tag 'float32) (snap-to-float32 r) r))))))))

    (define (divide-dispatch x)
      (lambda (y)
        (if (pair? x)
            (divide-term x y)
            (/ (* 1.0 x) (* 1.0 y)))))

    ;; abs :: numeric x => x -> x
    (define hydra_overlay_scheme_lib_math_abs
      (numeric-unary "abs" (lambda (a) (abs a)) (lambda (a) (abs a))))

    ;; acos :: Double -> Double  (domain [-1, 1]; out-of-domain -> NaN)
    (define hydra_overlay_scheme_lib_math_acos
      (lambda (x)
        (if (or (nan? x) (< x -1.0) (> x 1.0))
            +nan.0
            (acos x))))

    ;; acosh :: Double -> Double  (domain [1, +inf); out-of-domain -> NaN)
    (define hydra_overlay_scheme_lib_math_acosh
      (lambda (x)
        (cond ((nan? x) +nan.0)
              ((< x 1.0) +nan.0)
              ((= x +inf.0) +inf.0)
              (else (acosh x)))))

    ;; add :: numeric x => x -> x -> x
    (define hydra_overlay_scheme_lib_math_add
      (numeric-binary "add" + +))

    ;; addFloat64 :: Double -> Double -> Double
    (define hydra_overlay_scheme_lib_math_add_float64
      (lambda (a)
        (lambda (b)
          (+ (* 1.0 a) (* 1.0 b)))))

    ;; asin :: Double -> Double  (domain [-1, 1]; out-of-domain -> NaN)
    (define hydra_overlay_scheme_lib_math_asin
      (lambda (x)
        (if (or (nan? x) (< x -1.0) (> x 1.0))
            +nan.0
            (asin x))))

    ;; asinh :: Double -> Double  (unrestricted domain)
    (define hydra_overlay_scheme_lib_math_asinh
      (lambda (x)
        (cond ((nan? x) +nan.0)
              ((= x +inf.0) +inf.0)
              ((= x -inf.0) -inf.0)
              (else (asinh x)))))

    ;; atan :: Double -> Double  (unrestricted domain)
    (define hydra_overlay_scheme_lib_math_atan
      (lambda (x) (atan x)))

    ;; atan2 :: Double -> Double -> Double
    ;; Match Haskell: atan2 returns NaN when both arguments are infinite
    ;; (Scheme's two-arg atan returns ±pi/4 or ±3pi/4 in these cases).
    (define hydra_overlay_scheme_lib_math_atan2
      (lambda (y)
        (lambda (x)
          (if (and (infinite? y) (infinite? x))
              +nan.0
              (atan y x)))))

    ;; atanh :: Double -> Double  (domain (-1, 1); boundary -> ±Inf; |x|>1 -> NaN)
    (define hydra_overlay_scheme_lib_math_atanh
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
    (define hydra_overlay_scheme_lib_math_ceiling
      (lambda (x)
        (if (or (nan? x) (infinite? x))
            x
            (inexact (ceiling x)))))

    ;; cos :: Double -> Double
    (define hydra_overlay_scheme_lib_math_cos
      (lambda (x) (cos x)))

    ;; cosh :: Double -> Double
    (define hydra_overlay_scheme_lib_math_cosh
      (lambda (x) (cosh x)))

    ;; e :: Double
    (define hydra_overlay_scheme_lib_math_e (exp 1))

    ;; even :: integral x => x -> Bool
    (define hydra_overlay_scheme_lib_math_even
      (even-or-odd "even" #t))

    ;; exp :: Double -> Double
    (define hydra_overlay_scheme_lib_math_exp
      (lambda (x) (exp x)))

    ;; floor :: Double -> Double
    ;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
    (define hydra_overlay_scheme_lib_math_floor
      (lambda (x)
        (if (or (nan? x) (infinite? x))
            x
            (inexact (floor x)))))

    ;; log :: Double -> Double  (domain (0, +inf); x=0 -> -Inf; x<0 -> NaN)
    (define hydra_overlay_scheme_lib_math_log
      (lambda (x)
        (cond ((nan? x) +nan.0)
              ((< x 0.0) +nan.0)
              ((= x 0.0) -inf.0)
              ((= x -inf.0) +nan.0)
              (else (log x)))))

    ;; logBase :: Double -> Double -> Double
    ;; Defined via the guarded log, so NaN/Inf compose correctly.
    (define hydra_overlay_scheme_lib_math_logBase
      (lambda (base)
        (lambda (x)
          (/ (hydra_overlay_scheme_lib_math_log x) (hydra_overlay_scheme_lib_math_log base)))))

    (define hydra_overlay_scheme_lib_math_log_base hydra_overlay_scheme_lib_math_logBase)

    ;; div :: integral x => x -> x -> optional x
    (define hydra_overlay_scheme_lib_math_div
      (integral-binary "div" floor-quotient #t))

    ;; divide :: fractional x => x -> x -> x
    (define hydra_overlay_scheme_lib_math_divide
      divide-dispatch)

    ;; mod :: integral x => x -> x -> optional x
    (define hydra_overlay_scheme_lib_math_mod
      (integral-binary "mod" floor-remainder #f))

    ;; rem :: integral x => x -> x -> optional x
    (define hydra_overlay_scheme_lib_math_rem
      (integral-binary "rem" truncate-remainder #f))

    ;; mul :: numeric x => x -> x -> x
    (define hydra_overlay_scheme_lib_math_mul
      (numeric-binary "mul" * *))

    ;; mulFloat64 :: Double -> Double -> Double
    (define hydra_overlay_scheme_lib_math_mul_float64
      (lambda (a)
        (lambda (b)
          (* (* 1.0 a) (* 1.0 b)))))

    ;; negate :: numeric x => x -> x
    (define hydra_overlay_scheme_lib_math_negate
      (numeric-unary "negate" - -))

    ;; negateFloat64 :: Double -> Double
    (define hydra_overlay_scheme_lib_math_negate_float64
      (lambda (a)
        (- (* 1.0 a))))

    ;; odd :: integral x => x -> Bool
    (define hydra_overlay_scheme_lib_math_odd
      (even-or-odd "odd" #f))

    ;; pi :: Double
    (define hydra_overlay_scheme_lib_math_pi (* 4 (atan 1)))

    ;; pow :: Double -> Double -> Double
    ;; Scheme's expt can return complex numbers (e.g. negative base with
    ;; fractional exponent); Haskell's (**) returns NaN in such cases.
    ;; pow :: Double -> Double -> Double
    ;; Match Haskell's (**): 0^negative = Inf, complex results -> NaN
    (define hydra_overlay_scheme_lib_math_pow
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

    ;; range :: Int -> Int -> [Int]  (half-open, [start, end))
    (define hydra_overlay_scheme_lib_math_range
      (lambda (start)
        (lambda (end)
          (let loop ((i start) (acc '()))
            (if (>= i end)
                (reverse acc)
                (loop (+ i 1) (cons i acc)))))))

    ;; round :: Double -> Double
    ;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
    (define hydra_overlay_scheme_lib_math_round
      (lambda (x)
        (if (or (nan? x) (infinite? x))
            x
            (inexact (round x)))))

    ;; roundFloat64 :: Int -> Double -> Double
    ;; Returns NaN/Inf inputs unchanged (no rounding is possible).
    (define hydra_overlay_scheme_lib_math_round_float64
      (lambda (n)
        (lambda (x)
          (cond ((or (nan? x) (infinite? x)) x)
                ((= x 0.0) 0.0)
                (else
                 (let ((factor (expt 10.0 (- n 1 (exact (floor (/ (log (abs x)) (log 10))))))))
                   (/ (inexact (round (* x factor))) factor)))))))

    ;; roundFloat32 :: Int -> Float -> Float
    ;; Rounds to N significant digits, then snaps through IEEE float32
    (define hydra_overlay_scheme_lib_math_round_float32
      (lambda (n)
        (lambda (x)
          (snap-to-float32 ((hydra_overlay_scheme_lib_math_round_float64 n) x)))))

    ;; signum :: numeric x => x -> x
    ;; Returns -1/0/1 for integers; for floats, preserves the sign of zero (signum(-0.0) = -0.0)
    ;; and propagates NaN, matching the other hosts' float signum.
    (define hydra_overlay_scheme_lib_math_signum
      (numeric-unary "signum"
        (lambda (a) (cond ((positive? a) 1) ((negative? a) -1) (else 0)))
        (lambda (a)
          (cond ((nan? a) a)
                ((> a 0.0) 1.0)
                ((< a 0.0) -1.0)
                ;; a is ±0.0: preserve the sign of zero
                (else a)))))

    ;; sin :: Double -> Double
    (define hydra_overlay_scheme_lib_math_sin
      (lambda (x) (sin x)))

    ;; sinh :: Double -> Double
    (define hydra_overlay_scheme_lib_math_sinh
      (lambda (x) (sinh x)))

    ;; sqrt :: Double -> Double  (domain [0, +inf); x<0 -> NaN)
    (define hydra_overlay_scheme_lib_math_sqrt
      (lambda (x)
        (cond ((nan? x) +nan.0)
              ((< x 0.0) +nan.0)
              ((= x -inf.0) +nan.0)
              (else (sqrt x)))))

    ;; sub :: numeric x => x -> x -> x
    (define hydra_overlay_scheme_lib_math_sub
      (numeric-binary "sub" - -))

    ;; subFloat64 :: Double -> Double -> Double
    (define hydra_overlay_scheme_lib_math_sub_float64
      (lambda (a)
        (lambda (b)
          (- (* 1.0 a) (* 1.0 b)))))

    ;; tan :: Double -> Double
    (define hydra_overlay_scheme_lib_math_tan
      (lambda (x) (tan x)))

    ;; tanh :: Double -> Double
    ;; The custom tanh (sinh/cosh) returns NaN for ±Inf because Inf/Inf = NaN.
    ;; Haskell's tanh returns ±1.0 at the infinities.
    (define hydra_overlay_scheme_lib_math_tanh
      (lambda (x)
        (cond ((nan? x) +nan.0)
              ((= x +inf.0) 1.0)
              ((= x -inf.0) -1.0)
              (else (tanh x)))))

    ;; truncate :: Double -> Double
    ;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
    (define hydra_overlay_scheme_lib_math_truncate
      (lambda (x)
        (if (or (nan? x) (infinite? x))
            x
            (inexact (truncate x)))))))
