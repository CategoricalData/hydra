;;; math.el --- Hydra math primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Helpers: detecting NaN and infinity.
;; Emacs Lisp's trig/log functions return NaN for out-of-domain real inputs
;; (matching IEEE 754), so explicit guards are only needed for the rounding
;; functions, which throw overflow-error on NaN/Inf.
(defsubst hydra--infinitep (x)
  "Non-nil if X is +Inf or -Inf."
  (or (= x 1.0e+INF) (= x -1.0e+INF)))

;; Emacs Lisp has only double-precision floats (no native single-precision type), so float32 is
;; represented as a double throughout, narrowed to what's representable at single precision by
;; explicitly rounding the mantissa to 24 bits and clamping magnitudes beyond float32's finite
;; range to +-Infinity (IEEE 754 overflow-to-infinity) -- without this, a double-precision result
;; like f32(0.1)+f32(0.2) or an overflowing f32 add would carry double-precision bits/magnitude
;; instead of the value an actual float32 computation would produce.
(defconst hydra--float32-max 3.4028234663852886e+38
  "The largest finite value representable in IEEE 754 binary32.")
(defconst hydra--float32-min-exponent -126
  "The minimum normal binary exponent for IEEE 754 binary32 (subnormals go below this).")
(defun hydra--to-float32 (x)
  "Snap a double to IEEE 754 float32 precision (24-bit mantissa), saturating overflow to
Infinity and flushing underflow (magnitudes below the smallest subnormal) to zero."
  (cond ((isnan x) x)
        ((hydra--infinitep x) x)
        ((= x 0.0) x)
        ((> (abs x) hydra--float32-max) (if (< x 0) -1.0e+INF 1.0e+INF))
        (t (let* ((sign (if (< x 0) -1.0 1.0))
                  (ax (abs x))
                  ;; Clamp the exponent at the subnormal floor: below hydra--float32-min-exponent,
                  ;; the mantissa scale must stop shrinking (matching IEEE 754's fixed subnormal
                  ;; step of 2^-149), or the naive (floor (log ax 2.0)) computation keeps scaling
                  ;; down and never rounds tiny-but-nonzero values to 0.0.
                  (e (max hydra--float32-min-exponent (floor (log ax 2.0))))
                  (scale (expt 2.0 (- 23 e)))
                  (mantissa (round (* ax scale)))
                  (snapped (* sign (/ mantissa scale))))
             ;; Rounding the mantissa up can itself push a near-max value past
             ;; hydra--float32-max (e.g. mantissa overflow at the top of the exponent range).
             (if (> (abs snapped) hydra--float32-max) (if (< x 0) -1.0e+INF 1.0e+INF) snapped)))))

;; Constraint-polymorphic ('numeric') dispatch for add/sub/mul/negate.
;;
;; These vars serve two distinct call contracts, both real:
;;
;;  1. The interpreter path: registered as hydra.lib.math.add etc. via prim2/prim1 with a
;;     'numeric' class constraint and identity (Term) coders (tc-variable) in libraries.el's
;;     register-math, so on that path the compute fn receives raw Terms -- list-encoded, e.g.
;;     (:literal (:integer (:int32 42))) -- and must dispatch on the operand's literal variant.
;;  2. The generated-kernel-code path: this SAME var is called directly by Hydra's own
;;     self-hosted Emacs Lisp source (e.g. hydra.names, hydra.lexical) as ordinary arithmetic
;;     over raw native numbers -- no Term involved at all.
;;
;; This mirrors the Haskell host's numericBinary/numericUnary (Hydra.Overlay.Haskell.Lib.Math)
;; and Java's NumericDispatch, except Java splits the two contracts onto two differently-typed
;; methods on one class; Emacs Lisp has one flat function namespace, so both contracts are
;; dispatched here by argument shape instead: a Term literal is a cons (a list), a native number
;; is not. No typeclass mechanism is consulted at runtime -- the host has none. Type inference
;; guarantees both operands of a binary op share one numeric type, so the Term-path dispatch keys
;; on the first operand and requires the second to match; a mismatch or a non-numeric operand is
;; an internal invariant violation and fails loudly.
;;
;; Fixed-width integer variants are narrowed back to the source width (two's-complement
;; wraparound, mirroring Java's NumericDispatch.rewrapInteger); bigint is arbitrary precision
;; (Emacs 27+ native bignums support this natively). The native-value path needs no narrowing:
;; Emacs Lisp's own numeric tower already gives the right per-type behavior for native types.

(defun hydra--int-width-bits (tag)
  (cond ((eq tag :int8) 8) ((eq tag :int16) 16) ((eq tag :int32) 32) ((eq tag :int64) 64)
        ((eq tag :uint8) 8) ((eq tag :uint16) 16) ((eq tag :uint32) 32) ((eq tag :uint64) 64)
        (t nil)))

(defun hydra--unsigned-int-width-p (tag)
  (memq tag '(:uint8 :uint16 :uint32 :uint64)))

(defun hydra--wrap-int (width-tag r)
  (if (eq width-tag :bigint)
      r
    (let* ((bits (hydra--int-width-bits width-tag))
           (m (ash 1 bits)))
      (if (hydra--unsigned-int-width-p width-tag)
          (mod r m)
        (let ((w (mod r m)))
          (if (>= w (/ m 2)) (- w m) w))))))

(defun hydra--numeric-literal (op-name term)
  (if (eq (car term) :literal)
      (cadr term)
    (error "hydra.lib.math.%s: expected a numeric literal term" op-name)))

(defun hydra--numeric-binary-term (op-name int-op float-op x y)
  (let* ((lx (hydra--numeric-literal op-name x))
         (ly (hydra--numeric-literal op-name y))
         (lx-kind (car lx))
         (ly-kind (car ly)))
    (unless (eq lx-kind ly-kind)
      (error "hydra.lib.math.%s: operands are not the same numeric kind" op-name))
    (cond
      ((eq lx-kind :integer)
       (let* ((vx (cadr lx)) (vy (cadr ly)) (vx-tag (car vx)) (vy-tag (car vy)))
         (unless (eq vx-tag vy-tag)
           (error "hydra.lib.math.%s: integer operands differ in precision" op-name))
         (list :literal (list :integer (list vx-tag (hydra--wrap-int vx-tag (funcall int-op (cadr vx) (cadr vy))))))))
      ((eq lx-kind :float)
       (let* ((vx (cadr lx)) (vy (cadr ly)) (vx-tag (car vx)) (vy-tag (car vy)))
         (unless (eq vx-tag vy-tag)
           (error "hydra.lib.math.%s: float operands differ in precision" op-name))
         (let ((r (funcall float-op (cadr vx) (cadr vy))))
           (list :literal (list :float (list vx-tag (if (eq vx-tag :float32) (hydra--to-float32 r) (float r))))))))
      (t (error "hydra.lib.math.%s: operand is not numeric" op-name)))))

(defun hydra--numeric-unary-term (op-name int-op float-op x)
  (let* ((lx (hydra--numeric-literal op-name x))
         (lx-kind (car lx)))
    (cond
      ((eq lx-kind :integer)
       (let* ((vx (cadr lx)) (vx-tag (car vx)))
         (list :literal (list :integer (list vx-tag (hydra--wrap-int vx-tag (funcall int-op (cadr vx))))))))
      ((eq lx-kind :float)
       (let* ((vx (cadr lx)) (vx-tag (car vx)))
         (let ((r (funcall float-op (cadr vx))))
           (list :literal (list :float (list vx-tag (if (eq vx-tag :float32) (hydra--to-float32 r) (float r))))))))
      (t (error "hydra.lib.math.%s: operand is not numeric" op-name)))))

(defun hydra--numeric-binary (op-name int-op float-op)
  (lambda (x)
    (lambda (y)
      (if (consp x)
          (hydra--numeric-binary-term op-name int-op float-op x y)
        (funcall int-op x y)))))

(defun hydra--numeric-unary (op-name int-op float-op)
  (lambda (x)
    (if (consp x)
        (hydra--numeric-unary-term op-name int-op float-op x)
      (funcall int-op x))))

;; --- Constraint-polymorphic ('integral') dispatch for div/mod/rem/even/odd ---
;;
;; div/mod are floor-based (sign follows the divisor); rem is truncated (sign follows the
;; dividend) -- mirroring the Haskell/Java/Python/Scala/TypeScript hosts' div/mod vs rem split.
;; Emacs Lisp's builtins match this directly: `floor'/`mod' are floor-based (like Python's //
;; and %), while `%' is truncated (like C). All three guard the zero-divisor case (returning
;; :none) before computing. The (minBound, -1) boundary needs an explicit wrap-to-minBound on div
;; only; mod/rem have no overflow there. Both call contracts (Term-path vs native-value path) are
;; supported, same dispatch-by-shape convention as numeric-binary/numeric-unary above.

(defun hydra--integral-literal (op-name term)
  (if (and (eq (car term) :literal) (eq (car (cadr term)) :integer))
      (cadr (cadr term))
    (error "hydra.lib.math.%s: expected an integer literal term" op-name)))

(defun hydra--integral-binary-term (op-name int-op wrap-min-boundary-on-div x y)
  (let* ((vx (hydra--integral-literal op-name x))
         (vy (hydra--integral-literal op-name y))
         (vx-tag (car vx)) (vy-tag (car vy)))
    (unless (eq vx-tag vy-tag)
      (error "hydra.lib.math.%s: integer operands differ in precision" op-name))
    (let ((a (cadr vx)) (b (cadr vy)))
      (if (= b 0)
          (list :none)
        (let* ((bits (hydra--int-width-bits vx-tag))
               (r (if (and wrap-min-boundary-on-div bits)
                      (let ((min-bound (- (ash 1 (1- bits)))))
                        (if (and (= a min-bound) (= b -1)) min-bound (funcall int-op a b)))
                    (funcall int-op a b))))
          (list :given (list :literal (list :integer (list vx-tag (hydra--wrap-int vx-tag r))))))))))

(defun hydra--integral-binary-native (op-name int-op wrap-min-boundary-on-div a b)
  (if (= b 0)
      (list :none)
    (list :given (funcall int-op a b))))

(defun hydra--integral-binary (op-name int-op wrap-min-boundary-on-div)
  (lambda (x)
    (lambda (y)
      (if (consp x)
          (hydra--integral-binary-term op-name int-op wrap-min-boundary-on-div x y)
        (hydra--integral-binary-native op-name int-op wrap-min-boundary-on-div x y)))))

(defun hydra--integral-to-bigint (v)
  (let ((tag (car v)) (n (cadr v)))
    (if (eq tag :uint8) (logand n #xff) n)))

(defun hydra--even-or-odd (op-name want-even)
  (lambda (x)
    (let ((n (if (consp x) (hydra--integral-to-bigint (hydra--integral-literal op-name x)) x)))
      (eq (cl-evenp n) want-even))))

;; --- Constraint-polymorphic ('fractional') dispatch for divide ---
;;
;; float32/float64 only, IEEE-total: division by zero yields ±Infinity/NaN, not an error.
;; Emacs Lisp's own `/' on floats already gives these sentinels for free (verified:
;; (/ 1.0 0.0) => 1.0e+INF, (/ 0.0 0.0) => NaN). Emacs Lisp has only double-precision
;; floats, so float32 is represented as a double, narrowed via hydra--to-float32.

(defun hydra--divide-term (x y)
  (let ((lx (hydra--numeric-literal "divide" x))
        (ly (hydra--numeric-literal "divide" y)))
    (unless (and (eq (car lx) :float) (eq (car ly) :float))
      (error "hydra.lib.math.divide: operands are not the same fractional kind"))
    (let* ((vx (cadr lx)) (vy (cadr ly)) (vx-tag (car vx)) (vy-tag (car vy)))
      (unless (eq vx-tag vy-tag)
        (error "hydra.lib.math.divide: float operands differ in precision"))
      (let ((r (/ (float (cadr vx)) (float (cadr vy)))))
        (list :literal (list :float (list vx-tag (if (eq vx-tag :float32) (hydra--to-float32 r) r))))))))

(defun hydra--divide-dispatch (x)
  (lambda (y)
    (if (consp x)
        (hydra--divide-term x y)
      (/ (float x) (float y)))))

;; abs :: numeric x => x -> x
(defvar hydra_overlay_emacs_lisp_lib_math_abs
  (hydra--numeric-unary "abs" (lambda (a) (abs a)) (lambda (a) (abs (float a)))))

;; acos :: Double -> Double
(defvar hydra_overlay_emacs_lisp_lib_math_acos
  (lambda (x) (acos (float x))))

;; acosh :: Double -> Double
;; acosh(x) = ln(x + sqrt(x^2 - 1))
(defvar hydra_overlay_emacs_lisp_lib_math_acosh
  (lambda (x)
    (let ((fx (float x)))
      (log (+ fx (sqrt (- (* fx fx) 1.0)))))))

;; add :: numeric x => x -> x -> x
(defvar hydra_overlay_emacs_lisp_lib_math_add
  (hydra--numeric-binary "add" #'+ #'+))

;; addFloat64 :: Double -> Double -> Double
(defvar hydra_overlay_emacs_lisp_lib_math_add_float64
  (lambda (a)
    (lambda (b)
      (+ (float a) (float b)))))

;; asin :: Double -> Double
(defvar hydra_overlay_emacs_lisp_lib_math_asin
  (lambda (x) (asin (float x))))

;; asinh :: Double -> Double
;; asinh(x) = ln(x + sqrt(x^2 + 1))
;; Special-case infinities: asinh(±Inf) = ±Inf (naive formula gives NaN for -Inf).
(defvar hydra_overlay_emacs_lisp_lib_math_asinh
  (lambda (x)
    (let ((fx (float x)))
      (if (hydra--infinitep fx)
          fx
        (log (+ fx (sqrt (+ (* fx fx) 1.0))))))))

;; atan :: Double -> Double
(defvar hydra_overlay_emacs_lisp_lib_math_atan
  (lambda (x) (atan (float x))))

;; atan2 :: Double -> Double -> Double
;; Match Haskell: atan2 returns NaN when both arguments are infinite.
(defvar hydra_overlay_emacs_lisp_lib_math_atan2
  (lambda (y)
    (lambda (x)
      (let ((fy (float y)) (fx (float x)))
        (if (and (hydra--infinitep fy) (hydra--infinitep fx))
            0.0e+NaN
          (atan fy fx))))))

;; atanh :: Double -> Double
;; atanh(x) = 0.5 * ln((1+x)/(1-x))
(defvar hydra_overlay_emacs_lisp_lib_math_atanh
  (lambda (x)
    (let ((fx (float x)))
      (* 0.5 (log (/ (+ 1.0 fx) (- 1.0 fx)))))))

;; ceiling :: Double -> Double
;; DIVERGENCE FROM HASKELL: Hydra returns a float, not an integer, so that
;; NaN/Inf propagate naturally per IEEE 754.
(defvar hydra_overlay_emacs_lisp_lib_math_ceiling
  (lambda (x)
    (let ((fx (float x)))
      (cond ((isnan fx) fx)
            ((hydra--infinitep fx) fx)
            (t (float (ceiling fx)))))))

;; cos :: Double -> Double
(defvar hydra_overlay_emacs_lisp_lib_math_cos
  (lambda (x) (cos (float x))))

;; cosh :: Double -> Double
;; cosh(x) = (e^x + e^(-x)) / 2
(defvar hydra_overlay_emacs_lisp_lib_math_cosh
  (lambda (x)
    (let ((fx (float x)))
      (/ (+ (exp fx) (exp (- fx))) 2.0))))

;; e :: Double
(defvar hydra_overlay_emacs_lisp_lib_math_e (exp 1.0))

;; even :: integral x => x -> Bool
(defvar hydra_overlay_emacs_lisp_lib_math_even
  (hydra--even-or-odd "even" t))

;; exp :: Double -> Double
(defvar hydra_overlay_emacs_lisp_lib_math_exp
  (lambda (x) (exp (float x))))

;; floor :: Double -> Double
;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
(defvar hydra_overlay_emacs_lisp_lib_math_floor
  (lambda (x)
    (let ((fx (float x)))
      (cond ((isnan fx) fx)
            ((hydra--infinitep fx) fx)
            (t (float (floor fx)))))))

;; log :: Double -> Double
(defvar hydra_overlay_emacs_lisp_lib_math_log
  (lambda (x) (log (float x))))

;; logBase :: Double -> Double -> Double
(defvar hydra_overlay_emacs_lisp_lib_math_logBase
  (lambda (base)
    (lambda (x)
      (/ (log (float x)) (log (float base))))))

;; log_base alias
(defvar hydra_overlay_emacs_lisp_lib_math_log_base hydra_overlay_emacs_lisp_lib_math_logBase)

;; div :: integral x => x -> x -> optional x
;; Floor division (`floor'); the (minBound, -1) boundary wraps to minBound (two's-complement).
(defvar hydra_overlay_emacs_lisp_lib_math_div
  (hydra--integral-binary "div" (lambda (a b) (floor a b)) t))

;; mod :: integral x => x -> x -> optional x
;; Floor modulus (`mod'; sign follows the divisor).
(defvar hydra_overlay_emacs_lisp_lib_math_mod
  (hydra--integral-binary "mod" (lambda (a b) (mod a b)) nil))

;; rem :: integral x => x -> x -> optional x
;; Truncated remainder (`%'; sign follows the dividend).
(defvar hydra_overlay_emacs_lisp_lib_math_rem
  (hydra--integral-binary "rem" (lambda (a b) (% a b)) nil))

;; divide :: fractional x => x -> x -> x
(defvar hydra_overlay_emacs_lisp_lib_math_divide
  #'hydra--divide-dispatch)

;; mul :: numeric x => x -> x -> x
(defvar hydra_overlay_emacs_lisp_lib_math_mul
  (hydra--numeric-binary "mul" #'* #'*))

;; mulFloat64 :: Double -> Double -> Double
(defvar hydra_overlay_emacs_lisp_lib_math_mul_float64
  (lambda (a)
    (lambda (b)
      (* (float a) (float b)))))

;; negate :: numeric x => x -> x
(defvar hydra_overlay_emacs_lisp_lib_math_negate
  (hydra--numeric-unary "negate" #'- #'-))

;; negateFloat64 :: Double -> Double
(defvar hydra_overlay_emacs_lisp_lib_math_negate_float64
  (lambda (a)
    (- (float a))))

;; odd :: integral x => x -> Bool
(defvar hydra_overlay_emacs_lisp_lib_math_odd
  (hydra--even-or-odd "odd" nil))

;; pi :: Double
(defvar hydra_overlay_emacs_lisp_lib_math_pi float-pi)

;; pow :: Double -> Double -> Double
(defvar hydra_overlay_emacs_lisp_lib_math_pow
  (lambda (base)
    (lambda (exp-val)
      (expt (float base) (float exp-val)))))

;; range :: Int -> Int -> [Int]  (half-open, [start, end))
(defvar hydra_overlay_emacs_lisp_lib_math_range
  (lambda (start)
    (lambda (end)
      (let ((acc nil))
        (let ((i start))
          (while (< i end)
            (push i acc)
            (setq i (1+ i))))
        (nreverse acc)))))

;; round :: Double -> Double
;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
(defvar hydra_overlay_emacs_lisp_lib_math_round
  (lambda (x)
    (let ((fx (float x)))
      (cond ((isnan fx) fx)
            ((hydra--infinitep fx) fx)
            (t (float (round fx)))))))

;; signum :: numeric x => x -> x
;; Returns -1/0/1 for integers; for floats, preserves the sign of zero (signum(-0.0) = -0.0)
;; and propagates NaN (signum(NaN) = NaN), matching Math/signum. `copysign' preserves -0.0;
;; the (= a 0.0) branch returns the operand itself, so a signed zero flows through unchanged.
(defvar hydra_overlay_emacs_lisp_lib_math_signum
  (hydra--numeric-unary "signum"
                        (lambda (a) (cond ((> a 0) 1) ((< a 0) -1) (t 0)))
                        (lambda (a) (cond ((isnan a) a) ((= a 0.0) a) (t (copysign 1.0 a))))))

;; sin :: Double -> Double
(defvar hydra_overlay_emacs_lisp_lib_math_sin
  (lambda (x) (sin (float x))))

;; sinh :: Double -> Double
;; sinh(x) = (e^x - e^(-x)) / 2
(defvar hydra_overlay_emacs_lisp_lib_math_sinh
  (lambda (x)
    (let ((fx (float x)))
      (/ (- (exp fx) (exp (- fx))) 2.0))))

;; sqrt :: Double -> Double
(defvar hydra_overlay_emacs_lisp_lib_math_sqrt
  (lambda (x) (sqrt (float x))))

;; sub :: numeric x => x -> x -> x
(defvar hydra_overlay_emacs_lisp_lib_math_sub
  (hydra--numeric-binary "sub" #'- #'-))

;; subFloat64 :: Double -> Double -> Double
(defvar hydra_overlay_emacs_lisp_lib_math_sub_float64
  (lambda (a)
    (lambda (b)
      (- (float a) (float b)))))

;; tan :: Double -> Double
(defvar hydra_overlay_emacs_lisp_lib_math_tan
  (lambda (x) (tan (float x))))

;; tanh :: Double -> Double
;; tanh(x) = sinh(x) / cosh(x) = (e^x - e^(-x)) / (e^x + e^(-x))
;; Special-case infinities: tanh(±Inf) = ±1.0 (naive formula gives NaN).
(defvar hydra_overlay_emacs_lisp_lib_math_tanh
  (lambda (x)
    (let ((fx (float x)))
      (cond ((hydra--infinitep fx) (if (> fx 0) 1.0 -1.0))
            (t (let ((ep (exp fx))
                     (en (exp (- fx))))
                 (/ (- ep en) (+ ep en))))))))

;; truncate :: Double -> Double
;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
(defvar hydra_overlay_emacs_lisp_lib_math_truncate
  (lambda (x)
    (let ((fx (float x)))
      (cond ((isnan fx) fx)
            ((hydra--infinitep fx) fx)
            (t (float (truncate fx)))))))

;; roundFloat64 :: Int -> Double -> Double
;; Returns NaN/Inf inputs unchanged (no rounding is possible).
(defvar hydra_overlay_emacs_lisp_lib_math_round_float64
  (lambda (n)
    (lambda (x)
      (let ((fx (float x)))
        (cond ((isnan fx) fx)
              ((hydra--infinitep fx) fx)
              ((= fx 0.0) 0.0)
              (t (let ((factor (expt 10.0 (- n 1 (floor (log (abs fx) 10))))))
                   (/ (fround (* fx factor)) factor))))))))

;; roundFloat32 :: Int -> Float -> Float
;; Rounds to N significant digits, then snaps through IEEE float32
(defvar hydra_overlay_emacs_lisp_lib_math_round_float32
  (lambda (n)
    (lambda (x)
      (hydra--to-float32 (funcall (funcall hydra_overlay_emacs_lisp_lib_math_round_float64 n) x)))))

(provide 'hydra.lib.math)
