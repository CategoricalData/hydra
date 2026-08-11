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

;; Constraint-polymorphic ('numeric') dispatch for add/sub/mul/negate.
;;
;; These vars serve two distinct call contracts, both real:
;;
;;  1. The interpreter path: registered as hydra.lib.math.add etc. via prim2/prim1 with a
;;     'numeric' class constraint and identity (Term) coders (tc-variable) in libraries.lisp's
;;     register-math, so on that path the compute fn receives raw Terms -- list-encoded, e.g.
;;     (:literal (:integer (:int32 42))) -- and must dispatch on the operand's literal variant.
;;  2. The generated-kernel-code path: this SAME var is called directly by Hydra's own
;;     self-hosted Common Lisp source (e.g. hydra.names, hydra.lexical) as ordinary arithmetic
;;     over raw native numbers -- no Term involved at all.
;;
;; This mirrors the Haskell host's numericBinary/numericUnary (Hydra.Overlay.Haskell.Lib.Math)
;; and Java's NumericDispatch, except Java splits the two contracts onto two differently-typed
;; methods on one class; Common Lisp has one flat function namespace, so both contracts are
;; dispatched here by argument shape instead: a Term literal is a cons (a list), a native number
;; is not. No typeclass mechanism is consulted at runtime -- the host has none. Type inference
;; guarantees both operands of a binary op share one numeric type, so the Term-path dispatch keys
;; on the first operand and requires the second to match; a mismatch or a non-numeric operand is
;; an internal invariant violation and fails loudly.
;;
;; Fixed-width integer variants are narrowed back to the source width (two's-complement
;; wraparound, mirroring Java's NumericDispatch.rewrapInteger); bigint is arbitrary precision.
;; The native-value path needs no narrowing: Common Lisp's own numeric tower already gives the
;; right per-type behavior for the native types in play.

(defun hydra-int-width-bits (tag)
  (case tag
    (:int8 8) (:int16 16) (:int32 32) (:int64 64)
    (:uint8 8) (:uint16 16) (:uint32 32) (:uint64 64)
    (t nil)))

(defun hydra-unsigned-int-width-p (tag)
  (member tag '(:uint8 :uint16 :uint32 :uint64)))

(defun hydra-wrap-int (width-tag r)
  (if (eq width-tag :bigint)
      r
      (let* ((bits (hydra-int-width-bits width-tag))
             (m (ash 1 bits)))
        (if (hydra-unsigned-int-width-p width-tag)
            (mod r m)
            (let ((w (mod r m)))
              (if (>= w (/ m 2)) (- w m) w))))))

(defun hydra-numeric-literal (op-name term)
  (if (eq (first term) :literal)
      (second term)
      (error "hydra.lib.math.~a: expected a numeric literal term" op-name)))

;; float32 is always represented internally as a double-float (matching literals.lisp's
;; `decimal_to_float32`/`read_float32`/`show_float32` convention), snapped to single-precision
;; via a round-trip through 1.0f0. That round-trip signals FLOATING-POINT-OVERFLOW on SBCL for
;; any magnitude beyond float32's finite range (verified empirically), instead of saturating to
;; +-Infinity per IEEE 754 -- so it must run with float traps masked, mirroring the existing
;; +hydra-pos-inf+/+hydra-nan+ constants at the top of this file (also computed under the
;; expectation that SBCL might trap; on other CL implementations this is a no-op via #-sbcl).
(defun hydra-to-float32 (x)
  #+sbcl (sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero)
           (float (float x 1.0f0) 1.0d0))
  #-sbcl (float (float x 1.0f0) 1.0d0))

(defun hydra-numeric-binary-term (op-name int-op float-op x y)
  (let* ((lx (hydra-numeric-literal op-name x))
         (ly (hydra-numeric-literal op-name y))
         (lx-kind (first lx))
         (ly-kind (first ly)))
    (unless (eq lx-kind ly-kind)
      (error "hydra.lib.math.~a: operands are not the same numeric kind" op-name))
    (cond
      ((eq lx-kind :integer)
       (let* ((vx (second lx)) (vy (second ly)) (vx-tag (first vx)) (vy-tag (first vy)))
         (unless (eq vx-tag vy-tag)
           (error "hydra.lib.math.~a: integer operands differ in precision" op-name))
         (list :literal (list :integer (list vx-tag (hydra-wrap-int vx-tag (funcall int-op (second vx) (second vy))))))))
      ((eq lx-kind :float)
       (let* ((vx (second lx)) (vy (second ly)) (vx-tag (first vx)) (vy-tag (first vy)))
         (unless (eq vx-tag vy-tag)
           (error "hydra.lib.math.~a: float operands differ in precision" op-name))
         (let ((r (funcall float-op (second vx) (second vy))))
           (list :literal (list :float (list vx-tag (if (eq vx-tag :float32) (hydra-to-float32 r) (float r 1.0d0))))))))
      (t (error "hydra.lib.math.~a: operand is not numeric" op-name)))))

(defun hydra-numeric-unary-term (op-name int-op float-op x)
  (let* ((lx (hydra-numeric-literal op-name x))
         (lx-kind (first lx)))
    (cond
      ((eq lx-kind :integer)
       (let* ((vx (second lx)) (vx-tag (first vx)))
         (list :literal (list :integer (list vx-tag (hydra-wrap-int vx-tag (funcall int-op (second vx))))))))
      ((eq lx-kind :float)
       (let* ((vx (second lx)) (vx-tag (first vx)))
         (let ((r (funcall float-op (second vx))))
           (list :literal (list :float (list vx-tag (if (eq vx-tag :float32) (hydra-to-float32 r) (float r 1.0d0))))))))
      (t (error "hydra.lib.math.~a: operand is not numeric" op-name)))))

(defun hydra-numeric-binary (op-name int-op float-op)
  (lambda (x)
    (lambda (y)
      (if (consp x)
          (hydra-numeric-binary-term op-name int-op float-op x y)
          (funcall int-op x y)))))

(defun hydra-numeric-unary (op-name int-op float-op)
  (lambda (x)
    (if (consp x)
        (hydra-numeric-unary-term op-name int-op float-op x)
        (funcall int-op x))))

;; --- Constraint-polymorphic ('integral') dispatch for div/mod/rem/even/odd ---
;;
;; div/mod are floor-based (sign follows the divisor); rem is truncated (sign follows the
;; dividend) -- mirroring the Haskell/Java/Python/Scala/TypeScript hosts' div/mod vs rem split.
;; Common Lisp's own FLOOR and MOD are already floor-based, and TRUNCATE/REM are already
;; truncated (per the CLHS), so no hand-rolled floor-div/floor-mod is needed here: the native
;; #'floor / #'mod / #'rem builtins have exactly the required semantics. (This differs from the
;; Clojure host, whose quot/rem are truncated and therefore hand-roll floor-div/floor-mod.)
;; All three guard the zero-divisor case (returning (:none)) before computing. The (minBound, -1)
;; boundary needs an explicit wrap-to-minBound on div only (the exact quotient overflows one past
;; maxBound); mod/rem have no overflow there. Both call contracts (Term-path vs native-value path)
;; are supported, same dispatch-by-shape convention as numeric-binary/numeric-unary above.

(defun hydra-integral-literal (op-name term)
  (if (and (consp term) (eq (first term) :literal) (eq (first (second term)) :integer))
      (second (second term))
      (error "hydra.lib.math.~a: expected an integer literal term" op-name)))

(defun hydra-integral-binary-term (op-name int-op wrap-min-boundary-on-div x y)
  (let* ((vx (hydra-integral-literal op-name x))
         (vy (hydra-integral-literal op-name y))
         (vx-tag (first vx)) (vy-tag (first vy)))
    (unless (eq vx-tag vy-tag)
      (error "hydra.lib.math.~a: integer operands differ in precision" op-name))
    (let ((a (second vx)) (b (second vy)))
      (if (= b 0)
          (list :none)
          (let* ((signed (and (hydra-int-width-bits vx-tag) (not (hydra-unsigned-int-width-p vx-tag))))
                 (r (if (and wrap-min-boundary-on-div signed)
                        (let* ((bits (hydra-int-width-bits vx-tag))
                               (min-bound (- (ash 1 (1- bits)))))
                          (if (and (= a min-bound) (= b -1)) min-bound (funcall int-op a b)))
                        (funcall int-op a b))))
            (list :given (list :literal (list :integer (list vx-tag (hydra-wrap-int vx-tag r))))))))))

(defun hydra-integral-binary-native (op-name int-op wrap-min-boundary-on-div a b)
  (declare (ignore op-name wrap-min-boundary-on-div))
  (if (= b 0)
      (list :none)
      (list :given (funcall int-op a b))))

(defun hydra-integral-binary (op-name int-op wrap-min-boundary-on-div)
  (lambda (x)
    (lambda (y)
      (if (consp x)
          (hydra-integral-binary-term op-name int-op wrap-min-boundary-on-div x y)
          (hydra-integral-binary-native op-name int-op wrap-min-boundary-on-div x y)))))

(defun hydra-integral-to-native (v)
  ;; uint8 is stored as a possibly-negative int32 in Java; normalize to its unsigned value
  ;; for the parity test. Other widths are already the correct integer.
  (let ((tag (first v)) (n (second v)))
    (if (eq tag :uint8) (logand n #xff) n)))

(defun hydra-even-or-odd (op-name want-even)
  (lambda (x)
    (let ((n (if (consp x) (hydra-integral-to-native (hydra-integral-literal op-name x)) x)))
      (eq (evenp n) want-even))))

;; --- Constraint-polymorphic ('fractional') dispatch for divide ---
;;
;; float32/float64 only, IEEE-total: division by zero yields ±Infinity/NaN, never an exception.
;; The native #'/ traps on a zero divisor, so guard it and produce the IEEE sentinel explicitly.

(defun hydra-ieee-divide (a b)
  (let ((da (float a 1.0d0)) (db (float b 1.0d0)))
    (cond
      ((or (hydra-nan-p da) (hydra-nan-p db)) +hydra-nan+)
      ((/= db 0.0d0) (/ da db))
      ;; db is (a signed) zero: a/0 = ±Inf for a nonzero numerator, NaN for 0/0.
      ((= da 0.0d0) +hydra-nan+)
      ;; sign of result follows sign of numerator XOR sign of the (possibly signed) zero divisor.
      ((eq (< da 0.0d0) (minusp (float-sign db))) +hydra-pos-inf+)
      (t +hydra-neg-inf+))))

(defun hydra-divide-term (x y)
  (let ((lx (hydra-numeric-literal "divide" x))
        (ly (hydra-numeric-literal "divide" y)))
    (unless (and (eq (first lx) :float) (eq (first ly) :float))
      (error "hydra.lib.math.divide: operands are not the same fractional kind"))
    (let* ((vx (second lx)) (vy (second ly)) (vx-tag (first vx)) (vy-tag (first vy)))
      (unless (eq vx-tag vy-tag)
        (error "hydra.lib.math.divide: float operands differ in precision"))
      (let ((r (hydra-ieee-divide (second vx) (second vy))))
        (list :literal (list :float (list vx-tag (if (eq vx-tag :float32) (hydra-to-float32 r) r))))))))

(defun hydra-divide-dispatch (x)
  (lambda (y)
    (if (consp x)
        (hydra-divide-term x y)
        (hydra-ieee-divide x y))))

;; abs :: numeric x => x -> x
(defvar hydra_overlay_common_lisp_lib_math_abs
  (hydra-numeric-unary "abs" (lambda (a) (abs a)) (lambda (a) (abs (float a 1.0d0)))))

;; acos :: Double -> Double  (domain [-1, 1]; out-of-domain -> NaN)
(defvar hydra_overlay_common_lisp_lib_math_acos
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) +hydra-nan+)
            ((or (< fx -1.0d0) (> fx 1.0d0)) +hydra-nan+)
            (t (acos fx))))))

;; acosh :: Double -> Double  (domain [1, +inf); out-of-domain -> NaN)
(defvar hydra_overlay_common_lisp_lib_math_acosh
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) +hydra-nan+)
            ((< fx 1.0d0) +hydra-nan+)
            ((and (hydra-inf-p fx) (> fx 0)) +hydra-pos-inf+)
            (t (acosh fx))))))

;; add :: numeric x => x -> x -> x
(defvar hydra_overlay_common_lisp_lib_math_add
  (hydra-numeric-binary "add" #'+ #'+))

;; addFloat64 :: Double -> Double -> Double
(defvar hydra_overlay_common_lisp_lib_math_add_float64
  (lambda (a)
    (lambda (b)
      (+ (float a 1.0d0) (float b 1.0d0)))))

;; asin :: Double -> Double  (domain [-1, 1]; out-of-domain -> NaN)
(defvar hydra_overlay_common_lisp_lib_math_asin
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) +hydra-nan+)
            ((or (< fx -1.0d0) (> fx 1.0d0)) +hydra-nan+)
            (t (asin fx))))))

;; asinh :: Double -> Double  (unrestricted domain)
(defvar hydra_overlay_common_lisp_lib_math_asinh
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) +hydra-nan+)
            ((hydra-inf-p fx) fx)
            (t (asinh fx))))))

;; atan :: Double -> Double
(defvar hydra_overlay_common_lisp_lib_math_atan
  (lambda (x) (atan (float x 1.0d0))))

;; atan2 :: Double -> Double -> Double
;; Match Haskell: atan2 returns NaN when both arguments are infinite.
(defvar hydra_overlay_common_lisp_lib_math_atan2
  (lambda (y)
    (lambda (x)
      (let ((fy (float y 1.0d0)) (fx (float x 1.0d0)))
        (if (and (hydra-inf-p fy) (hydra-inf-p fx))
            +hydra-nan+
            (atan fy fx))))))

;; atanh :: Double -> Double  (domain (-1, 1); boundary -> ±Inf; |x|>1 -> NaN)
(defvar hydra_overlay_common_lisp_lib_math_atanh
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
(defvar hydra_overlay_common_lisp_lib_math_ceiling
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) fx)
            ((hydra-inf-p fx) fx)
            (t (float (ceiling fx) 1.0d0))))))

;; cos :: Double -> Double
(defvar hydra_overlay_common_lisp_lib_math_cos
  (lambda (x) (cos (float x 1.0d0))))

;; cosh :: Double -> Double
(defvar hydra_overlay_common_lisp_lib_math_cosh
  (lambda (x) (cosh (float x 1.0d0))))

;; e :: Double
(defvar hydra_overlay_common_lisp_lib_math_e (exp 1.0d0))

;; even :: integral x => x -> Bool
(defvar hydra_overlay_common_lisp_lib_math_even
  (hydra-even-or-odd "even" t))

;; exp :: Double -> Double
(defvar hydra_overlay_common_lisp_lib_math_exp
  (lambda (x) (exp (float x 1.0d0))))

;; floor :: Double -> Double
;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
(defvar hydra_overlay_common_lisp_lib_math_floor
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) fx)
            ((hydra-inf-p fx) fx)
            (t (float (floor fx) 1.0d0))))))

;; log :: Double -> Double  (domain (0, +inf); x=0 -> -Inf; x<0 -> NaN)
(defvar hydra_overlay_common_lisp_lib_math_log
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
(defvar hydra_overlay_common_lisp_lib_math_logBase
  (lambda (base)
    (lambda (x)
      (/ (funcall hydra_overlay_common_lisp_lib_math_log x) (funcall hydra_overlay_common_lisp_lib_math_log base)))))

;; log_base alias
(defvar hydra_overlay_common_lisp_lib_math_log_base hydra_overlay_common_lisp_lib_math_logBase)

;; div :: integral x => x -> x -> optional x
;; Floor division (CL's #'floor is floor-based); wraps at the (minBound, -1) boundary.
(defvar hydra_overlay_common_lisp_lib_math_div
  (hydra-integral-binary "div" (lambda (a b) (floor a b)) t))

;; mod :: integral x => x -> x -> optional x
;; Floor modulus (CL's #'mod is floor-based; sign follows the divisor).
(defvar hydra_overlay_common_lisp_lib_math_mod
  (hydra-integral-binary "mod" (lambda (a b) (mod a b)) nil))

;; rem :: integral x => x -> x -> optional x
;; Truncated remainder (CL's #'rem is truncated; sign follows the dividend).
(defvar hydra_overlay_common_lisp_lib_math_rem
  (hydra-integral-binary "rem" (lambda (a b) (rem a b)) nil))

;; divide :: fractional x => x -> x -> x
(defvar hydra_overlay_common_lisp_lib_math_divide
  #'hydra-divide-dispatch)

;; mul :: numeric x => x -> x -> x
(defvar hydra_overlay_common_lisp_lib_math_mul
  (hydra-numeric-binary "mul" #'* #'*))

;; mulFloat64 :: Double -> Double -> Double
(defvar hydra_overlay_common_lisp_lib_math_mul_float64
  (lambda (a)
    (lambda (b)
      (* (float a 1.0d0) (float b 1.0d0)))))

;; negate :: numeric x => x -> x
(defvar hydra_overlay_common_lisp_lib_math_negate
  (hydra-numeric-unary "negate" #'- #'-))

;; negateFloat64 :: Double -> Double
(defvar hydra_overlay_common_lisp_lib_math_negate_float64
  (lambda (a)
    (- (float a 1.0d0))))

;; odd :: integral x => x -> Bool
(defvar hydra_overlay_common_lisp_lib_math_odd
  (hydra-even-or-odd "odd" nil))

;; pi :: Double
(defvar hydra_overlay_common_lisp_lib_math_pi (float pi 1.0d0))

;; pow :: Double -> Double -> Double
(defvar hydra_overlay_common_lisp_lib_math_pow
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

;; range :: Int -> Int -> [Int]  (half-open, [start, end))
(defvar hydra_overlay_common_lisp_lib_math_range
  (lambda (start)
    (lambda (end)
      (loop for i from start below end collect i))))

;; round :: Double -> Double
;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
(defvar hydra_overlay_common_lisp_lib_math_round
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) fx)
            ((hydra-inf-p fx) fx)
            (t (float (round fx) 1.0d0))))))

;; signum :: numeric x => x -> x
;; Returns -1/0/1 for integers; for floats, preserves the sign of zero (signum(-0.0) = -0.0)
;; and propagates NaN. CL's #'signum on a float already has this IEEE-correct behavior.
(defvar hydra_overlay_common_lisp_lib_math_signum
  (hydra-numeric-unary "signum"
    (lambda (a) (cond ((> a 0) 1) ((< a 0) -1) (t 0)))
    (lambda (a) (let ((fa (float a 1.0d0)))
                  (if (hydra-nan-p fa) +hydra-nan+ (signum fa))))))

;; sin :: Double -> Double
(defvar hydra_overlay_common_lisp_lib_math_sin
  (lambda (x) (sin (float x 1.0d0))))

;; sinh :: Double -> Double
(defvar hydra_overlay_common_lisp_lib_math_sinh
  (lambda (x) (sinh (float x 1.0d0))))

;; sqrt :: Double -> Double  (domain [0, +inf); x<0 -> NaN)
(defvar hydra_overlay_common_lisp_lib_math_sqrt
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) +hydra-nan+)
            ((< fx 0.0d0) +hydra-nan+)
            ((and (hydra-inf-p fx) (< fx 0)) +hydra-nan+)
            (t (sqrt fx))))))

;; sub :: numeric x => x -> x -> x
(defvar hydra_overlay_common_lisp_lib_math_sub
  (hydra-numeric-binary "sub" #'- #'-))

;; subFloat64 :: Double -> Double -> Double
(defvar hydra_overlay_common_lisp_lib_math_sub_float64
  (lambda (a)
    (lambda (b)
      (- (float a 1.0d0) (float b 1.0d0)))))


;; tan :: Double -> Double
(defvar hydra_overlay_common_lisp_lib_math_tan
  (lambda (x) (tan (float x 1.0d0))))

;; tanh :: Double -> Double
(defvar hydra_overlay_common_lisp_lib_math_tanh
  (lambda (x) (tanh (float x 1.0d0))))

;; truncate :: Double -> Double
;; DIVERGENCE FROM HASKELL: returns a float, not an integer (see ceiling).
(defvar hydra_overlay_common_lisp_lib_math_truncate
  (lambda (x)
    (let ((fx (float x 1.0d0)))
      (cond ((hydra-nan-p fx) fx)
            ((hydra-inf-p fx) fx)
            (t (float (truncate fx) 1.0d0))))))

;; roundFloat64 :: Int -> Double -> Double
;; Returns NaN/Inf inputs unchanged (no rounding is possible).
(defvar hydra_overlay_common_lisp_lib_math_round_float64
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
(defvar hydra_overlay_common_lisp_lib_math_round_float32
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
