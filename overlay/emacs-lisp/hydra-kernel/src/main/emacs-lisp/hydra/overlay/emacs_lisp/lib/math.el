;;; math.el --- Hydra math primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Helpers: detecting NaN and infinity.
;; Emacs Lisp's trig/log functions return NaN for out-of-domain real inputs
;; (matching IEEE 754), so explicit guards are only needed for the rounding
;; functions, which throw overflow-error on NaN/Inf.
(defsubst hydra--infinitep (x)
  "Non-nil if X is +Inf or -Inf."
  (or (= x 1.0e+INF) (= x -1.0e+INF)))

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
         (list :literal (list :float (list vx-tag (float (funcall float-op (cadr vx) (cadr vy))))))))
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
         (list :literal (list :float (list vx-tag (float (funcall float-op (cadr vx))))))))
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

;; abs :: Int -> Int
(defvar hydra_overlay_emacs_lisp_lib_math_abs
  (lambda (n) (abs n)))

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

;; even :: Int -> Bool
(defvar hydra_overlay_emacs_lisp_lib_math_even
  (lambda (n) (cl-evenp n)))

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

;; div :: Int -> Int -> Maybe Int
(defvar hydra_overlay_emacs_lisp_lib_math_div
  (lambda (a)
    (lambda (b)
      (if (= b 0)
          (list :none)
          (list :given (floor a b))))))

;; mod :: Int -> Int -> Maybe Int
(defvar hydra_overlay_emacs_lisp_lib_math_mod
  (lambda (a)
    (lambda (b)
      (if (= b 0)
          (list :none)
          (list :given (mod a b))))))

;; rem :: Int -> Int -> Maybe Int
(defvar hydra_overlay_emacs_lisp_lib_math_rem
  (lambda (a)
    (lambda (b)
      (if (= b 0)
          (list :none)
          (list :given (% a b))))))

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

;; odd :: Int -> Bool
(defvar hydra_overlay_emacs_lisp_lib_math_odd
  (lambda (n) (cl-oddp n)))

;; pi :: Double
(defvar hydra_overlay_emacs_lisp_lib_math_pi float-pi)

;; pow :: Double -> Double -> Double
(defvar hydra_overlay_emacs_lisp_lib_math_pow
  (lambda (base)
    (lambda (exp-val)
      (expt (float base) (float exp-val)))))

;; range :: Int -> Int -> [Int]  (inclusive both ends)
(defvar hydra_overlay_emacs_lisp_lib_math_range
  (lambda (start)
    (lambda (end)
      (let ((acc nil))
        (let ((i start))
          (while (<= i end)
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

;; signum :: Int -> Int
(defvar hydra_overlay_emacs_lisp_lib_math_signum
  (lambda (n)
    (cond ((> n 0) 1) ((< n 0) -1) (t 0))))

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
(defun snap-to-float32 (x)
  "Snap a double to IEEE 754 float32 precision (24-bit mantissa)."
  (cond ((isnan x) x)
        ((hydra--infinitep x) x)
        ((= x 0.0) 0.0)
        (t (let* ((sign (if (< x 0) -1.0 1.0))
                  (ax (abs x))
                  (e (floor (log ax 2.0)))
                  (scale (expt 2.0 (- 23 e)))
                  (mantissa (round (* ax scale))))
             (* sign (/ mantissa scale))))))
(defvar hydra_overlay_emacs_lisp_lib_math_round_float32
  (lambda (n)
    (lambda (x)
      (snap-to-float32 (funcall (funcall hydra_overlay_emacs_lisp_lib_math_round_float64 n) x)))))

(provide 'hydra.lib.math)
