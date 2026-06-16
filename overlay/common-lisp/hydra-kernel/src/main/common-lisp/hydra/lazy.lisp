(in-package :cl-user)

;; One-shot memoized thunks for kernel let-bindings.
;;
;; Hydra DSL `let` bindings have lazy semantics: the RHS is evaluated at most
;; once, on first use. The generated CL kernel code naively emits `(let ((x
;; EXPR)) BODY)` which evaluates EXPR eagerly — for deeply-nested kernel
;; inference code, that triggers exponential re-evaluation of sub-expressions
;; and matches the Python pre-#344 explosion described in
;; docs/history/python-host-perf-investigation.md.
;;
;; This file provides the runtime helpers. The loader (loader.lisp) rewrites
;; each `(let ((x EXPR)) BODY)` into:
;;
;;   (let ((x-thunk (make-lazy (lambda () EXPR))))
;;     (symbol-macrolet ((x (lazy-force x-thunk)))
;;       BODY))
;;
;; First reference to x via the symbol-macrolet expands to (lazy-force
;; x-thunk), forcing evaluation. Subsequent references hit the cached value.

;; A lazy cell is a single mutable cons cell whose car is a keyword tag:
;;   (:lazy-thunk . #'thunk)   — unforced
;;   (:lazy-value . value)     — forced
;; On force, we rplaca/rplacd the cons in place to update the tag and the
;; payload. Subsequent forces hit the value path with a single car-eq check.

(declaim (inline make-lazy lazy-force))

(defun make-lazy (thunk)
  "Wrap a zero-arg thunk into a lazy cell."
  (cons :lazy-thunk thunk))

(defun lazy-force (cell)
  "Force a lazy cell, memoizing the result. Idempotent on subsequent calls.
   Defensive on non-lazy input: returns it unchanged."
  (cond
    ((not (consp cell)) cell)
    ((eq (car cell) :lazy-value) (cdr cell))
    ((eq (car cell) :lazy-thunk)
     (let ((v (funcall (cdr cell))))
       (setf (car cell) :lazy-value
             (cdr cell) v)
       v))
    (t cell)))
