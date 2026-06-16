;;; lazy.el --- One-shot memoized thunks for Hydra-EL let bindings -*- lexical-binding: t -*-

;; Hydra DSL `let` bindings have lazy semantics: the RHS is evaluated at
;; most once, on first use. The generated Emacs Lisp kernel code naively
;; emits `(let ((x EXPR)) BODY)` which evaluates EXPR eagerly — for
;; deeply-nested kernel inference code, that triggers exponential
;; re-evaluation of sub-expressions, the same blowup that Python had
;; pre-#344 (docs/history/python-host-perf-investigation.md) and Common
;; Lisp had pre-#360.
;;
;; This file provides the runtime helpers. The loader (loader.el)
;; rewrites each non-trivial `(let ((x EXPR)) BODY)` into:
;;
;;   (let ((x-thunk (make-lazy (lambda () EXPR))))
;;     (cl-symbol-macrolet ((x (lazy-force x-thunk)))
;;       BODY))
;;
;; First reference to x via the cl-symbol-macrolet expands to
;; (lazy-force x-thunk), forcing evaluation. Subsequent references hit
;; the cached value.
;;
;; A lazy cell is a single mutable cons cell whose car is a keyword tag:
;;   (:lazy-thunk . #'thunk)   — unforced
;;   (:lazy-value . value)     — forced
;; On force, we setcar/setcdr the cons in place to update the tag and
;; the payload. Subsequent forces hit the value path with a single
;; car-eq check.

(defsubst make-lazy (thunk)
  "Wrap a zero-arg thunk into a lazy cell."
  (cons :lazy-thunk thunk))

(defsubst lazy-force (cell)
  "Force a lazy cell, memoizing the result. Idempotent on subsequent
   calls. Defensive on non-lazy input: returns it unchanged."
  (cond
   ((not (consp cell)) cell)
   ((eq (car cell) :lazy-value) (cdr cell))
   ((eq (car cell) :lazy-thunk)
    (let ((v (funcall (cdr cell))))
      (setcar cell :lazy-value)
      (setcdr cell v)
      v))
   (t cell)))

(provide 'hydra-lazy)
