;;; equality.el --- Hydra equality primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; equal :: a -> a -> Bool
;; Delegates to generic-compare (overlay/emacs_lisp/lib/ordering.el) rather
;; than native `equal`. Native `equal` is correct for cl-defstruct instances
;; (records) and hash-tables (Term.set's representation, sets.el), but WRONG
;; for a Term.map payload: maps.el represents a map as a raw cons-prepended
;; alist with no type marker once unwrapped from its union tag, so native
;; `equal` compares raw insertion-order list structure -- two maps with
;; identical logical content built via different insert sequences would
;; compare unequal (confirmed empirically; #742). generic-compare's :map
;; branch canonicalizes (dedup + key-sort) before comparing, so delegating
;; to it is correct for maps as well as everything else.
(defvar hydra_overlay_emacs_lisp_lib_equality_equal
  (lambda (a)
    "Check if two values are equal."
    (lambda (b)
      (zerop (generic-compare a b)))))

(provide 'hydra.lib.equality)
