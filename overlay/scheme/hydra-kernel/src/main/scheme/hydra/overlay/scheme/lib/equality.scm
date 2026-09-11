(define-library (hydra overlay scheme lib equality)
  (import (scheme base)
          (only (hydra overlay scheme lib ordering) generic-compare))
  (export hydra_overlay_scheme_lib_equality_equal)
  (begin

    ;; Check if two values are equal.
    ;; Delegates to generic-compare (overlay/scheme/lib/ordering.scm) rather
    ;; than native `equal?`. Guile's `equal?` is correct for
    ;; define-record-type instances (verified empirically under Guile 3.0.8)
    ;; but WRONG for a Term.map payload: maps.scm represents a map as a
    ;; Guile vhash, and `equal?` on two vhashes compares their physical
    ;; (insertion-order-dependent) structure, not their logical key/value
    ;; contents -- two maps with identical contents built via different
    ;; insert sequences compare unequal (confirmed empirically; #742).
    ;; generic-compare's vlist? branch canonicalizes (dedup + key-sort)
    ;; before comparing, so delegating to it is correct for maps as well as
    ;; everything else.
    (define hydra_overlay_scheme_lib_equality_equal
      (lambda (a)
        (lambda (b)
          (= (generic-compare a b) 0))))))
