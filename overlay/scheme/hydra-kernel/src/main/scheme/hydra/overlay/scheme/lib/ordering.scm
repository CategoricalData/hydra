(define-library (hydra overlay scheme lib ordering)
  (import (scheme base) (scheme write) (scheme inexact)
          (ice-9 vlist)
          (only (guile) make-hash-table hash-ref hash-set! sort))
  (export hydra_overlay_scheme_lib_ordering_compare
          hydra_overlay_scheme_lib_ordering_gt
          hydra_overlay_scheme_lib_ordering_gte
          hydra_overlay_scheme_lib_ordering_lt
          hydra_overlay_scheme_lib_ordering_lte
          hydra_overlay_scheme_lib_ordering_max
          hydra_overlay_scheme_lib_ordering_min
          generic-compare)
  (begin

    (define (obj->string x)
      (let ((p (open-output-string)))
        (write x p)
        (get-output-string p)))

    ;; A Literal.decimal wrapped as a Hydra term, as `equal`/`compare`'s generic x/x TermCoder
    ;; (which just passes terms through unchanged -- see prims.scm's tc-variable) actually
    ;; receives it: (literal (decimal (coefficient . scale))), not a bare cons.
    (define (hydra-decimal-term-p term)
      (and (pair? term) (eq? (car term) 'literal)
           (let ((lit (cadr term)))
             (and (pair? lit) (eq? (car lit) 'decimal)))))

    (define (hydra-decimal-term-value term) (cadr (cadr term)))

    ;; Ordered comparison of two decimals: numeric value first, then scale ascending as a
    ;; tiebreak (1.1 < 1.10 < 1.100), per docs/specification/ordering-and-equality.md. Cross-
    ;; multiplies to compare a.coefficient/10^a.scale against b.coefficient/10^b.scale exactly
    ;; (native Scheme bignum arithmetic), mirroring the TypeScript/Clojure/Common Lisp
    ;; equivalents.
    (define (hydra-compare-decimals a b)
      (let* ((ca (car a)) (sa (cdr a)) (cb (car b)) (sb (cdr b))
             (max-scale (max sa sb))
             (na (* ca (expt 10 (- max-scale sa))))
             (nb (* cb (expt 10 (- max-scale sb)))))
        (cond ((< na nb) -1) ((> na nb) 1) (else (- sa sb)))))

    ;; Declared-variant order for each hydra.core union family, transcribed
    ;; from the generated hydra/core.scm (hydra_core_term-variants,
    ;; hydra_core_type-variants, etc.), which in turn come from the DSL
    ;; declaration order (packages/hydra-kernel/.../Sources/Kernel/Types/
    ;; Core.hs). Mirrors Python's _VARIANT_ORDER table
    ;; (overlay/python/.../util/_compare.py) -- the established #718
    ;; precedent hand-authors variant order for kernel types only;
    ;; non-kernel (user-schema) unions fall back to a still-deterministic
    ;; but non-declared-order tag comparison (see generic-compare's symbol
    ;; branch below). Keyed by family name (not by tag alone): a union
    ;; value's tag symbol alone carries no runtime type identity, and tags
    ;; collide across unrelated unions repo-wide (literal, map, unit, etc.),
    ;; so a single flat tag->ordinal table would be unsound. Hydra's static
    ;; typing guarantees compare/equal are only ever called on same-typed
    ;; values (docs/specification/ordering-and-equality.md), so it is safe
    ;; to resolve the family from BOTH sides' tags and require them to agree.
    (define variant-order
      (list
       (cons 'term '(annotated application cases either lambda let list
                      literal map optional pair project record set
                      type_lambda type_application inject unit unwrap
                      variable wrap))
       (cons 'type '(annotated application effect either forall function
                      list literal map optional pair record set union
                      unit variable void wrap))
       (cons 'literal '(binary boolean decimal float integer string))
       (cons 'integer '(bigint int8 int16 int32 int64 uint8 uint16 uint32 uint64))
       (cons 'float '(float32 float64))))

    ;; If tags A and B both belong to the SAME known family, return
    ;; (ordinal-a . ordinal-b); otherwise #f (unknown family, or a family
    ;; mismatch that should not arise under Hydra's static typing --
    ;; callers fall back to a deterministic tag compare in that case).
    (define (list-position x lst)
      (let loop ((rest lst) (i 0))
        (cond ((null? rest) #f)
              ((eq? (car rest) x) i)
              (else (loop (cdr rest) (+ i 1))))))
    (define (variant-ordinals tag-a tag-b)
      (let loop ((entries variant-order))
        (if (null? entries) #f
            (let* ((variants (cdar entries))
                   (pa (list-position tag-a variants))
                   (pb (list-position tag-b variants)))
              (if (and pa pb) (cons pa pb) (loop (cdr entries)))))))

    ;; Term.map's payload (overlay/scheme/lib/maps.scm) is a Guile vhash --
    ;; an opaque type (vlist? #t, pair? #f), NOT cons-shaped, so it never
    ;; reaches the (pair? a) (pair? b) branch below and would otherwise fall
    ;; all the way to the print-based `else` branch: two vhashes with
    ;; identical logical content but different insertion order (vhash-cons
    ;; prepends physical entries; iteration order is insertion order, not
    ;; key order) would compare via `write`, which reflects insertion order,
    ;; not content -- always wrong on shape/order-divergent builds. This
    ;; mirrors the same bug found in Common Lisp's rbnode maps/sets and
    ;; Emacs Lisp's cons-alist maps (#742); TypeScript's CanonMap branch
    ;; (ordering.ts) is the same fix shape again.
    ;;
    ;; A self-contained unique-entries+sort helper is defined here (not
    ;; reused from maps.scm's private vhash-sorted-entries) to avoid a
    ;; circular import: maps.scm already imports generic-compare FROM this
    ;; library (to sort map entries by key), so this library cannot import
    ;; back from maps.scm.
    (define (vhash-sorted-unique-entries vh)
      (let ((seen (make-hash-table)))
        (sort
          (vhash-fold (lambda (k v acc)
                        (if (hash-ref seen k #f) acc
                            (begin (hash-set! seen k #t) (cons (cons k v) acc))))
                      '() vh)
          (lambda (x y) (< (generic-compare (car x) (car y)) 0)))))

    (define (generic-compare a b)
      (cond
        ((equal? a b) 0)
        ((and (real? a) (real? b) (inexact? a) (inexact? b))
         ;; IEEE 754 extended totalOrder (docs/specification/ordering-and-equality.md):
         ;; NaN is greatest and equal to itself; -0.0 < +0.0. Native < / = treat
         ;; NaN as unordered and (= -0.0 0.0) as true, so both need special-casing.
         (let ((na (nan? a)) (nb (nan? b)))
           (cond
             ((and na nb) 0)
             (na 1)
             (nb -1)
             ((< a b) -1)
             ((> a b) 1)
             ((and (= a 0) (= b 0))
              (let ((nega (negative? (/ 1.0 a))) (negb (negative? (/ 1.0 b))))
                (cond ((eq? nega negb) 0) (nega -1) (else 1))))
             (else 0))))
        ((and (number? a) (number? b))
         (cond ((< a b) -1) ((= a b) 0) (else 1)))
        ((and (string? a) (string? b))
         (cond ((string<? a b) -1) ((string=? a b) 0) (else 1)))
        ((and (char? a) (char? b))
         (cond ((char<? a b) -1) ((char=? a b) 0) (else 1)))
        ((and (boolean? a) (boolean? b))
         (cond ((and (not a) b) -1) ((eq? a b) 0) (else 1)))
        ((and (hydra-decimal-term-p a) (hydra-decimal-term-p b))
         (hydra-compare-decimals (hydra-decimal-term-value a) (hydra-decimal-term-value b)))
        ((and (vlist? a) (vlist? b))
         (generic-compare (vhash-sorted-unique-entries a) (vhash-sorted-unique-entries b)))
        ;; Union values: (tag . payload) or (tag payload ...). Compare by
        ;; declared-variant order (when both tags resolve to the same known
        ;; kernel family) rather than the symbol's print/alphabetical
        ;; order; same variant (or unknown family) recurses into the payload.
        ((and (pair? a) (pair? b) (symbol? (car a)) (symbol? (car b)))
         (if (eq? (car a) (car b))
             (generic-compare (cdr a) (cdr b))
             (let ((ordinals (variant-ordinals (car a) (car b))))
               (if ordinals
                   (- (car ordinals) (cdr ordinals))
                   ;; Unknown (non-kernel) family: no declared-order table
                   ;; available -- fall back to a deterministic (not
                   ;; print-based) symbol compare. Same scope limitation
                   ;; #718 carries on every host but Java.
                   (let ((sa (symbol->string (car a))) (sb (symbol->string (car b))))
                     (cond ((string<? sa sb) -1) ((string=? sa sb) 0) (else 1)))))))
        ((and (symbol? a) (symbol? b))
         (let ((sa (symbol->string a)) (sb (symbol->string b)))
           (cond ((string<? sa sb) -1) ((string=? sa sb) 0) (else 1))))
        ((and (pair? a) (pair? b))
         (let ((c (generic-compare (car a) (car b))))
           (if (= c 0)
               (generic-compare (cdr a) (cdr b))
               c)))
        ((and (null? a) (null? b)) 0)
        ((null? a) -1)
        ((null? b) 1)
        (else (let ((sa (obj->string a)) (sb (obj->string b)))
                (cond ((string<? sa sb) -1) ((string=? sa sb) 0) (else 1))))))

    ;; Compare two values and return a Comparison.
    (define hydra_overlay_scheme_lib_ordering_compare
      (lambda (a)
        (lambda (b)
          (let ((c (generic-compare a b)))
            (cond
              ((< c 0) (list 'less_than '()))
              ((= c 0) (list 'equal_to '()))
              (else    (list 'greater_than '())))))))

    ;; Check if first value is greater than second.
    (define hydra_overlay_scheme_lib_ordering_gt
      (lambda (a)
        (lambda (b)
          (> (generic-compare a b) 0))))

    ;; Check if first value is greater than or equal to second.
    (define hydra_overlay_scheme_lib_ordering_gte
      (lambda (a)
        (lambda (b)
          (>= (generic-compare a b) 0))))

    ;; Check if first value is less than second.
    (define hydra_overlay_scheme_lib_ordering_lt
      (lambda (a)
        (lambda (b)
          (< (generic-compare a b) 0))))

    ;; Check if first value is less than or equal to second.
    (define hydra_overlay_scheme_lib_ordering_lte
      (lambda (a)
        (lambda (b)
          (<= (generic-compare a b) 0))))

    ;; Return the maximum of two values.
    (define hydra_overlay_scheme_lib_ordering_max
      (lambda (a)
        (lambda (b)
          (if (>= (generic-compare a b) 0) a b))))

    ;; Return the minimum of two values.
    (define hydra_overlay_scheme_lib_ordering_min
      (lambda (a)
        (lambda (b)
          (if (<= (generic-compare a b) 0) a b))))))
