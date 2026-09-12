(define-library (hydra overlay scheme lib ordering)
  (import (scheme base) (scheme write))
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

    (define (generic-compare a b)
      (cond
        ((equal? a b) 0)
        ((and (number? a) (number? b))
         (cond ((< a b) -1) ((= a b) 0) (else 1)))
        ((and (string? a) (string? b))
         (cond ((string<? a b) -1) ((string=? a b) 0) (else 1)))
        ((and (char? a) (char? b))
         (cond ((char<? a b) -1) ((char=? a b) 0) (else 1)))
        ((and (symbol? a) (symbol? b))
         (let ((sa (symbol->string a)) (sb (symbol->string b)))
           (cond ((string<? sa sb) -1) ((string=? sa sb) 0) (else 1))))
        ((and (boolean? a) (boolean? b))
         (cond ((and (not a) b) -1) ((eq? a b) 0) (else 1)))
        ((and (hydra-decimal-term-p a) (hydra-decimal-term-p b))
         (hydra-compare-decimals (hydra-decimal-term-value a) (hydra-decimal-term-value b)))
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
