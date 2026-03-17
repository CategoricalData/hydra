(define-library (hydra lib equality)
  (import (scheme base) (scheme write))
  (export hydra_lib_equality_compare
          hydra_lib_equality_equal
          hydra_lib_equality_gt
          hydra_lib_equality_gte
          hydra_lib_equality_identity
          hydra_lib_equality_lt
          hydra_lib_equality_lte
          hydra_lib_equality_max
          hydra_lib_equality_min
          generic-compare)
  (begin

    (define (obj->string x)
      (let ((p (open-output-string)))
        (write x p)
        (get-output-string p)))

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

    ;; compare :: a -> a -> Comparison
    (define hydra_lib_equality_compare
      (lambda (a)
        (lambda (b)
          (let ((c (generic-compare a b)))
            (cond
              ((< c 0) (list 'less_than '()))
              ((= c 0) (list 'equal_to '()))
              (else    (list 'greater_than '())))))))

    ;; equal :: a -> a -> Bool
    (define hydra_lib_equality_equal
      (lambda (a)
        (lambda (b)
          (equal? a b))))

    ;; gt :: a -> a -> Bool
    (define hydra_lib_equality_gt
      (lambda (a)
        (lambda (b)
          (> (generic-compare a b) 0))))

    ;; gte :: a -> a -> Bool
    (define hydra_lib_equality_gte
      (lambda (a)
        (lambda (b)
          (>= (generic-compare a b) 0))))

    ;; identity :: a -> a
    (define hydra_lib_equality_identity
      (lambda (x) x))

    ;; lt :: a -> a -> Bool
    (define hydra_lib_equality_lt
      (lambda (a)
        (lambda (b)
          (< (generic-compare a b) 0))))

    ;; lte :: a -> a -> Bool
    (define hydra_lib_equality_lte
      (lambda (a)
        (lambda (b)
          (<= (generic-compare a b) 0))))

    ;; max :: a -> a -> a
    (define hydra_lib_equality_max
      (lambda (a)
        (lambda (b)
          (if (>= (generic-compare a b) 0) a b))))

    ;; min :: a -> a -> a
    (define hydra_lib_equality_min
      (lambda (a)
        (lambda (b)
          (if (<= (generic-compare a b) 0) a b))))))
