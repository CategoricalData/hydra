(define-library (hydra lib sets)
  (import (scheme base)
          (scheme write))
  (export hydra_lib_sets_delete
          hydra_lib_sets_difference
          hydra_lib_sets_empty
          hydra_lib_sets_from_list
          hydra_lib_sets_insert
          hydra_lib_sets_intersection
          hydra_lib_sets_map
          hydra_lib_sets_member
          hydra_lib_sets_null
          hydra_lib_sets_singleton
          hydra_lib_sets_to_list
          hydra_lib_sets_union
          hydra_lib_sets_size
          hydra_lib_sets_unions)
  (begin

    ;; Sets are sorted lists (no duplicates)

    (define (obj->string x)
      (let ((p (open-output-string)))
        (write x p)
        (get-output-string p)))

    (define (generic-compare a b)
      (cond
        ((equal? a b) 0)  ;; Fast path: if equal, no need for ordering
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
           (if (= c 0) (generic-compare (cdr a) (cdr b)) c)))
        ((and (null? a) (null? b)) 0)
        ((null? a) -1)
        ((null? b) 1)
        ;; Fallback: compare string representations for records and other types
        (else (let ((sa (obj->string a)) (sb (obj->string b)))
                (cond ((string<? sa sb) -1) ((string=? sa sb) 0) (else 1))))))

    (define (set-insert x s)
      (cond
        ((null? s) (list x))
        ((= (generic-compare x (car s)) 0) s)
        ((< (generic-compare x (car s)) 0) (cons x s))
        (else (cons (car s) (set-insert x (cdr s))))))

    (define (set-delete x s)
      (cond
        ((null? s) '())
        ((equal? x (car s)) (cdr s))
        (else (cons (car s) (set-delete x (cdr s))))))

    (define (set-member? x s)
      (cond
        ((null? s) #f)
        ((equal? x (car s)) #t)
        ((< (generic-compare x (car s)) 0) #f)
        (else (set-member? x (cdr s)))))

    ;; delete :: a -> Set a -> Set a
    (define hydra_lib_sets_delete
      (lambda (x)
        (lambda (s)
          (set-delete x s))))

    ;; difference :: Set a -> Set a -> Set a
    (define hydra_lib_sets_difference
      (lambda (s1)
        (lambda (s2)
          (let loop ((rest s1) (acc '()))
            (if (null? rest)
                (reverse acc)
                (if (set-member? (car rest) s2)
                    (loop (cdr rest) acc)
                    (loop (cdr rest) (cons (car rest) acc))))))))

    ;; empty :: Set a
    (define hydra_lib_sets_empty '())

    ;; from_list :: [a] -> Set a
    (define hydra_lib_sets_from_list
      (lambda (xs)
        (let loop ((rest xs) (acc '()))
          (if (null? rest)
              acc
              (loop (cdr rest) (set-insert (car rest) acc))))))

    ;; insert :: a -> Set a -> Set a
    (define hydra_lib_sets_insert
      (lambda (x)
        (lambda (s)
          (set-insert x s))))

    ;; intersection :: Set a -> Set a -> Set a
    (define hydra_lib_sets_intersection
      (lambda (s1)
        (lambda (s2)
          (let loop ((rest s1) (acc '()))
            (if (null? rest)
                (reverse acc)
                (if (set-member? (car rest) s2)
                    (loop (cdr rest) (cons (car rest) acc))
                    (loop (cdr rest) acc)))))))

    ;; map :: (a -> b) -> Set a -> Set b
    (define hydra_lib_sets_map
      (lambda (f)
        (lambda (s)
          (let loop ((rest s) (acc '()))
            (if (null? rest)
                acc
                (loop (cdr rest) (set-insert (f (car rest)) acc)))))))

    ;; member :: a -> Set a -> Bool
    (define hydra_lib_sets_member
      (lambda (x)
        (lambda (s)
          (set-member? x s))))

    ;; null :: Set a -> Bool
    (define hydra_lib_sets_null
      (lambda (s)
        (null? s)))

    ;; singleton :: a -> Set a
    (define hydra_lib_sets_singleton
      (lambda (x)
        (list x)))

    ;; to_list :: Set a -> [a]
    (define hydra_lib_sets_to_list
      (lambda (s)
        s))

    ;; union :: Set a -> Set a -> Set a
    (define hydra_lib_sets_union
      (lambda (s1)
        (lambda (s2)
          (let loop ((rest s2) (acc s1))
            (if (null? rest)
                acc
                (loop (cdr rest) (set-insert (car rest) acc)))))))

    ;; size :: Set a -> Int
    (define hydra_lib_sets_size
      (lambda (s)
        (length s)))

    ;; unions :: [Set a] -> Set a
    (define hydra_lib_sets_unions
      (lambda (sets)
        (let loop ((rest sets) (acc '()))
          (if (null? rest)
              acc
              (loop (cdr rest)
                    ((hydra_lib_sets_union acc) (car rest)))))))))
