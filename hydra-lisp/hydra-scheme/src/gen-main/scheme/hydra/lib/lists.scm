(define-library (hydra lib lists)
  (import (scheme base)
          (scheme write))
  (export hydra_lib_lists_at
          hydra_lib_lists_concat
          hydra_lib_lists_concat2
          hydra_lib_lists_cons
          hydra_lib_lists_drop
          hydra_lib_lists_drop_while
          hydra_lib_lists_elem
          hydra_lib_lists_filter
          hydra_lib_lists_find
          hydra_lib_lists_foldl
          hydra_lib_lists_foldr
          hydra_lib_lists_head
          hydra_lib_lists_init
          hydra_lib_lists_intercalate
          hydra_lib_lists_intersperse
          hydra_lib_lists_length
          hydra_lib_lists_map
          hydra_lib_lists_nub
          hydra_lib_lists_null
          hydra_lib_lists_partition
          hydra_lib_lists_pure
          hydra_lib_lists_replicate
          hydra_lib_lists_reverse
          hydra_lib_lists_safe_head
          hydra_lib_lists_singleton
          hydra_lib_lists_sort
          hydra_lib_lists_sort_on
          hydra_lib_lists_span
          hydra_lib_lists_tail
          hydra_lib_lists_take
          hydra_lib_lists_transpose
          hydra_lib_lists_apply
          hydra_lib_lists_bind
          hydra_lib_lists_group
          hydra_lib_lists_last
          hydra_lib_lists_zip
          hydra_lib_lists_zip_with)
  (begin

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

    (define (generic<? a b) (< (generic-compare a b) 0))

    ;; Merge sort
    (define (merge-sort less? lst)
      (define (merge a b)
        (cond
          ((null? a) b)
          ((null? b) a)
          ((less? (car b) (car a)) (cons (car b) (merge a (cdr b))))
          (else (cons (car a) (merge (cdr a) b)))))
      (define (split lst)
        (let loop ((rest lst) (left '()) (right '()) (toggle #t))
          (if (null? rest)
              (cons (reverse left) (reverse right))
              (if toggle
                  (loop (cdr rest) (cons (car rest) left) right #f)
                  (loop (cdr rest) left (cons (car rest) right) #t)))))
      (if (or (null? lst) (null? (cdr lst)))
          lst
          (let ((halves (split lst)))
            (merge (merge-sort less? (car halves))
                   (merge-sort less? (cdr halves))))))

    ;; at :: Int -> [a] -> a
    (define hydra_lib_lists_at
      (lambda (n)
        (lambda (xs)
          (list-ref xs n))))

    ;; concat :: [[a]] -> [a]
    (define hydra_lib_lists_concat
      (lambda (xss)
        (apply append xss)))

    ;; concat2 :: [a] -> [a] -> [a]
    (define hydra_lib_lists_concat2
      (lambda (xs)
        (lambda (ys)
          (append xs ys))))

    ;; cons :: a -> [a] -> [a]
    (define hydra_lib_lists_cons
      (lambda (x)
        (lambda (xs)
          (cons x xs))))

    ;; drop :: Int -> [a] -> [a]
    (define hydra_lib_lists_drop
      (lambda (n)
        (lambda (xs)
          (let loop ((k n) (rest xs))
            (if (or (<= k 0) (null? rest))
                rest
                (loop (- k 1) (cdr rest)))))))

    ;; drop_while :: (a -> Bool) -> [a] -> [a]
    (define hydra_lib_lists_drop_while
      (lambda (pred)
        (lambda (xs)
          (let loop ((rest xs))
            (if (or (null? rest) (not (pred (car rest))))
                rest
                (loop (cdr rest)))))))

    ;; elem :: a -> [a] -> Bool
    (define hydra_lib_lists_elem
      (lambda (x)
        (lambda (xs)
          (let loop ((rest xs))
            (cond
              ((null? rest) #f)
              ((equal? x (car rest)) #t)
              (else (loop (cdr rest))))))))

    ;; filter :: (a -> Bool) -> [a] -> [a]
    (define hydra_lib_lists_filter
      (lambda (pred)
        (lambda (xs)
          (let loop ((rest xs) (acc '()))
            (if (null? rest)
                (reverse acc)
                (if (pred (car rest))
                    (loop (cdr rest) (cons (car rest) acc))
                    (loop (cdr rest) acc)))))))

    ;; find :: (a -> Bool) -> [a] -> Maybe a
    (define hydra_lib_lists_find
      (lambda (pred)
        (lambda (xs)
          (let loop ((rest xs))
            (cond
              ((null? rest) (list 'nothing))
              ((pred (car rest)) (list 'just (car rest)))
              (else (loop (cdr rest))))))))

    ;; foldl :: (b -> a -> b) -> b -> [a] -> b
    (define hydra_lib_lists_foldl
      (lambda (f)
        (lambda (init)
          (lambda (xs)
            (let loop ((acc init) (rest xs))
              (if (null? rest)
                  acc
                  (loop ((f acc) (car rest)) (cdr rest))))))))

    ;; foldr :: (a -> b -> b) -> b -> [a] -> b
    (define hydra_lib_lists_foldr
      (lambda (f)
        (lambda (init)
          (lambda (xs)
            (let loop ((rest (reverse xs)) (acc init))
              (if (null? rest)
                  acc
                  (loop (cdr rest) ((f (car rest)) acc))))))))

    ;; head :: [a] -> a
    (define hydra_lib_lists_head
      (lambda (xs)
        (car xs)))

    ;; init :: [a] -> [a]
    (define hydra_lib_lists_init
      (lambda (xs)
        (if (null? (cdr xs))
            '()
            (cons (car xs) (hydra_lib_lists_init (cdr xs))))))

    ;; intercalate :: [a] -> [[a]] -> [a]
    (define hydra_lib_lists_intercalate
      (lambda (sep)
        (lambda (xss)
          (if (null? xss)
              '()
              (let loop ((rest (cdr xss)) (acc (car xss)))
                (if (null? rest)
                    acc
                    (loop (cdr rest) (append acc sep (car rest)))))))))

    ;; intersperse :: a -> [a] -> [a]
    (define hydra_lib_lists_intersperse
      (lambda (sep)
        (lambda (xs)
          (if (or (null? xs) (null? (cdr xs)))
              xs
              (cons (car xs)
                    (let loop ((rest (cdr xs)))
                      (if (null? rest)
                          '()
                          (cons sep (cons (car rest) (loop (cdr rest)))))))))))

    ;; length :: [a] -> Int
    (define hydra_lib_lists_length
      (lambda (xs)
        (length xs)))

    ;; map :: (a -> b) -> [a] -> [b]
    (define hydra_lib_lists_map
      (lambda (f)
        (lambda (xs)
          (map f xs))))

    ;; nub :: [a] -> [a]  (remove duplicates, keeping first occurrence)
    (define hydra_lib_lists_nub
      (lambda (xs)
        (let loop ((rest xs) (seen '()) (acc '()))
          (if (null? rest)
              (reverse acc)
              (if (member (car rest) seen)
                  (loop (cdr rest) seen acc)
                  (loop (cdr rest) (cons (car rest) seen) (cons (car rest) acc)))))))

    ;; null :: [a] -> Bool
    (define hydra_lib_lists_null
      (lambda (xs)
        (null? xs)))

    ;; partition :: (a -> Bool) -> [a] -> Pair [a] [a]
    (define hydra_lib_lists_partition
      (lambda (pred)
        (lambda (xs)
          (let loop ((rest xs) (yes '()) (no '()))
            (if (null? rest)
                (list (reverse yes) (reverse no))
                (if (pred (car rest))
                    (loop (cdr rest) (cons (car rest) yes) no)
                    (loop (cdr rest) yes (cons (car rest) no))))))))

    ;; pure :: a -> [a]
    (define hydra_lib_lists_pure
      (lambda (x)
        (list x)))

    ;; replicate :: Int -> a -> [a]
    (define hydra_lib_lists_replicate
      (lambda (n)
        (lambda (x)
          (let loop ((k n) (acc '()))
            (if (<= k 0)
                acc
                (loop (- k 1) (cons x acc)))))))

    ;; reverse :: [a] -> [a]
    (define hydra_lib_lists_reverse
      (lambda (xs)
        (reverse xs)))

    ;; safe_head :: [a] -> Maybe a
    (define hydra_lib_lists_safe_head
      (lambda (xs)
        (if (null? xs)
            (list 'nothing)
            (list 'just (car xs)))))

    ;; singleton :: a -> [a]
    (define hydra_lib_lists_singleton
      (lambda (x)
        (list x)))

    ;; sort :: [a] -> [a]
    (define hydra_lib_lists_sort
      (lambda (xs)
        (merge-sort generic<? xs)))

    ;; sort_on :: (a -> b) -> [a] -> [a]
    (define hydra_lib_lists_sort_on
      (lambda (f)
        (lambda (xs)
          (merge-sort (lambda (a b) (generic<? (f a) (f b))) xs))))

    ;; span :: (a -> Bool) -> [a] -> Pair [a] [a]
    (define hydra_lib_lists_span
      (lambda (pred)
        (lambda (xs)
          (let loop ((rest xs) (acc '()))
            (if (or (null? rest) (not (pred (car rest))))
                (list (reverse acc) rest)
                (loop (cdr rest) (cons (car rest) acc)))))))

    ;; tail :: [a] -> [a]
    (define hydra_lib_lists_tail
      (lambda (xs)
        (cdr xs)))

    ;; take :: Int -> [a] -> [a]
    (define hydra_lib_lists_take
      (lambda (n)
        (lambda (xs)
          (let loop ((k n) (rest xs) (acc '()))
            (if (or (<= k 0) (null? rest))
                (reverse acc)
                (loop (- k 1) (cdr rest) (cons (car rest) acc)))))))

    ;; transpose :: [[a]] -> [[a]]
    ;; Haskell behavior: filters out empty sublists (handles ragged matrices)
    (define hydra_lib_lists_transpose
      (lambda (xss)
        (let ((non-empty ((hydra_lib_lists_filter (lambda (xs) (not (null? xs)))) xss)))
          (if (null? non-empty)
              '()
              (cons (map car non-empty)
                    (hydra_lib_lists_transpose (map cdr non-empty)))))))

    ;; zip :: [a] -> [b] -> [Pair a b]
    (define hydra_lib_lists_zip
      (lambda (xs)
        (lambda (ys)
          (let loop ((a xs) (b ys) (acc '()))
            (if (or (null? a) (null? b))
                (reverse acc)
                (loop (cdr a) (cdr b) (cons (list (car a) (car b)) acc)))))))

    ;; zip_with :: (a -> b -> c) -> [a] -> [b] -> [c]
    (define hydra_lib_lists_zip_with
      (lambda (f)
        (lambda (xs)
          (lambda (ys)
            (let loop ((a xs) (b ys) (acc '()))
              (if (or (null? a) (null? b))
                  (reverse acc)
                  (loop (cdr a) (cdr b) (cons ((f (car a)) (car b)) acc))))))))

    ;; apply :: [a -> b] -> [a] -> [b]
    (define hydra_lib_lists_apply
      (lambda (fs)
        (lambda (xs)
          (let loop ((rest fs) (acc '()))
            (if (null? rest)
                (reverse acc)
                (loop (cdr rest)
                      (let inner ((xs2 xs) (a acc))
                        (if (null? xs2) a
                            (inner (cdr xs2) (cons ((car rest) (car xs2)) a))))))))))

    ;; bind :: [a] -> (a -> [b]) -> [b]
    (define hydra_lib_lists_bind
      (lambda (xs)
        (lambda (f)
          (let loop ((rest xs) (acc '()))
            (if (null? rest)
                (reverse acc)
                (loop (cdr rest)
                      (let inner ((ys (f (car rest))) (a acc))
                        (if (null? ys) a
                            (inner (cdr ys) (cons (car ys) a))))))))))

    ;; group :: [a] -> [[a]]
    (define hydra_lib_lists_group
      (lambda (xs)
        (if (null? xs) '()
            (let loop ((rest (cdr xs))
                       (current-val (car xs))
                       (current-group (list (car xs)))
                       (groups '()))
              (if (null? rest)
                  (reverse (cons (reverse current-group) groups))
                  (if (equal? (car rest) current-val)
                      (loop (cdr rest) current-val (cons (car rest) current-group) groups)
                      (loop (cdr rest) (car rest) (list (car rest))
                            (cons (reverse current-group) groups))))))))

    ;; last :: [a] -> a
    (define hydra_lib_lists_last
      (lambda (xs)
        (if (null? (cdr xs))
            (car xs)
            (hydra_lib_lists_last (cdr xs)))))))
