(define-library (hydra lib maps)
  (import (scheme base) (scheme write))
  (export hydra_lib_maps_alter
          hydra_lib_maps_bimap
          hydra_lib_maps_delete
          hydra_lib_maps_elems
          hydra_lib_maps_empty
          hydra_lib_maps_filter
          hydra_lib_maps_filter_with_key
          hydra_lib_maps_find_with_default
          hydra_lib_maps_from_list
          hydra_lib_maps_insert
          hydra_lib_maps_keys
          hydra_lib_maps_lookup
          hydra_lib_maps_map
          hydra_lib_maps_member
          hydra_lib_maps_null
          hydra_lib_maps_singleton
          hydra_lib_maps_size
          hydra_lib_maps_to_list
          hydra_lib_maps_map_keys
          hydra_lib_maps_union)
  (begin

    ;; Maps are association lists sorted by key: ((k1 . v1) (k2 . v2) ...)

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

    (define (alist-lookup key alist)
      (cond
        ((null? alist) #f)
        ((equal? key (caar alist)) (car alist))
        (else (alist-lookup key (cdr alist)))))

    (define (alist-insert key val alist)
      ;; Insert maintaining sorted order by key
      (cond
        ((null? alist) (list (cons key val)))
        ((= (generic-compare key (caar alist)) 0)
         (cons (cons key val) (cdr alist)))
        ((< (generic-compare key (caar alist)) 0)
         (cons (cons key val) alist))
        (else (cons (car alist) (alist-insert key val (cdr alist))))))

    (define (alist-delete key alist)
      (cond
        ((null? alist) '())
        ((equal? key (caar alist)) (cdr alist))
        (else (cons (car alist) (alist-delete key (cdr alist))))))

    ;; alter :: (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
    ;; Handle multiple Maybe representations: (nothing), (just v), (maybe '()), (maybe v), '()
    (define (alter-is-nothing? m)
      (or (null? m)
          (and (pair? m) (eq? (car m) 'nothing))
          (and (pair? m) (eq? (car m) 'maybe)
               (or (null? (cdr m)) (null? (cadr m))))))
    (define (alter-get-value m)
      (cond
        ((and (pair? m) (eq? (car m) 'just)) (cadr m))
        ((and (pair? m) (eq? (car m) 'maybe)) (cadr m))
        (else m)))
    (define hydra_lib_maps_alter
      (lambda (f)
        (lambda (k)
          (lambda (m)
            (let* ((existing (alist-lookup k m))
                   (old-maybe (if existing
                                  (list 'just (cdr existing))
                                  (list 'nothing '())))
                   (new-maybe (f old-maybe)))
              (if (alter-is-nothing? new-maybe)
                  (alist-delete k m)
                  (alist-insert k (alter-get-value new-maybe) m)))))))

    ;; bimap :: (k1 -> k2) -> (v1 -> v2) -> Map k1 v1 -> Map k2 v2
    (define hydra_lib_maps_bimap
      (lambda (fk)
        (lambda (fv)
          (lambda (m)
            (let loop ((rest m) (acc '()))
              (if (null? rest)
                  ;; Re-sort by new keys
                  (let sort-loop ((remaining (reverse acc)) (result '()))
                    (if (null? remaining)
                        result
                        (sort-loop (cdr remaining)
                                   (alist-insert (caar remaining) (cdar remaining) result))))
                  (loop (cdr rest)
                        (cons (cons (fk (caar rest)) (fv (cdar rest))) acc))))))))

    ;; delete :: k -> Map k v -> Map k v
    (define hydra_lib_maps_delete
      (lambda (k)
        (lambda (m)
          (alist-delete k m))))

    ;; elems :: Map k v -> [v]
    (define hydra_lib_maps_elems
      (lambda (m)
        (map cdr m)))

    ;; empty :: Map k v
    (define hydra_lib_maps_empty '())

    ;; filter :: (v -> Bool) -> Map k v -> Map k v
    (define hydra_lib_maps_filter
      (lambda (pred)
        (lambda (m)
          (let loop ((rest m) (acc '()))
            (if (null? rest)
                (reverse acc)
                (if (pred (cdar rest))
                    (loop (cdr rest) (cons (car rest) acc))
                    (loop (cdr rest) acc)))))))

    ;; filter_with_key :: (k -> v -> Bool) -> Map k v -> Map k v
    (define hydra_lib_maps_filter_with_key
      (lambda (pred)
        (lambda (m)
          (let loop ((rest m) (acc '()))
            (if (null? rest)
                (reverse acc)
                (if ((pred (caar rest)) (cdar rest))
                    (loop (cdr rest) (cons (car rest) acc))
                    (loop (cdr rest) acc)))))))

    ;; find_with_default :: v -> k -> Map k v -> v
    (define hydra_lib_maps_find_with_default
      (lambda (def)
        (lambda (k)
          (lambda (m)
            (let ((entry (alist-lookup k m)))
              (if entry (cdr entry) def))))))

    ;; from_list :: [Pair k v] -> Map k v
    ;; Input is list of (list k v) pairs
    (define hydra_lib_maps_from_list
      (lambda (pairs)
        (let loop ((rest pairs) (acc '()))
          (if (null? rest)
              acc
              (let ((p (car rest)))
                (loop (cdr rest)
                      (alist-insert (car p) (cadr p) acc)))))))

    ;; insert :: k -> v -> Map k v -> Map k v
    (define hydra_lib_maps_insert
      (lambda (k)
        (lambda (v)
          (lambda (m)
            (alist-insert k v m)))))

    ;; keys :: Map k v -> [k]
    (define hydra_lib_maps_keys
      (lambda (m)
        (map car m)))

    ;; lookup :: k -> Map k v -> Maybe v
    (define hydra_lib_maps_lookup
      (lambda (k)
        (lambda (m)
          (let ((entry (alist-lookup k m)))
            (if entry
                (list 'just (cdr entry))
                (list 'nothing '()))))))

    ;; map :: (v1 -> v2) -> Map k v1 -> Map k v2
    (define hydra_lib_maps_map
      (lambda (f)
        (lambda (m)
          (map (lambda (entry) (cons (car entry) (f (cdr entry)))) m))))

    ;; member :: k -> Map k v -> Bool
    (define hydra_lib_maps_member
      (lambda (k)
        (lambda (m)
          (if (alist-lookup k m) #t #f))))

    ;; null :: Map k v -> Bool
    (define hydra_lib_maps_null
      (lambda (m)
        (null? m)))

    ;; singleton :: k -> v -> Map k v
    (define hydra_lib_maps_singleton
      (lambda (k)
        (lambda (v)
          (list (cons k v)))))

    ;; size :: Map k v -> Int
    (define hydra_lib_maps_size
      (lambda (m)
        (length m)))

    ;; to_list :: Map k v -> [Pair k v]
    (define hydra_lib_maps_to_list
      (lambda (m)
        (map (lambda (entry) (list (car entry) (cdr entry))) m)))

    ;; map_keys :: (k1 -> k2) -> Map k1 v -> Map k2 v
    (define hydra_lib_maps_map_keys
      (lambda (f)
        (lambda (m)
          (let loop ((rest m) (acc '()))
            (if (null? rest)
                acc
                (loop (cdr rest)
                      (alist-insert (f (caar rest)) (cdar rest) acc)))))))

    ;; union :: Map k v -> Map k v -> Map k v
    ;; Left-biased: entries from first map take precedence
    (define hydra_lib_maps_union
      (lambda (m1)
        (lambda (m2)
          (let loop ((rest m2) (acc m1))
            (if (null? rest)
                acc
                (let ((k (caar rest)) (v (cdar rest)))
                  (if (alist-lookup k acc)
                      (loop (cdr rest) acc)
                      (loop (cdr rest) (alist-insert k v acc)))))))))))
