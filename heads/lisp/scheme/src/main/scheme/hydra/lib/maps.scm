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

    ;; Maps use Guile's vhash for O(1) amortized lookup/insert.
    ;; Operations that produce ordered output (to_list, keys, elems, map)
    ;; deduplicate and sort on demand. The previous implementation used
    ;; sorted alists, which made from_list/insert quadratic — fatally slow
    ;; for the bootstrap codegen pipeline (large namespace_map and
    ;; schema_types maps built via repeated insertion).
    ;;
    ;; (ice-9 vlist) is brought in via use-modules rather than the
    ;; define-library import block because the bootstrap loader strips the
    ;; define-library wrapper and evaluates body forms in
    ;; (interaction-environment); imports declared in the wrapper would be
    ;; lost. Mirrors sets.scm.
    (use-modules (ice-9 vlist))

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
           (if (= c 0) (generic-compare (cdr a) (cdr b)) c)))
        ((and (null? a) (null? b)) 0)
        ((null? a) -1)
        ((null? b) 1)
        (else (let ((sa (obj->string a)) (sb (obj->string b)))
                (cond ((string<? sa sb) -1) ((string=? sa sb) 0) (else 1))))))

    ;; Convert a representation to a vhash. Accepts:
    ;;   - vlist (treated as already a vhash)
    ;;   - alist of pairs ((k . v) ...) (legacy / generated-code form)
    ;;   - empty list
    ;; Generated code constructs maps as alists of pairs via
    ;; ((k1 . v1) (k2 . v2) ...) — those are converted on first use.
    (define (alist-pair? x) (and (pair? x) (not (pair? (car x)))))
    (define (ensure-vhash m)
      (cond
        ((vlist? m) m)
        ((null? m) vlist-null)
        ((pair? m)
         ;; Could be an alist of pairs ((k . v) ...) or a single pair.
         (let loop ((rest m) (vh vlist-null))
           (if (null? rest) vh
               (let ((p (car rest)))
                 (loop (cdr rest)
                       (cond
                         ((pair? p) (vhash-cons (car p) (cdr p) vh))
                         (else vh)))))))
        (else vlist-null)))

    ;; Get unique entries (key . val) preferring most-recently-cons'd value.
    (define (vhash-unique-entries vh)
      (let ((seen (make-hash-table)))
        (vhash-fold (lambda (k v acc)
                      (if (hash-ref seen k #f) acc
                          (begin (hash-ref seen k #t) ; mark
                                 (hash-set! seen k #t)
                                 (cons (cons k v) acc))))
                    '() vh)))

    ;; Sort entries by key.
    (define (sort-entries-by-key entries)
      (sort entries (lambda (a b) (< (generic-compare (car a) (car b)) 0))))

    (define (vhash-sorted-entries vh)
      (sort-entries-by-key (vhash-unique-entries vh)))

    ;; alter :: (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
    ;; Handle multiple Maybe representations.
    (define (alter-is-nothing? m)
      (or (null? m)
          (and (pair? m) (eq? (car m) 'nothing))
          (and (pair? m) (eq? (car m) 'maybe)
               (or (null? (cdr m)) (null? (cadr m))))))
    (define (alter-get-value m)
      (cond
        ((and (pair? m) (eq? (car m) 'just)) (cadr m))
        ((and (pair? m) (eq? (car m) 'maybe))
         (let ((body (cadr m)))
           (if (and (pair? body) (eq? (car body) 'just))
               (cadr body)
               body)))
        (else m)))
    (define hydra_lib_maps_alter
      (lambda (f)
        (lambda (k)
          (lambda (m)
            (let* ((vh (ensure-vhash m))
                   (existing (vhash-assoc k vh))
                   (old-maybe (if existing
                                  (list 'just (cdr existing))
                                  (list 'nothing)))
                   (new-maybe (f old-maybe)))
              (if (alter-is-nothing? new-maybe)
                  ;; Delete: rebuild without k
                  (vhash-fold (lambda (kk vv acc)
                                (if (equal? kk k) acc (vhash-cons kk vv acc)))
                              vlist-null vh)
                  ;; Insert: vhash-cons shadows older entry
                  (vhash-cons k (alter-get-value new-maybe) vh)))))))

    ;; bimap :: (k1 -> k2) -> (v1 -> v2) -> Map k1 v1 -> Map k2 v2
    (define hydra_lib_maps_bimap
      (lambda (fk)
        (lambda (fv)
          (lambda (m)
            (let ((entries (vhash-unique-entries (ensure-vhash m))))
              (let loop ((rest entries) (vh vlist-null))
                (if (null? rest) vh
                    (let ((p (car rest)))
                      (loop (cdr rest)
                            (vhash-cons (fk (car p)) (fv (cdr p)) vh))))))))))

    ;; delete :: k -> Map k v -> Map k v
    (define hydra_lib_maps_delete
      (lambda (k)
        (lambda (m)
          (let ((vh (ensure-vhash m)))
            (vhash-fold (lambda (kk vv acc)
                          (if (equal? kk k) acc (vhash-cons kk vv acc)))
                        vlist-null vh)))))

    ;; elems :: Map k v -> [v]
    ;; Returned in key-sorted order (matching Haskell Data.Map semantics).
    (define hydra_lib_maps_elems
      (lambda (m)
        (map cdr (vhash-sorted-entries (ensure-vhash m)))))

    ;; empty :: Map k v
    (define hydra_lib_maps_empty vlist-null)

    ;; filter :: (v -> Bool) -> Map k v -> Map k v
    (define hydra_lib_maps_filter
      (lambda (pred)
        (lambda (m)
          (let ((vh (ensure-vhash m)))
            (vhash-fold (lambda (k v acc)
                          (if (pred v) (vhash-cons k v acc) acc))
                        vlist-null vh)))))

    ;; filter_with_key :: (k -> v -> Bool) -> Map k v -> Map k v
    (define hydra_lib_maps_filter_with_key
      (lambda (pred)
        (lambda (m)
          (let ((vh (ensure-vhash m)))
            (vhash-fold (lambda (k v acc)
                          (if ((pred k) v) (vhash-cons k v acc) acc))
                        vlist-null vh)))))

    ;; find_with_default :: v -> k -> Map k v -> v
    (define hydra_lib_maps_find_with_default
      (lambda (def)
        (lambda (k)
          (lambda (m)
            (let ((entry (vhash-assoc k (ensure-vhash m))))
              (if entry (cdr entry) def))))))

    ;; from_list :: [Pair k v] -> Map k v
    ;; Input is list of (list k v) pairs.
    (define hydra_lib_maps_from_list
      (lambda (pairs)
        (let loop ((rest pairs) (vh vlist-null))
          (if (null? rest) vh
              (let ((p (car rest)))
                (loop (cdr rest)
                      (vhash-cons (car p) (cadr p) vh)))))))

    ;; insert :: k -> v -> Map k v -> Map k v
    ;; vhash-cons shadows the older entry, so subsequent lookups see the new value.
    (define hydra_lib_maps_insert
      (lambda (k)
        (lambda (v)
          (lambda (m)
            (vhash-cons k v (ensure-vhash m))))))

    ;; keys :: Map k v -> [k]
    (define hydra_lib_maps_keys
      (lambda (m)
        (map car (vhash-sorted-entries (ensure-vhash m)))))

    ;; lookup :: k -> Map k v -> Maybe v
    (define hydra_lib_maps_lookup
      (lambda (k)
        (lambda (m)
          (let ((entry (vhash-assoc k (ensure-vhash m))))
            (if entry
                (list 'just (cdr entry))
                (list 'nothing))))))

    ;; map :: (v1 -> v2) -> Map k v1 -> Map k v2
    (define hydra_lib_maps_map
      (lambda (f)
        (lambda (m)
          (let ((entries (vhash-unique-entries (ensure-vhash m))))
            (let loop ((rest entries) (vh vlist-null))
              (if (null? rest) vh
                  (let ((p (car rest)))
                    (loop (cdr rest)
                          (vhash-cons (car p) (f (cdr p)) vh)))))))))

    ;; member :: k -> Map k v -> Bool
    (define hydra_lib_maps_member
      (lambda (k)
        (lambda (m)
          (if (vhash-assoc k (ensure-vhash m)) #t #f))))

    ;; null :: Map k v -> Bool
    (define hydra_lib_maps_null
      (lambda (m)
        (cond
          ((vlist? m) (vlist-null? m))
          ((null? m) #t)
          ((pair? m) #f)
          (else #t))))

    ;; singleton :: k -> v -> Map k v
    (define hydra_lib_maps_singleton
      (lambda (k)
        (lambda (v)
          (vhash-cons k v vlist-null))))

    ;; size :: Map k v -> Int
    ;; Counts unique keys (vhash may have shadowed duplicates).
    (define hydra_lib_maps_size
      (lambda (m)
        (length (vhash-unique-entries (ensure-vhash m)))))

    ;; to_list :: Map k v -> [Pair k v]
    ;; Returned in key-sorted order, as (list k v) pairs.
    (define hydra_lib_maps_to_list
      (lambda (m)
        (map (lambda (p) (list (car p) (cdr p)))
             (vhash-sorted-entries (ensure-vhash m)))))

    ;; map_keys :: (k1 -> k2) -> Map k1 v -> Map k2 v
    (define hydra_lib_maps_map_keys
      (lambda (f)
        (lambda (m)
          (let ((entries (vhash-unique-entries (ensure-vhash m))))
            (let loop ((rest entries) (vh vlist-null))
              (if (null? rest) vh
                  (let ((p (car rest)))
                    (loop (cdr rest)
                          (vhash-cons (f (car p)) (cdr p) vh)))))))))

    ;; union :: Map k v -> Map k v -> Map k v
    ;; Left-biased: entries from first map take precedence.
    ;; vhash-cons shadows older entries, so we cons m1's entries last.
    (define hydra_lib_maps_union
      (lambda (m1)
        (lambda (m2)
          (let ((vh1 (ensure-vhash m1))
                (vh2 (ensure-vhash m2)))
            ;; Start from m2, then cons m1's entries on top so they shadow.
            (vhash-fold (lambda (k v acc) (vhash-cons k v acc))
                        vh2
                        vh1)))))))
