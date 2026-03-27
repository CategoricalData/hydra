;; Load vhash support before the define-library (the bootstrap loader strips
;; define-library and evals the begin body, so this top-level form is harmless
;; in that context; for R7RS library loading, the import clause handles deps).
(use-modules (ice-9 vlist))

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
          hydra_lib_maps_map_keys
          hydra_lib_maps_member
          hydra_lib_maps_null
          hydra_lib_maps_singleton
          hydra_lib_maps_size
          hydra_lib_maps_to_list
          hydra_lib_maps_union)
  (begin

    ;; Persistent maps using Guile's vhash (ice-9 vlist).
    ;; O(1) amortized lookup and insert with structural sharing.
    ;; vhash-cons shadows previous entries for the same key (latest wins on lookup).
    ;; Alists from JSON/generated code are transparently converted on first use.

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

    ;; Convert an alist to a vhash.
    (define (alist->vh alist)
      (let loop ((rest alist) (vh vlist-null))
        (if (null? rest) vh
            (loop (cdr rest) (vhash-cons (caar rest) (cdar rest) vh)))))

    ;; Ensure m is a vhash. Transparently convert alists.
    (define (ensure-vh m)
      (cond
        ((vlist? m) m)
        ((null? m) vlist-null)
        ((pair? m) (alist->vh m))
        (else vlist-null)))

    ;; vhash delete: rebuild without the key.
    (define (vh-delete key vh)
      (vhash-fold (lambda (k v acc)
                    (if (equal? k key) acc
                        (vhash-cons k v acc)))
                  vlist-null vh))

    ;; Deduplicate a vhash into an alist (most recent entry per key wins).
    ;; Uses an alist-based seen set to avoid needing hash tables.
    (define (vh-unique-fold vh)
      (vhash-fold (lambda (k v acc)
                    (let ((seen (car acc))
                          (result (cdr acc)))
                      (if (assoc k seen)
                          acc
                          (cons (cons (cons k #t) seen)
                                (cons (cons k v) result)))))
                  (cons '() '()) vh))

    ;; Alter a value at a key using a function.
    ;; Handle multiple Maybe representations: (nothing), (just v), (maybe '()), (maybe v), '()
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
            (let* ((vh (ensure-vh m))
                   (existing (vhash-assoc k vh))
                   (old-maybe (if existing
                                  (list 'just (cdr existing))
                                  (list 'nothing)))
                   (new-maybe (f old-maybe)))
              (let ((base (if existing (vh-delete k vh) vh)))
                (if (alter-is-nothing? new-maybe)
                    base
                    (vhash-cons k (alter-get-value new-maybe) base))))))))

    ;; Map a function over the keys and values of a map.
    (define hydra_lib_maps_bimap
      (lambda (fk)
        (lambda (fv)
          (lambda (m)
            (vhash-fold (lambda (k v acc) (vhash-cons (fk k) (fv v) acc))
                        vlist-null (ensure-vh m))))))

    ;; Remove a key from a map.
    (define hydra_lib_maps_delete
      (lambda (k)
        (lambda (m)
          (vh-delete k (ensure-vh m)))))

    ;; Get the values of a map.
    (define hydra_lib_maps_elems
      (lambda (m)
        (let ((result (vh-unique-fold (ensure-vh m))))
          (map cdr (cdr result)))))

    ;; Create an empty map.
    (define hydra_lib_maps_empty vlist-null)

    ;; Filter a map based on values.
    (define hydra_lib_maps_filter
      (lambda (pred)
        (lambda (m)
          (vhash-fold (lambda (k v acc)
                        (if (pred v) (vhash-cons k v acc) acc))
                      vlist-null (ensure-vh m)))))

    ;; Filter a map based on key-value pairs.
    (define hydra_lib_maps_filter_with_key
      (lambda (pred)
        (lambda (m)
          (vhash-fold (lambda (k v acc)
                        (if ((pred k) v) (vhash-cons k v acc) acc))
                      vlist-null (ensure-vh m)))))

    ;; Lookup a value with a default.
    (define hydra_lib_maps_find_with_default
      (lambda (def)
        (lambda (k)
          (lambda (m)
            (let ((result (vhash-assoc k (ensure-vh m))))
              (if result (cdr result) def))))))

    ;; Create a map from a list of key-value pairs.
    ;; Input is list of (list k v) pairs. First entry wins for duplicates
    ;; (matching Haskell's Data.Map.fromList which is left-biased).
    (define hydra_lib_maps_from_list
      (lambda (pairs)
        (let loop ((rest (reverse pairs)) (vh vlist-null))
          (if (null? rest) vh
              (loop (cdr rest)
                    (vhash-cons (caar rest) (cadar rest) vh))))))

    ;; Insert a key-value pair into a map.
    ;; vhash-cons shadows previous entries (latest wins on lookup). O(1).
    (define hydra_lib_maps_insert
      (lambda (k)
        (lambda (v)
          (lambda (m)
            (vhash-cons k v (ensure-vh m))))))

    ;; Get the keys of a map.
    (define hydra_lib_maps_keys
      (lambda (m)
        (let ((result (vh-unique-fold (ensure-vh m))))
          (map car (cdr result)))))

    ;; Lookup a value in a map.
    (define hydra_lib_maps_lookup
      (lambda (k)
        (lambda (m)
          (let ((result (vhash-assoc k (ensure-vh m))))
            (if result
                (list 'just (cdr result))
                (list 'nothing))))))

    ;; Map a function over a map.
    (define hydra_lib_maps_map
      (lambda (f)
        (lambda (m)
          (vhash-fold (lambda (k v acc) (vhash-cons k (f v) acc))
                      vlist-null (ensure-vh m)))))

    ;; Map a function over the keys of a map.
    (define hydra_lib_maps_map_keys
      (lambda (f)
        (lambda (m)
          (vhash-fold (lambda (k v acc) (vhash-cons (f k) v acc))
                      vlist-null (ensure-vh m)))))

    ;; Check if a key is present in a map.
    (define hydra_lib_maps_member
      (lambda (k)
        (lambda (m)
          (if (vhash-assoc k (ensure-vh m)) #t #f))))

    ;; Check if a map is empty.
    (define hydra_lib_maps_null
      (lambda (m)
        (cond
          ((vlist? m) (vlist-null? m))
          ((null? m) #t)
          ((pair? m) #f)
          (else #t))))

    ;; Create a map with a single key-value pair.
    (define hydra_lib_maps_singleton
      (lambda (k)
        (lambda (v)
          (vhash-cons k v vlist-null))))

    ;; Get the size of a map.
    (define hydra_lib_maps_size
      (lambda (m)
        (let ((result (vh-unique-fold (ensure-vh m))))
          (length (cdr result)))))

    ;; Convert a map to a list of key-value pairs.
    ;; Sort by key for deterministic output; deduplicate shadowed entries.
    (define hydra_lib_maps_to_list
      (lambda (m)
        (let* ((result (vh-unique-fold (ensure-vh m)))
               (pairs (map (lambda (entry) (list (car entry) (cdr entry)))
                           (cdr result))))
          (sort pairs (lambda (a b) (< (generic-compare (car a) (car b)) 0))))))

    ;; Union two maps, with the first taking precedence.
    ;; Start with m2, then overlay m1 (m1 entries shadow m2 on lookup).
    (define hydra_lib_maps_union
      (lambda (m1)
        (lambda (m2)
          (let ((vh1 (ensure-vh m1))
                (vh2 (ensure-vh m2)))
            (vhash-fold (lambda (k v acc) (vhash-cons k v acc))
                        vh2 vh1)))))))
