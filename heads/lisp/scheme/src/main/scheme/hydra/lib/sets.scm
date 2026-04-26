(define-library (hydra lib sets)
  (import (scheme base)
          (scheme write)
          (only (guile) make-hash-table hash-ref hash-set! hash-fold sort))
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
          hydra_lib_sets_size
          hydra_lib_sets_to_list
          hydra_lib_sets_union
          hydra_lib_sets_unions)
  (begin

    ;; Sets use Guile's vhash for O(1) amortized membership test and insert.
    ;; A set is a vhash mapping elements to #t.
    ;; Sorted lists from generated code are transparently converted on first use.
    ;;
    ;; Note: (ice-9 vlist) is brought in via use-modules rather than the
    ;; define-library import block because the bootstrap loader strips the
    ;; define-library wrapper and evaluates body forms in (interaction-environment).
    ;; Imports declared in the wrapper would be lost; use-modules executes as a
    ;; body form and persists.
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

    ;; Convert a sorted list to a vhash set
    (define (list->vhset lst)
      (let loop ((rest lst) (vh vlist-null))
        (if (null? rest) vh
            (loop (cdr rest) (vhash-cons (car rest) #t vh)))))

    ;; Ensure s is a vhash set. Transparently convert sorted lists.
    (define (ensure-vhset s)
      (cond
        ((vlist? s) s)
        ((null? s) vlist-null)
        ((pair? s) (list->vhset s))
        (else vlist-null)))

    ;; Delete an element from a set.
    (define hydra_lib_sets_delete
      (lambda (x)
        (lambda (s)
          (vhash-fold (lambda (k v acc)
                        (if (equal? k x) acc (vhash-cons k #t acc)))
                      vlist-null (ensure-vhset s)))))

    ;; Compute the difference of two sets.
    (define hydra_lib_sets_difference
      (lambda (s1)
        (lambda (s2)
          (let ((vh2 (ensure-vhset s2)))
            (vhash-fold (lambda (k v acc)
                          (if (vhash-assoc k vh2) acc (vhash-cons k #t acc)))
                        vlist-null (ensure-vhset s1))))))

    ;; Create an empty set.
    (define hydra_lib_sets_empty vlist-null)

    ;; Create a set from a list.
    (define hydra_lib_sets_from_list
      (lambda (xs)
        (let loop ((rest xs) (vh vlist-null))
          (if (null? rest) vh
              (loop (cdr rest) (vhash-cons (car rest) #t vh))))))

    ;; Insert an element into a set. O(1) via vhash-cons.
    (define hydra_lib_sets_insert
      (lambda (x)
        (lambda (s)
          (vhash-cons x #t (ensure-vhset s)))))

    ;; Compute the intersection of two sets.
    (define hydra_lib_sets_intersection
      (lambda (s1)
        (lambda (s2)
          (let ((vh2 (ensure-vhset s2)))
            (vhash-fold (lambda (k v acc)
                          (if (vhash-assoc k vh2) (vhash-cons k #t acc) acc))
                        vlist-null (ensure-vhset s1))))))

    ;; Map a function over a set.
    (define hydra_lib_sets_map
      (lambda (f)
        (lambda (s)
          (vhash-fold (lambda (k v acc) (vhash-cons (f k) #t acc))
                      vlist-null (ensure-vhset s)))))

    ;; Check if an element is in a set. O(1) via vhash-assoc.
    (define hydra_lib_sets_member
      (lambda (x)
        (lambda (s)
          (if (vhash-assoc x (ensure-vhset s)) #t #f))))

    ;; Check if a set is empty.
    (define hydra_lib_sets_null
      (lambda (s)
        (cond
          ((vlist? s) (vlist-null? s))
          ((null? s) #t)
          ((pair? s) #f)
          (else #t))))

    ;; Create a singleton set.
    (define hydra_lib_sets_singleton
      (lambda (x)
        (vhash-cons x #t vlist-null)))

    ;; Get the size of a set.
    ;; Note: vhash may have shadowed duplicates, but for sets created through
    ;; our API, duplicates don't change membership semantics.
    ;; We count unique keys for correctness.
    (define hydra_lib_sets_size
      (lambda (s)
        (if (vlist? s)
            (let ((seen (make-hash-table)))
              (vhash-fold (lambda (k v acc)
                            (if (hash-ref seen k #f) acc
                                (begin (hash-set! seen k #t) (+ acc 1))))
                          0 s))
            (length s))))

    ;; Convert a set to a sorted list.
    (define hydra_lib_sets_to_list
      (lambda (s)
        (let ((seen (make-hash-table)))
          (let ((unique (vhash-fold (lambda (k v acc)
                                      (if (hash-ref seen k #f) acc
                                          (begin (hash-set! seen k #t)
                                                 (cons k acc))))
                                    '() (ensure-vhset s))))
            (sort unique (lambda (a b) (< (generic-compare a b) 0)))))))

    ;; Compute the union of two sets.
    (define hydra_lib_sets_union
      (lambda (s1)
        (lambda (s2)
          (let ((vh1 (ensure-vhset s1))
                (vh2 (ensure-vhset s2)))
            (vhash-fold (lambda (k v acc) (vhash-cons k #t acc))
                        vh2 vh1)))))

    ;; Compute the union of multiple sets.
    (define hydra_lib_sets_unions
      (lambda (sets)
        (let loop ((rest sets) (acc vlist-null))
          (if (null? rest)
              acc
              (loop (cdr rest)
                    ((hydra_lib_sets_union acc) (car rest)))))))))
