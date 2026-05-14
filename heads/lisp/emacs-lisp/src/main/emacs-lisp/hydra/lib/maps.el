;;; maps.el --- Hydra map primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Maps are backed by Emacs `make-hash-table :test 'equal`, with
;; copy-on-write semantics on each mutating operation (insert, delete,
;; union, alter, …). This mirrors the persistent-collection facade
;; pattern used in hydra-java (#359), hydra-python (#362), and the
;; hash-table iteration that hydra-common-lisp's RB-tree maps support
;; via `generic-compare`.
;;
;; The empty map is represented as `nil` (back-compat with prior
;; alist-based code that passes nil for an empty map). Any value-bearing
;; map is a hash-table. `alist-lookup` etc. are kept as compatibility
;; helpers, but all entry points accept either representation on read.
;;
;; Iteration helpers (to_list, keys, elems) return entries in
;; key-sorted order (via `generic-compare`), matching Haskell
;; Data.Map semantics so that downstream serialization is
;; deterministic.

(defun hydra-map-p (m)
  "True if M is a hash-table-backed map (not the nil empty map)."
  (hash-table-p m))

(defun hydra-map-empty-p (m)
  "True if M represents the empty map (nil or empty hash-table)."
  (or (null m) (and (hash-table-p m) (zerop (hash-table-count m)))))

(defun hydra-map-size (m)
  (cond
   ((null m) 0)
   ((hash-table-p m) (hash-table-count m))
   ;; legacy alist
   ((listp m) (length m))
   (t (error "hydra-map-size: not a map: %S" m))))

(defun hydra-map-lookup (k m)
  "Return the cons-cell (k . v) if k is present in M, else nil.
   Accepts hash-table, nil, or legacy alist for M."
  (cond
   ((null m) nil)
   ((hash-table-p m)
    (let ((v (gethash k m :hydra-not-found)))
      (if (eq v :hydra-not-found) nil (cons k v))))
   ((listp m) (assoc k m))
   (t (error "hydra-map-lookup: not a map: %S" m))))

(defun hydra-map-to-hash (m)
  "Return a hash-table view of M. Copies if M is a hash-table, builds
   from alist or empty if not. Result is always safe to mutate."
  (cond
   ((null m) (make-hash-table :test 'equal))
   ((hash-table-p m) (copy-hash-table m))
   ((listp m)
    (let ((h (make-hash-table :test 'equal :size (max 1 (length m)))))
      (dolist (entry m h)
        (puthash (car entry) (cdr entry) h))))
   (t (error "hydra-map-to-hash: not a map: %S" m))))

(defun hydra-map-foreach (f m)
  "Call F on each (k . v) pair in M (any representation)."
  (cond
   ((null m) nil)
   ((hash-table-p m) (maphash (lambda (k v) (funcall f k v)) m))
   ((listp m) (dolist (entry m) (funcall f (car entry) (cdr entry))))
   (t (error "hydra-map-foreach: not a map: %S" m))))

(defun hydra-map-sorted-pairs (m)
  "Return entries of M as a list of (k . v) pairs sorted by key via
   `generic-compare`. Order is deterministic and matches CL/Java
   serialization order."
  (let ((pairs nil))
    (hydra-map-foreach (lambda (k v) (push (cons k v) pairs)) m)
    (sort pairs (lambda (a b) (< (generic-compare (car a) (car b)) 0)))))

;; Back-compat shims for any code still calling the alist helpers
;; directly. New code should use the hash-table-aware variants above.
(defun alist-lookup (key alist)
  (hydra-map-lookup key alist))

(defun alist-insert (key val m)
  "Return M with key→val inserted, copy-on-write."
  (let ((h (hydra-map-to-hash m)))
    (puthash key val h)
    h))

(defun alist-delete (key m)
  "Return M with key removed, copy-on-write."
  (cond
   ((null m) nil)
   ((hash-table-p m)
    (if (eq (gethash key m :hydra-not-found) :hydra-not-found)
        m
      (let ((h (copy-hash-table m)))
        (remhash key h)
        h)))
   ((listp m) (cl-remove-if (lambda (entry) (equal key (car entry))) m))
   (t (error "alist-delete: not a map: %S" m))))

;; alter :: (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
(defun alter-is-nothing-p (m)
  (or (null m)
      (and (consp m) (eq (car m) :nothing))
      (and (consp m) (eq (car m) :maybe)
           (or (null (cdr m)) (null (cadr m))))))

(defun alter-get-value (m)
  (cond
    ((and (consp m) (eq (car m) :just)) (cadr m))
    ((and (consp m) (eq (car m) :maybe))
     (let ((body (cadr m)))
       (if (and (consp body) (eq (car body) :just))
           (cadr body)
         body)))
    (t m)))

(defvar hydra_lib_maps_alter
  (lambda (f)
    "Alter a value at a key using a function."
    (lambda (k)
      (lambda (m)
        (let* ((existing (hydra-map-lookup k m))
               (old-maybe (if existing
                              (list :just (cdr existing))
                              (list :nothing)))
               (new-maybe (funcall f old-maybe)))
          (if (alter-is-nothing-p new-maybe)
              (alist-delete k m)
            (alist-insert k (alter-get-value new-maybe) m)))))))

;; bimap :: (k1 -> k2) -> (v1 -> v2) -> Map k1 v1 -> Map k2 v2
(defvar hydra_lib_maps_bimap
  (lambda (fk)
    "Map a function over the keys and values of a map."
    (lambda (fv)
      (lambda (m)
        (let ((result (make-hash-table :test 'equal :size (max 1 (hydra-map-size m)))))
          (hydra-map-foreach
           (lambda (k v) (puthash (funcall fk k) (funcall fv v) result))
           m)
          result)))))

;; delete :: k -> Map k v -> Map k v
(defvar hydra_lib_maps_delete
  (lambda (k)
    "Remove a key from a map."
    (lambda (m)
      (alist-delete k m))))

;; elems :: Map k v -> [v]
(defvar hydra_lib_maps_elems
  (lambda (m)
    "Get the values of a map, ordered by key."
    (mapcar #'cdr (hydra-map-sorted-pairs m))))

;; empty :: Map k v
(defvar hydra_lib_maps_empty nil
  "Create an empty map.")

;; filter :: (v -> Bool) -> Map k v -> Map k v
(defvar hydra_lib_maps_filter
  (lambda (pred)
    "Filter a map based on values."
    (lambda (m)
      (let ((result (make-hash-table :test 'equal)))
        (hydra-map-foreach
         (lambda (k v) (when (funcall pred v) (puthash k v result)))
         m)
        result))))

;; filter_with_key :: (k -> v -> Bool) -> Map k v -> Map k v
(defvar hydra_lib_maps_filter_with_key
  (lambda (pred)
    "Filter a map based on key-value pairs."
    (lambda (m)
      (let ((result (make-hash-table :test 'equal)))
        (hydra-map-foreach
         (lambda (k v) (when (funcall (funcall pred k) v) (puthash k v result)))
         m)
        result))))

;; find_with_default :: v -> k -> Map k v -> v
(defvar hydra_lib_maps_find_with_default
  (lambda (def)
    "Lookup a value with a default."
    (lambda (k)
      (lambda (m)
        (let ((entry (hydra-map-lookup k m)))
          (if entry (cdr entry) def))))))

;; from_list :: [Pair k v] -> Map k v
;; Input is list of (list k v) pairs
(defvar hydra_lib_maps_from_list
  (lambda (pairs)
    "Create a map from a list of key-value pairs."
    (let ((result (make-hash-table :test 'equal :size (max 1 (length pairs)))))
      (dolist (p pairs result)
        (puthash (car p) (cadr p) result)))))

;; insert :: k -> v -> Map k v -> Map k v
(defvar hydra_lib_maps_insert
  (lambda (k)
    "Insert a key-value pair into a map."
    (lambda (v)
      (lambda (m)
        (alist-insert k v m)))))

;; keys :: Map k v -> [k]
(defvar hydra_lib_maps_keys
  (lambda (m)
    "Get the keys of a map, in sorted order."
    (mapcar #'car (hydra-map-sorted-pairs m))))

;; lookup :: k -> Map k v -> Maybe v
(defvar hydra_lib_maps_lookup
  (lambda (k)
    "Lookup a value in a map."
    (lambda (m)
      (let ((entry (hydra-map-lookup k m)))
        (if entry
            (list :just (cdr entry))
            (list :nothing))))))

;; map :: (v1 -> v2) -> Map k v1 -> Map k v2
(defvar hydra_lib_maps_map
  (lambda (f)
    "Map a function over a map."
    (lambda (m)
      (let ((result (make-hash-table :test 'equal :size (max 1 (hydra-map-size m)))))
        (hydra-map-foreach
         (lambda (k v) (puthash k (funcall f v) result))
         m)
        result))))

;; map_keys :: (k1 -> k2) -> Map k1 v -> Map k2 v
(defvar hydra_lib_maps_map_keys
  (lambda (f)
    "Map a function over the keys of a map."
    (lambda (m)
      (let ((result (make-hash-table :test 'equal :size (max 1 (hydra-map-size m)))))
        (hydra-map-foreach
         (lambda (k v) (puthash (funcall f k) v result))
         m)
        result))))

;; member :: k -> Map k v -> Bool
(defvar hydra_lib_maps_member
  (lambda (k)
    "Check if a key is present in a map."
    (lambda (m)
      (if (hydra-map-lookup k m) t nil))))

;; null :: Map k v -> Bool
(defvar hydra_lib_maps_null
  (lambda (m)
    "Check if a map is empty."
    (hydra-map-empty-p m)))

;; singleton :: k -> v -> Map k v
(defvar hydra_lib_maps_singleton
  (lambda (k)
    "Create a map with a single key-value pair."
    (lambda (v)
      (let ((h (make-hash-table :test 'equal :size 1)))
        (puthash k v h)
        h))))

;; size :: Map k v -> Int
(defvar hydra_lib_maps_size
  (lambda (m)
    "Get the size of a map."
    (hydra-map-size m)))

;; to_list :: Map k v -> [Pair k v]
(defvar hydra_lib_maps_to_list
  (lambda (m)
    "Convert a map to a list of key-value pairs, in sorted-by-key order."
    (mapcar (lambda (entry) (list (car entry) (cdr entry)))
            (hydra-map-sorted-pairs m))))

;; union :: Map k v -> Map k v -> Map k v
;; Left-biased: entries from first map take precedence
(defvar hydra_lib_maps_union
  (lambda (m1)
    "Union two maps, with the first taking precedence."
    (lambda (m2)
      (let ((result (hydra-map-to-hash m2)))
        ;; m1 entries overwrite m2 entries (left-biased)
        (hydra-map-foreach (lambda (k v) (puthash k v result)) m1)
        result))))

(provide 'hydra.lib.maps)
