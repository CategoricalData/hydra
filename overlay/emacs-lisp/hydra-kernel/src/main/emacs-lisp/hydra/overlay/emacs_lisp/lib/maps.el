;;; maps.el --- Hydra map primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Maps are backed by a cons-prepended alist of (k . v) pairs, structurally
;; shared between versions: `insert`/`delete`/`alter` prepend a new entry (or
;; a tombstone) in O(1) without copying the existing list, so old references
;; stay valid and cheap (persistent-map semantics). Newer entries shadow
;; older ones with the same key; a plain `assoc` walk (`hydra-map-lookup`)
;; finds the most recent entry first, so single lookups stay correct without
;; deduplication.
;;
;; Read operations that need a canonical view (to_list, keys, elems, foreach,
;; size, bimap, filter, map, map_keys, union) call `hydra-map-dedup`, which
;; walks once, keeps only the first (= most recent) occurrence of each key,
;; and drops delete-tombstones. This mirrors the "sorted alists made
;; from_list/insert quadratic -- fatally slow for the bootstrap codegen
;; pipeline" fix already applied to the Scheme host (overlay/scheme/.../lib/
;; maps.scm, which switched sorted alists to Guile's vhash for the same
;; reason). The prior Emacs Lisp implementation used `make-hash-table` with
;; `copy-hash-table` on every insert/delete -- O(n) per mutation, making a
;; sequential n-key fold-insert O(n^2); confirmed by benchmark (#586: 8000
;; sequential inserts took 5.4s in a superlinear ~n^1.5-n^2 curve, dominating
;; the self-hosted EL kernel's own type-inference/codegen passes, which build
;; environment/substitution maps via exactly that fold-insert pattern).
;;
;; The empty map is represented as `nil`. Any value-bearing map is a list of
;; (k . v) pairs (most recent first) possibly containing shadowed/tombstoned
;; entries pending dedup. A tombstone is a cons of key to the sentinel
;; `hydra-map-tombstone`.
;;
;; Iteration helpers (to_list, keys, elems) return entries in key-sorted
;; order (via `generic-compare`), matching Haskell Data.Map semantics so
;; that downstream serialization is deterministic.

(defconst hydra-map-tombstone (make-symbol "hydra-map-tombstone")
  "Sentinel value marking a deleted key in the pending (undeduplicated) list.")

(defun hydra-map-p (m)
  "True if M is a non-empty map (a cons)."
  (consp m))

(defun hydra-map-lookup (k m)
  "Return the cons-cell (k . v) if k is present in M, else nil.
   Walks M from the front (most recent entries first), so the first match
   for K is authoritative; a tombstone match means K is deleted."
  (let ((entry (assoc k m)))
    (if (and entry (eq (cdr entry) hydra-map-tombstone))
        nil
      entry)))

(defun hydra-map-dedup (m)
  "Return M's entries as a list of (k . v) pairs, most-recent value per key,
   tombstones dropped, in unspecified order."
  (let ((seen (make-hash-table :test 'equal))
        (acc nil))
    (dolist (entry m (nreverse acc))
      (let ((k (car entry)))
        (unless (gethash k seen)
          (puthash k t seen)
          (unless (eq (cdr entry) hydra-map-tombstone)
            (push entry acc)))))))

(defun hydra-map-empty-p (m)
  "True if M represents the empty map."
  (null (hydra-map-dedup m)))

(defun hydra-map-size (m)
  (length (hydra-map-dedup m)))

(defun hydra-map-foreach (f m)
  "Call F on each (k . v) pair in M's deduplicated view."
  (dolist (entry (hydra-map-dedup m))
    (funcall f (car entry) (cdr entry))))

(defun hydra-map-sorted-pairs (m)
  "Return entries of M as a list of (k . v) pairs sorted by key via
   `generic-compare`. Order is deterministic and matches CL/Java
   serialization order."
  (sort (hydra-map-dedup m)
        (lambda (a b) (< (generic-compare (car a) (car b)) 0))))

;; Back-compat shims for any code still calling the alist helpers directly.
(defun alist-lookup (key alist)
  (hydra-map-lookup key alist))

(defun alist-insert (key val m)
  "Return M with key -> val inserted. O(1): prepends without copying."
  (cons (cons key val) m))

(defun alist-delete (key m)
  "Return M with key removed. O(1): prepends a tombstone without copying."
  (cons (cons key hydra-map-tombstone) m))

;; alter :: (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
(defun alter-is-nothing-p (m)
  (or (null m)
      (and (consp m) (eq (car m) :none))
      (and (consp m) (eq (car m) :optional)
           (or (null (cdr m)) (null (cadr m))))))

(defun alter-get-value (m)
  (cond
    ((and (consp m) (eq (car m) :given)) (cadr m))
    ((and (consp m) (eq (car m) :optional))
     (let ((body (cadr m)))
       (if (and (consp body) (eq (car body) :given))
           (cadr body)
         body)))
    (t m)))

(defvar hydra_overlay_emacs_lisp_lib_maps_alter
  (lambda (f)
    "Alter a value at a key using a function."
    (lambda (k)
      (lambda (m)
        (let* ((existing (hydra-map-lookup k m))
               (old-maybe (if existing
                              (list :given (cdr existing))
                              (list :none)))
               (new-maybe (funcall f old-maybe)))
          (if (alter-is-nothing-p new-maybe)
              (alist-delete k m)
            (alist-insert k (alter-get-value new-maybe) m)))))))

;; bimap :: (k1 -> k2) -> (v1 -> v2) -> Map k1 v1 -> Map k2 v2
(defvar hydra_overlay_emacs_lisp_lib_maps_bimap
  (lambda (fk)
    "Map a function over the keys and values of a map."
    (lambda (fv)
      (lambda (m)
        (let (result)
          (hydra-map-foreach
           (lambda (k v) (push (cons (funcall fk k) (funcall fv v)) result))
           m)
          result)))))

;; delete :: k -> Map k v -> Map k v
(defvar hydra_overlay_emacs_lisp_lib_maps_delete
  (lambda (k)
    "Remove a key from a map."
    (lambda (m)
      (alist-delete k m))))

;; elems :: Map k v -> [v]
(defvar hydra_overlay_emacs_lisp_lib_maps_elems
  (lambda (m)
    "Get the values of a map, ordered by key."
    (mapcar #'cdr (hydra-map-sorted-pairs m))))

;; empty :: Map k v
(defvar hydra_overlay_emacs_lisp_lib_maps_empty nil
  "Create an empty map.")

;; filter :: (v -> Bool) -> Map k v -> Map k v
(defvar hydra_overlay_emacs_lisp_lib_maps_filter
  (lambda (pred)
    "Filter a map based on values."
    (lambda (m)
      (let (result)
        (hydra-map-foreach
         (lambda (k v) (when (funcall pred v) (push (cons k v) result)))
         m)
        result))))

;; filter_with_key :: (k -> v -> Bool) -> Map k v -> Map k v
(defvar hydra_overlay_emacs_lisp_lib_maps_filter_with_key
  (lambda (pred)
    "Filter a map based on key-value pairs."
    (lambda (m)
      (let (result)
        (hydra-map-foreach
         (lambda (k v) (when (funcall (funcall pred k) v) (push (cons k v) result)))
         m)
        result))))

;; find_with_default :: v -> k -> Map k v -> v
;; Thunk-aware: the default is lazy (#391); if def is a zero-arg function, only call it on a miss.
(defvar hydra_overlay_emacs_lisp_lib_maps_find_with_default
  (lambda (def)
    "Lookup a value with a default."
    (lambda (k)
      (lambda (m)
        (let ((entry (hydra-map-lookup k m)))
          (if entry (cdr entry) (if (functionp def) (funcall def) def)))))))

;; from_list :: [Pair k v] -> Map k v
;; Input is list of (list k v) pairs
(defvar hydra_overlay_emacs_lisp_lib_maps_from_list
  (lambda (pairs)
    "Create a map from a list of key-value pairs."
    (let (result)
      (dolist (p pairs result)
        (push (cons (car p) (cadr p)) result)))))

;; insert :: k -> v -> Map k v -> Map k v
(defvar hydra_overlay_emacs_lisp_lib_maps_insert
  (lambda (k)
    "Insert a key-value pair into a map."
    (lambda (v)
      (lambda (m)
        (alist-insert k v m)))))

;; keys :: Map k v -> [k]
(defvar hydra_overlay_emacs_lisp_lib_maps_keys
  (lambda (m)
    "Get the keys of a map, in sorted order."
    (mapcar #'car (hydra-map-sorted-pairs m))))

;; lookup :: k -> Map k v -> Maybe v
(defvar hydra_overlay_emacs_lisp_lib_maps_lookup
  (lambda (k)
    "Lookup a value in a map."
    (lambda (m)
      (let ((entry (hydra-map-lookup k m)))
        (if entry
            (list :given (cdr entry))
            (list :none))))))

;; map :: (v1 -> v2) -> Map k v1 -> Map k v2
(defvar hydra_overlay_emacs_lisp_lib_maps_map
  (lambda (f)
    "Map a function over a map."
    (lambda (m)
      (let (result)
        (hydra-map-foreach
         (lambda (k v) (push (cons k (funcall f v)) result))
         m)
        result))))

;; map_keys :: (k1 -> k2) -> Map k1 v -> Map k2 v
(defvar hydra_overlay_emacs_lisp_lib_maps_map_keys
  (lambda (f)
    "Map a function over the keys of a map."
    (lambda (m)
      (let (result)
        (hydra-map-foreach
         (lambda (k v) (push (cons (funcall f k) v) result))
         m)
        result))))

;; member :: k -> Map k v -> Bool
(defvar hydra_overlay_emacs_lisp_lib_maps_member
  (lambda (k)
    "Check if a key is present in a map."
    (lambda (m)
      (if (hydra-map-lookup k m) t nil))))

;; null :: Map k v -> Bool
(defvar hydra_overlay_emacs_lisp_lib_maps_null
  (lambda (m)
    "Check if a map is empty."
    (hydra-map-empty-p m)))

;; singleton :: k -> v -> Map k v
(defvar hydra_overlay_emacs_lisp_lib_maps_singleton
  (lambda (k)
    "Create a map with a single key-value pair."
    (lambda (v)
      (list (cons k v)))))

;; size :: Map k v -> Int
(defvar hydra_overlay_emacs_lisp_lib_maps_size
  (lambda (m)
    "Get the size of a map."
    (hydra-map-size m)))

;; to_list :: Map k v -> [Pair k v]
(defvar hydra_overlay_emacs_lisp_lib_maps_to_list
  (lambda (m)
    "Convert a map to a list of key-value pairs, in sorted-by-key order."
    (mapcar (lambda (entry) (list (car entry) (cdr entry)))
            (hydra-map-sorted-pairs m))))

;; union :: Map k v -> Map k v -> Map k v
;; Left-biased: entries from first map take precedence
(defvar hydra_overlay_emacs_lisp_lib_maps_union
  (lambda (m1)
    "Union two maps, with the first taking precedence."
    (lambda (m2)
      ;; m1's entries are prepended (most recent), so they shadow m2's
      ;; entries of the same key in the deduplicated view -- O(size m1),
      ;; no copy of m2.
      (append m1 m2))))

(provide 'hydra.lib.maps)
