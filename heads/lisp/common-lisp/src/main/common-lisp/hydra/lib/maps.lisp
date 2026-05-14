(in-package :cl-user)

;; Maps are persistent ordered red-black trees with structural sharing,
;; ported from heads/java/.../PersistentMap.java (Chris Okasaki's purely
;; functional RB trees). All mutating operations (insert/delete/union/etc.)
;; are O(log n) and return a new map that shares most of its internal
;; structure with the original.
;;
;; Representation:
;;   - The empty map is `nil` (also accepted as alias for empty).
;;   - A node is a 7-element list:
;;       (:rbnode COLOR LEFT KEY VALUE RIGHT SIZE)
;;     where COLOR is :red or :black, LEFT and RIGHT are subtrees (nodes
;;     or nil), and SIZE is the precomputed total count.
;;
;; Key comparison goes through `generic-compare` (defined in equality.lisp),
;; which returns -1, 0, or 1. This supports the heterogeneous keys Hydra
;; uses (strings, alist-shaped Name records, etc.) — no Comparable
;; type-class needed.
;;
;; Iteration helpers (to_list, keys, elems) return entries in key-sorted
;; order, matching Haskell Data.Map semantics.
;;
;; Back-compat: a few read-only operations (lookup, member,
;; find_with_default, size, to_list) also accept legacy alist values, so
;; that callers built before this representation existed (e.g. raw alists
;; passed through hand-written kernel-loading helpers) keep working. The
;; alist path is O(n) per op; new callers should use the lib facade.

;; --- Node representation ---
;;
;; Nodes are SBCL defstructs (allocated as compact records with O(1)
;; inline-compiled accessors). The earlier list-based representation
;; (`(:rbnode color left key value right size)`) used `(nth N list)`
;; for each field access, which walks N cons cells per get — about 5
;; per right-child access. Switching to defstruct eliminated that
;; pointer-chasing in the hot inference loops.

(defstruct (rbnode
            (:constructor rb-make (color left key value right size))
            (:predicate rbnode-p)
            (:copier nil))
  (color  :black :type symbol)
  (left   nil)
  (key    nil)
  (value  nil)
  (right  nil)
  (size   0     :type fixnum))

(declaim (inline rb-size))
(defun rb-size (n)
  (if (rbnode-p n) (rbnode-size n) 0))

(declaim (inline maps-alist-p))
(defun maps-alist-p (m)
  "True if m is a non-empty alist (legacy representation)."
  (and (consp m) (consp (first m))))

;; --- Fast comparison ---
;;
;; The hot path in inference walks RB-trees keyed by Name records
;; (`((:value . "string"))` alists). `rb-cmp` short-circuits the
;; dominant case so SBCL can compile it to a tight string comparison
;; sequence — the structural-sharing win of an RB-tree depends on the
;; per-compare cost being small. The Name fast-path is checked FIRST
;; because Names are by far the most common map-key shape in kernel
;; inference (binding names, type variable names, etc).
;;
;; To keep this monomorphic for SBCL's native compiler, we avoid
;; `generic-compare`'s symbolic dispatch on the hot path.

(declaim (inline name-value-string))
(defun name-value-string (x)
  "If x is a Name-shaped alist `((:value . \"...\"))`, return the inner
   string. Otherwise nil. Inlined into rb-cmp."
  (when (and (consp x) (consp (car x))
             (eq (caar x) :value))
    (let ((s (cdar x)))
      (when (stringp s) s))))

(declaim (inline rb-cmp))
(defun rb-cmp (a b)
  (cond
    ((eq a b) 0)
    ;; Name-record fast-path: most map keys in inference are Names.
    ;; Compare the inner string directly — a single C-level string<.
    (t
     (let ((sa (name-value-string a))
           (sb (name-value-string b)))
       (cond
         ((and sa sb)
          (cond ((string< sa sb) -1) ((string= sa sb) 0) (t 1)))
         ;; Both bare strings: also common.
         ((and (stringp a) (stringp b))
          (cond ((string< a b) -1) ((string= a b) 0) (t 1)))
         ;; Equal fast-path: walks structures C-fast.
         ((equal a b) 0)
         ;; Fallback: generic-compare for complex / heterogeneous keys.
         (t (generic-compare a b)))))))

;; --- Balance (Okasaki) ---

(defun rb-balance (color left key value right)
  (let ((new-size (+ (rb-size left) 1 (rb-size right))))
    (when (eq color :black)
      (when (rbnode-p left)
        (let ((l-color (rbnode-color left)))
          (when (eq l-color :red)
            ;; Case 1: left-left red
            (let ((ll (rbnode-left left)))
              (when (and (rbnode-p ll) (eq (rbnode-color ll) :red))
                (return-from rb-balance
                  (rb-make :red
                           (rb-make :black (rbnode-left ll) (rbnode-key ll) (rbnode-value ll) (rbnode-right ll)
                                    (+ (rb-size (rbnode-left ll)) 1 (rb-size (rbnode-right ll))))
                           (rbnode-key left) (rbnode-value left)
                           (rb-make :black (rbnode-right left) key value right
                                    (+ (rb-size (rbnode-right left)) 1 (rb-size right)))
                           new-size))))
            ;; Case 2: left-right red
            (let ((lr (rbnode-right left)))
              (when (and (rbnode-p lr) (eq (rbnode-color lr) :red))
                (return-from rb-balance
                  (rb-make :red
                           (rb-make :black (rbnode-left left) (rbnode-key left) (rbnode-value left) (rbnode-left lr)
                                    (+ (rb-size (rbnode-left left)) 1 (rb-size (rbnode-left lr))))
                           (rbnode-key lr) (rbnode-value lr)
                           (rb-make :black (rbnode-right lr) key value right
                                    (+ (rb-size (rbnode-right lr)) 1 (rb-size right)))
                           new-size))))))) ; end left-red cases
      (when (rbnode-p right)
        (let ((r-color (rbnode-color right)))
          (when (eq r-color :red)
            ;; Case 3: right-left red
            (let ((rl (rbnode-left right)))
              (when (and (rbnode-p rl) (eq (rbnode-color rl) :red))
                (return-from rb-balance
                  (rb-make :red
                           (rb-make :black left key value (rbnode-left rl)
                                    (+ (rb-size left) 1 (rb-size (rbnode-left rl))))
                           (rbnode-key rl) (rbnode-value rl)
                           (rb-make :black (rbnode-right rl) (rbnode-key right) (rbnode-value right) (rbnode-right right)
                                    (+ (rb-size (rbnode-right rl)) 1 (rb-size (rbnode-right right))))
                           new-size))))
            ;; Case 4: right-right red
            (let ((rr (rbnode-right right)))
              (when (and (rbnode-p rr) (eq (rbnode-color rr) :red))
                (return-from rb-balance
                  (rb-make :red
                           (rb-make :black left key value (rbnode-left right)
                                    (+ (rb-size left) 1 (rb-size (rbnode-left right))))
                           (rbnode-key right) (rbnode-value right)
                           (rb-make :black (rbnode-left rr) (rbnode-key rr) (rbnode-value rr) (rbnode-right rr)
                                    (+ (rb-size (rbnode-left rr)) 1 (rb-size (rbnode-right rr))))
                           new-size))))))))
    ;; Fallthrough: no rebalancing
    (rb-make color left key value right new-size)))

;; --- Insert ---

(defun rb-ins (tree key value)
  (cond
    ((not (rbnode-p tree))
     (rb-make :red nil key value nil 1))
    (t
     (let ((cmp (rb-cmp key (rbnode-key tree))))
       (cond
         ((< cmp 0)
          (rb-balance (rbnode-color tree)
                      (rb-ins (rbnode-left tree) key value)
                      (rbnode-key tree) (rbnode-value tree)
                      (rbnode-right tree)))
         ((> cmp 0)
          (rb-balance (rbnode-color tree)
                      (rbnode-left tree)
                      (rbnode-key tree) (rbnode-value tree)
                      (rb-ins (rbnode-right tree) key value)))
         (t
          ;; Key exists; replace value (no rebalance needed)
          (if (or (eq value (rbnode-value tree))
                  (equal value (rbnode-value tree)))
              tree
              (rb-make (rbnode-color tree)
                       (rbnode-left tree) key value (rbnode-right tree)
                       (rbnode-size tree)))))))))

(defun rb-insert (tree key value)
  "O(log n) insert. Root color forced to :black."
  (let ((result (rb-ins tree key value)))
    (if (and (rbnode-p result) (eq (rbnode-color result) :red))
        (rb-make :black (rbnode-left result) (rbnode-key result) (rbnode-value result)
                 (rbnode-right result) (rbnode-size result))
        result)))

;; --- Delete (Okasaki: simple merge variant) ---

(defun rb-find-min (node)
  (loop while (rbnode-p (rbnode-left node))
        do (setf node (rbnode-left node)))
  node)

(defun rb-delete-min (tree)
  (cond
    ((not (rbnode-p tree)) tree)
    ((not (rbnode-p (rbnode-left tree))) (rbnode-right tree))
    (t (rb-balance (rbnode-color tree)
                   (rb-delete-min (rbnode-left tree))
                   (rbnode-key tree) (rbnode-value tree)
                   (rbnode-right tree)))))

(defun rb-merge (left right)
  (cond
    ((not (rbnode-p left)) right)
    ((not (rbnode-p right)) left)
    (t (let* ((min-right (rb-find-min right))
              (new-right (rb-delete-min right)))
         (rb-balance :black left (rbnode-key min-right) (rbnode-value min-right) new-right)))))

(defun rb-del (tree key)
  (cond
    ((not (rbnode-p tree)) tree)
    (t
     (let ((cmp (rb-cmp key (rbnode-key tree))))
       (cond
         ((< cmp 0)
          (let ((new-left (rb-del (rbnode-left tree) key)))
            (if (eq new-left (rbnode-left tree))
                tree
                (rb-balance (rbnode-color tree)
                            new-left (rbnode-key tree) (rbnode-value tree) (rbnode-right tree)))))
         ((> cmp 0)
          (let ((new-right (rb-del (rbnode-right tree) key)))
            (if (eq new-right (rbnode-right tree))
                tree
                (rb-balance (rbnode-color tree)
                            (rbnode-left tree) (rbnode-key tree) (rbnode-value tree) new-right))))
         (t
          (rb-merge (rbnode-left tree) (rbnode-right tree))))))))

(defun rb-delete (tree key)
  (let ((result (rb-del tree key)))
    (if (and (rbnode-p result) (eq (rbnode-color result) :red))
        (rb-make :black (rbnode-left result) (rbnode-key result) (rbnode-value result)
                 (rbnode-right result) (rbnode-size result))
        result)))

;; --- Lookup ---

(defun rb-lookup (tree key)
  "Returns (values val found-p)."
  (loop
    (cond
      ((not (rbnode-p tree)) (return (values nil nil)))
      (t (let ((cmp (rb-cmp key (rbnode-key tree))))
           (cond
             ((< cmp 0) (setf tree (rbnode-left tree)))
             ((> cmp 0) (setf tree (rbnode-right tree)))
             (t (return (values (rbnode-value tree) t)))))))))

;; --- In-order traversal (returns sorted list of (k . v) cons cells) ---

(defun rb-entries (tree)
  (let ((acc nil))
    (labels ((walk (n)
               (when (rbnode-p n)
                 (walk (rbnode-right n))
                 (push (cons (rbnode-key n) (rbnode-value n)) acc)
                 (walk (rbnode-left n)))))
      (walk tree)
      acc)))

(declaim (inline rb-for-each))
(defun rb-for-each (tree fn)
  "Call fn on each (key, value) of tree in in-order. fn receives 2 args."
  (labels ((walk (n)
             (when (rbnode-p n)
               (walk (rbnode-left n))
               (funcall fn (rbnode-key n) (rbnode-value n))
               (walk (rbnode-right n)))))
    (walk tree)))

;; --- Coercion + back-compat for alist values ---

(defun maps-lookup-raw (k m)
  "Internal: return (values val found-p) for key k in map m.
   Accepts rbnode trees, legacy alists, or nil."
  (cond
    ((null m) (values nil nil))
    ((rbnode-p m) (rb-lookup m k))
    ((maps-alist-p m)
     (let ((entry (assoc k m :test #'equal)))
       (if entry (values (cdr entry) t) (values nil nil))))
    (t (values nil nil))))

(defun maps-entries-sorted (m)
  "Return entries as (k . v) cons cells in key-sorted order.
   Accepts rbnode tree, alist, or nil."
  (cond
    ((null m) nil)
    ((rbnode-p m) (rb-entries m))
    ((maps-alist-p m)
     (sort (copy-alist m)
           (lambda (a b) (< (generic-compare (car a) (car b)) 0))))
    (t nil)))

(defun maps-from-pairs (pairs)
  "Build a map from a list of (key . val) cons cells. Later entries win."
  (let ((result nil))
    (dolist (p pairs result)
      (setf result (rb-insert result (car p) (cdr p))))))

;; --- Legacy helpers retained for non-lib callers ---

(defun alist-lookup (key m)
  "Look up a key in an alist. Returns (key . val) or nil. Legacy helper."
  (assoc key m :test #'equal))

(defun make-hydra-map (&optional pairs)
  "Create a persistent map from a list of (key . val) cons cells."
  (maps-from-pairs pairs))

;; --- alter helpers (decode Maybe values, unrelated to map representation) ---

(defun alter-is-nothing-p (m)
  (or (null m)
      (and (consp m) (eq (first m) :nothing))
      (and (consp m) (eq (first m) :maybe)
           (or (null (cdr m)) (null (second m))))))

(defun alter-get-value (m)
  (cond
    ((and (consp m) (eq (first m) :just)) (second m))
    ((and (consp m) (eq (first m) :maybe))
     (let ((body (second m)))
       (if (and (consp body) (eq (first body) :just))
           (second body)
         body)))
    (t m)))

;; ============================================================================
;; Public API: hydra_lib_maps_*
;; ============================================================================

;; alter :: (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
(defvar hydra_lib_maps_alter
  (lambda (f)
    (lambda (k)
      (lambda (m)
        (multiple-value-bind (v found) (maps-lookup-raw k m)
          (let* ((old-maybe (if found (list :just v) (list :nothing)))
                 (new-maybe (funcall f old-maybe)))
            (cond
              ((alter-is-nothing-p new-maybe)
               (cond
                 ((rbnode-p m) (rb-delete m k))
                 ((maps-alist-p m) (remove k m :key #'car :test #'equal))
                 (t m)))
              (t
               (let ((new-val (alter-get-value new-maybe)))
                 (cond
                   ((or (rbnode-p m) (null m))
                    (rb-insert m k new-val))
                   ((maps-alist-p m)
                    ;; Convert legacy alist input to tree on first mutation.
                    (rb-insert (maps-from-pairs m) k new-val))
                   (t (rb-insert nil k new-val))))))))))))

;; bimap :: (k1 -> k2) -> (v1 -> v2) -> Map k1 v1 -> Map k2 v2
(defvar hydra_lib_maps_bimap
  (lambda (fk)
    (lambda (fv)
      (lambda (m)
        (let ((result nil))
          (cond
            ((rbnode-p m)
             (rb-for-each m (lambda (k v)
                              (setf result (rb-insert result (funcall fk k) (funcall fv v))))))
            ((maps-alist-p m)
             (dolist (pair m)
               (setf result (rb-insert result (funcall fk (car pair)) (funcall fv (cdr pair)))))))
          result)))))

;; delete :: k -> Map k v -> Map k v
(defvar hydra_lib_maps_delete
  (lambda (k)
    (lambda (m)
      (cond
        ((null m) nil)
        ((rbnode-p m) (rb-delete m k))
        ((maps-alist-p m) (remove k m :key #'car :test #'equal))
        (t m)))))

;; elems :: Map k v -> [v]
;; Values in key-sorted order, matching Haskell Data.Map.elems.
(defvar hydra_lib_maps_elems
  (lambda (m)
    (mapcar #'cdr (maps-entries-sorted m))))

;; empty :: Map k v
(defvar hydra_lib_maps_empty nil)

;; filter :: (v -> Bool) -> Map k v -> Map k v
(defvar hydra_lib_maps_filter
  (lambda (pred)
    (lambda (m)
      (let ((result nil))
        (cond
          ((rbnode-p m)
           (rb-for-each m (lambda (k v)
                            (when (funcall pred v)
                              (setf result (rb-insert result k v))))))
          ((maps-alist-p m)
           (dolist (pair m)
             (when (funcall pred (cdr pair))
               (setf result (rb-insert result (car pair) (cdr pair)))))))
        result))))

;; filter_with_key :: (k -> v -> Bool) -> Map k v -> Map k v
(defvar hydra_lib_maps_filter_with_key
  (lambda (pred)
    (lambda (m)
      (let ((result nil))
        (cond
          ((rbnode-p m)
           (rb-for-each m (lambda (k v)
                            (when (funcall (funcall pred k) v)
                              (setf result (rb-insert result k v))))))
          ((maps-alist-p m)
           (dolist (pair m)
             (when (funcall (funcall pred (car pair)) (cdr pair))
               (setf result (rb-insert result (car pair) (cdr pair)))))))
        result))))

;; find_with_default :: v -> k -> Map k v -> v
(defvar hydra_lib_maps_find_with_default
  (lambda (def)
    (lambda (k)
      (lambda (m)
        (multiple-value-bind (v found) (maps-lookup-raw k m)
          (if found v def))))))

;; from_list :: [Pair k v] -> Map k v
;; Pairs are two-element lists (key val).
;; Later entries override earlier ones for duplicate keys (last wins).
(defvar hydra_lib_maps_from_list
  (lambda (pairs)
    (let ((result nil))
      (dolist (p pairs result)
        (setf result (rb-insert result (first p) (second p)))))))

;; insert :: k -> v -> Map k v -> Map k v
(defvar hydra_lib_maps_insert
  (lambda (k)
    (lambda (v)
      (lambda (m)
        (cond
          ((or (rbnode-p m) (null m)) (rb-insert m k v))
          ((maps-alist-p m) (rb-insert (maps-from-pairs m) k v))
          (t (rb-insert nil k v)))))))

;; keys :: Map k v -> [k]
;; Sorted via generic-compare.
(defvar hydra_lib_maps_keys
  (lambda (m)
    (mapcar #'car (maps-entries-sorted m))))

;; lookup :: k -> Map k v -> Maybe v
(defvar hydra_lib_maps_lookup
  (lambda (k)
    (lambda (m)
      (multiple-value-bind (v found) (maps-lookup-raw k m)
        (if found (list :just v) (list :nothing))))))

;; map :: (v1 -> v2) -> Map k v1 -> Map k v2
(defvar hydra_lib_maps_map
  (lambda (f)
    (lambda (m)
      (let ((result nil))
        (cond
          ((rbnode-p m)
           (rb-for-each m (lambda (k v)
                            (setf result (rb-insert result k (funcall f v))))))
          ((maps-alist-p m)
           (dolist (pair m)
             (setf result (rb-insert result (car pair) (funcall f (cdr pair)))))))
        result))))

;; map_keys :: (k1 -> k2) -> Map k1 v -> Map k2 v
(defvar hydra_lib_maps_map_keys
  (lambda (f)
    (lambda (m)
      (let ((result nil))
        (cond
          ((rbnode-p m)
           (rb-for-each m (lambda (k v)
                            (setf result (rb-insert result (funcall f k) v)))))
          ((maps-alist-p m)
           (dolist (pair m)
             (setf result (rb-insert result (funcall f (car pair)) (cdr pair))))))
        result))))

;; member :: k -> Map k v -> Bool
(defvar hydra_lib_maps_member
  (lambda (k)
    (lambda (m)
      (nth-value 1 (maps-lookup-raw k m)))))

;; null :: Map k v -> Bool
(defvar hydra_lib_maps_null
  (lambda (m)
    (cond
      ((null m) t)
      ((rbnode-p m) (zerop (rbnode-size m)))
      ((maps-alist-p m) nil)
      (t t))))

;; singleton :: k -> v -> Map k v
(defvar hydra_lib_maps_singleton
  (lambda (k)
    (lambda (v)
      (rb-insert nil k v))))

;; size :: Map k v -> Int
(defvar hydra_lib_maps_size
  (lambda (m)
    (cond
      ((null m) 0)
      ((rbnode-p m) (rbnode-size m))
      ((maps-alist-p m) (length m))
      (t 0))))

;; to_list :: Map k v -> [Pair k v]
;; Sorted by key for deterministic output. Returns [(k v) ...].
(defvar hydra_lib_maps_to_list
  (lambda (m)
    (mapcar (lambda (pair) (list (car pair) (cdr pair)))
            (maps-entries-sorted m))))

;; union :: Map k v -> Map k v -> Map k v (left-biased)
;; m1's values win on key collision.
;; Implementation: iterate over the smaller map and insert into the larger.
(defvar hydra_lib_maps_union
  (lambda (m1)
    (lambda (m2)
      (cond
        ((null m1) m2)
        ((null m2) m1)
        (t
         ;; Iterate m1 (preferred) into a copy of m2; m1's values win
         ;; because rb-insert always replaces.
         (let ((result (cond
                         ((rbnode-p m2) m2)
                         ((maps-alist-p m2) (maps-from-pairs m2))
                         (t nil))))
           (cond
             ((rbnode-p m1)
              (rb-for-each m1 (lambda (k v) (setf result (rb-insert result k v)))))
             ((maps-alist-p m1)
              (dolist (pair m1)
                (setf result (rb-insert result (car pair) (cdr pair))))))
           result))))))
