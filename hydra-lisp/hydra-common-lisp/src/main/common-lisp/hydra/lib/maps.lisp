(in-package :cl-user)

;; Maps are alists for compatibility with generated code that iterates maps as lists.
;; Uses generic-compare for deterministic key ordering in toList.

(defun alist-lookup (key m)
  "Look up a key in an alist. Returns (key . val) or nil."
  (assoc key m :test #'equal))

(defun make-hydra-map (&optional pairs)
  "Create an alist from a list of (key . val) pairs."
  pairs)

;; alter :: (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
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

(defvar hydra_lib_maps_alter
  (lambda (f)
    (lambda (k)
      (lambda (m)
        (let* ((old-entry (assoc k m :test #'equal))
               (old-maybe (if old-entry (list :just (cdr old-entry)) (list :nothing)))
               (new-maybe (funcall f old-maybe))
               (new-m (remove k m :key #'car :test #'equal)))
          (if (alter-is-nothing-p new-maybe)
              new-m
              (cons (cons k (alter-get-value new-maybe)) new-m)))))))

;; bimap :: (k1 -> k2) -> (v1 -> v2) -> Map k1 v1 -> Map k2 v2
(defvar hydra_lib_maps_bimap
  (lambda (fk)
    (lambda (fv)
      (lambda (m)
        (mapcar (lambda (pair) (cons (funcall fk (car pair)) (funcall fv (cdr pair)))) m)))))

;; delete :: k -> Map k v -> Map k v
(defvar hydra_lib_maps_delete
  (lambda (k)
    (lambda (m)
      (remove k m :key #'car :test #'equal))))

;; elems :: Map k v -> [v]
(defvar hydra_lib_maps_elems
  (lambda (m)
    (mapcar #'cdr m)))

;; empty :: Map k v
(defvar hydra_lib_maps_empty nil)

;; filter :: (v -> Bool) -> Map k v -> Map k v
(defvar hydra_lib_maps_filter
  (lambda (pred)
    (lambda (m)
      (remove-if-not (lambda (pair) (funcall pred (cdr pair))) m))))

;; filter_with_key :: (k -> v -> Bool) -> Map k v -> Map k v
(defvar hydra_lib_maps_filter_with_key
  (lambda (pred)
    (lambda (m)
      (remove-if-not (lambda (pair) (funcall (funcall pred (car pair)) (cdr pair))) m))))

;; find_with_default :: v -> k -> Map k v -> v
(defvar hydra_lib_maps_find_with_default
  (lambda (def)
    (lambda (k)
      (lambda (m)
        (let ((entry (assoc k m :test #'equal)))
          (if entry (cdr entry) def))))))

;; from_list :: [Pair k v] -> Map k v
;; Pairs are (key val) two-element lists; convert to alist (key . val)
;; Later entries override earlier ones for duplicate keys (last wins)
(defvar hydra_lib_maps_from_list
  (lambda (pairs)
    (let ((result nil))
      (dolist (p (reverse pairs) result)
        (unless (assoc (first p) result :test #'equal)
          (push (cons (first p) (second p)) result))))))

;; insert :: k -> v -> Map k v -> Map k v
(defvar hydra_lib_maps_insert
  (lambda (k)
    (lambda (v)
      (lambda (m)
        (cons (cons k v) (remove k m :key #'car :test #'equal))))))

;; keys :: Map k v -> [k]
(defvar hydra_lib_maps_keys
  (lambda (m) (mapcar #'car m)))

;; lookup :: k -> Map k v -> Maybe v
(defvar hydra_lib_maps_lookup
  (lambda (k)
    (lambda (m)
      (let ((entry (assoc k m :test #'equal)))
        (if entry
            (list :just (cdr entry))
            (list :nothing))))))

;; map :: (v1 -> v2) -> Map k v1 -> Map k v2
(defvar hydra_lib_maps_map
  (lambda (f)
    (lambda (m)
      (mapcar (lambda (pair) (cons (car pair) (funcall f (cdr pair)))) m))))

;; map_keys :: (k1 -> k2) -> Map k1 v -> Map k2 v
(defvar hydra_lib_maps_map_keys
  (lambda (f)
    (lambda (m)
      (mapcar (lambda (pair) (cons (funcall f (car pair)) (cdr pair))) m))))

;; member :: k -> Map k v -> Bool
(defvar hydra_lib_maps_member
  (lambda (k)
    (lambda (m)
      (if (assoc k m :test #'equal) t nil))))

;; null :: Map k v -> Bool
(defvar hydra_lib_maps_null
  (lambda (m) (null m)))

;; singleton :: k -> v -> Map k v
(defvar hydra_lib_maps_singleton
  (lambda (k)
    (lambda (v)
      (list (cons k v)))))

;; size :: Map k v -> Int
(defvar hydra_lib_maps_size
  (lambda (m) (length m)))

;; to_list :: Map k v -> [Pair k v]
;; Sort by key for deterministic output
(defvar hydra_lib_maps_to_list
  (lambda (m)
    (let ((pairs (mapcar (lambda (pair) (list (car pair) (cdr pair))) m)))
      (sort pairs (lambda (a b) (< (generic-compare (first a) (first b)) 0))))))

;; union :: Map k v -> Map k v -> Map k v (left-biased)
(defvar hydra_lib_maps_union
  (lambda (m1)
    (lambda (m2)
      (let ((result (copy-list m2)))
        (dolist (pair m1 result)
          (let ((existing (assoc (car pair) result :test #'equal)))
            (if existing
                (setf (cdr existing) (cdr pair))
                (push pair result))))))))
