(in-package :cl-user)

;; Maps are association lists sorted by key: ((k1 . v1) (k2 . v2) ...)

(defun alist-lookup (key alist)
  (assoc key alist :test #'equal))

(defun alist-insert (key val alist)
  "Insert maintaining sorted order by key."
  (cond
    ((null alist) (list (cons key val)))
    ((= (generic-compare key (caar alist)) 0)
     (cons (cons key val) (cdr alist)))
    ((< (generic-compare key (caar alist)) 0)
     (cons (cons key val) alist))
    (t (cons (car alist) (alist-insert key val (cdr alist))))))

(defun alist-delete (key alist)
  (remove key alist :test #'equal :key #'car))

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
        (let* ((existing (alist-lookup k m))
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
    (lambda (fv)
      (lambda (m)
        (let ((pairs (mapcar (lambda (entry)
                               (cons (funcall fk (car entry))
                                     (funcall fv (cdr entry))))
                             m)))
          ;; Re-sort by new keys
          (let ((result nil))
            (dolist (p pairs result)
              (setf result (alist-insert (car p) (cdr p) result)))))))))

;; delete :: k -> Map k v -> Map k v
(defvar hydra_lib_maps_delete
  (lambda (k)
    (lambda (m)
      (alist-delete k m))))

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
      (remove-if-not (lambda (entry) (funcall pred (cdr entry))) m))))

;; filter_with_key :: (k -> v -> Bool) -> Map k v -> Map k v
(defvar hydra_lib_maps_filter_with_key
  (lambda (pred)
    (lambda (m)
      (remove-if-not (lambda (entry)
                       (funcall (funcall pred (car entry)) (cdr entry)))
                     m))))

;; find_with_default :: v -> k -> Map k v -> v
(defvar hydra_lib_maps_find_with_default
  (lambda (def)
    (lambda (k)
      (lambda (m)
        (let ((entry (alist-lookup k m)))
          (if entry (cdr entry) def))))))

;; from_list :: [Pair k v] -> Map k v
;; Input is list of (list k v) pairs
(defvar hydra_lib_maps_from_list
  (lambda (pairs)
    (let ((acc nil))
      (dolist (p pairs acc)
        (setf acc (alist-insert (first p) (second p) acc))))))

;; insert :: k -> v -> Map k v -> Map k v
(defvar hydra_lib_maps_insert
  (lambda (k)
    (lambda (v)
      (lambda (m)
        (alist-insert k v m)))))

;; keys :: Map k v -> [k]
(defvar hydra_lib_maps_keys
  (lambda (m)
    (mapcar #'car m)))

;; lookup :: k -> Map k v -> Maybe v
(defvar hydra_lib_maps_lookup
  (lambda (k)
    (lambda (m)
      (let ((entry (alist-lookup k m)))
        (if entry
            (list :just (cdr entry))
            (list :nothing))))))

;; map :: (v1 -> v2) -> Map k v1 -> Map k v2
(defvar hydra_lib_maps_map
  (lambda (f)
    (lambda (m)
      (mapcar (lambda (entry) (cons (car entry) (funcall f (cdr entry)))) m))))

;; map_keys :: (k1 -> k2) -> Map k1 v -> Map k2 v
(defvar hydra_lib_maps_map_keys
  (lambda (f)
    (lambda (m)
      (let ((result nil))
        (dolist (entry m result)
          (setf result (alist-insert (funcall f (car entry)) (cdr entry) result)))))))

;; member :: k -> Map k v -> Bool
(defvar hydra_lib_maps_member
  (lambda (k)
    (lambda (m)
      (if (alist-lookup k m) t nil))))

;; null :: Map k v -> Bool
(defvar hydra_lib_maps_null
  (lambda (m)
    (null m)))

;; singleton :: k -> v -> Map k v
(defvar hydra_lib_maps_singleton
  (lambda (k)
    (lambda (v)
      (list (cons k v)))))

;; size :: Map k v -> Int
(defvar hydra_lib_maps_size
  (lambda (m)
    (length m)))

;; to_list :: Map k v -> [Pair k v]
(defvar hydra_lib_maps_to_list
  (lambda (m)
    (mapcar (lambda (entry) (list (car entry) (cdr entry))) m)))

;; union :: Map k v -> Map k v -> Map k v
;; Left-biased: entries from first map take precedence
(defvar hydra_lib_maps_union
  (lambda (m1)
    (lambda (m2)
      (let ((acc m1))
        (dolist (entry m2 acc)
          (unless (alist-lookup (car entry) acc)
            (setf acc (alist-insert (car entry) (cdr entry) acc))))))))
