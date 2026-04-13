;;; maps.el --- Hydra map primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Maps are association lists sorted by key: ((k1 . v1) (k2 . v2) ...)

(defun alist-lookup (key alist)
  (assoc key alist))

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
  (cl-remove-if (lambda (entry) (equal key (car entry))) alist))

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
    "Map a function over the keys and values of a map."
    (lambda (fv)
      (lambda (m)
        (let ((pairs (mapcar (lambda (entry)
                               (cons (funcall fk (car entry))
                                     (funcall fv (cdr entry))))
                             m)))
          ;; Re-sort by new keys
          (let ((result nil))
            (dolist (p pairs result)
              (setq result (alist-insert (car p) (cdr p) result)))))))))

;; delete :: k -> Map k v -> Map k v
(defvar hydra_lib_maps_delete
  (lambda (k)
    "Remove a key from a map."
    (lambda (m)
      (alist-delete k m))))

;; elems :: Map k v -> [v]
(defvar hydra_lib_maps_elems
  (lambda (m)
    "Get the values of a map."
    (mapcar #'cdr m)))

;; empty :: Map k v
(defvar hydra_lib_maps_empty nil
  "Create an empty map.")

;; filter :: (v -> Bool) -> Map k v -> Map k v
(defvar hydra_lib_maps_filter
  (lambda (pred)
    "Filter a map based on values."
    (lambda (m)
      (cl-remove-if-not (lambda (entry) (funcall pred (cdr entry))) m))))

;; filter_with_key :: (k -> v -> Bool) -> Map k v -> Map k v
(defvar hydra_lib_maps_filter_with_key
  (lambda (pred)
    "Filter a map based on key-value pairs."
    (lambda (m)
      (cl-remove-if-not (lambda (entry)
                           (funcall (funcall pred (car entry)) (cdr entry)))
                         m))))

;; find_with_default :: v -> k -> Map k v -> v
(defvar hydra_lib_maps_find_with_default
  (lambda (def)
    "Lookup a value with a default."
    (lambda (k)
      (lambda (m)
        (let ((entry (alist-lookup k m)))
          (if entry (cdr entry) def))))))

;; from_list :: [Pair k v] -> Map k v
;; Input is list of (list k v) pairs
(defvar hydra_lib_maps_from_list
  (lambda (pairs)
    "Create a map from a list of key-value pairs."
    (let ((acc nil))
      (dolist (p pairs acc)
        (setq acc (alist-insert (car p) (cadr p) acc))))))

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
    "Get the keys of a map."
    (mapcar #'car m)))

;; lookup :: k -> Map k v -> Maybe v
(defvar hydra_lib_maps_lookup
  (lambda (k)
    "Lookup a value in a map."
    (lambda (m)
      (let ((entry (alist-lookup k m)))
        (if entry
            (list :just (cdr entry))
            (list :nothing))))))

;; map :: (v1 -> v2) -> Map k v1 -> Map k v2
(defvar hydra_lib_maps_map
  (lambda (f)
    "Map a function over a map."
    (lambda (m)
      (mapcar (lambda (entry) (cons (car entry) (funcall f (cdr entry)))) m))))

;; map_keys :: (k1 -> k2) -> Map k1 v -> Map k2 v
(defvar hydra_lib_maps_map_keys
  (lambda (f)
    "Map a function over the keys of a map."
    (lambda (m)
      (let ((result nil))
        (dolist (entry m result)
          (setq result (alist-insert (funcall f (car entry)) (cdr entry) result)))))))

;; member :: k -> Map k v -> Bool
(defvar hydra_lib_maps_member
  (lambda (k)
    "Check if a key is present in a map."
    (lambda (m)
      (if (alist-lookup k m) t nil))))

;; null :: Map k v -> Bool
(defvar hydra_lib_maps_null
  (lambda (m)
    "Check if a map is empty."
    (null m)))

;; singleton :: k -> v -> Map k v
(defvar hydra_lib_maps_singleton
  (lambda (k)
    "Create a map with a single key-value pair."
    (lambda (v)
      (list (cons k v)))))

;; size :: Map k v -> Int
(defvar hydra_lib_maps_size
  (lambda (m)
    "Get the size of a map."
    (length m)))

;; to_list :: Map k v -> [Pair k v]
(defvar hydra_lib_maps_to_list
  (lambda (m)
    "Convert a map to a list of key-value pairs."
    (mapcar (lambda (entry) (list (car entry) (cdr entry))) m)))

;; union :: Map k v -> Map k v -> Map k v
;; Left-biased: entries from first map take precedence
(defvar hydra_lib_maps_union
  (lambda (m1)
    "Union two maps, with the first taking precedence."
    (lambda (m2)
      (let ((acc m1))
        (dolist (entry m2 acc)
          (unless (alist-lookup (car entry) acc)
            (setq acc (alist-insert (car entry) (cdr entry) acc))))))))

(provide 'hydra.lib.maps)
