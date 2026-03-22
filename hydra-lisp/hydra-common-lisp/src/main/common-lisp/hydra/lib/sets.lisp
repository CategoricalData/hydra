(in-package :cl-user)

;; Sets are sorted lists for compatibility with generated code that iterates sets as lists.
;; Uses generic-compare for deterministic ordering.

(defun set-sorted-insert (x s)
  "Insert x into sorted list s (no duplicates)."
  (cond
    ((null s) (list x))
    (t (let ((c (generic-compare x (car s))))
         (cond
           ((= c 0) s)  ;; already present
           ((< c 0) (cons x s))
           (t (cons (car s) (set-sorted-insert x (cdr s)))))))))

(defun set-from-list (xs)
  "Build a sorted set (list) from xs."
  (let ((result nil))
    (dolist (x xs result)
      (setq result (set-sorted-insert x result)))))

(defun set-member-p (x s)
  (member x s :test #'equal))

;; delete :: a -> Set a -> Set a
(defvar hydra_lib_sets_delete
  (lambda (x)
    (lambda (s)
      (remove x s :test #'equal))))

;; difference :: Set a -> Set a -> Set a
(defvar hydra_lib_sets_difference
  (lambda (s1)
    (lambda (s2)
      (remove-if (lambda (x) (member x s2 :test #'equal)) s1))))

;; empty :: Set a
(defvar hydra_lib_sets_empty nil)

;; from_list :: [a] -> Set a
(defvar hydra_lib_sets_from_list
  (lambda (xs) (set-from-list xs)))

;; insert :: a -> Set a -> Set a
(defvar hydra_lib_sets_insert
  (lambda (x)
    (lambda (s)
      (set-sorted-insert x s))))

;; intersection :: Set a -> Set a -> Set a
(defvar hydra_lib_sets_intersection
  (lambda (s1)
    (lambda (s2)
      (remove-if-not (lambda (x) (member x s2 :test #'equal)) s1))))

;; map :: (a -> b) -> Set a -> Set b
(defvar hydra_lib_sets_map
  (lambda (f)
    (lambda (s)
      (set-from-list (mapcar f s)))))

;; member :: a -> Set a -> Bool
(defvar hydra_lib_sets_member
  (lambda (x)
    (lambda (s)
      (if (member x s :test #'equal) t nil))))

;; null :: Set a -> Bool
(defvar hydra_lib_sets_null
  (lambda (s) (null s)))

;; size :: Set a -> Int
(defvar hydra_lib_sets_size
  (lambda (s) (length s)))

;; singleton :: a -> Set a
(defvar hydra_lib_sets_singleton
  (lambda (x) (list x)))

;; to_list :: Set a -> [a]
(defvar hydra_lib_sets_to_list
  (lambda (s)
    (sort (copy-list s) (lambda (a b) (< (generic-compare a b) 0)))))

;; union :: Set a -> Set a -> Set a
(defvar hydra_lib_sets_union
  (lambda (s1)
    (lambda (s2)
      (let ((result (copy-list s2)))
        (dolist (x s1 result)
          (setq result (set-sorted-insert x result)))))))

;; unions :: [Set a] -> Set a
(defvar hydra_lib_sets_unions
  (lambda (sets)
    (let ((result nil))
      (dolist (s sets result)
        (dolist (x s)
          (setq result (set-sorted-insert x result)))))))
