(in-package :cl-user)

;; Sets are sorted lists (no duplicates), using generic-compare for ordering.

(defun set-insert (x s)
  (cond
    ((null s) (list x))
    ((= (generic-compare x (car s)) 0) s)
    ((< (generic-compare x (car s)) 0) (cons x s))
    (t (cons (car s) (set-insert x (cdr s))))))

(defun set-delete (x s)
  (cond
    ((null s) nil)
    ((equal x (car s)) (cdr s))
    (t (cons (car s) (set-delete x (cdr s))))))

(defun set-member-p (x s)
  (cond
    ((null s) nil)
    ((equal x (car s)) t)
    ((< (generic-compare x (car s)) 0) nil)
    (t (set-member-p x (cdr s)))))

(defun set-from-list (xs)
  (let ((acc nil))
    (dolist (x xs acc)
      (setf acc (set-insert x acc)))))

;; delete :: a -> Set a -> Set a
(defvar hydra_lib_sets_delete
  (lambda (x)
    (lambda (s)
      (set-delete x s))))

;; difference :: Set a -> Set a -> Set a
(defvar hydra_lib_sets_difference
  (lambda (s1)
    (lambda (s2)
      (loop for x in s1
            unless (set-member-p x s2)
            collect x))))

;; empty :: Set a
(defvar hydra_lib_sets_empty nil)

;; from_list :: [a] -> Set a
(defvar hydra_lib_sets_from_list
  (lambda (xs)
    (set-from-list xs)))

;; insert :: a -> Set a -> Set a
(defvar hydra_lib_sets_insert
  (lambda (x)
    (lambda (s)
      (set-insert x s))))

;; intersection :: Set a -> Set a -> Set a
(defvar hydra_lib_sets_intersection
  (lambda (s1)
    (lambda (s2)
      (loop for x in s1
            when (set-member-p x s2)
            collect x))))

;; map :: (a -> b) -> Set a -> Set b
(defvar hydra_lib_sets_map
  (lambda (f)
    (lambda (s)
      (let ((acc nil))
        (dolist (x s acc)
          (setf acc (set-insert (funcall f x) acc)))))))

;; member :: a -> Set a -> Bool
(defvar hydra_lib_sets_member
  (lambda (x)
    (lambda (s)
      (set-member-p x s))))

;; null :: Set a -> Bool
(defvar hydra_lib_sets_null
  (lambda (s)
    (null s)))

;; size :: Set a -> Int
(defvar hydra_lib_sets_size
  (lambda (s)
    (length s)))

;; singleton :: a -> Set a
(defvar hydra_lib_sets_singleton
  (lambda (x)
    (list x)))

;; to_list :: Set a -> [a]
(defvar hydra_lib_sets_to_list
  (lambda (s)
    s))

;; union :: Set a -> Set a -> Set a
(defvar hydra_lib_sets_union
  (lambda (s1)
    (lambda (s2)
      (let ((acc s1))
        (dolist (x s2 acc)
          (setf acc (set-insert x acc)))))))

;; unions :: [Set a] -> Set a
(defvar hydra_lib_sets_unions
  (lambda (sets)
    (let ((acc nil))
      (dolist (s sets acc)
        (setf acc (funcall (funcall hydra_lib_sets_union acc) s))))))
