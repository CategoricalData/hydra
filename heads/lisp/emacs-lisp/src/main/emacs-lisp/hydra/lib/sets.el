;;; sets.el --- Hydra set primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Sets are backed by Emacs `make-hash-table :test 'equal` with values
;; uniformly bound to `t`. All mutating operations (insert, delete,
;; union, difference, intersection, …) copy the table first, preserving
;; the immutable semantics Hydra expects.
;;
;; The empty set is represented as `nil` (back-compat with prior
;; sorted-list code that passed nil for empty). Any value-bearing set
;; is a hash-table. Iteration helpers (to_list) return entries in
;; key-sorted order via `generic-compare`.

(defun hydra-set-p (s)
  (hash-table-p s))

(defun hydra-set-empty-p (s)
  (or (null s) (and (hash-table-p s) (zerop (hash-table-count s)))))

(defun hydra-set-size (s)
  (cond
   ((null s) 0)
   ((hash-table-p s) (hash-table-count s))
   ((listp s) (length s))
   (t (error "hydra-set-size: not a set: %S" s))))

(defun hydra-set-member-p (x s)
  (cond
   ((null s) nil)
   ((hash-table-p s) (not (eq (gethash x s :hydra-not-found) :hydra-not-found)))
   ((listp s) (cl-some (lambda (y) (equal x y)) s))
   (t (error "hydra-set-member-p: not a set: %S" s))))

(defun hydra-set-to-hash (s)
  "Return a hash-table view of S. Always safe to mutate."
  (cond
   ((null s) (make-hash-table :test 'equal))
   ((hash-table-p s) (copy-hash-table s))
   ((listp s)
    (let ((h (make-hash-table :test 'equal :size (max 1 (length s)))))
      (dolist (x s h) (puthash x t h))))
   (t (error "hydra-set-to-hash: not a set: %S" s))))

(defun hydra-set-foreach (f s)
  (cond
   ((null s) nil)
   ((hash-table-p s) (maphash (lambda (k _v) (funcall f k)) s))
   ((listp s) (dolist (x s) (funcall f x)))
   (t (error "hydra-set-foreach: not a set: %S" s))))

(defun hydra-set-sorted-list (s)
  "Return the elements of S as a list, sorted via `generic-compare`."
  (let ((xs nil))
    (hydra-set-foreach (lambda (x) (push x xs)) s)
    (sort xs (lambda (a b) (< (generic-compare a b) 0)))))

(defun set-insert (x s)
  "Return S with X inserted, copy-on-write."
  (let ((h (hydra-set-to-hash s)))
    (puthash x t h)
    h))

(defun set-delete (x s)
  "Return S with X removed, copy-on-write."
  (cond
   ((null s) nil)
   ((hash-table-p s)
    (if (eq (gethash x s :hydra-not-found) :hydra-not-found)
        s
      (let ((h (copy-hash-table s)))
        (remhash x h)
        h)))
   ((listp s) (cl-remove-if (lambda (y) (equal x y)) s))
   (t (error "set-delete: not a set: %S" s))))

(defun set-member-p (x s)
  (hydra-set-member-p x s))

(defun set-from-list (xs)
  (let ((h (make-hash-table :test 'equal :size (max 1 (length xs)))))
    (dolist (x xs h) (puthash x t h))))

;; delete :: a -> Set a -> Set a
(defvar hydra_lib_sets_delete
  (lambda (x)
    "Delete an element from a set."
    (lambda (s)
      (set-delete x s))))

;; difference :: Set a -> Set a -> Set a
(defvar hydra_lib_sets_difference
  (lambda (s1)
    "Compute the difference of two sets."
    (lambda (s2)
      (let ((result (make-hash-table :test 'equal)))
        (hydra-set-foreach
         (lambda (x) (unless (hydra-set-member-p x s2) (puthash x t result)))
         s1)
        result))))

;; empty :: Set a
(defvar hydra_lib_sets_empty nil
  "Create an empty set.")

;; from_list :: [a] -> Set a
(defvar hydra_lib_sets_from_list
  (lambda (xs)
    "Create a set from a list."
    (set-from-list xs)))

;; insert :: a -> Set a -> Set a
(defvar hydra_lib_sets_insert
  (lambda (x)
    "Insert an element into a set."
    (lambda (s)
      (set-insert x s))))

;; intersection :: Set a -> Set a -> Set a
(defvar hydra_lib_sets_intersection
  (lambda (s1)
    "Compute the intersection of two sets."
    (lambda (s2)
      (let ((result (make-hash-table :test 'equal)))
        (hydra-set-foreach
         (lambda (x) (when (hydra-set-member-p x s2) (puthash x t result)))
         s1)
        result))))

;; map :: (a -> b) -> Set a -> Set b
(defvar hydra_lib_sets_map
  (lambda (f)
    "Map a function over a set."
    (lambda (s)
      (let ((result (make-hash-table :test 'equal :size (max 1 (hydra-set-size s)))))
        (hydra-set-foreach
         (lambda (x) (puthash (funcall f x) t result))
         s)
        result))))

;; member :: a -> Set a -> Bool
(defvar hydra_lib_sets_member
  (lambda (x)
    "Check if an element is in a set."
    (lambda (s)
      (hydra-set-member-p x s))))

;; null :: Set a -> Bool
(defvar hydra_lib_sets_null
  (lambda (s)
    "Check if a set is empty."
    (hydra-set-empty-p s)))

;; singleton :: a -> Set a
(defvar hydra_lib_sets_singleton
  (lambda (x)
    "Create a singleton set."
    (let ((h (make-hash-table :test 'equal :size 1)))
      (puthash x t h)
      h)))

;; size :: Set a -> Int
(defvar hydra_lib_sets_size
  (lambda (s)
    "Get the size of a set."
    (hydra-set-size s)))

;; to_list :: Set a -> [a]
(defvar hydra_lib_sets_to_list
  (lambda (s)
    "Convert a set to a sorted list."
    (hydra-set-sorted-list s)))

;; union :: Set a -> Set a -> Set a
(defvar hydra_lib_sets_union
  (lambda (s1)
    "Compute the union of two sets."
    (lambda (s2)
      (let ((result (hydra-set-to-hash s2)))
        (hydra-set-foreach (lambda (x) (puthash x t result)) s1)
        result))))

;; unions :: [Set a] -> Set a
(defvar hydra_lib_sets_unions
  (lambda (sets)
    "Compute the union of multiple sets."
    (let ((result (make-hash-table :test 'equal)))
      (dolist (s sets result)
        (hydra-set-foreach (lambda (x) (puthash x t result)) s)))))

(provide 'hydra.lib.sets)
