(in-package :cl-user)

;; Sets are persistent ordered red-black trees with structural sharing,
;; backed by the same RB-tree machinery as maps.lisp but storing only keys
;; (values are uniformly nil). All mutating operations are O(log n).
;;
;; Representation:
;;   - The empty set is `nil`.
;;   - A non-empty set is a 7-element list with tag :rbnode (same as maps).
;;
;; Back-compat: read-only ops accept legacy sorted-list values.
;;
;; Sets use the same `rbnode-*` accessors and `rb-insert`/`rb-delete`
;; helpers defined in maps.lisp. The "value" slot is unused; we store t.

(defun sets-list-p (s)
  "True if s is a non-empty plain list (legacy sorted-list representation)."
  (and (consp s) (not (eq (first s) :rbnode))))

(defun sets-empty-p (s)
  (or (null s)
      (and (rbnode-p s) (zerop (rbnode-size s)))))

(defun sets-member-raw (x s)
  (cond
    ((null s) nil)
    ((rbnode-p s) (nth-value 1 (rb-lookup s x)))
    ((sets-list-p s) (and (member x s :test #'equal) t))
    (t nil)))

(defun sets-each (s fn)
  "Call fn on each element of s in sorted order. Accepts rbnode, list, or nil."
  (cond
    ((null s) nil)
    ((rbnode-p s) (rb-for-each s (lambda (k v) (declare (ignore v)) (funcall fn k))))
    ((sets-list-p s) (dolist (x s) (funcall fn x)))))

(defun sets-elements-sorted (s)
  "Return elements of s sorted by generic-compare. Accepts rbnode, list, or nil."
  (cond
    ((null s) nil)
    ((rbnode-p s) (mapcar #'car (rb-entries s)))
    ((sets-list-p s) (sort (copy-list s) (lambda (a b) (< (generic-compare a b) 0))))
    (t nil)))

(defun sets-from-list-raw (xs)
  "Build an RB-tree set from a list."
  (let ((result nil))
    (dolist (x xs result)
      (setf result (rb-insert result x t)))))

;; Legacy helper retained for callers in other lib files (eithers.lisp).
(defun set-from-list (xs)
  "Build a set from xs. Returns an RB-tree."
  (sets-from-list-raw xs))

;; delete :: a -> Set a -> Set a
(defvar hydra_lib_sets_delete
  (lambda (x)
    (lambda (s)
      (cond
        ((null s) nil)
        ((rbnode-p s) (rb-delete s x))
        ((sets-list-p s) (remove x s :test #'equal))
        (t s)))))

;; difference :: Set a -> Set a -> Set a
(defvar hydra_lib_sets_difference
  (lambda (s1)
    (lambda (s2)
      (let ((result nil))
        (sets-each s1 (lambda (x)
                        (unless (sets-member-raw x s2)
                          (setf result (rb-insert result x t)))))
        result))))

;; empty :: Set a
(defvar hydra_lib_sets_empty nil)

;; from_list :: [a] -> Set a
(defvar hydra_lib_sets_from_list
  (lambda (xs) (sets-from-list-raw xs)))

;; insert :: a -> Set a -> Set a
(defvar hydra_lib_sets_insert
  (lambda (x)
    (lambda (s)
      (cond
        ((or (null s) (rbnode-p s)) (rb-insert s x t))
        ((sets-list-p s) (rb-insert (sets-from-list-raw s) x t))
        (t (rb-insert nil x t))))))

;; intersection :: Set a -> Set a -> Set a
(defvar hydra_lib_sets_intersection
  (lambda (s1)
    (lambda (s2)
      (let ((result nil))
        (sets-each s1 (lambda (x)
                        (when (sets-member-raw x s2)
                          (setf result (rb-insert result x t)))))
        result))))

;; map :: (a -> b) -> Set a -> Set b
(defvar hydra_lib_sets_map
  (lambda (f)
    (lambda (s)
      (let ((result nil))
        (sets-each s (lambda (x) (setf result (rb-insert result (funcall f x) t))))
        result))))

;; member :: a -> Set a -> Bool
(defvar hydra_lib_sets_member
  (lambda (x)
    (lambda (s) (sets-member-raw x s))))

;; null :: Set a -> Bool
(defvar hydra_lib_sets_null
  (lambda (s) (sets-empty-p s)))

;; size :: Set a -> Int
(defvar hydra_lib_sets_size
  (lambda (s)
    (cond
      ((null s) 0)
      ((rbnode-p s) (rbnode-size s))
      ((sets-list-p s) (length s))
      (t 0))))

;; singleton :: a -> Set a
(defvar hydra_lib_sets_singleton
  (lambda (x)
    (rb-insert nil x t)))

;; to_list :: Set a -> [a]
;; Sorted via generic-compare for determinism.
(defvar hydra_lib_sets_to_list
  (lambda (s) (sets-elements-sorted s)))

;; union :: Set a -> Set a -> Set a
(defvar hydra_lib_sets_union
  (lambda (s1)
    (lambda (s2)
      (cond
        ((null s1) s2)
        ((null s2) s1)
        (t
         (let ((result (cond
                         ((rbnode-p s2) s2)
                         ((sets-list-p s2) (sets-from-list-raw s2))
                         (t nil))))
           (sets-each s1 (lambda (x) (setf result (rb-insert result x t))))
           result))))))

;; unions :: [Set a] -> Set a
(defvar hydra_lib_sets_unions
  (lambda (sets)
    (let ((result nil))
      (dolist (s sets result)
        (sets-each s (lambda (x) (setf result (rb-insert result x t))))))))
