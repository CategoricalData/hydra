;;; equality.el --- Hydra equality and comparison primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Generic comparison for ordering heterogeneous values.
;; Returns -1, 0, or 1.
(defun generic-compare (a b)
  (cond
    ((equal a b) 0)
    ((and (numberp a) (numberp b))
     (cond ((< a b) -1) ((= a b) 0) (t 1)))
    ((and (stringp a) (stringp b))
     (cond ((string< a b) -1) ((string= a b) 0) (t 1)))
    ((and (characterp a) (characterp b))
     (cond ((< a b) -1) ((= a b) 0) (t 1)))
    ((and (symbolp a) (symbolp b))
     (let ((sa (symbol-name a)) (sb (symbol-name b)))
       (cond ((string< sa sb) -1) ((string= sa sb) 0) (t 1))))
    ((and (consp a) (consp b))
     (let ((c (generic-compare (car a) (car b))))
       (if (= c 0)
           (generic-compare (cdr a) (cdr b))
           c)))
    ((and (null a) (null b)) 0)
    ((null a) -1)
    ((null b) 1)
    (t (let ((sa (format "%S" a)) (sb (format "%S" b)))
         (cond ((string< sa sb) -1) ((string= sa sb) 0) (t 1))))))

;; compare :: a -> a -> Int
(defvar hydra_lib_equality_compare
  (lambda (a)
    (lambda (b)
      (let ((c (generic-compare a b)))
        (cond
         ((< c 0) (list :less_than nil))
         ((> c 0) (list :greater_than nil))
         (t (list :equal_to nil)))))))

;; equal :: a -> a -> Bool
(defvar hydra_lib_equality_equal
  (lambda (a)
    (lambda (b)
      (equal a b))))

;; gt :: a -> a -> Bool
(defvar hydra_lib_equality_gt
  (lambda (a)
    (lambda (b)
      (> (generic-compare a b) 0))))

;; gte :: a -> a -> Bool
(defvar hydra_lib_equality_gte
  (lambda (a)
    (lambda (b)
      (>= (generic-compare a b) 0))))

;; identity :: a -> a
(defvar hydra_lib_equality_identity
  (lambda (x) x))

;; lt :: a -> a -> Bool
(defvar hydra_lib_equality_lt
  (lambda (a)
    (lambda (b)
      (< (generic-compare a b) 0))))

;; lte :: a -> a -> Bool
(defvar hydra_lib_equality_lte
  (lambda (a)
    (lambda (b)
      (<= (generic-compare a b) 0))))

;; max :: a -> a -> a
(defvar hydra_lib_equality_max
  (lambda (a)
    (lambda (b)
      (if (>= (generic-compare a b) 0) a b))))

;; min :: a -> a -> a
(defvar hydra_lib_equality_min
  (lambda (a)
    (lambda (b)
      (if (<= (generic-compare a b) 0) a b))))

(provide 'hydra.lib.equality)
