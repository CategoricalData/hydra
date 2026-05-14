;;; equality.el --- Hydra equality and comparison primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Generic comparison for ordering heterogeneous values.
;; Returns -1, 0, or 1.
(defun hash-table-structurally-equal-p (a b)
  "Compare two hash tables for structural equality."
  (and (= (hash-table-count a) (hash-table-count b))
       (catch 'done
         (maphash (lambda (k v)
                    (let ((bv (gethash k b :hydra-not-found)))
                      (when (or (eq bv :hydra-not-found)
                                (not (= (generic-compare v bv) 0)))
                        (throw 'done nil))))
                  a)
         t)))

(defun generic-compare (a b)
  (cond
    ((and (null a) (null b)) 0)
    ;; Treat an empty hash-table as equal to nil — both represent the
    ;; empty map/set, and the kernel mixes the two representations
    ;; (e.g. `hydra_lib_maps_empty` is nil; `(from_list nil)` is a
    ;; zero-count hash-table).
    ((and (null a) (hash-table-p b) (zerop (hash-table-count b))) 0)
    ((and (hash-table-p a) (zerop (hash-table-count a)) (null b)) 0)
    ((null a) -1)
    ((null b) 1)
    ;; Hash-tables compare by sorted key/value pairs. Compare via a
    ;; structural-equality fast-path first to avoid the sort when equal.
    ((and (hash-table-p a) (hash-table-p b))
     (if (hash-table-structurally-equal-p a b) 0
       (let ((al nil) (bl nil))
         (maphash (lambda (k v) (push (cons k v) al)) a)
         (maphash (lambda (k v) (push (cons k v) bl)) b)
         (setq al (sort al (lambda (x y) (< (generic-compare (car x) (car y)) 0))))
         (setq bl (sort bl (lambda (x y) (< (generic-compare (car x) (car y)) 0))))
         (generic-compare al bl))))
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
    (t (let ((sa (format "%S" a)) (sb (format "%S" b)))
         (cond ((string< sa sb) -1) ((string= sa sb) 0) (t 1))))))

;; compare :: a -> a -> Comparison
(defvar hydra_lib_equality_compare
  (lambda (a)
    "Compare two values and return a Comparison."
    (lambda (b)
      (let ((c (generic-compare a b)))
        (cond
         ((< c 0) (list :less_than nil))
         ((> c 0) (list :greater_than nil))
         (t (list :equal_to nil)))))))

;; equal :: a -> a -> Bool
(defvar hydra_lib_equality_equal
  (lambda (a)
    "Check if two values are equal."
    (lambda (b)
      (equal a b))))

;; gt :: a -> a -> Bool
(defvar hydra_lib_equality_gt
  (lambda (a)
    "Check if first value is greater than second."
    (lambda (b)
      (> (generic-compare a b) 0))))

;; gte :: a -> a -> Bool
(defvar hydra_lib_equality_gte
  (lambda (a)
    "Check if first value is greater than or equal to second."
    (lambda (b)
      (>= (generic-compare a b) 0))))

;; identity :: a -> a
(defvar hydra_lib_equality_identity
  (lambda (x)
    "Return a value unchanged."
    x))

;; lt :: a -> a -> Bool
(defvar hydra_lib_equality_lt
  (lambda (a)
    "Check if first value is less than second."
    (lambda (b)
      (< (generic-compare a b) 0))))

;; lte :: a -> a -> Bool
(defvar hydra_lib_equality_lte
  (lambda (a)
    "Check if first value is less than or equal to second."
    (lambda (b)
      (<= (generic-compare a b) 0))))

;; max :: a -> a -> a
(defvar hydra_lib_equality_max
  (lambda (a)
    "Return the maximum of two values."
    (lambda (b)
      (if (>= (generic-compare a b) 0) a b))))

;; min :: a -> a -> a
(defvar hydra_lib_equality_min
  (lambda (a)
    "Return the minimum of two values."
    (lambda (b)
      (if (<= (generic-compare a b) 0) a b))))

(provide 'hydra.lib.equality)
