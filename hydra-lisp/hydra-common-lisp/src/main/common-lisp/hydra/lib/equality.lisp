(in-package :cl-user)

;; Generic comparison for ordering heterogeneous values.
;; Returns -1, 0, or 1.
(defun hash-table-equal-p (a b)
  "Compare two hash tables for structural equality."
  (and (= (hash-table-count a) (hash-table-count b))
       (block nil
         (maphash (lambda (k v)
                    (multiple-value-bind (bv found) (gethash k b)
                      (unless (and found (= (generic-compare v bv) 0))
                        (return nil))))
                  a)
         t)))

(defun generic-compare (a b)
  (cond
    ((eq a b) 0)
    ((and (null a) (null b)) 0)
    ((null a) -1)
    ((null b) 1)
    ((and (hash-table-p a) (hash-table-p b))
     ;; Compare hash tables by converting to sorted alists
     (if (hash-table-equal-p a b) 0
         (let* ((al (sort (let (r) (maphash (lambda (k v) (push (cons k v) r)) a) r)
                          (lambda (x y) (< (generic-compare (car x) (car y)) 0))))
                (bl (sort (let (r) (maphash (lambda (k v) (push (cons k v) r)) b) r)
                          (lambda (x y) (< (generic-compare (car x) (car y)) 0)))))
           (generic-compare al bl))))
    ((and (numberp a) (numberp b))
     (cond ((< a b) -1) ((= a b) 0) (t 1)))
    ((and (stringp a) (stringp b))
     (cond ((string< a b) -1) ((string= a b) 0) (t 1)))
    ((and (characterp a) (characterp b))
     (cond ((char< a b) -1) ((char= a b) 0) (t 1)))
    ((and (symbolp a) (symbolp b))
     (let ((sa (symbol-name a)) (sb (symbol-name b)))
       (cond ((string< sa sb) -1) ((string= sa sb) 0) (t 1))))
    ((and (typep a 'boolean) (typep b 'boolean))
     (cond ((and (not a) b) -1) ((eq a b) 0) (t 1)))
    ((and (consp a) (consp b))
     (let ((c (generic-compare (car a) (car b))))
       (if (= c 0)
           (generic-compare (cdr a) (cdr b))
           c)))
    (t (let ((sa (write-to-string a)) (sb (write-to-string b)))
         (cond ((string< sa sb) -1) ((string= sa sb) 0) (t 1))))))

;; compare :: a -> a -> Comparison
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
      (= (generic-compare a b) 0))))

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
