(in-package :cl-user)

;; A Literal.decimal wrapped as a Hydra term, as `equal`/`compare`'s generic x/x TermCoder
;; (which just passes terms through unchanged -- see prims.lisp's tc-variable) actually
;; receives it: (:literal (:decimal (coefficient . scale))), not a bare cons.
(defun hydra-decimal-term-p (term)
  (and (consp term) (eq (car term) :literal)
       (let ((lit (cadr term)))
         (and (consp lit) (eq (car lit) :decimal)))))

(defun hydra-decimal-term-value (term) (cadr (cadr term)))

;; Ordered comparison of two decimals: numeric value first, then scale ascending as a
;; tiebreak (1.1 < 1.10 < 1.100), per docs/specification/ordering-and-equality.md. Cross-
;; multiplies to compare a.coefficient/10^a.scale against b.coefficient/10^b.scale exactly
;; (native CL bignum arithmetic), mirroring the TypeScript/Clojure/Java equivalents.
(defun hydra-compare-decimals (a b)
  (let* ((ca (car a)) (sa (cdr a)) (cb (car b)) (sb (cdr b))
         (max-scale (max sa sb))
         (na (* ca (expt 10 (- max-scale sa))))
         (nb (* cb (expt 10 (- max-scale sb)))))
    (cond ((< na nb) -1) ((> na nb) 1) (t (- sa sb)))))

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
    ((and (hydra-decimal-term-p a) (hydra-decimal-term-p b))
     (hydra-compare-decimals (hydra-decimal-term-value a) (hydra-decimal-term-value b)))
    ((and (consp a) (consp b))
     (let ((c (generic-compare (car a) (car b))))
       (if (= c 0)
           (generic-compare (cdr a) (cdr b))
           c)))
    (t (let ((sa (write-to-string a)) (sb (write-to-string b)))
         (cond ((string< sa sb) -1) ((string= sa sb) 0) (t 1))))))

;; compare :: a -> a -> Comparison
;; Compare two values and return a Comparison.
(defvar hydra_overlay_common_lisp_lib_ordering_compare
  (lambda (a)
    (lambda (b)
      (let ((c (generic-compare a b)))
        (cond
         ((< c 0) (list :less_than nil))
         ((> c 0) (list :greater_than nil))
         (t (list :equal_to nil)))))))

;; gt :: a -> a -> Bool
;; Check if first value is greater than second.
(defvar hydra_overlay_common_lisp_lib_ordering_gt
  (lambda (a)
    (lambda (b)
      (> (generic-compare a b) 0))))

;; gte :: a -> a -> Bool
;; Check if first value is greater than or equal to second.
(defvar hydra_overlay_common_lisp_lib_ordering_gte
  (lambda (a)
    (lambda (b)
      (>= (generic-compare a b) 0))))

;; lt :: a -> a -> Bool
;; Check if first value is less than second.
(defvar hydra_overlay_common_lisp_lib_ordering_lt
  (lambda (a)
    (lambda (b)
      (< (generic-compare a b) 0))))

;; lte :: a -> a -> Bool
;; Check if first value is less than or equal to second.
(defvar hydra_overlay_common_lisp_lib_ordering_lte
  (lambda (a)
    (lambda (b)
      (<= (generic-compare a b) 0))))

;; max :: a -> a -> a
;; Return the maximum of two values.
(defvar hydra_overlay_common_lisp_lib_ordering_max
  (lambda (a)
    (lambda (b)
      (if (>= (generic-compare a b) 0) a b))))

;; min :: a -> a -> a
;; Return the minimum of two values.
(defvar hydra_overlay_common_lisp_lib_ordering_min
  (lambda (a)
    (lambda (b)
      (if (<= (generic-compare a b) 0) a b))))
