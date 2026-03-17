(in-package :cl-user)

;; Merge sort with custom comparator
(defun merge-sorted (less-fn a b)
  (cond
    ((null a) b)
    ((null b) a)
    ((funcall less-fn (car b) (car a))
     (cons (car b) (merge-sorted less-fn a (cdr b))))
    (t (cons (car a) (merge-sorted less-fn (cdr a) b)))))

(defun merge-sort (less-fn lst)
  (if (or (null lst) (null (cdr lst)))
      lst
      (let* ((mid (floor (length lst) 2))
             (left (subseq lst 0 mid))
             (right (subseq lst mid)))
        (merge-sorted less-fn
                      (merge-sort less-fn left)
                      (merge-sort less-fn right)))))

(defun generic<? (a b)
  (< (generic-compare a b) 0))

;; at :: Int -> [a] -> a
(defvar hydra_lib_lists_at
  (lambda (n)
    (lambda (xs)
      (nth n xs))))

;; concat :: [[a]] -> [a]
(defvar hydra_lib_lists_concat
  (lambda (xss)
    (apply #'append xss)))

;; concat2 :: [a] -> [a] -> [a]
(defvar hydra_lib_lists_concat2
  (lambda (xs)
    (lambda (ys)
      (append xs ys))))

;; cons :: a -> [a] -> [a]
(defvar hydra_lib_lists_cons
  (lambda (x)
    (lambda (xs)
      (cons x xs))))

;; drop :: Int -> [a] -> [a]
(defvar hydra_lib_lists_drop
  (lambda (n)
    (lambda (xs)
      (if (<= n 0) xs (nthcdr n xs)))))

;; drop_while :: (a -> Bool) -> [a] -> [a]
(defvar hydra_lib_lists_drop_while
  (lambda (pred)
    (lambda (xs)
      (loop for rest on xs
            while (funcall pred (car rest))
            finally (return rest)))))

;; elem :: a -> [a] -> Bool
(defvar hydra_lib_lists_elem
  (lambda (x)
    (lambda (xs)
      (if (member x xs :test #'equal) t nil))))

;; filter :: (a -> Bool) -> [a] -> [a]
(defvar hydra_lib_lists_filter
  (lambda (pred)
    (lambda (xs)
      (remove-if-not pred xs))))

;; find :: (a -> Bool) -> [a] -> Maybe a
(defvar hydra_lib_lists_find
  (lambda (pred)
    (lambda (xs)
      (let ((found (find-if pred xs)))
        (if found
            (list :just found)
            (list :nothing))))))

;; foldl :: (b -> a -> b) -> b -> [a] -> b
(defvar hydra_lib_lists_foldl
  (lambda (f)
    (lambda (init)
      (lambda (xs)
        (let ((acc init))
          (dolist (x xs acc)
            (setf acc (funcall (funcall f acc) x))))))))

;; head :: [a] -> a
(defvar hydra_lib_lists_head
  (lambda (xs)
    (car xs)))

;; init :: [a] -> [a]
(defvar hydra_lib_lists_init
  (lambda (xs)
    (butlast xs)))

;; intercalate :: [a] -> [[a]] -> [a]
(defvar hydra_lib_lists_intercalate
  (lambda (sep)
    (lambda (xss)
      (if (null xss)
          nil
          (let ((acc (car xss)))
            (dolist (xs (cdr xss) acc)
              (setf acc (append acc sep xs))))))))

;; intersperse :: a -> [a] -> [a]
(defvar hydra_lib_lists_intersperse
  (lambda (sep)
    (lambda (xs)
      (if (or (null xs) (null (cdr xs)))
          xs
          (cons (car xs)
                (loop for x in (cdr xs)
                      append (list sep x)))))))

;; length :: [a] -> Int
(defvar hydra_lib_lists_length
  (lambda (xs)
    (length xs)))

;; map :: (a -> b) -> [a] -> [b]
(defvar hydra_lib_lists_map
  (lambda (f)
    (lambda (xs)
      (mapcar f xs))))

;; nub :: [a] -> [a]  (remove duplicates, keeping first occurrence)
(defvar hydra_lib_lists_nub
  (lambda (xs)
    (let ((seen nil)
          (acc nil))
      (dolist (x xs (nreverse acc))
        (unless (member x seen :test #'equal)
          (push x seen)
          (push x acc))))))

;; null :: [a] -> Bool
(defvar hydra_lib_lists_null
  (lambda (xs)
    (null xs)))

;; partition :: (a -> Bool) -> [a] -> Pair [a] [a]
(defvar hydra_lib_lists_partition
  (lambda (pred)
    (lambda (xs)
      (let ((yes nil) (no nil))
        (dolist (x xs (list (nreverse yes) (nreverse no)))
          (if (funcall pred x)
              (push x yes)
              (push x no)))))))

;; pure :: a -> [a]
(defvar hydra_lib_lists_pure
  (lambda (x)
    (list x)))

;; replicate :: Int -> a -> [a]
(defvar hydra_lib_lists_replicate
  (lambda (n)
    (lambda (x)
      (loop repeat n collect x))))

;; reverse :: [a] -> [a]
(defvar hydra_lib_lists_reverse
  (lambda (xs)
    (reverse xs)))

;; safe_head :: [a] -> Maybe a
(defvar hydra_lib_lists_safe_head
  (lambda (xs)
    (if (null xs)
        (list :nothing)
        (list :just (car xs)))))

;; singleton :: a -> [a]
(defvar hydra_lib_lists_singleton
  (lambda (x)
    (list x)))

;; sort :: [a] -> [a]
(defvar hydra_lib_lists_sort
  (lambda (xs)
    (merge-sort #'generic<? xs)))

;; sort_on :: (a -> b) -> [a] -> [a]
(defvar hydra_lib_lists_sort_on
  (lambda (f)
    (lambda (xs)
      (merge-sort (lambda (a b)
                    (generic<? (funcall f a) (funcall f b)))
                  xs))))

;; span :: (a -> Bool) -> [a] -> Pair [a] [a]
(defvar hydra_lib_lists_span
  (lambda (pred)
    (lambda (xs)
      (let ((acc nil))
        (loop for rest on xs
              while (funcall pred (car rest))
              do (push (car rest) acc)
              finally (return (list (nreverse acc) rest)))))))

;; tail :: [a] -> [a]
(defvar hydra_lib_lists_tail
  (lambda (xs)
    (cdr xs)))

;; take :: Int -> [a] -> [a]
(defvar hydra_lib_lists_take
  (lambda (n)
    (lambda (xs)
      (loop for x in xs
            repeat n
            collect x))))

;; transpose :: [[a]] -> [[a]]
(defun transpose-helper (xss)
  ;; Filter out empty rows, then take heads and recurse on tails
  (let ((non-empty (remove-if #'null xss)))
    (if (null non-empty)
        nil
        (cons (mapcar #'car non-empty)
              (transpose-helper (mapcar #'cdr non-empty))))))

(defvar hydra_lib_lists_transpose
  (lambda (xss)
    (transpose-helper xss)))

;; apply :: [a -> b] -> [a] -> [b]
(defvar hydra_lib_lists_apply
  (lambda (fs)
    (lambda (xs)
      (loop for f in fs
            append (mapcar f xs)))))

;; bind :: [a] -> (a -> [b]) -> [b]
(defvar hydra_lib_lists_bind
  (lambda (xs)
    (lambda (f)
      (loop for x in xs
            append (funcall f x)))))

;; group :: (a -> a -> Comparison) -> [a] -> [[a]]
(defvar hydra_lib_lists_group
  (lambda (xs)
    (if (null xs)
        nil
        (let ((groups nil)
              (current-group (list (car xs)))
              (current-val (car xs)))
          (dolist (x (cdr xs))
            (if (equal current-val x)
                (push x current-group)
                (progn
                  (push (nreverse current-group) groups)
                  (setf current-group (list x))
                  (setf current-val x))))
          (push (nreverse current-group) groups)
          (nreverse groups)))))

;; last :: [a] -> a
(defvar hydra_lib_lists_last
  (lambda (xs)
    (car (last xs))))

;; zip :: [a] -> [b] -> [Pair a b]
(defvar hydra_lib_lists_zip
  (lambda (xs)
    (lambda (ys)
      (mapcar #'list xs ys))))

;; zip_with :: (a -> b -> c) -> [a] -> [b] -> [c]
(defvar hydra_lib_lists_zip_with
  (lambda (f)
    (lambda (xs)
      (lambda (ys)
        (mapcar (lambda (a b) (funcall (funcall f a) b)) xs ys)))))
