;;; lists.el --- Hydra list primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Merge sort with custom comparator (non-destructive)
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
      (let* ((mid (/ (length lst) 2))
             (left (cl-subseq lst 0 mid))
             (right (cl-subseq lst mid)))
        (merge-sorted less-fn
                      (merge-sort less-fn left)
                      (merge-sort less-fn right)))))

(defun generic<? (a b)
  (< (generic-compare a b) 0))

;; apply :: [a -> b] -> [a] -> [b]
(defvar hydra_lib_lists_apply
  (lambda (fs)
    "Apply a list of functions to a list of values (applicative style)."
    (lambda (xs)
      (let ((acc nil))
        (dolist (f fs (nreverse acc))
          (dolist (x xs)
            (push (funcall f x) acc)))))))

;; bind :: [a] -> (a -> [b]) -> [b]
(defvar hydra_lib_lists_bind
  (lambda (xs)
    "Apply a function that returns lists to each element and flatten results."
    (lambda (f)
      (apply #'append (mapcar f xs)))))

;; concat :: [[a]] -> [a]
(defvar hydra_lib_lists_concat
  (lambda (xss)
    "Concatenate a list of lists."
    (apply #'append xss)))

;; concat2 :: [a] -> [a] -> [a]
(defvar hydra_lib_lists_concat2
  (lambda (xs)
    "Concatenate two lists."
    (lambda (ys)
      (append xs ys))))

;; cons :: a -> [a] -> [a]
(defvar hydra_lib_lists_cons
  (lambda (x)
    "Prepend a value to a list."
    (lambda (xs)
      (cons x xs))))

;; drop :: Int -> [a] -> [a]
(defvar hydra_lib_lists_drop
  (lambda (n)
    "Drop the first n elements from a list."
    (lambda (xs)
      (if (<= n 0) xs (nthcdr n xs)))))

;; drop_while :: (a -> Bool) -> [a] -> [a]
(defvar hydra_lib_lists_drop_while
  (lambda (pred)
    "Drop elements from the beginning of a list while predicate is true."
    (lambda (xs)
      (let ((rest xs))
        (while (and rest (funcall pred (car rest)))
          (setq rest (cdr rest)))
        rest))))

;; elem :: a -> [a] -> Bool
(defvar hydra_lib_lists_elem
  (lambda (x)
    "Check if an element is in a list."
    (lambda (xs)
      (if (member x xs) t nil))))

;; filter :: (a -> Bool) -> [a] -> [a]
(defvar hydra_lib_lists_filter
  (lambda (pred)
    "Filter a list based on a predicate."
    (lambda (xs)
      (cl-remove-if-not pred xs))))

;; find :: (a -> Bool) -> [a] -> Maybe a
(defvar hydra_lib_lists_find
  (lambda (pred)
    "Find the first element matching a predicate."
    (lambda (xs)
      (let ((found (cl-find-if pred xs)))
        (if found
            (list :just found)
            (list :nothing))))))

;; foldl :: (b -> a -> b) -> b -> [a] -> b
(defvar hydra_lib_lists_foldl
  (lambda (f)
    "Fold a list from the left."
    (lambda (init)
      (lambda (xs)
        (let ((acc init))
          (dolist (x xs acc)
            (setq acc (funcall (funcall f acc) x))))))))

;; foldr :: (a -> b -> b) -> b -> [a] -> b
(defvar hydra_lib_lists_foldr
  (lambda (f)
    "Fold a list from the right."
    (lambda (init)
      (lambda (xs)
        (let ((acc init))
          (dolist (x (reverse xs) acc)
            (setq acc (funcall (funcall f x) acc))))))))

;; group :: [a] -> [[a]]
(defvar hydra_lib_lists_group
  (lambda (xs)
    "Group consecutive equal elements."
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
                  (setq current-group (list x))
                  (setq current-val x))))
          (push (nreverse current-group) groups)
          (nreverse groups)))))

;; intercalate :: [a] -> [[a]] -> [a]
(defvar hydra_lib_lists_intercalate
  (lambda (sep)
    "Intercalate a list of lists with a separator list between each."
    (lambda (xss)
      (if (null xss)
          nil
          (let ((acc (car xss)))
            (dolist (xs (cdr xss) acc)
              (setq acc (append acc sep xs))))))))

;; intersperse :: a -> [a] -> [a]
(defvar hydra_lib_lists_intersperse
  (lambda (sep)
    "Intersperse a value between elements of a list."
    (lambda (xs)
      (if (or (null xs) (null (cdr xs)))
          xs
          (cons (car xs)
                (let ((acc nil))
                  (dolist (x (cdr xs) (nreverse acc))
                    (push sep acc)
                    (push x acc))))))))

;; length :: [a] -> Int
(defvar hydra_lib_lists_length
  (lambda (xs)
    "Get the length of a list."
    (length xs)))

;; map :: (a -> b) -> [a] -> [b]
(defvar hydra_lib_lists_map
  (lambda (f)
    "Map a function over a list."
    (lambda (xs)
      (mapcar f xs))))

;; maybe_at :: Int -> [a] -> Maybe a
(defvar hydra_lib_lists_maybe_at
  (lambda (n)
    "Get the element at a specified index, returning Nothing if the index is out of bounds."
    (lambda (xs)
      (if (and (>= n 0) (< n (length xs)))
          (list :just (nth n xs))
          (list :nothing)))))

;; maybe_head :: [a] -> Maybe a
(defvar hydra_lib_lists_maybe_head
  (lambda (xs)
    "Get the first element of a list, returning Nothing if the list is empty."
    (if (null xs)
        (list :nothing)
        (list :just (car xs)))))

;; maybe_init :: [a] -> Maybe [a]
(defvar hydra_lib_lists_maybe_init
  (lambda (xs)
    "Return all elements except the last, returning Nothing if the list is empty."
    (if (null xs)
        (list :nothing)
        (list :just (butlast xs)))))

;; maybe_last :: [a] -> Maybe a
(defvar hydra_lib_lists_maybe_last
  (lambda (xs)
    "Get the last element of a list, returning Nothing if the list is empty."
    (if (null xs)
        (list :nothing)
        (list :just (car (last xs))))))

;; maybe_tail :: [a] -> Maybe [a]
(defvar hydra_lib_lists_maybe_tail
  (lambda (xs)
    "Get all elements except the first, returning Nothing if the list is empty."
    (if (null xs)
        (list :nothing)
        (list :just (cdr xs)))))

;; nub :: [a] -> [a]  (remove duplicates, keeping first occurrence)
(defvar hydra_lib_lists_nub
  (lambda (xs)
    "Remove duplicate elements from a list."
    (let ((seen nil)
          (acc nil))
      (dolist (x xs (nreverse acc))
        (unless (member x seen)
          (push x seen)
          (push x acc))))))

;; null :: [a] -> Bool
(defvar hydra_lib_lists_null
  (lambda (xs)
    "Check if a list is empty."
    (null xs)))

;; partition :: (a -> Bool) -> [a] -> Pair [a] [a]
(defvar hydra_lib_lists_partition
  (lambda (pred)
    "Partition a list into elements that satisfy a predicate and elements that do not."
    (lambda (xs)
      (let ((yes nil) (no nil))
        (dolist (x xs (list (nreverse yes) (nreverse no)))
          (if (funcall pred x)
              (push x yes)
              (push x no)))))))

;; pure :: a -> [a]
(defvar hydra_lib_lists_pure
  (lambda (x)
    "Create a list with a single element."
    (list x)))

;; replicate :: Int -> a -> [a]
(defvar hydra_lib_lists_replicate
  (lambda (n)
    "Create a list with n copies of a value."
    (lambda (x)
      (let ((acc nil))
        (dotimes (_ n (nreverse acc))
          (push x acc))))))

;; reverse :: [a] -> [a]
(defvar hydra_lib_lists_reverse
  (lambda (xs)
    "Reverse a list."
    (reverse xs)))

;; singleton :: a -> [a]
(defvar hydra_lib_lists_singleton
  (lambda (x)
    "Create a single-element list."
    (list x)))

;; sort :: [a] -> [a]
(defvar hydra_lib_lists_sort
  (lambda (xs)
    "Sort a list."
    (merge-sort #'generic<? xs)))

;; sort_on :: (a -> b) -> [a] -> [a]
(defvar hydra_lib_lists_sort_on
  (lambda (f)
    "Sort a list based on a key function."
    (lambda (xs)
      (merge-sort (lambda (a b)
                    (generic<? (funcall f a) (funcall f b)))
                  xs))))

;; span :: (a -> Bool) -> [a] -> Pair [a] [a]
(defvar hydra_lib_lists_span
  (lambda (pred)
    "Split a list at the first element where predicate fails."
    (lambda (xs)
      (let ((acc nil)
            (rest xs))
        (while (and rest (funcall pred (car rest)))
          (push (car rest) acc)
          (setq rest (cdr rest)))
        (list (nreverse acc) rest)))))

;; take :: Int -> [a] -> [a]
(defvar hydra_lib_lists_take
  (lambda (n)
    "Take the first n elements from a list."
    (lambda (xs)
      (let ((acc nil)
            (rest xs)
            (i 0))
        (while (and rest (< i n))
          (push (car rest) acc)
          (setq rest (cdr rest))
          (setq i (1+ i)))
        (nreverse acc)))))

;; transpose :: [[a]] -> [[a]]
(defun transpose-helper (xss)
  (let ((non-empty (cl-remove-if #'null xss)))
    (if (null non-empty)
        nil
        (cons (mapcar #'car non-empty)
              (transpose-helper (mapcar #'cdr non-empty))))))

(defvar hydra_lib_lists_transpose
  (lambda (xss)
    "Transpose a list of lists."
    (transpose-helper xss)))

;; uncons :: [a] -> Maybe (a, [a])
(defvar hydra_lib_lists_uncons
  (lambda (xs)
    "Split a list into its head and tail, returning Nothing if the list is empty."
    (if (null xs)
        (list :nothing)
        (list :just (list (car xs) (cdr xs))))))

;; zip :: [a] -> [b] -> [Pair a b]
(defvar hydra_lib_lists_zip
  (lambda (xs)
    "Zip two lists into pairs."
    (lambda (ys)
      (cl-mapcar #'list xs ys))))

;; zip_with :: (a -> b -> c) -> [a] -> [b] -> [c]
(defvar hydra_lib_lists_zip_with
  (lambda (f)
    "Zip two lists with a combining function."
    (lambda (xs)
      (lambda (ys)
        (cl-mapcar (lambda (a b) (funcall (funcall f a) b)) xs ys)))))

(provide 'hydra.lib.lists)
