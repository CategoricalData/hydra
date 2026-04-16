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

;; transpose :: [[a]] -> [[a]]
;; Transpose a list of lists.
(defun transpose-helper (xss)
  ;; Filter out empty rows, then take heads and recurse on tails
  (let ((non-empty (remove-if #'null xss)))
    (if (null non-empty)
        nil
        (cons (mapcar #'car non-empty)
              (transpose-helper (mapcar #'cdr non-empty))))))

;; apply :: [a -> b] -> [a] -> [b]
;; Apply a list of functions to a list of values (applicative style).
(defvar hydra_lib_lists_apply
  (lambda (fs)
    (lambda (xs)
      (loop for f in fs
            append (mapcar f xs)))))

;; bind :: [a] -> (a -> [b]) -> [b]
;; Apply a function that returns lists to each element and flatten results.
(defvar hydra_lib_lists_bind
  (lambda (xs)
    (lambda (f)
      (loop for x in xs
            append (funcall f x)))))

;; concat :: [[a]] -> [a]
;; Concatenate a list of lists.
(defvar hydra_lib_lists_concat
  (lambda (xss)
    (apply #'append xss)))

;; concat2 :: [a] -> [a] -> [a]
;; Concatenate two lists.
(defvar hydra_lib_lists_concat2
  (lambda (xs)
    (lambda (ys)
      (append xs ys))))

;; cons :: a -> [a] -> [a]
;; Prepend a value to a list.
(defvar hydra_lib_lists_cons
  (lambda (x)
    (lambda (xs)
      (cons x xs))))

;; drop :: Int -> [a] -> [a]
;; Drop the first n elements from a list.
(defvar hydra_lib_lists_drop
  (lambda (n)
    (lambda (xs)
      (if (<= n 0) xs (nthcdr n xs)))))

;; drop_while :: (a -> Bool) -> [a] -> [a]
;; Drop elements from the beginning of a list while predicate is true.
(defvar hydra_lib_lists_drop_while
  (lambda (pred)
    (lambda (xs)
      (loop for rest on xs
            while (funcall pred (car rest))
            finally (return rest)))))

;; elem :: a -> [a] -> Bool
;; Check if an element is in a list.
(defvar hydra_lib_lists_elem
  (lambda (x)
    (lambda (xs)
      (if (member x xs :test #'equal) t nil))))

;; filter :: (a -> Bool) -> [a] -> [a]
;; Filter a list based on a predicate.
(defvar hydra_lib_lists_filter
  (lambda (pred)
    (lambda (xs)
      (remove-if-not pred xs))))

;; find :: (a -> Bool) -> [a] -> Maybe a
;; Find the first element matching a predicate.
(defvar hydra_lib_lists_find
  (lambda (pred)
    (lambda (xs)
      (let ((found (find-if pred xs)))
        (if found
            (list :just found)
            (list :nothing))))))

;; foldl :: (b -> a -> b) -> b -> [a] -> b
;; Fold a list from the left.
(defvar hydra_lib_lists_foldl
  (lambda (f)
    (lambda (init)
      (lambda (xs)
        (let ((acc init))
          (dolist (x xs acc)
            (setf acc (funcall (funcall f acc) x))))))))

;; foldr :: (a -> b -> b) -> b -> [a] -> b
;; Fold a list from the right.
(defvar hydra_lib_lists_foldr
  (lambda (f)
    (lambda (init)
      (lambda (xs)
        (let ((acc init))
          (dolist (x (reverse xs) acc)
            (setf acc (funcall (funcall f x) acc))))))))

;; group :: [a] -> [[a]]
;; Group consecutive equal elements.
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

;; intercalate :: [a] -> [[a]] -> [a]
;; Intercalate a list of lists with a separator list between each.
(defvar hydra_lib_lists_intercalate
  (lambda (sep)
    (lambda (xss)
      (if (null xss)
          nil
          (let ((acc (car xss)))
            (dolist (xs (cdr xss) acc)
              (setf acc (append acc sep xs))))))))

;; intersperse :: a -> [a] -> [a]
;; Intersperse a value between elements of a list.
(defvar hydra_lib_lists_intersperse
  (lambda (sep)
    (lambda (xs)
      (if (or (null xs) (null (cdr xs)))
          xs
          (cons (car xs)
                (loop for x in (cdr xs)
                      append (list sep x)))))))

;; length :: [a] -> Int
;; Get the length of a list.
(defvar hydra_lib_lists_length
  (lambda (xs)
    (length xs)))

;; map :: (a -> b) -> [a] -> [b]
;; Map a function over a list.
(defvar hydra_lib_lists_map
  (lambda (f)
    (lambda (xs)
      (mapcar f xs))))

;; maybe_at :: Int -> [a] -> Maybe a
;; Get the element at a specified index, returning Nothing if the index is out of bounds.
(defvar hydra_lib_lists_maybe_at
  (lambda (n)
    (lambda (xs)
      (if (and (>= n 0) (< n (length xs)))
          (list :just (nth n xs))
          (list :nothing)))))

;; maybe_head :: [a] -> Maybe a
;; Get the first element of a list, returning Nothing if the list is empty.
(defvar hydra_lib_lists_maybe_head
  (lambda (xs)
    (if (null xs)
        (list :nothing)
        (list :just (car xs)))))

;; maybe_init :: [a] -> Maybe [a]
;; Return all elements except the last, returning Nothing if the list is empty.
(defvar hydra_lib_lists_maybe_init
  (lambda (xs)
    (if (null xs)
        (list :nothing)
        (list :just (butlast xs)))))

;; maybe_last :: [a] -> Maybe a
;; Get the last element of a list, returning Nothing if the list is empty.
(defvar hydra_lib_lists_maybe_last
  (lambda (xs)
    (if (null xs)
        (list :nothing)
        (list :just (car (last xs))))))

;; maybe_tail :: [a] -> Maybe [a]
;; Get all elements except the first, returning Nothing if the list is empty.
(defvar hydra_lib_lists_maybe_tail
  (lambda (xs)
    (if (null xs)
        (list :nothing)
        (list :just (cdr xs)))))

;; nub :: [a] -> [a]
;; Remove duplicate elements from a list.
(defvar hydra_lib_lists_nub
  (lambda (xs)
    (let ((seen nil)
          (acc nil))
      (dolist (x xs (nreverse acc))
        (unless (member x seen :test #'equal)
          (push x seen)
          (push x acc))))))

;; null :: [a] -> Bool
;; Check if a list is empty.
(defvar hydra_lib_lists_null
  (lambda (xs)
    (null xs)))

;; partition :: (a -> Bool) -> [a] -> Pair [a] [a]
;; Partition a list into elements that satisfy a predicate and elements that do not.
(defvar hydra_lib_lists_partition
  (lambda (pred)
    (lambda (xs)
      (let ((yes nil) (no nil))
        (dolist (x xs (list (nreverse yes) (nreverse no)))
          (if (funcall pred x)
              (push x yes)
              (push x no)))))))

;; pure :: a -> [a]
;; Create a list with a single element.
(defvar hydra_lib_lists_pure
  (lambda (x)
    (list x)))

;; replicate :: Int -> a -> [a]
;; Create a list with n copies of a value.
(defvar hydra_lib_lists_replicate
  (lambda (n)
    (lambda (x)
      (loop repeat n collect x))))

;; reverse :: [a] -> [a]
;; Reverse a list.
(defvar hydra_lib_lists_reverse
  (lambda (xs)
    (reverse xs)))

;; singleton :: a -> [a]
;; Create a single-element list.
(defvar hydra_lib_lists_singleton
  (lambda (x)
    (list x)))

;; sort :: [a] -> [a]
;; Sort a list.
(defvar hydra_lib_lists_sort
  (lambda (xs)
    (merge-sort #'generic<? xs)))

;; sort_on :: (a -> b) -> [a] -> [a]
;; Sort a list based on a key function.
(defvar hydra_lib_lists_sort_on
  (lambda (f)
    (lambda (xs)
      (merge-sort (lambda (a b)
                    (generic<? (funcall f a) (funcall f b)))
                  xs))))

;; span :: (a -> Bool) -> [a] -> Pair [a] [a]
;; Split a list at the first element where predicate fails.
(defvar hydra_lib_lists_span
  (lambda (pred)
    (lambda (xs)
      (let ((acc nil))
        (loop for rest on xs
              while (funcall pred (car rest))
              do (push (car rest) acc)
              finally (return (list (nreverse acc) rest)))))))

;; take :: Int -> [a] -> [a]
;; Take the first n elements from a list.
(defvar hydra_lib_lists_take
  (lambda (n)
    (lambda (xs)
      (loop for x in xs
            repeat n
            collect x))))

;; transpose :: [[a]] -> [[a]]
;; Transpose a list of lists.
(defvar hydra_lib_lists_transpose
  (lambda (xss)
    (transpose-helper xss)))

;; uncons :: [a] -> Maybe (a, [a])
;; Split a list into its head and tail, returning Nothing if the list is empty.
(defvar hydra_lib_lists_uncons
  (lambda (xs)
    (if (null xs)
        (list :nothing)
        (list :just (list (car xs) (cdr xs))))))

;; zip :: [a] -> [b] -> [Pair a b]
;; Zip two lists into pairs.
(defvar hydra_lib_lists_zip
  (lambda (xs)
    (lambda (ys)
      (mapcar #'list xs ys))))

;; zip_with :: (a -> b -> c) -> [a] -> [b] -> [c]
;; Zip two lists with a combining function.
(defvar hydra_lib_lists_zip_with
  (lambda (f)
    (lambda (xs)
      (lambda (ys)
        (mapcar (lambda (a b) (funcall (funcall f a) b)) xs ys)))))
