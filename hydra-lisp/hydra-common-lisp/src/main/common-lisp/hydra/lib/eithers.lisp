(in-package :cl-user)

;; Either representation: (list :left val) or (list :right val)

(defun either-tag (e) (first e))
(defun either-val (e) (second e))

;; bimap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
;; Map over both sides of an Either value.
(defvar hydra_lib_eithers_bimap
  (lambda (f)
    (lambda (g)
      (lambda (e)
        (if (eq (either-tag e) :left)
            (list :left (funcall f (either-val e)))
            (list :right (funcall g (either-val e))))))))

;; bind :: Either a b -> (b -> Either a c) -> Either a c
;; Bind (flatMap) for Either: if Right, apply the function; if Left, return unchanged.
(defvar hydra_lib_eithers_bind
  (lambda (e)
    (lambda (f)
      (if (eq (either-tag e) :left)
          e
          (funcall f (either-val e))))))

;; either :: (a -> c) -> (b -> c) -> Either a b -> c
;; Eliminate an Either value by applying one of two functions.
(defvar hydra_lib_eithers_either
  (lambda (f)
    (lambda (g)
      (lambda (e)
        (if (eq (either-tag e) :left)
            (funcall f (either-val e))
            (funcall g (either-val e)))))))

;; foldl :: (a -> b -> Either c a) -> a -> [b] -> Either c a
;; Left-fold over a list with an Either-returning function, short-circuiting on Left.
(defvar hydra_lib_eithers_foldl
  (lambda (f)
    (lambda (init)
      (lambda (xs)
        (loop with acc = init
              for x in xs
              for result = (funcall (funcall f acc) x)
              do (if (eq (either-tag result) :left)
                     (return result)
                     (setf acc (either-val result)))
              finally (return (list :right acc)))))))

;; from_left :: a -> Either a b -> a
;; Extract the Left value, or return a default.
;; Thunk-aware: if def is a zero-arg function (thunk), only called when Either is Right.
(defvar hydra_lib_eithers_from_left
  (lambda (def)
    (lambda (e)
      (if (eq (either-tag e) :left)
          (either-val e)
          (if (functionp def) (funcall def) def)))))

;; from_right :: b -> Either a b -> b
;; Extract the Right value, or return a default.
;; Thunk-aware: if def is a zero-arg function (thunk), only called when Either is Left.
(defvar hydra_lib_eithers_from_right
  (lambda (def)
    (lambda (e)
      (if (eq (either-tag e) :right)
          (either-val e)
          (if (functionp def) (funcall def) def)))))

;; is_left :: Either a b -> Bool
;; Check if an Either is a Left value.
(defvar hydra_lib_eithers_is_left
  (lambda (e)
    (eq (either-tag e) :left)))

;; is_right :: Either a b -> Bool
;; Check if an Either is a Right value.
(defvar hydra_lib_eithers_is_right
  (lambda (e)
    (eq (either-tag e) :right)))

;; lefts :: [Either a b] -> [a]
;; Extract all Left values from a list of Eithers.
(defvar hydra_lib_eithers_lefts
  (lambda (es)
    (loop for e in es
          when (eq (either-tag e) :left)
          collect (either-val e))))

;; map :: (b -> c) -> Either a b -> Either a c
;; Map a function over the Right side of an Either (standard functor map).
(defvar hydra_lib_eithers_map
  (lambda (f)
    (lambda (e)
      (if (eq (either-tag e) :left)
          e
          (list :right (funcall f (either-val e)))))))

;; map_list :: (a -> Either e b) -> [a] -> Either e [b]
;; Map a function returning Either over a list, collecting results or short-circuiting on Left.
(defvar hydra_lib_eithers_map_list
  (lambda (f)
    (lambda (xs)
      (loop with acc = nil
            for x in xs
            for result = (funcall f x)
            do (if (eq (either-tag result) :left)
                   (return result)
                   (push (either-val result) acc))
            finally (return (list :right (nreverse acc)))))))

;; map_maybe :: (a -> Either e b) -> Maybe a -> Either e (Maybe b)
;; Map a function returning Either over a Maybe, or return Right Nothing if Nothing.
(defvar hydra_lib_eithers_map_maybe
  (lambda (f)
    (lambda (m)
      (if (or (null m) (and (consp m) (eq (first m) :nothing)))
          (list :right (list :nothing))
          (let* ((val (if (and (consp m) (eq (first m) :just))
                         (second m)
                         m))
                 (result (funcall f val)))
            (if (eq (either-tag result) :left)
                result
                (list :right (list :just (either-val result)))))))))

;; map_set :: (a -> Either e b) -> Set a -> Either e (Set b)
;; Map a function returning Either over a Set, collecting results or short-circuiting on Left.
(defvar hydra_lib_eithers_map_set
  (lambda (f)
    (lambda (s)
      (loop with acc = nil
            for x in s
            for result = (funcall f x)
            do (if (eq (either-tag result) :left)
                   (return result)
                   (push (either-val result) acc))
            finally (return (list :right (set-from-list (nreverse acc))))))))

;; partition_eithers :: [Either a b] -> Pair [a] [b]
;; Partition a list of Eithers into lefts and rights.
(defvar hydra_lib_eithers_partition_eithers
  (lambda (es)
    (loop with lefts = nil
          with rights = nil
          for e in es
          do (if (eq (either-tag e) :left)
                 (push (either-val e) lefts)
                 (push (either-val e) rights))
          finally (return (list (nreverse lefts) (nreverse rights))))))

;; rights :: [Either a b] -> [b]
;; Extract all Right values from a list of Eithers.
(defvar hydra_lib_eithers_rights
  (lambda (es)
    (loop for e in es
          when (eq (either-tag e) :right)
          collect (either-val e))))
