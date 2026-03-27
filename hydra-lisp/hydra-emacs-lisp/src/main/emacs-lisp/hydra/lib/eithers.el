;;; eithers.el --- Hydra Either primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Either representation: (list :left val) or (list :right val)

(defun either-tag (e) (car e))
(defun either-val (e) (cadr e))

;; bimap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
(defvar hydra_lib_eithers_bimap
  (lambda (f)
    "Map over both sides of an Either value."
    (lambda (g)
      (lambda (e)
        (if (eq (either-tag e) :left)
            (list :left (funcall f (either-val e)))
            (list :right (funcall g (either-val e))))))))

;; bind :: Either a b -> (b -> Either a c) -> Either a c
(defvar hydra_lib_eithers_bind
  (lambda (e)
    "Bind (flatMap) for Either: if Right, apply the function; if Left, return unchanged."
    (lambda (f)
      (if (eq (either-tag e) :left)
          e
          (funcall f (either-val e))))))

;; either :: (a -> c) -> (b -> c) -> Either a b -> c
(defvar hydra_lib_eithers_either
  (lambda (f)
    "Eliminate an Either value by applying one of two functions."
    (lambda (g)
      (lambda (e)
        (if (eq (either-tag e) :left)
            (funcall f (either-val e))
            (funcall g (either-val e)))))))

;; foldl :: (a -> b -> Either c a) -> a -> [b] -> Either c a
(defvar hydra_lib_eithers_foldl
  (lambda (f)
    "Left-fold over a list with an Either-returning function, short-circuiting on Left."
    (lambda (init)
      (lambda (xs)
        (let ((acc init))
          (cl-dolist (x xs (list :right acc))
            (let ((result (funcall (funcall f acc) x)))
              (if (eq (either-tag result) :left)
                  (cl-return result)
                  (setq acc (either-val result))))))))))

;; from_left :: a -> Either a b -> a
;; Thunk-aware: if def is a zero-arg function (thunk), only called when Either is Right
(defvar hydra_lib_eithers_from_left
  (lambda (def)
    "Extract the Left value, or return a default."
    (lambda (e)
      (if (eq (either-tag e) :left)
          (either-val e)
          (if (functionp def) (funcall def) def)))))

;; from_right :: b -> Either a b -> b
;; Thunk-aware: if def is a zero-arg function (thunk), only called when Either is Left
(defvar hydra_lib_eithers_from_right
  (lambda (def)
    "Extract the Right value, or return a default."
    (lambda (e)
      (if (eq (either-tag e) :right)
          (either-val e)
          (if (functionp def) (funcall def) def)))))

;; is_left :: Either a b -> Bool
(defvar hydra_lib_eithers_is_left
  (lambda (e)
    "Check if an Either is a Left value."
    (eq (either-tag e) :left)))

;; is_right :: Either a b -> Bool
(defvar hydra_lib_eithers_is_right
  (lambda (e)
    "Check if an Either is a Right value."
    (eq (either-tag e) :right)))

;; lefts :: [Either a b] -> [a]
(defvar hydra_lib_eithers_lefts
  (lambda (es)
    "Extract all Left values from a list of Eithers."
    (let ((acc nil))
      (dolist (e es (nreverse acc))
        (when (eq (either-tag e) :left)
          (push (either-val e) acc))))))

;; map :: (b -> c) -> Either a b -> Either a c
(defvar hydra_lib_eithers_map
  (lambda (f)
    "Map a function over the Right side of an Either (standard functor map)."
    (lambda (e)
      (if (eq (either-tag e) :left)
          e
          (list :right (funcall f (either-val e)))))))

;; map_list :: (a -> Either e b) -> [a] -> Either e [b]
(defvar hydra_lib_eithers_map_list
  (lambda (f)
    "Map a function returning Either over a list, collecting results or short-circuiting on Left."
    (lambda (xs)
      (let ((acc nil))
        (cl-dolist (x xs (list :right (nreverse acc)))
          (let ((result (funcall f x)))
            (if (eq (either-tag result) :left)
                (cl-return result)
                (push (either-val result) acc))))))))

;; map_maybe :: (a -> Either e b) -> Maybe a -> Either e (Maybe b)
(defvar hydra_lib_eithers_map_maybe
  (lambda (f)
    "Map a function returning Either over a Maybe, or return Right Nothing if Nothing."
    (lambda (m)
      (if (or (null m) (and (consp m) (eq (car m) :nothing)))
          (list :right (list :nothing))
          (let* ((val (if (and (consp m) (eq (car m) :just))
                         (cadr m)
                         m))
                 (result (funcall f val)))
            (if (eq (either-tag result) :left)
                result
                (list :right (list :just (either-val result)))))))))

;; map_set :: (a -> Either e b) -> Set a -> Either e (Set b)
(defvar hydra_lib_eithers_map_set
  (lambda (f)
    "Map a function returning Either over a Set, collecting results or short-circuiting on Left."
    (lambda (s)
      (let ((acc nil))
        (cl-dolist (x s (list :right (set-from-list (nreverse acc))))
          (let ((result (funcall f x)))
            (if (eq (either-tag result) :left)
                (cl-return result)
                (push (either-val result) acc))))))))

;; partition_eithers :: [Either a b] -> Pair [a] [b]
(defvar hydra_lib_eithers_partition_eithers
  (lambda (es)
    "Partition a list of Eithers into lefts and rights."
    (let ((lefts nil)
          (rights nil))
      (dolist (e es (list (nreverse lefts) (nreverse rights)))
        (if (eq (either-tag e) :left)
            (push (either-val e) lefts)
            (push (either-val e) rights))))))

;; rights :: [Either a b] -> [b]
(defvar hydra_lib_eithers_rights
  (lambda (es)
    "Extract all Right values from a list of Eithers."
    (let ((acc nil))
      (dolist (e es (nreverse acc))
        (when (eq (either-tag e) :right)
          (push (either-val e) acc))))))

(provide 'hydra.lib.eithers)
