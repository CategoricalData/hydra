(in-package :cl-user)

;; Pairs are 2-element lists: (list a b)

;; bimap :: (a -> c) -> (b -> d) -> Pair a b -> Pair c d
(defvar hydra_lib_pairs_bimap
  (lambda (f)
    (lambda (g)
      (lambda (p)
        (list (funcall f (first p)) (funcall g (second p)))))))

;; first :: Pair a b -> a
(defvar hydra_lib_pairs_first
  (lambda (p)
    (first p)))

;; second :: Pair a b -> b
(defvar hydra_lib_pairs_second
  (lambda (p)
    (second p)))
