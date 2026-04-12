(in-package :cl-user)

;; Pairs are 2-element lists: (list a b)

;; bimap :: (a -> c) -> (b -> d) -> Pair a b -> Pair c d
;; Map over both elements of a pair.
(defvar hydra_lib_pairs_bimap
  (lambda (f)
    (lambda (g)
      (lambda (p)
        (list (funcall f (first p)) (funcall g (second p)))))))

;; first :: Pair a b -> a
;; Get the first element of a pair.
(defvar hydra_lib_pairs_first
  (lambda (p)
    (first p)))

;; second :: Pair a b -> b
;; Get the second element of a pair.
(defvar hydra_lib_pairs_second
  (lambda (p)
    (second p)))
