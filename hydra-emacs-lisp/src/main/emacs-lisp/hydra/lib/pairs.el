;;; pairs.el --- Hydra pair primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Pairs are 2-element lists: (list a b)

;; bimap :: (a -> c) -> (b -> d) -> Pair a b -> Pair c d
(defvar hydra_lib_pairs_bimap
  (lambda (f)
    (lambda (g)
      (lambda (p)
        (list (funcall f (car p)) (funcall g (cadr p)))))))

;; first :: Pair a b -> a
(defvar hydra_lib_pairs_first
  (lambda (p)
    (car p)))

;; second :: Pair a b -> b
(defvar hydra_lib_pairs_second
  (lambda (p)
    (cadr p)))

(provide 'hydra.lib.pairs)
