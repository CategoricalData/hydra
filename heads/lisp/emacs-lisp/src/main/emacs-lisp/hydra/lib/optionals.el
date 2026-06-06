;;; optionals.el --- Hydra Optional primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Maybe representation: (list :just val) or (list :nothing)

;; Do NOT treat (:maybe ...) as native maybe — that is Hydra's term-level
;; representation and collides with nested optionals like maybe<maybe<string>>.
(defun maybe-nothing-p (m)
  (or (null m)
      (and (consp m) (eq (car m) :nothing))))

(defun maybe-value (m)
  (cond
    ((and (consp m) (eq (car m) :just)) (cadr m))
    (t m)))

;; apply :: Maybe (a -> b) -> Maybe a -> Maybe b
(defvar hydra_lib_optionals_apply
  (lambda (mf)
    (lambda (mx)
      (if (maybe-nothing-p mf)
          (list :nothing)
          (if (maybe-nothing-p mx)
              (list :nothing)
              (list :just (funcall (maybe-value mf) (maybe-value mx))))))))

;; bind :: Maybe a -> (a -> Maybe b) -> Maybe b
(defvar hydra_lib_optionals_bind
  (lambda (m)
    (lambda (f)
      (if (maybe-nothing-p m)
          (list :nothing)
          (funcall f (maybe-value m))))))

;; cases :: Maybe a -> b -> (a -> b) -> b
;; Thunk-aware: if def is a zero-arg function (thunk), only called when Maybe is Nothing
(defvar hydra_lib_optionals_cases
  (lambda (m)
    (lambda (def)
      (lambda (f)
        (if (maybe-nothing-p m)
            (if (functionp def) (funcall def) def)
            (funcall f (maybe-value m)))))))

;; cat :: [Maybe a] -> [a]
(defvar hydra_lib_optionals_cat
  (lambda (ms)
    (let ((acc nil))
      (dolist (m ms (nreverse acc))
        (unless (maybe-nothing-p m)
          (push (maybe-value m) acc))))))

;; compose :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
(defvar hydra_lib_optionals_compose
  (lambda (f)
    (lambda (g)
      (lambda (x)
        (let ((result (funcall f x)))
          (if (maybe-nothing-p result)
              (list :nothing)
              (funcall g (maybe-value result))))))))

;; from_optional :: a -> Maybe a -> a
;; Thunk-aware: if def is a zero-arg function (thunk), only called when Maybe is Nothing
(defvar hydra_lib_optionals_from_optional
  (lambda (def)
    (lambda (m)
      (if (maybe-nothing-p m)
          (if (functionp def) (funcall def) def)
          (maybe-value m)))))

;; is_given :: Maybe a -> Bool
(defvar hydra_lib_optionals_is_given
  (lambda (m)
    (not (maybe-nothing-p m))))

;; is_none :: Maybe a -> Bool
(defvar hydra_lib_optionals_is_none
  (lambda (m)
    (maybe-nothing-p m)))

;; map :: (a -> b) -> Maybe a -> Maybe b
(defvar hydra_lib_optionals_map
  (lambda (f)
    (lambda (m)
      (if (maybe-nothing-p m)
          (list :nothing)
          (list :just (funcall f (maybe-value m)))))))

;; map_optional :: (a -> Maybe b) -> [a] -> [b]
(defvar hydra_lib_optionals_map_optional
  (lambda (f)
    (lambda (xs)
      (let ((acc nil))
        (dolist (x xs (nreverse acc))
          (let ((result (funcall f x)))
            (unless (maybe-nothing-p result)
              (push (maybe-value result) acc))))))))

;; pure :: a -> Maybe a
(defvar hydra_lib_optionals_pure
  (lambda (x)
    (list :just x)))

;; to_list :: Maybe a -> [a]
(defvar hydra_lib_optionals_to_list
  (lambda (m)
    (if (maybe-nothing-p m)
        nil
      (list (maybe-value m)))))

(provide 'hydra.lib.optionals)
