(in-package :cl-user)

;; Maybe representation: (list :just val) or (list :nothing)

;; Do NOT treat (:maybe ...) as native maybe -- that is Hydra's term-level
;; representation and collides with nested optionals like maybe<maybe<string>>.
(defun maybe-nothing-p (m)
  (or (null m)
      (and (consp m) (eq (first m) :nothing))))

(defun maybe-value (m)
  (cond
    ((and (consp m) (eq (first m) :just)) (second m))
    (t m)))

;; apply :: Maybe (a -> b) -> Maybe a -> Maybe b
;; Apply a function to an argument (applicative).
(defvar hydra_lib_maybes_apply
  (lambda (mf)
    (lambda (mx)
      (if (maybe-nothing-p mf)
          (list :nothing)
          (if (maybe-nothing-p mx)
              (list :nothing)
              (list :just (funcall (maybe-value mf) (maybe-value mx))))))))

;; bind :: Maybe a -> (a -> Maybe b) -> Maybe b
;; Chain operations on optional values, handling Nothing cases automatically.
(defvar hydra_lib_maybes_bind
  (lambda (m)
    (lambda (f)
      (if (maybe-nothing-p m)
          (list :nothing)
          (funcall f (maybe-value m))))))

;; cases :: Maybe a -> b -> (a -> b) -> b
;; Handle an optional value with the maybe value as the first argument.
;; Thunk-aware: if def is a zero-arg function (thunk), only called when Maybe is Nothing.
(defvar hydra_lib_maybes_cases
  (lambda (m)
    (lambda (def)
      (lambda (f)
        (if (maybe-nothing-p m)
            (if (functionp def) (funcall def) def)
            (funcall f (maybe-value m)))))))

;; cat :: [Maybe a] -> [a]
;; Filter out Nothing values from a list.
(defvar hydra_lib_maybes_cat
  (lambda (ms)
    (loop for m in ms
          unless (maybe-nothing-p m)
          collect (maybe-value m))))

;; compose :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
;; Compose two Maybe-returning functions (Kleisli composition).
(defvar hydra_lib_maybes_compose
  (lambda (f)
    (lambda (g)
      (lambda (x)
        (let ((result (funcall f x)))
          (if (maybe-nothing-p result)
              (list :nothing)
              (funcall g (maybe-value result))))))))

;; from_just :: Maybe a -> a
;; Extract value from a Just, or error on Nothing (partial function).
(defvar hydra_lib_maybes_from_just
  (lambda (m)
    (if (maybe-nothing-p m)
        (error "fromJust: Nothing")
        (maybe-value m))))

;; from_maybe :: a -> Maybe a -> a
;; Get a value from an optional value, or return a default value.
;; Thunk-aware: if def is a zero-arg function (thunk), only called when Maybe is Nothing.
(defvar hydra_lib_maybes_from_maybe
  (lambda (def)
    (lambda (m)
      (if (maybe-nothing-p m)
          (if (functionp def) (funcall def) def)
          (maybe-value m)))))

;; is_just :: Maybe a -> Bool
;; Check if a value is Just.
(defvar hydra_lib_maybes_is_just
  (lambda (m)
    (not (maybe-nothing-p m))))

;; is_nothing :: Maybe a -> Bool
;; Check if a value is Nothing.
(defvar hydra_lib_maybes_is_nothing
  (lambda (m)
    (maybe-nothing-p m)))

;; map :: (a -> b) -> Maybe a -> Maybe b
;; Map a function over an optional value.
(defvar hydra_lib_maybes_map
  (lambda (f)
    (lambda (m)
      (if (maybe-nothing-p m)
          (list :nothing)
          (list :just (funcall f (maybe-value m)))))))

;; map_maybe :: (a -> Maybe b) -> [a] -> [b]
;; Map a function over a list and collect Just results.
(defvar hydra_lib_maybes_map_maybe
  (lambda (f)
    (lambda (xs)
      (loop for x in xs
            for result = (funcall f x)
            unless (maybe-nothing-p result)
            collect (maybe-value result)))))

;; maybe :: b -> (a -> b) -> Maybe a -> b
;; Eliminate an optional value with a default and a function.
;; Thunk-aware: if def is a zero-arg function (thunk), only called when Maybe is Nothing.
(defvar hydra_lib_maybes_maybe
  (lambda (def)
    (lambda (f)
      (lambda (m)
        (if (maybe-nothing-p m)
            (if (functionp def) (funcall def) def)
            (funcall f (maybe-value m)))))))

;; pure :: a -> Maybe a
;; Lift a value into the Maybe type.
(defvar hydra_lib_maybes_pure
  (lambda (x)
    (list :just x)))

;; to_list :: Maybe a -> [a]
;; Convert a Maybe to a list: Just x becomes [x], Nothing becomes [].
(defvar hydra_lib_maybes_to_list
  (lambda (m)
    (if (maybe-nothing-p m)
        nil
        (list (maybe-value m)))))
