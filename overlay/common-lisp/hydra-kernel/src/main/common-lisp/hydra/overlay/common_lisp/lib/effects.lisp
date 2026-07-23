(in-package :cl-user)

;; Common Lisp implementations of hydra.lib.effects primitives (#494).
;;
;; The Hydra type effect<t> is transparent in the Lisp dialects (effect<t> = t): there is no
;; TypeVariantEffect in the target, so effectful programs are ordinary eager native code and
;; "running the effect" simply means forcing the value. These primitives therefore reduce to
;; ordinary applications. See the Haskell reference implementation in Hydra.Haskell.Lib.Effects
;; (where effect<t> = IO t). All functions are curried, matching the CL prim runtime style.
;;
;; Optional representation (CL target): (list :given val) or (list :none); a bare value is also
;; tolerated as a present optional (term-level maybe), mirroring the optionals/eithers runtimes.

;; apply :: effect<x -> y> -> effect<x> -> effect<y>
(defvar hydra_overlay_common_lisp_lib_effects_apply
  (lambda (ef)
    (lambda (ex)
      (funcall ef ex))))

;; bind :: effect<x> -> (x -> effect<y>) -> effect<y>
(defvar hydra_overlay_common_lisp_lib_effects_bind
  (lambda (a)
    (lambda (f)
      (funcall f a))))

;; compose :: (x -> effect<y>) -> (y -> effect<z>) -> x -> effect<z>
(defvar hydra_overlay_common_lisp_lib_effects_compose
  (lambda (f)
    (lambda (g)
      (lambda (x)
        (funcall g (funcall f x))))))

;; foldList :: (x -> y -> effect<x>) -> x -> [y] -> effect<x>
(defvar hydra_overlay_common_lisp_lib_effects_fold_list
  (lambda (f)
    (lambda (acc)
      (lambda (xs)
        (let ((a acc))
          (dolist (x xs a)
            (setf a (funcall (funcall f a) x))))))))

;; map :: (x -> y) -> effect<x> -> effect<y>
(defvar hydra_overlay_common_lisp_lib_effects_map
  (lambda (f)
    (lambda (a)
      (funcall f a))))

;; mapList :: (x -> effect<y>) -> [x] -> effect<[y]>
(defvar hydra_overlay_common_lisp_lib_effects_map_list
  (lambda (f)
    (lambda (xs)
      (mapcar f xs))))

;; mapOptional :: (x -> effect<y>) -> Maybe x -> effect<Maybe y>
(defvar hydra_overlay_common_lisp_lib_effects_map_optional
  (lambda (f)
    (lambda (m)
      (cond
        ((or (null m)
             (and (consp m) (eq (first m) :none)))
         (list :none))
        ((and (consp m) (eq (first m) :given))
         (list :given (funcall f (second m))))
        ;; bare value (term-level maybe)
        (t
         (list :given (funcall f m)))))))

;; pure :: x -> effect<x>
(defvar hydra_overlay_common_lisp_lib_effects_pure
  (lambda (x)
    x))
