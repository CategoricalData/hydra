;;; Hydra Common Lisp standard library registration
;;;
;;; Registers all primitive functions (chars, equality, eithers, lists, literals,
;;; logic, maps, math, optionals, pairs, sets, strings, annotations) for the
;;; generated reducer.  Direct translation of Clojure hydra/lib/libraries.clj.

(in-package :cl-user)

;; ============================================================================
;; Helpers
;; ============================================================================

;; #473: primitive names are derived from the generated hydra.lib.* PrimitiveDefinition def-modules
;; (the single source of truth). Those def-modules are rewriting-style generated code, so they must be
;; loaded via hydra-load-file (not plain cl:load), and they depend on the kernel (hydra.packaging /
;; hydra.typing) which is already loaded by the time this registry loads. We load them here, before the
;; register-* functions below reference their def vars. (find-package guards against a double-load.)
;; Always load (don't guard on find-package): consumer modules' (:use :hydra.lib.<sub>) clauses
;; auto-vivify an EMPTY :hydra.lib.<sub> package during gen-main load, so a find-package guard would
;; skip the real def-module and leave the def vars unbound. hydra-load-file populating an existing
;; (empty) package is fine.
(dolist (sub '("chars" "effects" "eithers" "equality" "files" "lists" "literals" "logic" "maps"
               "math" "optionals" "pairs" "regex" "sets" "strings" "text"))
  (hydra-load-file (merge-pathnames (concatenate 'string "lib/" sub ".lisp")
                                    *hydra-gen-main-dir*)))

(defun prim-name (def)
  "Derive a primitive's canonical name from its generated hydra.lib.* PrimitiveDefinition (#473).
   The def var is loaded into :cl-user by the dolist above (hydra-load-file flattens generated
   modules into :cl-user; defpackage/in-package forms are skipped by the loader), so callers pass the
   bare def var, e.g. (prim-name hydra_lib_chars_is_alpha_num)."
  (hydra_packaging_primitive_definition-name def))

(defun fun (dom cod)
  "A TermCoder for function types using beta reduction to bridge
   term-level functions to native functions."
  (tc-function-with-reduce
    (lambda (cx g t_)
      (funcall (funcall (funcall (funcall hydra_reduction_reduce_term cx) g) cl:t) t_))
    dom cod))

;; ============================================================================
;; Chars
;; ============================================================================

(defun register-chars ()
  (let ()
    (list
      (cons (prim-name hydra_lib_chars_is_alpha_num) (prim1 (prim-name hydra_lib_chars_is_alpha_num) hydra_overlay_common_lisp_lib_chars_is_alpha_num nil (tc-int32) (tc-boolean)))
      (cons (prim-name hydra_lib_chars_is_lower)    (prim1 (prim-name hydra_lib_chars_is_lower)    hydra_overlay_common_lisp_lib_chars_is_lower    nil (tc-int32) (tc-boolean)))
      (cons (prim-name hydra_lib_chars_is_space)    (prim1 (prim-name hydra_lib_chars_is_space)    hydra_overlay_common_lisp_lib_chars_is_space    nil (tc-int32) (tc-boolean)))
      (cons (prim-name hydra_lib_chars_is_upper)    (prim1 (prim-name hydra_lib_chars_is_upper)    hydra_overlay_common_lisp_lib_chars_is_upper    nil (tc-int32) (tc-boolean)))
      (cons (prim-name hydra_lib_chars_to_lower)    (prim1 (prim-name hydra_lib_chars_to_lower)    hydra_overlay_common_lisp_lib_chars_to_lower    nil (tc-int32) (tc-int32)))
      (cons (prim-name hydra_lib_chars_to_upper)    (prim1 (prim-name hydra_lib_chars_to_upper)    hydra_overlay_common_lisp_lib_chars_to_upper    nil (tc-int32) (tc-int32))))))

;; ============================================================================
;; Effects (#494)
;; ============================================================================
;;
;; effect<t> is transparent in Common Lisp (effect<t> = t). These are registered so the inference
;; graph can resolve the hydra.lib.effects.* names; their type schemes match the kernel signatures
;; exactly (note pure is x -> effect<x>, including the function arrow). The real evaluation happens
;; through the relocated hydra.common_lisp.lib.effects runtime (flat hydra_overlay_common_lisp_lib_effects_*),
;; reached via the bootstrap redirect.

(defun register-effects ()
  (let (
        (x (tc-variable "x"))
        (y (tc-variable "y"))
        (z (tc-variable "z")))
    (let ((eff (lambda (c) (tc-effect c))))
      (list
        (cons (prim-name hydra_lib_effects_apply)        (prim2 (prim-name hydra_lib_effects_apply)
                                                   hydra_overlay_common_lisp_lib_effects_apply
                                                   '("x" "y") (funcall eff (tc-function x y)) (funcall eff x) (funcall eff y)))
        (cons (prim-name hydra_lib_effects_bind)         (prim2 (prim-name hydra_lib_effects_bind)
                                                   hydra_overlay_common_lisp_lib_effects_bind
                                                   '("x" "y") (funcall eff x) (tc-function x (funcall eff y)) (funcall eff y)))
        (cons (prim-name hydra_lib_effects_compose)      (prim3 (prim-name hydra_lib_effects_compose)
                                                   hydra_overlay_common_lisp_lib_effects_compose
                                                   '("x" "y" "z") (tc-function x (funcall eff y)) (tc-function y (funcall eff z)) x (funcall eff z)))
        (cons (prim-name hydra_lib_effects_foldl)        (prim3 (prim-name hydra_lib_effects_foldl)
                                                   hydra_overlay_common_lisp_lib_effects_foldl
                                                   '("x" "y") (tc-function x (tc-function y (funcall eff x))) x (tc-list y) (funcall eff x)))
        (cons (prim-name hydra_lib_effects_map)          (prim2 (prim-name hydra_lib_effects_map)
                                                   hydra_overlay_common_lisp_lib_effects_map
                                                   '("x" "y") (tc-function x y) (funcall eff x) (funcall eff y)))
        (cons (prim-name hydra_lib_effects_map_list)     (prim2 (prim-name hydra_lib_effects_map_list)
                                                   hydra_overlay_common_lisp_lib_effects_map_list
                                                   '("x" "y") (tc-function x (funcall eff y)) (tc-list x) (funcall eff (tc-list y))))
        (cons (prim-name hydra_lib_effects_map_optional) (prim2 (prim-name hydra_lib_effects_map_optional)
                                                   hydra_overlay_common_lisp_lib_effects_map_optional
                                                   '("x" "y") (tc-function x (funcall eff y)) (tc-optional x) (funcall eff (tc-optional y))))
        (cons (prim-name hydra_lib_effects_pure)         (prim1 (prim-name hydra_lib_effects_pure)
                                                   hydra_overlay_common_lisp_lib_effects_pure
                                                   '("x") x (funcall eff x)))))))

;; ============================================================================
;; Eithers
;; ============================================================================

(defun register-eithers ()
  (let (
        (x (tc-variable "x"))
        (y (tc-variable "y"))
        (z (tc-variable "z"))
        (w (tc-variable "w")))
    (list
      (cons (prim-name hydra_lib_eithers_bind)    (prim2 (prim-name hydra_lib_eithers_bind)
                                          hydra_overlay_common_lisp_lib_eithers_bind
                                          nil (tc-either x y) (fun y (tc-either x z)) (tc-either x z)))
      (cons (prim-name hydra_lib_eithers_bimap)   (prim3 (prim-name hydra_lib_eithers_bimap)
                                          hydra_overlay_common_lisp_lib_eithers_bimap
                                          nil (fun x z) (fun y w) (tc-either x y) (tc-either z w)))
      (cons (prim-name hydra_lib_eithers_either)  (prim3 (prim-name hydra_lib_eithers_either)
                                          hydra_overlay_common_lisp_lib_eithers_either
                                          nil (fun x z) (fun y z) (tc-either x y) z))
      (cons (prim-name hydra_lib_eithers_foldl)   (prim3 (prim-name hydra_lib_eithers_foldl)
                                          hydra_overlay_common_lisp_lib_eithers_foldl
                                          nil (fun x (fun y (tc-either z x))) x (tc-list y) (tc-either z x)))
      (cons (prim-name hydra_lib_eithers_from_left)  (lazy-args '(0) (prim2 (prim-name hydra_lib_eithers_from_left)
                                            hydra_overlay_common_lisp_lib_eithers_from_left
                                            nil x (tc-either x y) x)))
      (cons (prim-name hydra_lib_eithers_from_right) (lazy-args '(0) (prim2 (prim-name hydra_lib_eithers_from_right)
                                            hydra_overlay_common_lisp_lib_eithers_from_right
                                            nil y (tc-either x y) y)))
      (cons (prim-name hydra_lib_eithers_is_left)  (prim1 (prim-name hydra_lib_eithers_is_left)  hydra_overlay_common_lisp_lib_eithers_is_left  nil (tc-either x y) (tc-boolean)))
      (cons (prim-name hydra_lib_eithers_is_right) (prim1 (prim-name hydra_lib_eithers_is_right) hydra_overlay_common_lisp_lib_eithers_is_right nil (tc-either x y) (tc-boolean)))
      (cons (prim-name hydra_lib_eithers_lefts)   (prim1 (prim-name hydra_lib_eithers_lefts)   hydra_overlay_common_lisp_lib_eithers_lefts   nil (tc-list (tc-either x y)) (tc-list x)))
      (cons (prim-name hydra_lib_eithers_map)     (prim2 (prim-name hydra_lib_eithers_map)
                                          hydra_overlay_common_lisp_lib_eithers_map
                                          nil (fun x y) (tc-either z x) (tc-either z y)))
      (cons (prim-name hydra_lib_eithers_map_list) (prim2 (prim-name hydra_lib_eithers_map_list)
                                          hydra_overlay_common_lisp_lib_eithers_map_list
                                          nil (fun x (tc-either z y)) (tc-list x) (tc-either z (tc-list y))))
      (cons (prim-name hydra_lib_eithers_map_optional) (prim2 (prim-name hydra_lib_eithers_map_optional)
                                           hydra_overlay_common_lisp_lib_eithers_map_optional
                                           nil (fun x (tc-either z y)) (tc-optional x) (tc-either z (tc-optional y))))
      (cons (prim-name hydra_lib_eithers_map_set)  (prim2 (prim-name hydra_lib_eithers_map_set)
                                          hydra_overlay_common_lisp_lib_eithers_map_set
                                          nil (fun x (tc-either z y)) (tc-set x) (tc-either z (tc-set y))))
      (cons (prim-name hydra_lib_eithers_partition_eithers) (prim1 (prim-name hydra_lib_eithers_partition_eithers)
                                                   hydra_overlay_common_lisp_lib_eithers_partition_eithers
                                                   nil (tc-list (tc-either x y)) (tc-pair (tc-list x) (tc-list y))))
      (cons (prim-name hydra_lib_eithers_rights)  (prim1 (prim-name hydra_lib_eithers_rights)  hydra_overlay_common_lisp_lib_eithers_rights  nil (tc-list (tc-either x y)) (tc-list y))))))

;; ============================================================================
;; Equality
;; ============================================================================

(defun register-equality ()
  (let (
        (x (tc-variable "x"))
        (ord-x '(("x" . ("ordering"))))
        (eq-x '(("x" . ("equality")))))
    (list
      (cons (prim-name hydra_lib_equality_compare)  (prim2 (prim-name hydra_lib_equality_compare)  hydra_overlay_common_lisp_lib_equality_compare  nil x x (tc-comparison) ord-x))
      (cons (prim-name hydra_lib_equality_equal)    (prim2 (prim-name hydra_lib_equality_equal)    hydra_overlay_common_lisp_lib_equality_equal    nil x x (tc-boolean) eq-x))
      (cons (prim-name hydra_lib_equality_gt)       (prim2 (prim-name hydra_lib_equality_gt)       hydra_overlay_common_lisp_lib_equality_gt       nil x x (tc-boolean) ord-x))
      (cons (prim-name hydra_lib_equality_gte)      (prim2 (prim-name hydra_lib_equality_gte)      hydra_overlay_common_lisp_lib_equality_gte      nil x x (tc-boolean) ord-x))
      (cons (prim-name hydra_lib_equality_identity) (prim1 (prim-name hydra_lib_equality_identity) #'identity                 nil x x))
      (cons (prim-name hydra_lib_equality_lt)       (prim2 (prim-name hydra_lib_equality_lt)       hydra_overlay_common_lisp_lib_equality_lt       nil x x (tc-boolean) ord-x))
      (cons (prim-name hydra_lib_equality_lte)      (prim2 (prim-name hydra_lib_equality_lte)      hydra_overlay_common_lisp_lib_equality_lte      nil x x (tc-boolean) ord-x))
      (cons (prim-name hydra_lib_equality_max)      (prim2 (prim-name hydra_lib_equality_max)      hydra_overlay_common_lisp_lib_equality_max      nil x x x ord-x))
      (cons (prim-name hydra_lib_equality_min)      (prim2 (prim-name hydra_lib_equality_min)      hydra_overlay_common_lisp_lib_equality_min      nil x x x ord-x)))))

;; ============================================================================
;; Files (#494)
;; ============================================================================
;;
;; FilePath and FileError are nominal kernel types (referenced by name). unit maps to nil, binary
;; to a byte vector, either to the (list :left/:right) representation. As with effects, the type
;; schemes are registered for inference name-resolution; the real I/O happens in the relocated
;; hydra.common_lisp.lib.files runtime (flat hydra_overlay_common_lisp_lib_files_*), reached via the redirect.

(defun register-files ()
  (let (
        (bool (tc-boolean))
        (bin (tc-binary))
        (fp (tc-named "hydra.file.FilePath"))
        (ferr (tc-named "hydra.error.file.FileError"))
        (unit (tc-unit)))
    (let ((eff (lambda (c) (tc-effect c))))
      (list
        (cons (prim-name hydra_lib_files_append_file)     (prim2 (prim-name hydra_lib_files_append_file)
                                                   hydra_overlay_common_lisp_lib_files_append_file
                                                   nil fp bin (funcall eff (tc-either ferr unit))))
        (cons (prim-name hydra_lib_files_create_directory) (prim2 (prim-name hydra_lib_files_create_directory)
                                                   hydra_overlay_common_lisp_lib_files_create_directory
                                                   nil bool fp (funcall eff (tc-either ferr unit))))
        (cons (prim-name hydra_lib_files_exists)          (prim1 (prim-name hydra_lib_files_exists)
                                                   hydra_overlay_common_lisp_lib_files_exists
                                                   nil fp (funcall eff (tc-either ferr bool))))
        (cons (prim-name hydra_lib_files_list_directory)  (prim1 (prim-name hydra_lib_files_list_directory)
                                                   hydra_overlay_common_lisp_lib_files_list_directory
                                                   nil fp (funcall eff (tc-either ferr (tc-list fp)))))
        (cons (prim-name hydra_lib_files_read_file)       (prim1 (prim-name hydra_lib_files_read_file)
                                                   hydra_overlay_common_lisp_lib_files_read_file
                                                   nil fp (funcall eff (tc-either ferr bin))))
        (cons (prim-name hydra_lib_files_remove_file)     (prim1 (prim-name hydra_lib_files_remove_file)
                                                   hydra_overlay_common_lisp_lib_files_remove_file
                                                   nil fp (funcall eff (tc-either ferr unit))))
        (cons (prim-name hydra_lib_files_rename)          (prim2 (prim-name hydra_lib_files_rename)
                                                   hydra_overlay_common_lisp_lib_files_rename
                                                   nil fp fp (funcall eff (tc-either ferr unit))))
        (cons (prim-name hydra_lib_files_write_file)      (prim2 (prim-name hydra_lib_files_write_file)
                                                   hydra_overlay_common_lisp_lib_files_write_file
                                                   nil fp bin (funcall eff (tc-either ferr unit))))))))

;; ============================================================================
;; Lists
;; ============================================================================

(defun register-lists ()
  (let (
        (a (tc-variable "a"))
        (b (tc-variable "b"))
        (c (tc-variable "c")))
    (list
      (cons (prim-name hydra_lib_lists_apply)      (prim2 (prim-name hydra_lib_lists_apply)
                                              hydra_overlay_common_lisp_lib_lists_apply
                                              nil (tc-list (fun a b)) (tc-list a) (tc-list b)))
      (cons (prim-name hydra_lib_lists_bind)       (prim2 (prim-name hydra_lib_lists_bind)
                                              hydra_overlay_common_lisp_lib_lists_bind
                                              nil (tc-list a) (fun a (tc-list b)) (tc-list b)))
      (cons (prim-name hydra_lib_lists_concat)     (prim1 (prim-name hydra_lib_lists_concat)     hydra_overlay_common_lisp_lib_lists_concat     nil (tc-list (tc-list a)) (tc-list a)))
      (cons (prim-name hydra_lib_lists_concat2)    (prim2 (prim-name hydra_lib_lists_concat2)
                                              hydra_overlay_common_lisp_lib_lists_concat2
                                              nil (tc-list a) (tc-list a) (tc-list a)))
      (cons (prim-name hydra_lib_lists_cons)       (prim2 (prim-name hydra_lib_lists_cons)
                                              hydra_overlay_common_lisp_lib_lists_cons
                                              nil a (tc-list a) (tc-list a)))
      (cons (prim-name hydra_lib_lists_drop)       (prim2 (prim-name hydra_lib_lists_drop)
                                              hydra_overlay_common_lisp_lib_lists_drop
                                              nil (tc-int32) (tc-list a) (tc-list a)))
      (cons (prim-name hydra_lib_lists_drop_while)  (prim2 (prim-name hydra_lib_lists_drop_while)
                                              hydra_overlay_common_lisp_lib_lists_drop_while
                                              nil (fun a (tc-boolean)) (tc-list a) (tc-list a)))
      (cons (prim-name hydra_lib_lists_elem)       (prim2 (prim-name hydra_lib_lists_elem)
                                              hydra_overlay_common_lisp_lib_lists_elem
                                              nil a (tc-list a) (tc-boolean) '(("a" . ("equality")))))
      (cons (prim-name hydra_lib_lists_filter)     (prim2 (prim-name hydra_lib_lists_filter)
                                              hydra_overlay_common_lisp_lib_lists_filter
                                              nil (fun a (tc-boolean)) (tc-list a) (tc-list a)))
      (cons (prim-name hydra_lib_lists_find)       (prim2 (prim-name hydra_lib_lists_find)
                                              hydra_overlay_common_lisp_lib_lists_find
                                              nil (fun a (tc-boolean)) (tc-list a) (tc-optional a)))
      (cons (prim-name hydra_lib_lists_foldl)      (prim3 (prim-name hydra_lib_lists_foldl)
                                              (lambda (f)
                                                (lambda (init)
                                                  (lambda (xs)
                                                    (funcall (funcall (funcall hydra_overlay_common_lisp_lib_lists_foldl
                                                                               (lambda (acc) (lambda (el) (funcall (funcall f acc) el))))
                                                                      init) xs))))
                                              nil (fun b (fun a b)) b (tc-list a) b))
      (cons (prim-name hydra_lib_lists_foldr)      (prim3 (prim-name hydra_lib_lists_foldr)
                                              (lambda (f)
                                                (lambda (init)
                                                  (lambda (xs)
                                                    (funcall (funcall (funcall hydra_overlay_common_lisp_lib_lists_foldr
                                                                               (lambda (el) (lambda (acc) (funcall (funcall f el) acc))))
                                                                      init) xs))))
                                              nil (fun a (fun b b)) b (tc-list a) b))
      (cons (prim-name hydra_lib_lists_group)      (prim1 (prim-name hydra_lib_lists_group)      hydra_overlay_common_lisp_lib_lists_group      nil (tc-list a) (tc-list (tc-list a)) '(("a" . ("equality")))))
      (cons (prim-name hydra_lib_lists_intercalate) (prim2 (prim-name hydra_lib_lists_intercalate)
                                               hydra_overlay_common_lisp_lib_lists_intercalate
                                               nil (tc-list a) (tc-list (tc-list a)) (tc-list a)))
      (cons (prim-name hydra_lib_lists_intersperse) (prim2 (prim-name hydra_lib_lists_intersperse)
                                               hydra_overlay_common_lisp_lib_lists_intersperse
                                               nil a (tc-list a) (tc-list a)))
      (cons (prim-name hydra_lib_lists_length)     (prim1 (prim-name hydra_lib_lists_length)     hydra_overlay_common_lisp_lib_lists_length     nil (tc-list a) (tc-int32)))
      (cons (prim-name hydra_lib_lists_map)        (prim2 (prim-name hydra_lib_lists_map)
                                              hydra_overlay_common_lisp_lib_lists_map
                                              nil (fun a b) (tc-list a) (tc-list b)))
      (cons (prim-name hydra_lib_lists_maybe_at)    (prim2 (prim-name hydra_lib_lists_maybe_at)    hydra_overlay_common_lisp_lib_lists_maybe_at   nil (tc-int32) (tc-list a) (tc-optional a)))
      (cons (prim-name hydra_lib_lists_maybe_head)  (prim1 (prim-name hydra_lib_lists_maybe_head)  hydra_overlay_common_lisp_lib_lists_maybe_head nil (tc-list a) (tc-optional a)))
      (cons (prim-name hydra_lib_lists_maybe_init)  (prim1 (prim-name hydra_lib_lists_maybe_init)  hydra_overlay_common_lisp_lib_lists_maybe_init nil (tc-list a) (tc-optional (tc-list a))))
      (cons (prim-name hydra_lib_lists_maybe_last)  (prim1 (prim-name hydra_lib_lists_maybe_last)  hydra_overlay_common_lisp_lib_lists_maybe_last nil (tc-list a) (tc-optional a)))
      (cons (prim-name hydra_lib_lists_maybe_tail)  (prim1 (prim-name hydra_lib_lists_maybe_tail)  hydra_overlay_common_lisp_lib_lists_maybe_tail nil (tc-list a) (tc-optional (tc-list a))))
      (cons (prim-name hydra_lib_lists_nub)        (prim1 (prim-name hydra_lib_lists_nub)        hydra_overlay_common_lisp_lib_lists_nub        nil (tc-list a) (tc-list a) '(("a" . ("equality")))))
      (cons (prim-name hydra_lib_lists_null)       (prim1 (prim-name hydra_lib_lists_null)       hydra_overlay_common_lisp_lib_lists_null       nil (tc-list a) (tc-boolean)))
      (cons (prim-name hydra_lib_lists_partition)   (prim2 (prim-name hydra_lib_lists_partition)
                                               hydra_overlay_common_lisp_lib_lists_partition
                                               nil (fun a (tc-boolean)) (tc-list a) (tc-pair (tc-list a) (tc-list a))))
      (cons (prim-name hydra_lib_lists_pure)       (prim1 (prim-name hydra_lib_lists_pure)       hydra_overlay_common_lisp_lib_lists_pure       nil a (tc-list a)))
      (cons (prim-name hydra_lib_lists_replicate)  (prim2 (prim-name hydra_lib_lists_replicate)
                                              hydra_overlay_common_lisp_lib_lists_replicate
                                              nil (tc-int32) a (tc-list a)))
      (cons (prim-name hydra_lib_lists_reverse)    (prim1 (prim-name hydra_lib_lists_reverse)    hydra_overlay_common_lisp_lib_lists_reverse    nil (tc-list a) (tc-list a)))
      (cons (prim-name hydra_lib_lists_singleton)  (prim1 (prim-name hydra_lib_lists_singleton)  hydra_overlay_common_lisp_lib_lists_singleton  nil a (tc-list a)))
      (cons (prim-name hydra_lib_lists_sort)       (prim1 (prim-name hydra_lib_lists_sort)       hydra_overlay_common_lisp_lib_lists_sort       nil (tc-list a) (tc-list a) '(("a" . ("ordering")))))
      (cons (prim-name hydra_lib_lists_sort_on)     (prim2 (prim-name hydra_lib_lists_sort_on)
                                              hydra_overlay_common_lisp_lib_lists_sort_on
                                              nil (fun a b) (tc-list a) (tc-list a)))
      (cons (prim-name hydra_lib_lists_span)       (prim2 (prim-name hydra_lib_lists_span)
                                              hydra_overlay_common_lisp_lib_lists_span
                                              nil (fun a (tc-boolean)) (tc-list a) (tc-pair (tc-list a) (tc-list a))))
      (cons (prim-name hydra_lib_lists_take)       (prim2 (prim-name hydra_lib_lists_take)
                                              hydra_overlay_common_lisp_lib_lists_take
                                              nil (tc-int32) (tc-list a) (tc-list a)))
      (cons (prim-name hydra_lib_lists_transpose)  (prim1 (prim-name hydra_lib_lists_transpose)  hydra_overlay_common_lisp_lib_lists_transpose  nil (tc-list (tc-list a)) (tc-list (tc-list a))))
      (cons (prim-name hydra_lib_lists_uncons)     (prim1 (prim-name hydra_lib_lists_uncons)     hydra_overlay_common_lisp_lib_lists_uncons     nil (tc-list a) (tc-optional (tc-pair a (tc-list a)))))
      (cons (prim-name hydra_lib_lists_zip)        (prim2 (prim-name hydra_lib_lists_zip)
                                              hydra_overlay_common_lisp_lib_lists_zip
                                              nil (tc-list a) (tc-list b) (tc-list (tc-pair a b))))
      (cons (prim-name hydra_lib_lists_zip_with)    (prim3 (prim-name hydra_lib_lists_zip_with)
                                              (lambda (f)
                                                (lambda (xs)
                                                  (lambda (ys)
                                                    (funcall (funcall (funcall hydra_overlay_common_lisp_lib_lists_zip_with
                                                                               (lambda (a) (lambda (b) (funcall (funcall f a) b))))
                                                                      xs) ys))))
                                              nil (fun a (fun b c)) (tc-list a) (tc-list b) (tc-list c))))))

;; ============================================================================
;; Logic
;; ============================================================================

(defun register-logic ()
  (let (
        (a (tc-variable "a")))
    (list
      (cons (prim-name hydra_lib_logic_and)    (prim2 (prim-name hydra_lib_logic_and)
                                         hydra_overlay_common_lisp_lib_logic_and
                                         nil (tc-boolean) (tc-boolean) (tc-boolean)))
      (cons (prim-name hydra_lib_logic_if_else) (lazy-args '(1 2) (prim3 (prim-name hydra_lib_logic_if_else)
                                         hydra_overlay_common_lisp_lib_logic_if_else
                                         nil (tc-boolean) a a a)))
      (cons (prim-name hydra_lib_logic_not)    (prim1 (prim-name hydra_lib_logic_not)    hydra_overlay_common_lisp_lib_logic_not nil (tc-boolean) (tc-boolean)))
      (cons (prim-name hydra_lib_logic_or)     (prim2 (prim-name hydra_lib_logic_or)
                                         hydra_overlay_common_lisp_lib_logic_or
                                         nil (tc-boolean) (tc-boolean) (tc-boolean))))))

;; ============================================================================
;; Maps
;; ============================================================================

(defun register-maps ()
  (let (
        (k  (tc-variable "k"))
        (k1 (tc-variable "k1"))
        (k2 (tc-variable "k2"))
        (v  (tc-variable "v"))
        (v1 (tc-variable "v1"))
        (v2 (tc-variable "v2"))
        (ord-k '(("k" . ("ordering"))))
        (ord-k1k2 '(("k1" . ("ordering")) ("k2" . ("ordering")))))
    (let ((map-kv (tc-map k v)))
      (list
        (cons (prim-name hydra_lib_maps_alter)          (prim3 (prim-name hydra_lib_maps_alter)
                                                   hydra_overlay_common_lisp_lib_maps_alter
                                                   nil (fun (tc-optional v) (tc-optional v)) k map-kv map-kv ord-k))
        (cons (prim-name hydra_lib_maps_bimap)          (prim3 (prim-name hydra_lib_maps_bimap)
                                                   hydra_overlay_common_lisp_lib_maps_bimap
                                                   nil (fun k1 k2) (fun v1 v2) (tc-map k1 v1) (tc-map k2 v2) ord-k1k2))
        (cons (prim-name hydra_lib_maps_delete)         (prim2 (prim-name hydra_lib_maps_delete)
                                                   hydra_overlay_common_lisp_lib_maps_delete
                                                   nil k map-kv map-kv ord-k))
        (cons (prim-name hydra_lib_maps_elems)          (prim1 (prim-name hydra_lib_maps_elems)  hydra_overlay_common_lisp_lib_maps_elems  nil map-kv (tc-list v) ord-k))
        (cons (prim-name hydra_lib_maps_empty)          (prim0 (prim-name hydra_lib_maps_empty)  (lambda () hydra_overlay_common_lisp_lib_maps_empty)  nil map-kv ord-k))
        (cons (prim-name hydra_lib_maps_filter)         (prim2 (prim-name hydra_lib_maps_filter)
                                                   hydra_overlay_common_lisp_lib_maps_filter
                                                   nil (fun v (tc-boolean)) map-kv map-kv ord-k))
        (cons (prim-name hydra_lib_maps_filter_with_key)  (prim2 (prim-name hydra_lib_maps_filter_with_key)
                                                   hydra_overlay_common_lisp_lib_maps_filter_with_key
                                                   nil (fun k (fun v (tc-boolean))) map-kv map-kv ord-k))
        (cons (prim-name hydra_lib_maps_find_with_default) (lazy-args '(0) (prim3 (prim-name hydra_lib_maps_find_with_default)
                                                    hydra_overlay_common_lisp_lib_maps_find_with_default
                                                    nil v k map-kv v ord-k)))
        (cons (prim-name hydra_lib_maps_from_list)       (prim1 (prim-name hydra_lib_maps_from_list) hydra_overlay_common_lisp_lib_maps_from_list nil (tc-list (tc-pair k v)) map-kv ord-k))
        (cons (prim-name hydra_lib_maps_insert)         (prim3 (prim-name hydra_lib_maps_insert)
                                                   hydra_overlay_common_lisp_lib_maps_insert
                                                   nil k v map-kv map-kv ord-k))
        (cons (prim-name hydra_lib_maps_keys)           (prim1 (prim-name hydra_lib_maps_keys)   hydra_overlay_common_lisp_lib_maps_keys   nil map-kv (tc-list k) ord-k))
        (cons (prim-name hydra_lib_maps_lookup)         (prim2 (prim-name hydra_lib_maps_lookup)
                                                   hydra_overlay_common_lisp_lib_maps_lookup
                                                   nil k map-kv (tc-optional v) ord-k))
        (cons (prim-name hydra_lib_maps_map)            (prim2 (prim-name hydra_lib_maps_map)
                                                   hydra_overlay_common_lisp_lib_maps_map
                                                   nil (fun v1 v2) (tc-map k v1) (tc-map k v2) ord-k))
        (cons (prim-name hydra_lib_maps_map_keys)        (prim2 (prim-name hydra_lib_maps_map_keys)
                                                   hydra_overlay_common_lisp_lib_maps_map_keys
                                                   nil (fun k1 k2) (tc-map k1 v) (tc-map k2 v) ord-k1k2))
        (cons (prim-name hydra_lib_maps_member)         (prim2 (prim-name hydra_lib_maps_member)
                                                   hydra_overlay_common_lisp_lib_maps_member
                                                   nil k map-kv (tc-boolean) ord-k))
        (cons (prim-name hydra_lib_maps_null)           (prim1 (prim-name hydra_lib_maps_null)   hydra_overlay_common_lisp_lib_maps_null   nil map-kv (tc-boolean) ord-k))
        (cons (prim-name hydra_lib_maps_singleton)      (prim2 (prim-name hydra_lib_maps_singleton)
                                                   hydra_overlay_common_lisp_lib_maps_singleton
                                                   nil k v map-kv ord-k))
        (cons (prim-name hydra_lib_maps_size)           (prim1 (prim-name hydra_lib_maps_size)   hydra_overlay_common_lisp_lib_maps_size   nil map-kv (tc-int32) ord-k))
        (cons (prim-name hydra_lib_maps_to_list)         (prim1 (prim-name hydra_lib_maps_to_list) hydra_overlay_common_lisp_lib_maps_to_list nil map-kv (tc-list (tc-pair k v)) ord-k))
        (cons (prim-name hydra_lib_maps_union)          (prim2 (prim-name hydra_lib_maps_union)
                                                   hydra_overlay_common_lisp_lib_maps_union
                                                   nil map-kv map-kv map-kv ord-k))))))

;; ============================================================================
;; Math
;; ============================================================================

(defun register-math ()
  (let (
        (i32 (tc-int32))
        (f32 (tc-float32))
        (f64 (tc-float64))
        (bi  (tc-bigint))
        (b   (tc-boolean)))
    (append
      ;; Int32 primitives
      (list
        (cons (prim-name hydra_lib_math_abs)    (prim1 (prim-name hydra_lib_math_abs)    hydra_overlay_common_lisp_lib_math_abs    nil i32 i32))
        (cons (prim-name hydra_lib_math_add)    (prim2 (prim-name hydra_lib_math_add)    hydra_overlay_common_lisp_lib_math_add    nil i32 i32 i32))
        (cons (prim-name hydra_lib_math_even)   (prim1 (prim-name hydra_lib_math_even)   hydra_overlay_common_lisp_lib_math_even   nil i32 b))
        (cons (prim-name hydra_lib_math_mul)    (prim2 (prim-name hydra_lib_math_mul)    hydra_overlay_common_lisp_lib_math_mul    nil i32 i32 i32))
        (cons (prim-name hydra_lib_math_negate) (prim1 (prim-name hydra_lib_math_negate) hydra_overlay_common_lisp_lib_math_negate nil i32 i32))
        (cons (prim-name hydra_lib_math_odd)    (prim1 (prim-name hydra_lib_math_odd)    hydra_overlay_common_lisp_lib_math_odd    nil i32 b))
        (cons (prim-name hydra_lib_math_range)  (prim2 (prim-name hydra_lib_math_range)  hydra_overlay_common_lisp_lib_math_range  nil i32 i32 (tc-list i32)))
        (cons (prim-name hydra_lib_math_signum) (prim1 (prim-name hydra_lib_math_signum) hydra_overlay_common_lisp_lib_math_signum nil i32 i32))
        (cons (prim-name hydra_lib_math_sub)    (prim2 (prim-name hydra_lib_math_sub)    hydra_overlay_common_lisp_lib_math_sub    nil i32 i32 i32))
        (cons (prim-name hydra_lib_math_max)    (prim2 (prim-name hydra_lib_math_max)    hydra_overlay_common_lisp_lib_math_max    nil i32 i32 i32))
        (cons (prim-name hydra_lib_math_maybe_div)  (prim2 (prim-name hydra_lib_math_maybe_div)  hydra_overlay_common_lisp_lib_math_maybe_div  nil i32 i32 (tc-optional i32)))
        (cons (prim-name hydra_lib_math_maybe_mod)  (prim2 (prim-name hydra_lib_math_maybe_mod)  hydra_overlay_common_lisp_lib_math_maybe_mod  nil i32 i32 (tc-optional i32)))
        (cons (prim-name hydra_lib_math_maybe_pred) (prim1 (prim-name hydra_lib_math_maybe_pred) hydra_overlay_common_lisp_lib_math_maybe_pred nil i32 (tc-optional i32)))
        (cons (prim-name hydra_lib_math_maybe_rem)  (prim2 (prim-name hydra_lib_math_maybe_rem)  hydra_overlay_common_lisp_lib_math_maybe_rem  nil i32 i32 (tc-optional i32)))
        (cons (prim-name hydra_lib_math_maybe_succ) (prim1 (prim-name hydra_lib_math_maybe_succ) hydra_overlay_common_lisp_lib_math_maybe_succ nil i32 (tc-optional i32)))
        (cons (prim-name hydra_lib_math_min)    (prim2 (prim-name hydra_lib_math_min)    hydra_overlay_common_lisp_lib_math_min    nil i32 i32 i32)))
      ;; Float64 primitives
      (list
        (cons (prim-name hydra_lib_math_acos)     (prim1 (prim-name hydra_lib_math_acos)     hydra_overlay_common_lisp_lib_math_acos     nil f64 f64))
        (cons (prim-name hydra_lib_math_acosh)    (prim1 (prim-name hydra_lib_math_acosh)    hydra_overlay_common_lisp_lib_math_acosh    nil f64 f64))
        (cons (prim-name hydra_lib_math_add_float64) (prim2 (prim-name hydra_lib_math_add_float64) hydra_overlay_common_lisp_lib_math_add_float64 nil f64 f64 f64))
        (cons (prim-name hydra_lib_math_asin)     (prim1 (prim-name hydra_lib_math_asin)     hydra_overlay_common_lisp_lib_math_asin     nil f64 f64))
        (cons (prim-name hydra_lib_math_asinh)    (prim1 (prim-name hydra_lib_math_asinh)    hydra_overlay_common_lisp_lib_math_asinh    nil f64 f64))
        (cons (prim-name hydra_lib_math_atan)     (prim1 (prim-name hydra_lib_math_atan)     hydra_overlay_common_lisp_lib_math_atan     nil f64 f64))
        (cons (prim-name hydra_lib_math_atan2)    (prim2 (prim-name hydra_lib_math_atan2)    hydra_overlay_common_lisp_lib_math_atan2    nil f64 f64 f64))
        (cons (prim-name hydra_lib_math_atanh)    (prim1 (prim-name hydra_lib_math_atanh)    hydra_overlay_common_lisp_lib_math_atanh    nil f64 f64))
        (cons (prim-name hydra_lib_math_ceiling)  (prim1 (prim-name hydra_lib_math_ceiling)  hydra_overlay_common_lisp_lib_math_ceiling  nil f64 f64))
        (cons (prim-name hydra_lib_math_cos)      (prim1 (prim-name hydra_lib_math_cos)      hydra_overlay_common_lisp_lib_math_cos      nil f64 f64))
        (cons (prim-name hydra_lib_math_cosh)     (prim1 (prim-name hydra_lib_math_cosh)     hydra_overlay_common_lisp_lib_math_cosh     nil f64 f64))
        (cons (prim-name hydra_lib_math_e)        (prim0 (prim-name hydra_lib_math_e)        (lambda () hydra_overlay_common_lisp_lib_math_e)        nil f64))
        (cons (prim-name hydra_lib_math_exp)      (prim1 (prim-name hydra_lib_math_exp)      hydra_overlay_common_lisp_lib_math_exp      nil f64 f64))
        (cons (prim-name hydra_lib_math_floor)    (prim1 (prim-name hydra_lib_math_floor)    hydra_overlay_common_lisp_lib_math_floor    nil f64 f64))
        (cons (prim-name hydra_lib_math_log)      (prim1 (prim-name hydra_lib_math_log)      hydra_overlay_common_lisp_lib_math_log      nil f64 f64))
        (cons (prim-name hydra_lib_math_log_base)  (prim2 (prim-name hydra_lib_math_log_base)  hydra_overlay_common_lisp_lib_math_log_base nil f64 f64 f64))
        (cons (prim-name hydra_lib_math_mul_float64) (prim2 (prim-name hydra_lib_math_mul_float64) hydra_overlay_common_lisp_lib_math_mul_float64 nil f64 f64 f64))
        (cons (prim-name hydra_lib_math_negate_float64) (prim1 (prim-name hydra_lib_math_negate_float64) hydra_overlay_common_lisp_lib_math_negate_float64 nil f64 f64))
        (cons (prim-name hydra_lib_math_pi)       (prim0 (prim-name hydra_lib_math_pi)       (lambda () hydra_overlay_common_lisp_lib_math_pi)       nil f64))
        (cons (prim-name hydra_lib_math_pow)      (prim2 (prim-name hydra_lib_math_pow)      hydra_overlay_common_lisp_lib_math_pow      nil f64 f64 f64))
        (cons (prim-name hydra_lib_math_round)    (prim1 (prim-name hydra_lib_math_round)    hydra_overlay_common_lisp_lib_math_round    nil f64 f64))
        (cons (prim-name hydra_lib_math_round_float32)  (prim2 (prim-name hydra_lib_math_round_float32)  hydra_overlay_common_lisp_lib_math_round_float32  nil i32 f32 f32))
        (cons (prim-name hydra_lib_math_round_float64)  (prim2 (prim-name hydra_lib_math_round_float64)  hydra_overlay_common_lisp_lib_math_round_float64  nil i32 f64 f64))
        (cons (prim-name hydra_lib_math_sin)      (prim1 (prim-name hydra_lib_math_sin)      hydra_overlay_common_lisp_lib_math_sin      nil f64 f64))
        (cons (prim-name hydra_lib_math_sinh)     (prim1 (prim-name hydra_lib_math_sinh)     hydra_overlay_common_lisp_lib_math_sinh     nil f64 f64))
        (cons (prim-name hydra_lib_math_sqrt)     (prim1 (prim-name hydra_lib_math_sqrt)     hydra_overlay_common_lisp_lib_math_sqrt     nil f64 f64))
        (cons (prim-name hydra_lib_math_sub_float64) (prim2 (prim-name hydra_lib_math_sub_float64) hydra_overlay_common_lisp_lib_math_sub_float64 nil f64 f64 f64))
        (cons (prim-name hydra_lib_math_tan)      (prim1 (prim-name hydra_lib_math_tan)      hydra_overlay_common_lisp_lib_math_tan      nil f64 f64))
        (cons (prim-name hydra_lib_math_tanh)     (prim1 (prim-name hydra_lib_math_tanh)     hydra_overlay_common_lisp_lib_math_tanh     nil f64 f64))
        (cons (prim-name hydra_lib_math_truncate) (prim1 (prim-name hydra_lib_math_truncate) hydra_overlay_common_lisp_lib_math_truncate nil f64 f64))))))

;; ============================================================================
;; Maybes
;; ============================================================================

(defun register-optionals ()
  (let (
        (a (tc-variable "a"))
        (b (tc-variable "b"))
        (c (tc-variable "c")))
    (list
      (cons (prim-name hydra_lib_optionals_apply)    (prim2 (prim-name hydra_lib_optionals_apply)
                                           hydra_overlay_common_lisp_lib_optionals_apply
                                           nil (tc-optional (fun a b)) (tc-optional a) (tc-optional b)))
      (cons (prim-name hydra_lib_optionals_bind)     (prim2 (prim-name hydra_lib_optionals_bind)
                                           hydra_overlay_common_lisp_lib_optionals_bind
                                           nil (tc-optional a) (fun a (tc-optional b)) (tc-optional b)))
      (cons (prim-name hydra_lib_optionals_cases)    (lazy-args '(1) (prim3 (prim-name hydra_lib_optionals_cases)
                                           hydra_overlay_common_lisp_lib_optionals_cases
                                           nil (tc-optional a) b (fun a b) b)))
      (cons (prim-name hydra_lib_optionals_cat)      (prim1 (prim-name hydra_lib_optionals_cat)      hydra_overlay_common_lisp_lib_optionals_cat      nil (tc-list (tc-optional a)) (tc-list a)))
      (cons (prim-name hydra_lib_optionals_compose)  (prim3 (prim-name hydra_lib_optionals_compose)
                                           hydra_overlay_common_lisp_lib_optionals_compose
                                           nil (fun a (tc-optional b)) (fun b (tc-optional c)) a (tc-optional c)))
      (cons (prim-name hydra_lib_optionals_from_optional) (lazy-args '(0) (prim2 (prim-name hydra_lib_optionals_from_optional)
                                            hydra_overlay_common_lisp_lib_optionals_from_optional
                                            nil a (tc-optional a) a)))
      (cons (prim-name hydra_lib_optionals_is_given)    (prim1 (prim-name hydra_lib_optionals_is_given)    hydra_overlay_common_lisp_lib_optionals_is_given    nil (tc-optional a) (tc-boolean)))
      (cons (prim-name hydra_lib_optionals_is_none) (prim1 (prim-name hydra_lib_optionals_is_none) hydra_overlay_common_lisp_lib_optionals_is_none nil (tc-optional a) (tc-boolean)))
      (cons (prim-name hydra_lib_optionals_map)       (prim2 (prim-name hydra_lib_optionals_map)
                                            hydra_overlay_common_lisp_lib_optionals_map
                                            nil (fun a b) (tc-optional a) (tc-optional b)))
      (cons (prim-name hydra_lib_optionals_map_optional)  (prim2 (prim-name hydra_lib_optionals_map_optional)
                                            hydra_overlay_common_lisp_lib_optionals_map_optional
                                            nil (fun a (tc-optional b)) (tc-list a) (tc-list b)))
      (cons (prim-name hydra_lib_optionals_pure)      (prim1 (prim-name hydra_lib_optionals_pure)      hydra_overlay_common_lisp_lib_optionals_pure      nil a (tc-optional a)))
      (cons (prim-name hydra_lib_optionals_to_list)    (prim1 (prim-name hydra_lib_optionals_to_list)    hydra_overlay_common_lisp_lib_optionals_to_list   nil (tc-optional a) (tc-list a))))))

;; ============================================================================
;; Pairs
;; ============================================================================

(defun register-pairs ()
  (let (
        (a (tc-variable "a"))
        (b (tc-variable "b"))
        (c (tc-variable "c"))
        (d (tc-variable "d")))
    (list
      (cons (prim-name hydra_lib_pairs_bimap)  (prim3 (prim-name hydra_lib_pairs_bimap)
                                         hydra_overlay_common_lisp_lib_pairs_bimap
                                         nil (fun a c) (fun b d) (tc-pair a b) (tc-pair c d)))
      (cons (prim-name hydra_lib_pairs_first)  (prim1 (prim-name hydra_lib_pairs_first)  hydra_overlay_common_lisp_lib_pairs_first  nil (tc-pair a b) a))
      (cons (prim-name hydra_lib_pairs_second) (prim1 (prim-name hydra_lib_pairs_second) hydra_overlay_common_lisp_lib_pairs_second nil (tc-pair a b) b)))))

;; ============================================================================
;; Sets
;; ============================================================================

(defun register-sets ()
  (let (
        (a (tc-variable "a"))
        (b (tc-variable "b"))
        (ord-a '(("a" . ("ordering"))))
        (ord-ab '(("a" . ("ordering")) ("b" . ("ordering")))))
    (list
      (cons (prim-name hydra_lib_sets_delete)       (prim2 (prim-name hydra_lib_sets_delete)
                                               hydra_overlay_common_lisp_lib_sets_delete
                                               nil a (tc-set a) (tc-set a) ord-a))
      (cons (prim-name hydra_lib_sets_difference)   (prim2 (prim-name hydra_lib_sets_difference)
                                               hydra_overlay_common_lisp_lib_sets_difference
                                               nil (tc-set a) (tc-set a) (tc-set a) ord-a))
      (cons (prim-name hydra_lib_sets_empty)        (prim0 (prim-name hydra_lib_sets_empty)   (lambda () hydra_overlay_common_lisp_lib_sets_empty)   nil (tc-set a) ord-a))
      (cons (prim-name hydra_lib_sets_from_list)     (prim1 (prim-name hydra_lib_sets_from_list) hydra_overlay_common_lisp_lib_sets_from_list nil (tc-list a) (tc-set a) ord-a))
      (cons (prim-name hydra_lib_sets_insert)       (prim2 (prim-name hydra_lib_sets_insert)
                                               hydra_overlay_common_lisp_lib_sets_insert
                                               nil a (tc-set a) (tc-set a) ord-a))
      (cons (prim-name hydra_lib_sets_intersection) (prim2 (prim-name hydra_lib_sets_intersection)
                                               hydra_overlay_common_lisp_lib_sets_intersection
                                               nil (tc-set a) (tc-set a) (tc-set a) ord-a))
      (cons (prim-name hydra_lib_sets_map)          (prim2 (prim-name hydra_lib_sets_map)
                                               hydra_overlay_common_lisp_lib_sets_map
                                               nil (fun a b) (tc-set a) (tc-set b) ord-ab))
      (cons (prim-name hydra_lib_sets_member)       (prim2 (prim-name hydra_lib_sets_member)
                                               hydra_overlay_common_lisp_lib_sets_member
                                               nil a (tc-set a) (tc-boolean) ord-a))
      (cons (prim-name hydra_lib_sets_null)         (prim1 (prim-name hydra_lib_sets_null)     hydra_overlay_common_lisp_lib_sets_null     nil (tc-set a) (tc-boolean) ord-a))
      (cons (prim-name hydra_lib_sets_singleton)    (prim1 (prim-name hydra_lib_sets_singleton) hydra_overlay_common_lisp_lib_sets_singleton nil a (tc-set a) ord-a))
      (cons (prim-name hydra_lib_sets_size)         (prim1 (prim-name hydra_lib_sets_size)     hydra_overlay_common_lisp_lib_sets_size     nil (tc-set a) (tc-int32) ord-a))
      (cons (prim-name hydra_lib_sets_to_list)       (prim1 (prim-name hydra_lib_sets_to_list)   hydra_overlay_common_lisp_lib_sets_to_list  nil (tc-set a) (tc-list a) ord-a))
      (cons (prim-name hydra_lib_sets_union)        (prim2 (prim-name hydra_lib_sets_union)
                                               hydra_overlay_common_lisp_lib_sets_union
                                               nil (tc-set a) (tc-set a) (tc-set a) ord-a))
      (cons (prim-name hydra_lib_sets_unions)       (prim1 (prim-name hydra_lib_sets_unions)   hydra_overlay_common_lisp_lib_sets_unions   nil (tc-list (tc-set a)) (tc-set a) ord-a)))))

;; ============================================================================
;; Strings
;; ============================================================================

(defun register-strings ()
  (let (
        (s (tc-string))
        (i (tc-int32))
        (b (tc-boolean)))
    (list
      (cons (prim-name hydra_lib_strings_cat)         (prim1 (prim-name hydra_lib_strings_cat)         hydra_overlay_common_lisp_lib_strings_cat         nil (tc-list s) s))
      (cons (prim-name hydra_lib_strings_cat2)        (prim2 (prim-name hydra_lib_strings_cat2)
                                               hydra_overlay_common_lisp_lib_strings_cat2
                                               nil s s s))
      (cons (prim-name hydra_lib_strings_from_list)    (prim1 (prim-name hydra_lib_strings_from_list)    hydra_overlay_common_lisp_lib_strings_from_list    nil (tc-list i) s))
      (cons (prim-name hydra_lib_strings_intercalate) (prim2 (prim-name hydra_lib_strings_intercalate)
                                               hydra_overlay_common_lisp_lib_strings_intercalate
                                               nil s (tc-list s) s))
      (cons (prim-name hydra_lib_strings_length)      (prim1 (prim-name hydra_lib_strings_length)      hydra_overlay_common_lisp_lib_strings_length      nil s i))
      (cons (prim-name hydra_lib_strings_lines)       (prim1 (prim-name hydra_lib_strings_lines)       hydra_overlay_common_lisp_lib_strings_lines       nil s (tc-list s)))
      (cons (prim-name hydra_lib_strings_maybe_char_at) (prim2 (prim-name hydra_lib_strings_maybe_char_at) hydra_overlay_common_lisp_lib_strings_maybe_char_at nil i s (tc-optional i)))
      (cons (prim-name hydra_lib_strings_null)        (prim1 (prim-name hydra_lib_strings_null)        hydra_overlay_common_lisp_lib_strings_null        nil s b))
      (cons (prim-name hydra_lib_strings_split_on)     (prim2 (prim-name hydra_lib_strings_split_on)
                                               hydra_overlay_common_lisp_lib_strings_split_on
                                               nil s s (tc-list s)))
      (cons (prim-name hydra_lib_strings_to_list)      (prim1 (prim-name hydra_lib_strings_to_list)      hydra_overlay_common_lisp_lib_strings_to_list     nil s (tc-list i)))
      (cons (prim-name hydra_lib_strings_to_lower)     (prim1 (prim-name hydra_lib_strings_to_lower)     hydra_overlay_common_lisp_lib_strings_to_lower    nil s s))
      (cons (prim-name hydra_lib_strings_to_upper)     (prim1 (prim-name hydra_lib_strings_to_upper)     hydra_overlay_common_lisp_lib_strings_to_upper    nil s s))
      (cons (prim-name hydra_lib_strings_unlines)     (prim1 (prim-name hydra_lib_strings_unlines)     hydra_overlay_common_lisp_lib_strings_unlines     nil (tc-list s) s)))))

;; ============================================================================
;; Text (#494)
;; ============================================================================
;;
;; UTF-8 codecs bridging Hydra strings and raw bytes. decodeUtf8 :: binary -> either<string, string>
;; (Left message on invalid UTF-8); encodeUtf8 :: string -> binary (total). The real codecs live in
;; the relocated hydra.common_lisp.lib.text runtime (flat hydra_overlay_common_lisp_lib_text_*).

(defun register-text ()
  (let (
        (s (tc-string))
        (bin (tc-binary)))
    (list
      (cons (prim-name hydra_lib_text_decode_utf8) (prim1 (prim-name hydra_lib_text_decode_utf8)
                                                 hydra_overlay_common_lisp_lib_text_decode_utf8
                                                 nil bin (tc-either s s)))
      (cons (prim-name hydra_lib_text_encode_utf8) (prim1 (prim-name hydra_lib_text_encode_utf8)
                                                 hydra_overlay_common_lisp_lib_text_encode_utf8
                                                 nil s bin)))))

;; ============================================================================
;; Literals
;; ============================================================================

(defun register-literals ()
  (let (
        (bi  (tc-bigint))
        (dec (tc-decimal))
        (f32 (tc-float32))
        (f64 (tc-float64))
        (i8  (tc-int8))
        (i16 (tc-int16))
        (i32 (tc-int32))
        (i64 (tc-int64))
        (u8  (tc-uint8))
        (u16 (tc-uint16))
        (u32 (tc-uint32))
        (u64 (tc-uint64))
        (s   (tc-string))
        (b   (tc-boolean))
        (bin (tc-binary)))
    (append
      ;; Conversions
      (list
        (cons (prim-name hydra_lib_literals_bigint_to_decimal)    (prim1 (prim-name hydra_lib_literals_bigint_to_decimal)    hydra_overlay_common_lisp_lib_literals_bigint_to_decimal    nil bi dec))
        (cons (prim-name hydra_lib_literals_bigint_to_int8)       (prim1 (prim-name hydra_lib_literals_bigint_to_int8)       hydra_overlay_common_lisp_lib_literals_bigint_to_int8       nil bi i8))
        (cons (prim-name hydra_lib_literals_bigint_to_int16)      (prim1 (prim-name hydra_lib_literals_bigint_to_int16)      hydra_overlay_common_lisp_lib_literals_bigint_to_int16      nil bi i16))
        (cons (prim-name hydra_lib_literals_bigint_to_int32)      (prim1 (prim-name hydra_lib_literals_bigint_to_int32)      hydra_overlay_common_lisp_lib_literals_bigint_to_int32      nil bi i32))
        (cons (prim-name hydra_lib_literals_bigint_to_int64)      (prim1 (prim-name hydra_lib_literals_bigint_to_int64)      hydra_overlay_common_lisp_lib_literals_bigint_to_int64      nil bi i64))
        (cons (prim-name hydra_lib_literals_bigint_to_uint8)      (prim1 (prim-name hydra_lib_literals_bigint_to_uint8)      hydra_overlay_common_lisp_lib_literals_bigint_to_uint8      nil bi u8))
        (cons (prim-name hydra_lib_literals_bigint_to_uint16)     (prim1 (prim-name hydra_lib_literals_bigint_to_uint16)     hydra_overlay_common_lisp_lib_literals_bigint_to_uint16     nil bi u16))
        (cons (prim-name hydra_lib_literals_bigint_to_uint32)     (prim1 (prim-name hydra_lib_literals_bigint_to_uint32)     hydra_overlay_common_lisp_lib_literals_bigint_to_uint32     nil bi u32))
        (cons (prim-name hydra_lib_literals_bigint_to_uint64)     (prim1 (prim-name hydra_lib_literals_bigint_to_uint64)     hydra_overlay_common_lisp_lib_literals_bigint_to_uint64     nil bi u64))
        (cons (prim-name hydra_lib_literals_binary_to_bytes)      (prim1 (prim-name hydra_lib_literals_binary_to_bytes)      hydra_overlay_common_lisp_lib_literals_binary_to_bytes      nil bin (tc-list i32)))
        (cons (prim-name hydra_lib_literals_binary_to_string)     (prim1 (prim-name hydra_lib_literals_binary_to_string)     hydra_overlay_common_lisp_lib_literals_binary_to_string     nil bin s))
        (cons (prim-name hydra_lib_literals_decimal_to_bigint)    (prim1 (prim-name hydra_lib_literals_decimal_to_bigint)    hydra_overlay_common_lisp_lib_literals_decimal_to_bigint    nil dec bi))
        (cons (prim-name hydra_lib_literals_decimal_to_float32)   (prim1 (prim-name hydra_lib_literals_decimal_to_float32)   hydra_overlay_common_lisp_lib_literals_decimal_to_float32   nil dec f32))
        (cons (prim-name hydra_lib_literals_decimal_to_float64)   (prim1 (prim-name hydra_lib_literals_decimal_to_float64)   hydra_overlay_common_lisp_lib_literals_decimal_to_float64   nil dec f64))
        (cons (prim-name hydra_lib_literals_float32_to_decimal)   (prim1 (prim-name hydra_lib_literals_float32_to_decimal)   hydra_overlay_common_lisp_lib_literals_float32_to_decimal   nil f32 dec))
        (cons (prim-name hydra_lib_literals_float32_to_float64)   (prim1 (prim-name hydra_lib_literals_float32_to_float64)   hydra_overlay_common_lisp_lib_literals_float32_to_float64   nil f32 f64))
        (cons (prim-name hydra_lib_literals_float64_to_decimal)   (prim1 (prim-name hydra_lib_literals_float64_to_decimal)   hydra_overlay_common_lisp_lib_literals_float64_to_decimal   nil f64 dec))
        (cons (prim-name hydra_lib_literals_float64_to_float32)   (prim1 (prim-name hydra_lib_literals_float64_to_float32)   hydra_overlay_common_lisp_lib_literals_float64_to_float32   nil f64 f32))
        (cons (prim-name hydra_lib_literals_int8_to_bigint)       (prim1 (prim-name hydra_lib_literals_int8_to_bigint)       hydra_overlay_common_lisp_lib_literals_int8_to_bigint       nil i8 bi))
        (cons (prim-name hydra_lib_literals_int16_to_bigint)      (prim1 (prim-name hydra_lib_literals_int16_to_bigint)      hydra_overlay_common_lisp_lib_literals_int16_to_bigint      nil i16 bi))
        (cons (prim-name hydra_lib_literals_int32_to_bigint)      (prim1 (prim-name hydra_lib_literals_int32_to_bigint)      hydra_overlay_common_lisp_lib_literals_int32_to_bigint      nil i32 bi))
        (cons (prim-name hydra_lib_literals_int64_to_bigint)      (prim1 (prim-name hydra_lib_literals_int64_to_bigint)      hydra_overlay_common_lisp_lib_literals_int64_to_bigint      nil i64 bi))
        (cons (prim-name hydra_lib_literals_uint8_to_bigint)      (prim1 (prim-name hydra_lib_literals_uint8_to_bigint)      hydra_overlay_common_lisp_lib_literals_uint8_to_bigint      nil u8 bi))
        (cons (prim-name hydra_lib_literals_uint16_to_bigint)     (prim1 (prim-name hydra_lib_literals_uint16_to_bigint)     hydra_overlay_common_lisp_lib_literals_uint16_to_bigint     nil u16 bi))
        (cons (prim-name hydra_lib_literals_uint32_to_bigint)     (prim1 (prim-name hydra_lib_literals_uint32_to_bigint)     hydra_overlay_common_lisp_lib_literals_uint32_to_bigint     nil u32 bi))
        (cons (prim-name hydra_lib_literals_uint64_to_bigint)     (prim1 (prim-name hydra_lib_literals_uint64_to_bigint)     hydra_overlay_common_lisp_lib_literals_uint64_to_bigint     nil u64 bi))
        (cons (prim-name hydra_lib_literals_string_to_binary)     (prim1 (prim-name hydra_lib_literals_string_to_binary)     hydra_overlay_common_lisp_lib_literals_string_to_binary     nil s bin)))
      ;; Read primitives
      (list
        (cons (prim-name hydra_lib_literals_read_bigint)   (prim1 (prim-name hydra_lib_literals_read_bigint)   hydra_overlay_common_lisp_lib_literals_read_bigint   nil s (tc-optional bi)))
        (cons (prim-name hydra_lib_literals_read_boolean)  (prim1 (prim-name hydra_lib_literals_read_boolean)  hydra_overlay_common_lisp_lib_literals_read_boolean  nil s (tc-optional b)))
        (cons (prim-name hydra_lib_literals_read_decimal)  (prim1 (prim-name hydra_lib_literals_read_decimal)  hydra_overlay_common_lisp_lib_literals_read_decimal  nil s (tc-optional dec)))
        (cons (prim-name hydra_lib_literals_read_float32)  (prim1 (prim-name hydra_lib_literals_read_float32)  hydra_overlay_common_lisp_lib_literals_read_float32  nil s (tc-optional f32)))
        (cons (prim-name hydra_lib_literals_read_float64)  (prim1 (prim-name hydra_lib_literals_read_float64)  hydra_overlay_common_lisp_lib_literals_read_float64  nil s (tc-optional f64)))
        (cons (prim-name hydra_lib_literals_read_int8)     (prim1 (prim-name hydra_lib_literals_read_int8)     hydra_overlay_common_lisp_lib_literals_read_int8     nil s (tc-optional i8)))
        (cons (prim-name hydra_lib_literals_read_int16)    (prim1 (prim-name hydra_lib_literals_read_int16)    hydra_overlay_common_lisp_lib_literals_read_int16    nil s (tc-optional i16)))
        (cons (prim-name hydra_lib_literals_read_int32)    (prim1 (prim-name hydra_lib_literals_read_int32)    hydra_overlay_common_lisp_lib_literals_read_int32    nil s (tc-optional i32)))
        (cons (prim-name hydra_lib_literals_read_int64)    (prim1 (prim-name hydra_lib_literals_read_int64)    hydra_overlay_common_lisp_lib_literals_read_int64    nil s (tc-optional i64)))
        (cons (prim-name hydra_lib_literals_read_string)   (prim1 (prim-name hydra_lib_literals_read_string)   hydra_overlay_common_lisp_lib_literals_read_string   nil s (tc-optional s)))
        (cons (prim-name hydra_lib_literals_read_uint8)    (prim1 (prim-name hydra_lib_literals_read_uint8)    hydra_overlay_common_lisp_lib_literals_read_uint8    nil s (tc-optional u8)))
        (cons (prim-name hydra_lib_literals_read_uint16)   (prim1 (prim-name hydra_lib_literals_read_uint16)   hydra_overlay_common_lisp_lib_literals_read_uint16   nil s (tc-optional u16)))
        (cons (prim-name hydra_lib_literals_read_uint32)   (prim1 (prim-name hydra_lib_literals_read_uint32)   hydra_overlay_common_lisp_lib_literals_read_uint32   nil s (tc-optional u32)))
        (cons (prim-name hydra_lib_literals_read_uint64)   (prim1 (prim-name hydra_lib_literals_read_uint64)   hydra_overlay_common_lisp_lib_literals_read_uint64   nil s (tc-optional u64))))
      ;; Show primitives
      (list
        (cons (prim-name hydra_lib_literals_show_bigint)   (prim1 (prim-name hydra_lib_literals_show_bigint)   hydra_overlay_common_lisp_lib_literals_show_bigint   nil bi s))
        (cons (prim-name hydra_lib_literals_show_boolean)  (prim1 (prim-name hydra_lib_literals_show_boolean)  hydra_overlay_common_lisp_lib_literals_show_boolean  nil b s))
        (cons (prim-name hydra_lib_literals_show_decimal)  (prim1 (prim-name hydra_lib_literals_show_decimal)  hydra_overlay_common_lisp_lib_literals_show_decimal  nil dec s))
        (cons (prim-name hydra_lib_literals_show_float32)  (prim1 (prim-name hydra_lib_literals_show_float32)  hydra_overlay_common_lisp_lib_literals_show_float32  nil f32 s))
        (cons (prim-name hydra_lib_literals_show_float64)  (prim1 (prim-name hydra_lib_literals_show_float64)  hydra_overlay_common_lisp_lib_literals_show_float64  nil f64 s))
        (cons (prim-name hydra_lib_literals_show_int8)     (prim1 (prim-name hydra_lib_literals_show_int8)     hydra_overlay_common_lisp_lib_literals_show_int8     nil i8 s))
        (cons (prim-name hydra_lib_literals_show_int16)    (prim1 (prim-name hydra_lib_literals_show_int16)    hydra_overlay_common_lisp_lib_literals_show_int16    nil i16 s))
        (cons (prim-name hydra_lib_literals_show_int32)    (prim1 (prim-name hydra_lib_literals_show_int32)    hydra_overlay_common_lisp_lib_literals_show_int32    nil i32 s))
        (cons (prim-name hydra_lib_literals_show_int64)    (prim1 (prim-name hydra_lib_literals_show_int64)    hydra_overlay_common_lisp_lib_literals_show_int64    nil i64 s))
        (cons (prim-name hydra_lib_literals_show_uint8)    (prim1 (prim-name hydra_lib_literals_show_uint8)    hydra_overlay_common_lisp_lib_literals_show_uint8    nil u8 s))
        (cons (prim-name hydra_lib_literals_show_uint16)   (prim1 (prim-name hydra_lib_literals_show_uint16)   hydra_overlay_common_lisp_lib_literals_show_uint16   nil u16 s))
        (cons (prim-name hydra_lib_literals_show_uint32)   (prim1 (prim-name hydra_lib_literals_show_uint32)   hydra_overlay_common_lisp_lib_literals_show_uint32   nil u32 s))
        (cons (prim-name hydra_lib_literals_show_uint64)   (prim1 (prim-name hydra_lib_literals_show_uint64)   hydra_overlay_common_lisp_lib_literals_show_uint64   nil u64 s))
        (cons (prim-name hydra_lib_literals_show_string)   (prim1 (prim-name hydra_lib_literals_show_string)   hydra_overlay_common_lisp_lib_literals_show_string   nil s s))))))

;; ============================================================================
;; Annotations (term-level functions registered as primitives)
;; ============================================================================

(defun term-maybe-to-native (m)
  "Convert a term-level maybe (:optional val_or_nil) to native maybe (:given val) / (:none)."
  (cond
    ((null m) (list :none))
    ((not (consp m)) (list :given m))
    ((eq (first m) :none) (list :none))
    ((eq (first m) :given) m)
    ((eq (first m) :optional)
     (let ((inner (second m)))
       (if (or (null inner)
               (and (consp inner) (eq (first inner) :none)))
           (list :none)
           (list :given inner))))
    (t (list :given m))))

(defun native-maybe-to-term (m)
  "Convert a native maybe (:given val) / (:none) to term-level (:optional val_or_nil)."
  (cond
    ((null m) (list :optional nil))
    ((not (consp m)) (list :optional m))
    ((eq (first m) :given) (list :optional (second m)))
    ((eq (first m) :none) (list :optional nil))
    (t (list :optional m))))

(defun register-annotations ()
  (let ((t_ (tc-term)))
    (list
      ;; setTermAnnotation :: Name -> Maybe Term -> Term -> Term
      (cons "hydra.annotations.setTermAnnotation"
            (prim3 "hydra.annotations.setTermAnnotation"
                   (lambda (key)
                     (lambda (val)
                       (lambda (term)
                         (let ((native-maybe (term-maybe-to-native val)))
                           (funcall (funcall (funcall hydra_annotations_set_term_annotation key) native-maybe) term)))))
                   nil t_ t_ t_ t_))
      ;; getTermAnnotation :: Name -> Term -> Maybe Term
      (cons "hydra.annotations.getTermAnnotation"
            (prim2 "hydra.annotations.getTermAnnotation"
                   (lambda (key)
                     (lambda (term)
                       (native-maybe-to-term
                         (funcall (funcall hydra_annotations_get_term_annotation key) term))))
                   nil t_ t_ t_))
      ;; setTermDescription :: Maybe String -> Term -> Term
      (cons "hydra.annotations.setTermDescription"
            (prim2 "hydra.annotations.setTermDescription"
                   (lambda (d)
                     (lambda (term)
                       (let* ((native-d (term-maybe-to-native d))
                              (native-str-d
                                (cond
                                  ((eq (first native-d) :none) (list :none))
                                  (t (let ((inner (second native-d)))
                                       (if (and (consp inner) (eq (first inner) :literal)
                                                (consp (second inner)) (eq (first (second inner)) :string))
                                           (list :given (second (second inner)))
                                           native-d))))))
                         (funcall (funcall hydra_annotations_set_term_description native-str-d) term))))
                   nil t_ t_ t_))
      ;; getTermDescription :: InferenceContext -> Graph -> Term -> Either (Maybe String)
      (cons "hydra.annotations.getTermDescription"
            (prim3 "hydra.annotations.getTermDescription"
                   (lambda (cx)
                     (lambda (graph)
                       (lambda (term)
                         (let ((result (funcall (funcall (funcall hydra_annotations_get_term_description cx) graph) term)))
                           ;; Result is Either Error (Maybe String)
                           (if (eq (first result) :left)
                               (list :either result)
                               (let* ((maybe-str (second result))
                                      (term-maybe
                                        (cond
                                          ((null maybe-str) (list :optional nil))
                                          ((eq (first maybe-str) :none) (list :optional nil))
                                          ((eq (first maybe-str) :given)
                                           (list :optional (list :literal (list :string (second maybe-str)))))
                                          (t (list :optional maybe-str)))))
                                 (list :either (list :right term-maybe))))))))
                   nil t_ t_ t_ t_)))))

;; ============================================================================
;; Regex
;; ============================================================================

(defun register-regex ()
  (let (
        (s (tc-string))
        (b (tc-boolean)))
    (list
      (cons (prim-name hydra_lib_regex_find)       (prim2 (prim-name hydra_lib_regex_find)
                                              hydra_overlay_common_lisp_lib_regex_find
                                              nil s s (tc-optional s)))
      (cons (prim-name hydra_lib_regex_find_all)    (prim2 (prim-name hydra_lib_regex_find_all)
                                              hydra_overlay_common_lisp_lib_regex_find_all
                                              nil s s (tc-list s)))
      (cons (prim-name hydra_lib_regex_matches)    (prim2 (prim-name hydra_lib_regex_matches)
                                              hydra_overlay_common_lisp_lib_regex_matches
                                              nil s s b))
      (cons (prim-name hydra_lib_regex_replace)    (prim3 (prim-name hydra_lib_regex_replace)
                                              hydra_overlay_common_lisp_lib_regex_replace
                                              nil s s s s))
      (cons (prim-name hydra_lib_regex_replace_all) (prim3 (prim-name hydra_lib_regex_replace_all)
                                              hydra_overlay_common_lisp_lib_regex_replace_all
                                              nil s s s s))
      (cons (prim-name hydra_lib_regex_split)      (prim2 (prim-name hydra_lib_regex_split)
                                              hydra_overlay_common_lisp_lib_regex_split
                                              nil s s (tc-list s))))))

;; ============================================================================
;; Standard library: all primitives combined
;; ============================================================================

(defun standard-library ()
  "Returns an alist from primitive name (string) to Primitive record."
  (append
    (register-chars)
    (register-effects)
    (register-eithers)
    (register-equality)
    (register-files)
    (register-lists)
    (register-literals)
    (register-logic)
    (register-maps)
    (register-math)
    (register-optionals)
    (register-pairs)
    (register-regex)
    (register-sets)
    (register-strings)
    (register-text)
    (register-annotations)))
