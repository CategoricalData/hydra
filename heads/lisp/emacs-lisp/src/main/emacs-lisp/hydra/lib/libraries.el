;;; libraries.el --- Hydra Emacs Lisp standard library registration -*- lexical-binding: t; -*-

;;; Commentary:

;; Registers all primitive functions (chars, equality, eithers, lists, literals,
;; logic, maps, math, maybes, pairs, sets, strings, annotations) for the
;; generated reducer.  Direct translation of Common Lisp hydra/lib/libraries.lisp.

;;; Code:

(require 'cl-lib)

;; ============================================================================
;; Helpers
;; ============================================================================

(defun qname (ns_ local)
  (concat ns_ "." local))

(defun fun (dom cod)
  "A TermCoder for function types using beta reduction to bridge
   term-level functions to native functions."
  (tc-function-with-reduce
    (lambda (cx g t_)
      (funcall (funcall (funcall (funcall hydra_reduction_reduce_term cx) g) t) t_))
    dom cod))

;; ============================================================================
;; Chars
;; ============================================================================

(defun register-chars ()
  (let ((ns_ "hydra.lib.chars"))
    (list
      (cons (qname ns_ "isAlphaNum") (prim1 (qname ns_ "isAlphaNum") hydra_lib_chars_is_alpha_num nil (tc-int32) (tc-boolean)))
      (cons (qname ns_ "isLower")    (prim1 (qname ns_ "isLower")    hydra_lib_chars_is_lower    nil (tc-int32) (tc-boolean)))
      (cons (qname ns_ "isSpace")    (prim1 (qname ns_ "isSpace")    hydra_lib_chars_is_space    nil (tc-int32) (tc-boolean)))
      (cons (qname ns_ "isUpper")    (prim1 (qname ns_ "isUpper")    hydra_lib_chars_is_upper    nil (tc-int32) (tc-boolean)))
      (cons (qname ns_ "toLower")    (prim1 (qname ns_ "toLower")    hydra_lib_chars_to_lower    nil (tc-int32) (tc-int32)))
      (cons (qname ns_ "toUpper")    (prim1 (qname ns_ "toUpper")    hydra_lib_chars_to_upper    nil (tc-int32) (tc-int32))))))

;; ============================================================================
;; Eithers
;; ============================================================================

(defun register-eithers ()
  (let ((ns_ "hydra.lib.eithers")
        (x (tc-variable "x"))
        (y (tc-variable "y"))
        (z (tc-variable "z"))
        (w (tc-variable "w")))
    (list
      (cons (qname ns_ "bind")    (prim2 (qname ns_ "bind")
                                          hydra_lib_eithers_bind
                                          nil (tc-either x y) (fun y (tc-either x z)) (tc-either x z)))
      (cons (qname ns_ "bimap")   (prim3 (qname ns_ "bimap")
                                          hydra_lib_eithers_bimap
                                          nil (fun x z) (fun y w) (tc-either x y) (tc-either z w)))
      (cons (qname ns_ "either")  (prim3 (qname ns_ "either")
                                          hydra_lib_eithers_either
                                          nil (fun x z) (fun y z) (tc-either x y) z))
      (cons (qname ns_ "foldl")   (prim3 (qname ns_ "foldl")
                                          hydra_lib_eithers_foldl
                                          nil (fun x (fun y (tc-either z x))) x (tc-list y) (tc-either z x)))
      (cons (qname ns_ "fromLeft")  (prim2 (qname ns_ "fromLeft")
                                            hydra_lib_eithers_from_left
                                            nil x (tc-either x y) x))
      (cons (qname ns_ "fromRight") (prim2 (qname ns_ "fromRight")
                                            hydra_lib_eithers_from_right
                                            nil y (tc-either x y) y))
      (cons (qname ns_ "isLeft")  (prim1 (qname ns_ "isLeft")  hydra_lib_eithers_is_left  nil (tc-either x y) (tc-boolean)))
      (cons (qname ns_ "isRight") (prim1 (qname ns_ "isRight") hydra_lib_eithers_is_right nil (tc-either x y) (tc-boolean)))
      (cons (qname ns_ "lefts")   (prim1 (qname ns_ "lefts")   hydra_lib_eithers_lefts   nil (tc-list (tc-either x y)) (tc-list x)))
      (cons (qname ns_ "map")     (prim2 (qname ns_ "map")
                                          hydra_lib_eithers_map
                                          nil (fun x y) (tc-either z x) (tc-either z y)))
      (cons (qname ns_ "mapList") (prim2 (qname ns_ "mapList")
                                          hydra_lib_eithers_map_list
                                          nil (fun x (tc-either z y)) (tc-list x) (tc-either z (tc-list y))))
      (cons (qname ns_ "mapMaybe") (prim2 (qname ns_ "mapMaybe")
                                           hydra_lib_eithers_map_maybe
                                           nil (fun x (tc-either z y)) (tc-optional x) (tc-either z (tc-optional y))))
      (cons (qname ns_ "mapSet")  (prim2 (qname ns_ "mapSet")
                                          hydra_lib_eithers_map_set
                                          nil (fun x (tc-either z y)) (tc-set x) (tc-either z (tc-set y))))
      (cons (qname ns_ "partitionEithers") (prim1 (qname ns_ "partitionEithers")
                                                   hydra_lib_eithers_partition_eithers
                                                   nil (tc-list (tc-either x y)) (tc-pair (tc-list x) (tc-list y))))
      (cons (qname ns_ "rights")  (prim1 (qname ns_ "rights")  hydra_lib_eithers_rights  nil (tc-list (tc-either x y)) (tc-list y))))))

;; ============================================================================
;; Equality
;; ============================================================================

(defun register-equality ()
  (let ((ns_ "hydra.lib.equality")
        (x (tc-variable "x"))
        (ord-x '(("x" . ("ordering"))))
        (eq-x '(("x" . ("equality")))))
    (list
      (cons (qname ns_ "compare")  (prim2 (qname ns_ "compare")  hydra_lib_equality_compare  nil x x (tc-comparison) ord-x))
      (cons (qname ns_ "equal")    (prim2 (qname ns_ "equal")    hydra_lib_equality_equal    nil x x (tc-boolean) eq-x))
      (cons (qname ns_ "gt")       (prim2 (qname ns_ "gt")       hydra_lib_equality_gt       nil x x (tc-boolean) ord-x))
      (cons (qname ns_ "gte")      (prim2 (qname ns_ "gte")      hydra_lib_equality_gte      nil x x (tc-boolean) ord-x))
      (cons (qname ns_ "identity") (prim1 (qname ns_ "identity") #'identity                 nil x x))
      (cons (qname ns_ "lt")       (prim2 (qname ns_ "lt")       hydra_lib_equality_lt       nil x x (tc-boolean) ord-x))
      (cons (qname ns_ "lte")      (prim2 (qname ns_ "lte")      hydra_lib_equality_lte      nil x x (tc-boolean) ord-x))
      (cons (qname ns_ "max")      (prim2 (qname ns_ "max")      hydra_lib_equality_max      nil x x x ord-x))
      (cons (qname ns_ "min")      (prim2 (qname ns_ "min")      hydra_lib_equality_min      nil x x x ord-x)))))

;; ============================================================================
;; Lists
;; ============================================================================

(defun register-lists ()
  (let ((ns_ "hydra.lib.lists")
        (a (tc-variable "a"))
        (b (tc-variable "b"))
        (c (tc-variable "c")))
    (list
      (cons (qname ns_ "apply")      (prim2 (qname ns_ "apply")
                                              hydra_lib_lists_apply
                                              nil (tc-list (fun a b)) (tc-list a) (tc-list b)))
      (cons (qname ns_ "bind")       (prim2 (qname ns_ "bind")
                                              hydra_lib_lists_bind
                                              nil (tc-list a) (fun a (tc-list b)) (tc-list b)))
      (cons (qname ns_ "concat")     (prim1 (qname ns_ "concat")     hydra_lib_lists_concat     nil (tc-list (tc-list a)) (tc-list a)))
      (cons (qname ns_ "concat2")    (prim2 (qname ns_ "concat2")
                                              hydra_lib_lists_concat2
                                              nil (tc-list a) (tc-list a) (tc-list a)))
      (cons (qname ns_ "cons")       (prim2 (qname ns_ "cons")
                                              hydra_lib_lists_cons
                                              nil a (tc-list a) (tc-list a)))
      (cons (qname ns_ "drop")       (prim2 (qname ns_ "drop")
                                              hydra_lib_lists_drop
                                              nil (tc-int32) (tc-list a) (tc-list a)))
      (cons (qname ns_ "dropWhile")  (prim2 (qname ns_ "dropWhile")
                                              hydra_lib_lists_drop_while
                                              nil (fun a (tc-boolean)) (tc-list a) (tc-list a)))
      (cons (qname ns_ "elem")       (prim2 (qname ns_ "elem")
                                              hydra_lib_lists_elem
                                              nil a (tc-list a) (tc-boolean) '(("a" . ("equality")))))
      (cons (qname ns_ "filter")     (prim2 (qname ns_ "filter")
                                              hydra_lib_lists_filter
                                              nil (fun a (tc-boolean)) (tc-list a) (tc-list a)))
      (cons (qname ns_ "find")       (prim2 (qname ns_ "find")
                                              hydra_lib_lists_find
                                              nil (fun a (tc-boolean)) (tc-list a) (tc-optional a)))
      (cons (qname ns_ "foldl")      (prim3 (qname ns_ "foldl")
                                              (lambda (f)
                                                (lambda (init)
                                                  (lambda (xs)
                                                    (funcall (funcall (funcall hydra_lib_lists_foldl
                                                                               (lambda (acc) (lambda (el) (funcall (funcall f acc) el))))
                                                                      init) xs))))
                                              nil (fun b (fun a b)) b (tc-list a) b))
      (cons (qname ns_ "foldr")      (prim3 (qname ns_ "foldr")
                                              (lambda (f)
                                                (lambda (init)
                                                  (lambda (xs)
                                                    (funcall (funcall (funcall hydra_lib_lists_foldr
                                                                               (lambda (el) (lambda (acc) (funcall (funcall f el) acc))))
                                                                      init) xs))))
                                              nil (fun a (fun b b)) b (tc-list a) b))
      (cons (qname ns_ "group")      (prim1 (qname ns_ "group")      hydra_lib_lists_group      nil (tc-list a) (tc-list (tc-list a)) '(("a" . ("equality")))))
      (cons (qname ns_ "intercalate") (prim2 (qname ns_ "intercalate")
                                               hydra_lib_lists_intercalate
                                               nil (tc-list a) (tc-list (tc-list a)) (tc-list a)))
      (cons (qname ns_ "intersperse") (prim2 (qname ns_ "intersperse")
                                               hydra_lib_lists_intersperse
                                               nil a (tc-list a) (tc-list a)))
      (cons (qname ns_ "length")     (prim1 (qname ns_ "length")     hydra_lib_lists_length     nil (tc-list a) (tc-int32)))
      (cons (qname ns_ "map")        (prim2 (qname ns_ "map")
                                              hydra_lib_lists_map
                                              nil (fun a b) (tc-list a) (tc-list b)))
      (cons (qname ns_ "maybeAt")    (prim2 (qname ns_ "maybeAt")    hydra_lib_lists_maybe_at   nil (tc-int32) (tc-list a) (tc-optional a)))
      (cons (qname ns_ "maybeHead")  (prim1 (qname ns_ "maybeHead")  hydra_lib_lists_maybe_head nil (tc-list a) (tc-optional a)))
      (cons (qname ns_ "maybeInit")  (prim1 (qname ns_ "maybeInit")  hydra_lib_lists_maybe_init nil (tc-list a) (tc-optional (tc-list a))))
      (cons (qname ns_ "maybeLast")  (prim1 (qname ns_ "maybeLast")  hydra_lib_lists_maybe_last nil (tc-list a) (tc-optional a)))
      (cons (qname ns_ "maybeTail")  (prim1 (qname ns_ "maybeTail")  hydra_lib_lists_maybe_tail nil (tc-list a) (tc-optional (tc-list a))))
      (cons (qname ns_ "nub")        (prim1 (qname ns_ "nub")        hydra_lib_lists_nub        nil (tc-list a) (tc-list a) '(("a" . ("equality")))))
      (cons (qname ns_ "null")       (prim1 (qname ns_ "null")       hydra_lib_lists_null       nil (tc-list a) (tc-boolean)))
      (cons (qname ns_ "partition")   (prim2 (qname ns_ "partition")
                                               hydra_lib_lists_partition
                                               nil (fun a (tc-boolean)) (tc-list a) (tc-pair (tc-list a) (tc-list a))))
      (cons (qname ns_ "pure")       (prim1 (qname ns_ "pure")       hydra_lib_lists_pure       nil a (tc-list a)))
      (cons (qname ns_ "replicate")  (prim2 (qname ns_ "replicate")
                                              hydra_lib_lists_replicate
                                              nil (tc-int32) a (tc-list a)))
      (cons (qname ns_ "reverse")    (prim1 (qname ns_ "reverse")    hydra_lib_lists_reverse    nil (tc-list a) (tc-list a)))
      (cons (qname ns_ "singleton")  (prim1 (qname ns_ "singleton")  hydra_lib_lists_singleton  nil a (tc-list a)))
      (cons (qname ns_ "sort")       (prim1 (qname ns_ "sort")       hydra_lib_lists_sort       nil (tc-list a) (tc-list a) '(("a" . ("ordering")))))
      (cons (qname ns_ "sortOn")     (prim2 (qname ns_ "sortOn")
                                              hydra_lib_lists_sort_on
                                              nil (fun a b) (tc-list a) (tc-list a)))
      (cons (qname ns_ "span")       (prim2 (qname ns_ "span")
                                              hydra_lib_lists_span
                                              nil (fun a (tc-boolean)) (tc-list a) (tc-pair (tc-list a) (tc-list a))))
      (cons (qname ns_ "take")       (prim2 (qname ns_ "take")
                                              hydra_lib_lists_take
                                              nil (tc-int32) (tc-list a) (tc-list a)))
      (cons (qname ns_ "transpose")  (prim1 (qname ns_ "transpose")  hydra_lib_lists_transpose  nil (tc-list (tc-list a)) (tc-list (tc-list a))))
      (cons (qname ns_ "uncons")     (prim1 (qname ns_ "uncons")     hydra_lib_lists_uncons     nil (tc-list a) (tc-optional (tc-pair a (tc-list a)))))
      (cons (qname ns_ "zip")        (prim2 (qname ns_ "zip")
                                              hydra_lib_lists_zip
                                              nil (tc-list a) (tc-list b) (tc-list (tc-pair a b))))
      (cons (qname ns_ "zipWith")    (prim3 (qname ns_ "zipWith")
                                              (lambda (f)
                                                (lambda (xs)
                                                  (lambda (ys)
                                                    (funcall (funcall (funcall hydra_lib_lists_zip_with
                                                                               (lambda (a) (lambda (b) (funcall (funcall f a) b))))
                                                                      xs) ys))))
                                              nil (fun a (fun b c)) (tc-list a) (tc-list b) (tc-list c))))))

;; ============================================================================
;; Logic
;; ============================================================================

(defun register-logic ()
  (let ((ns_ "hydra.lib.logic")
        (a (tc-variable "a")))
    (list
      (cons (qname ns_ "and")    (prim2 (qname ns_ "and")
                                         hydra_lib_logic_and
                                         nil (tc-boolean) (tc-boolean) (tc-boolean)))
      (cons (qname ns_ "ifElse") (prim3 (qname ns_ "ifElse")
                                         hydra_lib_logic_if_else
                                         nil (tc-boolean) a a a))
      (cons (qname ns_ "not")    (prim1 (qname ns_ "not")    hydra_lib_logic_not nil (tc-boolean) (tc-boolean)))
      (cons (qname ns_ "or")     (prim2 (qname ns_ "or")
                                         hydra_lib_logic_or
                                         nil (tc-boolean) (tc-boolean) (tc-boolean))))))

;; ============================================================================
;; Maps
;; ============================================================================

(defun register-maps ()
  (let ((ns_ "hydra.lib.maps")
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
        (cons (qname ns_ "alter")          (prim3 (qname ns_ "alter")
                                                   hydra_lib_maps_alter
                                                   nil (fun (tc-optional v) (tc-optional v)) k map-kv map-kv ord-k))
        (cons (qname ns_ "bimap")          (prim3 (qname ns_ "bimap")
                                                   hydra_lib_maps_bimap
                                                   nil (fun k1 k2) (fun v1 v2) (tc-map k1 v1) (tc-map k2 v2) ord-k1k2))
        (cons (qname ns_ "delete")         (prim2 (qname ns_ "delete")
                                                   hydra_lib_maps_delete
                                                   nil k map-kv map-kv ord-k))
        (cons (qname ns_ "elems")          (prim1 (qname ns_ "elems")  hydra_lib_maps_elems  nil map-kv (tc-list v) ord-k))
        (cons (qname ns_ "empty")          (prim0 (qname ns_ "empty")  (lambda () hydra_lib_maps_empty)  nil map-kv ord-k))
        (cons (qname ns_ "filter")         (prim2 (qname ns_ "filter")
                                                   hydra_lib_maps_filter
                                                   nil (fun v (tc-boolean)) map-kv map-kv ord-k))
        (cons (qname ns_ "filterWithKey")  (prim2 (qname ns_ "filterWithKey")
                                                   hydra_lib_maps_filter_with_key
                                                   nil (fun k (fun v (tc-boolean))) map-kv map-kv ord-k))
        (cons (qname ns_ "findWithDefault") (prim3 (qname ns_ "findWithDefault")
                                                    hydra_lib_maps_find_with_default
                                                    nil v k map-kv v ord-k))
        (cons (qname ns_ "fromList")       (prim1 (qname ns_ "fromList") hydra_lib_maps_from_list nil (tc-list (tc-pair k v)) map-kv ord-k))
        (cons (qname ns_ "insert")         (prim3 (qname ns_ "insert")
                                                   hydra_lib_maps_insert
                                                   nil k v map-kv map-kv ord-k))
        (cons (qname ns_ "keys")           (prim1 (qname ns_ "keys")   hydra_lib_maps_keys   nil map-kv (tc-list k) ord-k))
        (cons (qname ns_ "lookup")         (prim2 (qname ns_ "lookup")
                                                   hydra_lib_maps_lookup
                                                   nil k map-kv (tc-optional v) ord-k))
        (cons (qname ns_ "map")            (prim2 (qname ns_ "map")
                                                   hydra_lib_maps_map
                                                   nil (fun v1 v2) (tc-map k v1) (tc-map k v2) ord-k))
        (cons (qname ns_ "mapKeys")        (prim2 (qname ns_ "mapKeys")
                                                   hydra_lib_maps_map_keys
                                                   nil (fun k1 k2) (tc-map k1 v) (tc-map k2 v) ord-k1k2))
        (cons (qname ns_ "member")         (prim2 (qname ns_ "member")
                                                   hydra_lib_maps_member
                                                   nil k map-kv (tc-boolean) ord-k))
        (cons (qname ns_ "null")           (prim1 (qname ns_ "null")   hydra_lib_maps_null   nil map-kv (tc-boolean) ord-k))
        (cons (qname ns_ "singleton")      (prim2 (qname ns_ "singleton")
                                                   hydra_lib_maps_singleton
                                                   nil k v map-kv ord-k))
        (cons (qname ns_ "size")           (prim1 (qname ns_ "size")   hydra_lib_maps_size   nil map-kv (tc-int32) ord-k))
        (cons (qname ns_ "toList")         (prim1 (qname ns_ "toList") hydra_lib_maps_to_list nil map-kv (tc-list (tc-pair k v)) ord-k))
        (cons (qname ns_ "union")          (prim2 (qname ns_ "union")
                                                   hydra_lib_maps_union
                                                   nil map-kv map-kv map-kv ord-k))))))

;; ============================================================================
;; Math
;; ============================================================================

(defun register-math ()
  (let ((ns_ "hydra.lib.math")
        (i32 (tc-int32))
        (f32 (tc-float32))
        (f64 (tc-float64))
        (bf  (tc-bigfloat))
        (bi  (tc-bigint))
        (b   (tc-boolean)))
    (append
      ;; Int32 primitives
      (list
        (cons (qname ns_ "abs")    (prim1 (qname ns_ "abs")    hydra_lib_math_abs    nil i32 i32))
        (cons (qname ns_ "add")    (prim2 (qname ns_ "add")    hydra_lib_math_add    nil i32 i32 i32))
        (cons (qname ns_ "even")   (prim1 (qname ns_ "even")   hydra_lib_math_even   nil i32 b))
        (cons (qname ns_ "mul")    (prim2 (qname ns_ "mul")    hydra_lib_math_mul    nil i32 i32 i32))
        (cons (qname ns_ "negate") (prim1 (qname ns_ "negate") hydra_lib_math_negate nil i32 i32))
        (cons (qname ns_ "odd")    (prim1 (qname ns_ "odd")    hydra_lib_math_odd    nil i32 b))
        (cons (qname ns_ "range")  (prim2 (qname ns_ "range")  hydra_lib_math_range  nil i32 i32 (tc-list i32)))
        (cons (qname ns_ "signum") (prim1 (qname ns_ "signum") hydra_lib_math_signum nil i32 i32))
        (cons (qname ns_ "sub")    (prim2 (qname ns_ "sub")    hydra_lib_math_sub    nil i32 i32 i32))
        (cons (qname ns_ "max")    (prim2 (qname ns_ "max")    hydra_lib_math_max    nil i32 i32 i32))
        (cons (qname ns_ "maybeDiv")  (prim2 (qname ns_ "maybeDiv")  hydra_lib_math_maybe_div  nil i32 i32 (tc-optional i32)))
        (cons (qname ns_ "maybeMod")  (prim2 (qname ns_ "maybeMod")  hydra_lib_math_maybe_mod  nil i32 i32 (tc-optional i32)))
        (cons (qname ns_ "maybePred") (prim1 (qname ns_ "maybePred") hydra_lib_math_maybe_pred nil i32 (tc-optional i32)))
        (cons (qname ns_ "maybeRem")  (prim2 (qname ns_ "maybeRem")  hydra_lib_math_maybe_rem  nil i32 i32 (tc-optional i32)))
        (cons (qname ns_ "maybeSucc") (prim1 (qname ns_ "maybeSucc") hydra_lib_math_maybe_succ nil i32 (tc-optional i32)))
        (cons (qname ns_ "min")    (prim2 (qname ns_ "min")    hydra_lib_math_min    nil i32 i32 i32)))
      ;; Float64 primitives
      (list
        (cons (qname ns_ "acos")     (prim1 (qname ns_ "acos")     hydra_lib_math_acos     nil f64 f64))
        (cons (qname ns_ "acosh")    (prim1 (qname ns_ "acosh")    hydra_lib_math_acosh    nil f64 f64))
        (cons (qname ns_ "addFloat64") (prim2 (qname ns_ "addFloat64") hydra_lib_math_add_float64 nil f64 f64 f64))
        (cons (qname ns_ "asin")     (prim1 (qname ns_ "asin")     hydra_lib_math_asin     nil f64 f64))
        (cons (qname ns_ "asinh")    (prim1 (qname ns_ "asinh")    hydra_lib_math_asinh    nil f64 f64))
        (cons (qname ns_ "atan")     (prim1 (qname ns_ "atan")     hydra_lib_math_atan     nil f64 f64))
        (cons (qname ns_ "atan2")    (prim2 (qname ns_ "atan2")    hydra_lib_math_atan2    nil f64 f64 f64))
        (cons (qname ns_ "atanh")    (prim1 (qname ns_ "atanh")    hydra_lib_math_atanh    nil f64 f64))
        (cons (qname ns_ "ceiling")  (prim1 (qname ns_ "ceiling")  hydra_lib_math_ceiling  nil f64 f64))
        (cons (qname ns_ "cos")      (prim1 (qname ns_ "cos")      hydra_lib_math_cos      nil f64 f64))
        (cons (qname ns_ "cosh")     (prim1 (qname ns_ "cosh")     hydra_lib_math_cosh     nil f64 f64))
        (cons (qname ns_ "e")        (prim0 (qname ns_ "e")        (lambda () hydra_lib_math_e)        nil f64))
        (cons (qname ns_ "exp")      (prim1 (qname ns_ "exp")      hydra_lib_math_exp      nil f64 f64))
        (cons (qname ns_ "floor")    (prim1 (qname ns_ "floor")    hydra_lib_math_floor    nil f64 f64))
        (cons (qname ns_ "log")      (prim1 (qname ns_ "log")      hydra_lib_math_log      nil f64 f64))
        (cons (qname ns_ "logBase")  (prim2 (qname ns_ "logBase")  hydra_lib_math_log_base nil f64 f64 f64))
        (cons (qname ns_ "mulFloat64") (prim2 (qname ns_ "mulFloat64") hydra_lib_math_mul_float64 nil f64 f64 f64))
        (cons (qname ns_ "negateFloat64") (prim1 (qname ns_ "negateFloat64") hydra_lib_math_negate_float64 nil f64 f64))
        (cons (qname ns_ "pi")       (prim0 (qname ns_ "pi")       (lambda () hydra_lib_math_pi)       nil f64))
        (cons (qname ns_ "pow")      (prim2 (qname ns_ "pow")      hydra_lib_math_pow      nil f64 f64 f64))
        (cons (qname ns_ "round")    (prim1 (qname ns_ "round")    hydra_lib_math_round    nil f64 f64))
        (cons (qname ns_ "roundBigfloat") (prim2 (qname ns_ "roundBigfloat") hydra_lib_math_round_bigfloat nil i32 bf bf))
        (cons (qname ns_ "roundFloat32")  (prim2 (qname ns_ "roundFloat32")  hydra_lib_math_round_float32  nil i32 f32 f32))
        (cons (qname ns_ "roundFloat64")  (prim2 (qname ns_ "roundFloat64")  hydra_lib_math_round_float64  nil i32 f64 f64))
        (cons (qname ns_ "sin")      (prim1 (qname ns_ "sin")      hydra_lib_math_sin      nil f64 f64))
        (cons (qname ns_ "sinh")     (prim1 (qname ns_ "sinh")     hydra_lib_math_sinh     nil f64 f64))
        (cons (qname ns_ "sqrt")     (prim1 (qname ns_ "sqrt")     hydra_lib_math_sqrt     nil f64 f64))
        (cons (qname ns_ "subFloat64") (prim2 (qname ns_ "subFloat64") hydra_lib_math_sub_float64 nil f64 f64 f64))
        (cons (qname ns_ "tan")      (prim1 (qname ns_ "tan")      hydra_lib_math_tan      nil f64 f64))
        (cons (qname ns_ "tanh")     (prim1 (qname ns_ "tanh")     hydra_lib_math_tanh     nil f64 f64))
        (cons (qname ns_ "truncate") (prim1 (qname ns_ "truncate") hydra_lib_math_truncate nil f64 f64))))))

;; ============================================================================
;; Maybes
;; ============================================================================

(defun register-maybes ()
  (let ((ns_ "hydra.lib.maybes")
        (a (tc-variable "a"))
        (b (tc-variable "b"))
        (c (tc-variable "c")))
    (list
      (cons (qname ns_ "apply")    (prim2 (qname ns_ "apply")
                                           hydra_lib_maybes_apply
                                           nil (tc-optional (fun a b)) (tc-optional a) (tc-optional b)))
      (cons (qname ns_ "bind")     (prim2 (qname ns_ "bind")
                                           hydra_lib_maybes_bind
                                           nil (tc-optional a) (fun a (tc-optional b)) (tc-optional b)))
      (cons (qname ns_ "cases")    (prim3 (qname ns_ "cases")
                                           hydra_lib_maybes_cases
                                           nil (tc-optional a) b (fun a b) b))
      (cons (qname ns_ "cat")      (prim1 (qname ns_ "cat")      hydra_lib_maybes_cat      nil (tc-list (tc-optional a)) (tc-list a)))
      (cons (qname ns_ "compose")  (prim3 (qname ns_ "compose")
                                           hydra_lib_maybes_compose
                                           nil (fun a (tc-optional b)) (fun b (tc-optional c)) a (tc-optional c)))
      (cons (qname ns_ "fromMaybe") (prim2 (qname ns_ "fromMaybe")
                                            hydra_lib_maybes_from_maybe
                                            nil a (tc-optional a) a))
      (cons (qname ns_ "isJust")    (prim1 (qname ns_ "isJust")    hydra_lib_maybes_is_just    nil (tc-optional a) (tc-boolean)))
      (cons (qname ns_ "isNothing") (prim1 (qname ns_ "isNothing") hydra_lib_maybes_is_nothing nil (tc-optional a) (tc-boolean)))
      (cons (qname ns_ "map")       (prim2 (qname ns_ "map")
                                            hydra_lib_maybes_map
                                            nil (fun a b) (tc-optional a) (tc-optional b)))
      (cons (qname ns_ "mapMaybe")  (prim2 (qname ns_ "mapMaybe")
                                            hydra_lib_maybes_map_maybe
                                            nil (fun a (tc-optional b)) (tc-list a) (tc-list b)))
      (cons (qname ns_ "maybe")     (prim3 (qname ns_ "maybe")
                                            hydra_lib_maybes_maybe
                                            nil b (fun a b) (tc-optional a) b))
      (cons (qname ns_ "pure")      (prim1 (qname ns_ "pure")      hydra_lib_maybes_pure      nil a (tc-optional a)))
      (cons (qname ns_ "toList")    (prim1 (qname ns_ "toList")    hydra_lib_maybes_to_list   nil (tc-optional a) (tc-list a))))))

;; ============================================================================
;; Pairs
;; ============================================================================

(defun register-pairs ()
  (let ((ns_ "hydra.lib.pairs")
        (a (tc-variable "a"))
        (b (tc-variable "b"))
        (c (tc-variable "c"))
        (d (tc-variable "d")))
    (list
      (cons (qname ns_ "bimap")  (prim3 (qname ns_ "bimap")
                                         hydra_lib_pairs_bimap
                                         nil (fun a c) (fun b d) (tc-pair a b) (tc-pair c d)))
      (cons (qname ns_ "first")  (prim1 (qname ns_ "first")  hydra_lib_pairs_first  nil (tc-pair a b) a))
      (cons (qname ns_ "second") (prim1 (qname ns_ "second") hydra_lib_pairs_second nil (tc-pair a b) b)))))

;; ============================================================================
;; Sets
;; ============================================================================

(defun register-sets ()
  (let ((ns_ "hydra.lib.sets")
        (a (tc-variable "a"))
        (b (tc-variable "b"))
        (ord-a '(("a" . ("ordering"))))
        (ord-ab '(("a" . ("ordering")) ("b" . ("ordering")))))
    (list
      (cons (qname ns_ "delete")       (prim2 (qname ns_ "delete")
                                               hydra_lib_sets_delete
                                               nil a (tc-set a) (tc-set a) ord-a))
      (cons (qname ns_ "difference")   (prim2 (qname ns_ "difference")
                                               hydra_lib_sets_difference
                                               nil (tc-set a) (tc-set a) (tc-set a) ord-a))
      (cons (qname ns_ "empty")        (prim0 (qname ns_ "empty")   (lambda () hydra_lib_sets_empty)   nil (tc-set a) ord-a))
      (cons (qname ns_ "fromList")     (prim1 (qname ns_ "fromList") hydra_lib_sets_from_list nil (tc-list a) (tc-set a) ord-a))
      (cons (qname ns_ "insert")       (prim2 (qname ns_ "insert")
                                               hydra_lib_sets_insert
                                               nil a (tc-set a) (tc-set a) ord-a))
      (cons (qname ns_ "intersection") (prim2 (qname ns_ "intersection")
                                               hydra_lib_sets_intersection
                                               nil (tc-set a) (tc-set a) (tc-set a) ord-a))
      (cons (qname ns_ "map")          (prim2 (qname ns_ "map")
                                               hydra_lib_sets_map
                                               nil (fun a b) (tc-set a) (tc-set b) ord-ab))
      (cons (qname ns_ "member")       (prim2 (qname ns_ "member")
                                               hydra_lib_sets_member
                                               nil a (tc-set a) (tc-boolean) ord-a))
      (cons (qname ns_ "null")         (prim1 (qname ns_ "null")     hydra_lib_sets_null     nil (tc-set a) (tc-boolean) ord-a))
      (cons (qname ns_ "singleton")    (prim1 (qname ns_ "singleton") hydra_lib_sets_singleton nil a (tc-set a) ord-a))
      (cons (qname ns_ "size")         (prim1 (qname ns_ "size")     hydra_lib_sets_size     nil (tc-set a) (tc-int32) ord-a))
      (cons (qname ns_ "toList")       (prim1 (qname ns_ "toList")   hydra_lib_sets_to_list  nil (tc-set a) (tc-list a) ord-a))
      (cons (qname ns_ "union")        (prim2 (qname ns_ "union")
                                               hydra_lib_sets_union
                                               nil (tc-set a) (tc-set a) (tc-set a) ord-a))
      (cons (qname ns_ "unions")       (prim1 (qname ns_ "unions")   hydra_lib_sets_unions   nil (tc-list (tc-set a)) (tc-set a) ord-a)))))

;; ============================================================================
;; Strings
;; ============================================================================

(defun register-strings ()
  (let ((ns_ "hydra.lib.strings")
        (s (tc-string))
        (i (tc-int32))
        (b (tc-boolean)))
    (list
      (cons (qname ns_ "cat")         (prim1 (qname ns_ "cat")         hydra_lib_strings_cat         nil (tc-list s) s))
      (cons (qname ns_ "cat2")        (prim2 (qname ns_ "cat2")
                                               hydra_lib_strings_cat2
                                               nil s s s))
      (cons (qname ns_ "fromList")    (prim1 (qname ns_ "fromList")    hydra_lib_strings_from_list    nil (tc-list i) s))
      (cons (qname ns_ "intercalate") (prim2 (qname ns_ "intercalate")
                                               hydra_lib_strings_intercalate
                                               nil s (tc-list s) s))
      (cons (qname ns_ "length")      (prim1 (qname ns_ "length")      hydra_lib_strings_length      nil s i))
      (cons (qname ns_ "lines")       (prim1 (qname ns_ "lines")       hydra_lib_strings_lines       nil s (tc-list s)))
      (cons (qname ns_ "maybeCharAt") (prim2 (qname ns_ "maybeCharAt") hydra_lib_strings_maybe_char_at nil i s (tc-optional i)))
      (cons (qname ns_ "null")        (prim1 (qname ns_ "null")        hydra_lib_strings_null        nil s b))
      (cons (qname ns_ "splitOn")     (prim2 (qname ns_ "splitOn")
                                               hydra_lib_strings_split_on
                                               nil s s (tc-list s)))
      (cons (qname ns_ "toList")      (prim1 (qname ns_ "toList")      hydra_lib_strings_to_list     nil s (tc-list i)))
      (cons (qname ns_ "toLower")     (prim1 (qname ns_ "toLower")     hydra_lib_strings_to_lower    nil s s))
      (cons (qname ns_ "toUpper")     (prim1 (qname ns_ "toUpper")     hydra_lib_strings_to_upper    nil s s))
      (cons (qname ns_ "unlines")     (prim1 (qname ns_ "unlines")     hydra_lib_strings_unlines     nil (tc-list s) s)))))

;; ============================================================================
;; Literals
;; ============================================================================

(defun register-literals ()
  (let ((ns_ "hydra.lib.literals")
        (bf  (tc-bigfloat))
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
        (cons (qname ns_ "bigfloatToBigint")   (prim1 (qname ns_ "bigfloatToBigint")   hydra_lib_literals_bigfloat_to_bigint   nil bf bi))
        (cons (qname ns_ "bigfloatToFloat32")  (prim1 (qname ns_ "bigfloatToFloat32")  hydra_lib_literals_bigfloat_to_float32  nil bf f32))
        (cons (qname ns_ "bigfloatToFloat64")  (prim1 (qname ns_ "bigfloatToFloat64")  hydra_lib_literals_bigfloat_to_float64  nil bf f64))
        (cons (qname ns_ "bigintToBigfloat")   (prim1 (qname ns_ "bigintToBigfloat")   hydra_lib_literals_bigint_to_bigfloat   nil bi bf))
        (cons (qname ns_ "bigintToDecimal")    (prim1 (qname ns_ "bigintToDecimal")    hydra_lib_literals_bigint_to_decimal    nil bi dec))
        (cons (qname ns_ "bigintToInt8")       (prim1 (qname ns_ "bigintToInt8")       hydra_lib_literals_bigint_to_int8       nil bi i8))
        (cons (qname ns_ "bigintToInt16")      (prim1 (qname ns_ "bigintToInt16")      hydra_lib_literals_bigint_to_int16      nil bi i16))
        (cons (qname ns_ "bigintToInt32")      (prim1 (qname ns_ "bigintToInt32")      hydra_lib_literals_bigint_to_int32      nil bi i32))
        (cons (qname ns_ "bigintToInt64")      (prim1 (qname ns_ "bigintToInt64")      hydra_lib_literals_bigint_to_int64      nil bi i64))
        (cons (qname ns_ "bigintToUint8")      (prim1 (qname ns_ "bigintToUint8")      hydra_lib_literals_bigint_to_uint8      nil bi u8))
        (cons (qname ns_ "bigintToUint16")     (prim1 (qname ns_ "bigintToUint16")     hydra_lib_literals_bigint_to_uint16     nil bi u16))
        (cons (qname ns_ "bigintToUint32")     (prim1 (qname ns_ "bigintToUint32")     hydra_lib_literals_bigint_to_uint32     nil bi u32))
        (cons (qname ns_ "bigintToUint64")     (prim1 (qname ns_ "bigintToUint64")     hydra_lib_literals_bigint_to_uint64     nil bi u64))
        (cons (qname ns_ "binaryToBytes")      (prim1 (qname ns_ "binaryToBytes")      hydra_lib_literals_binary_to_bytes      nil bin (tc-list i32)))
        (cons (qname ns_ "binaryToString")     (prim1 (qname ns_ "binaryToString")     hydra_lib_literals_binary_to_string     nil bin s))
        (cons (qname ns_ "decimalToBigint")    (prim1 (qname ns_ "decimalToBigint")    hydra_lib_literals_decimal_to_bigint    nil dec bi))
        (cons (qname ns_ "decimalToFloat32")   (prim1 (qname ns_ "decimalToFloat32")   hydra_lib_literals_decimal_to_float32   nil dec f32))
        (cons (qname ns_ "decimalToFloat64")   (prim1 (qname ns_ "decimalToFloat64")   hydra_lib_literals_decimal_to_float64   nil dec f64))
        (cons (qname ns_ "float32ToBigfloat")  (prim1 (qname ns_ "float32ToBigfloat")  hydra_lib_literals_float32_to_bigfloat  nil f32 bf))
        (cons (qname ns_ "float32ToDecimal")   (prim1 (qname ns_ "float32ToDecimal")   hydra_lib_literals_float32_to_decimal   nil f32 dec))
        (cons (qname ns_ "float64ToBigfloat")  (prim1 (qname ns_ "float64ToBigfloat")  hydra_lib_literals_float64_to_bigfloat  nil f64 bf))
        (cons (qname ns_ "float64ToDecimal")   (prim1 (qname ns_ "float64ToDecimal")   hydra_lib_literals_float64_to_decimal   nil f64 dec))
        (cons (qname ns_ "int8ToBigint")       (prim1 (qname ns_ "int8ToBigint")       hydra_lib_literals_int8_to_bigint       nil i8 bi))
        (cons (qname ns_ "int16ToBigint")      (prim1 (qname ns_ "int16ToBigint")      hydra_lib_literals_int16_to_bigint      nil i16 bi))
        (cons (qname ns_ "int32ToBigint")      (prim1 (qname ns_ "int32ToBigint")      hydra_lib_literals_int32_to_bigint      nil i32 bi))
        (cons (qname ns_ "int64ToBigint")      (prim1 (qname ns_ "int64ToBigint")      hydra_lib_literals_int64_to_bigint      nil i64 bi))
        (cons (qname ns_ "uint8ToBigint")      (prim1 (qname ns_ "uint8ToBigint")      hydra_lib_literals_uint8_to_bigint      nil u8 bi))
        (cons (qname ns_ "uint16ToBigint")     (prim1 (qname ns_ "uint16ToBigint")     hydra_lib_literals_uint16_to_bigint     nil u16 bi))
        (cons (qname ns_ "uint32ToBigint")     (prim1 (qname ns_ "uint32ToBigint")     hydra_lib_literals_uint32_to_bigint     nil u32 bi))
        (cons (qname ns_ "uint64ToBigint")     (prim1 (qname ns_ "uint64ToBigint")     hydra_lib_literals_uint64_to_bigint     nil u64 bi))
        (cons (qname ns_ "stringToBinary")     (prim1 (qname ns_ "stringToBinary")     hydra_lib_literals_string_to_binary     nil s bin)))
      ;; Read primitives
      (list
        (cons (qname ns_ "readBigfloat") (prim1 (qname ns_ "readBigfloat") hydra_lib_literals_read_bigfloat nil s (tc-optional bf)))
        (cons (qname ns_ "readBigint")   (prim1 (qname ns_ "readBigint")   hydra_lib_literals_read_bigint   nil s (tc-optional bi)))
        (cons (qname ns_ "readBoolean")  (prim1 (qname ns_ "readBoolean")  hydra_lib_literals_read_boolean  nil s (tc-optional b)))
        (cons (qname ns_ "readDecimal")  (prim1 (qname ns_ "readDecimal")  hydra_lib_literals_read_decimal  nil s (tc-optional dec)))
        (cons (qname ns_ "readFloat32")  (prim1 (qname ns_ "readFloat32")  hydra_lib_literals_read_float32  nil s (tc-optional f32)))
        (cons (qname ns_ "readFloat64")  (prim1 (qname ns_ "readFloat64")  hydra_lib_literals_read_float64  nil s (tc-optional f64)))
        (cons (qname ns_ "readInt8")     (prim1 (qname ns_ "readInt8")     hydra_lib_literals_read_int8     nil s (tc-optional i8)))
        (cons (qname ns_ "readInt16")    (prim1 (qname ns_ "readInt16")    hydra_lib_literals_read_int16    nil s (tc-optional i16)))
        (cons (qname ns_ "readInt32")    (prim1 (qname ns_ "readInt32")    hydra_lib_literals_read_int32    nil s (tc-optional i32)))
        (cons (qname ns_ "readInt64")    (prim1 (qname ns_ "readInt64")    hydra_lib_literals_read_int64    nil s (tc-optional i64)))
        (cons (qname ns_ "readString")   (prim1 (qname ns_ "readString")   hydra_lib_literals_read_string   nil s (tc-optional s)))
        (cons (qname ns_ "readUint8")    (prim1 (qname ns_ "readUint8")    hydra_lib_literals_read_uint8    nil s (tc-optional u8)))
        (cons (qname ns_ "readUint16")   (prim1 (qname ns_ "readUint16")   hydra_lib_literals_read_uint16   nil s (tc-optional u16)))
        (cons (qname ns_ "readUint32")   (prim1 (qname ns_ "readUint32")   hydra_lib_literals_read_uint32   nil s (tc-optional u32)))
        (cons (qname ns_ "readUint64")   (prim1 (qname ns_ "readUint64")   hydra_lib_literals_read_uint64   nil s (tc-optional u64))))
      ;; Show primitives
      (list
        (cons (qname ns_ "showBigfloat") (prim1 (qname ns_ "showBigfloat") hydra_lib_literals_show_bigfloat nil bf s))
        (cons (qname ns_ "showBigint")   (prim1 (qname ns_ "showBigint")   hydra_lib_literals_show_bigint   nil bi s))
        (cons (qname ns_ "showBoolean")  (prim1 (qname ns_ "showBoolean")  hydra_lib_literals_show_boolean  nil b s))
        (cons (qname ns_ "showDecimal")  (prim1 (qname ns_ "showDecimal")  hydra_lib_literals_show_decimal  nil dec s))
        (cons (qname ns_ "showFloat32")  (prim1 (qname ns_ "showFloat32")  hydra_lib_literals_show_float32  nil f32 s))
        (cons (qname ns_ "showFloat64")  (prim1 (qname ns_ "showFloat64")  hydra_lib_literals_show_float64  nil f64 s))
        (cons (qname ns_ "showInt8")     (prim1 (qname ns_ "showInt8")     hydra_lib_literals_show_int8     nil i8 s))
        (cons (qname ns_ "showInt16")    (prim1 (qname ns_ "showInt16")    hydra_lib_literals_show_int16    nil i16 s))
        (cons (qname ns_ "showInt32")    (prim1 (qname ns_ "showInt32")    hydra_lib_literals_show_int32    nil i32 s))
        (cons (qname ns_ "showInt64")    (prim1 (qname ns_ "showInt64")    hydra_lib_literals_show_int64    nil i64 s))
        (cons (qname ns_ "showUint8")    (prim1 (qname ns_ "showUint8")    hydra_lib_literals_show_uint8    nil u8 s))
        (cons (qname ns_ "showUint16")   (prim1 (qname ns_ "showUint16")   hydra_lib_literals_show_uint16   nil u16 s))
        (cons (qname ns_ "showUint32")   (prim1 (qname ns_ "showUint32")   hydra_lib_literals_show_uint32   nil u32 s))
        (cons (qname ns_ "showUint64")   (prim1 (qname ns_ "showUint64")   hydra_lib_literals_show_uint64   nil u64 s))
        (cons (qname ns_ "showString")   (prim1 (qname ns_ "showString")   hydra_lib_literals_show_string   nil s s))))))

;; ============================================================================
;; Annotations (term-level functions registered as primitives)
;; ============================================================================

(defun term-maybe-to-native (m)
  "Convert a term-level maybe (:maybe val_or_nil) to native maybe (:just val) / (:nothing)."
  (cond
    ((null m) (list :nothing))
    ((not (consp m)) (list :just m))
    ((eq (car m) :nothing) (list :nothing))
    ((eq (car m) :just) m)
    ((eq (car m) :maybe)
     (let ((inner (cadr m)))
       (if (or (null inner)
               (and (consp inner) (eq (car inner) :nothing)))
           (list :nothing)
           (list :just inner))))
    (t (list :just m))))

(defun native-maybe-to-term (m)
  "Convert a native maybe (:just val) / (:nothing) to term-level (:maybe val_or_nil)."
  (cond
    ((null m) (list :maybe nil))
    ((not (consp m)) (list :maybe m))
    ((eq (car m) :just) (list :maybe (cadr m)))
    ((eq (car m) :nothing) (list :maybe nil))
    (t (list :maybe m))))

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
                                  ((eq (car native-d) :nothing) (list :nothing))
                                  (t (let ((inner (cadr native-d)))
                                       (if (and (consp inner) (eq (car inner) :literal)
                                                (consp (cadr inner)) (eq (car (cadr inner)) :string))
                                           (list :just (cadr (cadr inner)))
                                           native-d))))))
                         (funcall (funcall hydra_annotations_set_term_description native-str-d) term))))
                   nil t_ t_ t_))
      ;; getTermDescription :: Context -> Graph -> Term -> Either (Maybe String)
      (cons "hydra.annotations.getTermDescription"
            (prim3 "hydra.annotations.getTermDescription"
                   (lambda (cx)
                     (lambda (graph)
                       (lambda (term)
                         (let ((result (funcall (funcall (funcall hydra_annotations_get_term_description cx) graph) term)))
                           ;; Result is Either Error (Maybe String)
                           (if (eq (car result) :left)
                               (list :either result)
                               (let* ((maybe-str (cadr result))
                                      (term-maybe
                                        (cond
                                          ((null maybe-str) (list :maybe nil))
                                          ((eq (car maybe-str) :nothing) (list :maybe nil))
                                          ((eq (car maybe-str) :just)
                                           (list :maybe (list :literal (list :string (cadr maybe-str)))))
                                          (t (list :maybe maybe-str)))))
                                 (list :either (list :right term-maybe))))))))
                   nil t_ t_ t_ t_)))))

;; ============================================================================
;; Regex
;; ============================================================================

(defun register-regex ()
  (let ((ns_ "hydra.lib.regex")
        (s (tc-string))
        (b (tc-boolean)))
    (list
      (cons (qname ns_ "find")       (prim2 (qname ns_ "find")
                                              hydra_lib_regex_find
                                              nil s s (tc-optional s)))
      (cons (qname ns_ "findAll")    (prim2 (qname ns_ "findAll")
                                              hydra_lib_regex_find_all
                                              nil s s (tc-list s)))
      (cons (qname ns_ "matches")    (prim2 (qname ns_ "matches")
                                              hydra_lib_regex_matches
                                              nil s s b))
      (cons (qname ns_ "replace")    (prim3 (qname ns_ "replace")
                                              hydra_lib_regex_replace
                                              nil s s s s))
      (cons (qname ns_ "replaceAll") (prim3 (qname ns_ "replaceAll")
                                              hydra_lib_regex_replace_all
                                              nil s s s s))
      (cons (qname ns_ "split")      (prim2 (qname ns_ "split")
                                              hydra_lib_regex_split
                                              nil s s (tc-list s))))))

;; ============================================================================
;; Standard library: all primitives combined
;; ============================================================================

(defun standard-library ()
  "Returns an alist from primitive name (string) to Primitive record."
  (append
    (register-chars)
    (register-eithers)
    (register-equality)
    (register-lists)
    (register-literals)
    (register-logic)
    (register-maps)
    (register-math)
    (register-maybes)
    (register-pairs)
    (register-regex)
    (register-sets)
    (register-strings)
    (register-annotations)))

(provide 'hydra.lib.libraries)

;;; libraries.el ends here
