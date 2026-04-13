(define-library (hydra lib libraries)
  (import (scheme base)
          (hydra core)
          (hydra context)
          (hydra graph)
          (hydra prims)
          (hydra reduction)
          (hydra lib chars)
          (hydra lib eithers)
          (hydra lib equality)
          (hydra lib lists)
          (hydra lib literals)
          (hydra lib logic)
          (hydra lib maps)
          (hydra lib math)
          (hydra lib maybes)
          (hydra lib pairs)
          (hydra lib regex)
          (hydra lib sets)
          (hydra lib strings))
  (export standard-library)
  (begin

    ;; ============================================================================
    ;; Helpers
    ;; ============================================================================

    (define (qname ns local)
      (string-append ns "." local))

    (define (fun dom cod)
      (tc-function-with-reduce
        (lambda (cx g t) ((((hydra_reduction_reduce_term cx) g) #t) t))
        dom cod))

    ;; ============================================================================
    ;; Chars
    ;; ============================================================================

    (define (register-chars)
      (let ((ns "hydra.lib.chars"))
        (list
          (cons (qname ns "isAlphaNum") (prim1 (qname ns "isAlphaNum") hydra_lib_chars_is_alpha_num #f (tc-int32) (tc-boolean)))
          (cons (qname ns "isLower")    (prim1 (qname ns "isLower")    hydra_lib_chars_is_lower    #f (tc-int32) (tc-boolean)))
          (cons (qname ns "isSpace")    (prim1 (qname ns "isSpace")    hydra_lib_chars_is_space    #f (tc-int32) (tc-boolean)))
          (cons (qname ns "isUpper")    (prim1 (qname ns "isUpper")    hydra_lib_chars_is_upper    #f (tc-int32) (tc-boolean)))
          (cons (qname ns "toLower")    (prim1 (qname ns "toLower")    hydra_lib_chars_to_lower    #f (tc-int32) (tc-int32)))
          (cons (qname ns "toUpper")    (prim1 (qname ns "toUpper")    hydra_lib_chars_to_upper    #f (tc-int32) (tc-int32))))))

    ;; ============================================================================
    ;; Eithers
    ;; ============================================================================

    (define (register-eithers)
      (let ((ns "hydra.lib.eithers")
            (x (tc-variable "x"))
            (y (tc-variable "y"))
            (z (tc-variable "z"))
            (w (tc-variable "w")))
        (list
          (cons (qname ns "bind")    (prim2 (qname ns "bind")
                                             hydra_lib_eithers_bind
                                             #f (tc-either x y) (fun y (tc-either x z)) (tc-either x z)))
          (cons (qname ns "bimap")   (prim3 (qname ns "bimap")
                                             hydra_lib_eithers_bimap
                                             #f (fun x z) (fun y w) (tc-either x y) (tc-either z w)))
          (cons (qname ns "either")  (prim3 (qname ns "either")
                                             hydra_lib_eithers_either
                                             #f (fun x z) (fun y z) (tc-either x y) z))
          (cons (qname ns "foldl")   (prim3 (qname ns "foldl")
                                             hydra_lib_eithers_foldl
                                             #f (fun x (fun y (tc-either z x))) x (tc-list y) (tc-either z x)))
          (cons (qname ns "fromLeft")  (prim2 (qname ns "fromLeft")
                                               hydra_lib_eithers_from_left
                                               #f x (tc-either x y) x))
          (cons (qname ns "fromRight") (prim2 (qname ns "fromRight")
                                               hydra_lib_eithers_from_right
                                               #f y (tc-either x y) y))
          (cons (qname ns "isLeft")  (prim1 (qname ns "isLeft")  hydra_lib_eithers_is_left  #f (tc-either x y) (tc-boolean)))
          (cons (qname ns "isRight") (prim1 (qname ns "isRight") hydra_lib_eithers_is_right #f (tc-either x y) (tc-boolean)))
          (cons (qname ns "lefts")   (prim1 (qname ns "lefts")   hydra_lib_eithers_lefts   #f (tc-list (tc-either x y)) (tc-list x)))
          (cons (qname ns "map")     (prim2 (qname ns "map")
                                             hydra_lib_eithers_map
                                             #f (fun x y) (tc-either z x) (tc-either z y)))
          (cons (qname ns "mapList") (prim2 (qname ns "mapList")
                                             hydra_lib_eithers_map_list
                                             #f (fun x (tc-either z y)) (tc-list x) (tc-either z (tc-list y))))
          (cons (qname ns "mapMaybe") (prim2 (qname ns "mapMaybe")
                                              hydra_lib_eithers_map_maybe
                                              #f (fun x (tc-either z y)) (tc-optional x) (tc-either z (tc-optional y))))
          (cons (qname ns "mapSet")  (prim2 (qname ns "mapSet")
                                             hydra_lib_eithers_map_set
                                             #f (fun x (tc-either z y)) (tc-set x) (tc-either z (tc-set y))))
          (cons (qname ns "partitionEithers") (prim1 (qname ns "partitionEithers")
                                                      hydra_lib_eithers_partition_eithers
                                                      #f (tc-list (tc-either x y)) (tc-pair (tc-list x) (tc-list y))))
          (cons (qname ns "rights")  (prim1 (qname ns "rights")  hydra_lib_eithers_rights  #f (tc-list (tc-either x y)) (tc-list y))))))

    ;; ============================================================================
    ;; Equality
    ;; ============================================================================

    (define (register-equality)
      (let ((ns "hydra.lib.equality")
            (x (tc-variable "x"))
            (ord-x (list (cons "x" (make-hydra_core_type_variable_metadata (list "ordering")))))
            (eq-x  (list (cons "x" (make-hydra_core_type_variable_metadata (list "equality"))))))
        (list
          (cons (qname ns "compare")  (prim2 (qname ns "compare")  hydra_lib_equality_compare  #f x x (tc-comparison) ord-x))
          (cons (qname ns "equal")    (prim2 (qname ns "equal")    hydra_lib_equality_equal    #f x x (tc-boolean) eq-x))
          (cons (qname ns "gt")       (prim2 (qname ns "gt")       hydra_lib_equality_gt       #f x x (tc-boolean) ord-x))
          (cons (qname ns "gte")      (prim2 (qname ns "gte")      hydra_lib_equality_gte      #f x x (tc-boolean) ord-x))
          (cons (qname ns "identity") (prim1 (qname ns "identity") (lambda (x) x)             #f x x))
          (cons (qname ns "lt")       (prim2 (qname ns "lt")       hydra_lib_equality_lt       #f x x (tc-boolean) ord-x))
          (cons (qname ns "lte")      (prim2 (qname ns "lte")      hydra_lib_equality_lte      #f x x (tc-boolean) ord-x))
          (cons (qname ns "max")      (prim2 (qname ns "max")      hydra_lib_equality_max      #f x x x ord-x))
          (cons (qname ns "min")      (prim2 (qname ns "min")      hydra_lib_equality_min      #f x x x ord-x)))))

    ;; ============================================================================
    ;; Lists
    ;; ============================================================================

    (define (register-lists)
      (let ((ns "hydra.lib.lists")
            (a (tc-variable "a"))
            (b (tc-variable "b"))
            (c (tc-variable "c"))
            (ord-a (list (cons "a" (make-hydra_core_type_variable_metadata (list "ordering")))))
            (eq-a  (list (cons "a" (make-hydra_core_type_variable_metadata (list "equality"))))))
        (list
          (cons (qname ns "apply")      (prim2 (qname ns "apply")
                                                 hydra_lib_lists_apply
                                                 #f (tc-list (fun a b)) (tc-list a) (tc-list b)))
          (cons (qname ns "at")         (prim2 (qname ns "at")
                                                 hydra_lib_lists_at
                                                 #f (tc-int32) (tc-list a) a))
          (cons (qname ns "bind")       (prim2 (qname ns "bind")
                                                 hydra_lib_lists_bind
                                                 #f (tc-list a) (fun a (tc-list b)) (tc-list b)))
          (cons (qname ns "concat")     (prim1 (qname ns "concat")     hydra_lib_lists_concat     #f (tc-list (tc-list a)) (tc-list a)))
          (cons (qname ns "concat2")    (prim2 (qname ns "concat2")
                                                 hydra_lib_lists_concat2
                                                 #f (tc-list a) (tc-list a) (tc-list a)))
          (cons (qname ns "cons")       (prim2 (qname ns "cons")
                                                 hydra_lib_lists_cons
                                                 #f a (tc-list a) (tc-list a)))
          (cons (qname ns "drop")       (prim2 (qname ns "drop")
                                                 hydra_lib_lists_drop
                                                 #f (tc-int32) (tc-list a) (tc-list a)))
          (cons (qname ns "dropWhile")  (prim2 (qname ns "dropWhile")
                                                 hydra_lib_lists_drop_while
                                                 #f (fun a (tc-boolean)) (tc-list a) (tc-list a)))
          (cons (qname ns "elem")       (prim2 (qname ns "elem")
                                                 hydra_lib_lists_elem
                                                 #f a (tc-list a) (tc-boolean) eq-a))
          (cons (qname ns "filter")     (prim2 (qname ns "filter")
                                                 hydra_lib_lists_filter
                                                 #f (fun a (tc-boolean)) (tc-list a) (tc-list a)))
          (cons (qname ns "find")       (prim2 (qname ns "find")
                                                 hydra_lib_lists_find
                                                 #f (fun a (tc-boolean)) (tc-list a) (tc-optional a)))
          (cons (qname ns "foldl")      (prim3 (qname ns "foldl")
                                                 (lambda (f)
                                                   (lambda (init)
                                                     (lambda (xs)
                                                       (((hydra_lib_lists_foldl
                                                           (lambda (acc) (lambda (el) ((f acc) el))))
                                                         init) xs))))
                                                 #f (fun b (fun a b)) b (tc-list a) b))
          (cons (qname ns "foldr")      (prim3 (qname ns "foldr")
                                                 (lambda (f)
                                                   (lambda (init)
                                                     (lambda (xs)
                                                       (((hydra_lib_lists_foldr
                                                           (lambda (el) (lambda (acc) ((f el) acc))))
                                                         init) xs))))
                                                 #f (fun a (fun b b)) b (tc-list a) b))
          (cons (qname ns "group")      (prim1 (qname ns "group")      hydra_lib_lists_group      #f (tc-list a) (tc-list (tc-list a)) eq-a))
          (cons (qname ns "head")       (prim1 (qname ns "head")       hydra_lib_lists_head       #f (tc-list a) a))
          (cons (qname ns "init")       (prim1 (qname ns "init")       hydra_lib_lists_init       #f (tc-list a) (tc-list a)))
          (cons (qname ns "intercalate") (prim2 (qname ns "intercalate")
                                                  hydra_lib_lists_intercalate
                                                  #f (tc-list a) (tc-list (tc-list a)) (tc-list a)))
          (cons (qname ns "intersperse") (prim2 (qname ns "intersperse")
                                                  hydra_lib_lists_intersperse
                                                  #f a (tc-list a) (tc-list a)))
          (cons (qname ns "last")       (prim1 (qname ns "last")       hydra_lib_lists_last       #f (tc-list a) a))
          (cons (qname ns "length")     (prim1 (qname ns "length")     hydra_lib_lists_length     #f (tc-list a) (tc-int32)))
          (cons (qname ns "map")        (prim2 (qname ns "map")
                                                 hydra_lib_lists_map
                                                 #f (fun a b) (tc-list a) (tc-list b)))
          (cons (qname ns "maybeAt")    (prim2 (qname ns "maybeAt")    hydra_lib_lists_maybe_at   #f (tc-int32) (tc-list a) (tc-optional a)))
          (cons (qname ns "maybeHead")  (prim1 (qname ns "maybeHead")  hydra_lib_lists_maybe_head #f (tc-list a) (tc-optional a)))
          (cons (qname ns "maybeInit")  (prim1 (qname ns "maybeInit")  hydra_lib_lists_maybe_init #f (tc-list a) (tc-optional (tc-list a))))
          (cons (qname ns "maybeLast")  (prim1 (qname ns "maybeLast")  hydra_lib_lists_maybe_last #f (tc-list a) (tc-optional a)))
          (cons (qname ns "maybeTail")  (prim1 (qname ns "maybeTail")  hydra_lib_lists_maybe_tail #f (tc-list a) (tc-optional (tc-list a))))
          (cons (qname ns "nub")        (prim1 (qname ns "nub")        hydra_lib_lists_nub        #f (tc-list a) (tc-list a) eq-a))
          (cons (qname ns "null")       (prim1 (qname ns "null")       hydra_lib_lists_null       #f (tc-list a) (tc-boolean)))
          (cons (qname ns "partition")   (prim2 (qname ns "partition")
                                                  hydra_lib_lists_partition
                                                  #f (fun a (tc-boolean)) (tc-list a) (tc-pair (tc-list a) (tc-list a))))
          (cons (qname ns "pure")       (prim1 (qname ns "pure")       hydra_lib_lists_pure       #f a (tc-list a)))
          (cons (qname ns "replicate")  (prim2 (qname ns "replicate")
                                                 hydra_lib_lists_replicate
                                                 #f (tc-int32) a (tc-list a)))
          (cons (qname ns "reverse")    (prim1 (qname ns "reverse")    hydra_lib_lists_reverse    #f (tc-list a) (tc-list a)))
          (cons (qname ns "safeHead")   (prim1 (qname ns "safeHead")   hydra_lib_lists_safe_head  #f (tc-list a) (tc-optional a)))
          (cons (qname ns "singleton")  (prim1 (qname ns "singleton")  hydra_lib_lists_singleton  #f a (tc-list a)))
          (cons (qname ns "sort")       (prim1 (qname ns "sort")       hydra_lib_lists_sort       #f (tc-list a) (tc-list a) ord-a))
          (cons (qname ns "sortOn")     (prim2 (qname ns "sortOn")
                                                 hydra_lib_lists_sort_on
                                                 #f (fun a b) (tc-list a) (tc-list a)))
          (cons (qname ns "span")       (prim2 (qname ns "span")
                                                 hydra_lib_lists_span
                                                 #f (fun a (tc-boolean)) (tc-list a) (tc-pair (tc-list a) (tc-list a))))
          (cons (qname ns "tail")       (prim1 (qname ns "tail")       hydra_lib_lists_tail       #f (tc-list a) (tc-list a)))
          (cons (qname ns "take")       (prim2 (qname ns "take")
                                                 hydra_lib_lists_take
                                                 #f (tc-int32) (tc-list a) (tc-list a)))
          (cons (qname ns "transpose")  (prim1 (qname ns "transpose")  hydra_lib_lists_transpose  #f (tc-list (tc-list a)) (tc-list (tc-list a))))
          (cons (qname ns "zip")        (prim2 (qname ns "zip")
                                                 hydra_lib_lists_zip
                                                 #f (tc-list a) (tc-list b) (tc-list (tc-pair a b))))
          (cons (qname ns "zipWith")    (prim3 (qname ns "zipWith")
                                                 (lambda (f)
                                                   (lambda (xs)
                                                     (lambda (ys)
                                                       (((hydra_lib_lists_zip_with
                                                           (lambda (a) (lambda (b) ((f a) b))))
                                                         xs) ys))))
                                                 #f (fun a (fun b c)) (tc-list a) (tc-list b) (tc-list c))))))

    ;; ============================================================================
    ;; Logic
    ;; ============================================================================

    (define (register-logic)
      (let ((ns "hydra.lib.logic")
            (a (tc-variable "a")))
        (list
          (cons (qname ns "and")    (prim2 (qname ns "and")
                                            hydra_lib_logic_and
                                            #f (tc-boolean) (tc-boolean) (tc-boolean)))
          (cons (qname ns "ifElse") (prim3 (qname ns "ifElse")
                                            hydra_lib_logic_if_else
                                            #f (tc-boolean) a a a))
          (cons (qname ns "not")    (prim1 (qname ns "not")    hydra_lib_logic_not #f (tc-boolean) (tc-boolean)))
          (cons (qname ns "or")     (prim2 (qname ns "or")
                                            hydra_lib_logic_or
                                            #f (tc-boolean) (tc-boolean) (tc-boolean))))))

    ;; ============================================================================
    ;; Maps
    ;; ============================================================================

    (define (register-maps)
      (let ((ns "hydra.lib.maps")
            (k  (tc-variable "k"))
            (k1 (tc-variable "k1"))
            (k2 (tc-variable "k2"))
            (v  (tc-variable "v"))
            (v1 (tc-variable "v1"))
            (v2 (tc-variable "v2"))
            (ord-k (list (cons "k" (make-hydra_core_type_variable_metadata (list "ordering")))))
            (ord-k1k2 (list (cons "k1" (make-hydra_core_type_variable_metadata (list "ordering")))
                            (cons "k2" (make-hydra_core_type_variable_metadata (list "ordering"))))))
        (let ((map-kv (tc-map k v)))
          (list
            (cons (qname ns "alter")          (prim3 (qname ns "alter")
                                                      hydra_lib_maps_alter
                                                      #f (fun (tc-optional v) (tc-optional v)) k map-kv map-kv ord-k))
            (cons (qname ns "bimap")          (prim3 (qname ns "bimap")
                                                      hydra_lib_maps_bimap
                                                      #f (fun k1 k2) (fun v1 v2) (tc-map k1 v1) (tc-map k2 v2) ord-k1k2))
            (cons (qname ns "delete")         (prim2 (qname ns "delete")
                                                      hydra_lib_maps_delete
                                                      #f k map-kv map-kv ord-k))
            (cons (qname ns "elems")          (prim1 (qname ns "elems")  hydra_lib_maps_elems  #f map-kv (tc-list v) ord-k))
            (cons (qname ns "empty")          (prim0 (qname ns "empty")  (lambda () hydra_lib_maps_empty)  #f map-kv ord-k))
            (cons (qname ns "filter")         (prim2 (qname ns "filter")
                                                      hydra_lib_maps_filter
                                                      #f (fun v (tc-boolean)) map-kv map-kv ord-k))
            (cons (qname ns "filterWithKey")  (prim2 (qname ns "filterWithKey")
                                                      hydra_lib_maps_filter_with_key
                                                      #f (fun k (fun v (tc-boolean))) map-kv map-kv ord-k))
            (cons (qname ns "findWithDefault") (prim3 (qname ns "findWithDefault")
                                                       hydra_lib_maps_find_with_default
                                                       #f v k map-kv v ord-k))
            (cons (qname ns "fromList")       (prim1 (qname ns "fromList") hydra_lib_maps_from_list #f (tc-list (tc-pair k v)) map-kv ord-k))
            (cons (qname ns "insert")         (prim3 (qname ns "insert")
                                                      hydra_lib_maps_insert
                                                      #f k v map-kv map-kv ord-k))
            (cons (qname ns "keys")           (prim1 (qname ns "keys")   hydra_lib_maps_keys   #f map-kv (tc-list k) ord-k))
            (cons (qname ns "lookup")         (prim2 (qname ns "lookup")
                                                      hydra_lib_maps_lookup
                                                      #f k map-kv (tc-optional v) ord-k))
            (cons (qname ns "map")            (prim2 (qname ns "map")
                                                      hydra_lib_maps_map
                                                      #f (fun v1 v2) (tc-map k v1) (tc-map k v2) ord-k))
            (cons (qname ns "mapKeys")        (prim2 (qname ns "mapKeys")
                                                      hydra_lib_maps_map_keys
                                                      #f (fun k1 k2) (tc-map k1 v) (tc-map k2 v) ord-k1k2))
            (cons (qname ns "member")         (prim2 (qname ns "member")
                                                      hydra_lib_maps_member
                                                      #f k map-kv (tc-boolean) ord-k))
            (cons (qname ns "null")           (prim1 (qname ns "null")   hydra_lib_maps_null   #f map-kv (tc-boolean) ord-k))
            (cons (qname ns "singleton")      (prim2 (qname ns "singleton")
                                                      hydra_lib_maps_singleton
                                                      #f k v map-kv ord-k))
            (cons (qname ns "size")           (prim1 (qname ns "size")   hydra_lib_maps_size   #f map-kv (tc-int32) ord-k))
            (cons (qname ns "toList")         (prim1 (qname ns "toList") hydra_lib_maps_to_list #f map-kv (tc-list (tc-pair k v)) ord-k))
            (cons (qname ns "union")          (prim2 (qname ns "union")
                                                      hydra_lib_maps_union
                                                      #f map-kv map-kv map-kv ord-k))))))

    ;; ============================================================================
    ;; Math
    ;; ============================================================================

    (define (register-math)
      (let ((ns "hydra.lib.math")
            (i32 (tc-int32))
            (f32 (tc-float32))
            (f64 (tc-float64))
            (bf  (tc-bigfloat))
            (bi  (tc-bigint))
            (b   (tc-boolean)))
        (append
          (list
            (cons (qname ns "abs")    (prim1 (qname ns "abs")    hydra_lib_math_abs    #f i32 i32))
            (cons (qname ns "add")    (prim2 (qname ns "add")    hydra_lib_math_add    #f i32 i32 i32))
            (cons (qname ns "div")    (prim2 (qname ns "div")    hydra_lib_math_div    #f i32 i32 i32))
            (cons (qname ns "even")   (prim1 (qname ns "even")   hydra_lib_math_even   #f i32 b))
            (cons (qname ns "mod")    (prim2 (qname ns "mod")    hydra_lib_math_mod    #f i32 i32 i32))
            (cons (qname ns "mul")    (prim2 (qname ns "mul")    hydra_lib_math_mul    #f i32 i32 i32))
            (cons (qname ns "negate") (prim1 (qname ns "negate") hydra_lib_math_negate #f i32 i32))
            (cons (qname ns "odd")    (prim1 (qname ns "odd")    hydra_lib_math_odd    #f i32 b))
            (cons (qname ns "pred")   (prim1 (qname ns "pred")   hydra_lib_math_pred   #f i32 i32))
            (cons (qname ns "range")  (prim2 (qname ns "range")  hydra_lib_math_range  #f i32 i32 (tc-list i32)))
            (cons (qname ns "rem")    (prim2 (qname ns "rem")    hydra_lib_math_rem    #f i32 i32 i32))
            (cons (qname ns "signum") (prim1 (qname ns "signum") hydra_lib_math_signum #f i32 i32))
            (cons (qname ns "sub")    (prim2 (qname ns "sub")    hydra_lib_math_sub    #f i32 i32 i32))
            (cons (qname ns "succ")   (prim1 (qname ns "succ")   hydra_lib_math_succ   #f i32 i32))
            (cons (qname ns "max")    (prim2 (qname ns "max")    hydra_lib_math_max    #f i32 i32 i32))
            (cons (qname ns "maybeDiv")  (prim2 (qname ns "maybeDiv")  hydra_lib_math_maybe_div  #f i32 i32 (tc-optional i32)))
            (cons (qname ns "maybeMod")  (prim2 (qname ns "maybeMod")  hydra_lib_math_maybe_mod  #f i32 i32 (tc-optional i32)))
            (cons (qname ns "maybePred") (prim1 (qname ns "maybePred") hydra_lib_math_maybe_pred #f i32 (tc-optional i32)))
            (cons (qname ns "maybeRem")  (prim2 (qname ns "maybeRem")  hydra_lib_math_maybe_rem  #f i32 i32 (tc-optional i32)))
            (cons (qname ns "maybeSucc") (prim1 (qname ns "maybeSucc") hydra_lib_math_maybe_succ #f i32 (tc-optional i32)))
            (cons (qname ns "min")    (prim2 (qname ns "min")    hydra_lib_math_min    #f i32 i32 i32)))
          (list
            (cons (qname ns "acos")     (prim1 (qname ns "acos")     hydra_lib_math_acos     #f f64 f64))
            (cons (qname ns "acosh")    (prim1 (qname ns "acosh")    hydra_lib_math_acosh    #f f64 f64))
            (cons (qname ns "addFloat64") (prim2 (qname ns "addFloat64") hydra_lib_math_add_float64 #f f64 f64 f64))
            (cons (qname ns "asin")     (prim1 (qname ns "asin")     hydra_lib_math_asin     #f f64 f64))
            (cons (qname ns "asinh")    (prim1 (qname ns "asinh")    hydra_lib_math_asinh    #f f64 f64))
            (cons (qname ns "atan")     (prim1 (qname ns "atan")     hydra_lib_math_atan     #f f64 f64))
            (cons (qname ns "atan2")    (prim2 (qname ns "atan2")    hydra_lib_math_atan2    #f f64 f64 f64))
            (cons (qname ns "atanh")    (prim1 (qname ns "atanh")    hydra_lib_math_atanh    #f f64 f64))
            (cons (qname ns "ceiling")  (prim1 (qname ns "ceiling")  hydra_lib_math_ceiling  #f f64 f64))
            (cons (qname ns "cos")      (prim1 (qname ns "cos")      hydra_lib_math_cos      #f f64 f64))
            (cons (qname ns "cosh")     (prim1 (qname ns "cosh")     hydra_lib_math_cosh     #f f64 f64))
            (cons (qname ns "e")        (prim0 (qname ns "e")        (lambda () hydra_lib_math_e)        #f f64))
            (cons (qname ns "exp")      (prim1 (qname ns "exp")      hydra_lib_math_exp      #f f64 f64))
            (cons (qname ns "floor")    (prim1 (qname ns "floor")    hydra_lib_math_floor    #f f64 f64))
            (cons (qname ns "log")      (prim1 (qname ns "log")      hydra_lib_math_log      #f f64 f64))
            (cons (qname ns "logBase")  (prim2 (qname ns "logBase")  hydra_lib_math_log_base #f f64 f64 f64))
            (cons (qname ns "mulFloat64") (prim2 (qname ns "mulFloat64") hydra_lib_math_mul_float64 #f f64 f64 f64))
            (cons (qname ns "negateFloat64") (prim1 (qname ns "negateFloat64") hydra_lib_math_negate_float64 #f f64 f64))
            (cons (qname ns "pi")       (prim0 (qname ns "pi")       (lambda () hydra_lib_math_pi)       #f f64))
            (cons (qname ns "pow")      (prim2 (qname ns "pow")      hydra_lib_math_pow      #f f64 f64 f64))
            (cons (qname ns "round")    (prim1 (qname ns "round")    hydra_lib_math_round    #f f64 f64))
            (cons (qname ns "roundBigfloat") (prim2 (qname ns "roundBigfloat") hydra_lib_math_round_bigfloat #f i32 bf bf))
            (cons (qname ns "roundFloat32")  (prim2 (qname ns "roundFloat32")  hydra_lib_math_round_float32  #f i32 f32 f32))
            (cons (qname ns "roundFloat64")  (prim2 (qname ns "roundFloat64")  hydra_lib_math_round_float64  #f i32 f64 f64))
            (cons (qname ns "sin")      (prim1 (qname ns "sin")      hydra_lib_math_sin      #f f64 f64))
            (cons (qname ns "sinh")     (prim1 (qname ns "sinh")     hydra_lib_math_sinh     #f f64 f64))
            (cons (qname ns "sqrt")     (prim1 (qname ns "sqrt")     hydra_lib_math_sqrt     #f f64 f64))
            (cons (qname ns "subFloat64") (prim2 (qname ns "subFloat64") hydra_lib_math_sub_float64 #f f64 f64 f64))
            (cons (qname ns "tan")      (prim1 (qname ns "tan")      hydra_lib_math_tan      #f f64 f64))
            (cons (qname ns "tanh")     (prim1 (qname ns "tanh")     hydra_lib_math_tanh     #f f64 f64))
            (cons (qname ns "truncate") (prim1 (qname ns "truncate") hydra_lib_math_truncate #f f64 f64))))))

    ;; ============================================================================
    ;; Maybes
    ;; ============================================================================

    (define (register-maybes)
      (let ((ns "hydra.lib.maybes")
            (a (tc-variable "a"))
            (b (tc-variable "b"))
            (c (tc-variable "c")))
        (list
          (cons (qname ns "apply")    (prim2 (qname ns "apply")
                                              hydra_lib_maybes_apply
                                              #f (tc-optional (fun a b)) (tc-optional a) (tc-optional b)))
          (cons (qname ns "bind")     (prim2 (qname ns "bind")
                                              hydra_lib_maybes_bind
                                              #f (tc-optional a) (fun a (tc-optional b)) (tc-optional b)))
          (cons (qname ns "cases")    (prim3 (qname ns "cases")
                                              hydra_lib_maybes_cases
                                              #f (tc-optional a) b (fun a b) b))
          (cons (qname ns "cat")      (prim1 (qname ns "cat")      hydra_lib_maybes_cat      #f (tc-list (tc-optional a)) (tc-list a)))
          (cons (qname ns "compose")  (prim3 (qname ns "compose")
                                              hydra_lib_maybes_compose
                                              #f (fun a (tc-optional b)) (fun b (tc-optional c)) a (tc-optional c)))
          (cons (qname ns "fromJust") (prim1 (qname ns "fromJust") hydra_lib_maybes_from_just #f (tc-optional a) a))
          (cons (qname ns "fromMaybe") (prim2 (qname ns "fromMaybe")
                                               hydra_lib_maybes_from_maybe
                                               #f a (tc-optional a) a))
          (cons (qname ns "isJust")    (prim1 (qname ns "isJust")    hydra_lib_maybes_is_just    #f (tc-optional a) (tc-boolean)))
          (cons (qname ns "isNothing") (prim1 (qname ns "isNothing") hydra_lib_maybes_is_nothing #f (tc-optional a) (tc-boolean)))
          (cons (qname ns "map")       (prim2 (qname ns "map")
                                               hydra_lib_maybes_map
                                               #f (fun a b) (tc-optional a) (tc-optional b)))
          (cons (qname ns "mapMaybe")  (prim2 (qname ns "mapMaybe")
                                               hydra_lib_maybes_map_maybe
                                               #f (fun a (tc-optional b)) (tc-list a) (tc-list b)))
          (cons (qname ns "maybe")     (prim3 (qname ns "maybe")
                                               hydra_lib_maybes_maybe
                                               #f b (fun a b) (tc-optional a) b))
          (cons (qname ns "pure")      (prim1 (qname ns "pure")      hydra_lib_maybes_pure      #f a (tc-optional a)))
          (cons (qname ns "toList")    (prim1 (qname ns "toList")    hydra_lib_maybes_to_list   #f (tc-optional a) (tc-list a))))))

    ;; ============================================================================
    ;; Pairs
    ;; ============================================================================

    (define (register-pairs)
      (let ((ns "hydra.lib.pairs")
            (a (tc-variable "a"))
            (b (tc-variable "b"))
            (c (tc-variable "c"))
            (d (tc-variable "d")))
        (list
          (cons (qname ns "bimap")  (prim3 (qname ns "bimap")
                                            hydra_lib_pairs_bimap
                                            #f (fun a c) (fun b d) (tc-pair a b) (tc-pair c d)))
          (cons (qname ns "first")  (prim1 (qname ns "first")  hydra_lib_pairs_first  #f (tc-pair a b) a))
          (cons (qname ns "second") (prim1 (qname ns "second") hydra_lib_pairs_second #f (tc-pair a b) b)))))

    ;; ============================================================================
    ;; Sets
    ;; ============================================================================

    (define (register-sets)
      (let ((ns "hydra.lib.sets")
            (a (tc-variable "a"))
            (b (tc-variable "b"))
            (ord-a (list (cons "a" (make-hydra_core_type_variable_metadata (list "ordering")))))
            (ord-ab (list (cons "a" (make-hydra_core_type_variable_metadata (list "ordering")))
                          (cons "b" (make-hydra_core_type_variable_metadata (list "ordering"))))))
        (list
          (cons (qname ns "delete")       (prim2 (qname ns "delete")
                                                  hydra_lib_sets_delete
                                                  #f a (tc-set a) (tc-set a) ord-a))
          (cons (qname ns "difference")   (prim2 (qname ns "difference")
                                                  hydra_lib_sets_difference
                                                  #f (tc-set a) (tc-set a) (tc-set a) ord-a))
          (cons (qname ns "empty")        (prim0 (qname ns "empty")   (lambda () hydra_lib_sets_empty)   #f (tc-set a) ord-a))
          (cons (qname ns "fromList")     (prim1 (qname ns "fromList") hydra_lib_sets_from_list #f (tc-list a) (tc-set a) ord-a))
          (cons (qname ns "insert")       (prim2 (qname ns "insert")
                                                  hydra_lib_sets_insert
                                                  #f a (tc-set a) (tc-set a) ord-a))
          (cons (qname ns "intersection") (prim2 (qname ns "intersection")
                                                  hydra_lib_sets_intersection
                                                  #f (tc-set a) (tc-set a) (tc-set a) ord-a))
          (cons (qname ns "map")          (prim2 (qname ns "map")
                                                  hydra_lib_sets_map
                                                  #f (fun a b) (tc-set a) (tc-set b) ord-ab))
          (cons (qname ns "member")       (prim2 (qname ns "member")
                                                  hydra_lib_sets_member
                                                  #f a (tc-set a) (tc-boolean) ord-a))
          (cons (qname ns "null")         (prim1 (qname ns "null")     hydra_lib_sets_null     #f (tc-set a) (tc-boolean) ord-a))
          (cons (qname ns "singleton")    (prim1 (qname ns "singleton") hydra_lib_sets_singleton #f a (tc-set a) ord-a))
          (cons (qname ns "size")         (prim1 (qname ns "size")     hydra_lib_sets_size     #f (tc-set a) (tc-int32) ord-a))
          (cons (qname ns "toList")       (prim1 (qname ns "toList")   hydra_lib_sets_to_list  #f (tc-set a) (tc-list a) ord-a))
          (cons (qname ns "union")        (prim2 (qname ns "union")
                                                  hydra_lib_sets_union
                                                  #f (tc-set a) (tc-set a) (tc-set a) ord-a))
          (cons (qname ns "unions")       (prim1 (qname ns "unions")   hydra_lib_sets_unions   #f (tc-list (tc-set a)) (tc-set a) ord-a)))))

    ;; ============================================================================
    ;; Strings
    ;; ============================================================================

    (define (register-strings)
      (let ((ns "hydra.lib.strings")
            (s (tc-string))
            (i (tc-int32))
            (b (tc-boolean)))
        (list
          (cons (qname ns "cat")         (prim1 (qname ns "cat")         hydra_lib_strings_cat         #f (tc-list s) s))
          (cons (qname ns "cat2")        (prim2 (qname ns "cat2")
                                                  hydra_lib_strings_cat2
                                                  #f s s s))
          (cons (qname ns "charAt")      (prim2 (qname ns "charAt")
                                                  hydra_lib_strings_char_at
                                                  #f i s i))
          (cons (qname ns "fromList")    (prim1 (qname ns "fromList")    hydra_lib_strings_from_list    #f (tc-list i) s))
          (cons (qname ns "intercalate") (prim2 (qname ns "intercalate")
                                                  hydra_lib_strings_intercalate
                                                  #f s (tc-list s) s))
          (cons (qname ns "length")      (prim1 (qname ns "length")      hydra_lib_strings_length      #f s i))
          (cons (qname ns "lines")       (prim1 (qname ns "lines")       hydra_lib_strings_lines       #f s (tc-list s)))
          (cons (qname ns "maybeCharAt") (prim2 (qname ns "maybeCharAt") hydra_lib_strings_maybe_char_at #f i s (tc-optional i)))
          (cons (qname ns "null")        (prim1 (qname ns "null")        hydra_lib_strings_null        #f s b))
          (cons (qname ns "splitOn")     (prim2 (qname ns "splitOn")
                                                  hydra_lib_strings_split_on
                                                  #f s s (tc-list s)))
          (cons (qname ns "toList")      (prim1 (qname ns "toList")      hydra_lib_strings_to_list     #f s (tc-list i)))
          (cons (qname ns "toLower")     (prim1 (qname ns "toLower")     hydra_lib_strings_to_lower    #f s s))
          (cons (qname ns "toUpper")     (prim1 (qname ns "toUpper")     hydra_lib_strings_to_upper    #f s s))
          (cons (qname ns "unlines")     (prim1 (qname ns "unlines")     hydra_lib_strings_unlines     #f (tc-list s) s)))))

    ;; ============================================================================
    ;; Literals
    ;; ============================================================================

    (define (register-literals)
      (let ((ns "hydra.lib.literals")
            (bf  (tc-bigfloat))
            (bi  (tc-bigint))
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
          (list
            (cons (qname ns "bigfloatToBigint")   (prim1 (qname ns "bigfloatToBigint")   hydra_lib_literals_bigfloat_to_bigint   #f bf bi))
            (cons (qname ns "bigfloatToFloat32")  (prim1 (qname ns "bigfloatToFloat32")  hydra_lib_literals_bigfloat_to_float32  #f bf f32))
            (cons (qname ns "bigfloatToFloat64")  (prim1 (qname ns "bigfloatToFloat64")  hydra_lib_literals_bigfloat_to_float64  #f bf f64))
            (cons (qname ns "bigintToBigfloat")   (prim1 (qname ns "bigintToBigfloat")   hydra_lib_literals_bigint_to_bigfloat   #f bi bf))
            (cons (qname ns "bigintToInt8")       (prim1 (qname ns "bigintToInt8")       hydra_lib_literals_bigint_to_int8       #f bi i8))
            (cons (qname ns "bigintToInt16")      (prim1 (qname ns "bigintToInt16")      hydra_lib_literals_bigint_to_int16      #f bi i16))
            (cons (qname ns "bigintToInt32")      (prim1 (qname ns "bigintToInt32")      hydra_lib_literals_bigint_to_int32      #f bi i32))
            (cons (qname ns "bigintToInt64")      (prim1 (qname ns "bigintToInt64")      hydra_lib_literals_bigint_to_int64      #f bi i64))
            (cons (qname ns "bigintToUint8")      (prim1 (qname ns "bigintToUint8")      hydra_lib_literals_bigint_to_uint8      #f bi u8))
            (cons (qname ns "bigintToUint16")     (prim1 (qname ns "bigintToUint16")     hydra_lib_literals_bigint_to_uint16     #f bi u16))
            (cons (qname ns "bigintToUint32")     (prim1 (qname ns "bigintToUint32")     hydra_lib_literals_bigint_to_uint32     #f bi u32))
            (cons (qname ns "bigintToUint64")     (prim1 (qname ns "bigintToUint64")     hydra_lib_literals_bigint_to_uint64     #f bi u64))
            (cons (qname ns "binaryToBytes")      (prim1 (qname ns "binaryToBytes")      hydra_lib_literals_binary_to_bytes      #f bin (tc-list i32)))
            (cons (qname ns "binaryToString")     (prim1 (qname ns "binaryToString")     hydra_lib_literals_binary_to_string     #f bin s))
            (cons (qname ns "float32ToBigfloat")  (prim1 (qname ns "float32ToBigfloat")  hydra_lib_literals_float32_to_bigfloat  #f f32 bf))
            (cons (qname ns "float64ToBigfloat")  (prim1 (qname ns "float64ToBigfloat")  hydra_lib_literals_float64_to_bigfloat  #f f64 bf))
            (cons (qname ns "int8ToBigint")       (prim1 (qname ns "int8ToBigint")       hydra_lib_literals_int8_to_bigint       #f i8 bi))
            (cons (qname ns "int16ToBigint")      (prim1 (qname ns "int16ToBigint")      hydra_lib_literals_int16_to_bigint      #f i16 bi))
            (cons (qname ns "int32ToBigint")      (prim1 (qname ns "int32ToBigint")      hydra_lib_literals_int32_to_bigint      #f i32 bi))
            (cons (qname ns "int64ToBigint")      (prim1 (qname ns "int64ToBigint")      hydra_lib_literals_int64_to_bigint      #f i64 bi))
            (cons (qname ns "uint8ToBigint")      (prim1 (qname ns "uint8ToBigint")      hydra_lib_literals_uint8_to_bigint      #f u8 bi))
            (cons (qname ns "uint16ToBigint")     (prim1 (qname ns "uint16ToBigint")     hydra_lib_literals_uint16_to_bigint     #f u16 bi))
            (cons (qname ns "uint32ToBigint")     (prim1 (qname ns "uint32ToBigint")     hydra_lib_literals_uint32_to_bigint     #f u32 bi))
            (cons (qname ns "uint64ToBigint")     (prim1 (qname ns "uint64ToBigint")     hydra_lib_literals_uint64_to_bigint     #f u64 bi))
            (cons (qname ns "stringToBinary")     (prim1 (qname ns "stringToBinary")     hydra_lib_literals_string_to_binary     #f s bin)))
          (list
            (cons (qname ns "readBigfloat") (prim1 (qname ns "readBigfloat") hydra_lib_literals_read_bigfloat #f s (tc-optional bf)))
            (cons (qname ns "readBigint")   (prim1 (qname ns "readBigint")   hydra_lib_literals_read_bigint   #f s (tc-optional bi)))
            (cons (qname ns "readBoolean")  (prim1 (qname ns "readBoolean")  hydra_lib_literals_read_boolean  #f s (tc-optional b)))
            (cons (qname ns "readFloat32")  (prim1 (qname ns "readFloat32")  hydra_lib_literals_read_float32  #f s (tc-optional f32)))
            (cons (qname ns "readFloat64")  (prim1 (qname ns "readFloat64")  hydra_lib_literals_read_float64  #f s (tc-optional f64)))
            (cons (qname ns "readInt8")     (prim1 (qname ns "readInt8")     hydra_lib_literals_read_int8     #f s (tc-optional i8)))
            (cons (qname ns "readInt16")    (prim1 (qname ns "readInt16")    hydra_lib_literals_read_int16    #f s (tc-optional i16)))
            (cons (qname ns "readInt32")    (prim1 (qname ns "readInt32")    hydra_lib_literals_read_int32    #f s (tc-optional i32)))
            (cons (qname ns "readInt64")    (prim1 (qname ns "readInt64")    hydra_lib_literals_read_int64    #f s (tc-optional i64)))
            (cons (qname ns "readString")   (prim1 (qname ns "readString")   hydra_lib_literals_read_string   #f s (tc-optional s)))
            (cons (qname ns "readUint8")    (prim1 (qname ns "readUint8")    hydra_lib_literals_read_uint8    #f s (tc-optional u8)))
            (cons (qname ns "readUint16")   (prim1 (qname ns "readUint16")   hydra_lib_literals_read_uint16   #f s (tc-optional u16)))
            (cons (qname ns "readUint32")   (prim1 (qname ns "readUint32")   hydra_lib_literals_read_uint32   #f s (tc-optional u32)))
            (cons (qname ns "readUint64")   (prim1 (qname ns "readUint64")   hydra_lib_literals_read_uint64   #f s (tc-optional u64))))
          (list
            (cons (qname ns "showBigfloat") (prim1 (qname ns "showBigfloat") hydra_lib_literals_show_bigfloat #f bf s))
            (cons (qname ns "showBigint")   (prim1 (qname ns "showBigint")   hydra_lib_literals_show_bigint   #f bi s))
            (cons (qname ns "showBoolean")  (prim1 (qname ns "showBoolean")  hydra_lib_literals_show_boolean  #f b s))
            (cons (qname ns "showFloat32")  (prim1 (qname ns "showFloat32")  hydra_lib_literals_show_float32  #f f32 s))
            (cons (qname ns "showFloat64")  (prim1 (qname ns "showFloat64")  hydra_lib_literals_show_float64  #f f64 s))
            (cons (qname ns "showInt8")     (prim1 (qname ns "showInt8")     hydra_lib_literals_show_int8     #f i8 s))
            (cons (qname ns "showInt16")    (prim1 (qname ns "showInt16")    hydra_lib_literals_show_int16    #f i16 s))
            (cons (qname ns "showInt32")    (prim1 (qname ns "showInt32")    hydra_lib_literals_show_int32    #f i32 s))
            (cons (qname ns "showInt64")    (prim1 (qname ns "showInt64")    hydra_lib_literals_show_int64    #f i64 s))
            (cons (qname ns "showUint8")    (prim1 (qname ns "showUint8")    hydra_lib_literals_show_uint8    #f u8 s))
            (cons (qname ns "showUint16")   (prim1 (qname ns "showUint16")   hydra_lib_literals_show_uint16   #f u16 s))
            (cons (qname ns "showUint32")   (prim1 (qname ns "showUint32")   hydra_lib_literals_show_uint32   #f u32 s))
            (cons (qname ns "showUint64")   (prim1 (qname ns "showUint64")   hydra_lib_literals_show_uint64   #f u64 s))
            (cons (qname ns "showString")   (prim1 (qname ns "showString")   hydra_lib_literals_show_string   #f s s))))))

    ;; ============================================================================
    ;; Regex
    ;; ============================================================================

    (define (register-regex)
      (let ((ns "hydra.lib.regex")
            (s (tc-string))
            (b (tc-boolean)))
        (list
          (cons (qname ns "find")       (prim2 (qname ns "find")
                                                hydra_lib_regex_find
                                                #f s s (tc-optional s)))
          (cons (qname ns "findAll")    (prim2 (qname ns "findAll")
                                                hydra_lib_regex_find_all
                                                #f s s (tc-list s)))
          (cons (qname ns "matches")    (prim2 (qname ns "matches")
                                                hydra_lib_regex_matches
                                                #f s s b))
          (cons (qname ns "replace")    (prim3 (qname ns "replace")
                                                hydra_lib_regex_replace
                                                #f s s s s))
          (cons (qname ns "replaceAll") (prim3 (qname ns "replaceAll")
                                                hydra_lib_regex_replace_all
                                                #f s s s s))
          (cons (qname ns "split")      (prim2 (qname ns "split")
                                                hydra_lib_regex_split
                                                #f s s (tc-list s))))))

    ;; ============================================================================
    ;; Standard library: all primitives combined
    ;; ============================================================================

    (define (standard-library)
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
        (register-strings)))

)
)
