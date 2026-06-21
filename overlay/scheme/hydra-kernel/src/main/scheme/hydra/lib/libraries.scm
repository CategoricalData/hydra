(define-library (hydra lib libraries)
  (import (scheme base)
          (hydra core)
          (hydra graph)
          (hydra packaging)  ; #473: hydra_packaging_primitive_definition-name accessor
          (hydra prims)
          (hydra reduction)
          (hydra scheme lib chars)
          (hydra scheme lib effects)
          (hydra scheme lib eithers)
          (hydra scheme lib equality)
          (hydra scheme lib files)
          (hydra scheme lib lists)
          (hydra scheme lib literals)
          (hydra scheme lib logic)
          (hydra scheme lib maps)
          (hydra scheme lib math)
          (hydra scheme lib optionals)
          (hydra scheme lib pairs)
          (hydra scheme lib regex)
          (hydra scheme lib sets)
          (hydra scheme lib strings)
          (hydra scheme lib text)
          ;; #473: import the generated hydra.lib.* PrimitiveDefinition def-modules under a `def:`
          ;; prefix (their exported names collide with the impl libs above) so the registry can derive
          ;; each primitive's canonical name from its definition — the single source of truth.
          (prefix (hydra lib chars) def:)
          (prefix (hydra lib effects) def:)
          (prefix (hydra lib eithers) def:)
          (prefix (hydra lib equality) def:)
          (prefix (hydra lib files) def:)
          (prefix (hydra lib lists) def:)
          (prefix (hydra lib literals) def:)
          (prefix (hydra lib logic) def:)
          (prefix (hydra lib maps) def:)
          (prefix (hydra lib math) def:)
          (prefix (hydra lib optionals) def:)
          (prefix (hydra lib pairs) def:)
          (prefix (hydra lib regex) def:)
          (prefix (hydra lib sets) def:)
          (prefix (hydra lib strings) def:)
          (prefix (hydra lib text) def:))
  (export standard-library)
  (begin

    ;; ============================================================================
    ;; Helpers
    ;; ============================================================================

    (define (prim-name def)
      ;; Derive a primitive's canonical name from its generated PrimitiveDefinition (#473).
      (hydra_packaging_primitive_definition-name def))

    (define (fun dom cod)
      (tc-function-with-reduce
        (lambda (cx g t) ((((hydra_reduction_reduce_term cx) g) #t) t))
        dom cod))

    ;; Wrap class names into TypeClassConstraint.simple variants (#156).
    (define (constraints . classes)
      (map (lambda (c) (list 'simple c)) classes))

    ;; ============================================================================
    ;; Chars
    ;; ============================================================================

    (define (register-chars)
      (let ()
        (list
          (cons (prim-name def:hydra_lib_chars_is_alpha_num) (prim1 (prim-name def:hydra_lib_chars_is_alpha_num) hydra_lib_chars_is_alpha_num #f (tc-int32) (tc-boolean)))
          (cons (prim-name def:hydra_lib_chars_is_lower)    (prim1 (prim-name def:hydra_lib_chars_is_lower)    hydra_lib_chars_is_lower    #f (tc-int32) (tc-boolean)))
          (cons (prim-name def:hydra_lib_chars_is_space)    (prim1 (prim-name def:hydra_lib_chars_is_space)    hydra_lib_chars_is_space    #f (tc-int32) (tc-boolean)))
          (cons (prim-name def:hydra_lib_chars_is_upper)    (prim1 (prim-name def:hydra_lib_chars_is_upper)    hydra_lib_chars_is_upper    #f (tc-int32) (tc-boolean)))
          (cons (prim-name def:hydra_lib_chars_to_lower)    (prim1 (prim-name def:hydra_lib_chars_to_lower)    hydra_lib_chars_to_lower    #f (tc-int32) (tc-int32)))
          (cons (prim-name def:hydra_lib_chars_to_upper)    (prim1 (prim-name def:hydra_lib_chars_to_upper)    hydra_lib_chars_to_upper    #f (tc-int32) (tc-int32))))))

    ;; ============================================================================
    ;; Effects (#494)
    ;; ============================================================================
    ;;
    ;; effect<t> is transparent in Scheme (effect<t> = t). These are registered so the inference
    ;; graph can resolve the hydra.lib.effects.* names; their type schemes match the kernel
    ;; signatures exactly (note pure is x -> effect<x>, including the function arrow). The real
    ;; evaluation happens through the relocated hydra.scheme.lib.effects runtime, reached via the
    ;; bootstrap redirect; the impls wired here are consistent with that runtime.

    (define (register-effects)
      (let (
            (x (tc-variable "x"))
            (y (tc-variable "y"))
            (z (tc-variable "z")))
        (let ((eff (lambda (c) (tc-effect c))))
          (list
            (cons (prim-name def:hydra_lib_effects_apply)   (prim2 (prim-name def:hydra_lib_effects_apply)
                                               hydra_lib_effects_apply
                                               #f (eff (tc-function x y)) (eff x) (eff y)))
            (cons (prim-name def:hydra_lib_effects_bind)    (prim2 (prim-name def:hydra_lib_effects_bind)
                                               hydra_lib_effects_bind
                                               #f (eff x) (tc-function x (eff y)) (eff y)))
            (cons (prim-name def:hydra_lib_effects_compose) (prim3 (prim-name def:hydra_lib_effects_compose)
                                               hydra_lib_effects_compose
                                               #f (tc-function x (eff y)) (tc-function y (eff z)) x (eff z)))
            (cons (prim-name def:hydra_lib_effects_foldl)   (prim3 (prim-name def:hydra_lib_effects_foldl)
                                               hydra_lib_effects_foldl
                                               #f (tc-function x (tc-function y (eff x))) x (tc-list y) (eff x)))
            (cons (prim-name def:hydra_lib_effects_map)     (prim2 (prim-name def:hydra_lib_effects_map)
                                               hydra_lib_effects_map
                                               #f (tc-function x y) (eff x) (eff y)))
            (cons (prim-name def:hydra_lib_effects_map_list) (prim2 (prim-name def:hydra_lib_effects_map_list)
                                               hydra_lib_effects_map_list
                                               #f (tc-function x (eff y)) (tc-list x) (eff (tc-list y))))
            (cons (prim-name def:hydra_lib_effects_map_optional) (prim2 (prim-name def:hydra_lib_effects_map_optional)
                                               hydra_lib_effects_map_optional
                                               #f (tc-function x (eff y)) (tc-optional x) (eff (tc-optional y))))
            (cons (prim-name def:hydra_lib_effects_pure)    (prim1 (prim-name def:hydra_lib_effects_pure)
                                               hydra_lib_effects_pure
                                               #f x (eff x)))))))

    ;; ============================================================================
    ;; Eithers
    ;; ============================================================================

    (define (register-eithers)
      (let (
            (x (tc-variable "x"))
            (y (tc-variable "y"))
            (z (tc-variable "z"))
            (w (tc-variable "w")))
        (list
          (cons (prim-name def:hydra_lib_eithers_bind)    (prim2 (prim-name def:hydra_lib_eithers_bind)
                                             hydra_lib_eithers_bind
                                             #f (tc-either x y) (fun y (tc-either x z)) (tc-either x z)))
          (cons (prim-name def:hydra_lib_eithers_bimap)   (prim3 (prim-name def:hydra_lib_eithers_bimap)
                                             hydra_lib_eithers_bimap
                                             #f (fun x z) (fun y w) (tc-either x y) (tc-either z w)))
          (cons (prim-name def:hydra_lib_eithers_either)  (prim3 (prim-name def:hydra_lib_eithers_either)
                                             hydra_lib_eithers_either
                                             #f (fun x z) (fun y z) (tc-either x y) z))
          (cons (prim-name def:hydra_lib_eithers_foldl)   (prim3 (prim-name def:hydra_lib_eithers_foldl)
                                             hydra_lib_eithers_foldl
                                             #f (fun x (fun y (tc-either z x))) x (tc-list y) (tc-either z x)))
          (cons (prim-name def:hydra_lib_eithers_from_left)  (prim2 (prim-name def:hydra_lib_eithers_from_left)
                                               hydra_lib_eithers_from_left
                                               #f x (tc-either x y) x))
          (cons (prim-name def:hydra_lib_eithers_from_right) (prim2 (prim-name def:hydra_lib_eithers_from_right)
                                               hydra_lib_eithers_from_right
                                               #f y (tc-either x y) y))
          (cons (prim-name def:hydra_lib_eithers_is_left)  (prim1 (prim-name def:hydra_lib_eithers_is_left)  hydra_lib_eithers_is_left  #f (tc-either x y) (tc-boolean)))
          (cons (prim-name def:hydra_lib_eithers_is_right) (prim1 (prim-name def:hydra_lib_eithers_is_right) hydra_lib_eithers_is_right #f (tc-either x y) (tc-boolean)))
          (cons (prim-name def:hydra_lib_eithers_lefts)   (prim1 (prim-name def:hydra_lib_eithers_lefts)   hydra_lib_eithers_lefts   #f (tc-list (tc-either x y)) (tc-list x)))
          (cons (prim-name def:hydra_lib_eithers_map)     (prim2 (prim-name def:hydra_lib_eithers_map)
                                             hydra_lib_eithers_map
                                             #f (fun x y) (tc-either z x) (tc-either z y)))
          (cons (prim-name def:hydra_lib_eithers_map_list) (prim2 (prim-name def:hydra_lib_eithers_map_list)
                                             hydra_lib_eithers_map_list
                                             #f (fun x (tc-either z y)) (tc-list x) (tc-either z (tc-list y))))
          (cons (prim-name def:hydra_lib_eithers_map_optional) (prim2 (prim-name def:hydra_lib_eithers_map_optional)
                                              hydra_lib_eithers_map_optional
                                              #f (fun x (tc-either z y)) (tc-optional x) (tc-either z (tc-optional y))))
          (cons (prim-name def:hydra_lib_eithers_map_set)  (prim2 (prim-name def:hydra_lib_eithers_map_set)
                                             hydra_lib_eithers_map_set
                                             #f (fun x (tc-either z y)) (tc-set x) (tc-either z (tc-set y))))
          (cons (prim-name def:hydra_lib_eithers_partition_eithers) (prim1 (prim-name def:hydra_lib_eithers_partition_eithers)
                                                      hydra_lib_eithers_partition_eithers
                                                      #f (tc-list (tc-either x y)) (tc-pair (tc-list x) (tc-list y))))
          (cons (prim-name def:hydra_lib_eithers_rights)  (prim1 (prim-name def:hydra_lib_eithers_rights)  hydra_lib_eithers_rights  #f (tc-list (tc-either x y)) (tc-list y))))))

    ;; ============================================================================
    ;; Equality
    ;; ============================================================================

    (define (register-equality)
      (let (
            (x (tc-variable "x"))
            (ord-x (list (list "x" (make-hydra_core_type_variable_constraints (constraints "ordering")))))
            (eq-x  (list (list "x" (make-hydra_core_type_variable_constraints (constraints "equality"))))))
        (list
          (cons (prim-name def:hydra_lib_equality_compare)  (prim2 (prim-name def:hydra_lib_equality_compare)  hydra_lib_equality_compare  #f x x (tc-comparison) ord-x))
          (cons (prim-name def:hydra_lib_equality_equal)    (prim2 (prim-name def:hydra_lib_equality_equal)    hydra_lib_equality_equal    #f x x (tc-boolean) eq-x))
          (cons (prim-name def:hydra_lib_equality_gt)       (prim2 (prim-name def:hydra_lib_equality_gt)       hydra_lib_equality_gt       #f x x (tc-boolean) ord-x))
          (cons (prim-name def:hydra_lib_equality_gte)      (prim2 (prim-name def:hydra_lib_equality_gte)      hydra_lib_equality_gte      #f x x (tc-boolean) ord-x))
          (cons (prim-name def:hydra_lib_equality_identity) (prim1 (prim-name def:hydra_lib_equality_identity) (lambda (x) x)             #f x x))
          (cons (prim-name def:hydra_lib_equality_lt)       (prim2 (prim-name def:hydra_lib_equality_lt)       hydra_lib_equality_lt       #f x x (tc-boolean) ord-x))
          (cons (prim-name def:hydra_lib_equality_lte)      (prim2 (prim-name def:hydra_lib_equality_lte)      hydra_lib_equality_lte      #f x x (tc-boolean) ord-x))
          (cons (prim-name def:hydra_lib_equality_max)      (prim2 (prim-name def:hydra_lib_equality_max)      hydra_lib_equality_max      #f x x x ord-x))
          (cons (prim-name def:hydra_lib_equality_min)      (prim2 (prim-name def:hydra_lib_equality_min)      hydra_lib_equality_min      #f x x x ord-x)))))

    ;; ============================================================================
    ;; Files (#494)
    ;; ============================================================================
    ;;
    ;; FilePath and FileError are nominal kernel types (referenced by name). unit maps to '(),
    ;; binary to a bytevector, either to the (list 'left/'right) representation. As with effects,
    ;; the type schemes are registered for inference name-resolution; the real I/O happens in
    ;; hydra.scheme.lib.files, reached via the bootstrap redirect.

    (define (register-files)
      (let (
            (bool (tc-boolean))
            (bin (tc-binary))
            (fp (tc-named "hydra.file.FilePath"))
            (ferr (tc-named "hydra.error.file.FileError"))
            (unit (tc-unit)))
        (let ((eff (lambda (c) (tc-effect c))))
          (list
            (cons (prim-name def:hydra_lib_files_append_file) (prim2 (prim-name def:hydra_lib_files_append_file)
                                               (lambda (path) (lambda (contents) ((hydra_lib_files_append_file path) contents)))
                                               #f fp bin (eff (tc-either ferr unit))))
            (cons (prim-name def:hydra_lib_files_create_directory) (prim2 (prim-name def:hydra_lib_files_create_directory)
                                               (lambda (recursive) (lambda (path) ((hydra_lib_files_create_directory recursive) path)))
                                               #f bool fp (eff (tc-either ferr unit))))
            (cons (prim-name def:hydra_lib_files_exists) (prim1 (prim-name def:hydra_lib_files_exists)
                                               hydra_lib_files_exists
                                               #f fp (eff (tc-either ferr bool))))
            (cons (prim-name def:hydra_lib_files_list_directory) (prim1 (prim-name def:hydra_lib_files_list_directory)
                                               hydra_lib_files_list_directory
                                               #f fp (eff (tc-either ferr (tc-list fp)))))
            (cons (prim-name def:hydra_lib_files_read_file) (prim1 (prim-name def:hydra_lib_files_read_file)
                                               hydra_lib_files_read_file
                                               #f fp (eff (tc-either ferr bin))))
            (cons (prim-name def:hydra_lib_files_remove_file) (prim1 (prim-name def:hydra_lib_files_remove_file)
                                               hydra_lib_files_remove_file
                                               #f fp (eff (tc-either ferr unit))))
            (cons (prim-name def:hydra_lib_files_rename) (prim2 (prim-name def:hydra_lib_files_rename)
                                               (lambda (source) (lambda (destination) ((hydra_lib_files_rename source) destination)))
                                               #f fp fp (eff (tc-either ferr unit))))
            (cons (prim-name def:hydra_lib_files_write_file) (prim2 (prim-name def:hydra_lib_files_write_file)
                                               (lambda (path) (lambda (contents) ((hydra_lib_files_write_file path) contents)))
                                               #f fp bin (eff (tc-either ferr unit))))))))

    ;; ============================================================================
    ;; Lists
    ;; ============================================================================

    (define (register-lists)
      (let (
            (a (tc-variable "a"))
            (b (tc-variable "b"))
            (c (tc-variable "c"))
            (ord-a (list (list "a" (make-hydra_core_type_variable_constraints (constraints "ordering")))))
            (eq-a  (list (list "a" (make-hydra_core_type_variable_constraints (constraints "equality"))))))
        (list
          (cons (prim-name def:hydra_lib_lists_apply)      (prim2 (prim-name def:hydra_lib_lists_apply)
                                                 hydra_lib_lists_apply
                                                 #f (tc-list (fun a b)) (tc-list a) (tc-list b)))
          (cons (prim-name def:hydra_lib_lists_bind)       (prim2 (prim-name def:hydra_lib_lists_bind)
                                                 hydra_lib_lists_bind
                                                 #f (tc-list a) (fun a (tc-list b)) (tc-list b)))
          (cons (prim-name def:hydra_lib_lists_concat)     (prim1 (prim-name def:hydra_lib_lists_concat)     hydra_lib_lists_concat     #f (tc-list (tc-list a)) (tc-list a)))
          (cons (prim-name def:hydra_lib_lists_concat2)    (prim2 (prim-name def:hydra_lib_lists_concat2)
                                                 hydra_lib_lists_concat2
                                                 #f (tc-list a) (tc-list a) (tc-list a)))
          (cons (prim-name def:hydra_lib_lists_cons)       (prim2 (prim-name def:hydra_lib_lists_cons)
                                                 hydra_lib_lists_cons
                                                 #f a (tc-list a) (tc-list a)))
          (cons (prim-name def:hydra_lib_lists_drop)       (prim2 (prim-name def:hydra_lib_lists_drop)
                                                 hydra_lib_lists_drop
                                                 #f (tc-int32) (tc-list a) (tc-list a)))
          (cons (prim-name def:hydra_lib_lists_drop_while)  (prim2 (prim-name def:hydra_lib_lists_drop_while)
                                                 hydra_lib_lists_drop_while
                                                 #f (fun a (tc-boolean)) (tc-list a) (tc-list a)))
          (cons (prim-name def:hydra_lib_lists_elem)       (prim2 (prim-name def:hydra_lib_lists_elem)
                                                 hydra_lib_lists_elem
                                                 #f a (tc-list a) (tc-boolean) eq-a))
          (cons (prim-name def:hydra_lib_lists_filter)     (prim2 (prim-name def:hydra_lib_lists_filter)
                                                 hydra_lib_lists_filter
                                                 #f (fun a (tc-boolean)) (tc-list a) (tc-list a)))
          (cons (prim-name def:hydra_lib_lists_find)       (prim2 (prim-name def:hydra_lib_lists_find)
                                                 hydra_lib_lists_find
                                                 #f (fun a (tc-boolean)) (tc-list a) (tc-optional a)))
          (cons (prim-name def:hydra_lib_lists_foldl)      (prim3 (prim-name def:hydra_lib_lists_foldl)
                                                 (lambda (f)
                                                   (lambda (init)
                                                     (lambda (xs)
                                                       (((hydra_lib_lists_foldl
                                                           (lambda (acc) (lambda (el) ((f acc) el))))
                                                         init) xs))))
                                                 #f (fun b (fun a b)) b (tc-list a) b))
          (cons (prim-name def:hydra_lib_lists_foldr)      (prim3 (prim-name def:hydra_lib_lists_foldr)
                                                 (lambda (f)
                                                   (lambda (init)
                                                     (lambda (xs)
                                                       (((hydra_lib_lists_foldr
                                                           (lambda (el) (lambda (acc) ((f el) acc))))
                                                         init) xs))))
                                                 #f (fun a (fun b b)) b (tc-list a) b))
          (cons (prim-name def:hydra_lib_lists_group)      (prim1 (prim-name def:hydra_lib_lists_group)      hydra_lib_lists_group      #f (tc-list a) (tc-list (tc-list a)) eq-a))
          (cons (prim-name def:hydra_lib_lists_intercalate) (prim2 (prim-name def:hydra_lib_lists_intercalate)
                                                  hydra_lib_lists_intercalate
                                                  #f (tc-list a) (tc-list (tc-list a)) (tc-list a)))
          (cons (prim-name def:hydra_lib_lists_intersperse) (prim2 (prim-name def:hydra_lib_lists_intersperse)
                                                  hydra_lib_lists_intersperse
                                                  #f a (tc-list a) (tc-list a)))
          (cons (prim-name def:hydra_lib_lists_length)     (prim1 (prim-name def:hydra_lib_lists_length)     hydra_lib_lists_length     #f (tc-list a) (tc-int32)))
          (cons (prim-name def:hydra_lib_lists_map)        (prim2 (prim-name def:hydra_lib_lists_map)
                                                 hydra_lib_lists_map
                                                 #f (fun a b) (tc-list a) (tc-list b)))
          (cons (prim-name def:hydra_lib_lists_maybe_at)    (prim2 (prim-name def:hydra_lib_lists_maybe_at)    hydra_lib_lists_maybe_at   #f (tc-int32) (tc-list a) (tc-optional a)))
          (cons (prim-name def:hydra_lib_lists_maybe_head)  (prim1 (prim-name def:hydra_lib_lists_maybe_head)  hydra_lib_lists_maybe_head #f (tc-list a) (tc-optional a)))
          (cons (prim-name def:hydra_lib_lists_maybe_init)  (prim1 (prim-name def:hydra_lib_lists_maybe_init)  hydra_lib_lists_maybe_init #f (tc-list a) (tc-optional (tc-list a))))
          (cons (prim-name def:hydra_lib_lists_maybe_last)  (prim1 (prim-name def:hydra_lib_lists_maybe_last)  hydra_lib_lists_maybe_last #f (tc-list a) (tc-optional a)))
          (cons (prim-name def:hydra_lib_lists_maybe_tail)  (prim1 (prim-name def:hydra_lib_lists_maybe_tail)  hydra_lib_lists_maybe_tail #f (tc-list a) (tc-optional (tc-list a))))
          (cons (prim-name def:hydra_lib_lists_nub)        (prim1 (prim-name def:hydra_lib_lists_nub)        hydra_lib_lists_nub        #f (tc-list a) (tc-list a) eq-a))
          (cons (prim-name def:hydra_lib_lists_null)       (prim1 (prim-name def:hydra_lib_lists_null)       hydra_lib_lists_null       #f (tc-list a) (tc-boolean)))
          (cons (prim-name def:hydra_lib_lists_partition)   (prim2 (prim-name def:hydra_lib_lists_partition)
                                                  hydra_lib_lists_partition
                                                  #f (fun a (tc-boolean)) (tc-list a) (tc-pair (tc-list a) (tc-list a))))
          (cons (prim-name def:hydra_lib_lists_pure)       (prim1 (prim-name def:hydra_lib_lists_pure)       hydra_lib_lists_pure       #f a (tc-list a)))
          (cons (prim-name def:hydra_lib_lists_replicate)  (prim2 (prim-name def:hydra_lib_lists_replicate)
                                                 hydra_lib_lists_replicate
                                                 #f (tc-int32) a (tc-list a)))
          (cons (prim-name def:hydra_lib_lists_reverse)    (prim1 (prim-name def:hydra_lib_lists_reverse)    hydra_lib_lists_reverse    #f (tc-list a) (tc-list a)))
          (cons (prim-name def:hydra_lib_lists_singleton)  (prim1 (prim-name def:hydra_lib_lists_singleton)  hydra_lib_lists_singleton  #f a (tc-list a)))
          (cons (prim-name def:hydra_lib_lists_sort)       (prim1 (prim-name def:hydra_lib_lists_sort)       hydra_lib_lists_sort       #f (tc-list a) (tc-list a) ord-a))
          (cons (prim-name def:hydra_lib_lists_sort_on)     (prim2 (prim-name def:hydra_lib_lists_sort_on)
                                                 hydra_lib_lists_sort_on
                                                 #f (fun a b) (tc-list a) (tc-list a)))
          (cons (prim-name def:hydra_lib_lists_span)       (prim2 (prim-name def:hydra_lib_lists_span)
                                                 hydra_lib_lists_span
                                                 #f (fun a (tc-boolean)) (tc-list a) (tc-pair (tc-list a) (tc-list a))))
          (cons (prim-name def:hydra_lib_lists_take)       (prim2 (prim-name def:hydra_lib_lists_take)
                                                 hydra_lib_lists_take
                                                 #f (tc-int32) (tc-list a) (tc-list a)))
          (cons (prim-name def:hydra_lib_lists_transpose)  (prim1 (prim-name def:hydra_lib_lists_transpose)  hydra_lib_lists_transpose  #f (tc-list (tc-list a)) (tc-list (tc-list a))))
          (cons (prim-name def:hydra_lib_lists_uncons)     (prim1 (prim-name def:hydra_lib_lists_uncons)     hydra_lib_lists_uncons     #f (tc-list a) (tc-optional (tc-pair a (tc-list a)))))
          (cons (prim-name def:hydra_lib_lists_zip)        (prim2 (prim-name def:hydra_lib_lists_zip)
                                                 hydra_lib_lists_zip
                                                 #f (tc-list a) (tc-list b) (tc-list (tc-pair a b))))
          (cons (prim-name def:hydra_lib_lists_zip_with)    (prim3 (prim-name def:hydra_lib_lists_zip_with)
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
      (let (
            (a (tc-variable "a")))
        (list
          (cons (prim-name def:hydra_lib_logic_and)    (prim2 (prim-name def:hydra_lib_logic_and)
                                            hydra_lib_logic_and
                                            #f (tc-boolean) (tc-boolean) (tc-boolean)))
          (cons (prim-name def:hydra_lib_logic_if_else) (prim3 (prim-name def:hydra_lib_logic_if_else)
                                            hydra_lib_logic_if_else
                                            #f (tc-boolean) a a a))
          (cons (prim-name def:hydra_lib_logic_not)    (prim1 (prim-name def:hydra_lib_logic_not)    hydra_lib_logic_not #f (tc-boolean) (tc-boolean)))
          (cons (prim-name def:hydra_lib_logic_or)     (prim2 (prim-name def:hydra_lib_logic_or)
                                            hydra_lib_logic_or
                                            #f (tc-boolean) (tc-boolean) (tc-boolean))))))

    ;; ============================================================================
    ;; Maps
    ;; ============================================================================

    (define (register-maps)
      (let (
            (k  (tc-variable "k"))
            (k1 (tc-variable "k1"))
            (k2 (tc-variable "k2"))
            (v  (tc-variable "v"))
            (v1 (tc-variable "v1"))
            (v2 (tc-variable "v2"))
            (ord-k (list (list "k" (make-hydra_core_type_variable_constraints (constraints "ordering")))))
            (ord-k1k2 (list (list "k1" (make-hydra_core_type_variable_constraints (constraints "ordering")))
                            (list "k2" (make-hydra_core_type_variable_constraints (constraints "ordering"))))))
        (let ((map-kv (tc-map k v)))
          (list
            (cons (prim-name def:hydra_lib_maps_alter)          (prim3 (prim-name def:hydra_lib_maps_alter)
                                                      hydra_lib_maps_alter
                                                      #f (fun (tc-optional v) (tc-optional v)) k map-kv map-kv ord-k))
            (cons (prim-name def:hydra_lib_maps_bimap)          (prim3 (prim-name def:hydra_lib_maps_bimap)
                                                      hydra_lib_maps_bimap
                                                      #f (fun k1 k2) (fun v1 v2) (tc-map k1 v1) (tc-map k2 v2) ord-k1k2))
            (cons (prim-name def:hydra_lib_maps_delete)         (prim2 (prim-name def:hydra_lib_maps_delete)
                                                      hydra_lib_maps_delete
                                                      #f k map-kv map-kv ord-k))
            (cons (prim-name def:hydra_lib_maps_elems)          (prim1 (prim-name def:hydra_lib_maps_elems)  hydra_lib_maps_elems  #f map-kv (tc-list v) ord-k))
            (cons (prim-name def:hydra_lib_maps_empty)          (prim0 (prim-name def:hydra_lib_maps_empty)  (lambda () hydra_lib_maps_empty)  #f map-kv ord-k))
            (cons (prim-name def:hydra_lib_maps_filter)         (prim2 (prim-name def:hydra_lib_maps_filter)
                                                      hydra_lib_maps_filter
                                                      #f (fun v (tc-boolean)) map-kv map-kv ord-k))
            (cons (prim-name def:hydra_lib_maps_filter_with_key)  (prim2 (prim-name def:hydra_lib_maps_filter_with_key)
                                                      hydra_lib_maps_filter_with_key
                                                      #f (fun k (fun v (tc-boolean))) map-kv map-kv ord-k))
            (cons (prim-name def:hydra_lib_maps_find_with_default) (prim3 (prim-name def:hydra_lib_maps_find_with_default)
                                                       hydra_lib_maps_find_with_default
                                                       #f v k map-kv v ord-k))
            (cons (prim-name def:hydra_lib_maps_from_list)       (prim1 (prim-name def:hydra_lib_maps_from_list) hydra_lib_maps_from_list #f (tc-list (tc-pair k v)) map-kv ord-k))
            (cons (prim-name def:hydra_lib_maps_insert)         (prim3 (prim-name def:hydra_lib_maps_insert)
                                                      hydra_lib_maps_insert
                                                      #f k v map-kv map-kv ord-k))
            (cons (prim-name def:hydra_lib_maps_keys)           (prim1 (prim-name def:hydra_lib_maps_keys)   hydra_lib_maps_keys   #f map-kv (tc-list k) ord-k))
            (cons (prim-name def:hydra_lib_maps_lookup)         (prim2 (prim-name def:hydra_lib_maps_lookup)
                                                      hydra_lib_maps_lookup
                                                      #f k map-kv (tc-optional v) ord-k))
            (cons (prim-name def:hydra_lib_maps_map)            (prim2 (prim-name def:hydra_lib_maps_map)
                                                      hydra_lib_maps_map
                                                      #f (fun v1 v2) (tc-map k v1) (tc-map k v2) ord-k))
            (cons (prim-name def:hydra_lib_maps_map_keys)        (prim2 (prim-name def:hydra_lib_maps_map_keys)
                                                      hydra_lib_maps_map_keys
                                                      #f (fun k1 k2) (tc-map k1 v) (tc-map k2 v) ord-k1k2))
            (cons (prim-name def:hydra_lib_maps_member)         (prim2 (prim-name def:hydra_lib_maps_member)
                                                      hydra_lib_maps_member
                                                      #f k map-kv (tc-boolean) ord-k))
            (cons (prim-name def:hydra_lib_maps_null)           (prim1 (prim-name def:hydra_lib_maps_null)   hydra_lib_maps_null   #f map-kv (tc-boolean) ord-k))
            (cons (prim-name def:hydra_lib_maps_singleton)      (prim2 (prim-name def:hydra_lib_maps_singleton)
                                                      hydra_lib_maps_singleton
                                                      #f k v map-kv ord-k))
            (cons (prim-name def:hydra_lib_maps_size)           (prim1 (prim-name def:hydra_lib_maps_size)   hydra_lib_maps_size   #f map-kv (tc-int32) ord-k))
            (cons (prim-name def:hydra_lib_maps_to_list)         (prim1 (prim-name def:hydra_lib_maps_to_list) hydra_lib_maps_to_list #f map-kv (tc-list (tc-pair k v)) ord-k))
            (cons (prim-name def:hydra_lib_maps_union)          (prim2 (prim-name def:hydra_lib_maps_union)
                                                      hydra_lib_maps_union
                                                      #f map-kv map-kv map-kv ord-k))))))

    ;; ============================================================================
    ;; Math
    ;; ============================================================================

    (define (register-math)
      (let (
            (i32 (tc-int32))
            (f32 (tc-float32))
            (f64 (tc-float64))
            (bi  (tc-bigint))
            (b   (tc-boolean)))
        (append
          (list
            (cons (prim-name def:hydra_lib_math_abs)    (prim1 (prim-name def:hydra_lib_math_abs)    hydra_lib_math_abs    #f i32 i32))
            (cons (prim-name def:hydra_lib_math_add)    (prim2 (prim-name def:hydra_lib_math_add)    hydra_lib_math_add    #f i32 i32 i32))
            (cons (prim-name def:hydra_lib_math_even)   (prim1 (prim-name def:hydra_lib_math_even)   hydra_lib_math_even   #f i32 b))
            (cons (prim-name def:hydra_lib_math_mul)    (prim2 (prim-name def:hydra_lib_math_mul)    hydra_lib_math_mul    #f i32 i32 i32))
            (cons (prim-name def:hydra_lib_math_negate) (prim1 (prim-name def:hydra_lib_math_negate) hydra_lib_math_negate #f i32 i32))
            (cons (prim-name def:hydra_lib_math_odd)    (prim1 (prim-name def:hydra_lib_math_odd)    hydra_lib_math_odd    #f i32 b))
            (cons (prim-name def:hydra_lib_math_range)  (prim2 (prim-name def:hydra_lib_math_range)  hydra_lib_math_range  #f i32 i32 (tc-list i32)))
            (cons (prim-name def:hydra_lib_math_signum) (prim1 (prim-name def:hydra_lib_math_signum) hydra_lib_math_signum #f i32 i32))
            (cons (prim-name def:hydra_lib_math_sub)    (prim2 (prim-name def:hydra_lib_math_sub)    hydra_lib_math_sub    #f i32 i32 i32))
            (cons (prim-name def:hydra_lib_math_max)    (prim2 (prim-name def:hydra_lib_math_max)    hydra_lib_math_max    #f i32 i32 i32))
            (cons (prim-name def:hydra_lib_math_maybe_div)  (prim2 (prim-name def:hydra_lib_math_maybe_div)  hydra_lib_math_maybe_div  #f i32 i32 (tc-optional i32)))
            (cons (prim-name def:hydra_lib_math_maybe_mod)  (prim2 (prim-name def:hydra_lib_math_maybe_mod)  hydra_lib_math_maybe_mod  #f i32 i32 (tc-optional i32)))
            (cons (prim-name def:hydra_lib_math_maybe_pred) (prim1 (prim-name def:hydra_lib_math_maybe_pred) hydra_lib_math_maybe_pred #f i32 (tc-optional i32)))
            (cons (prim-name def:hydra_lib_math_maybe_rem)  (prim2 (prim-name def:hydra_lib_math_maybe_rem)  hydra_lib_math_maybe_rem  #f i32 i32 (tc-optional i32)))
            (cons (prim-name def:hydra_lib_math_maybe_succ) (prim1 (prim-name def:hydra_lib_math_maybe_succ) hydra_lib_math_maybe_succ #f i32 (tc-optional i32)))
            (cons (prim-name def:hydra_lib_math_min)    (prim2 (prim-name def:hydra_lib_math_min)    hydra_lib_math_min    #f i32 i32 i32)))
          (list
            (cons (prim-name def:hydra_lib_math_acos)     (prim1 (prim-name def:hydra_lib_math_acos)     hydra_lib_math_acos     #f f64 f64))
            (cons (prim-name def:hydra_lib_math_acosh)    (prim1 (prim-name def:hydra_lib_math_acosh)    hydra_lib_math_acosh    #f f64 f64))
            (cons (prim-name def:hydra_lib_math_add_float64) (prim2 (prim-name def:hydra_lib_math_add_float64) hydra_lib_math_add_float64 #f f64 f64 f64))
            (cons (prim-name def:hydra_lib_math_asin)     (prim1 (prim-name def:hydra_lib_math_asin)     hydra_lib_math_asin     #f f64 f64))
            (cons (prim-name def:hydra_lib_math_asinh)    (prim1 (prim-name def:hydra_lib_math_asinh)    hydra_lib_math_asinh    #f f64 f64))
            (cons (prim-name def:hydra_lib_math_atan)     (prim1 (prim-name def:hydra_lib_math_atan)     hydra_lib_math_atan     #f f64 f64))
            (cons (prim-name def:hydra_lib_math_atan2)    (prim2 (prim-name def:hydra_lib_math_atan2)    hydra_lib_math_atan2    #f f64 f64 f64))
            (cons (prim-name def:hydra_lib_math_atanh)    (prim1 (prim-name def:hydra_lib_math_atanh)    hydra_lib_math_atanh    #f f64 f64))
            (cons (prim-name def:hydra_lib_math_ceiling)  (prim1 (prim-name def:hydra_lib_math_ceiling)  hydra_lib_math_ceiling  #f f64 f64))
            (cons (prim-name def:hydra_lib_math_cos)      (prim1 (prim-name def:hydra_lib_math_cos)      hydra_lib_math_cos      #f f64 f64))
            (cons (prim-name def:hydra_lib_math_cosh)     (prim1 (prim-name def:hydra_lib_math_cosh)     hydra_lib_math_cosh     #f f64 f64))
            (cons (prim-name def:hydra_lib_math_e)        (prim0 (prim-name def:hydra_lib_math_e)        (lambda () hydra_lib_math_e)        #f f64))
            (cons (prim-name def:hydra_lib_math_exp)      (prim1 (prim-name def:hydra_lib_math_exp)      hydra_lib_math_exp      #f f64 f64))
            (cons (prim-name def:hydra_lib_math_floor)    (prim1 (prim-name def:hydra_lib_math_floor)    hydra_lib_math_floor    #f f64 f64))
            (cons (prim-name def:hydra_lib_math_log)      (prim1 (prim-name def:hydra_lib_math_log)      hydra_lib_math_log      #f f64 f64))
            (cons (prim-name def:hydra_lib_math_log_base)  (prim2 (prim-name def:hydra_lib_math_log_base)  hydra_lib_math_log_base #f f64 f64 f64))
            (cons (prim-name def:hydra_lib_math_mul_float64) (prim2 (prim-name def:hydra_lib_math_mul_float64) hydra_lib_math_mul_float64 #f f64 f64 f64))
            (cons (prim-name def:hydra_lib_math_negate_float64) (prim1 (prim-name def:hydra_lib_math_negate_float64) hydra_lib_math_negate_float64 #f f64 f64))
            (cons (prim-name def:hydra_lib_math_pi)       (prim0 (prim-name def:hydra_lib_math_pi)       (lambda () hydra_lib_math_pi)       #f f64))
            (cons (prim-name def:hydra_lib_math_pow)      (prim2 (prim-name def:hydra_lib_math_pow)      hydra_lib_math_pow      #f f64 f64 f64))
            (cons (prim-name def:hydra_lib_math_round)    (prim1 (prim-name def:hydra_lib_math_round)    hydra_lib_math_round    #f f64 f64))
            (cons (prim-name def:hydra_lib_math_round_float32)  (prim2 (prim-name def:hydra_lib_math_round_float32)  hydra_lib_math_round_float32  #f i32 f32 f32))
            (cons (prim-name def:hydra_lib_math_round_float64)  (prim2 (prim-name def:hydra_lib_math_round_float64)  hydra_lib_math_round_float64  #f i32 f64 f64))
            (cons (prim-name def:hydra_lib_math_sin)      (prim1 (prim-name def:hydra_lib_math_sin)      hydra_lib_math_sin      #f f64 f64))
            (cons (prim-name def:hydra_lib_math_sinh)     (prim1 (prim-name def:hydra_lib_math_sinh)     hydra_lib_math_sinh     #f f64 f64))
            (cons (prim-name def:hydra_lib_math_sqrt)     (prim1 (prim-name def:hydra_lib_math_sqrt)     hydra_lib_math_sqrt     #f f64 f64))
            (cons (prim-name def:hydra_lib_math_sub_float64) (prim2 (prim-name def:hydra_lib_math_sub_float64) hydra_lib_math_sub_float64 #f f64 f64 f64))
            (cons (prim-name def:hydra_lib_math_tan)      (prim1 (prim-name def:hydra_lib_math_tan)      hydra_lib_math_tan      #f f64 f64))
            (cons (prim-name def:hydra_lib_math_tanh)     (prim1 (prim-name def:hydra_lib_math_tanh)     hydra_lib_math_tanh     #f f64 f64))
            (cons (prim-name def:hydra_lib_math_truncate) (prim1 (prim-name def:hydra_lib_math_truncate) hydra_lib_math_truncate #f f64 f64))))))

    ;; ============================================================================
    ;; Maybes
    ;; ============================================================================

    (define (register-optionals)
      (let (
            (a (tc-variable "a"))
            (b (tc-variable "b"))
            (c (tc-variable "c")))
        (list
          (cons (prim-name def:hydra_lib_optionals_apply)    (prim2 (prim-name def:hydra_lib_optionals_apply)
                                              hydra_lib_optionals_apply
                                              #f (tc-optional (fun a b)) (tc-optional a) (tc-optional b)))
          (cons (prim-name def:hydra_lib_optionals_bind)     (prim2 (prim-name def:hydra_lib_optionals_bind)
                                              hydra_lib_optionals_bind
                                              #f (tc-optional a) (fun a (tc-optional b)) (tc-optional b)))
          (cons (prim-name def:hydra_lib_optionals_cases)    (prim3 (prim-name def:hydra_lib_optionals_cases)
                                              hydra_lib_optionals_cases
                                              #f (tc-optional a) b (fun a b) b))
          (cons (prim-name def:hydra_lib_optionals_cat)      (prim1 (prim-name def:hydra_lib_optionals_cat)      hydra_lib_optionals_cat      #f (tc-list (tc-optional a)) (tc-list a)))
          (cons (prim-name def:hydra_lib_optionals_compose)  (prim3 (prim-name def:hydra_lib_optionals_compose)
                                              hydra_lib_optionals_compose
                                              #f (fun a (tc-optional b)) (fun b (tc-optional c)) a (tc-optional c)))
          (cons (prim-name def:hydra_lib_optionals_from_optional) (prim2 (prim-name def:hydra_lib_optionals_from_optional)
                                               hydra_lib_optionals_from_optional
                                               #f a (tc-optional a) a))
          (cons (prim-name def:hydra_lib_optionals_is_given)    (prim1 (prim-name def:hydra_lib_optionals_is_given)    hydra_lib_optionals_is_given    #f (tc-optional a) (tc-boolean)))
          (cons (prim-name def:hydra_lib_optionals_is_none) (prim1 (prim-name def:hydra_lib_optionals_is_none) hydra_lib_optionals_is_none #f (tc-optional a) (tc-boolean)))
          (cons (prim-name def:hydra_lib_optionals_map)       (prim2 (prim-name def:hydra_lib_optionals_map)
                                               hydra_lib_optionals_map
                                               #f (fun a b) (tc-optional a) (tc-optional b)))
          (cons (prim-name def:hydra_lib_optionals_map_optional)  (prim2 (prim-name def:hydra_lib_optionals_map_optional)
                                               hydra_lib_optionals_map_optional
                                               #f (fun a (tc-optional b)) (tc-list a) (tc-list b)))
          (cons (prim-name def:hydra_lib_optionals_pure)      (prim1 (prim-name def:hydra_lib_optionals_pure)      hydra_lib_optionals_pure      #f a (tc-optional a)))
          (cons (prim-name def:hydra_lib_optionals_to_list)    (prim1 (prim-name def:hydra_lib_optionals_to_list)    hydra_lib_optionals_to_list   #f (tc-optional a) (tc-list a))))))

    ;; ============================================================================
    ;; Pairs
    ;; ============================================================================

    (define (register-pairs)
      (let (
            (a (tc-variable "a"))
            (b (tc-variable "b"))
            (c (tc-variable "c"))
            (d (tc-variable "d")))
        (list
          (cons (prim-name def:hydra_lib_pairs_bimap)  (prim3 (prim-name def:hydra_lib_pairs_bimap)
                                            hydra_lib_pairs_bimap
                                            #f (fun a c) (fun b d) (tc-pair a b) (tc-pair c d)))
          (cons (prim-name def:hydra_lib_pairs_first)  (prim1 (prim-name def:hydra_lib_pairs_first)  hydra_lib_pairs_first  #f (tc-pair a b) a))
          (cons (prim-name def:hydra_lib_pairs_second) (prim1 (prim-name def:hydra_lib_pairs_second) hydra_lib_pairs_second #f (tc-pair a b) b)))))

    ;; ============================================================================
    ;; Sets
    ;; ============================================================================

    (define (register-sets)
      (let (
            (a (tc-variable "a"))
            (b (tc-variable "b"))
            (ord-a (list (list "a" (make-hydra_core_type_variable_constraints (constraints "ordering")))))
            (ord-ab (list (list "a" (make-hydra_core_type_variable_constraints (constraints "ordering")))
                          (list "b" (make-hydra_core_type_variable_constraints (constraints "ordering"))))))
        (list
          (cons (prim-name def:hydra_lib_sets_delete)       (prim2 (prim-name def:hydra_lib_sets_delete)
                                                  hydra_lib_sets_delete
                                                  #f a (tc-set a) (tc-set a) ord-a))
          (cons (prim-name def:hydra_lib_sets_difference)   (prim2 (prim-name def:hydra_lib_sets_difference)
                                                  hydra_lib_sets_difference
                                                  #f (tc-set a) (tc-set a) (tc-set a) ord-a))
          (cons (prim-name def:hydra_lib_sets_empty)        (prim0 (prim-name def:hydra_lib_sets_empty)   (lambda () hydra_lib_sets_empty)   #f (tc-set a) ord-a))
          (cons (prim-name def:hydra_lib_sets_from_list)     (prim1 (prim-name def:hydra_lib_sets_from_list) hydra_lib_sets_from_list #f (tc-list a) (tc-set a) ord-a))
          (cons (prim-name def:hydra_lib_sets_insert)       (prim2 (prim-name def:hydra_lib_sets_insert)
                                                  hydra_lib_sets_insert
                                                  #f a (tc-set a) (tc-set a) ord-a))
          (cons (prim-name def:hydra_lib_sets_intersection) (prim2 (prim-name def:hydra_lib_sets_intersection)
                                                  hydra_lib_sets_intersection
                                                  #f (tc-set a) (tc-set a) (tc-set a) ord-a))
          (cons (prim-name def:hydra_lib_sets_map)          (prim2 (prim-name def:hydra_lib_sets_map)
                                                  hydra_lib_sets_map
                                                  #f (fun a b) (tc-set a) (tc-set b) ord-ab))
          (cons (prim-name def:hydra_lib_sets_member)       (prim2 (prim-name def:hydra_lib_sets_member)
                                                  hydra_lib_sets_member
                                                  #f a (tc-set a) (tc-boolean) ord-a))
          (cons (prim-name def:hydra_lib_sets_null)         (prim1 (prim-name def:hydra_lib_sets_null)     hydra_lib_sets_null     #f (tc-set a) (tc-boolean) ord-a))
          (cons (prim-name def:hydra_lib_sets_singleton)    (prim1 (prim-name def:hydra_lib_sets_singleton) hydra_lib_sets_singleton #f a (tc-set a) ord-a))
          (cons (prim-name def:hydra_lib_sets_size)         (prim1 (prim-name def:hydra_lib_sets_size)     hydra_lib_sets_size     #f (tc-set a) (tc-int32) ord-a))
          (cons (prim-name def:hydra_lib_sets_to_list)       (prim1 (prim-name def:hydra_lib_sets_to_list)   hydra_lib_sets_to_list  #f (tc-set a) (tc-list a) ord-a))
          (cons (prim-name def:hydra_lib_sets_union)        (prim2 (prim-name def:hydra_lib_sets_union)
                                                  hydra_lib_sets_union
                                                  #f (tc-set a) (tc-set a) (tc-set a) ord-a))
          (cons (prim-name def:hydra_lib_sets_unions)       (prim1 (prim-name def:hydra_lib_sets_unions)   hydra_lib_sets_unions   #f (tc-list (tc-set a)) (tc-set a) ord-a)))))

    ;; ============================================================================
    ;; Strings
    ;; ============================================================================

    (define (register-strings)
      (let (
            (s (tc-string))
            (i (tc-int32))
            (b (tc-boolean)))
        (list
          (cons (prim-name def:hydra_lib_strings_cat)         (prim1 (prim-name def:hydra_lib_strings_cat)         hydra_lib_strings_cat         #f (tc-list s) s))
          (cons (prim-name def:hydra_lib_strings_cat2)        (prim2 (prim-name def:hydra_lib_strings_cat2)
                                                  hydra_lib_strings_cat2
                                                  #f s s s))
          (cons (prim-name def:hydra_lib_strings_from_list)    (prim1 (prim-name def:hydra_lib_strings_from_list)    hydra_lib_strings_from_list    #f (tc-list i) s))
          (cons (prim-name def:hydra_lib_strings_intercalate) (prim2 (prim-name def:hydra_lib_strings_intercalate)
                                                  hydra_lib_strings_intercalate
                                                  #f s (tc-list s) s))
          (cons (prim-name def:hydra_lib_strings_length)      (prim1 (prim-name def:hydra_lib_strings_length)      hydra_lib_strings_length      #f s i))
          (cons (prim-name def:hydra_lib_strings_lines)       (prim1 (prim-name def:hydra_lib_strings_lines)       hydra_lib_strings_lines       #f s (tc-list s)))
          (cons (prim-name def:hydra_lib_strings_maybe_char_at) (prim2 (prim-name def:hydra_lib_strings_maybe_char_at) hydra_lib_strings_maybe_char_at #f i s (tc-optional i)))
          (cons (prim-name def:hydra_lib_strings_null)        (prim1 (prim-name def:hydra_lib_strings_null)        hydra_lib_strings_null        #f s b))
          (cons (prim-name def:hydra_lib_strings_split_on)     (prim2 (prim-name def:hydra_lib_strings_split_on)
                                                  hydra_lib_strings_split_on
                                                  #f s s (tc-list s)))
          (cons (prim-name def:hydra_lib_strings_to_list)      (prim1 (prim-name def:hydra_lib_strings_to_list)      hydra_lib_strings_to_list     #f s (tc-list i)))
          (cons (prim-name def:hydra_lib_strings_to_lower)     (prim1 (prim-name def:hydra_lib_strings_to_lower)     hydra_lib_strings_to_lower    #f s s))
          (cons (prim-name def:hydra_lib_strings_to_upper)     (prim1 (prim-name def:hydra_lib_strings_to_upper)     hydra_lib_strings_to_upper    #f s s))
          (cons (prim-name def:hydra_lib_strings_unlines)     (prim1 (prim-name def:hydra_lib_strings_unlines)     hydra_lib_strings_unlines     #f (tc-list s) s)))))

    ;; ============================================================================
    ;; Text (#494)
    ;; ============================================================================
    ;;
    ;; UTF-8 codecs bridging Hydra strings and raw bytes. decodeUtf8 :: binary -> either<string, string>
    ;; (Left message on invalid UTF-8); encodeUtf8 :: string -> binary (total).

    (define (register-text)
      (let (
            (s (tc-string))
            (bin (tc-binary)))
        (list
          (cons (prim-name def:hydra_lib_text_decode_utf8) (prim1 (prim-name def:hydra_lib_text_decode_utf8)
                                             hydra_lib_text_decode_utf8
                                             #f bin (tc-either s s)))
          (cons (prim-name def:hydra_lib_text_encode_utf8) (prim1 (prim-name def:hydra_lib_text_encode_utf8)
                                             hydra_lib_text_encode_utf8
                                             #f s bin)))))

    ;; ============================================================================
    ;; Literals
    ;; ============================================================================

    (define (register-literals)
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
          (list
            (cons (prim-name def:hydra_lib_literals_bigint_to_decimal)    (prim1 (prim-name def:hydra_lib_literals_bigint_to_decimal)    hydra_lib_literals_bigint_to_decimal    #f bi dec))
            (cons (prim-name def:hydra_lib_literals_bigint_to_int8)       (prim1 (prim-name def:hydra_lib_literals_bigint_to_int8)       hydra_lib_literals_bigint_to_int8       #f bi i8))
            (cons (prim-name def:hydra_lib_literals_bigint_to_int16)      (prim1 (prim-name def:hydra_lib_literals_bigint_to_int16)      hydra_lib_literals_bigint_to_int16      #f bi i16))
            (cons (prim-name def:hydra_lib_literals_bigint_to_int32)      (prim1 (prim-name def:hydra_lib_literals_bigint_to_int32)      hydra_lib_literals_bigint_to_int32      #f bi i32))
            (cons (prim-name def:hydra_lib_literals_bigint_to_int64)      (prim1 (prim-name def:hydra_lib_literals_bigint_to_int64)      hydra_lib_literals_bigint_to_int64      #f bi i64))
            (cons (prim-name def:hydra_lib_literals_bigint_to_uint8)      (prim1 (prim-name def:hydra_lib_literals_bigint_to_uint8)      hydra_lib_literals_bigint_to_uint8      #f bi u8))
            (cons (prim-name def:hydra_lib_literals_bigint_to_uint16)     (prim1 (prim-name def:hydra_lib_literals_bigint_to_uint16)     hydra_lib_literals_bigint_to_uint16     #f bi u16))
            (cons (prim-name def:hydra_lib_literals_bigint_to_uint32)     (prim1 (prim-name def:hydra_lib_literals_bigint_to_uint32)     hydra_lib_literals_bigint_to_uint32     #f bi u32))
            (cons (prim-name def:hydra_lib_literals_bigint_to_uint64)     (prim1 (prim-name def:hydra_lib_literals_bigint_to_uint64)     hydra_lib_literals_bigint_to_uint64     #f bi u64))
            (cons (prim-name def:hydra_lib_literals_binary_to_bytes)      (prim1 (prim-name def:hydra_lib_literals_binary_to_bytes)      hydra_lib_literals_binary_to_bytes      #f bin (tc-list i32)))
            (cons (prim-name def:hydra_lib_literals_binary_to_string)     (prim1 (prim-name def:hydra_lib_literals_binary_to_string)     hydra_lib_literals_binary_to_string     #f bin s))
            (cons (prim-name def:hydra_lib_literals_decimal_to_bigint)    (prim1 (prim-name def:hydra_lib_literals_decimal_to_bigint)    hydra_lib_literals_decimal_to_bigint    #f dec bi))
            (cons (prim-name def:hydra_lib_literals_decimal_to_float32)   (prim1 (prim-name def:hydra_lib_literals_decimal_to_float32)   hydra_lib_literals_decimal_to_float32   #f dec f32))
            (cons (prim-name def:hydra_lib_literals_decimal_to_float64)   (prim1 (prim-name def:hydra_lib_literals_decimal_to_float64)   hydra_lib_literals_decimal_to_float64   #f dec f64))
            (cons (prim-name def:hydra_lib_literals_float32_to_decimal)   (prim1 (prim-name def:hydra_lib_literals_float32_to_decimal)   hydra_lib_literals_float32_to_decimal   #f f32 dec))
            (cons (prim-name def:hydra_lib_literals_float32_to_float64)   (prim1 (prim-name def:hydra_lib_literals_float32_to_float64)   hydra_lib_literals_float32_to_float64   #f f32 f64))
            (cons (prim-name def:hydra_lib_literals_float64_to_decimal)   (prim1 (prim-name def:hydra_lib_literals_float64_to_decimal)   hydra_lib_literals_float64_to_decimal   #f f64 dec))
            (cons (prim-name def:hydra_lib_literals_float64_to_float32)   (prim1 (prim-name def:hydra_lib_literals_float64_to_float32)   hydra_lib_literals_float64_to_float32   #f f64 f32))
            (cons (prim-name def:hydra_lib_literals_int8_to_bigint)       (prim1 (prim-name def:hydra_lib_literals_int8_to_bigint)       hydra_lib_literals_int8_to_bigint       #f i8 bi))
            (cons (prim-name def:hydra_lib_literals_int16_to_bigint)      (prim1 (prim-name def:hydra_lib_literals_int16_to_bigint)      hydra_lib_literals_int16_to_bigint      #f i16 bi))
            (cons (prim-name def:hydra_lib_literals_int32_to_bigint)      (prim1 (prim-name def:hydra_lib_literals_int32_to_bigint)      hydra_lib_literals_int32_to_bigint      #f i32 bi))
            (cons (prim-name def:hydra_lib_literals_int64_to_bigint)      (prim1 (prim-name def:hydra_lib_literals_int64_to_bigint)      hydra_lib_literals_int64_to_bigint      #f i64 bi))
            (cons (prim-name def:hydra_lib_literals_uint8_to_bigint)      (prim1 (prim-name def:hydra_lib_literals_uint8_to_bigint)      hydra_lib_literals_uint8_to_bigint      #f u8 bi))
            (cons (prim-name def:hydra_lib_literals_uint16_to_bigint)     (prim1 (prim-name def:hydra_lib_literals_uint16_to_bigint)     hydra_lib_literals_uint16_to_bigint     #f u16 bi))
            (cons (prim-name def:hydra_lib_literals_uint32_to_bigint)     (prim1 (prim-name def:hydra_lib_literals_uint32_to_bigint)     hydra_lib_literals_uint32_to_bigint     #f u32 bi))
            (cons (prim-name def:hydra_lib_literals_uint64_to_bigint)     (prim1 (prim-name def:hydra_lib_literals_uint64_to_bigint)     hydra_lib_literals_uint64_to_bigint     #f u64 bi))
            (cons (prim-name def:hydra_lib_literals_string_to_binary)     (prim1 (prim-name def:hydra_lib_literals_string_to_binary)     hydra_lib_literals_string_to_binary     #f s bin)))
          (list
            (cons (prim-name def:hydra_lib_literals_read_bigint)   (prim1 (prim-name def:hydra_lib_literals_read_bigint)   hydra_lib_literals_read_bigint   #f s (tc-optional bi)))
            (cons (prim-name def:hydra_lib_literals_read_boolean)  (prim1 (prim-name def:hydra_lib_literals_read_boolean)  hydra_lib_literals_read_boolean  #f s (tc-optional b)))
            (cons (prim-name def:hydra_lib_literals_read_decimal)  (prim1 (prim-name def:hydra_lib_literals_read_decimal)  hydra_lib_literals_read_decimal  #f s (tc-optional dec)))
            (cons (prim-name def:hydra_lib_literals_read_float32)  (prim1 (prim-name def:hydra_lib_literals_read_float32)  hydra_lib_literals_read_float32  #f s (tc-optional f32)))
            (cons (prim-name def:hydra_lib_literals_read_float64)  (prim1 (prim-name def:hydra_lib_literals_read_float64)  hydra_lib_literals_read_float64  #f s (tc-optional f64)))
            (cons (prim-name def:hydra_lib_literals_read_int8)     (prim1 (prim-name def:hydra_lib_literals_read_int8)     hydra_lib_literals_read_int8     #f s (tc-optional i8)))
            (cons (prim-name def:hydra_lib_literals_read_int16)    (prim1 (prim-name def:hydra_lib_literals_read_int16)    hydra_lib_literals_read_int16    #f s (tc-optional i16)))
            (cons (prim-name def:hydra_lib_literals_read_int32)    (prim1 (prim-name def:hydra_lib_literals_read_int32)    hydra_lib_literals_read_int32    #f s (tc-optional i32)))
            (cons (prim-name def:hydra_lib_literals_read_int64)    (prim1 (prim-name def:hydra_lib_literals_read_int64)    hydra_lib_literals_read_int64    #f s (tc-optional i64)))
            (cons (prim-name def:hydra_lib_literals_read_string)   (prim1 (prim-name def:hydra_lib_literals_read_string)   hydra_lib_literals_read_string   #f s (tc-optional s)))
            (cons (prim-name def:hydra_lib_literals_read_uint8)    (prim1 (prim-name def:hydra_lib_literals_read_uint8)    hydra_lib_literals_read_uint8    #f s (tc-optional u8)))
            (cons (prim-name def:hydra_lib_literals_read_uint16)   (prim1 (prim-name def:hydra_lib_literals_read_uint16)   hydra_lib_literals_read_uint16   #f s (tc-optional u16)))
            (cons (prim-name def:hydra_lib_literals_read_uint32)   (prim1 (prim-name def:hydra_lib_literals_read_uint32)   hydra_lib_literals_read_uint32   #f s (tc-optional u32)))
            (cons (prim-name def:hydra_lib_literals_read_uint64)   (prim1 (prim-name def:hydra_lib_literals_read_uint64)   hydra_lib_literals_read_uint64   #f s (tc-optional u64))))
          (list
            (cons (prim-name def:hydra_lib_literals_show_bigint)   (prim1 (prim-name def:hydra_lib_literals_show_bigint)   hydra_lib_literals_show_bigint   #f bi s))
            (cons (prim-name def:hydra_lib_literals_show_boolean)  (prim1 (prim-name def:hydra_lib_literals_show_boolean)  hydra_lib_literals_show_boolean  #f b s))
            (cons (prim-name def:hydra_lib_literals_show_decimal)  (prim1 (prim-name def:hydra_lib_literals_show_decimal)  hydra_lib_literals_show_decimal  #f dec s))
            (cons (prim-name def:hydra_lib_literals_show_float32)  (prim1 (prim-name def:hydra_lib_literals_show_float32)  hydra_lib_literals_show_float32  #f f32 s))
            (cons (prim-name def:hydra_lib_literals_show_float64)  (prim1 (prim-name def:hydra_lib_literals_show_float64)  hydra_lib_literals_show_float64  #f f64 s))
            (cons (prim-name def:hydra_lib_literals_show_int8)     (prim1 (prim-name def:hydra_lib_literals_show_int8)     hydra_lib_literals_show_int8     #f i8 s))
            (cons (prim-name def:hydra_lib_literals_show_int16)    (prim1 (prim-name def:hydra_lib_literals_show_int16)    hydra_lib_literals_show_int16    #f i16 s))
            (cons (prim-name def:hydra_lib_literals_show_int32)    (prim1 (prim-name def:hydra_lib_literals_show_int32)    hydra_lib_literals_show_int32    #f i32 s))
            (cons (prim-name def:hydra_lib_literals_show_int64)    (prim1 (prim-name def:hydra_lib_literals_show_int64)    hydra_lib_literals_show_int64    #f i64 s))
            (cons (prim-name def:hydra_lib_literals_show_uint8)    (prim1 (prim-name def:hydra_lib_literals_show_uint8)    hydra_lib_literals_show_uint8    #f u8 s))
            (cons (prim-name def:hydra_lib_literals_show_uint16)   (prim1 (prim-name def:hydra_lib_literals_show_uint16)   hydra_lib_literals_show_uint16   #f u16 s))
            (cons (prim-name def:hydra_lib_literals_show_uint32)   (prim1 (prim-name def:hydra_lib_literals_show_uint32)   hydra_lib_literals_show_uint32   #f u32 s))
            (cons (prim-name def:hydra_lib_literals_show_uint64)   (prim1 (prim-name def:hydra_lib_literals_show_uint64)   hydra_lib_literals_show_uint64   #f u64 s))
            (cons (prim-name def:hydra_lib_literals_show_string)   (prim1 (prim-name def:hydra_lib_literals_show_string)   hydra_lib_literals_show_string   #f s s))))))

    ;; ============================================================================
    ;; Regex
    ;; ============================================================================

    (define (register-regex)
      (let (
            (s (tc-string))
            (b (tc-boolean)))
        (list
          (cons (prim-name def:hydra_lib_regex_find)       (prim2 (prim-name def:hydra_lib_regex_find)
                                                hydra_lib_regex_find
                                                #f s s (tc-optional s)))
          (cons (prim-name def:hydra_lib_regex_find_all)    (prim2 (prim-name def:hydra_lib_regex_find_all)
                                                hydra_lib_regex_find_all
                                                #f s s (tc-list s)))
          (cons (prim-name def:hydra_lib_regex_matches)    (prim2 (prim-name def:hydra_lib_regex_matches)
                                                hydra_lib_regex_matches
                                                #f s s b))
          (cons (prim-name def:hydra_lib_regex_replace)    (prim3 (prim-name def:hydra_lib_regex_replace)
                                                hydra_lib_regex_replace
                                                #f s s s s))
          (cons (prim-name def:hydra_lib_regex_replace_all) (prim3 (prim-name def:hydra_lib_regex_replace_all)
                                                hydra_lib_regex_replace_all
                                                #f s s s s))
          (cons (prim-name def:hydra_lib_regex_split)      (prim2 (prim-name def:hydra_lib_regex_split)
                                                hydra_lib_regex_split
                                                #f s s (tc-list s))))))

    ;; ============================================================================
    ;; Standard library: all primitives combined
    ;; ============================================================================

    (define (standard-library)
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
        (register-text)))

)
)
