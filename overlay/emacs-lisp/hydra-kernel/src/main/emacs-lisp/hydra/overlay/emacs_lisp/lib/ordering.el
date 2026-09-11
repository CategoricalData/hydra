;;; ordering.el --- Hydra ordering primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; A Literal.decimal wrapped as a Hydra term, as `equal`/`compare`'s generic x/x TermCoder
;; (which just passes terms through unchanged -- see prims.el's tc-variable) actually
;; receives it: (:literal (:decimal (coefficient . scale))), not a bare cons.
(defun hydra-decimal-term-p (term)
  (and (consp term) (eq (car term) :literal)
       (let ((lit (cadr term)))
         (and (consp lit) (eq (car lit) :decimal)))))

(defun hydra-decimal-term-value (term) (cadr (cadr term)))

;; Ordered comparison of two decimals: numeric value first, then scale ascending as a
;; tiebreak (1.1 < 1.10 < 1.100), per docs/specification/ordering-and-equality.md. Cross-
;; multiplies to compare a.coefficient/10^a.scale against b.coefficient/10^b.scale exactly
;; (native Emacs Lisp bignum arithmetic), mirroring the TypeScript/Clojure/Common
;; Lisp/Scheme equivalents.
(defun hydra-compare-decimals (a b)
  (let* ((ca (car a)) (sa (cdr a)) (cb (car b)) (sb (cdr b))
         (max-scale (max sa sb))
         (na (* ca (expt 10 (- max-scale sa))))
         (nb (* cb (expt 10 (- max-scale sb)))))
    (cond ((< na nb) -1) ((> na nb) 1) (t (- sa sb)))))

;; Declared-variant order for each hydra.core union family, transcribed from
;; the generated hydra/core.el (hydra_core_term-variants, hydra_core_type-
;; variants, etc.), which in turn come from the DSL declaration order
;; (packages/hydra-kernel/.../Sources/Kernel/Types/Core.hs). Mirrors Python's
;; _VARIANT_ORDER table (overlay/python/.../util/_compare.py) -- the
;; established #718 precedent hand-authors variant order for kernel types
;; only; non-kernel (user-schema) unions fall back to a still-deterministic
;; but non-declared-order tag comparison (see the tag-keyword branch below).
;; Keyed by family name (not by tag alone): EL union values are bare
;; (:tag . payload) conses with no runtime type identity, and tag keywords
;; collide across unrelated unions repo-wide (:literal, :map, :unit, etc.),
;; so a single flat tag->ordinal table would be unsound. Hydra's static
;; typing guarantees compare/equal are only ever called on same-typed
;; values (docs/specification/ordering-and-equality.md), so it is safe to
;; resolve the family from BOTH sides' tags and require them to agree.
(defconst hydra--variant-order
  '(("term" . (:annotated :application :cases :either :lambda :let :list
               :literal :map :optional :pair :project :record :set
               :type_lambda :type_application :inject :unit :unwrap
               :variable :wrap))
    ("type" . (:annotated :application :effect :either :forall :function
               :list :literal :map :optional :pair :record :set :union
               :unit :variable :void :wrap))
    ("literal" . (:binary :boolean :decimal :float :integer :string))
    ("integer" . (:bigint :int8 :int16 :int32 :int64 :uint8 :uint16 :uint32 :uint64))
    ("float" . (:float32 :float64))))

;; If tags A and B both belong to the SAME known family, return
;; (ordinal-a . ordinal-b); otherwise nil (unknown family, or a family
;; mismatch that should not arise under Hydra's static typing -- callers
;; fall back to a deterministic tag compare in that case).
(defun hydra--variant-ordinals (tag-a tag-b)
  (catch 'found
    (dolist (entry hydra--variant-order)
      (let* ((variants (cdr entry))
             (pa (cl-position tag-a variants))
             (pb (cl-position tag-b variants)))
        (when (and pa pb) (throw 'found (cons pa pb)))))
    nil))

;; Generic comparison for ordering heterogeneous values.
;; Returns -1, 0, or 1.
(defun hash-table-structurally-equal-p (a b)
  "Compare two hash tables for structural equality."
  (and (= (hash-table-count a) (hash-table-count b))
       (catch 'done
         (maphash (lambda (k v)
                    (let ((bv (gethash k b :hydra-not-found)))
                      (when (or (eq bv :hydra-not-found)
                                (not (= (generic-compare v bv) 0)))
                        (throw 'done nil))))
                  a)
         t)))

(defun generic-compare (a b)
  (cond
    ((and (null a) (null b)) 0)
    ;; Treat an empty hash-table as equal to nil — both represent the
    ;; empty map/set, and the kernel mixes the two representations
    ;; (e.g. `hydra_overlay_emacs_lisp_lib_maps_empty` is nil; `(from_list nil)` is a
    ;; zero-count hash-table).
    ((and (null a) (hash-table-p b) (zerop (hash-table-count b))) 0)
    ((and (hash-table-p a) (zerop (hash-table-count a)) (null b)) 0)
    ((null a) -1)
    ((null b) 1)
    ;; Hash-tables compare by sorted key/value pairs. Compare via a
    ;; structural-equality fast-path first to avoid the sort when equal.
    ((and (hash-table-p a) (hash-table-p b))
     (if (hash-table-structurally-equal-p a b) 0
       (let ((al nil) (bl nil))
         (maphash (lambda (k v) (push (cons k v) al)) a)
         (maphash (lambda (k v) (push (cons k v) bl)) b)
         (setq al (sort al (lambda (x y) (< (generic-compare (car x) (car y)) 0))))
         (setq bl (sort bl (lambda (x y) (< (generic-compare (car x) (car y)) 0))))
         (generic-compare al bl))))
    ((equal a b) 0)
    ((and (numberp a) (numberp b))
     (cond ((< a b) -1) ((= a b) 0) (t 1)))
    ((and (stringp a) (stringp b))
     (cond ((string< a b) -1) ((string= a b) 0) (t 1)))
    ((and (characterp a) (characterp b))
     (cond ((< a b) -1) ((= a b) 0) (t 1)))
    ((and (hydra-decimal-term-p a) (hydra-decimal-term-p b))
     (hydra-compare-decimals (hydra-decimal-term-value a) (hydra-decimal-term-value b)))
    ;; Union values: (:tag . payload) or (:tag payload ...). Compare by
    ;; declared-variant order (when both tags resolve to the same known
    ;; kernel family) rather than the keyword's print/alphabetical order;
    ;; same variant (or unknown family) recurses into the payload.
    ;;
    ;; :map is special-cased: a Term.map payload (overlay/emacs_lisp/lib/
    ;; maps.el) is a raw cons-prepended alist -- possibly with shadowed/
    ;; tombstoned entries from prior inserts/deletes -- that is
    ;; INDISTINGUISHABLE from generic cons data once unwrapped from the
    ;; union tag (no type marker survives the unwrap), so plain (consp a)
    ;; (consp b) car/cdr recursion below would compare raw insertion-order
    ;; list structure instead of map contents: two maps with identical
    ;; logical content built via different insert sequences would compare
    ;; UNEQUAL (confirmed empirically). Canonicalize via
    ;; hydra-map-sorted-pairs (overlay/emacs_lisp/lib/maps.el, dedup +
    ;; key-sort) before recursing -- same fix shape as CL's rbnode branch
    ;; above and TS's CanonMap branch (ordering.ts). Term.set does not need
    ;; the same treatment: its payload is a hash-table (sets.el), already
    ;; handled by the hash-table-p branch above regardless of union-tag
    ;; wrapping.
    ((and (consp a) (consp b) (eq (car a) :map) (eq (car b) :map))
     (generic-compare (hydra-map-sorted-pairs (cadr a)) (hydra-map-sorted-pairs (cadr b))))
    ((and (consp a) (consp b) (keywordp (car a)) (keywordp (car b)))
     (cond
      ((eq (car a) (car b)) (generic-compare (cdr a) (cdr b)))
      (t (let ((ordinals (hydra--variant-ordinals (car a) (car b))))
           (cond
            (ordinals (- (car ordinals) (cdr ordinals)))
            (t
             ;; Unknown (non-kernel) family: no declared-order table
             ;; available -- fall back to a deterministic (not
             ;; print-based) keyword compare. Same scope limitation
             ;; #718 carries on every host but Java.
             (let ((sa (symbol-name (car a))) (sb (symbol-name (car b))))
               (cond ((string< sa sb) -1) ((string= sa sb) 0) (t 1)))))))))
    ((and (symbolp a) (symbolp b))
     (let ((sa (symbol-name a)) (sb (symbol-name b)))
       (cond ((string< sa sb) -1) ((string= sa sb) 0) (t 1))))
    ;; Structs (records): compare structurally via `equal` (EL's `equal`
    ;; recurses into cl-defstruct instances), falling back to a
    ;; deterministic (not print-based) type+slot-value compare when unequal
    ;; -- structs reach here as leaves; field-level structural recursion for
    ;; nested records already happens through decode/encode's own field
    ;; walk before generic-compare sees a bare struct value.
    ((and (recordp a) (recordp b) (eq (type-of a) (type-of b)))
     (if (equal a b) 0
       (let ((sa (format "%S" a)) (sb (format "%S" b)))
         (cond ((string< sa sb) -1) ((string= sa sb) 0) (t 1)))))
    ((and (consp a) (consp b))
     (let ((c (generic-compare (car a) (car b))))
       (if (= c 0)
           (generic-compare (cdr a) (cdr b))
           c)))
    (t (let ((sa (format "%S" a)) (sb (format "%S" b)))
         (cond ((string< sa sb) -1) ((string= sa sb) 0) (t 1))))))

;; compare :: a -> a -> Comparison
(defvar hydra_overlay_emacs_lisp_lib_ordering_compare
  (lambda (a)
    "Compare two values and return a Comparison."
    (lambda (b)
      (let ((c (generic-compare a b)))
        (cond
         ((< c 0) (list :less_than nil))
         ((> c 0) (list :greater_than nil))
         (t (list :equal_to nil)))))))

;; gt :: a -> a -> Bool
(defvar hydra_overlay_emacs_lisp_lib_ordering_gt
  (lambda (a)
    "Check if first value is greater than second."
    (lambda (b)
      (> (generic-compare a b) 0))))

;; gte :: a -> a -> Bool
(defvar hydra_overlay_emacs_lisp_lib_ordering_gte
  (lambda (a)
    "Check if first value is greater than or equal to second."
    (lambda (b)
      (>= (generic-compare a b) 0))))

;; lt :: a -> a -> Bool
(defvar hydra_overlay_emacs_lisp_lib_ordering_lt
  (lambda (a)
    "Check if first value is less than second."
    (lambda (b)
      (< (generic-compare a b) 0))))

;; lte :: a -> a -> Bool
(defvar hydra_overlay_emacs_lisp_lib_ordering_lte
  (lambda (a)
    "Check if first value is less than or equal to second."
    (lambda (b)
      (<= (generic-compare a b) 0))))

;; max :: a -> a -> a
(defvar hydra_overlay_emacs_lisp_lib_ordering_max
  (lambda (a)
    "Return the maximum of two values."
    (lambda (b)
      (if (>= (generic-compare a b) 0) a b))))

;; min :: a -> a -> a
(defvar hydra_overlay_emacs_lisp_lib_ordering_min
  (lambda (a)
    "Return the minimum of two values."
    (lambda (b)
      (if (<= (generic-compare a b) 0) a b))))

(provide 'hydra.lib.ordering)
