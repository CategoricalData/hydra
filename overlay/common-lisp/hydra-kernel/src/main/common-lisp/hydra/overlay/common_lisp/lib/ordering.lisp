(in-package :cl-user)

;; A Literal.decimal wrapped as a Hydra term, as `equal`/`compare`'s generic x/x TermCoder
;; (which just passes terms through unchanged -- see prims.lisp's tc-variable) actually
;; receives it: (:literal (:decimal (coefficient . scale))), not a bare cons.
(defun hydra-decimal-term-p (term)
  (and (consp term) (eq (car term) :literal)
       (let ((lit (cadr term)))
         (and (consp lit) (eq (car lit) :decimal)))))

(defun hydra-decimal-term-value (term) (cadr (cadr term)))

;; Ordered comparison of two decimals: numeric value first, then scale ascending as a
;; tiebreak (1.1 < 1.10 < 1.100), per docs/specification/ordering-and-equality.md. Cross-
;; multiplies to compare a.coefficient/10^a.scale against b.coefficient/10^b.scale exactly
;; (native CL bignum arithmetic), mirroring the TypeScript/Clojure/Java equivalents.
(defun hydra-compare-decimals (a b)
  (let* ((ca (car a)) (sa (cdr a)) (cb (car b)) (sb (cdr b))
         (max-scale (max sa sb))
         (na (* ca (expt 10 (- max-scale sa))))
         (nb (* cb (expt 10 (- max-scale sb)))))
    (cond ((< na nb) -1) ((> na nb) 1) (t (- sa sb)))))

;; Declared-variant order for each hydra.core union family, transcribed from
;; the generated hydra/core.lisp (hydra_core_term-variants, hydra_core_type-
;; variants, etc.), which in turn come from the DSL declaration order
;; (packages/hydra-kernel/.../Sources/Kernel/Types/Core.hs). Mirrors Python's
;; _VARIANT_ORDER table (overlay/python/.../util/_compare.py) -- the
;; established #718 precedent hand-authors variant order for kernel types
;; only; non-kernel (user-schema) unions fall back to a still-deterministic
;; but non-declared-order tag comparison (see the tag-keyword branch below).
;; Keyed by family name (not by tag alone): CL union values are bare
;; (:tag . payload) conses with no runtime type identity, and tag keywords
;; collide across unrelated unions repo-wide (:literal, :map, :unit, etc.),
;; so a single flat tag->ordinal table would be unsound. Hydra's static
;; typing guarantees compare/equal are only ever called on same-typed
;; values (docs/specification/ordering-and-equality.md), so it is safe to
;; resolve the family from BOTH sides' tags and require them to agree.
(defparameter *hydra-variant-order*
  (list
   (cons "term" '(:annotated :application :cases :either :lambda :let :list
                   :literal :map :optional :pair :project :record :set
                   :type_lambda :type_application :inject :unit :unwrap
                   :variable :wrap))
   (cons "type" '(:annotated :application :effect :either :forall :function
                   :list :literal :map :optional :pair :record :set :union
                   :unit :variable :void :wrap))
   (cons "literal" '(:binary :boolean :decimal :float :integer :string))
   (cons "integer" '(:bigint :int8 :int16 :int32 :int64 :uint8 :uint16 :uint32 :uint64))
   (cons "float" '(:float32 :float64))))

;; If tags A and B both belong to the SAME known family, return their
;; (ordinal-a . ordinal-b); otherwise NIL (unknown family, or a family
;; mismatch that should not arise under Hydra's static typing -- callers
;; fall back to a deterministic tag compare in that case).
(defun hydra-variant-ordinals (tag-a tag-b)
  (dolist (entry *hydra-variant-order*)
    (let ((variants (cdr entry)))
      (let ((pa (position tag-a variants)) (pb (position tag-b variants)))
        (when (and pa pb) (return-from hydra-variant-ordinals (cons pa pb))))))
  nil)

;; Generic comparison for ordering heterogeneous values.
;; Returns -1, 0, or 1.
(defun hash-table-equal-p (a b)
  "Compare two hash tables for structural equality."
  (and (= (hash-table-count a) (hash-table-count b))
       (block nil
         (maphash (lambda (k v)
                    (multiple-value-bind (bv found) (gethash k b)
                      (unless (and found (= (generic-compare v bv) 0))
                        (return nil))))
                  a)
         t)))

(defun generic-compare (a b)
  (cond
    ((eq a b) 0)
    ((and (null a) (null b)) 0)
    ((null a) -1)
    ((null b) 1)
    ((and (hash-table-p a) (hash-table-p b))
     ;; Compare hash tables by converting to sorted alists
     (if (hash-table-equal-p a b) 0
         (let* ((al (sort (let (r) (maphash (lambda (k v) (push (cons k v) r)) a) r)
                          (lambda (x y) (< (generic-compare (car x) (car y)) 0))))
                (bl (sort (let (r) (maphash (lambda (k v) (push (cons k v) r)) b) r)
                          (lambda (x y) (< (generic-compare (car x) (car y)) 0)))))
           (generic-compare al bl))))
    ((and (floatp a) (floatp b))
     ;; IEEE 754 extended totalOrder (docs/specification/ordering-and-equality.md):
     ;; NaN is greatest and equal to itself; -0.0 < +0.0. Native CL < / = treat
     ;; NaN as unordered and (= -0.0 0.0) as true, so both need special-casing
     ;; (hydra-nan-p from math.lisp, loaded before this file).
     (let ((na (hydra-nan-p a)) (nb (hydra-nan-p b)))
       (cond
         ((and na nb) 0)
         (na 1)
         (nb -1)
         ((< a b) -1)
         ((> a b) 1)
         ((and (= a 0) (= b 0))
          (let ((nega (minusp (float-sign a))) (negb (minusp (float-sign b))))
            (cond ((eq nega negb) 0) (nega -1) (t 1))))
         (t 0))))
    ((and (numberp a) (numberp b))
     (cond ((< a b) -1) ((= a b) 0) (t 1)))
    ((and (stringp a) (stringp b))
     (cond ((string< a b) -1) ((string= a b) 0) (t 1)))
    ((and (characterp a) (characterp b))
     (cond ((char< a b) -1) ((char= a b) 0) (t 1)))
    ((and (typep a 'boolean) (typep b 'boolean))
     (cond ((and (not a) b) -1) ((eq a b) 0) (t 1)))
    ((and (hydra-decimal-term-p a) (hydra-decimal-term-p b))
     (hydra-compare-decimals (hydra-decimal-term-value a) (hydra-decimal-term-value b)))
    ;; Union values: (:tag . payload) or (:tag payload ...). Compare by
    ;; declared-variant order (when both tags resolve to the same known
    ;; kernel family) rather than the keyword's print/alphabetical order;
    ;; same variant (or unknown family) recurses into the payload.
    ((and (consp a) (consp b) (keywordp (car a)) (keywordp (car b)))
     (cond
       ((eq (car a) (car b)) (generic-compare (cdr a) (cdr b)))
       (t (let ((ordinals (hydra-variant-ordinals (car a) (car b))))
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
    ;; Maps and sets (overlay/common_lisp/lib/maps.lisp, sets.lisp): persistent
    ;; red-black trees (rbnode structs, loaded before this file -- see
    ;; run-tests.lisp's load order). An rbnode's shape depends on INSERTION
    ;; ORDER (that's the whole point of a balanced tree: different insertion
    ;; sequences produce different tree shapes for the same logical content),
    ;; so comparing rbnode structs as generic opaque structs (equalp, then a
    ;; write-to-string fallback below) is WRONG whenever two maps/sets with
    ;; identical contents were built in different orders -- equalp fails
    ;; (different shape) and the printed form differs too, so `equal`/
    ;; `compare` would disagree with two maps/sets that the spec considers
    ;; equal. Canonicalize to a key-sorted entry list first (maps-entries-
    ;; sorted / sets-elements-sorted do an in-order tree traversal, already
    ;; key-sorted by the BST invariant -- no extra sort needed) and compare
    ;; that structurally, mirroring the CanonMap/CanonSet branches in the
    ;; TypeScript fix (ordering.ts).
    ((and (rbnode-p a) (rbnode-p b))
     (generic-compare (maps-entries-sorted a) (maps-entries-sorted b)))
    ((or (rbnode-p a) (rbnode-p b))
     ;; One side is an rbnode and the other is the legacy nil/alist/sorted-list
     ;; representation maps.lisp/sets.lisp also accept (see maps-alist-p) --
     ;; canonicalize both sides the same way rather than falling through to
     ;; struct/print comparison, which would never even reach here for a
     ;; non-struct legacy value anyway (its own conses would take the (consp
     ;; a) (consp b) branch below with no rbnode-aware canonicalization).
     ;; Delegate to maps-entries-sorted, which already accepts rbnode, alist,
     ;; or nil uniformly.
     (generic-compare (maps-entries-sorted a) (maps-entries-sorted b)))
    ;; Structs (records): compare by slot values in declaration order.
    ;; struct-slots is portable across CL implementations via SBCL/CCL's
    ;; sb-mop/mop introspection would add a dependency; instead rely on the
    ;; struct's own PRINT-OBJECT-independent slot accessors being applied by
    ;; the caller -- structs reach generic-compare only via slot recursion
    ;; from decode/encode, which already walks fields in order, so a bare
    ;; struct value here is a LEAF (non-decomposed) struct with no further
    ;; structure to recurse into from this generic function; fall through to
    ;; the equalp-based struct comparison below.
    ((and (typep a 'structure-object) (typep b 'structure-object) (eq (type-of a) (type-of b)))
     (if (equalp a b) 0
         (let ((sa (write-to-string a)) (sb (write-to-string b)))
           (cond ((string< sa sb) -1) ((string= sa sb) 0) (t 1)))))
    ((and (consp a) (consp b))
     (let ((c (generic-compare (car a) (car b))))
       (if (= c 0)
           (generic-compare (cdr a) (cdr b))
           c)))
    (t (let ((sa (write-to-string a)) (sb (write-to-string b)))
         (cond ((string< sa sb) -1) ((string= sa sb) 0) (t 1))))))

;; compare :: a -> a -> Comparison
;; Compare two values and return a Comparison.
(defvar hydra_overlay_common_lisp_lib_ordering_compare
  (lambda (a)
    (lambda (b)
      (let ((c (generic-compare a b)))
        (cond
         ((< c 0) (list :less_than nil))
         ((> c 0) (list :greater_than nil))
         (t (list :equal_to nil)))))))

;; gt :: a -> a -> Bool
;; Check if first value is greater than second.
(defvar hydra_overlay_common_lisp_lib_ordering_gt
  (lambda (a)
    (lambda (b)
      (> (generic-compare a b) 0))))

;; gte :: a -> a -> Bool
;; Check if first value is greater than or equal to second.
(defvar hydra_overlay_common_lisp_lib_ordering_gte
  (lambda (a)
    (lambda (b)
      (>= (generic-compare a b) 0))))

;; lt :: a -> a -> Bool
;; Check if first value is less than second.
(defvar hydra_overlay_common_lisp_lib_ordering_lt
  (lambda (a)
    (lambda (b)
      (< (generic-compare a b) 0))))

;; lte :: a -> a -> Bool
;; Check if first value is less than or equal to second.
(defvar hydra_overlay_common_lisp_lib_ordering_lte
  (lambda (a)
    (lambda (b)
      (<= (generic-compare a b) 0))))

;; max :: a -> a -> a
;; Return the maximum of two values.
(defvar hydra_overlay_common_lisp_lib_ordering_max
  (lambda (a)
    (lambda (b)
      (if (>= (generic-compare a b) 0) a b))))

;; min :: a -> a -> a
;; Return the minimum of two values.
(defvar hydra_overlay_common_lisp_lib_ordering_min
  (lambda (a)
    (lambda (b)
      (if (<= (generic-compare a b) 0) a b))))
