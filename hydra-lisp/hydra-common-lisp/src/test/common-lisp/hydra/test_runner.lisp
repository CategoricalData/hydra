;;; Hydra Common Lisp test runner
;;;
;;; Slim test runner modeled after the Haskell TestSuiteSpec.hs and Clojure test_runner.clj.
;;; Delegates evaluation to the generated hydra_reduction_reduce_term and display to
;;; hydra_show_core_term. Only annotation primitives are implemented inline.

(in-package :cl-user)

;; ==========================================================================
;; Annotation primitives (native CL, operating on meta-encoded terms)
;; ==========================================================================

;; --------------------------------------------------------------------------
;; Meta-encoding to struct-compat conversion
;; --------------------------------------------------------------------------

(defun meta-injection-p (term)
  "Check if term is a meta-encoded union injection (:union Injection)."
  (and (consp term) (eq (first term) :union)
       (consp (second term))
       (assoc :type_name (second term))))

(defun meta-record-p (term)
  "Check if term is a meta-encoded record (:record Record)."
  (and (consp term) (eq (first term) :record)
       (consp (second term))
       (assoc :type_name (second term))))

(defun meta-field-value (fields field-name)
  "Get the :term value of a named field from a list of Field alists."
  (let ((f (find-if (lambda (fld) (equal (cdr (assoc :name fld)) field-name)) fields)))
    (when f (cdr (assoc :term f)))))

(defun meta-to-struct (term)
  "Convert a meta-encoded term to struct-compat format. Passes through non-meta terms."
  (cond
    ;; Already struct-compat or primitive
    ((not (consp term)) term)
    ;; (:union Injection) where Injection has type_name and field
    ((meta-injection-p term)
     (let* ((inj (second term))
            (type-name (cdr (assoc :type_name inj)))
            (field-alist (cdr (assoc :field inj)))
            (field-name (cdr (assoc :name field-alist)))
            (field-term (cdr (assoc :term field-alist))))
       (cond
         ;; hydra.core.Term injection -> struct-compat term variant
         ((equal type-name "hydra.core.Term")
          (let ((variant-key (intern (string-upcase field-name) :keyword))
                (converted-body (meta-to-struct field-term)))
            (cond
              ;; :annotated -> (:annotated AnnotatedTerm)
              ((eq variant-key :annotated)
               (if (meta-record-p converted-body)
                   (let* ((rec (second converted-body))
                          (fields (cdr (assoc :fields rec)))
                          (body (meta-to-struct (meta-field-value fields "body")))
                          (ann (meta-to-struct (meta-field-value fields "annotation"))))
                     (list :annotated (make-annotated_term body ann)))
                   (list :annotated converted-body)))
              ;; :literal -> (:literal LiteralValue)
              ((eq variant-key :literal) (list :literal (meta-to-struct-literal converted-body)))
              ;; :application -> (:application Application)
              ((eq variant-key :application)
               (if (meta-record-p converted-body)
                   (let* ((rec (second converted-body))
                          (fields (cdr (assoc :fields rec)))
                          (fn (meta-to-struct (meta-field-value fields "function")))
                          (arg (meta-to-struct (meta-field-value fields "argument"))))
                     (list :application (make-application fn arg)))
                   (list :application converted-body)))
              ;; Other term variants pass through with the variant key
              (t (list variant-key converted-body)))))
         ;; hydra.core.Literal injection -> struct-compat literal
         ((equal type-name "hydra.core.Literal")
          (meta-to-struct-literal-variant field-name (meta-to-struct field-term)))
         ;; hydra.core.IntegerValue
         ((equal type-name "hydra.core.IntegerValue")
          (let ((int-key (intern (string-upcase field-name) :keyword)))
            (list :integer (list int-key (meta-to-struct field-term)))))
         ;; hydra.core.FloatValue
         ((equal type-name "hydra.core.FloatValue")
          (let ((float-key (intern (string-upcase field-name) :keyword)))
            (list :float (list float-key (meta-to-struct field-term)))))
         ;; Other type injections: return as variant
         (t (let ((variant-key (intern (string-upcase field-name) :keyword)))
              (list variant-key (meta-to-struct field-term)))))))
    ;; (:record Record) -> convert fields
    ((meta-record-p term)
     (let* ((rec (second term))
            (fields (cdr (assoc :fields rec))))
       ;; Return as alist
       (mapcar (lambda (fld)
                 (cons (intern (string-upcase (cdr (assoc :name fld))) :keyword)
                       (meta-to-struct (cdr (assoc :term fld)))))
               fields)))
    ;; (:map entries) -> convert entries
    ((and (eq (first term) :map) (listp (second term)))
     (list :map (mapcar (lambda (entry)
                          (cons (meta-to-struct (car entry))
                                (meta-to-struct (cdr entry))))
                        (second term))))
    ;; (:list items) -> convert items
    ((and (eq (first term) :list) (listp (second term)))
     (list :list (mapcar #'meta-to-struct (second term))))
    ;; (:maybe val) -> convert val
    ((eq (first term) :maybe)
     (list :maybe (when (second term) (meta-to-struct (second term)))))
    ;; (:literal val) -> pass through
    ((eq (first term) :literal) term)
    ;; (:wrap WrappedTerm) -> pass through
    ((eq (first term) :wrap) term)
    ;; Default: pass through
    (t term)))

(defun meta-to-struct-literal-variant (field-name inner)
  "Convert a Literal injection variant to struct-compat."
  (cond
    ((equal field-name "string") (list :string inner))
    ((equal field-name "boolean") (list :boolean inner))
    ((equal field-name "integer") inner) ;; inner is already (:integer ...)
    ((equal field-name "float") inner)   ;; inner is already (:float ...)
    (t (list (intern (string-upcase field-name) :keyword) inner))))

(defun meta-to-struct-literal (term)
  "Convert a meta-encoded Literal to struct-compat."
  (if (meta-injection-p term)
      (let* ((inj (second term))
             (field-alist (cdr (assoc :field inj)))
             (field-name (cdr (assoc :name field-alist)))
             (field-term (cdr (assoc :term field-alist))))
        (meta-to-struct-literal-variant field-name (meta-to-struct field-term)))
      term))

;; --------------------------------------------------------------------------
;; Annotation extraction and manipulation
;; --------------------------------------------------------------------------

(defun is-meta-annotated-p (t_)
  "Check if term is an annotated term (struct-compat format)."
  (and (consp t_) (eq (first t_) :annotated)))

(defun deannotate-term (t_)
  (if (is-meta-annotated-p t_)
      (deannotate-term (annotated_term-body (second t_)))
      t_))

(defun map-term-to-alist (m)
  "Convert map contents to an alist of (key . value) pairs."
  (cond
    ((and (listp m) (every #'consp m)) m)
    ((listp m) (mapcar (lambda (pair) (cons (first pair) (second pair))) m))
    (t nil)))

(defun term-annotation-internal (term)
  "Extract annotation map from an annotated term (struct-compat format). Returns an alist."
  (labels ((recur (t_ pairs)
             (if (is-meta-annotated-p t_)
                 (let* ((at (second t_))
                        (ann (annotated_term-annotation at))
                        (body (annotated_term-body at)))
                   ;; ann is either a map term (:map ...) or an alist directly
                   (let ((ann-pairs (cond
                                     ((and (consp ann) (eq (first ann) :map))
                                      (map-term-to-alist (second ann)))
                                     ((and (consp ann) (consp (first ann)))
                                      (map-term-to-alist ann))
                                     (t nil))))
                     (recur body (cons ann-pairs pairs))))
                 (apply #'append pairs))))
    (recur term nil)))

(defun maybe-nothing-p (val)
  (or (null val)
      (and (consp val) (null (cdr val)) (null (car val)))
      (and (consp val) (eq (first val) :nothing))
      (and (consp val) (eq (first val) :maybe)
           (or (< (length val) 2)
               (null (second val))
               (and (consp (second val)) (eq (first (second val)) :nothing))))))

(defun ann-maybe-value (val)
  (cond
    ((and (consp val) (eq (first val) :just)) (second val))
    ((and (consp val) (eq (first val) :maybe))
     (let ((body (second val)))
       (if (and (consp body) (eq (first body) :just))
           (second body)
           body)))
    (t val)))

(defun deep-equal-p (a b)
  "Structural equality that works across alist-based records and hash tables."
  (cond
    ((eq a b) t)
    ((equal a b) t)
    ;; Hash table equality
    ((and (hash-table-p a) (hash-table-p b))
     (and (= (hash-table-count a) (hash-table-count b))
          (block nil
            (maphash (lambda (k v)
                       (multiple-value-bind (bv found) (gethash k b)
                         (unless (and found (deep-equal-p v bv))
                           (return nil))))
                     a)
            t)))
    ;; Hash table vs nil (empty map/set)
    ((and (hash-table-p a) (null b)) (= (hash-table-count a) 0))
    ((and (null a) (hash-table-p b)) (= (hash-table-count b) 0))
    ((and (consp a) (consp b) (consp (car a)) (consp (car b))
          (equal (mapcar #'car a) (mapcar #'car b)))
     (every (lambda (ka) (deep-equal-p (cdr (assoc (car ka) a))
                                        (cdr (assoc (car ka) b)))) a))
    ((and (consp a) (consp b) (= (length a) (length b)))
     (every #'deep-equal-p a b))
    (t nil)))

(defun set-annotation (key val m)
  (let ((cleaned (remove-if (lambda (pair) (deep-equal-p (car pair) key)) m)))
    (if (maybe-nothing-p val)
        cleaned
        (cons (cons key (ann-maybe-value val)) cleaned))))

(defun sort-map-entries (entries)
  "Sort map alist entries by key using generic-compare."
  (sort (copy-list entries) (lambda (a b) (< (generic-compare (car a) (car b)) 0))))

(defun make-meta-annotated (body anns-alist)
  "Build an annotated term in struct-compat format."
  (list :annotated (make-annotated_term body (list :map (sort-map-entries anns-alist)))))

(defun make-ann-type-scheme (arity)
  (labels ((make-type (n)
             (if (<= n 0) (list :unit)
                 (list :function (make-function_type (list :unit) (make-type (1- n)))))))
    (make-type_scheme nil (make-type arity) nil)))

(defun make-annotation-primitive (pname arity impl-fn)
  "Create a Primitive for annotation operations."
  (make-primitive pname (make-ann-type-scheme arity)
    (lambda (cx)
      (lambda (g)
        (lambda (args)
          ;; (format t "  DBG-PRIM ~A called with ~A args~%" pname (length args))
          (handler-case
              (let ((result (funcall impl-fn cx g args)))
                (if (eq (first result) :left)
                    (list :left (make-in_context
                                  (list :other (make-in_context (second result) cx)) cx))
                    result))
            (error (e)
              (format t "  DBG-PRIM ~A error: ~A~%" pname e)
              (list :left (make-in_context
                            (list :other (make-in_context (princ-to-string e) cx)) cx)))))))))

;; --------------------------------------------------------------------------
;; Extract annotations from terms in either format
;; --------------------------------------------------------------------------

(defun extract-annotations-any (term)
  "Extract annotation alist from a term that may be struct-compat or meta-encoded.
   Returns (values body annotations-alist) where body is the unwrapped body
   and annotations is the merged alist of all annotation layers."
  (cond
    ;; Struct-compat annotated: (:annotated AnnotatedTerm)
    ((is-meta-annotated-p term)
     (term-annotation-internal term))
    ;; Meta-encoded annotated: (:union Injection) where field is "annotated"
    ((and (meta-injection-p term)
          (let* ((inj (second term))
                 (field-alist (cdr (assoc :field inj)))
                 (field-name (cdr (assoc :name field-alist))))
            (equal field-name "annotated")))
     (let* ((inj (second term))
            (field-alist (cdr (assoc :field inj)))
            (field-term (cdr (assoc :term field-alist))))
       ;; field-term should be a Record for AnnotatedTerm with "body" and "annotation" fields
       (if (meta-record-p field-term)
           (let* ((rec (second field-term))
                  (fields (cdr (assoc :fields rec)))
                  (body (meta-field-value fields "body"))
                  (ann (meta-field-value fields "annotation")))
             ;; ann is a map term (:map alist)
             (let ((ann-pairs (cond
                                ((and (consp ann) (eq (first ann) :map))
                                 (map-term-to-alist (second ann)))
                                (t nil)))
                   ;; Recurse into body for layered annotations
                   (inner-anns (extract-annotations-any body)))
               (append ann-pairs inner-anns)))
           nil)))
    (t nil)))

(defun deannotate-any (term)
  "Strip all annotation layers from a term in either format."
  (cond
    ((is-meta-annotated-p term)
     (deannotate-term term))
    ((and (meta-injection-p term)
          (let* ((inj (second term))
                 (field-alist (cdr (assoc :field inj)))
                 (field-name (cdr (assoc :name field-alist))))
            (equal field-name "annotated")))
     (let* ((inj (second term))
            (field-alist (cdr (assoc :field inj)))
            (field-term (cdr (assoc :term field-alist))))
       (if (meta-record-p field-term)
           (let* ((rec (second field-term))
                  (fields (cdr (assoc :fields rec)))
                  (body (meta-field-value fields "body")))
             (deannotate-any body))
           term)))
    (t term)))

;; setTermAnnotation :: Name -> Maybe Term -> Term -> Term
(defun prim-set-term-annotation (cx g args)
  (declare (ignore cx g))
  (let* ((key (first args))
         (val (second args))
         (term (third args))
         ;; term is already stripped by the reducer's deannotate_term.
         ;; Check the cache for annotations that were stripped.
         (cached-anns (lookup-cached-annotations term))
         ;; Also check the term itself (may still have annotations in some cases)
         (term-anns (extract-annotations-any term))
         (existing-anns (or (and cached-anns (append cached-anns term-anns))
                            term-anns))
         (stripped (deannotate-any term))
         (anns (set-annotation key val existing-anns)))
    (list :right
          (if (null anns) stripped (make-meta-annotated stripped anns)))))

;; getTermAnnotation :: Name -> Term -> Maybe Term
(defun prim-get-term-annotation (cx g args)
  (declare (ignore cx g))
  (let* ((key (first args))
         (term (second args))
         ;; Check cache for stripped annotations
         (cached-anns (lookup-cached-annotations term))
         (term-anns (extract-annotations-any term))
         (anns (or (and cached-anns (append cached-anns term-anns))
                   term-anns))
         (result (cdr (assoc key anns :test #'deep-equal-p))))
    (list :right
          (if result (list :maybe result) (list :maybe nil)))))

;; setTermDescription :: Maybe String -> Term -> Term
(defun prim-set-term-description (cx g args)
  (let* ((d (first args))
         (term (second args))
         (term-val
           (when (not (maybe-nothing-p d))
             (let* ((inner (ann-maybe-value d))
                    (s (if (and (consp inner) (eq (first inner) :literal)
                                (consp (second inner)) (eq (first (second inner)) :string))
                           (second (second inner))
                           (princ-to-string inner))))
               (list :literal (list :string s)))))
         (desc-key (list :wrap (make-wrapped_term "hydra.core.Name"
                                 (list :literal (list :string "description")))))
         (maybe-val (if term-val (list :maybe term-val) (list :maybe (list :nothing)))))
    (prim-set-term-annotation cx g (list desc-key maybe-val term))))

;; getTermDescription :: Context -> Graph -> Term -> Either Error (Maybe String)
(defun prim-get-term-description (cx g args)
  (declare (ignore cx g))
  (let* ((term (third args))
         ;; Peel type lambdas/applications (struct-compat format)
         (peeled
           (labels ((peel (t_)
                      (cond
                        ((and (consp t_) (member (first t_) '(:type_lambda :type_application)))
                         (let* ((rec (second t_))
                                (body (cond
                                        ((eq (first t_) :type_lambda) (type_lambda-body rec))
                                        ((eq (first t_) :type_application) (type_application_term-body rec)))))
                           (peel body)))
                        (t t_))))
             (peel term)))
         (cached-anns (lookup-cached-annotations peeled))
         (term-anns (extract-annotations-any peeled))
         (anns (or (and cached-anns (append cached-anns term-anns)) term-anns))
         (desc-entry
           (find-if (lambda (pair)
                      (let ((k (car pair)))
                        (and (consp k) (eq (first k) :wrap)
                             (let ((wt (second k)))
                               (and (equal (wrapped_term-type_name wt) "hydra.core.Name")
                                    (let ((b (wrapped_term-body wt)))
                                      (and (consp b) (eq (first b) :literal)
                                           (consp (second b)) (eq (first (second b)) :string)
                                           (equal (second (second b)) "description"))))))))
                    anns))
         (desc-term (when desc-entry (cdr desc-entry))))
    (if desc-term
        (labels ((extract-str (t_)
                   ;; In struct-compat format: (:literal (:string "value"))
                   (when (and (consp t_) (eq (first t_) :literal)
                              (consp (second t_)) (eq (first (second t_)) :string))
                     (second (second t_)))))
          (let ((s (extract-str desc-term)))
            (if s
                (list :right (list :either (list :right (list :maybe (list :literal (list :string s))))))
                (list :right (list :either (list :right (list :maybe nil)))))))
        (list :right (list :either (list :right (list :maybe nil)))))))

;; ==========================================================================
;; Graph construction
;; ==========================================================================

;; Side channel for preserving annotations across primitive dispatch.
;; The reducer calls deannotate_term on primitive args, which strips annotations.
;; We wrap deannotate_term to cache annotations keyed by (eq) identity of the body.
(defvar *annotation-cache* (make-hash-table :test #'equal))

(defun install-annotation-cache ()
  "Wrap hydra_rewriting_deannotate_term to cache annotations before stripping."
  (let ((original hydra_rewriting_deannotate_term))
    (setf hydra_rewriting_deannotate_term
          (lambda (t_)
            (when (and (consp t_) (eq (first t_) :annotated))
              ;; Cache the full annotated term's annotations keyed by the stripped body
              (let ((body (funcall original t_))
                    (anns (term-annotation-internal t_)))
                (when anns
                  ;; Replace any existing cached annotations (latest snapshot wins)
                  (setf (gethash body *annotation-cache*) anns))))
            (funcall original t_)))))

(defun lookup-cached-annotations (term)
  "Look up cached annotations for a term (by eq identity)."
  (gethash term *annotation-cache*))

(defun clear-annotation-cache ()
  (clrhash *annotation-cache*))

(defun build-test-graph ()
  (let* ((std-prims (standard-library))
         (ann-prims-list
           (list
             (list "hydra.annotations.setTermAnnotation"
                   (make-annotation-primitive "hydra.annotations.setTermAnnotation" 3
                     #'prim-set-term-annotation))
             (list "hydra.annotations.getTermAnnotation"
                   (make-annotation-primitive "hydra.annotations.getTermAnnotation" 2
                     #'prim-get-term-annotation))
             (list "hydra.annotations.setTermDescription"
                   (make-annotation-primitive "hydra.annotations.setTermDescription" 2
                     #'prim-set-term-description))
             (list "hydra.annotations.getTermDescription"
                   (make-annotation-primitive "hydra.annotations.getTermDescription" 3
                     #'prim-get-term-description))))
         (ann-prims-map (funcall hydra_lib_maps_from_list ann-prims-list))
         (all-prims (funcall (funcall hydra_lib_maps_union ann-prims-map) std-prims))
         (prim-entries (funcall hydra_lib_maps_to_list all-prims))
         (bound-terms
           (funcall hydra_lib_maps_from_list
             (append
               (mapcar (lambda (entry)
                         (list (first entry) (list :function (list :primitive (first entry)))))
                       prim-entries)
               (list
                 (list "hydra.monads.emptyContext" (list :unit))
                 (list "hydra.lexical.emptyGraph" (list :unit)))))))
    (list (cons :bound_terms bound-terms)
          (cons :bound_types hydra_lib_maps_empty)
          (cons :class_constraints hydra_lib_maps_empty)
          (cons :lambda_variables hydra_lib_sets_empty)
          (cons :metadata hydra_lib_maps_empty)
          (cons :primitives all-prims)
          (cons :schema_types hydra_lib_maps_empty)
          (cons :type_variables hydra_lib_sets_empty))))

(defvar *test-graph* nil)
(defvar *annotation-cache-installed* nil)
(defun get-test-graph ()
  (unless *annotation-cache-installed*
    (install-annotation-cache)
    (setf *annotation-cache-installed* t))
  (unless *test-graph*
    (let ((base (build-test-graph)))
      ;; Enhance with schema types from bootstrap and test types
      (handler-case
        (let* (;; Bootstrap types (kernel type schemes)
               (bootstrap-types (if (boundp 'hydra_json_bootstrap_types_by_name)
                                    hydra_json_bootstrap_types_by_name
                                    nil))
               ;; Test types
               (test-types (if (boundp 'hydra_test_test_graph_test_types)
                               hydra_test_test_graph_test_types
                               nil))
               ;; f_type_to_type_scheme conversion
               (type-to-ts (when (boundp 'hydra_rewriting_f_type_to_type_scheme)
                             hydra_rewriting_f_type_to_type_scheme))
               ;; Build kernel schemas: map each bootstrap type through type-to-ts
               (kernel-entries
                (when (and bootstrap-types type-to-ts)
                  (mapcar (lambda (entry)
                            (list (first entry) (funcall type-to-ts (second entry))))
                          (funcall hydra_lib_maps_to_list bootstrap-types))))
               ;; Build test schemas (test-types is a Hydra map, convert to list first)
               (test-entries
                (when (and test-types type-to-ts)
                  (mapcar (lambda (entry)
                            (list (first entry) (funcall type-to-ts (second entry))))
                          (funcall hydra_lib_maps_to_list test-types))))
               ;; Merge all schema types
               (all-entries (append (or kernel-entries nil) (or test-entries nil)))
               (schema-types (funcall hydra_lib_maps_from_list all-entries))
               ;; Test terms (already a Hydra map)
               (test-terms-map (if (boundp 'hydra_test_test_graph_test_terms)
                                   hydra_test_test_graph_test_terms
                                   hydra_lib_maps_empty))
               ;; Update graph
               (enhanced (copy-list base)))
          (setf (cdr (assoc :schema_types enhanced)) schema-types)
          (setf (cdr (assoc :bound_terms enhanced))
                (funcall (funcall hydra_lib_maps_union test-terms-map)
                         (cdr (assoc :bound_terms base))))
          (let ((st-count (length (funcall hydra_lib_maps_to_list schema-types))))
            (format t "DEBUG: schema types: ~A~%" st-count))
          (setf *test-graph* enhanced))
        (error (e)
          (format t "WARNING: Could not enhance test graph: ~A~%" e)
          (setf *test-graph* base)))))
  *test-graph*)

(defun empty-context ()
  (list (cons :functions nil)
        (cons :annotations nil)
        (cons :variable_types nil)))

;; ==========================================================================
;; Term comparison (following Java TestSuiteRunner / Clojure pattern)
;; ==========================================================================

(defun show-term (t_)
  (handler-case (funcall hydra_show_core_term t_)
    (error () nil)))

(defun normalize-show (s)
  "Normalize set element ordering in show output: {a, b, c} -> sorted."
  (if (null s) s
      (let ((result (copy-seq s))
            (start 0))
        (loop
          (let ((open-pos (position #\{ result :start start)))
            (unless open-pos (return result))
            (let ((close-pos (position #\} result :start (1+ open-pos))))
              (unless close-pos (return result))
              (let* ((contents (subseq result (1+ open-pos) close-pos))
                     (elems (mapcar (lambda (e) (string-trim " " e))
                                    (hydra_lib_strings_split_on "," contents)))
                     (sorted (sort (copy-list elems) #'string<))
                     (joined (format nil "~{~A~^, ~}" sorted))
                     (new-block (format nil "{~A}" joined)))
                (setf result (concatenate 'string
                               (subseq result 0 open-pos)
                               new-block
                               (subseq result (1+ close-pos))))
                (setf start (+ open-pos (length new-block))))))))))

(defun float-close-p (actual expected)
  "Check if two float64 show strings are close enough."
  (let ((suffix ":float64"))
    (when (and (> (length actual) (length suffix))
               (string= suffix (subseq actual (- (length actual) (length suffix))))
               (> (length expected) (length suffix))
               (string= suffix (subseq expected (- (length expected) (length suffix)))))
      (handler-case
          (let ((av (read-from-string (subseq actual 0 (- (length actual) (length suffix)))))
                (ev (read-from-string (subseq expected 0 (- (length expected) (length suffix))))))
            (when (and (numberp av) (numberp ev))
              (or (= av ev)
                  (< (abs (- (coerce av 'double-float) (coerce ev 'double-float)))
                     1.0d-10))))
        (error () nil)))))

(defun term-to-meta (term)
  "Recursively convert struct-compat term to meta-encoded representation."
  (if (not (consp term)) term
    (let ((tag (first term)))
      (case tag
        (:annotated
          (let* ((at (second term))
                 (body (annotated_term-body at))
                 (ann (annotated_term-annotation at))
                 (ann-term (if (and (consp ann) (eq (first ann) :map)) ann (list :map ann))))
            (list :union (make-injection "hydra.core.Term"
                    (make-field "annotated"
                      (list :record (make-record "hydra.core.AnnotatedTerm"
                        (list (make-field "body" (term-to-meta body))
                              (make-field "annotation" (term-to-meta-map ann-term))))))))))
        (:literal
          (let ((lit (second term)))
            (list :union (make-injection "hydra.core.Term"
                    (make-field "literal" (literal-to-meta lit))))))
        (:application
          (let ((app (second term)))
            (list :union (make-injection "hydra.core.Term"
                    (make-field "application"
                      (list :record (make-record "hydra.core.Application"
                        (list (make-field "function" (term-to-meta (application-function app)))
                              (make-field "argument" (term-to-meta (application-argument app)))))))))))
        (:variable
          (let ((name (second term)))
            (list :union (make-injection "hydra.core.Term"
                    (make-field "variable"
                      (list :wrap (make-wrapped_term "hydra.core.Name"
                              (list :literal (list :string name)))))))))
        (:wrap
          (let ((wt (second term)))
            (list :union (make-injection "hydra.core.Term"
                    (make-field "wrap"
                      (list :record (make-record "hydra.core.WrappedTerm"
                        (list (make-field "typeName"
                                (list :wrap (make-wrapped_term "hydra.core.Name"
                                        (list :literal (list :string (wrapped_term-type_name wt))))))
                              (make-field "body" (term-to-meta (wrapped_term-body wt)))))))))))
        (:maybe
          (list :union (make-injection "hydra.core.Term"
                  (make-field "maybe"
                    (if (second term)
                        (list :maybe (term-to-meta (second term)))
                        (list :maybe nil))))))
        (:list
          (list :union (make-injection "hydra.core.Term"
                  (make-field "list"
                    (list :list (mapcar #'term-to-meta (second term)))))))
        (:map
          (list :union (make-injection "hydra.core.Term"
                  (make-field "map" (term-to-meta-map term)))))
        (otherwise term)))))

(defun literal-to-meta (lit)
  "Convert struct-compat literal to meta-encoded."
  (if (not (consp lit)) lit
    (let ((tag (first lit)))
      (case tag
        (:string (list :union (make-injection "hydra.core.Literal"
                        (make-field "string" (list :literal lit)))))
        (:boolean (list :union (make-injection "hydra.core.Literal"
                         (make-field "boolean" (list :literal lit)))))
        (:integer (integer-to-meta (second lit)))
        (:float (float-to-meta (second lit)))
        (otherwise (list :union (make-injection "hydra.core.Literal"
                          (make-field (string-downcase (symbol-name tag))
                            (list :literal lit)))))))))

(defun integer-to-meta (ival)
  (if (not (consp ival)) ival
    (let ((tag (first ival)))
      (list :union (make-injection "hydra.core.Literal"
              (make-field "integer"
                (list :union (make-injection "hydra.core.IntegerValue"
                        (make-field (string-downcase (symbol-name tag))
                          (list :literal (list :integer ival)))))))))))

(defun float-to-meta (fval)
  (if (not (consp fval)) fval
    (let ((tag (first fval)))
      (list :union (make-injection "hydra.core.Literal"
              (make-field "float"
                (list :union (make-injection "hydra.core.FloatValue"
                        (make-field (string-downcase (symbol-name tag))
                          (list :literal (list :float fval)))))))))))

(defun ensure-name-key (k)
  "If k is a plain string, wrap it as a Name term (:WRAP WrappedTerm). If already wrapped, leave as-is."
  (if (stringp k)
      (list :wrap (make-wrapped_term "hydra.core.Name" (list :literal (list :string k))))
      k))

(defun term-to-meta-map (m)
  "Convert map term, keeping (:map ...) wrapper, wrapping string keys as Names, and converting values."
  (if (and (consp m) (eq (first m) :map))
      (list :map (mapcar (lambda (entry)
                           (cons (ensure-name-key (car entry))
                                 (term-to-meta (cdr entry))))
                         (second m)))
      m))

(defun hash-table-is-set-p (ht)
  "Check if a hash table represents a set (all values are T)."
  (block nil
    (maphash (lambda (k v) (declare (ignore k)) (unless (eq v t) (return nil))) ht)
    t))

(defun normalize-hash-tables (x)
  "Recursively convert hash tables to sorted lists/alists for comparison.
   Sets (all values T) become sorted lists of keys.
   Maps become sorted alists of (key . val) pairs."
  (cond
    ((hash-table-p x)
     (if (hash-table-is-set-p x)
         ;; Set: return sorted list of keys
         (let ((keys nil))
           (maphash (lambda (k v) (declare (ignore v)) (push (normalize-hash-tables k) keys)) x)
           (sort keys (lambda (a b) (< (generic-compare a b) 0))))
         ;; Map: return sorted alist
         (let ((pairs nil))
           (maphash (lambda (k v) (push (cons (normalize-hash-tables k)
                                              (normalize-hash-tables v)) pairs)) x)
           (sort pairs (lambda (a b) (< (generic-compare (car a) (car b)) 0))))))
    ((consp x)
     (cons (normalize-hash-tables (car x))
           (normalize-hash-tables (cdr x))))
    (t x)))

(defun terms-match-p (actual expected)
  (or (deep-equal-p (normalize-hash-tables actual) (normalize-hash-tables expected))
      (handler-case
          (let ((a-str (show-term actual))
                (e-str (show-term expected)))
            (or (equal a-str e-str)
                (equal (normalize-show a-str) (normalize-show e-str))
                (float-close-p a-str e-str)))
        (error () nil))
      ;; Try converting struct-compat annotated terms to meta-encoding
      (when (and (consp actual) (eq (first actual) :annotated))
        (handler-case
          (let ((meta-actual (term-to-meta actual)))
            (or (equal meta-actual expected)
                (handler-case
                    (let ((a-str (show-term meta-actual))
                          (e-str (show-term expected)))
                      (or (equal a-str e-str)
                          (equal (normalize-show a-str) (normalize-show e-str))))
                  (error () nil))))
          (error () nil)))
      ;; Try converting Maybe-wrapped struct-compat terms
      (when (and (consp actual) (eq (first actual) :maybe)
                 (consp (second actual)) (eq (first (second actual)) :annotated))
        (handler-case
          (let ((meta-actual (list :maybe (term-to-meta (second actual)))))
            (or (equal meta-actual expected)
                (handler-case
                    (let ((a-str (show-term meta-actual))
                          (e-str (show-term expected)))
                      (or (equal a-str e-str)
                          (equal (normalize-show a-str) (normalize-show e-str))))
                  (error () nil))))
          (error () nil)))))

;; ==========================================================================
;; Test execution
;; ==========================================================================

(defun extract-error-msg (err)
  "Extract a human-readable message from nested InContext/OtherError."
  (cond
    ((and (consp err) (assoc :object err))
     (extract-error-msg (cdr (assoc :object err))))
    ((and (consp err) (eq (first err) :other))
     (extract-error-msg (second err)))
    (t (princ-to-string err))))

(defun run-evaluation-test (path tc)
  (clear-annotation-cache)
  (let* ((input (cdr (assoc :input tc)))
         (expected (cdr (assoc :output tc)))
         (graph (get-test-graph))
         (cx (empty-context))
         (eager t))
    (handler-case
        (let ((result (progn
                        (funcall (funcall (funcall (funcall hydra_reduction_reduce_term cx)
                                                 graph)
                                        eager)
                               input))))
          (if (eq (first result) :left)
              (progn
                (format t "FAIL: ~A~%" path)
                (format t "  ERROR: ~A~%" (extract-error-msg (second result)))
                (list 0 1 0))
              (let ((actual (second result)))
                (if (terms-match-p actual expected)
                    (list 1 0 0)
                    (progn
                      (format t "FAIL: ~A~%" path)
                      (handler-case
                          (progn
                            (format t "  Expected: ~A~%" (show-term expected))
                            (format t "  Actual:   ~A~%" (show-term actual)))
                        (error ()
                          (format t "  Expected (raw): ~S~%" expected)
                          (format t "  Actual (raw):   ~S~%" actual)))
                      (list 0 1 0))))))
      (error (e)
        (format t "FAIL: ~A~%" path)
        (format t "  EXCEPTION: ~A~%" e)
        (list 0 1 0)))))

;; ==========================================================================
;; Additional test runners (non-evaluation)
;; ==========================================================================

(defun a (key alist) (cdr (assoc key alist :test #'eq)))

(defun show-type (t_)
  (handler-case (funcall hydra_show_core_type t_) (error () nil)))

(defun show-type-scheme (ts)
  (handler-case (funcall hydra_show_core_type_scheme ts) (error () nil)))

(defun show-let-fn (l)
  (handler-case (funcall hydra_show_core_let l) (error () nil)))

(defun normalize-type-var-names (s)
  "Simple alpha-equivalence normalization for type variable names."
  (if (or (null s) (not (stringp s))) s s))  ;; TODO: implement full normalization

(defun string-comparison-test (path expected-str actual-str)
  (handler-case
    (let ((e (or expected-str "#f"))
          (a_ (or actual-str "#f")))
      (if (equal e a_)
          (list 1 0 0)
          (progn
            (format t "FAIL: ~A~%" path)
            (format t "  Expected: ~A~%" e)
            (format t "  Actual:   ~A~%" a_)
            (list 0 1 0))))
    (error (e) (format t "FAIL: ~A~%  EXCEPTION: ~A~%" path e) (list 0 1 0))))

(defun run-simple-test (path expected actual-fn)
  (handler-case
    (let ((actual (funcall actual-fn)))
      (if (terms-match-p actual expected)
          (list 1 0 0)
          (progn
            (format t "FAIL: ~A~%" path)
            (handler-case
              (progn (format t "  Expected: ~A~%" (show-term expected))
                     (format t "  Actual:   ~A~%" (show-term actual)))
              (error () (format t "  Expected (raw): ~S~%  Actual (raw): ~S~%" expected actual)))
            (list 0 1 0))))
    (error (e) (format t "FAIL: ~A~%  EXCEPTION: ~A~%" path e) (list 0 1 0))))

(defun run-either-test (path expected either-result)
  (handler-case
    (if (eq (first either-result) :left)
        (progn (format t "FAIL: ~A~%  ERROR: ~A~%" path (second either-result)) (list 0 1 0))
        (let ((actual (second either-result)))
          (if (terms-match-p actual expected) (list 1 0 0)
              (progn
                (format t "FAIL: ~A~%" path)
                (handler-case
                  (progn (format t "  Expected: ~A~%" (show-term expected))
                         (format t "  Actual:   ~A~%" (show-term actual)))
                  (error () (format t "  Expected (raw): ~S~%  Actual (raw): ~S~%" expected actual)))
                (list 0 1 0)))))
    (error (e) (format t "FAIL: ~A~%  EXCEPTION: ~A~%" path e) (list 0 1 0))))

(defun run-alpha-conversion-test (path tc)
  (run-simple-test path (a :result tc)
    (lambda () (funcall (funcall (funcall hydra_reduction_alpha_convert (a :old_variable tc)) (a :new_variable tc)) (a :term tc)))))

(defun run-case-conversion-test (path tc)
  (run-simple-test path (a :to_string tc)
    (lambda () (funcall (funcall (funcall hydra_formatting_convert_case (a :from_convention tc)) (a :to_convention tc)) (a :from_string tc)))))

(defun run-deannotate-term-test (path tc)
  (run-simple-test path (a :output tc) (lambda () (funcall hydra_rewriting_deannotate_term (a :input tc)))))

(defun run-deannotate-type-test (path tc)
  (run-simple-test path (a :output tc) (lambda () (funcall hydra_rewriting_deannotate_type (a :input tc)))))

(defun run-flatten-let-terms-test (path tc)
  (run-simple-test path (a :output tc) (lambda () (funcall hydra_rewriting_flatten_let_terms (a :input tc)))))

(defun run-free-variables-test (path tc)
  (run-simple-test path (a :output tc) (lambda () (funcall hydra_rewriting_free_variables_in_term (a :input tc)))))

(defun run-lift-lambda-test (path tc)
  (run-simple-test path (a :output tc) (lambda () (funcall hydra_rewriting_lift_lambda_above_let (a :input tc)))))

(defun run-simplify-term-test (path tc)
  (run-simple-test path (a :output tc) (lambda () (funcall hydra_rewriting_simplify_term (a :input tc)))))

(defun run-normalize-type-vars-test (path tc)
  (run-simple-test path (a :output tc) (lambda () (funcall hydra_rewriting_normalize_type_variables_in_term (a :input tc)))))

(defun run-topological-sort-test (path tc)
  (run-simple-test path (a :expected tc) (lambda () (funcall hydra_sorting_topological_sort (a :adjacency_list tc)))))

(defun run-topological-sort-scc-test (path tc)
  (run-simple-test path (a :expected tc) (lambda () (funcall hydra_sorting_topological_sort_components (a :adjacency_list tc)))))

(defun run-serialization-test (path tc)
  (run-simple-test path (a :output tc)
    (lambda () (funcall hydra_serialization_print_expr (funcall hydra_serialization_parenthesize (a :input tc))))))

(defun run-type-reduction-test (path tc)
  (let ((cx (empty-context)) (graph (get-test-graph)))
    (run-either-test path (a :output tc)
      (funcall (funcall (funcall hydra_reduction_beta_reduce_type cx) graph) (a :input tc)))))

(defun run-unshadow-variables-test (path tc)
  (run-simple-test path (a :output tc) (lambda () (funcall hydra_rewriting_unshadow_variables (a :input tc)))))

(defun run-eta-expansion-test (path tc)
  (let ((cx (empty-context)) (graph (get-test-graph)))
    (run-either-test path (a :output tc)
      (funcall (funcall (funcall hydra_reduction_eta_expand_typed_term cx) graph) (a :input tc)))))

(defun run-inference-test (path tc)
  (let ((cx (empty-context)) (graph (get-test-graph)))
    (handler-case
      (let ((result (funcall (funcall (funcall hydra_inference_infer_type_of cx) graph) (a :input tc))))
        (if (eq (first result) :left)
            (progn (format t "FAIL: ~A~%  Inference ERROR: ~A~%" path (second result)) (list 0 1 0))
            (let* ((pair-val (second result))
                   (inner-pair (funcall hydra_lib_pairs_first pair-val))
                   (result-scheme (funcall hydra_lib_pairs_second inner-pair))
                   (expected-str (show-type-scheme (a :output tc)))
                   (actual-str (show-type-scheme result-scheme)))
              (string-comparison-test path expected-str actual-str))))
      (error (e) (format t "FAIL: ~A~%  EXCEPTION: ~A~%" path e) (list 0 1 0)))))

(defun run-inference-failure-test (path tc)
  (let ((cx (empty-context)) (graph (get-test-graph)))
    (handler-case
      (let ((result (funcall (funcall (funcall hydra_inference_infer_type_of cx) graph) (a :input tc))))
        (if (eq (first result) :left) (list 1 0 0)
            (progn (format t "FAIL: ~A~%  Expected inference failure but got success~%" path) (list 0 1 0))))
      (error (e) (declare (ignore e)) (list 1 0 0)))))

(defun type-scheme-to-type (ts)
  "Convert a TypeScheme back to a Type by wrapping forall binders."
  (let ((vars (cdr (assoc :variables ts)))
        (body (cdr (assoc :type ts))))
    (reduce (lambda (t_ v) (list :forall (make-forall_type v t_)))
            (reverse vars) :initial-value body)))

(defun run-type-checking-test (path tc)
  (let ((cx (empty-context)) (graph (get-test-graph)))
    (handler-case
      (let ((infer-result (funcall (funcall (funcall hydra_inference_infer_type_of cx) graph)
                                   (a :input tc))))
        (if (eq (first infer-result) :left)
            (progn (format t "FAIL: ~A~%  Inference failed: ~A~%" path (second infer-result))
                   (list 0 1 0))
            (let* ((pair-val (second infer-result))
                   (inner-pair (funcall hydra_lib_pairs_first pair-val))
                   (inferred-term (funcall hydra_lib_pairs_first inner-pair))
                   (result-scheme (funcall hydra_lib_pairs_second inner-pair))
                   (infer-cx (funcall hydra_lib_pairs_second pair-val))
                   (inferred-type (type-scheme-to-type result-scheme))
                   ;; Reconstruct via typeOf
                   (type-of-result (funcall (funcall (funcall (funcall hydra_checking_type_of infer-cx) graph)
                                                     nil) inferred-term)))
              (if (eq (first type-of-result) :left)
                  (progn (format t "FAIL: ~A~%  Type reconstruction failed: ~A~%" path (second type-of-result))
                         (list 0 1 0))
                  (let* ((reconstructed-type (funcall hydra_lib_pairs_first (second type-of-result)))
                         ;; Compare using show strings
                         (show-ts (when (boundp 'hydra_show_core_type_scheme) hydra_show_core_type_scheme))
                         (show-tp (when (boundp 'hydra_show_core_type) hydra_show_core_type))
                         (show-tm (when (boundp 'hydra_show_core_term) hydra_show_core_term))
                         (term-ok (equal (when show-tm (funcall show-tm (a :output_term tc)))
                                         (when show-tm (funcall show-tm inferred-term))))
                         (type-ok (equal (when show-tp (funcall show-tp (a :output_type tc)))
                                         (when show-tp (funcall show-tp inferred-type))))
                         (recon-ok (equal (when show-tp (funcall show-tp (a :output_type tc)))
                                          (when show-tp (funcall show-tp reconstructed-type)))))
                    (if (and term-ok type-ok recon-ok)
                        (list 1 0 0)
                        (progn
                          (format t "FAIL: ~A~%" path)
                          (unless term-ok
                            (format t "  Inferred term mismatch~%"))
                          (unless type-ok
                            (format t "  Inferred type mismatch~%"))
                          (unless recon-ok
                            (format t "  Reconstructed type mismatch~%"))
                          (list 0 1 0))))))))
      (error (e) (format t "FAIL: ~A~%  EXCEPTION: ~A~%" path e) (list 0 1 0)))))

(defun run-type-checking-failure-test (path tc)
  (let ((cx (empty-context)) (graph (get-test-graph)))
    (handler-case
      (let ((result (funcall (funcall (funcall hydra_inference_infer_type_of cx) graph)
                             (a :input tc))))
        (if (eq (first result) :left) (list 1 0 0)
            (progn (format t "FAIL: ~A~%  Expected type checking failure~%" path)
                   (list 0 1 0))))
      (error (e) (declare (ignore e)) (list 1 0 0)))))

(defun run-variable-occurs-in-type-test (path tc)
  (run-simple-test path (a :expected tc)
    (lambda () (funcall (funcall hydra_unification_variable_occurs_in_type (a :variable tc)) (a :type tc)))))

(defun run-subst-in-type-test (path tc)
  (let ((subst-alist (funcall hydra_lib_maps_from_list (a :substitution tc))))
    (run-simple-test path (a :output tc)
      (lambda () (funcall (funcall hydra_substitution_subst_in_type subst-alist) (a :input tc))))))

(defun run-unify-types-test (path tc)
  (let* ((cx (empty-context))
         (schema-entries (mapcar (lambda (n) (list n (make-type_scheme nil (list :variable n) nil))) (a :schema_types tc)))
         (schema-types (funcall hydra_lib_maps_from_list schema-entries))
         (result (funcall (funcall (funcall (funcall (funcall hydra_unification_unify_types cx) schema-types)
                                            (a :left tc)) (a :right tc)) "test"))
         (expected (a :expected tc)))
    (handler-case
      (cond
        ((eq (first expected) :left) (if (eq (first result) :left) (list 1 0 0)
                                         (progn (format t "FAIL: ~A~%  Expected failure~%" path) (list 0 1 0))))
        ((eq (first expected) :right)
         (if (eq (first result) :left)
             (progn (format t "FAIL: ~A~%  Expected success but got: ~A~%" path (second result)) (list 0 1 0))
             (let ((ne (sort (copy-list (normalize-hash-tables (second expected)))
                             (lambda (a b) (< (generic-compare a b) 0))))
                   (nr (sort (copy-list (normalize-hash-tables (second result)))
                             (lambda (a b) (< (generic-compare a b) 0)))))
               (if (deep-equal-p ne nr) (list 1 0 0)
                   (progn (format t "FAIL: ~A~%  Subst mismatch~%  Expected: ~S~%  Actual:   ~S~%" path ne nr)
                          (list 0 1 0))))))
        (t (list 0 1 0)))
      (error (e) (format t "FAIL: ~A~%  EXCEPTION: ~A~%" path e) (list 0 1 0)))))

(defun run-join-types-test (path tc)
  (let* ((cx (empty-context))
         (result (funcall (funcall (funcall (funcall hydra_unification_join_types cx) (a :left tc)) (a :right tc)) "test"))
         (expected (a :expected tc)))
    (handler-case
      (cond
        ((eq (first expected) :left) (if (eq (first result) :left) (list 1 0 0)
                                         (progn (format t "FAIL: ~A~%  Expected failure~%" path) (list 0 1 0))))
        ((eq (first expected) :right)
         (if (eq (first result) :left)
             (progn (format t "FAIL: ~A~%  Expected success but got: ~A~%" path (second result)) (list 0 1 0))
             (let ((ne (sort (copy-list (normalize-hash-tables (second expected)))
                             (lambda (a b) (< (generic-compare a b) 0))))
                   (nr (sort (copy-list (normalize-hash-tables (second result)))
                             (lambda (a b) (< (generic-compare a b) 0)))))
               (if (deep-equal-p ne nr) (list 1 0 0)
                   (progn (format t "FAIL: ~A~%  Value mismatch~%" path) (list 0 1 0))))))
        (t (list 0 1 0)))
      (error (e) (format t "FAIL: ~A~%  EXCEPTION: ~A~%" path e) (list 0 1 0)))))

(defun run-topological-sort-bindings-test (path tc)
  (let* ((binding-map (funcall hydra_lib_maps_from_list (a :bindings tc)))
         (result (funcall hydra_rewriting_topological_sort_binding_map binding-map))
         (expected (a :expected tc)))
    (if (equal expected result) (list 1 0 0)
        (progn (format t "FAIL: ~A~%  Expected: ~S~%  Actual: ~S~%" path expected result) (list 0 1 0)))))

(defun empty-graph-fn ()
  (let ((std-prims (standard-library)))
    (list (cons :bound_terms nil) (cons :bound_types nil) (cons :class_constraints nil)
          (cons :lambda_variables nil) (cons :metadata nil) (cons :primitives std-prims)
          (cons :schema_types nil) (cons :type_variables nil))))

(defun run-hoist-case-statements-test (path tc)
  (let ((eg (empty-graph-fn)))
    (run-simple-test path (a :output tc)
      (lambda () (funcall (funcall hydra_hoisting_hoist_case_statements eg) (a :input tc))))))

(defun hoist-predicate-fn (pred)
  (let ((pred-type (first pred)))
    (cond
      ((eq pred-type :nothing) (lambda (pair) (declare (ignore pair)) nil))
      ((eq pred-type :lists) (lambda (pair)
                               (let ((term (funcall hydra_lib_pairs_second pair)))
                                 (and (consp term) (eq (first term) :list)))))
      ((eq pred-type :applications) (lambda (pair)
                                      (let ((term (funcall hydra_lib_pairs_second pair)))
                                        (and (consp term) (eq (first term) :application)))))
      ((eq pred-type :case_statements) (lambda (pair)
                                         (let ((term (funcall hydra_lib_pairs_second pair)))
                                           (and (consp term) (eq (first term) :function)
                                                (let ((f (second term)))
                                                  (and (consp f) (eq (first f) :elimination)))))))
      (t (lambda (pair) (declare (ignore pair)) nil)))))

(defun run-hoist-subterms-test (path tc)
  (let ((eg (empty-graph-fn)) (pred (hoist-predicate-fn (a :predicate tc))))
    (run-simple-test path (a :output tc)
      (lambda () (funcall (funcall (funcall hydra_hoisting_hoist_subterms pred) eg) (a :input tc))))))

(defun run-hoist-let-bindings-test (path tc)
  (handler-case
    (let* ((result (funcall hydra_hoisting_hoist_all_let_bindings (a :input tc)))
           (expected-str (show-let-fn (a :output tc)))
           (actual-str (show-let-fn result)))
      (string-comparison-test path expected-str actual-str))
    (error (e) (format t "FAIL: ~A~%  EXCEPTION: ~A~%" path e) (list 0 1 0))))

(defun run-hoist-polymorphic-let-bindings-test (path tc)
  (handler-case
    (let* ((result (funcall (funcall hydra_hoisting_hoist_polymorphic_let_bindings
                                     (lambda (x) (declare (ignore x)) t)) (a :input tc)))
           (expected-str (show-let-fn (a :output tc)))
           (actual-str (show-let-fn result)))
      (string-comparison-test path expected-str actual-str))
    (error (e) (format t "FAIL: ~A~%  EXCEPTION: ~A~%" path e) (list 0 1 0))))

(defun run-rewrite-term-test (path tc)
  (let* ((rewriter (a :rewriter tc))
         (rewriter-type (first rewriter))
         (rewrite-impl
          (cond
            ((eq rewriter-type :replace_foo_with_bar)
             (lambda (recurse) (lambda (term)
               (if (and (consp term) (eq (first term) :literal)
                        (consp (second term)) (eq (first (second term)) :string) (equal (second (second term)) "foo"))
                   (list :literal (list :string "bar")) (funcall recurse term)))))
            ((eq rewriter-type :replace_int32_with_int64)
             (lambda (recurse) (lambda (term)
               (if (and (consp term) (eq (first term) :literal)
                        (consp (second term)) (eq (first (second term)) :integer)
                        (consp (second (second term))) (eq (first (second (second term))) :int32))
                   (list :literal (list :integer (list :int64 (second (second (second term))))))
                   (funcall recurse term)))))
            (t (lambda (recurse) (lambda (term) (funcall recurse term)))))))
    (run-simple-test path (a :output tc)
      (lambda () (funcall (funcall hydra_rewriting_rewrite_term rewrite-impl) (a :input tc))))))

(defun run-rewrite-type-test (path tc)
  (let* ((rewriter (a :rewriter tc))
         (rewriter-type (first rewriter))
         (rewrite-impl
          (cond
            ((eq rewriter-type :replace_string_with_int32)
             (lambda (recurse) (lambda (typ)
               (if (and (consp typ) (eq (first typ) :literal)
                        (consp (second typ)) (eq (first (second typ)) :string))
                   (list :literal (list :integer (list :int32 nil))) (funcall recurse typ)))))
            (t (lambda (recurse) (lambda (typ) (funcall recurse typ)))))))
    (run-simple-test path (a :output tc)
      (lambda () (funcall (funcall hydra_rewriting_rewrite_type rewrite-impl) (a :input tc))))))

(defun get-int32-val (term)
  (if (and (consp term) (eq (first term) :literal) (consp (second term)) (eq (first (second term)) :integer)
           (consp (second (second term))) (eq (first (second (second term))) :int32))
      (second (second (second term))) 0))

(defun run-fold-over-term-test (path tc)
  (let* ((order (a :traversal_order tc))
         (operation (a :operation tc))
         (op-type (first operation))
         (input (a :input tc)))
    (handler-case
      (let ((result
             (cond
               ((eq op-type :sum_int32_literals)
                (let ((sum (funcall (funcall (funcall (funcall hydra_rewriting_fold_over_term order)
                                                      (lambda (acc) (lambda (term) (+ acc (get-int32-val term))))) 0) input)))
                  (list :literal (list :integer (list :int32 sum)))))
               ((eq op-type :collect_list_lengths)
                (let ((lengths (funcall (funcall (funcall (funcall hydra_rewriting_fold_over_term order)
                                                          (lambda (acc) (lambda (term)
                                                            (if (and (consp term) (eq (first term) :list))
                                                                (append acc (list (length (second term))))
                                                                acc)))) nil) input)))
                  (list :list (mapcar (lambda (len) (list :literal (list :integer (list :int32 len)))) lengths))))
               ((eq op-type :collect_labels)
                (let ((labels_ (funcall (funcall (funcall (funcall hydra_rewriting_fold_over_term order)
                                                           (lambda (acc) (lambda (term)
                                                             (if (and (consp term) (eq (first term) :pair))
                                                                 (let* ((fst (funcall hydra_lib_pairs_first (second term))))
                                                                   (if (and (consp fst) (eq (first fst) :literal)
                                                                            (consp (second fst)) (eq (first (second fst)) :string))
                                                                       (append acc (list (second fst))) acc))
                                                                 acc)))) nil) input)))
                  (list :list (mapcar (lambda (label) (list :literal label)) labels_)))))))
        (if (terms-match-p result (a :output tc)) (list 1 0 0)
            (progn (format t "FAIL: ~A~%" path)
                   (handler-case (progn (format t "  Expected: ~A~%" (show-term (a :output tc)))
                                        (format t "  Actual:   ~A~%" (show-term result)))
                     (error () nil))
                   (list 0 1 0))))
      (error (e) (format t "FAIL: ~A~%  EXCEPTION: ~A~%" path e) (list 0 1 0)))))

(defun run-json-parser-test (path tc)
  (handler-case
    (let ((result (funcall hydra_json_parser_parse_json (a :input tc))))
      (if (terms-match-p result (a :output tc)) (list 1 0 0)
          (progn (format t "FAIL: ~A~%  Expected (raw): ~S~%  Actual (raw): ~S~%" path (a :output tc) result) (list 0 1 0))))
    (error (e) (format t "FAIL: ~A~%  EXCEPTION: ~A~%" path e) (list 0 1 0))))

(defun run-json-writer-test (path tc)
  (run-simple-test path (a :output tc) (lambda () (funcall hydra_json_writer_print_json (a :input tc)))))

;; ==========================================================================
;; JSON coder tests
;; ==========================================================================

(defun run-json-coder-test (path tc)
  (let ((empty-types hydra_lib_maps_empty))
    (handler-case
      (let ((encode-result (funcall hydra_json_encode_to_json (a :term tc))))
        (if (eq (first encode-result) :left)
            (progn (format t "FAIL: ~A~%  JSON encode failed: ~A~%" path (second encode-result)) (list 0 1 0))
            (let ((encoded (second encode-result)))
              (if (not (terms-match-p (a :json tc) encoded))
                  (progn (format t "FAIL: ~A~%  JSON encode mismatch~%" path) (list 0 1 0))
                  (let ((decode-result (funcall (funcall (funcall (funcall hydra_json_decode_from_json empty-types)
                                                                  (make-hydra_core_name :value "test"))
                                                         (a :type tc))
                                                encoded)))
                    (if (eq (first decode-result) :left)
                        (progn (format t "FAIL: ~A~%  JSON decode failed: ~A~%" path (second decode-result)) (list 0 1 0))
                        (if (terms-match-p (a :term tc) (second decode-result))
                            (list 1 0 0)
                            (progn (format t "FAIL: ~A~%  Roundtrip mismatch~%" path) (list 0 1 0)))))))))
      (error (e) (format t "FAIL: ~A~%  EXCEPTION: ~A~%" path e) (list 0 1 0)))))

(defun run-json-roundtrip-test (path tc)
  (let ((empty-types hydra_lib_maps_empty))
    (handler-case
      (let ((encode-result (funcall hydra_json_encode_to_json (a :term tc))))
        (if (eq (first encode-result) :left)
            (progn (format t "FAIL: ~A~%  JSON encode failed: ~A~%" path (second encode-result)) (list 0 1 0))
            (let* ((encoded (second encode-result))
                   (decode-result (funcall (funcall (funcall (funcall hydra_json_decode_from_json empty-types)
                                                             (make-hydra_core_name :value "test"))
                                                    (a :type tc))
                                           encoded)))
              (if (eq (first decode-result) :left)
                  (progn (format t "FAIL: ~A~%  JSON decode failed: ~A~%" path (second decode-result)) (list 0 1 0))
                  (if (terms-match-p (a :term tc) (second decode-result))
                      (list 1 0 0)
                      (progn (format t "FAIL: ~A~%  Roundtrip mismatch~%" path) (list 0 1 0)))))))
      (error (e) (format t "FAIL: ~A~%  EXCEPTION: ~A~%" path e) (list 0 1 0)))))

(defun run-json-decode-test (path tc)
  (let ((empty-types hydra_lib_maps_empty))
    (handler-case
      (let* ((decode-result (funcall (funcall (funcall (funcall hydra_json_decode_from_json empty-types)
                                                       (make-hydra_core_name :value "test"))
                                              (a :type tc))
                                     (a :json tc)))
             (expected (a :expected tc)))
        (cond
          ((eq (first expected) :left)
           (if (eq (first decode-result) :left) (list 1 0 0)
               (progn (format t "FAIL: ~A~%  Expected decode failure~%" path) (list 0 1 0))))
          ((eq (first expected) :right)
           (if (eq (first decode-result) :left)
               (progn (format t "FAIL: ~A~%  JSON decode failed: ~A~%" path (second decode-result)) (list 0 1 0))
               (if (terms-match-p (second expected) (second decode-result))
                   (list 1 0 0)
                   (progn (format t "FAIL: ~A~%  Decode mismatch~%" path) (list 0 1 0)))))
          (t (list 0 1 0))))
      (error (e) (format t "FAIL: ~A~%  EXCEPTION: ~A~%" path e) (list 0 1 0)))))

(defun run-validate-core-term-test (path tc)
  (if (boundp 'hydra_validate_core_term)
    (run-simple-test path (a :output tc)
      (lambda () (funcall (funcall (funcall (symbol-value 'hydra_validate_core_term) (a :typed tc)) (get-test-graph)) (a :input tc))))
    (list 0 0 1)))

;; ==========================================================================
;; Test case dispatcher
;; ==========================================================================

(defun run-test-case (path tcase)
  (handler-case
      (let* ((tname (cdr (assoc :name tcase)))
             (full (format nil "~A > ~A" path tname))
             (tags (cdr (assoc :tags tcase)))
             (disabled-p (member "disabled" tags :test #'equal))
             (tc (cdr (assoc :case tcase))))
        (if disabled-p
            (list 0 0 1)
            (let ((case-type (first tc)) (case-data (second tc)))
              (cond
                ((eq case-type :evaluation)              (run-evaluation-test full case-data))
                ((eq case-type :alpha_conversion)        (run-alpha-conversion-test full case-data))
                ((eq case-type :case_conversion)         (run-case-conversion-test full case-data))
                ((eq case-type :deannotate_term)         (run-deannotate-term-test full case-data))
                ((eq case-type :deannotate_type)         (run-deannotate-type-test full case-data))
                ((eq case-type :flatten_let_terms)       (run-flatten-let-terms-test full case-data))
                ((eq case-type :free_variables)          (run-free-variables-test full case-data))
                ((eq case-type :lift_lambda_above_let)   (run-lift-lambda-test full case-data))
                ((eq case-type :simplify_term)           (run-simplify-term-test full case-data))
                ((eq case-type :normalize_type_variables) (run-normalize-type-vars-test full case-data))
                ((eq case-type :topological_sort)        (run-topological-sort-test full case-data))
                ((eq case-type :topological_sort_s_c_c)  (run-topological-sort-scc-test full case-data))
                ((eq case-type :serialization)           (run-serialization-test full case-data))
                ((eq case-type :type_reduction)          (run-type-reduction-test full case-data))
                ((eq case-type :unshadow_variables)      (run-unshadow-variables-test full case-data))
                ((eq case-type :eta_expansion)           (run-eta-expansion-test full case-data))
                ((eq case-type :inference)               (run-inference-test full case-data))
                ((eq case-type :inference_failure)       (run-inference-failure-test full case-data))
                ((eq case-type :type_checking)           (run-type-checking-test full case-data))
                ((eq case-type :type_checking_failure)   (run-type-checking-failure-test full case-data))
                ((eq case-type :variable_occurs_in_type) (run-variable-occurs-in-type-test full case-data))
                ((eq case-type :subst_in_type)           (run-subst-in-type-test full case-data))
                ((eq case-type :unify_types)             (run-unify-types-test full case-data))
                ((eq case-type :join_types)              (run-join-types-test full case-data))
                ((eq case-type :topological_sort_bindings) (run-topological-sort-bindings-test full case-data))
                ((eq case-type :hoist_case_statements)   (run-hoist-case-statements-test full case-data))
                ((eq case-type :hoist_subterms)          (run-hoist-subterms-test full case-data))
                ((eq case-type :hoist_let_bindings)      (run-hoist-let-bindings-test full case-data))
                ((eq case-type :hoist_polymorphic_let_bindings) (run-hoist-polymorphic-let-bindings-test full case-data))
                ((eq case-type :rewrite_term)            (run-rewrite-term-test full case-data))
                ((eq case-type :rewrite_type)            (run-rewrite-type-test full case-data))
                ((eq case-type :fold_over_term)          (run-fold-over-term-test full case-data))
                ((eq case-type :json_parser)             (run-json-parser-test full case-data))
                ((eq case-type :json_writer)             (run-json-writer-test full case-data))
                ((eq case-type :json_coder)              (run-json-coder-test full case-data))
                ((eq case-type :json_roundtrip)          (run-json-roundtrip-test full case-data))
                ((eq case-type :json_decode)             (run-json-decode-test full case-data))
                ((eq case-type :json_encode)             (list 0 0 1)) ;; no test data (Java also skips)
                ((eq case-type :validate_core_term)      (run-validate-core-term-test full case-data))
                ((eq case-type :delegated_evaluation)    (list 0 0 1))
                (t (list 0 0 1))))))
    (error (e)
      (let* ((tname (cdr (assoc :name tcase)))
             (full (format nil "~A > ~A" path tname)))
        (format t "FAIL: ~A~%" full)
        (format t "  EXCEPTION: ~A~%" e)
        (list 0 1 0)))))

;; Benchmark helpers

(defun benchmark-to-json (b)
  "Convert a benchmark alist to a JSON string."
  (let* ((path (cdr (assoc :path b)))
         (passed (cdr (assoc :passed b)))
         (failed (cdr (assoc :failed b)))
         (skipped (cdr (assoc :skipped b)))
         (total-ms (cdr (assoc :total-time-ms b)))
         (subs (cdr (assoc :subgroups b)))
         (sub-json (if (null subs) ""
                       (format nil ", \"subgroups\": [~{~A~^, ~}]"
                               (mapcar #'benchmark-to-json subs)))))
    (format nil "{\"path\": \"~A\", \"passed\": ~A, \"failed\": ~A, \"skipped\": ~A, \"totalTimeMs\": ~A~A}"
            path passed failed skipped total-ms sub-json)))

(defun write-benchmark-json (benchmark total-ms)
  "Write benchmark JSON to HYDRA_BENCHMARK_OUTPUT if set."
  (let ((output-path #+sbcl (sb-ext:posix-getenv "HYDRA_BENCHMARK_OUTPUT")
                     #+ccl (ccl:getenv "HYDRA_BENCHMARK_OUTPUT")
                     #-(or sbcl ccl) nil))
    (when output-path
      (let* ((top-groups (cdr (assoc :subgroups benchmark)))
             (json-groups (format nil "~{    ~A~^,~%~}" (mapcar #'benchmark-to-json top-groups)))
             (passed (cdr (assoc :passed benchmark)))
             (failed (cdr (assoc :failed benchmark)))
             (skipped (cdr (assoc :skipped benchmark)))
             (json (format nil "{~%  \"groups\": [~%~A~%  ],~%  \"summary\": {~%    \"totalPassed\": ~A,~%    \"totalFailed\": ~A,~%    \"totalSkipped\": ~A,~%    \"totalTimeMs\": ~A~%  }~%}"
                           json-groups passed failed skipped (round total-ms))))
        (with-open-file (out output-path :direction :output :if-exists :supersede)
          (write-string json out))
        (format t "Benchmark output: ~A~%" output-path)))))

(defun run-test-group (path group &optional (bench-prefix ""))
  "Run a test group. Returns (list pass fail skip benchmark).
   benchmark is an alist with :path, :passed, :failed, :skipped, :total-time-ms, :subgroups."
  (handler-case
      (let* ((gname (cdr (assoc :name group)))
             (full (if (string= path "") gname (format nil "~A > ~A" path gname)))
             (bench-path (if (string= bench-prefix "") gname (format nil "~A/~A" bench-prefix gname)))
             (t0 (get-internal-real-time))
             (sub-results (mapcar (lambda (sg) (run-test-group full sg bench-path))
                                  (cdr (assoc :subgroups group))))
             (case-results (mapcar (lambda (tc) (run-test-case full tc))
                                   (cdr (assoc :cases group))))
             (elapsed-ms (* 1000.0 (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))
             (pass (+ (reduce #'+ sub-results :key #'first :initial-value 0)
                      (reduce #'+ case-results :key #'first :initial-value 0)))
             (fail (+ (reduce #'+ sub-results :key #'second :initial-value 0)
                      (reduce #'+ case-results :key #'second :initial-value 0)))
             (skip (+ (reduce #'+ sub-results :key #'third :initial-value 0)
                      (reduce #'+ case-results :key #'third :initial-value 0)))
             (sub-benchmarks (remove nil (mapcar #'fourth sub-results)))
             (benchmark (list (cons :path bench-path)
                              (cons :passed pass)
                              (cons :failed fail)
                              (cons :skipped skip)
                              (cons :total-time-ms (round elapsed-ms))
                              (cons :subgroups sub-benchmarks))))
        (list pass fail skip benchmark))
    (error (e)
      (let* ((gname (cdr (assoc :name group)))
             (full (if (string= path "") gname (format nil "~A > ~A" path gname))))
        (format t "GROUP FAIL: ~A~%" full)
        (format t "  EXCEPTION: ~A~%" e)
        (list 0 1 0 nil)))))
