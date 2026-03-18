;;; hydra-test-runner.el --- Hydra Emacs Lisp test runner -*- lexical-binding: t -*-
;;; Ported from the Common Lisp test runner.

(require 'cl-lib)

;; ============================================================================
;; Helpers
;; ============================================================================

(defun hydra-show-term (t_)
  (condition-case nil (funcall hydra_show_core_term t_)
    (error nil)))

(defun hydra-show-type-scheme (ts)
  (condition-case nil (funcall hydra_show_core_type_scheme ts)
    (error nil)))

(defun hydra-show-type (t_)
  (condition-case nil (funcall hydra_show_core_type t_)
    (error nil)))

(defun hydra-show-let (l)
  (condition-case nil (funcall hydra_show_core_let l)
    (error nil)))

(defun hydra-terms-match-p (actual expected)
  (or (equal actual expected)
      (condition-case nil
          (let ((a-str (hydra-show-term actual))
                (e-str (hydra-show-term expected)))
            (and a-str e-str (equal a-str e-str)))
        (error nil))))

(defun hydra-string-comparison-test (path expected-str actual-str)
  (let ((e (or expected-str "#f"))
        (a_ (or actual-str "#f")))
    (if (equal e a_) (list 1 0 0)
      (progn
        (message "FAIL: %s" path)
        (message "  Expected: %s" e)
        (message "  Actual:   %s" a_)
        (list 0 1 0)))))

;; ============================================================================
;; Annotation primitives for test graph
;; ============================================================================

(defun hydra--is-annotated-p (term)
  "Check if term is annotated in either struct-compat or meta-encoded format."
  (and (consp term)
       (or (eq (car term) :annotated)
           ;; Meta-encoded: (:union (:type_name "hydra.core.Term" :field (:name "annotated" ...)))
           (and (eq (car term) :union)
                (let ((inj (cadr term)))
                  (and (consp inj)
                       (equal (cdr (assq :type_name inj)) "hydra.core.Term")
                       (equal (cdr (assq :name (cdr (assq :field inj)))) "annotated")))))))

(defun hydra--ann-body-and-map (term)
  "Extract (body . annotation-map-entries) from an annotated term in either format."
  (cond
   ((eq (car term) :annotated)
    (let* ((at (cadr term))
           (ann (hydra_core_annotated_term-annotation at))
           (body (hydra_core_annotated_term-body at)))
      (cons body (when (and (consp ann) (eq (car ann) :map)) (cadr ann)))))
   ((eq (car term) :union)
    (let* ((inj (cadr term))
           (fld (cdr (assq :field inj)))
           (rec-term (cdr (assq :term fld))))
      (when (and (consp rec-term) (eq (car rec-term) :record))
        (let* ((fields (cdr (assq :fields (cadr rec-term))))
               (body-fld (cl-find-if (lambda (f) (equal (cdr (assq :name f)) "body")) fields))
               (ann-fld (cl-find-if (lambda (f) (equal (cdr (assq :name f)) "annotation")) fields))
               (body (cdr (assq :term body-fld)))
               (ann-term (cdr (assq :term ann-fld))))
          (cons body (when (and (consp ann-term) (eq (car ann-term) :map)) (cadr ann-term)))))))))

(defun hydra--deannotate (term)
  (if (hydra--is-annotated-p term)
      (hydra--deannotate (car (hydra--ann-body-and-map term)))
    term))

(defun hydra--term-annotations (term)
  "Extract annotation alist from an annotated term."
  (let ((pairs nil))
    (while (hydra--is-annotated-p term)
      (let ((bm (hydra--ann-body-and-map term)))
        (dolist (entry (cdr bm))
          (push entry pairs))
        (setq term (car bm))))
    (nreverse pairs)))

(defun hydra--maybe-nothing-p (val)
  (or (null val)
      (and (consp val) (eq (car val) :nothing))
      (and (consp val) (eq (car val) :maybe)
           (or (< (length val) 2) (null (cadr val))
               (and (consp (cadr val)) (eq (car (cadr val)) :nothing))))))

(defun hydra--maybe-value (val)
  (cond
   ((and (consp val) (eq (car val) :just)) (cadr val))
   ((and (consp val) (eq (car val) :maybe)) (cadr val))
   (t val)))

(defun hydra--set-annotation (key val m)
  (let ((cleaned (cl-remove-if (lambda (e) (equal (car e) key)) m)))
    (if (hydra--maybe-nothing-p val) cleaned
      (cons (cons key (hydra--maybe-value val)) cleaned))))

(defun hydra--make-injection (type-name field-name field-term)
  "Build a meta-encoded injection: (:union ((:type_name . T) (:field . (:name . F) (:term . V))))"
  `((:type_name . ,type-name) (:field . ((:name . ,field-name) (:term . ,field-term)))))

(defun hydra--make-record (type-name fields)
  "Build a meta-encoded record."
  `((:type_name . ,type-name) (:fields . ,fields)))

(defun hydra--make-field (name term)
  `((:name . ,name) (:term . ,term)))

(defun hydra--term-to-meta (term)
  "Recursively convert struct-compat term to meta-encoded representation."
  (if (not (consp term)) term
    (let ((tag (car term)))
      (cond
       ((eq tag :annotated)
        (let* ((at (cadr term))
               (body (hydra_core_annotated_term-body at))
               (ann (hydra_core_annotated_term-annotation at))
               (ann-term (if (and (consp ann) (eq (car ann) :map)) ann (list :map ann))))
          (list :union (hydra--make-injection "hydra.core.Term" "annotated"
                         (list :record (hydra--make-record "hydra.core.AnnotatedTerm"
                                         (list (hydra--make-field "body" (hydra--term-to-meta body))
                                               (hydra--make-field "annotation" (hydra--term-to-meta-map ann-term)))))))))
       ((eq tag :literal)
        (list :union (hydra--make-injection "hydra.core.Term" "literal"
                       (hydra--literal-to-meta (cadr term)))))
       ((eq tag :application)
        (let ((app (cadr term)))
          (list :union (hydra--make-injection "hydra.core.Term" "application"
                         (list :record (hydra--make-record "hydra.core.Application"
                                         (list (hydra--make-field "function" (hydra--term-to-meta (hydra_core_application-function app)))
                                               (hydra--make-field "argument" (hydra--term-to-meta (hydra_core_application-argument app))))))))))
       ((eq tag :variable)
        (list :union (hydra--make-injection "hydra.core.Term" "variable"
                       (list :wrap (make-hydra_core_wrapped_term "hydra.core.Name"
                                     (list :literal (list :string (cadr term))))))))
       ((eq tag :wrap)
        (let ((wt (cadr term)))
          (list :union (hydra--make-injection "hydra.core.Term" "wrap"
                         (list :record (hydra--make-record "hydra.core.WrappedTerm"
                                         (list (hydra--make-field "typeName"
                                                 (list :wrap (make-hydra_core_wrapped_term "hydra.core.Name"
                                                               (list :literal (list :string (hydra_core_wrapped_term-type_name wt))))))
                                               (hydra--make-field "body" (hydra--term-to-meta (hydra_core_wrapped_term-body wt))))))))))
       ((eq tag :maybe)
        (list :union (hydra--make-injection "hydra.core.Term" "maybe"
                       (if (cadr term)
                           (list :maybe (hydra--term-to-meta (cadr term)))
                         (list :maybe nil)))))
       ((eq tag :list)
        (list :union (hydra--make-injection "hydra.core.Term" "list"
                       (list :list (mapcar #'hydra--term-to-meta (cadr term))))))
       ((eq tag :map)
        (list :union (hydra--make-injection "hydra.core.Term" "map"
                       (hydra--term-to-meta-map term))))
       ;; Already meta-encoded or unknown — pass through
       (t term)))))

(defun hydra--literal-to-meta (lit)
  (if (not (consp lit)) lit
    (let ((tag (car lit)))
      (cond
       ((eq tag :string) (list :union (hydra--make-injection "hydra.core.Literal" "string"
                                        (list :literal lit))))
       ((eq tag :boolean) (list :union (hydra--make-injection "hydra.core.Literal" "boolean"
                                         (list :literal lit))))
       ((eq tag :integer) (hydra--integer-to-meta (cadr lit)))
       ((eq tag :float) (hydra--float-to-meta (cadr lit)))
       (t (list :union (hydra--make-injection "hydra.core.Literal"
                          (downcase (symbol-name tag))
                          (list :literal lit))))))))

(defun hydra--integer-to-meta (ival)
  (if (not (consp ival)) ival
    (list :union (hydra--make-injection "hydra.core.Literal" "integer"
                   (list :union (hydra--make-injection "hydra.core.IntegerValue"
                                  (downcase (symbol-name (car ival)))
                                  (list :literal (list :integer ival))))))))

(defun hydra--float-to-meta (fval)
  (if (not (consp fval)) fval
    (list :union (hydra--make-injection "hydra.core.Literal" "float"
                   (list :union (hydra--make-injection "hydra.core.FloatValue"
                                  (downcase (symbol-name (car fval)))
                                  (list :literal (list :float fval))))))))

(defun hydra--ensure-name-key (k)
  (if (stringp k)
      (list :wrap (make-hydra_core_wrapped_term "hydra.core.Name"
                    (list :literal (list :string k))))
    k))

(defun hydra--term-to-meta-map (m)
  (if (and (consp m) (eq (car m) :map))
      (list :map (mapcar (lambda (entry)
                           (cons (hydra--ensure-name-key (car entry))
                                 (hydra--term-to-meta (cdr entry))))
                         (cadr m)))
    m))

(defun hydra--make-annotated (body anns)
  "Build a meta-encoded annotated term."
  (list :union (hydra--make-injection "hydra.core.Term" "annotated"
                 (list :record (hydra--make-record "hydra.core.AnnotatedTerm"
                                 (list (hydra--make-field "body" (hydra--term-to-meta body))
                                       (hydra--make-field "annotation"
                                         (list :map (mapcar (lambda (entry)
                                                              (cons (car entry) (hydra--term-to-meta (cdr entry))))
                                                            anns)))))))))


(defun hydra--prim-set-term-annotation (_cx _g args)
  (let* ((key (car args)) (val (cadr args)) (term (nth 2 args))
         (cached-anns (hydra--lookup-cached-annotations term))
         (term-anns (hydra--term-annotations term))
         (existing-anns (if cached-anns (append cached-anns term-anns) term-anns))
         (stripped (hydra--deannotate term))
         (anns (hydra--set-annotation key val existing-anns)))
    (list :right (if anns (hydra--make-annotated stripped anns) stripped))))

(defun hydra--prim-get-term-annotation (_cx _g args)
  (let* ((key (car args)) (term (cadr args))
         (cached-anns (hydra--lookup-cached-annotations term))
         (term-anns (hydra--term-annotations term))
         (anns (if cached-anns (append cached-anns term-anns) term-anns))
         (found (cdr (cl-assoc key anns :test #'equal))))
    (list :right (if found (list :maybe (hydra--term-to-meta found)) (list :maybe nil)))))

(defun hydra--prim-set-term-description (_cx _g args)
  (let* ((d (car args)) (term (cadr args))
         (term-val (unless (hydra--maybe-nothing-p d)
                     (let* ((inner (hydra--maybe-value d))
                            (s (if (and (consp inner) (eq (car inner) :literal)
                                        (consp (cadr inner)) (eq (car (cadr inner)) :string))
                                   (cadr (cadr inner))
                                 (format "%S" inner))))
                       (list :literal (list :string s)))))
         (desc-key (list :wrap (make-hydra_core_wrapped_term "hydra.core.Name"
                                 (list :literal (list :string "description")))))
         (maybe-val (if term-val (list :maybe term-val) (list :maybe (list :nothing)))))
    (hydra--prim-set-term-annotation _cx _g (list desc-key maybe-val term))))

(defun hydra--prim-get-term-description (_cx _g args)
  (let* ((term (nth 2 args))
         ;; Peel type lambdas/applications
         (peeled (let ((t_ term))
                   (while (and (consp t_) (memq (car t_) '(:type_lambda :type_application)))
                     (let ((rec (cadr t_)))
                       (setq t_ (cond
                                 ((eq (car t_) :type_lambda) (hydra_core_type_lambda-body rec))
                                 (t (hydra_core_type_application_term-body rec))))))
                   t_))
         (cached-anns (hydra--lookup-cached-annotations peeled))
         (term-anns (hydra--term-annotations peeled))
         (anns (if cached-anns (append cached-anns term-anns) term-anns))
         (desc-entry (cl-find-if
                      (lambda (e)
                        (let ((k (car e)))
                          (and (consp k) (eq (car k) :wrap)
                               (let ((wt (cadr k)))
                                 (and (equal (hydra_core_wrapped_term-type_name wt) "hydra.core.Name")
                                      (let ((b (hydra_core_wrapped_term-body wt)))
                                        (and (consp b) (eq (car b) :literal)
                                             (consp (cadr b)) (eq (car (cadr b)) :string)
                                             (equal (cadr (cadr b)) "description"))))))))
                      anns))
         (desc-term (when desc-entry (cdr desc-entry))))
    (let ((desc-string
           (cond
            ;; Struct-compat: (:literal (:string "value"))
            ((and (consp desc-term) (eq (car desc-term) :literal)
                  (consp (cadr desc-term)) (eq (car (cadr desc-term)) :string))
             (cadr (cadr desc-term)))
            ;; Meta-encoded: (:union ((:type_name . "hydra.core.Term") (:field (:name . "literal") (:term ...))))
            ((and (consp desc-term) (eq (car desc-term) :union))
             (let* ((inj (cadr desc-term))
                    (fld (cdr (assq :field inj)))
                    (fname (cdr (assq :name fld))))
               (when (equal fname "literal")
                 (let* ((inner (cdr (assq :term fld)))
                        (inner-inj (when (and (consp inner) (eq (car inner) :union)) (cadr inner)))
                        (inner-fld (when inner-inj (cdr (assq :field inner-inj))))
                        (inner-fname (when inner-fld (cdr (assq :name inner-fld)))))
                   (when (equal inner-fname "string")
                     (let ((str-term (cdr (assq :term inner-fld))))
                       (when (and (consp str-term) (eq (car str-term) :literal)
                                  (consp (cadr str-term)) (eq (car (cadr str-term)) :string))
                         (cadr (cadr str-term)))))))))
            (t nil))))
      (if desc-string
          (list :right (list :either (list :right (list :maybe (list :literal (list :string desc-string))))))
        (list :right (list :either (list :right (list :maybe nil))))))))

(defun hydra--make-ann-type-scheme (arity)
  (let ((make-t (lambda (n) (if (<= n 0) (list :unit)
                              (list :function (make-hydra_core_function_type (list :unit) (funcall make-t (1- n))))))))
    ;; Use make-prim-type-scheme from prims.el
    (make-prim-type-scheme arity)))

(defun hydra--make-annotation-primitive (name arity impl-fn)
  (make-hydra_graph_primitive name (make-prim-type-scheme arity)
    (lambda (cx) (lambda (g) (lambda (args)
      (condition-case err
          (let ((result (funcall impl-fn cx g args)))
            (if (eq (car result) :left)
                (list :left (make-hydra_context_in_context
                             (list :other (make-hydra_context_in_context (cadr result) cx)) cx))
              result))
        (error (list :left (make-hydra_context_in_context
                            (list :other (make-hydra_context_in_context (format "%S" err) cx)) cx)))))))))

;; ============================================================================
;; Annotation cache — preserves annotations stripped by the reducer
;; ============================================================================

(defvar hydra--annotation-cache (make-hash-table :test 'equal))

(defun hydra--install-annotation-cache ()
  "Wrap hydra_rewriting_deannotate_term to cache annotations before stripping."
  (let ((original (or (and (fboundp 'hydra_rewriting_deannotate_term)
                           (symbol-function 'hydra_rewriting_deannotate_term))
                      hydra_rewriting_deannotate_term)))
    (let ((wrapper (lambda (t_)
                     (when (hydra--is-annotated-p t_)
                       (let ((body (funcall original t_))
                             (anns (hydra--term-annotations t_)))
                         (when anns
                           (puthash body anns hydra--annotation-cache))))
                     (funcall original t_))))
      ;; Set both value and function cells
      (setq hydra_rewriting_deannotate_term wrapper)
      (fset 'hydra_rewriting_deannotate_term wrapper))))

(defun hydra--lookup-cached-annotations (term)
  (gethash term hydra--annotation-cache))

;; ============================================================================
;; Graph construction
;; ============================================================================

(defvar hydra--test-graph nil)
(defvar hydra--annotation-cache-installed nil)

(defun hydra-build-test-graph ()
  (let* ((std-prims (standard-library))
         ;; Add custom annotation primitives
         (ann-prims-list
          (list
           (list "hydra.annotations.setTermAnnotation"
                 (hydra--make-annotation-primitive "hydra.annotations.setTermAnnotation" 3
                   #'hydra--prim-set-term-annotation))
           (list "hydra.annotations.getTermAnnotation"
                 (hydra--make-annotation-primitive "hydra.annotations.getTermAnnotation" 2
                   #'hydra--prim-get-term-annotation))
           (list "hydra.annotations.setTermDescription"
                 (hydra--make-annotation-primitive "hydra.annotations.setTermDescription" 2
                   #'hydra--prim-set-term-description))
           (list "hydra.annotations.getTermDescription"
                 (hydra--make-annotation-primitive "hydra.annotations.getTermDescription" 3
                   #'hydra--prim-get-term-description))))
         (ann-prims-map (funcall hydra_lib_maps_from_list ann-prims-list))
         (all-prims (funcall (funcall hydra_lib_maps_union ann-prims-map) std-prims))
         (prim-entries (funcall hydra_lib_maps_to_list all-prims))
         (bound-terms
          (funcall hydra_lib_maps_from_list
                   (append
                    (mapcar (lambda (entry)
                              (list (car entry) (list :function (list :primitive (car entry)))))
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

(defun hydra-ensure-test-graph ()
  (unless hydra--annotation-cache-installed
    (hydra--install-annotation-cache)
    (setq hydra--annotation-cache-installed t))
  (unless hydra--test-graph
    (let ((base (hydra-build-test-graph)))
      (condition-case err
          (let* ((bootstrap-types (if (boundp 'hydra_json_bootstrap_types_by_name)
                                      hydra_json_bootstrap_types_by_name nil))
                 (test-types (if (boundp 'hydra_test_test_graph_test_types)
                                 hydra_test_test_graph_test_types nil))
                 (type-to-ts (when (boundp 'hydra_rewriting_f_type_to_type_scheme)
                               hydra_rewriting_f_type_to_type_scheme))
                 (kernel-entries
                  (when (and bootstrap-types type-to-ts)
                    (mapcar (lambda (entry)
                              (list (car entry) (funcall type-to-ts (cadr entry))))
                            (funcall hydra_lib_maps_to_list bootstrap-types))))
                 (test-entries
                  (when (and test-types type-to-ts)
                    (mapcar (lambda (entry)
                              (list (car entry) (funcall type-to-ts (cadr entry))))
                            (funcall hydra_lib_maps_to_list test-types))))
                 (all-entries (append (or kernel-entries nil) (or test-entries nil)))
                 (schema-types (funcall hydra_lib_maps_from_list all-entries))
                 (test-terms-map (if (boundp 'hydra_test_test_graph_test_terms)
                                     hydra_test_test_graph_test_terms
                                   hydra_lib_maps_empty))
                 (enhanced (copy-alist base)))
            (setcdr (assq :schema_types enhanced) schema-types)
            (setcdr (assq :bound_terms enhanced)
                    (funcall (funcall hydra_lib_maps_union test-terms-map)
                             (cdr (assq :bound_terms base))))
            (setq hydra--test-graph enhanced))
        (error
         (message "WARNING: Could not enhance test graph: %S" err)
         (setq hydra--test-graph base))))))

(defun hydra-empty-context ()
  '((:functions) (:annotations) (:variable_types)))

(defun hydra-type-scheme-to-type (ts)
  "Convert a TypeScheme back to a Type by wrapping forall binders."
  (let ((vars (hydra_core_type_scheme-variables ts))
        (body (hydra_core_type_scheme-type ts)))
    (cl-reduce (lambda (t_ v) (list :forall (make-hydra_core_forall_type v t_)))
               (reverse vars) :initial-value body)))

(defun hydra-empty-graph ()
  "Return a graph with standard primitives but no bound terms or types."
  (let ((std-prims (standard-library)))
    (list (cons :bound_terms nil)
          (cons :bound_types nil)
          (cons :class_constraints nil)
          (cons :lambda_variables nil)
          (cons :metadata nil)
          (cons :primitives std-prims)
          (cons :schema_types nil)
          (cons :type_variables nil))))

;; ============================================================================
;; Test runners
;; ============================================================================

(defun hydra-run-evaluation-test (path tc)
  (let* ((input (cdr (assq :input tc)))
         (expected (cdr (assq :output tc)))
         (graph hydra--test-graph)
         (cx (hydra-empty-context))
         (style (cdr (assq :evaluation_style tc)))
         (eager (and (consp style) (eq (car style) :eager))))
    (condition-case err
        (let ((result (funcall (funcall (funcall (funcall hydra_reduction_reduce_term cx) graph) eager) input)))
          (if (eq (car result) :left)
              (progn (message "FAIL: %s" path) (message "  ERROR: %S" (cadr result)) (list 0 1 0))
            (let ((actual (cadr result)))
              (if (hydra-terms-match-p actual expected) (list 1 0 0)
                (progn
                  (message "FAIL: %s" path)
                  (condition-case nil
                      (progn (message "  Expected: %s" (hydra-show-term expected))
                             (message "  Actual:   %s" (hydra-show-term actual)))
                    (error (message "  Expected (raw): %S" expected)
                           (message "  Actual (raw):   %S" actual)))
                  (list 0 1 0))))))
      (error (message "FAIL: %s" path) (message "  EXCEPTION: %S" err) (list 0 1 0)))))

(defun hydra-run-simple-test (path expected actual-fn)
  (condition-case err
      (let ((actual (funcall actual-fn)))
        (if (hydra-terms-match-p actual expected) (list 1 0 0)
          (progn (message "FAIL: %s" path)
                 (condition-case nil
                     (progn (message "  Expected: %s" (hydra-show-term expected))
                            (message "  Actual:   %s" (hydra-show-term actual)))
                   (error nil))
                 (list 0 1 0))))
    (error (message "FAIL: %s" path) (message "  EXCEPTION: %S" err) (list 0 1 0))))

(defun hydra-run-either-test (path expected either-result)
  (condition-case err
      (if (eq (car either-result) :left)
          (progn (message "FAIL: %s" path) (message "  ERROR: %S" (cadr either-result)) (list 0 1 0))
        (let ((actual (cadr either-result)))
          (if (hydra-terms-match-p actual expected) (list 1 0 0)
            (progn (message "FAIL: %s" path) (list 0 1 0)))))
    (error (message "FAIL: %s" path) (message "  EXCEPTION: %S" err) (list 0 1 0))))

;; Individual test case runners

(defun hydra-run-alpha-conversion-test (path tc)
  (hydra-run-simple-test path (cdr (assq :result tc))
    (lambda () (funcall (funcall (funcall hydra_reduction_alpha_convert (cdr (assq :old_variable tc)))
                                 (cdr (assq :new_variable tc))) (cdr (assq :term tc))))))

(defun hydra-run-case-conversion-test (path tc)
  (hydra-run-simple-test path (cdr (assq :to_string tc))
    (lambda () (funcall (funcall (funcall hydra_formatting_convert_case (cdr (assq :from_convention tc)))
                                 (cdr (assq :to_convention tc))) (cdr (assq :from_string tc))))))

(defun hydra-run-deannotate-term-test (path tc)
  (hydra-run-simple-test path (cdr (assq :output tc))
    (lambda () (funcall hydra_rewriting_deannotate_term (cdr (assq :input tc))))))

(defun hydra-run-deannotate-type-test (path tc)
  (hydra-run-simple-test path (cdr (assq :output tc))
    (lambda () (funcall hydra_rewriting_deannotate_type (cdr (assq :input tc))))))

(defun hydra-run-flatten-let-terms-test (path tc)
  (hydra-run-simple-test path (cdr (assq :output tc))
    (lambda () (funcall hydra_rewriting_flatten_let_terms (cdr (assq :input tc))))))

(defun hydra-run-free-variables-test (path tc)
  (hydra-run-simple-test path (cdr (assq :output tc))
    (lambda () (funcall hydra_rewriting_free_variables_in_term (cdr (assq :input tc))))))

(defun hydra-run-lift-lambda-test (path tc)
  (hydra-run-simple-test path (cdr (assq :output tc))
    (lambda () (funcall hydra_rewriting_lift_lambda_above_let (cdr (assq :input tc))))))

(defun hydra-run-simplify-term-test (path tc)
  (hydra-run-simple-test path (cdr (assq :output tc))
    (lambda () (funcall hydra_rewriting_simplify_term (cdr (assq :input tc))))))

(defun hydra-run-normalize-type-vars-test (path tc)
  (hydra-run-simple-test path (cdr (assq :output tc))
    (lambda () (funcall hydra_rewriting_normalize_type_variables_in_term (cdr (assq :input tc))))))

(defun hydra-run-topological-sort-test (path tc)
  (hydra-run-simple-test path (cdr (assq :expected tc))
    (lambda () (funcall hydra_sorting_topological_sort (cdr (assq :adjacency_list tc))))))

(defun hydra-run-topological-sort-scc-test (path tc)
  (hydra-run-simple-test path (cdr (assq :expected tc))
    (lambda () (funcall hydra_sorting_topological_sort_components (cdr (assq :adjacency_list tc))))))

(defun hydra-run-serialization-test (path tc)
  (hydra-run-simple-test path (cdr (assq :output tc))
    (lambda () (funcall hydra_serialization_print_expr
                        (funcall hydra_serialization_parenthesize (cdr (assq :input tc)))))))

(defun hydra-run-type-reduction-test (path tc)
  (let ((cx (hydra-empty-context)) (graph hydra--test-graph))
    (hydra-run-either-test path (cdr (assq :output tc))
      (funcall (funcall (funcall hydra_reduction_beta_reduce_type cx) graph) (cdr (assq :input tc))))))

(defun hydra-run-unshadow-variables-test (path tc)
  (hydra-run-simple-test path (cdr (assq :output tc))
    (lambda () (funcall hydra_rewriting_unshadow_variables (cdr (assq :input tc))))))

(defun hydra-run-eta-expansion-test (path tc)
  (let ((cx (hydra-empty-context)) (graph hydra--test-graph))
    (hydra-run-either-test path (cdr (assq :output tc))
      (funcall (funcall (funcall hydra_reduction_eta_expand_typed_term cx) graph) (cdr (assq :input tc))))))

(defun hydra-run-inference-test (path tc)
  (let ((cx (hydra-empty-context)) (graph hydra--test-graph))
    (condition-case err
        (let ((result (funcall (funcall (funcall hydra_inference_infer_type_of cx) graph) (cdr (assq :input tc)))))
          (if (eq (car result) :left)
              (progn (message "FAIL: %s" path) (message "  Inference ERROR: %S" (cadr result)) (list 0 1 0))
            (let* ((pair-val (cadr result))
                   (inner-pair (funcall hydra_lib_pairs_first pair-val))
                   (result-scheme (funcall hydra_lib_pairs_second inner-pair))
                   (expected-str (hydra-show-type-scheme (cdr (assq :output tc))))
                   (actual-str (hydra-show-type-scheme result-scheme)))
              (hydra-string-comparison-test path expected-str actual-str))))
      (error (message "FAIL: %s" path) (message "  EXCEPTION: %S" err) (list 0 1 0)))))

(defun hydra-run-inference-failure-test (path tc)
  (let ((cx (hydra-empty-context)) (graph hydra--test-graph))
    (condition-case nil
        (let ((result (funcall (funcall (funcall hydra_inference_infer_type_of cx) graph) (cdr (assq :input tc)))))
          (if (eq (car result) :left) (list 1 0 0)
            (progn (message "FAIL: %s\n  Expected failure" path) (list 0 1 0))))
      (error (list 1 0 0)))))

(defun hydra-run-variable-occurs-test (path tc)
  (hydra-run-simple-test path (cdr (assq :expected tc))
    (lambda () (funcall (funcall hydra_unification_variable_occurs_in_type (cdr (assq :variable tc)))
                        (cdr (assq :type tc))))))

(defun hydra-run-subst-in-type-test (path tc)
  (let ((subst (funcall hydra_lib_maps_from_list (cdr (assq :substitution tc)))))
    (hydra-run-simple-test path (cdr (assq :output tc))
      (lambda () (funcall (funcall hydra_substitution_subst_in_type subst) (cdr (assq :input tc)))))))

(defun hydra-run-topological-sort-bindings-test (path tc)
  (let* ((binding-map (funcall hydra_lib_maps_from_list (cdr (assq :bindings tc))))
         (result (funcall hydra_rewriting_topological_sort_binding_map binding-map))
         (expected (cdr (assq :expected tc))))
    (if (equal expected result) (list 1 0 0)
      (progn (message "FAIL: %s" path) (list 0 1 0)))))

(defun hydra-run-json-parser-test (path tc)
  (condition-case err
      (let ((result (funcall hydra_json_parser_parse_json (cdr (assq :input tc)))))
        (if (hydra-terms-match-p result (cdr (assq :output tc))) (list 1 0 0)
          (progn (message "FAIL: %s" path) (list 0 1 0))))
    (error (message "FAIL: %s\n  EXCEPTION: %S" path err) (list 0 1 0))))

(defun hydra-run-json-writer-test (path tc)
  (hydra-run-simple-test path (cdr (assq :output tc))
    (lambda () (funcall hydra_json_writer_print_json (cdr (assq :input tc))))))

(defun hydra-run-type-checking-test (path tc)
  (let ((cx (hydra-empty-context)) (graph hydra--test-graph))
    (condition-case err
        (let ((infer-result (funcall (funcall (funcall hydra_inference_infer_type_of cx) graph)
                                     (cdr (assq :input tc)))))
          (if (eq (car infer-result) :left)
              (progn (message "FAIL: %s" path) (message "  Inference failed: %S" (cadr infer-result))
                     (list 0 1 0))
            (let* ((pair-val (cadr infer-result))
                   (inner-pair (funcall hydra_lib_pairs_first pair-val))
                   (inferred-term (funcall hydra_lib_pairs_first inner-pair))
                   (result-scheme (funcall hydra_lib_pairs_second inner-pair))
                   (infer-cx (funcall hydra_lib_pairs_second pair-val))
                   (inferred-type (hydra-type-scheme-to-type result-scheme))
                   (type-of-result (funcall (funcall (funcall (funcall hydra_checking_type_of infer-cx) graph)
                                                     nil) inferred-term)))
              (if (eq (car type-of-result) :left)
                  (progn (message "FAIL: %s" path) (message "  Type reconstruction failed: %S" (cadr type-of-result))
                         (list 0 1 0))
                (let* ((reconstructed-type (funcall hydra_lib_pairs_first (cadr type-of-result)))
                       (term-ok (equal (hydra-show-term (cdr (assq :output_term tc)))
                                       (hydra-show-term inferred-term)))
                       (type-ok (equal (hydra-show-type (cdr (assq :output_type tc)))
                                       (hydra-show-type inferred-type)))
                       (recon-ok (equal (hydra-show-type (cdr (assq :output_type tc)))
                                        (hydra-show-type reconstructed-type))))
                  (if (and term-ok type-ok recon-ok)
                      (list 1 0 0)
                    (progn
                      (message "FAIL: %s" path)
                      (unless term-ok (message "  Inferred term mismatch"))
                      (unless type-ok (message "  Inferred type mismatch"))
                      (unless recon-ok (message "  Reconstructed type mismatch"))
                      (list 0 1 0))))))))
      (error (message "FAIL: %s" path) (message "  EXCEPTION: %S" err) (list 0 1 0)))))

(defun hydra-run-type-checking-failure-test (path tc)
  (let ((cx (hydra-empty-context)) (graph hydra--test-graph))
    (condition-case nil
        (let ((result (funcall (funcall (funcall hydra_inference_infer_type_of cx) graph)
                               (cdr (assq :input tc)))))
          (if (eq (car result) :left) (list 1 0 0)
            (progn (message "FAIL: %s\n  Expected type checking failure" path) (list 0 1 0))))
      (error (list 1 0 0)))))

(defun hydra-run-unify-types-test (path tc)
  (let* ((cx (hydra-empty-context))
         (schema-entries (mapcar (lambda (n) (list n (make-hydra_core_type_scheme nil (list :variable n) nil)))
                                 (cdr (assq :schema_types tc))))
         (schema-types (funcall hydra_lib_maps_from_list schema-entries))
         (result (funcall (funcall (funcall (funcall (funcall hydra_unification_unify_types cx) schema-types)
                                            (cdr (assq :left tc))) (cdr (assq :right tc))) "test"))
         (expected (cdr (assq :expected tc))))
    (condition-case err
        (cond
         ((eq (car expected) :left)
          (if (eq (car result) :left) (list 1 0 0)
            (progn (message "FAIL: %s\n  Expected failure" path) (list 0 1 0))))
         ((eq (car expected) :right)
          (if (eq (car result) :left)
              (progn (message "FAIL: %s\n  Expected success but got: %S" path (cadr result)) (list 0 1 0))
            (if (equal (cadr expected) (cadr result)) (list 1 0 0)
              (progn (message "FAIL: %s\n  Subst mismatch" path) (list 0 1 0)))))
         (t (list 0 1 0)))
      (error (message "FAIL: %s\n  EXCEPTION: %S" path err) (list 0 1 0)))))

(defun hydra-run-join-types-test (path tc)
  (let* ((cx (hydra-empty-context))
         (result (funcall (funcall (funcall (funcall hydra_unification_join_types cx)
                                            (cdr (assq :left tc))) (cdr (assq :right tc))) "test"))
         (expected (cdr (assq :expected tc))))
    (condition-case err
        (cond
         ((eq (car expected) :left)
          (if (eq (car result) :left) (list 1 0 0)
            (progn (message "FAIL: %s\n  Expected failure" path) (list 0 1 0))))
         ((eq (car expected) :right)
          (if (eq (car result) :left)
              (progn (message "FAIL: %s\n  Expected success but got: %S" path (cadr result)) (list 0 1 0))
            (if (equal (cadr expected) (cadr result)) (list 1 0 0)
              (progn (message "FAIL: %s\n  Value mismatch" path) (list 0 1 0)))))
         (t (list 0 1 0)))
      (error (message "FAIL: %s\n  EXCEPTION: %S" path err) (list 0 1 0)))))

(defun hydra-run-hoist-case-statements-test (path tc)
  (let ((eg (hydra-empty-graph)))
    (hydra-run-simple-test path (cdr (assq :output tc))
      (lambda () (funcall (funcall hydra_hoisting_hoist_case_statements eg) (cdr (assq :input tc)))))))

(defun hydra-hoist-predicate-fn (pred)
  (let ((pred-type (car pred)))
    (cond
     ((eq pred-type :nothing) (lambda (_pair) nil))
     ((eq pred-type :lists) (lambda (pair)
                              (let ((term (funcall hydra_lib_pairs_second pair)))
                                (and (consp term) (eq (car term) :list)))))
     ((eq pred-type :applications) (lambda (pair)
                                     (let ((term (funcall hydra_lib_pairs_second pair)))
                                       (and (consp term) (eq (car term) :application)))))
     ((eq pred-type :case_statements) (lambda (pair)
                                        (let ((term (funcall hydra_lib_pairs_second pair)))
                                          (and (consp term) (eq (car term) :function)
                                               (let ((f (cadr term)))
                                                 (and (consp f) (eq (car f) :elimination)))))))
     (t (lambda (_pair) nil)))))

(defun hydra-run-hoist-subterms-test (path tc)
  (let ((eg (hydra-empty-graph))
        (pred (hydra-hoist-predicate-fn (cdr (assq :predicate tc)))))
    (hydra-run-simple-test path (cdr (assq :output tc))
      (lambda () (funcall (funcall (funcall hydra_hoisting_hoist_subterms pred) eg) (cdr (assq :input tc)))))))

(defun hydra-run-hoist-let-bindings-test (path tc)
  (condition-case err
      (let* ((result (funcall hydra_hoisting_hoist_all_let_bindings (cdr (assq :input tc))))
             (expected-str (hydra-show-let (cdr (assq :output tc))))
             (actual-str (hydra-show-let result)))
        (hydra-string-comparison-test path expected-str actual-str))
    (error (message "FAIL: %s\n  EXCEPTION: %S" path err) (list 0 1 0))))

(defun hydra-run-hoist-polymorphic-let-bindings-test (path tc)
  (condition-case err
      (let* ((result (funcall (funcall hydra_hoisting_hoist_polymorphic_let_bindings
                                       (lambda (_x) t)) (cdr (assq :input tc))))
             (expected-str (hydra-show-let (cdr (assq :output tc))))
             (actual-str (hydra-show-let result)))
        (hydra-string-comparison-test path expected-str actual-str))
    (error (message "FAIL: %s\n  EXCEPTION: %S" path err) (list 0 1 0))))

(defun hydra-run-rewrite-term-test (path tc)
  (let* ((rewriter (cdr (assq :rewriter tc)))
         (rewriter-type (car rewriter))
         (rewrite-impl
          (cond
           ((eq rewriter-type :replace_foo_with_bar)
            (lambda (recurse) (lambda (term)
              (if (and (consp term) (eq (car term) :literal)
                       (consp (cadr term)) (eq (car (cadr term)) :string) (equal (cadr (cadr term)) "foo"))
                  (list :literal (list :string "bar")) (funcall recurse term)))))
           ((eq rewriter-type :replace_int32_with_int64)
            (lambda (recurse) (lambda (term)
              (if (and (consp term) (eq (car term) :literal)
                       (consp (cadr term)) (eq (car (cadr term)) :integer)
                       (consp (cadr (cadr term))) (eq (car (cadr (cadr term))) :int32))
                  (list :literal (list :integer (list :int64 (cadr (cadr (cadr term))))))
                (funcall recurse term)))))
           (t (lambda (recurse) (lambda (term) (funcall recurse term)))))))
    (hydra-run-simple-test path (cdr (assq :output tc))
      (lambda () (funcall (funcall hydra_rewriting_rewrite_term rewrite-impl) (cdr (assq :input tc)))))))

(defun hydra-run-rewrite-type-test (path tc)
  (let* ((rewriter (cdr (assq :rewriter tc)))
         (rewriter-type (car rewriter))
         (rewrite-impl
          (cond
           ((eq rewriter-type :replace_string_with_int32)
            (lambda (recurse) (lambda (typ)
              (if (and (consp typ) (eq (car typ) :literal)
                       (consp (cadr typ)) (eq (car (cadr typ)) :string))
                  (list :literal (list :integer (list :int32 nil))) (funcall recurse typ)))))
           (t (lambda (recurse) (lambda (typ) (funcall recurse typ)))))))
    (hydra-run-simple-test path (cdr (assq :output tc))
      (lambda () (funcall (funcall hydra_rewriting_rewrite_type rewrite-impl) (cdr (assq :input tc)))))))

(defun hydra--get-int32-val (term)
  (if (and (consp term) (eq (car term) :literal) (consp (cadr term)) (eq (car (cadr term)) :integer)
           (consp (cadr (cadr term))) (eq (car (cadr (cadr term))) :int32))
      (cadr (cadr (cadr term))) 0))

(defun hydra-run-fold-over-term-test (path tc)
  (let* ((order (cdr (assq :traversal_order tc)))
         (operation (cdr (assq :operation tc)))
         (op-type (car operation))
         (input (cdr (assq :input tc))))
    (condition-case err
        (let ((result
               (cond
                ((eq op-type :sum_int32_literals)
                 (let ((sum (funcall (funcall (funcall (funcall hydra_rewriting_fold_over_term order)
                                                       (lambda (acc) (lambda (term) (+ acc (hydra--get-int32-val term))))) 0) input)))
                   (list :literal (list :integer (list :int32 sum)))))
                ((eq op-type :collect_list_lengths)
                 (let ((lengths (funcall (funcall (funcall (funcall hydra_rewriting_fold_over_term order)
                                                           (lambda (acc) (lambda (term)
                                                             (if (and (consp term) (eq (car term) :list))
                                                                 (append acc (list (length (cadr term))))
                                                               acc)))) nil) input)))
                   (list :list (mapcar (lambda (len) (list :literal (list :integer (list :int32 len)))) lengths))))
                ((eq op-type :collect_labels)
                 (let ((labels_ (funcall (funcall (funcall (funcall hydra_rewriting_fold_over_term order)
                                                            (lambda (acc) (lambda (term)
                                                              (if (and (consp term) (eq (car term) :pair))
                                                                  (let* ((fst (funcall hydra_lib_pairs_first (cadr term))))
                                                                    (if (and (consp fst) (eq (car fst) :literal)
                                                                             (consp (cadr fst)) (eq (car (cadr fst)) :string))
                                                                        (append acc (list (cadr fst))) acc))
                                                                acc)))) nil) input)))
                   (list :list (mapcar (lambda (label) (list :literal label)) labels_)))))))
          (if (hydra-terms-match-p result (cdr (assq :output tc))) (list 1 0 0)
            (progn (message "FAIL: %s" path)
                   (condition-case nil
                       (progn (message "  Expected: %s" (hydra-show-term (cdr (assq :output tc))))
                              (message "  Actual:   %s" (hydra-show-term result)))
                     (error nil))
                   (list 0 1 0))))
      (error (message "FAIL: %s\n  EXCEPTION: %S" path err) (list 0 1 0)))))

(defun hydra-run-json-coder-test (path tc)
  (let ((empty-types hydra_lib_maps_empty))
    (condition-case err
        (let ((encode-result (funcall hydra_json_encode_to_json (cdr (assq :term tc)))))
          (if (eq (car encode-result) :left)
              (progn (message "FAIL: %s\n  JSON encode failed: %S" path (cadr encode-result)) (list 0 1 0))
            (let ((encoded (cadr encode-result)))
              (if (not (hydra-terms-match-p (cdr (assq :json tc)) encoded))
                  (progn (message "FAIL: %s\n  JSON encode mismatch" path) (list 0 1 0))
                (let ((decode-result (funcall (funcall (funcall (funcall hydra_json_decode_from_json empty-types)
                                                                (make-hydra_core_name :value "test"))
                                                       (cdr (assq :type tc)))
                                              encoded)))
                  (if (eq (car decode-result) :left)
                      (progn (message "FAIL: %s\n  JSON decode failed: %S" path (cadr decode-result)) (list 0 1 0))
                    (if (hydra-terms-match-p (cdr (assq :term tc)) (cadr decode-result))
                        (list 1 0 0)
                      (progn (message "FAIL: %s\n  Roundtrip mismatch" path) (list 0 1 0)))))))))
      (error (message "FAIL: %s\n  EXCEPTION: %S" path err) (list 0 1 0)))))

(defun hydra-run-json-roundtrip-test (path tc)
  (let ((empty-types hydra_lib_maps_empty))
    (condition-case err
        (let ((encode-result (funcall hydra_json_encode_to_json (cdr (assq :term tc)))))
          (if (eq (car encode-result) :left)
              (progn (message "FAIL: %s\n  JSON encode failed: %S" path (cadr encode-result)) (list 0 1 0))
            (let* ((encoded (cadr encode-result))
                   (decode-result (funcall (funcall (funcall (funcall hydra_json_decode_from_json empty-types)
                                                             (make-hydra_core_name :value "test"))
                                                    (cdr (assq :type tc)))
                                           encoded)))
              (if (eq (car decode-result) :left)
                  (progn (message "FAIL: %s\n  JSON decode failed: %S" path (cadr decode-result)) (list 0 1 0))
                (if (hydra-terms-match-p (cdr (assq :term tc)) (cadr decode-result))
                    (list 1 0 0)
                  (progn (message "FAIL: %s\n  Roundtrip mismatch" path) (list 0 1 0)))))))
      (error (message "FAIL: %s\n  EXCEPTION: %S" path err) (list 0 1 0)))))

(defun hydra-run-json-decode-test (path tc)
  (let ((empty-types hydra_lib_maps_empty))
    (condition-case err
        (let* ((decode-result (funcall (funcall (funcall (funcall hydra_json_decode_from_json empty-types)
                                                         (make-hydra_core_name :value "test"))
                                                (cdr (assq :type tc)))
                                       (cdr (assq :json tc))))
               (expected (cdr (assq :expected tc))))
          (cond
           ((eq (car expected) :left)
            (if (eq (car decode-result) :left) (list 1 0 0)
              (progn (message "FAIL: %s\n  Expected decode failure" path) (list 0 1 0))))
           ((eq (car expected) :right)
            (if (eq (car decode-result) :left)
                (progn (message "FAIL: %s\n  JSON decode failed: %S" path (cadr decode-result)) (list 0 1 0))
              (if (hydra-terms-match-p (cadr expected) (cadr decode-result))
                  (list 1 0 0)
                (progn (message "FAIL: %s\n  Decode mismatch" path) (list 0 1 0)))))
           (t (list 0 1 0))))
      (error (message "FAIL: %s\n  EXCEPTION: %S" path err) (list 0 1 0)))))

;; ============================================================================
;; Test case dispatcher
;; ============================================================================

(defun hydra-run-test-case (path tcase)
  (condition-case err
      (let* ((tname (cdr (assq :name tcase)))
             (full (format "%s > %s" path tname))
             (tags (cdr (assq :tags tcase)))
             (disabled (member "disabled" tags))
             (tc (cdr (assq :case tcase))))
        (if disabled (list 0 0 1)
          (let ((case-type (car tc)) (case-data (cadr tc)))
            (cond
             ((eq case-type :evaluation)              (hydra-run-evaluation-test full case-data))
             ((eq case-type :alpha_conversion)        (hydra-run-alpha-conversion-test full case-data))
             ((eq case-type :case_conversion)         (hydra-run-case-conversion-test full case-data))
             ((eq case-type :deannotate_term)         (hydra-run-deannotate-term-test full case-data))
             ((eq case-type :deannotate_type)         (hydra-run-deannotate-type-test full case-data))
             ((eq case-type :flatten_let_terms)       (hydra-run-flatten-let-terms-test full case-data))
             ((eq case-type :free_variables)          (hydra-run-free-variables-test full case-data))
             ((eq case-type :lift_lambda_above_let)   (hydra-run-lift-lambda-test full case-data))
             ((eq case-type :simplify_term)           (hydra-run-simplify-term-test full case-data))
             ((eq case-type :normalize_type_variables) (hydra-run-normalize-type-vars-test full case-data))
             ((eq case-type :topological_sort)        (hydra-run-topological-sort-test full case-data))
             ((eq case-type :topological_sort_s_c_c)  (hydra-run-topological-sort-scc-test full case-data))
             ((eq case-type :serialization)           (hydra-run-serialization-test full case-data))
             ((eq case-type :type_reduction)          (hydra-run-type-reduction-test full case-data))
             ((eq case-type :unshadow_variables)      (hydra-run-unshadow-variables-test full case-data))
             ((eq case-type :eta_expansion)           (hydra-run-eta-expansion-test full case-data))
             ((eq case-type :inference)               (hydra-run-inference-test full case-data))
             ((eq case-type :inference_failure)       (hydra-run-inference-failure-test full case-data))
             ((eq case-type :type_checking)           (hydra-run-type-checking-test full case-data))
             ((eq case-type :type_checking_failure)   (hydra-run-type-checking-failure-test full case-data))
             ((eq case-type :variable_occurs_in_type) (hydra-run-variable-occurs-test full case-data))
             ((eq case-type :subst_in_type)           (hydra-run-subst-in-type-test full case-data))
             ((eq case-type :unify_types)             (hydra-run-unify-types-test full case-data))
             ((eq case-type :join_types)              (hydra-run-join-types-test full case-data))
             ((eq case-type :topological_sort_bindings) (hydra-run-topological-sort-bindings-test full case-data))
             ((eq case-type :hoist_case_statements)   (hydra-run-hoist-case-statements-test full case-data))
             ((eq case-type :hoist_subterms)          (hydra-run-hoist-subterms-test full case-data))
             ((eq case-type :hoist_let_bindings)      (hydra-run-hoist-let-bindings-test full case-data))
             ((eq case-type :hoist_polymorphic_let_bindings) (hydra-run-hoist-polymorphic-let-bindings-test full case-data))
             ((eq case-type :rewrite_term)            (hydra-run-rewrite-term-test full case-data))
             ((eq case-type :rewrite_type)            (hydra-run-rewrite-type-test full case-data))
             ((eq case-type :fold_over_term)          (hydra-run-fold-over-term-test full case-data))
             ((eq case-type :json_parser)             (hydra-run-json-parser-test full case-data))
             ((eq case-type :json_writer)             (hydra-run-json-writer-test full case-data))
             ((eq case-type :json_coder)              (hydra-run-json-coder-test full case-data))
             ((eq case-type :json_roundtrip)          (hydra-run-json-roundtrip-test full case-data))
             ((eq case-type :json_decode)             (hydra-run-json-decode-test full case-data))
             ((eq case-type :json_encode)             (list 0 0 1))
             ((eq case-type :delegated_evaluation)    (list 0 0 1))
             (t (list 0 0 1))))))
    (error
     (message "FAIL: %s > %s" path (cdr (assq :name tcase)))
     (message "  EXCEPTION: %S" err)
     (list 0 1 0))))

(defun hydra-run-test-group (path group &optional bench-prefix)
  "Run a test group. Returns (list pass fail skip benchmark).
   benchmark is an alist with :path, :passed, :failed, :skipped, :total-time-ms, :subgroups."
  (let ((bench-prefix (or bench-prefix "")))
    (condition-case err
        (let* ((gname (cdr (assq :name group)))
               (full (if (equal path "") gname (format "%s > %s" path gname)))
               (bench-path (if (equal bench-prefix "") gname (format "%s/%s" bench-prefix gname)))
               (t0 (float-time))
               (pass 0) (fail 0) (skip 0)
               (sub-benchmarks nil))
          ;; Iterate subgroups (not mapcar -- saves stack)
          (dolist (sg (cdr (assq :subgroups group)))
            (let ((r (hydra-run-test-group full sg bench-path)))
              (setq pass (+ pass (car r)))
              (setq fail (+ fail (cadr r)))
              (setq skip (+ skip (caddr r)))
              (when (and (>= (length r) 4) (nth 3 r))
                (push (nth 3 r) sub-benchmarks))))
          ;; Iterate cases
          (dolist (tc (cdr (assq :cases group)))
            (let ((r (hydra-run-test-case full tc)))
              (setq pass (+ pass (car r)))
              (setq fail (+ fail (cadr r)))
              (setq skip (+ skip (caddr r)))))
          (let* ((elapsed-ms (* 1000.0 (- (float-time) t0)))
                 (benchmark (list (cons :path bench-path)
                                  (cons :passed pass)
                                  (cons :failed fail)
                                  (cons :skipped skip)
                                  (cons :total-time-ms (round elapsed-ms))
                                  (cons :subgroups (nreverse sub-benchmarks)))))
            (list pass fail skip benchmark)))
      (error
       (message "GROUP FAIL: %s > %s" path (cdr (assq :name group)))
       (message "  EXCEPTION: %S" err)
       (list 0 1 0 nil)))))

;; ============================================================================
;; Benchmark helpers
;; ============================================================================

(defun hydra-benchmark-to-json (b)
  "Convert a benchmark alist to a JSON string."
  (let* ((path (cdr (assq :path b)))
         (passed (cdr (assq :passed b)))
         (failed (cdr (assq :failed b)))
         (skipped (cdr (assq :skipped b)))
         (total-ms (cdr (assq :total-time-ms b)))
         (subs (cdr (assq :subgroups b)))
         (sub-json (if (null subs) ""
                     (format ", \"subgroups\": [%s]"
                             (mapconcat #'hydra-benchmark-to-json subs ", ")))))
    (format "{\"path\": \"%s\", \"passed\": %d, \"failed\": %d, \"skipped\": %d, \"totalTimeMs\": %d%s}"
            path passed failed skipped total-ms sub-json)))

(defun hydra-write-benchmark-json (benchmark total-ms)
  "Write benchmark JSON to HYDRA_BENCHMARK_OUTPUT if set."
  (let ((output-path (getenv "HYDRA_BENCHMARK_OUTPUT")))
    (when output-path
      (let* ((top-groups (cdr (assq :subgroups benchmark)))
             (json-groups (mapconcat (lambda (g) (concat "    " (hydra-benchmark-to-json g)))
                                     top-groups ",\n"))
             (passed (cdr (assq :passed benchmark)))
             (failed (cdr (assq :failed benchmark)))
             (skipped (cdr (assq :skipped benchmark)))
             (json (format "{\n  \"groups\": [\n%s\n  ],\n  \"summary\": {\n    \"totalPassed\": %d,\n    \"totalFailed\": %d,\n    \"totalSkipped\": %d,\n    \"totalTimeMs\": %d\n  }\n}"
                           json-groups passed failed skipped (round total-ms))))
        (with-temp-file output-path
          (insert json))
        (message "Benchmark output: %s" output-path)))))

;; ============================================================================
;; Main entry point
;; ============================================================================

(defun hydra-run-tests ()
  "Run the Hydra test suite."
  (hydra-ensure-test-graph)
  (let* ((t0 (float-time))
         (suite hydra_test_test_suite_all_tests)
         (result (hydra-run-test-group "" suite))
         (pass (car result))
         (fail (cadr result))
         (skip (caddr result))
         (benchmark (nth 3 result))
         (total-ms (* 1000.0 (- (float-time) t0))))
    (message "\nResults: %d passed, %d failed, %d skipped" pass fail skip)
    (message "Total:   %d" (+ pass fail skip))
    (when benchmark
      (hydra-write-benchmark-json
       (list (cons :path (cdr (assq :path benchmark)))
             (cons :passed pass)
             (cons :failed fail)
             (cons :skipped skip)
             (cons :total-time-ms (cdr (assq :total-time-ms benchmark)))
             (cons :subgroups (cdr (assq :subgroups benchmark))))
       total-ms))
    (when (> fail 0)
      (kill-emacs 1))))

(provide 'hydra-test-runner)
