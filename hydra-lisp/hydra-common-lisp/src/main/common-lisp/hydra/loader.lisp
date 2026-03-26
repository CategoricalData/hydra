;;; Hydra Common Lisp loader
;;; Loads generated files with compatibility transformations.
(in-package :cl-user)

;; Capture paths at load time.
;; Loader is at src/main/common-lisp/hydra/loader.lisp
;; Gen-main is at src/gen-main/common-lisp/hydra/
(defvar *hydra-loader-dir*
  (make-pathname :directory (pathname-directory (truename *load-truename*))
                 :defaults (truename *load-truename*)))

(defvar *hydra-gen-main-dir*
  (let* ((loader-dir (pathname-directory (truename *load-truename*)))
         ;; Loader dir ends with .../src/main/common-lisp/hydra
         ;; Replace the 3rd-from-last component ("main") with "gen-main"
         (prefix (butlast loader-dir 3))
         (suffix (last loader-dir 2)))
    (make-pathname :directory (append prefix (list "gen-main") suffix))))

;; Load the alist-based struct compatibility layer
(load (merge-pathnames "struct-compat.lisp" *hydra-loader-dir*))

;; Dynamic hydra-defstruct: creates alist-based constructor and accessors
;; at eval time for any struct not pre-defined in struct-compat.lisp
(defmacro hydra-defstruct (name &rest fields)
  "Define an alist-based struct with constructor and field accessors."
  (let ((constructor (intern (format nil "MAKE-~A" name)))
        (predicate (intern (format nil "~A?" name)))
        (field-keywords (mapcar (lambda (f) (intern (string f) :keyword)) fields)))
    `(progn
       (unless (fboundp ',constructor)
         (defun ,constructor (&rest args)
           (if (and args (keywordp (first args)))
               (loop for (k v) on args by #'cddr collect (cons k v))
               (mapcar #'cons ',field-keywords args)))
         (defun ,predicate (x) (and (listp x) (assoc ,(first field-keywords) x)))
         ,@(mapcar (lambda (field kw)
                     `(defun ,(intern (format nil "~A-~A" name field)) (rec)
                        (cdr (assoc ,kw rec))))
                   fields field-keywords)))))

;; Load hand-written native library implementations (before gen-main)
(dolist (f '("lib/chars.lisp" "lib/eithers.lisp" "lib/equality.lisp"
             "lib/lists.lisp" "lib/literals.lisp" "lib/logic.lisp"
             "lib/maps.lisp" "lib/math.lisp" "lib/maybes.lisp"
             "lib/pairs.lisp" "lib/regex.lisp" "lib/sets.lisp" "lib/strings.lisp"))
  (load (merge-pathnames f *hydra-loader-dir*)))

;; Note: prims.lisp and lib/libraries.lisp must be loaded AFTER gen-main
;; because prims depends on extract/core and libraries depends on prims.
;; Call (hydra-load-prims-and-libraries) after (hydra-load-gen-main).
(defun hydra-load-prims-and-libraries ()
  "Load prims and libraries (must be called after gen-main)."
  (load (merge-pathnames "prims.lisp" *hydra-loader-dir*))
  (load (merge-pathnames "lib/libraries.lisp" *hydra-loader-dir*)))

(defun hydra-set-function-bindings ()
  "For every HYDRA_* variable that holds a function and is not already fbound,
   set the symbol's function cell so it can be called in function position.
   This bridges the gap between CL's separate function/value namespaces and
   the generated code's Scheme-style calling conventions."
  (do-symbols (sym (find-package :cl-user))
    (let ((name (symbol-name sym)))
      (when (and (> (length name) 6)
                 (string= "HYDRA_" (subseq name 0 6))
                 (boundp sym)
                 (functionp (symbol-value sym))
                 (not (fboundp sym)))
        (setf (symbol-function sym) (symbol-value sym))))))

(defun hydra-fix-curried-calls (form &optional lambda-vars)
  "Transform Scheme/Lisp-1 calling conventions to CL Lisp-2 conventions.
   1. ((expr ...) args...) -> (funcall (expr ...) args...) for non-lambda exprs
   2. (local-var args...) -> (funcall local-var args...) when local-var is lambda-bound
   LAMBDA-VARS is a list of symbols that are lambda-bound in the current scope."
  (cond
    ((atom form) form)
    ;; Quote: unquote alists that contain forms needing evaluation
    ;; '(("key" . (make-foo ...)) ...) → (list (cons "key" (make-foo ...)) ...)
    ((eq (car form) 'quote)
     (let ((data (second form)))
       (if (and (consp data)
                (consp (first data))
                ;; Check if any entry has a cons that needs evaluation
                (some (lambda (entry)
                        (and (consp entry)
                             (or (and (consp (car entry)) (symbolp (caar entry)))
                                 (and (consp (cdr entry)) (symbolp (cadr entry))
                                      (not (keywordp (cadr entry)))))))
                      data))
           ;; Transform '((key . val) ...) to (list (cons key' val') ...)
           (cons 'list
                 (mapcar (lambda (entry)
                           (if (consp entry)
                               (list 'cons
                                     (hydra-fix-curried-calls (car entry) lambda-vars)
                                     (hydra-fix-curried-calls (cdr entry) lambda-vars))
                               entry))
                         data))
           form)))
    ;; Special form: cond - clauses are (test body...) pairs, not function calls
    ((eq (car form) 'cond)
     (cons 'cond
           (mapcar (lambda (clause)
                     (if (consp clause)
                       (mapcar (lambda (f) (hydra-fix-curried-calls f lambda-vars)) clause)
                       clause))
                   (cdr form))))
    ;; Lambda forms: track bound parameters
    ((or (eq (car form) 'cl:lambda) (eq (car form) 'lambda))
     (let* ((params (second form))
            (new-vars (append (if (listp params) params nil) lambda-vars)))
       (list* (car form) params
              (mapcar (lambda (f) (hydra-fix-curried-calls f new-vars))
                      (cddr form)))))
    ;; let/let*: track bound names as lambda-vars (for Lisp-1 → Lisp-2 conversion)
    ((member (car form) '(let let*) :test #'eq)
     (let* ((bindings (second form))
            (body (cddr form))
            ;; Extract binding names
            (names (mapcar (lambda (b) (if (consp b) (first b) b)) bindings))
            (new-vars (append names lambda-vars))
            ;; Transform binding values (with existing lambda-vars for let, accumulated for let*)
            (transformed-bindings
             (if (eq (car form) 'let)
                 (mapcar (lambda (b)
                           (if (consp b)
                               (list (first b) (hydra-fix-curried-calls (second b) lambda-vars))
                               b))
                         bindings)
                 ;; let*: accumulate bindings
                 (let ((acc-vars lambda-vars)
                       (result nil))
                   (dolist (b bindings (nreverse result))
                     (if (consp b)
                         (progn
                           (push (list (first b) (hydra-fix-curried-calls (second b) acc-vars)) result)
                           (push (first b) acc-vars))
                         (push b result))))))
            (transformed-body
             (mapcar (lambda (f) (hydra-fix-curried-calls f new-vars)) body)))
       (list* (car form) transformed-bindings transformed-body)))
    ;; Letrec fix: ((lambda (X) BODY) INIT) where INIT references X.
    ;; This is a recursive/self-referential binding (letrec in Scheme).
    ;; Transform to use a mutable cell for the fixpoint:
    ;; (let ((X-cell (list nil)))
    ;;   (let ((X (lambda (&rest args) (apply (car X-cell) args))))
    ;;     (setf (car X-cell) INIT')
    ;;     BODY'))
    ((and (consp (car form))
          (or (eq (caar form) 'cl:lambda) (eq (caar form) 'lambda))
          (= (length (cdr form)) 1)  ;; exactly one argument
          (let ((params (cadar form)))
            (and (listp params) (= (length params) 1)))
          (let* ((params (cadar form))
                 (param (car params))
                 (init (cadr form)))
            ;; Detect letrec (self-referential binding) patterns:
            ;; 1. Init is a lambda containing free ref to param (recursive function def)
            ;; 2. Body starts with (param ...) — Y-combinator pattern
            ;; Non-lambda inits that just shadow an outer variable are NOT letrec.
            (let ((body-forms (cddar form)))
              (or
                ;; Pattern 1: init is a lambda with free reference to param
                (and (consp init)
                     (or (eq (car init) 'cl:lambda) (eq (car init) 'lambda))
                     (labels ((has-free-ref (f sym)
                                (cond
                                  ((eq f sym) t)
                                  ((atom f) nil)
                                  ((eq (car f) 'quote) nil)
                                  ((and (or (eq (car f) 'cl:lambda) (eq (car f) 'lambda))
                                        (listp (cadr f)))
                                   (if (member sym (cadr f) :test #'eq)
                                     nil
                                     (some (lambda (sub) (has-free-ref sub sym)) (cddr f))))
                                  (t (or (has-free-ref (car f) sym)
                                         (has-free-ref (cdr f) sym))))))
                       (has-free-ref init param)))
                ;; Pattern 2: body is (param ...) — Y-combinator fixpoint
                (and (= (length body-forms) 1)
                     (consp (car body-forms))
                     (eq (caar body-forms) param))))))
     (let* ((param (car (cadar form)))
            (body-forms (cddar form))
            (init (cadr form))
            (cell-sym (intern (format nil "~A-CELL" param)))
            (new-vars (cons param lambda-vars)))
       `(let ((,cell-sym (list nil)))
          (let ((,param (lambda (&rest args) (apply (car ,cell-sym) args))))
            (setf (car ,cell-sym)
                  ,(hydra-fix-curried-calls init new-vars))
            ,@(mapcar (lambda (f) (hydra-fix-curried-calls f new-vars))
                      body-forms)))))
    ;; Immediately-invoked lambda: ((cl:lambda (params) body) args...)
    ;; Track params as lambda-bound when recursing into body
    ((and (consp (car form))
          (or (eq (caar form) 'cl:lambda) (eq (caar form) 'lambda))
          (cdr form))  ; has arguments (is an application, not just a bare lambda)
     (let* ((lam (car form))
            (params (second lam))
            (new-vars (append (if (listp params) params nil) lambda-vars))
            (transformed-body (mapcar (lambda (f) (hydra-fix-curried-calls f new-vars))
                                      (cddr lam)))
            (transformed-args (mapcar (lambda (f) (hydra-fix-curried-calls f lambda-vars))
                                      (cdr form))))
       (cons (list* (car lam) params transformed-body) transformed-args)))
    ;; if_else: (((HYDRA_LIB_LOGIC_IF_ELSE cond) then) else) -> (if cond then else)
    ;; Must be before compound-expression case. Converts eager-evaluated curried if_else
    ;; into CL's short-circuiting (if ...) special form to prevent infinite recursion.
    ((and (consp (car form))
          (consp (caar form))
          (= (length (cdr form)) 1)   ;; else arg
          (= (length (cdar form)) 1)  ;; then arg
          (consp (caar form))
          (let ((innermost (caaar form)))
            (and (symbolp innermost)
                 (eq innermost 'hydra_lib_logic_if_else))))
     (let ((cond-form (hydra-fix-curried-calls (cadar (car form)) lambda-vars))
           (then-form (hydra-fix-curried-calls (cadr (car form)) lambda-vars))
           (else-form (hydra-fix-curried-calls (cadr form) lambda-vars)))
       (list 'if cond-form then-form else-form)))
    ;; If the car is a non-lambda compound expression, wrap with funcall
    ((and (consp (car form))
          (not (eq (caar form) 'cl:lambda))
          (not (eq (caar form) 'lambda)))
     (let ((inner (hydra-fix-curried-calls (car form) lambda-vars))
           (args (mapcar (lambda (f) (hydra-fix-curried-calls f lambda-vars)) (cdr form))))
       (cons 'funcall (cons inner args))))
    ;; If the car is a lambda-bound symbol called in function position, wrap with funcall
    ((and (symbolp (car form))
          (member (car form) lambda-vars :test #'eq)
          (cdr form))  ; has arguments (not just a reference)
     (cons 'funcall
           (cons (car form)
                 (mapcar (lambda (f) (hydra-fix-curried-calls f lambda-vars)) (cdr form)))))
    ;; labels: transform bindings and body
    ;; labels-defined names are in the function namespace (NOT lambda-vars)
    ;; so (name args) is a direct function call, no funcall needed
    ((eq (car form) 'labels)
     (let* ((bindings (second form))
            (body (cddr form))
            (transformed-bindings
             (mapcar (lambda (b)
                       (let* ((name (first b))
                              (params (second b))
                              (fn-body (cddr b))
                              ;; params are lambda-bound within the function body
                              (inner-vars (append params lambda-vars)))
                         (declare (ignore name))
                         (list* (first b) params
                                (mapcar (lambda (f) (hydra-fix-curried-calls f inner-vars))
                                        fn-body))))
                     bindings))
            (transformed-body
             (mapcar (lambda (f) (hydra-fix-curried-calls f lambda-vars)) body)))
       (list* 'labels transformed-bindings transformed-body)))
    ;; letrec: transform to labels or mutable cells
    ((eq (car form) 'letrec)
     (let ((transformed (hydra-transform-letrec form)))
       (if transformed
           (hydra-fix-curried-calls transformed lambda-vars)
           (cons 'letrec
                 (mapcar (lambda (f) (hydra-fix-curried-calls f lambda-vars)) (cdr form))))))
    ;; Otherwise recurse into subforms
    (t (cons (if (consp (car form))
                 (hydra-fix-curried-calls (car form) lambda-vars)
                 (car form))
             (mapcar (lambda (f) (hydra-fix-curried-calls f lambda-vars)) (cdr form))))))

(defun hydra-skip-form-p (form)
  "Return T if this form should be skipped during loading."
  (and (consp form)
       (or (member (first form) '(defpackage in-package) :test #'eq)
           (keywordp (first form)))))

(defun hydra-transform-letrec (form)
  "Transform (letrec ((name init) ...) body...) to CL-compatible form.
   Uses let + mutable cells (setf) to implement Scheme's letrec semantics.
   We cannot use CL's labels because Hydra-generated code passes recursive
   function names as values (to higher-order functions), which requires
   Lisp-1 semantics (single namespace)."
  (when (and (consp form) (eq (first form) 'letrec))
    (let* ((bindings (second form))
           (body (cddr form))
           (cell-names (mapcar (lambda (b) (intern (format nil "~A-CELL" (first b)))) bindings)))
      `(let ,(mapcar (lambda (cn) `(,cn (list nil))) cell-names)
         (let ,(mapcar (lambda (b cn)
                         `(,(first b) (lambda (&rest args) (apply (car ,cn) args))))
                       bindings cell-names)
           ,@(mapcar (lambda (b cn)
                       `(setf (car ,cn) ,(hydra-fix-curried-calls (second b)
                                            (mapcar #'first bindings))))
                     bindings cell-names)
           ,@(mapcar (lambda (f) (hydra-fix-curried-calls f (mapcar #'first bindings)))
                     body))))))

(defun hydra-transform-form (form)
  "Transform a form before evaluation. Converts cl:defstruct to hydra-defstruct,
   letrec to labels/mutable-cell pattern."
  (cond
    ;; cl:defstruct -> hydra-defstruct
    ((and (consp form)
          (symbolp (first form))
          (string= (symbol-name (first form)) "DEFSTRUCT")
          (eq (symbol-package (first form)) (find-package :cl)))
     (cons 'hydra-defstruct (cdr form)))
    ;; letrec -> labels (for CL compatibility)
    ((and (consp form) (eq (first form) 'letrec))
     (hydra-transform-letrec (hydra-fix-curried-calls form)))
    ;; Default: apply curried call fixes
    (t (let ((fixed (hydra-fix-curried-calls form)))
         ;; Check if the fixed form contains letrec (nested)
         (if (and (consp fixed) (eq (first fixed) 'letrec))
             (hydra-transform-letrec fixed)
             fixed)))))

(defun hydra-load-file (path)
  "Load a generated Lisp file, skipping invalid directives.
   Uses multi-pass approach to handle forward references: tries each form,
   then retries failed ones up to 3 times (handles _NAME constants defined after
   the types that use them, and other ordering dependencies).
   Suppresses style-warnings from generated code (unused variables, etc.)."
  (handler-bind ((style-warning #'muffle-warning))
  (with-open-file (in path :direction :input)
    (let ((*package* (find-package :cl-user))
          (*readtable* (copy-readtable))
          (*read-default-float-format* 'double-float)
          (failed-forms nil))
      ;; Read all forms
      (handler-case
        (loop
          (let ((form (read in nil :eof)))
            (when (eq form :eof) (return))
            (unless (hydra-skip-form-p form)
              (handler-case
                (eval (hydra-transform-form form))
                (error (e)
                  (declare (ignore e))
                  (push form failed-forms))))))
        (error (e)
          (format t "Error reading ~A: ~A~%" path e)))
      ;; Retry failed forms up to 10 times (for cascading dependencies)
      (setf failed-forms (nreverse failed-forms))
      (dotimes (pass 10)
        (when (null failed-forms) (return))
        (let ((still-failed nil))
          (dolist (form failed-forms)
            (handler-case
              (eval (hydra-transform-form form))
              (error (e)
                (declare (ignore e))
                (push form still-failed))))
          (setf failed-forms (nreverse still-failed))
          ;; Set function bindings for newly defined HYDRA_ variables
          (hydra-set-function-bindings)))))))

(defun collect-lisp-files (dir &optional prefix)
  "Collect all .lisp files from DIR recursively, returning relative paths."
  (let ((result nil))
    (dolist (entry (directory (merge-pathnames
                               (make-pathname :name :wild :type "lisp")
                               dir)))
      (let ((rel (if prefix
                     (format nil "~A/~A.lisp" prefix (pathname-name entry))
                     (format nil "~A.lisp" (pathname-name entry)))))
        (push rel result)))
    ;; Recurse into subdirectories
    (dolist (subdir (directory (merge-pathnames
                                (make-pathname :directory '(:relative :wild))
                                dir)))
      (when (and (pathnamep subdir)
                 (null (pathname-name subdir))
                 (null (pathname-type subdir)))
        (let ((subdir-name (car (last (pathname-directory subdir)))))
          (when (stringp subdir-name)
            (setf result (append result
                                 (collect-lisp-files subdir
                                                     (if prefix
                                                         (format nil "~A/~A" prefix subdir-name)
                                                         subdir-name))))))))
    result))

(defun hydra-load-gen-main ()
  "Load all generated main modules. Uses retry loop to handle forward references."
  (let* ((base *hydra-gen-main-dir*)
         ;; Priority files: load these first for best dependency order
         (priority '("core.lisp" "error/core.lisp" "error/checking.lisp" "error/module.lisp"
                     "context.lisp" "graph.lisp"
                     "module.lisp" "ast.lisp" "coders.lisp" "grammar.lisp" "phantoms.lisp"
                     "parsing.lisp" "query.lisp" "relational.lisp" "tabular.lisp" "testing.lisp"
                     "topology.lisp" "typing.lisp" "util.lisp" "variants.lisp"
                     "json/model.lisp" "classes.lisp" "constants.lisp" "accessors.lisp"
                     "formatting.lisp" "tarjan.lisp" "rewriting.lisp" "sorting.lisp"
                     "names.lisp" "schemas.lisp" "arity.lisp" "lexical.lisp"
                     "literals.lisp" "reflect.lisp" "languages.lisp" "parsers.lisp" "grammars.lisp"
                     "templates.lisp" "encoding.lisp" "decoding.lisp" "code_generation.lisp"
                     "hoisting.lisp" "show/core.lisp" "show/error/core.lisp" "show/errors.lisp"
                     "validate/core.lisp"
                     "encode/core.lisp" "encode/error/core.lisp" "encode/error/checking.lisp" "encode/errors.lisp"
                     "decode/core.lisp" "decode/error/core.lisp" "decode/error/checking.lisp" "decode/errors.lisp"
                     "extract/core.lisp" "extract/helpers.lisp" "extract/util.lisp" "extract/json.lisp"
                     "substitution.lisp" "annotations.lisp" "unification.lisp"
                     "inference.lisp" "checking.lisp" "serialization.lisp" "reduction.lisp"
                     "json/coder.lisp" "json/parser.lisp" "json/writer.lisp"
                     "json/encode.lisp" "json/decode.lisp" "json/bootstrap.lisp"))
         ;; Collect ALL .lisp files from gen-main
         (all-files (collect-lisp-files base))
         ;; Build ordered list: priority first, then remaining
         (remaining (remove-if (lambda (f) (member f priority :test #'equal)) all-files))
         (ordered (append priority (sort (copy-list remaining) #'string<))))
    (dolist (f ordered)
      (let ((path (merge-pathnames f base)))
        (when (probe-file path)
          (format t "Loading ~A...~%" f)
          (hydra-load-file path))))))

(defvar *hydra-gen-test-dir*
  (let* ((loader-dir (pathname-directory (truename *load-truename*)))
         (prefix (butlast loader-dir 3))
         (suffix (last loader-dir 2)))
    (make-pathname :directory (append prefix (list "gen-test") suffix))))

(defun hydra-load-gen-test ()
  "Load all generated test modules."
  (let* ((base *hydra-gen-test-dir*)
         (files '(
           "test/test_types.lisp"
           "test/test_terms.lisp"
           "test/test_graph.lisp"
           "test/lib/chars.lisp"
           "test/lib/eithers.lisp"
           "test/lib/equality.lisp"
           "test/lib/lists.lisp"
           "test/lib/literals.lisp"
           "test/lib/logic.lisp"
           "test/lib/maps.lisp"
           "test/lib/math.lisp"
           "test/lib/maybes.lisp"
           "test/lib/pairs.lisp"
           "test/lib/regex.lisp"
           "test/lib/sets.lisp"
           "test/lib/strings.lisp"
           "test/annotations.lisp"
           "test/checking/advanced.lisp"
           "test/checking/algebraic_types.lisp"
           "test/checking/collections.lisp"
           "test/checking/failures.lisp"
           "test/checking/fundamentals.lisp"
           "test/checking/nominal_types.lisp"
           "test/checking/all.lisp"
           "test/eta_expansion.lisp"
           "test/formatting.lisp"
           "test/hoisting/cases.lisp"
           "test/hoisting/let.lisp"
           "test/hoisting/all.lisp"
           "test/inference/algebraic_types.lisp"
           "test/inference/algorithm_w.lisp"
           "test/inference/classes.lisp"
           "test/inference/failures.lisp"
           "test/inference/fundamentals.lisp"
           "test/inference/kernel_examples.lisp"
           "test/inference/nominal_types.lisp"
           "test/inference/all.lisp"
           "test/json/coder.lisp"
           "test/json/parser.lisp"
           "test/json/roundtrip.lisp"
           "test/json/writer.lisp"
           "test/reduction.lisp"
           "test/rewriting.lisp"
           "test/serialization.lisp"
           "test/sorting.lisp"
           "test/substitution.lisp"
           "test/unification.lisp"
           "test/validate/core.lisp"
           "test/validate/all.lisp"
           "test/test_suite.lisp")))
    (dolist (f files)
      (let ((path (merge-pathnames f base)))
        (when (probe-file path)
          (hydra-load-file path))))))
