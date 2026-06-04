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

;; Load lazy thunk primitives (make-lazy, lazy-force) used by the lazy-let
;; loader transformation below.
(load (merge-pathnames "lazy.lisp" *hydra-loader-dir*))

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

(defun hydra-let-rhs-trivial-p (rhs)
  "True if a let binding's RHS is so cheap that lazy-wrapping would only add
   overhead, rather than fixing exponential re-evaluation.

   Trivial RHSs include:
     - atoms (literals, symbols, numbers, strings)
     - quoted constants
     - already-lazy bindings (don't re-wrap)
     - bare lambda values (constructing a closure is cheap; the body only
       runs if the lambda is invoked, so wrapping in lazy adds an indirection
       without preventing any extra evaluation)
     - simple field accessors and constructors with all-atom args

   The exponential blowup we are guarding against comes from bindings whose
   RHS performs nontrivial computation that the body then references in
   multiple conditional branches or recursive paths. For those, lazy ensures
   single-evaluation. For bindings whose RHS is itself a lambda or a small
   constructor call, lazy adds overhead without changing semantics."
  (cond
    ((atom rhs) t)
    ((eq (car rhs) 'quote) t)
    ((eq (car rhs) 'make-lazy) t)                            ; already lazy
    ;; Lambda values: cheap to construct (no body evaluation), and lazy-
    ;; wrapping a lambda just adds an extra dereference per invocation.
    ((or (eq (car rhs) 'cl:lambda) (eq (car rhs) 'lambda)) t)
    ;; Single-level function call with all-atom args: cheap by construction.
    ;; E.g. `(make-foo a b c)`, `(funcall f x)`, `(car cell)`.
    ((and (symbolp (car rhs))
          (every (lambda (a) (or (atom a)
                                 (and (consp a) (eq (car a) 'quote))))
                 (cdr rhs)))
     t)
    (t nil)))

(defun hydra-let-binding-already-lazy-p (b)
  "True if a binding looks like an already-wrapped lazy thunk binding (so
   the let transformer should not re-wrap it)."
  (and (consp b)
       (symbolp (first b))
       (let ((name (symbol-name (first b))))
         (search "-LAZY-THUNK" name))))

(defun hydra-body-has-conditional-or-lambda-p (body)
  "True if BODY contains any branching form (cond/if/case) OR any lambda
   form. The exponential re-evaluation that lazy-let prevents arises when
   a binding is referenced from inside multiple conditional branches that
   each might re-trigger evaluation through recursion. Lambdas qualify
   because they may be invoked from arbitrary call sites (including from
   inside conditionals defined elsewhere). If the body has neither
   conditionals nor lambdas, lazy-wrapping is pure overhead."
  (labels ((scan (f)
             (cond
               ((atom f) nil)
               ((eq (car f) 'quote) nil)
               ((member (car f) '(if cond case cl:if cl:cond cl:case
                                  cl:lambda lambda)
                        :test #'eq)
                t)
               (t (or (scan (car f)) (scan (cdr f)))))))
    (some #'scan body)))

(defvar *hydra-lazy-debug-log* nil
  "If non-nil, a stream to write the wrapped form to (for debugging).")

(defun hydra-wrap-let-lazy (let-form bindings body)
  "Rewrite (LET-FORM BINDINGS BODY...) so that each non-trivial binding RHS
   is wrapped in (make-lazy (lambda () RHS)) and the body sees the names
   through a symbol-macrolet that calls (lazy-force NAME-thunk).
   Trivial bindings flow through unchanged.

   LET-FORM is either 'let or 'let* (preserved in the output for trivial
   bindings only; lazy bindings emit explicit nested lets to preserve
   evaluation order for let*).

   Implementation strategy:
   - Partition bindings into (trivial, lazy) pairs.
   - Build the output: a single let/let* for trivial bindings (matching
     original semantics), then for each lazy binding a (let ((name-thunk ...))
     (symbol-macrolet ((name (lazy-force name-thunk))) ...)) nest."
  (cond
    ;; No bindings or all trivial / already-lazy: emit unchanged.
    ((or (null bindings)
         (every (lambda (b) (or (not (consp b))
                                (hydra-let-rhs-trivial-p (second b))
                                (hydra-let-binding-already-lazy-p b)))
                bindings))
     (list* let-form bindings body))
    ;; Body has no conditional branching and no lambdas: there is no way
    ;; for a binding to be re-evaluated, so lazy-wrapping only adds
    ;; overhead. Emit the original let unchanged.
    ((not (hydra-body-has-conditional-or-lambda-p body))
     (list* let-form bindings body))
    (t
     ;; Walk bindings; emit lazy nests around non-trivial ones.
     ;; For correctness with `let` (parallel binding), we conceptually want
     ;; each thunk to see the OUTER scope. Easiest: emit each binding as its
     ;; own nested let-thunk pair. This converts a parallel let to a sequence,
     ;; but since the thunks defer evaluation and never observe each other's
     ;; in-scope name (the symbol-macrolet only kicks in for the body), the
     ;; observed semantics are unchanged for kernel-emitted let forms.
     (labels ((wrap (remaining)
                (if (null remaining)
                    (cons 'cl:progn body)
                    (let* ((b (car remaining))
                           (rest (cdr remaining)))
                      (cond
                        ((not (consp b))
                         ;; Body-only binding (no value), pass through.
                         (list 'let (list b) (wrap rest)))
                        ((or (hydra-let-rhs-trivial-p (second b))
                             (hydra-let-binding-already-lazy-p b))
                         ;; Trivial RHS or already-lazy: regular let.
                         (list 'let (list b) (wrap rest)))
                        (t
                         ;; Lazy-wrap this binding. The symbol-macrolet
                         ;; expansion inlines the already-forced fast path:
                         ;;   if the cell's car is :lazy-value, return its
                         ;;   cdr directly; else call lazy-force.
                         ;; This avoids a function call on every reference
                         ;; after the first.
                         (let* ((name (first b))
                                (rhs (second b))
                                (thunk-sym (intern (concatenate 'string
                                                    (symbol-name name) "-LAZY-THUNK"))))
                           `(let ((,thunk-sym (make-lazy (lambda () ,rhs))))
                              (symbol-macrolet
                                  ((,name (if (eq (car ,thunk-sym) :lazy-value)
                                              (cdr ,thunk-sym)
                                              (lazy-force ,thunk-sym))))
                                ,(wrap rest))))))))))
       (wrap bindings)))))

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
    ;; Special form: symbol-macrolet - binding list contains (symbol expansion)
    ;; pairs, NOT function calls. Without this, the binding list looks like a
    ;; compound expression and the fallthrough wraps it in funcall, mangling
    ;; the form. Only the BODY needs recursive transformation.
    ((eq (car form) 'symbol-macrolet)
     (let ((bindings (second form))
           (body (cddr form)))
       (list* 'symbol-macrolet
              bindings
              (mapcar (lambda (f) (hydra-fix-curried-calls f lambda-vars)) body))))
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
    ;; let/let*: track bound names as lambda-vars (for Lisp-1 → Lisp-2 conversion).
    ;;
    ;; *** Lazy-let transformation ***
    ;; Wrap each binding's RHS in (make-lazy (lambda () RHS)) and introduce a
    ;; symbol-macrolet around the body so references to the bound name expand
    ;; to (lazy-force NAME-thunk). This gives the kernel the lazy let semantics
    ;; Hydra DSL expects, preventing exponential re-evaluation of deeply nested
    ;; sub-expressions during inference. See docs/history/python-host-perf-
    ;; investigation.md for the analogous Python fix in #344.
    ;;
    ;; A binding is wrapped iff its RHS is "non-trivial" (not a literal, not a
    ;; bare variable). Trivial RHSs are emitted as a regular (let ((x V))) so
    ;; we don't pay the make-lazy / symbol-macrolet overhead for them.
    ((member (car form) '(let let*) :test #'eq)
     (let* ((bindings (second form))
            (body (cddr form))
            (names (mapcar (lambda (b) (if (consp b) (first b) b)) bindings))
            (new-vars (append names lambda-vars))
            ;; Transform binding values (with existing lambda-vars for let,
            ;; accumulated for let*).
            (transformed-bindings
             (if (eq (car form) 'let)
                 (mapcar (lambda (b)
                           (if (consp b)
                               (list (first b) (hydra-fix-curried-calls (second b) lambda-vars))
                               b))
                         bindings)
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
       (hydra-wrap-let-lazy (car form) transformed-bindings transformed-body)))
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

(defun hydra-letrec-binding-lambda-p (binding)
  "True if a letrec binding's RHS is syntactically a CL lambda. Non-lambda
   bindings (string/number/struct value computations) cannot be safely
   dispatcher-wrapped: the dispatcher would intercept value references and
   surface a function where the kernel expects the value."
  (let ((rhs (second binding)))
    (and (consp rhs)
         (or (eq (car rhs) 'cl:lambda) (eq (car rhs) 'lambda)))))

(defun hydra-transform-letrec (form)
  "Transform (letrec ((name init) ...) body...) to CL-compatible form.

   Lambda bindings are wrapped with a mutable-cell dispatcher so that recursive
   self- and mutual-references work as in Scheme letrec — even when the
   recursive function name is passed as a higher-order value, which is why we
   cannot use CL's labels (it would put recursive names in the function
   namespace only, breaking Lisp-1 value passing).

   Non-lambda bindings (e.g. string/value computations) are emitted as plain
   lexical variables that are SETQ'd in source order after the dispatcher
   bindings are in scope. This preserves Scheme letrec semantics for the
   common kernel-emitted shape (singleton self-referential lambda + a chain of
   pure value bindings, e.g. hydra/haskell/utils.lisp:326) without the
   loader silently substituting a dispatcher lambda for what should be a
   string. See issue #426."
  (when (and (consp form) (eq (first form) 'letrec))
    (let* ((bindings (second form))
           (body (cddr form))
           (names (mapcar #'first bindings))
           (lambda-bindings (remove-if-not #'hydra-letrec-binding-lambda-p bindings))
           (value-bindings (remove-if     #'hydra-letrec-binding-lambda-p bindings))
           (cell-names (mapcar (lambda (b)
                                 (intern (format nil "~A-CELL" (first b))))
                               lambda-bindings)))
      `(let ,(mapcar (lambda (cn) `(,cn (list nil))) cell-names)
         (let ,(append
                 ;; Lambda bindings: dispatcher into the mutable cell.
                 (mapcar (lambda (b cn)
                           `(,(first b)
                              (lambda (&rest args)
                                (apply (car ,cn) args))))
                         lambda-bindings cell-names)
                 ;; Value bindings: nil placeholder, mutated in source order
                 ;; once the dispatcher names are in scope.
                 (mapcar (lambda (b) `(,(first b) cl:nil))
                         value-bindings))
           ,@(mapcar (lambda (b)
                       (let ((init (hydra-fix-curried-calls (second b) names)))
                         (if (hydra-letrec-binding-lambda-p b)
                             `(setf (car ,(intern (format nil "~A-CELL" (first b)))) ,init)
                             `(setq ,(first b) ,init))))
                     bindings)  ;; preserve source order
           ,@(mapcar (lambda (f) (hydra-fix-curried-calls f names))
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
  "Load a generated Lisp file. Defers forms that fail due to forward references
   and retries them after subsequent definitions have been installed (up to 10
   passes). If any form still fails after the final pass, signals an error
   identifying the file, the last error, and the offending form.
   Suppresses style-warnings from generated code (unused variables, etc.)."
  (handler-bind ((style-warning #'muffle-warning))
  (with-open-file (in path :direction :input)
    (let ((*package* (find-package :cl-user))
          (*readtable* (copy-readtable))
          (*read-default-float-format* 'double-float)
          (pending nil))  ; list of (form . last-error) pairs
      ;; Read all forms, evaluating each; defer those that fail.
      (loop
        (let ((form (read in nil :eof)))
          (when (eq form :eof) (return))
          (unless (hydra-skip-form-p form)
            (handler-case
              (eval (hydra-transform-form form))
              (error (e)
                (push (cons form e) pending))))))
      (setf pending (nreverse pending))
      ;; Retry deferred forms up to 10 times to resolve forward references.
      (dotimes (pass 10)
        (when (null pending) (return))
        (let ((still-pending nil))
          (dolist (entry pending)
            (handler-case
              (eval (hydra-transform-form (car entry)))
              (error (e)
                (push (cons (car entry) e) still-pending))))
          (setf pending (nreverse still-pending))
          ;; Set function bindings for newly defined HYDRA_ variables
          (hydra-set-function-bindings)))
      ;; Fail loudly if anything remains unresolved.
      (when pending
        (let ((entry (first pending)))
          (error "hydra-load-file: ~A: ~A unresolved form(s) after 10 retries.~%First failure: ~A~%Form: ~S"
                 path (length pending) (cdr entry) (car entry))))))))

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

(defvar *hydra-skip-gen-main-files* nil
  "List of relative paths (e.g. \"lib/maps.lisp\") to skip when
   (hydra-load-gen-main) walks the gen-main tree. Used by the bootstrap
   demo, where hand-written files live alongside generated ones under
   the same hydra/ directory: the hand-written ones are loaded
   explicitly by run-tests.lisp via cl:load and must not be re-loaded
   by hydra-load-file (which only understands the rewriting style used
   by generated code).")

(defun hydra-load-gen-main ()
  "Load all generated main modules. Uses retry loop to handle forward references."
  ;; Resolve `..` segments in base via probe-file. Without this, SBCL's
  ;; `directory` recursion through a path containing `../` enumerates
  ;; only a partial subdir set, silently dropping files like
  ;; show/util.lisp from the load order. The bug is filesystem-dependent
  ;; and only surfaces under the run-tests harness (which sets cwd
  ;; elsewhere) — calling hydra-load-gen-main standalone happens to work.
  (let* ((dir-only (make-pathname :name nil :type nil
                                  :defaults *hydra-gen-main-dir*))
         (base (or (probe-file dir-only) dir-only))
         ;; Priority files: load these first for best dependency order
         (priority '("core.lisp" "error/core.lisp" "error/checking.lisp" "error/packaging.lisp"
                     "context.lisp" "graph.lisp"
                     "packaging.lisp" "ast.lisp" "coders.lisp" "phantoms.lisp"
                     "parsing.lisp" "query.lisp" "relational.lisp" "tabular.lisp" "testing.lisp"
                     "topology.lisp" "typing.lisp" "util.lisp" "variants.lisp"
                     "json/model.lisp" "classes.lisp" "constants.lisp" "paths.lisp"
                     "formatting.lisp" "rewriting.lisp" "sorting.lisp"
                     "names.lisp" "arity.lisp" "lexical.lisp"
                     "literals.lisp" "reflect.lisp" "languages.lisp" "parsers.lisp"
                     "templates.lisp"
                     "strip.lisp" "variables.lisp" "scoping.lisp" "dependencies.lisp"
                     "predicates.lisp" "resolution.lisp" "analysis.lisp" "environment.lisp"
                     "encoding.lisp" "decoding.lisp" "codegen.lisp"
                     "hoisting.lisp" "show/core.lisp" "show/error/core.lisp" "show/errors.lisp"
                     "show/error/packaging.lisp"
                     "validate/core.lisp" "validate/packaging.lisp"
                     "encode/core.lisp" "encode/error/core.lisp" "encode/error/checking.lisp" "encode/errors.lisp"
                     "encode/packaging.lisp"
                     "decode/core.lisp" "decode/error/core.lisp" "decode/error/checking.lisp" "decode/errors.lisp"
                     "decode/packaging.lisp"
                     "extract/core.lisp" "extract/json.lisp"
                     "substitution.lisp" "annotations.lisp" "unification.lisp"
                     "inference.lisp" "checking.lisp" "serialization.lisp" "reduction.lisp"
                     "json/coder.lisp" "json/parser.lisp" "json/writer.lisp"
                     "json/encode.lisp" "json/decode.lisp" "json/bootstrap.lisp"
                     "yaml/model.lisp" "dsl/yaml/model.lisp"
                     "json/yaml/encode.lisp" "json/yaml/decode.lisp"
                     ;; Language-specific coder modules (dependency order: syntax, environment, names, utils, serde, language, testing, coder)
                     "haskell/syntax.lisp" "haskell/language.lisp" "haskell/operators.lisp"
                     "haskell/environment.lisp" "haskell/utils.lisp"
                     "haskell/serde.lisp" "haskell/testing.lisp" "haskell/coder.lisp"
                     "java/syntax.lisp" "java/environment.lisp" "java/names.lisp"
                     "java/language.lisp"
                     "java/utils.lisp" "java/serde.lisp" "java/testing.lisp" "java/coder.lisp"
                     "python/syntax.lisp" "python/environment.lisp" "python/language.lisp"
                     "python/names.lisp" "python/utils.lisp" "python/serde.lisp"
                     "python/testing.lisp" "python/coder.lisp"
                     "scala/syntax.lisp" "scala/language.lisp" "scala/utils.lisp"
                     "scala/serde.lisp" "scala/coder.lisp"
                     "lisp/syntax.lisp" "lisp/language.lisp"
                     "lisp/serde.lisp" "lisp/coder.lisp"
                     "org/json/decoding.lisp"))
         ;; Collect ALL .lisp files from gen-main.
         (all-files (collect-lisp-files base))
         ;; Build ordered list: priority first, then remaining, then drop
         ;; anything in the caller-supplied skip set (see *hydra-skip-gen-main-files*).
         (remaining (remove-if (lambda (f) (member f priority :test #'equal)) all-files))
         (ordered (remove-if (lambda (f)
                               (member f *hydra-skip-gen-main-files* :test #'equal))
                             (append priority (sort (copy-list remaining) #'string<)))))
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
           "test/differentiation.lisp"
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
