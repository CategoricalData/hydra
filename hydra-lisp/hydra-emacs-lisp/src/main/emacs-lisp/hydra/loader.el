;;; hydra-loader.el --- Hydra Emacs Lisp loader -*- lexical-binding: t -*-
;;; Loads generated files with Lisp-1 → Lisp-2 compatibility transformations.

(require 'cl-lib)

;; ============================================================================
;; Path setup
;; ============================================================================

(defvar hydra-loader-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the loader.")

(defvar hydra-gen-main-dir
  (expand-file-name "../../../gen-main/emacs-lisp/hydra/" hydra-loader-dir)
  "Directory containing generated main modules.")

(defvar hydra-gen-test-dir
  (expand-file-name "../../../gen-test/emacs-lisp/hydra/" hydra-loader-dir)
  "Directory containing generated test modules.")

;; ============================================================================
;; cl-defstruct compatibility — override to use alist-based constructors
;; ============================================================================

;; The generated code uses (cl-defstruct name field1 field2 ...).
;; We redefine to create alist-based records with keyword keys,
;; matching the convention used by the coder.

(defvar hydra--defined-structs (make-hash-table :test 'equal)
  "Set of struct names already defined by hydra-defstruct.")

(defmacro hydra-defstruct (name &rest fields)
  "Define an alist-based struct with constructor and field accessors."
  (let ((constructor (intern (format "make-%s" name)))
        (field-keywords (mapcar (lambda (f) (intern (format ":%s" f) obarray)) fields)))
    `(unless (gethash ',name hydra--defined-structs)
       (puthash ',name t hydra--defined-structs)
       (defun ,constructor (&rest args)
         (if (and args (keywordp (car args)))
             (cl-loop for (k v) on args by #'cddr collect (cons k v))
           (cl-mapcar #'cons ',field-keywords args)))
       ,@(cl-mapcar
          (lambda (field kw)
            `(defun ,(intern (format "%s-%s" name field)) (rec)
               (cdr (assq ,kw rec))))
          fields field-keywords))))

;; ============================================================================
;; Lisp-1 → Lisp-2 transformation
;; ============================================================================

(defun hydra-fix-curried-calls (form &optional lambda-vars)
  "Transform Lisp-1 calling conventions to Emacs Lisp Lisp-2 conventions."
  (cond
   ((atom form) form)
   ;; Quote: unquote alists that contain forms needing evaluation
   ((eq (car form) 'quote)
    (let ((data (cadr form)))
      (if (and (consp data) (consp (car data))
               (cl-some (lambda (entry)
                          (and (consp entry)
                               (or (and (consp (car entry)) (symbolp (caar entry)))
                                   (and (consp (cdr entry)) (symbolp (cadr entry))
                                        (not (keywordp (cadr entry)))))))
                        data))
          (cons 'list
                (mapcar (lambda (entry)
                          (if (consp entry)
                              (list 'cons
                                    (hydra-fix-curried-calls (car entry) lambda-vars)
                                    (hydra-fix-curried-calls (cdr entry) lambda-vars))
                            entry))
                        data))
        form)))
   ;; cond: clauses are not function calls
   ((eq (car form) 'cond)
    (cons 'cond
          (mapcar (lambda (clause)
                    (if (consp clause)
                        (mapcar (lambda (f) (hydra-fix-curried-calls f lambda-vars)) clause)
                      clause))
                  (cdr form))))
   ;; Lambda forms: track bound parameters
   ((eq (car form) 'lambda)
    (let* ((params (cadr form))
           (new-vars (append (if (listp params) params nil) lambda-vars)))
      (cons 'lambda (cons params
                         (mapcar (lambda (f) (hydra-fix-curried-calls f new-vars))
                                 (cddr form))))))
   ;; let/let*: track bound names
   ((memq (car form) '(let let*))
    (let* ((bindings (cadr form))
           (body (cddr form))
           (names (mapcar (lambda (b) (if (consp b) (car b) b)) bindings))
           (new-vars (append names lambda-vars))
           (transformed-bindings
            (if (eq (car form) 'let)
                (mapcar (lambda (b)
                          (if (consp b)
                              (list (car b) (hydra-fix-curried-calls (cadr b) lambda-vars))
                            b))
                        bindings)
              ;; let*: accumulate bindings
              (let ((acc-vars lambda-vars)
                    (result nil))
                (dolist (b bindings (nreverse result))
                  (if (consp b)
                      (progn
                        (push (list (car b) (hydra-fix-curried-calls (cadr b) acc-vars)) result)
                        (push (car b) acc-vars))
                    (push b result))))))
           (transformed-body
            (mapcar (lambda (f) (hydra-fix-curried-calls f new-vars)) body)))
      (cons (car form) (cons transformed-bindings transformed-body))))
   ;; Letrec pattern: ((lambda (X) BODY) INIT) where INIT references X
   ((and (consp (car form))
         (eq (caar form) 'lambda)
         (= (length (cdr form)) 1)
         (let ((params (cadar form)))
           (and (listp params) (= (length params) 1)))
         (let* ((params (cadar form))
                (param (car params))
                (init (cadr form)))
           (let ((body-forms (cddar form)))
             (or
              ;; Pattern 1: init is a lambda with free reference to param
              (and (consp init) (eq (car init) 'lambda)
                   (cl-labels ((has-free-ref (f sym)
                                 (cond
                                  ((eq f sym) t)
                                  ((atom f) nil)
                                  ((eq (car f) 'quote) nil)
                                  ((and (eq (car f) 'lambda) (listp (cadr f)))
                                   (if (memq sym (cadr f)) nil
                                     (cl-some (lambda (sub) (has-free-ref sub sym)) (cddr f))))
                                  (t (or (has-free-ref (car f) sym)
                                         (has-free-ref (cdr f) sym))))))
                     (has-free-ref init param)))
              ;; Pattern 2: body is (param ...) — Y-combinator
              (and (= (length body-forms) 1)
                   (consp (car body-forms))
                   (eq (caar body-forms) param))))))
    (let* ((param (car (cadar form)))
           (body-forms (cddar form))
           (init (cadr form))
           (cell-sym (intern (format "%s-cell" param)))
           (new-vars (cons param lambda-vars)))
      `(let ((,cell-sym (list nil)))
         (let ((,param (lambda (&rest args) (apply (car ,cell-sym) args))))
           (setcar ,cell-sym
                   ,(hydra-fix-curried-calls init new-vars))
           ,@(mapcar (lambda (f) (hydra-fix-curried-calls f new-vars))
                     body-forms)))))
   ;; Immediately-invoked lambda
   ((and (consp (car form)) (eq (caar form) 'lambda) (cdr form))
    (let* ((lam (car form))
           (params (cadr lam))
           (new-vars (append (if (listp params) params nil) lambda-vars))
           (transformed-body (mapcar (lambda (f) (hydra-fix-curried-calls f new-vars))
                                     (cddr lam)))
           (transformed-args (mapcar (lambda (f) (hydra-fix-curried-calls f lambda-vars))
                                     (cdr form))))
      (cons (cons 'lambda (cons params transformed-body)) transformed-args)))
   ;; if_else optimization: eager → short-circuit
   ((and (consp (car form)) (consp (caar form))
         (= (length (cdr form)) 1)
         (= (length (cdar form)) 1)
         (consp (caar form))
         (let ((innermost (caaar form)))
           (and (symbolp innermost)
                (eq innermost 'hydra_lib_logic_if_else))))
    (let ((cond-form (hydra-fix-curried-calls (cadar (car form)) lambda-vars))
          (then-form (hydra-fix-curried-calls (cadr (car form)) lambda-vars))
          (else-form (hydra-fix-curried-calls (cadr form) lambda-vars)))
      (list 'if cond-form then-form else-form)))
   ;; Compound expression in function position → funcall
   ((and (consp (car form)) (not (eq (caar form) 'lambda)))
    (let ((inner (hydra-fix-curried-calls (car form) lambda-vars))
          (args (mapcar (lambda (f) (hydra-fix-curried-calls f lambda-vars)) (cdr form))))
      (cons 'funcall (cons inner args))))
   ;; Lambda-bound symbol in function position → funcall
   ((and (symbolp (car form))
         (memq (car form) lambda-vars)
         (cdr form))
    (cons 'funcall
          (cons (car form)
                (mapcar (lambda (f) (hydra-fix-curried-calls f lambda-vars)) (cdr form)))))
   ;; labels: recurse into bindings and body
   ((eq (car form) 'cl-labels)
    (let* ((bindings (cadr form))
           (body (cddr form))
           (transformed-bindings
            (mapcar (lambda (b)
                      (let* ((params (cadr b))
                             (fn-body (cddr b))
                             (inner-vars (append params lambda-vars)))
                        (cons (car b) (cons params
                                           (mapcar (lambda (f) (hydra-fix-curried-calls f inner-vars))
                                                   fn-body)))))
                    bindings))
           (transformed-body
            (mapcar (lambda (f) (hydra-fix-curried-calls f lambda-vars)) body)))
      (cons 'cl-labels (cons transformed-bindings transformed-body))))
   ;; letrec → mutable cell transform
   ((eq (car form) 'letrec)
    (let ((transformed (hydra-transform-letrec form)))
      (if transformed
          (hydra-fix-curried-calls transformed lambda-vars)
        (cons 'letrec
              (mapcar (lambda (f) (hydra-fix-curried-calls f lambda-vars)) (cdr form))))))
   ;; Default: recurse into subforms
   (t (cons (if (consp (car form))
                (hydra-fix-curried-calls (car form) lambda-vars)
              (car form))
            (mapcar (lambda (f) (hydra-fix-curried-calls f lambda-vars)) (cdr form))))))

(defun hydra--name-used-as-value-p (name form)
  "Check if NAME appears as a value (not in function position) in FORM."
  (cond
   ((eq form name) t)  ;; bare reference
   ((atom form) nil)
   ((eq (car form) 'quote) nil)
   ((eq (car form) 'lambda)
    (if (memq name (cadr form)) nil  ;; shadowed by lambda param
      (cl-some (lambda (f) (hydra--name-used-as-value-p name f)) (cddr form))))
   ;; (name args...) is a function call, not a value use — check args only
   ((eq (car form) name)
    (cl-some (lambda (f) (hydra--name-used-as-value-p name f)) (cdr form)))
   (t (or (hydra--name-used-as-value-p name (car form))
          (cl-some (lambda (f) (hydra--name-used-as-value-p name f)) (cdr form))))))

(defun hydra-transform-letrec (form)
  "Transform (letrec ((name init) ...) body...) to EL-compatible form.
   Uses cl-labels when safe (name only used in function position),
   otherwise falls back to mutable cells."
  (when (and (consp form) (eq (car form) 'letrec))
    (let* ((bindings (cadr form))
           (body (cddr form))
           (names (mapcar #'car bindings))
           (all-lambdas (cl-every (lambda (b) (and (consp (cadr b)) (eq (car (cadr b)) 'lambda))) bindings))
           ;; Check if any name is used as a value (not just called)
           (any-value-use
            (when all-lambdas
              (cl-some (lambda (name)
                         (or (cl-some (lambda (b) (hydra--name-used-as-value-p name (cadr b))) bindings)
                             (cl-some (lambda (f) (hydra--name-used-as-value-p name f)) body)))
                       names))))
      (if (and all-lambdas (not any-value-use))
          ;; Safe to use cl-labels (much faster)
          `(cl-labels ,(mapcar (lambda (b)
                                 (let* ((name (car b))
                                        (lam (cadr b))
                                        (params (cadr lam))
                                        (lbody (cddr lam)))
                                   `(,name ,params ,@lbody)))
                               bindings)
             ,@body)
        ;; Fall back to mutable cells
        (let ((cell-names (mapcar (lambda (b) (intern (format "%s-cell" (car b)))) bindings)))
          `(let ,(mapcar (lambda (cn) `(,cn (list nil))) cell-names)
             (let ,(cl-mapcar (lambda (b cn)
                                `(,(car b) (lambda (&rest args) (apply (car ,cn) args))))
                              bindings cell-names)
               ,@(cl-mapcar (lambda (b cn)
                              `(setcar ,cn ,(hydra-fix-curried-calls (cadr b) names)))
                            bindings cell-names)
               ,@(mapcar (lambda (f) (hydra-fix-curried-calls f names))
                         body))))))))

;; ============================================================================
;; Form transformation and loading
;; ============================================================================

(defun hydra-skip-form-p (form)
  "Return t if this form should be skipped during loading."
  (and (consp form)
       (or (memq (car form) '(require provide))
           (keywordp (car form)))))

(defun hydra-transform-form (form)
  "Transform a form before evaluation."
  (cond
   ;; cl-defstruct → hydra-defstruct
   ((and (consp form) (eq (car form) 'cl-defstruct))
    (cons 'hydra-defstruct (cdr form)))
   ;; letrec at top level
   ((and (consp form) (eq (car form) 'letrec))
    (hydra-transform-letrec (hydra-fix-curried-calls form)))
   ;; Default: apply curried call fixes
   (t (let ((fixed (hydra-fix-curried-calls form)))
        (if (and (consp fixed) (eq (car fixed) 'letrec))
            (hydra-transform-letrec fixed)
          fixed)))))

(defun hydra-set-function-bindings ()
  "For every hydra_* variable that holds a function, set the function cell."
  (mapatoms
   (lambda (sym)
     (let ((name (symbol-name sym)))
       (when (and (> (length name) 6)
                  (string-prefix-p "hydra_" name)
                  (boundp sym)
                  (functionp (symbol-value sym))
                  (not (fboundp sym)))
         (fset sym (symbol-value sym)))))))

(defun hydra-byte-compile-all ()
  "Byte-compile all hydra_* function values for ~20-90x speedup.
   For further performance, pre-compile .el files with native-compile
   (see README).
"
  (let ((compiled 0) (skipped 0))
    (mapatoms
     (lambda (sym)
       (let ((name (symbol-name sym)))
         (when (and (> (length name) 6)
                    (string-prefix-p "hydra_" name)
                    (boundp sym)
                    (functionp (symbol-value sym))
                    (not (byte-code-function-p (symbol-value sym))))
           (condition-case nil
               (progn
                 (set sym (byte-compile (symbol-value sym)))
                 (when (fboundp sym)
                   (fset sym (symbol-value sym)))
                 (setq compiled (1+ compiled)))
             (error (setq skipped (1+ skipped))))))))
    (message "Byte-compiled %d hydra functions (%d skipped)" compiled skipped)))

(defun hydra-load-file (path)
  "Load a generated Emacs Lisp file with transformations."
  (with-temp-buffer
    (set-buffer-multibyte t)
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (goto-char (point-min))
    (let ((failed-forms nil))
      ;; Read and eval all forms
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (unless (hydra-skip-form-p form)
                (condition-case _err
                    (eval (hydra-transform-form form) t)
                  (error
                   (push form failed-forms))))))
        (end-of-file nil))
      ;; Retry failed forms
      (setq failed-forms (nreverse failed-forms))
      (let ((pass 0))
        (while (and failed-forms (< pass 10))
          (let ((still-failed nil))
            (dolist (form failed-forms)
              (condition-case _err
                  (eval (hydra-transform-form form) t)
                (error
                 (push form still-failed))))
            (setq failed-forms (nreverse still-failed))
            (hydra-set-function-bindings))
          (setq pass (1+ pass)))))))

;; ============================================================================
;; Module loading
;; ============================================================================

(defun hydra-collect-el-files (dir &optional prefix)
  "Collect all .el files from DIR recursively."
  (let ((result nil))
    (dolist (entry (directory-files dir t "\\.el$"))
      (let ((rel (if prefix
                     (format "%s/%s" prefix (file-name-nondirectory entry))
                   (file-name-nondirectory entry))))
        (push rel result)))
    (dolist (subdir (directory-files dir t "^[^.]" t))
      (when (file-directory-p subdir)
        (let ((subdir-name (file-name-nondirectory subdir)))
          (setq result (append result
                               (hydra-collect-el-files subdir
                                                       (if prefix
                                                           (format "%s/%s" prefix subdir-name)
                                                         subdir-name)))))))
    result))

(defun hydra-load-gen-main ()
  "Load all generated main modules, then byte-compile for performance."
  (let* ((base hydra-gen-main-dir)
         (priority '("core.el" "error.el" "context.el" "graph.el"
                     "module.el" "ast.el" "coders.el" "grammar.el" "phantoms.el"
                     "parsing.el" "query.el" "relational.el" "tabular.el" "testing.el"
                     "topology.el" "typing.el" "util.el" "variants.el"
                     "json/model.el" "classes.el" "constants.el" "accessors.el"
                     "formatting.el" "tarjan.el" "rewriting.el" "sorting.el"
                     "names.el" "schemas.el" "arity.el" "lexical.el"
                     "literals.el" "reflect.el" "languages.el" "parsers.el" "grammars.el"
                     "templates.el" "encoding.el" "decoding.el" "code_generation.el"
                     "hoisting.el" "show/core.el" "encode/core.el" "decode/core.el"
                     "extract/core.el" "extract/helpers.el" "extract/util.el" "extract/json.el"
                     "substitution.el" "annotations.el" "unification.el"
                     "inference.el" "checking.el" "serialization.el" "reduction.el"
                     "json/parser.el" "json/writer.el"
                     "json/encode.el" "json/decode.el" "json/bootstrap.el"))
         (all-files (hydra-collect-el-files base))
         (remaining (cl-remove-if (lambda (f) (member f priority)) all-files))
         (ordered (append priority (sort (copy-sequence remaining) #'string<))))
    (dolist (f ordered)
      (let ((path (expand-file-name f base)))
        (when (file-exists-p path)
          (message "Loading %s..." f)
          (hydra-load-file path))))
    ;; Byte-compile all loaded functions for ~13x speedup on curried calls
    (hydra-byte-compile-all)))

(defun hydra-load-gen-test ()
  "Load all generated test modules."
  (let* ((base hydra-gen-test-dir)
         (files '("test/test_types.el"
                  "test/test_terms.el"
                  "test/test_graph.el"
                  "test/lib/chars.el"
                  "test/lib/eithers.el"
                  "test/lib/equality.el"
                  "test/lib/lists.el"
                  "test/lib/literals.el"
                  "test/lib/logic.el"
                  "test/lib/maps.el"
                  "test/lib/math.el"
                  "test/lib/maybes.el"
                  "test/lib/pairs.el"
                  "test/lib/sets.el"
                  "test/lib/strings.el"
                  "test/annotations.el"
                  "test/checking/advanced.el"
                  "test/checking/algebraic_types.el"
                  "test/checking/collections.el"
                  "test/checking/failures.el"
                  "test/checking/fundamentals.el"
                  "test/checking/nominal_types.el"
                  "test/checking/all.el"
                  "test/eta_expansion.el"
                  "test/formatting.el"
                  "test/hoisting/cases.el"
                  "test/hoisting/let.el"
                  "test/hoisting/all.el"
                  "test/inference/algebraic_types.el"
                  "test/inference/algorithm_w.el"
                  "test/inference/classes.el"
                  "test/inference/failures.el"
                  "test/inference/fundamentals.el"
                  "test/inference/kernel_examples.el"
                  "test/inference/nominal_types.el"
                  "test/inference/all.el"
                  "test/json/coder.el"
                  "test/json/parser.el"
                  "test/json/roundtrip.el"
                  "test/json/writer.el"
                  "test/reduction.el"
                  "test/rewriting.el"
                  "test/serialization.el"
                  "test/sorting.el"
                  "test/substitution.el"
                  "test/unification.el"
                  "test/test_suite.el")))
    (dolist (f files)
      (let ((path (expand-file-name f base)))
        (when (file-exists-p path)
          (hydra-load-file path))))
))

;; ============================================================================
;; Load hand-written native libraries
;; ============================================================================

(dolist (f '("lib/chars.el" "lib/eithers.el" "lib/equality.el"
             "lib/lists.el" "lib/literals.el" "lib/logic.el"
             "lib/maps.el" "lib/math.el" "lib/maybes.el"
             "lib/pairs.el" "lib/regex.el" "lib/sets.el" "lib/strings.el"))
  (load (expand-file-name f hydra-loader-dir) nil t))

(defun hydra-load-prims-and-libraries ()
  "Load prims and libraries (must be called after gen-main)."
  (load (expand-file-name "prims.el" hydra-loader-dir) nil t)
  (load (expand-file-name "lib/libraries.el" hydra-loader-dir) nil t))

(provide 'hydra-loader)
