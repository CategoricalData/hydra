;;; bootstrap.el --- Hydra Emacs Lisp bootstrap -*- lexical-binding: t -*-
;;; Loads Hydra modules from JSON and generates code for a target language.
;;;
;;; Usage: emacs --batch --no-init-file -l bootstrap.el -- --target <target> --json-dir <path> [OPTIONS]
;;;
;;; Options:
;;;   --target <lang>       Target language (haskell|java|python|clojure)
;;;   --json-dir <path>     Path to kernel JSON modules
;;;   --output <dir>        Output base directory (default: /tmp/hydra-bootstrapping-demo)
;;;   --kernel-only         Only generate kernel modules
;;;   --include-tests       Also generate test modules

(require 'cl-lib)

;; Increase limits for deeply nested generated code
(setq max-lisp-eval-depth 100000)
(setq max-specpdl-size 100000)

;; ============================================================================
;; Command-line argument parsing
;; ============================================================================

(defvar bootstrap-args command-line-args-left
  "Arguments passed after -- on the command line.")

;; Consume all args so Emacs doesn't try to interpret them
(setq command-line-args-left nil)

(defun bootstrap-get-arg (name &optional default)
  "Get the value following NAME in bootstrap-args, or DEFAULT."
  (let ((pos (cl-position name bootstrap-args :test #'string=)))
    (if (and pos (< (1+ pos) (length bootstrap-args)))
        (nth (1+ pos) bootstrap-args)
      default)))

(defun bootstrap-has-flag (name)
  "Return non-nil if NAME appears in bootstrap-args."
  (member name bootstrap-args))

(defvar bootstrap-target (bootstrap-get-arg "--target"))
(defvar bootstrap-json-dir (bootstrap-get-arg "--json-dir"))
(defvar bootstrap-output-base (bootstrap-get-arg "--output" "/tmp/hydra-bootstrapping-demo"))
(defvar bootstrap-kernel-only (bootstrap-has-flag "--kernel-only"))
(defvar bootstrap-include-tests (bootstrap-has-flag "--include-tests"))

(unless (and bootstrap-target bootstrap-json-dir)
  (princ "Usage: emacs --batch --no-init-file -l bootstrap.el -- --target <target> --json-dir <path> [OPTIONS]\n")
  (kill-emacs 1))

;; ============================================================================
;; Load the Hydra Emacs Lisp kernel
;; ============================================================================

(let* ((script-path (or load-file-name buffer-file-name
                        (expand-file-name "hydra/bootstrap.el" default-directory)))
       (hydra-dir (file-name-directory script-path)))
  ;; Load the kernel loader
  (load (expand-file-name "loader.el" hydra-dir) nil t)
  ;; Override gen-main and gen-test dirs to point at the dist/ generated content
  ;; (loader.el defaults to heads/lisp/emacs-lisp/src/gen-main/..., which no
  ;; longer exists; mirrors the override in run-tests.el).
  (let ((dist-base (expand-file-name "../../../../../../../dist/emacs-lisp/hydra-kernel/" hydra-dir)))
    (setq hydra-gen-main-dir (expand-file-name "src/main/emacs-lisp/hydra/" dist-base))
    (setq hydra-gen-test-dir (expand-file-name "src/test/emacs-lisp/hydra/" dist-base))))

(princ "Loading kernel...\n")

;; Pre-declare all exported symbols from coder modules.
;; The loader's retry mechanism drops forms with forward references;
;; pre-declaration ensures all symbols exist (as nil) before loading.
(defun bootstrap-pre-declare-exports (dir)
  "Scan all .el files in DIR recursively and pre-declare their defvar symbols."
  (dolist (file (directory-files-recursively dir "\\.el$"))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^(defvar \\(hydra_[a-zA-Z0-9_]+\\)" nil t)
        (let ((sym (intern (match-string 1))))
          (unless (boundp sym)
            (set sym nil)))))))

(let ((ext-dir (expand-file-name "ext/" hydra-gen-main-dir)))
  (when (file-directory-p ext-dir)
    (bootstrap-pre-declare-exports ext-dir)))

;; Override byte-compile-all: skip deeply nested functions that overflow
;; Emacs's C stack during byte-compilation or at runtime.
;; The skip list covers code generation, adaptation, type checking, inference,
;; and rewriting modules which have deeply recursive generated code.
;; Ext coder modules (hydra_*) ARE compiled — they need it for performance
;; and compile+run successfully.
(fset 'hydra-byte-compile-all
  (lambda ()
    (let ((skip-prefixes '("hydra_codegen_" "hydra_reduction_"
                           "hydra_adapt_" "hydra_checking_" "hydra_inference_"
                           "hydra_hoisting_" "hydra_encoding_" "hydra_decoding_"
                           "hydra_rewriting_" "hydra_schemas_")))
      (let ((compiled 0) (skipped 0))
        (mapatoms
          (lambda (sym)
            (let ((name (symbol-name sym)))
              (when (and (> (length name) 6)
                         (string-prefix-p "hydra_" name)
                         (boundp sym)
                         (functionp (symbol-value sym))
                         (not (byte-code-function-p (symbol-value sym))))
                (if (cl-some (lambda (p) (string-prefix-p p name)) skip-prefixes)
                    (cl-incf skipped)
                  (condition-case nil
                      (progn
                        (set sym (byte-compile (symbol-value sym)))
                        (when (fboundp sym)
                          (fset sym (symbol-value sym)))
                        (cl-incf compiled))
                    (error (cl-incf skipped))))))))
        (princ (format "Byte-compiled %d hydra functions (%d kept interpreted)\n" compiled skipped))))))

(hydra-load-gen-main)
(hydra-load-prims-and-libraries)
(hydra-set-function-bindings)

(princ "Kernel loaded.\n")

;; ============================================================================
;; JSON to Hydra conversion
;; ============================================================================

(defun bootstrap-read-json-file (path)
  "Read and parse a JSON file at PATH. Returns parsed JSON with hash-tables for objects."
  (with-temp-buffer
    (set-buffer-multibyte t)
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (json-parse-string (buffer-string)
                       :object-type 'hash-table
                       :null-object :null
                       :false-object :json-false)))

(defun bootstrap-json-to-hydra (obj)
  "Convert Emacs JSON value to Hydra JSON value (list-based tagged union).
Uses hash-tables for objects (from json-parse-string with object-type hash-table)."
  (cond
   ((eq obj :null) (list :null nil))
   ((eq obj t) (list :boolean t))
   ((eq obj :json-false) (list :boolean nil))
   ((numberp obj) (list :number (float obj)))
   ((stringp obj) (list :string obj))
   ((hash-table-p obj)
    ;; JSON object → Hydra object (alist of string keys to hydra-json values)
    (let ((pairs nil))
      (maphash (lambda (k v)
                 (push (cons k (bootstrap-json-to-hydra v)) pairs))
               obj)
      (list :object (nreverse pairs))))
   ((vectorp obj)
    ;; JSON array → Hydra array (list of hydra-json values)
    (list :array (mapcar #'bootstrap-json-to-hydra (append obj nil))))
   (t (error "Unexpected JSON value type: %s" (type-of obj)))))

;; ============================================================================
;; Bootstrap graph construction
;; ============================================================================

(defun bootstrap-graph ()
  "Create a minimal bootstrap graph."
  (funcall 'make-hydra_graph_graph nil nil nil nil nil
           (standard-library)
           nil nil))

(defun bootstrap-schema-map ()
  "Build the schema map from hydra_json_bootstrap_types_by_name."
  (let ((result nil))
    (dolist (pair (symbol-value 'hydra_json_bootstrap_types_by_name))
      (let* ((name (car pair))
             (typ (cdr pair))
             (ts (funcall (symbol-value 'hydra_scoping_f_type_to_type_scheme) typ))
             (stripped (funcall (symbol-value 'hydra_strip_deannotate_type_recursive)
                                (funcall 'hydra_core_type_scheme-type ts))))
        (push (cons name stripped) result)))
    (nreverse result)))

(defun bootstrap-namespace-to-path (ns)
  "Convert a namespace string to a file path."
  (funcall (symbol-value 'hydra_codegen_namespace_to_path) ns))

;; ============================================================================
;; Module loading from JSON
;; ============================================================================

(defun bootstrap-load-module-from-json (bs-graph schema-map ns-str)
  "Load a single module from its JSON file."
  (let* ((file-path (format "%s/%s.json" bootstrap-json-dir (bootstrap-namespace-to-path ns-str)))
         (json-obj (bootstrap-read-json-file file-path))
         (hydra-json (bootstrap-json-to-hydra json-obj))
         (mod-type (list :variable "hydra.packaging.Module"))
         (json-result (funcall (funcall (funcall (funcall
                        (symbol-value 'hydra_json_decode_from_json) schema-map)
                        "hydra.packaging.Module") mod-type) hydra-json)))
    (when (eq (car json-result) :left)
      (error "JSON decode error for %s: %s" ns-str (cadr json-result)))
    (let* ((term (cadr json-result))
           (mod-result (funcall (funcall
                         (symbol-value 'hydra_decode_packaging_module) bs-graph) term)))
      (when (eq (car mod-result) :left)
        (error "Module decode error for %s: %s" ns-str (cadr mod-result)))
      (cadr mod-result))))

(defun bootstrap-read-manifest-field (json-dir field-name)
  "Read a field from the manifest.json in JSON-DIR."
  (let* ((manifest-path (format "%s/manifest.json" json-dir))
         (manifest (bootstrap-read-json-file manifest-path)))
    (if (hash-table-p manifest)
        (let ((val (gethash field-name manifest)))
          (if (vectorp val) (append val nil) val))
      nil)))

(defun bootstrap-load-modules-from-json (json-dir namespaces)
  "Load modules from JSON for each namespace in NAMESPACES."
  (let ((bs-graph (bootstrap-graph))
        (schema-map (bootstrap-schema-map)))
    (mapcar (lambda (ns)
              (princ (format "  Loaded: %s\n" ns))
              (bootstrap-load-module-from-json bs-graph schema-map ns))
            namespaces)))

;; ============================================================================
;; Coder resolution
;; ============================================================================

(defun bootstrap-resolve-coder (target)
  "Resolve the coder, language, flags, and subdirectory for TARGET."
  (cond
   ((string= target "python")
    (list (symbol-value 'hydra_python_coder_module_to_python)
          (symbol-value 'hydra_python_language_python_language)
          (list nil t t nil)
          "python"))
   ((string= target "java")
    (list (symbol-value 'hydra_java_coder_module_to_java)
          (symbol-value 'hydra_java_language_java_language)
          (list nil t nil t)
          "java"))
   ((string= target "haskell")
    (list (symbol-value 'hydra_haskell_coder_module_to_haskell)
          (symbol-value 'hydra_haskell_language_haskell_language)
          (list nil nil nil nil)
          "haskell"))
   ((or (string= target "clojure") (string= target "scheme")
        (string= target "common-lisp") (string= target "emacs-lisp"))
    (let* ((dialect-alist '(("clojure" . :clojure) ("scheme" . :scheme)
                            ("common-lisp" . :commonLisp) ("emacs-lisp" . :emacsLisp)))
           (ext-alist '(("clojure" . ".clj") ("scheme" . ".scm")
                        ("common-lisp" . ".lisp") ("emacs-lisp" . ".el")))
           (dialect (cdr (assoc target dialect-alist)))
           (ext (cdr (assoc target ext-alist)))
           (mtl (symbol-value 'hydra_lisp_coder_module_to_lisp))
           (pte (symbol-value 'hydra_lisp_serde_program_to_expr)))
      (list (lambda (mod)
              (lambda (defs)
                (lambda (cx)
                  (lambda (g)
                    (let ((result (funcall (funcall (funcall (funcall (funcall mtl (list dialect nil)) mod) defs) cx) g)))
                      (if (eq (car result) :left)
                          result
                        (let* ((program (cadr result))
                               (code (funcall 'hydra_serialization_print_expr
                                       (funcall 'hydra_serialization_parenthesize
                                         (funcall pte program))))
                               (ns-val (let ((ns (cdr (assoc :namespace mod))))
                                         (if (stringp ns) ns (cdr (assoc :value ns)))))
                               (fp (format "%s%s" (bootstrap-namespace-to-path ns-val) ext)))
                          (list :right (list (cons fp code))))))))))
            (symbol-value 'hydra_lisp_language_lisp_language)
            (list nil nil nil nil)
            target)))
   (t (error "Unsupported target: %s" target))))


;; ============================================================================
;; Code generation
;; ============================================================================

(defun bootstrap-generate-sources (coder language flags out-dir universe-mods mods-to-generate)
  "Generate source files using the full generate_source_files pipeline.
Write output to OUT-DIR. UNIVERSE-MODS is the full set; MODS-TO-GENERATE is the subset to generate."
  (let* ((bs-graph (bootstrap-graph))
         (cx (funcall 'make-hydra_context_in_context nil nil))
         (do-infer (nth 0 flags))
         (do-expand (nth 1 flags))
         (do-hoist-case (nth 2 flags))
         (do-hoist-poly (nth 3 flags))
         (t0 (float-time))
         (result (condition-case err
                     (funcall (funcall (funcall (funcall (funcall (funcall (funcall (funcall
                       (funcall (funcall
                         (symbol-value 'hydra_codegen_generate_source_files)
                         coder) language) do-infer) do-expand) do-hoist-case) do-hoist-poly)
                       bs-graph) universe-mods) mods-to-generate) cx)
                   (error (princ (format "  GENERATE ERROR: %s\n" err)) (list :left (format "%s" err)))))
         (t1 (float-time)))
    (when (eq (car result) :left)
      (error "Code generation failed: %s" (cadr result)))
    (let ((files (cadr result)))
      (princ (format "  Code generation took %.1fs for %d files\n"
                     (- t1 t0) (length files)))
      (dolist (pair files)
        (let* ((path (format "%s/%s" out-dir (car pair)))
               (content (cadr pair))
               (content (if (and (> (length content) 0)
                                 (not (= (aref content (1- (length content))) ?\n)))
                            (concat content "\n")
                          content))
               (dir (file-name-directory path)))
          (make-directory dir t)
          (with-temp-file path
            (insert content))))
      (length files))))

;; ============================================================================
;; Main
;; ============================================================================

;; Re-set function bindings after coder modules are loaded
(hydra-set-function-bindings)

;; Ensure target_python_version is set (may be dropped by loader retry mechanism)
(unless (boundp 'hydra_python_coder_target_python_version)
  (when (boundp 'hydra_python_utils_target_python_version)
    (setq hydra_python_coder_target_python_version
          (symbol-value 'hydra_python_utils_target_python_version))))

(let ((coder-info (bootstrap-resolve-coder bootstrap-target))
      (target-cap (concat (upcase (substring bootstrap-target 0 1))
                          (substring bootstrap-target 1))))
  (let ((coder (car coder-info))
        (language (cadr coder-info))
        (flags (nth 2 coder-info))
        (subdir (nth 3 coder-info)))

    (princ (format "==========================================\n"))
    (princ (format "Mapping JSON to %s (via Emacs Lisp host)\n" target-cap))
    (princ (format "==========================================\n"))

    ;; Load main modules
    (princ (format "\nStep 1: Loading main modules from JSON...\n"))
    (let* ((main-ns (bootstrap-read-manifest-field bootstrap-json-dir "mainModules"))
           (eval-ns (bootstrap-read-manifest-field bootstrap-json-dir "evalLibModules"))
           (all-ns (append main-ns eval-ns))
           (all-mods (bootstrap-load-modules-from-json bootstrap-json-dir all-ns))
           (total-bindings (cl-reduce #'+ (mapcar (lambda (m)
                                                    (length (cdr (assoc :definitions m))))
                                                  all-mods))))
      (princ (format "  Loaded %d modules (%d bindings).\n" (length all-mods) total-bindings))

      ;; Filter if needed
      (let* ((mods-to-generate
              (if bootstrap-kernel-only
                  (cl-remove-if (lambda (m)
                                  (let ((ns (cdr (assoc :namespace m))))
                                    (let ((ns-str (if (stringp ns) ns (cdr (assoc :value ns)))))
                                      (or (string-match-p "hydra\\.ext\\." ns-str)
                                          (string-match-p "hydra\\.json\\.yaml\\." ns-str)))))
                                all-mods)
                all-mods))
             (out-main (format "%s/emacs-lisp-to-%s/src/gen-main/%s"
                               bootstrap-output-base bootstrap-target subdir)))

        (when bootstrap-kernel-only
          (princ (format "\nFiltering to kernel modules: %d of %d\n"
                         (length mods-to-generate) (length all-mods))))

        (princ (format "\nMapping %d modules to %s...\n" (length mods-to-generate) target-cap))
        (princ (format "  Output: %s\n" out-main))

        (let ((file-count (bootstrap-generate-sources coder language flags out-main all-mods mods-to-generate)))
          (princ (format "  Generated %d files.\n" file-count))

          ;; Tests
          (when bootstrap-include-tests
            (princ (format "\nLoading test modules from JSON...\n"))
            (let* ((test-json-dir2 (let ((pos (cl-search "gen-main" bootstrap-json-dir)))
                                     (if pos
                                         (concat (substring bootstrap-json-dir 0 pos)
                                                 "gen-test"
                                                 (substring bootstrap-json-dir (+ pos 8)))
                                       bootstrap-json-dir)))
                   (test-ns (bootstrap-read-manifest-field bootstrap-json-dir "testModules"))
                   (test-mods (bootstrap-load-modules-from-json test-json-dir2 test-ns))
                   (all-universe (append all-mods test-mods))
                   (out-test (format "%s/emacs-lisp-to-%s/src/gen-test/%s"
                                     bootstrap-output-base bootstrap-target subdir)))
              (princ (format "  Loaded %d test modules.\n" (length test-mods)))
              (princ (format "\nMapping test modules to %s...\n" target-cap))
              (let ((test-count (bootstrap-generate-sources coder language flags out-test all-universe test-mods)))
                (princ (format "  Generated %d test files.\n" test-count)))))

          (princ (format "\n==========================================\n"))
          (princ (format "Done: %d main files\n" file-count))
          (princ (format "  Output: %s/emacs-lisp-to-%s\n" bootstrap-output-base bootstrap-target))
          (princ (format "==========================================\n")))))))

(kill-emacs 0)
