;;; Hydra Common Lisp bootstrap
;;; Loads Hydra modules from JSON and generates code for a target language.
;;;
;;; Usage: sbcl --script bootstrap.lisp --target <target> --json-dir <path> [OPTIONS]
;;;
;;; Options:
;;;   --target <lang>       Target language (haskell|java|python|clojure)
;;;   --json-dir <path>     Path to kernel JSON modules
;;;   --output <dir>        Output base directory (default: /tmp/hydra-bootstrapping-demo)
;;;   --kernel-only         Only generate kernel modules
;;;   --include-tests       Also generate test modules
(in-package :cl-user)

;; Parse command-line arguments
;; With --load, SBCL puts everything after -- in *posix-argv*
(defvar *bootstrap-args*
  (let ((args (or sb-ext:*posix-argv* nil)))
    ;; Skip everything up to and including "--"
    (or (cdr (member "--" args :test #'string=)) args)))

(defun get-arg (name &optional default)
  (let ((pos (position name *bootstrap-args* :test #'string=)))
    (if (and pos (< (1+ pos) (length *bootstrap-args*)))
        (nth (1+ pos) *bootstrap-args*)
        default)))

(defun has-flag (name)
  (member name *bootstrap-args* :test #'string=))

(defvar *target* (get-arg "--target"))
(defvar *json-dir* (get-arg "--json-dir"))
(defvar *output-base* (get-arg "--output" "/tmp/hydra-bootstrapping-demo"))
(defvar *kernel-only* (has-flag "--kernel-only"))
(defvar *include-tests* (has-flag "--include-tests"))

(unless (and *target* *json-dir*)
  (format t "Usage: sbcl --script bootstrap.lisp --target <target> --json-dir <path> [OPTIONS]~%")
  (sb-ext:exit :code 1))

;; Load the Hydra Common Lisp kernel
(let* ((script-path (or *load-truename*
                        (merge-pathnames "hydra/bootstrap.lisp"
                          (make-pathname :directory (pathname-directory (truename *default-pathname-defaults*))))))
       (hydra-dir (make-pathname :directory (pathname-directory script-path))))
  ;; Load the kernel loader
  (load (merge-pathnames "loader.lisp" hydra-dir))
  ;; Load JSON reader
  (load (merge-pathnames "json-reader.lisp" hydra-dir)))

(format t "Loading kernel...~%")
(force-output)

(hydra-load-gen-main)
(hydra-load-prims-and-libraries)
(hydra-set-function-bindings)
(format t "Kernel loaded.~%")
(force-output)

;; --- JSON to Hydra conversion ---

(defun cl-to-hydra-json (obj)
  "Convert CL JSON value to Hydra JSON value (list-based tagged union)."
  (cond
    ((eq obj :null) (list :null nil))
    ((eq obj t) (list :boolean t))
    ((eq obj :json-false) (list :boolean nil))
    ((eq obj :json-empty-object) (list :object nil))
    ((null obj) (list :array nil))  ;; empty JSON arrays
    ((numberp obj) (list :number (coerce obj 'double-float)))
    ((stringp obj) (list :string obj))
    ((and (consp obj) (consp (car obj)) (stringp (caar obj)))
     ;; alist (JSON object)
     (list :object
           (mapcar (lambda (pair)
                     (cons (car pair) (cl-to-hydra-json (cdr pair))))
                   obj)))
    ((listp obj)
     ;; list (JSON array)
     (list :array (mapcar #'cl-to-hydra-json obj)))
    (t (error "Unexpected JSON value type: ~A" (type-of obj)))))

;; --- Bootstrap graph construction ---

(defun bootstrap-graph ()
  (funcall 'make-hydra_graph_graph nil nil nil nil nil
    (standard-library)
    nil nil))

(defun bootstrap-schema-map ()
  (let ((result nil))
    (dolist (pair (symbol-value 'hydra_json_bootstrap_types_by_name))
      (let* ((name (car pair))
             (typ (cdr pair))
             (ts (funcall (symbol-value 'hydra_rewriting_f_type_to_type_scheme) typ))
             (stripped (funcall (symbol-value 'hydra_rewriting_deannotate_type_recursive)
                               (funcall 'hydra_core_type_scheme-type ts))))
        (push (cons name stripped) result)))
    (nreverse result)))

(defun namespace-to-path (ns)
  (funcall (symbol-value 'hydra_code_generation_namespace_to_path) ns))

;; --- Module loading from JSON ---

(defun load-module-from-json (bs-graph schema-map ns-str &optional (json-dir *json-dir*))
  (let* ((file-path (format nil "~A/~A.json" json-dir (namespace-to-path ns-str)))
         (json-obj (json-read-file file-path))
         (hydra-json (cl-to-hydra-json json-obj))
         (mod-type (list :variable "hydra.module.Module"))
         (json-result (funcall (funcall (funcall (funcall
                        (symbol-value 'hydra_json_decode_from_json) schema-map)
                        "hydra.module.Module") mod-type) hydra-json)))
    (when (eq (first json-result) :left)
      (error "JSON decode error for ~A: ~A" ns-str (second json-result)))
    (let* ((term (second json-result))
           (mod-result (funcall (funcall
                         (symbol-value 'hydra_decode_module_module) bs-graph) term)))
      (when (eq (first mod-result) :left)
        (error "Module decode error for ~A: ~A" ns-str (second mod-result)))
      (second mod-result))))

(defun read-manifest-field (json-dir field-name)
  (let* ((manifest-path (format nil "~A/manifest.json" json-dir))
         (manifest (json-read-file manifest-path))
         (entry (assoc field-name manifest :test #'string=)))
    (if entry (cdr entry) nil)))

(defun load-modules-from-json (json-dir namespaces)
  (let ((bs-graph (bootstrap-graph))
        (schema-map (bootstrap-schema-map)))
    (mapcar (lambda (ns)
              (format t "  Loaded: ~A~%" ns)
              (force-output)
              (load-module-from-json bs-graph schema-map ns json-dir))
            namespaces)))

;; --- Coder resolution ---

(defun resolve-coder (target)
  (cond
    ((string= target "python")
     (list (symbol-value 'hydra_ext_python_coder_module_to_python)
           (symbol-value 'hydra_ext_python_language_python_language)
           (list nil t t nil)  ; flags: infer=f expand=t hoistCase=t hoistPoly=f
           "python"))
    ((string= target "java")
     (list (symbol-value 'hydra_ext_java_coder_module_to_java)
           (symbol-value 'hydra_ext_java_language_java_language)
           (list nil t nil t)
           "java"))
    ((string= target "haskell")
     (list (symbol-value 'hydra_ext_haskell_coder_module_to_haskell)
           (symbol-value 'hydra_ext_haskell_language_haskell_language)
           (list nil nil nil nil)
           "haskell"))
    ((or (string= target "clojure") (string= target "scheme")
         (string= target "common-lisp") (string= target "emacs-lisp"))
     ;; Lisp targets use Lisp coder with dialect parameter
     (let* ((dialect-map '(("clojure" . :clojure) ("scheme" . :scheme)
                           ("common-lisp" . :commonLisp) ("emacs-lisp" . :emacsLisp)))
            (ext-map '(("clojure" . ".clj") ("scheme" . ".scm")
                       ("common-lisp" . ".lisp") ("emacs-lisp" . ".el")))
            (dialect (cdr (assoc target dialect-map :test #'string=)))
            (ext (cdr (assoc target ext-map :test #'string=)))
            (mtl (symbol-value 'hydra_ext_lisp_coder_module_to_lisp))
            (pte (symbol-value 'hydra_ext_lisp_serde_program_to_expr)))
       (list (lambda (mod)
               (lambda (defs)
                 (lambda (cx)
                   (lambda (g)
                     (let ((result (funcall (funcall (funcall (funcall (funcall mtl (list dialect nil)) mod) defs) cx) g)))
                       (if (eq (first result) :left)
                           result
                           (let* ((program (second result))
                                  (code (funcall 'hydra_serialization_print_expr
                                          (funcall 'hydra_serialization_parenthesize
                                            (funcall pte program))))
                                  (ns-val (let ((ns (cdr (assoc :namespace mod))))
                                            (if (stringp ns) ns (cdr (assoc :value ns)))))
                                  (fp (format nil "~A~A" (namespace-to-path ns-val) ext)))
                             (list :right (list (cons fp code))))))))))
             (symbol-value 'hydra_ext_lisp_language_lisp_language)
             (list nil nil nil nil)
             target)))
    (t (error "Unsupported target: ~A" target))))

;; --- Code generation ---

(defun generate-sources (coder language flags out-dir universe-mods mods-to-generate)
  (let* ((bs-graph (bootstrap-graph))
         (cx (funcall 'make-hydra_context_in_context nil nil))
         (do-infer (first flags))
         (do-expand (second flags))
         (do-hoist-case (third flags))
         (do-hoist-poly (fourth flags))
         (t0 (get-internal-real-time))
         (result (handler-case
                   (funcall (funcall (funcall (funcall (funcall (funcall (funcall (funcall
                     (funcall (funcall
                       (symbol-value 'hydra_code_generation_generate_source_files)
                       coder) language) do-infer) do-expand) do-hoist-case) do-hoist-poly)
                     bs-graph) universe-mods) mods-to-generate) cx)
                   (undefined-function (e)
                     (format t "~%UNDEFINED FUNCTION: ~A~%" (cell-error-name e))
                     (error e))))
         (t1 (get-internal-real-time)))
    (when (eq (first result) :left)
      (error "Code generation failed: ~A" (second result)))
    (let ((files (second result)))
      (format t "  Code generation took ~,1Fs for ~A files~%"
              (/ (- t1 t0) (float internal-time-units-per-second)) (length files))
      (force-output)
      (dolist (pair files)
        (let* ((path (format nil "~A/~A" out-dir (first pair)))
               (content (second pair))
               (content (if (and (> (length content) 0)
                                 (char/= (char content (1- (length content))) #\Newline))
                            (concatenate 'string content (string #\Newline))
                            content))
               (dir (directory-namestring path)))
          (ensure-directories-exist dir)
          (with-open-file (out path :direction :output :if-exists :supersede
                                    :external-format :utf-8)
            (write-string content out))))
      (length files))))

;; --- Main ---

;; Re-set function bindings after coder modules are loaded
(hydra-set-function-bindings)

(let ((coder-info (resolve-coder *target*))
      (target-cap (concatenate 'string
                    (string (char-upcase (char *target* 0)))
                    (subseq *target* 1))))
  (let ((coder (first coder-info))
        (language (second coder-info))
        (flags (third coder-info))
        (subdir (fourth coder-info)))

    (format t "==========================================~%")
    (format t "Mapping JSON to ~A (via Common Lisp host)~%" target-cap)
    (format t "==========================================~%")
    (force-output)

    ;; Load main modules
    (format t "~%Step 1: Loading main modules from JSON...~%")
    (force-output)
    (let* ((main-ns (coerce (read-manifest-field *json-dir* "mainModules") 'list))
           (eval-ns (coerce (read-manifest-field *json-dir* "evalLibModules") 'list))
           (all-ns (append main-ns eval-ns))
           (all-mods (load-modules-from-json *json-dir* all-ns))
           (total-bindings (reduce #'+ (mapcar (lambda (m)
                                                 (length (cdr (assoc :definitions m))))
                                               all-mods))))
      (format t "  Loaded ~A modules (~A bindings).~%" (length all-mods) total-bindings)
      (force-output)

      ;; Filter if needed
      (let* ((mods-to-generate
               (if *kernel-only*
                   (remove-if (lambda (m)
                                (let ((ns (cdr (assoc :namespace m))))
                                  (let ((ns-str (if (stringp ns) ns (cdr (assoc :value ns)))))
                                    (or (search "hydra.ext." ns-str)
                                        (search "hydra.json.yaml." ns-str)))))
                              all-mods)
                   all-mods))
             (out-main (format nil "~A/common-lisp-to-~A/src/gen-main/~A"
                               *output-base* *target* subdir)))

        (when *kernel-only*
          (format t "~%Filtering to kernel modules: ~A of ~A~%"
                  (length mods-to-generate) (length all-mods)))

        (format t "~%Mapping ~A modules to ~A...~%" (length mods-to-generate) target-cap)
        (format t "  Output: ~A~%" out-main)
        (force-output)

        (let ((file-count (generate-sources coder language flags out-main all-mods mods-to-generate)))
          (format t "  Generated ~A files.~%" file-count)
          (force-output)

          ;; Tests
          (when *include-tests*
            (format t "~%Loading test modules from JSON...~%")
            (force-output)
            (let* ((test-json-dir (substitute "*test*" "*main*" *json-dir*))
                   ;; Use gen-test JSON directory
                   (test-json-dir2 (let ((pos (search "gen-main" *json-dir*)))
                                     (if pos
                                         (concatenate 'string
                                           (subseq *json-dir* 0 pos)
                                           "gen-test"
                                           (subseq *json-dir* (+ pos 8)))
                                         *json-dir*)))
                   (test-ns (coerce (read-manifest-field *json-dir* "testModules") 'list))
                   (test-mods (load-modules-from-json test-json-dir2 test-ns))
                   (all-universe (append all-mods test-mods))
                   (out-test (format nil "~A/common-lisp-to-~A/src/gen-test/~A"
                                    *output-base* *target* subdir)))
              (format t "  Loaded ~A test modules.~%" (length test-mods))
              (format t "~%Mapping test modules to ~A...~%" target-cap)
              (force-output)
              (let ((test-count (generate-sources coder language flags out-test all-universe test-mods)))
                (format t "  Generated ~A test files.~%" test-count))))

          (format t "~%==========================================~%")
          (format t "Done: ~A main files~%" file-count)
          (format t "  Output: ~A/common-lisp-to-~A~%" *output-base* *target*)
          (format t "==========================================~%")
          (force-output))))))

(sb-ext:exit :code 0)
