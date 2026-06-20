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

;; Ensure floats are read as double-precision by default.
;; Generated code contains decimal literals like 1.0 without a d0 suffix.
;; Without this setting, SBCL reads them as single-float, losing precision.
(setf *read-default-float-format* 'double-float)

;; Disable IEEE-754 floating-point traps so NaN- and overflow-producing
;; operations return quiet NaN/Inf instead of raising. Linux SBCL (and
;; native arm64 macOS SBCL 2.6+) leaves these enabled by default, so the
;; same operations raise FLOATING-POINT-INVALID-OPERATION when the kernel
;; evaluates expressions like (sqrt -1) or (log 0) during test generation.
;; Hydra's IEEE-754 semantics require quiet NaN/Inf.
#+sbcl (sb-int:set-floating-point-modes :traps nil)

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
       (hydra-dir (make-pathname :directory (pathname-directory script-path)))
       ;; #434 migrated the runtime (loader.lisp, json-reader.lisp, gen-main content) out of the head
       ;; into overlay -> dist/common-lisp/hydra-kernel, leaving only this driver (bootstrap.lisp) in
       ;; the head. Load the runtime from dist, not hydra-dir (heads, now empty).
       (dist-hydra-dir (merge-pathnames
                         "../../../../../../../dist/common-lisp/hydra-kernel/src/main/common-lisp/hydra/"
                         hydra-dir)))
  ;; Load the kernel loader
  (load (merge-pathnames "loader.lisp" dist-hydra-dir))
  ;; Load JSON reader
  (load (merge-pathnames "json-reader.lisp" dist-hydra-dir))
  ;; Point *hydra-gen-main-dir* at the dist/ generated content (loader.lisp defaults to
  ;; heads/lisp/common-lisp/src/gen-main/..., which no longer exists; mirrors run-tests.lisp).
  (setf *hydra-gen-main-dir* dist-hydra-dir))

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
     ;; alist (JSON object); each entry becomes a 2-element Pair list
     ;; per the Hydra JSON model (Value.object = [Pair String Value]).
     (list :object
           (mapcar (lambda (pair)
                     (list (car pair) (cl-to-hydra-json (cdr pair))))
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
             (ts (funcall (symbol-value 'hydra_scoping_f_type_to_type_scheme) typ))
             (stripped (funcall (symbol-value 'hydra_strip_deannotate_type_recursive)
                               (funcall 'hydra_core_type_scheme-body ts))))
        (push (cons name stripped) result)))
    (nreverse result)))

(defun namespace-to-path (ns)
  (funcall (symbol-value 'hydra_codegen_module_name_to_path) ns))

;; --- Module loading from JSON ---

(defun load-module-from-json (bs-graph schema-map ns-str &optional (json-dir *json-dir*))
  (let* ((file-path (format nil "~A/~A.json" json-dir (namespace-to-path ns-str)))
         (json-obj (json-read-file file-path))
         (hydra-json (cl-to-hydra-json json-obj))
         (mod-type (list :variable "hydra.packaging.Module"))
         (json-result (funcall (funcall (funcall (funcall
                        (symbol-value 'hydra_json_decode_from_json) schema-map)
                        "hydra.packaging.Module") mod-type) hydra-json)))
    (when (eq (first json-result) :left)
      (error "JSON decode error for ~A: ~A" ns-str (second json-result)))
    (let* ((term (second json-result))
           (mod-result (funcall (funcall
                         (symbol-value 'hydra_decode_packaging_module) bs-graph) term)))
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

;; --- Per-package loading (baseline + coders, mirroring Scala/Python hosts) ---

(defun ends-with-p (s suffix)
  (let ((ls (length s)) (lf (length suffix)))
    (and (>= ls lf) (string= (subseq s (- ls lf)) suffix))))

(defun dist-json-root-of (json-dir)
  "Strip a legacy <pkg>/src/main/json suffix to recover the dist/json root.
   If json-dir does not match the expected shape, return it unchanged."
  (let ((trimmed (string-right-trim "/" json-dir)))
    (if (ends-with-p trimmed "/src/main/json")
        (let* ((without-suffix (subseq trimmed 0 (- (length trimmed)
                                                   (length "/src/main/json"))))
               (slash (position #\/ without-suffix :from-end t)))
          (if slash (subseq without-suffix 0 slash) "."))
        trimmed)))

(defun package-main-dir (root pkg)
  (format nil "~A/~A/src/main/json" root pkg))

(defun read-manifest-field-or-empty (pkg-dir field-name)
  (let ((manifest-path (format nil "~A/manifest.json" pkg-dir)))
    (if (probe-file manifest-path)
        (coerce (read-manifest-field pkg-dir field-name) 'list)
        nil)))

(defun load-package-main (root pkg)
  "Load mainModules + defaultLibModules for the given package under dist/json/<pkg>."
  (let* ((pkg-dir (package-main-dir root pkg))
         (main-ns (read-manifest-field-or-empty pkg-dir "mainModules"))
         (default-ns (read-manifest-field-or-empty pkg-dir "defaultLibModules"))
         (all-ns (append main-ns default-ns)))
    (when all-ns
      (format t "  ~A: ~A modules from ~A~%" pkg (length all-ns) pkg-dir)
      (force-output)
      (load-modules-from-json pkg-dir all-ns))))

;; --- Coder resolution ---

(defun resolve-coder (target)
  (cond
    ((string= target "python")
     (list (symbol-value 'hydra_python_coder_module_to_python)
           (symbol-value 'hydra_python_language_python_language)
           (list nil t t nil)  ; flags: infer=f expand=t hoistCase=t hoistPoly=f
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
    ((string= target "scala")
     (list (symbol-value 'hydra_scala_coder_module_to_scala)
           (symbol-value 'hydra_scala_language_scala_language)
           (list nil nil nil nil)
           "scala"))
    ((string= target "typescript")
     (list (symbol-value 'hydra_type_script_coder_module_to_type_script)
           (symbol-value 'hydra_type_script_language_type_script_language)
           (list nil t t nil)
           "ts"))
    ((or (string= target "clojure") (string= target "scheme")
         (string= target "common-lisp") (string= target "emacs-lisp"))
     ;; Lisp targets use Lisp coder with dialect parameter
     (let* ((dialect-map '(("clojure" . :clojure) ("scheme" . :scheme)
                           ("common-lisp" . :common_lisp) ("emacs-lisp" . :emacs_lisp)))
            (ext-map '(("clojure" . ".clj") ("scheme" . ".scm")
                       ("common-lisp" . ".lisp") ("emacs-lisp" . ".el")))
            (dialect (cdr (assoc target dialect-map :test #'string=)))
            (ext (cdr (assoc target ext-map :test #'string=)))
            (mtl (symbol-value 'hydra_lisp_coder_module_to_lisp))
            (pte (symbol-value 'hydra_lisp_serde_program_to_expr)))
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
                                  (ns-val (let ((mn (hydra_packaging_module-name mod)))
                                            (if (stringp mn) mn (hydra_packaging_module_name-value mn))))
                                  (case-conv (if (string= target "clojure")
                                                 (list :camel nil)
                                                 (list :lower_snake nil)))
                                  (fp (funcall (funcall (funcall
                                        (symbol-value 'hydra_names_module_name_to_file_path)
                                        case-conv)
                                        (subseq ext 1))  ; ".lisp" -> "lisp"
                                        ns-val)))
                             (list :right (list (cons fp code))))))))))
             (symbol-value 'hydra_lisp_language_lisp_language)
             (list nil nil nil nil)
             target)))
    (t (error "Unsupported target: ~A" target))))

;; --- Code generation ---

(defun generate-sources (coder language flags out-dir universe-mods mods-to-generate)
  (let* ((bs-graph (bootstrap-graph))
         (cx (funcall 'make-hydra_typing_inference_context 0 nil))
         (do-infer (first flags))
         (t0 (get-internal-real-time))
         (result (handler-case
                   (funcall (funcall (funcall (funcall (funcall (funcall (funcall
                       (symbol-value 'hydra_codegen_generate_source_files)
                       coder) language) do-infer)
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
      (let ((written 0))
        (dolist (pair files)
          (let* ((path (format nil "~A/~A" out-dir (first pair)))
                 (content (second pair)))
            (when (and content (stringp content) (> (length content) 0))
              (let* ((content (if (char/= (char content (1- (length content))) #\Newline)
                                  (concatenate 'string content (string #\Newline))
                                  content))
                     (dir (directory-namestring path)))
                (ensure-directories-exist dir)
                (with-open-file (out path :direction :output :if-exists :supersede
                                          :external-format :utf-8)
                  (write-string content out))
                (incf written)))))
        (when (< written (length files))
          (format t "  WARNING: ~A of ~A files had empty content~%" (- (length files) written) (length files))
          (force-output)))
      (length files))))

;; #473 Step 0 — lib pass + redirect (mirrors the other host drivers + bootstrap-from-json/Main.hs).
;; Common Lisp is flat-namespace: native primitive IMPLEMENTATIONS were relocated to symbols
;; hydra_lisp_lib_<sub>_<fn> (in :cl-user); the def-modules ship as real packages :hydra.lib.<sub>
;; exporting the bare def symbols hydra_lib_<sub>_<fn> (PrimitiveDefinition values). The CL host must
;; (1) emit the :hydra.lib.<sub> def-modules from their LOWERED form (lib pass), and (2) in generated
;; consumers, rename call sites hydra_lib_<sub>_ -> hydra_lisp_lib_<sub>_ (hit the relocated impls) and
;; DROP the ":hydra.lib.<sub>" token from defpackage (:use ...) clauses (so a consumer doesn't import the
;; def-module DATA over the impl). Primitive NAME strings are dotted "hydra.lib..." and untouched by the
;; underscore rename. See project_473_self_host_lib_pass_gap.
(defparameter *lib-subs*
  '("chars" "eithers" "equality" "lists" "literals" "logic" "maps"
    "math" "optionals" "pairs" "regex" "sets" "strings"))

(defun lib-module-p (m)
  (let* ((mn (funcall 'hydra_packaging_module-name m))
         (ns (if (stringp mn) mn (funcall 'hydra_packaging_module_name-value mn))))
    (and (>= (length ns) 10) (string= (subseq ns 0 10) "hydra.lib."))))

(defun run-lib-pass (coder language flags out-main all-mods mods-to-generate)
  "Emit the :hydra.lib.<sub> def-modules from their LOWERED form (lib modules lowered; universe lowers
   ONLY lib modules)."
  (let* ((lower (symbol-value 'hydra_codegen_lower_primitive_definitions))
         (lib-mods (mapcar lower (remove-if-not #'lib-module-p mods-to-generate))))
    (when lib-mods
      (let ((lib-universe (mapcar (lambda (m) (if (lib-module-p m) (funcall lower m) m)) all-mods)))
        (format t "Lib pass: emitting ~A :hydra.lib.* definition modules~%" (length lib-mods))
        (force-output)
        (generate-sources coder language flags out-main lib-universe lib-mods)))))

(defun string-replace-all (s from to)
  "Replace every occurrence of FROM with TO in S."
  (let ((flen (length from)))
    (with-output-to-string (out)
      (let ((i 0) (slen (length s)))
        (loop
          (let ((pos (search from s :start2 i)))
            (cond
              ((null pos) (write-string s out :start i) (return))
              (t (write-string s out :start i :end pos)
                 (write-string to out)
                 (setf i (+ pos flen))))))))))

(defun read-file-string (path)
  (with-open-file (in path :direction :input :external-format :utf-8 :if-does-not-exist nil)
    (when in
      (let ((s (make-string (file-length in))))
        (let ((n (read-sequence s in))) (subseq s 0 n))))))

(defun write-file-string (path content)
  (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string content out)))

(defun lib-def-path-p (path)
  "Files under hydra/lib/ are the lib-pass def-modules + hand-written registry; never redirect them."
  (search "/hydra/lib/" (namestring path)))

(defun redirect-cl (s)
  "Rename consumer call sites + drop the def-module :use token, per *lib-subs*."
  (let ((out s))
    (dolist (sub *lib-subs* out)
      (setf out (string-replace-all out
                                    (concatenate 'string "hydra_lib_" sub "_")
                                    (concatenate 'string "hydra_lisp_lib_" sub "_")))
      ;; Drop the ":hydra.lib.<sub>" token from (:use ...) — appears on its own line.
      (setf out (string-replace-all out
                                    (concatenate 'string (string #\Newline) ":hydra.lib." sub)
                                    (string #\Newline))))))

(defun redirect-lib-calls (lang-dir)
  "#473 redirect over a generated dir (Common Lisp flat-namespace form)."
  (dolist (path (directory (merge-pathnames "**/*.*" (pathname (concatenate 'string lang-dir "/")))))
    (when (and (pathname-name path) (not (lib-def-path-p path)))
      (let ((s (read-file-string path)))
        (when (and s (or (search "hydra_lib_" s) (search ":hydra.lib." s)))
          (let ((out (redirect-cl s)))
            (when (string/= out s) (write-file-string path out))))))))

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

    ;; Load baseline packages (hydra-kernel + hydra-haskell), mirroring the
    ;; Scala and Python hosts. The hydra-haskell package supplies the runtime
    ;; AST modules (Hydra.Haskell.Syntax, .Environment, etc.) that the
    ;; generated DSL source modules import. Without it the Haskell rebuild
    ;; step fails on dangling imports.
    (format t "~%Step 1: Loading baseline main modules from JSON...~%")
    (force-output)
    (let* ((dist-json-root (dist-json-root-of *json-dir*))
           (kernel-mods (load-package-main dist-json-root "hydra-kernel"))
           (haskell-mods (load-package-main dist-json-root "hydra-haskell"))
           (all-mods (append kernel-mods haskell-mods))
           (total-bindings (reduce #'+ (mapcar (lambda (m)
                                                 (length (cdr (assoc :definitions m))))
                                               all-mods)))
           (kernel-ns-set (mapcar (lambda (m)
                                    (let ((ns (cdr (assoc :namespace m))))
                                      (if (stringp ns) ns (cdr (assoc :value ns)))))
                                  all-mods)))
      (format t "  Loaded ~A baseline modules (~A bindings).~%" (length all-mods) total-bindings)
      (force-output)

      ;; Filter if needed
      (let* ((mods-to-generate
               (if *kernel-only*
                   (remove-if-not (lambda (m)
                                    (let ((ns (cdr (assoc :namespace m))))
                                      (let ((ns-str (if (stringp ns) ns (cdr (assoc :value ns)))))
                                        (member ns-str kernel-ns-set :test #'string=))))
                                  all-mods)
                   all-mods))
             (out-main (format nil "~A/common-lisp-to-~A/src/main/~A"
                               *output-base* *target* subdir)))

        (when *kernel-only*
          (format t "~%Filtering to kernel modules: ~A of ~A~%"
                  (length mods-to-generate) (length all-mods)))

        (format t "~%Mapping ~A modules to ~A...~%" (length mods-to-generate) target-cap)
        (format t "  Output: ~A~%" out-main)
        (force-output)

        (let ((test-file-count 0)
              (main-start (get-internal-real-time)))
          (let ((file-count (generate-sources coder language flags out-main all-mods mods-to-generate)))
            (let ((main-secs (/ (- (get-internal-real-time) main-start) internal-time-units-per-second 1.0)))
              (format t "  Generated ~A files.~%" file-count)
              (format t "  Time: ~,1Fs~%" main-secs)
              (force-output)

              ;; #473 lib pass: emit the :hydra.lib.* def-modules now (redirect runs LAST, below).
              (unless (string= *target* "haskell")
                (run-lib-pass coder language flags out-main all-mods mods-to-generate))

              ;; Tests
              (when *include-tests*
                (format t "~%Loading test modules from JSON...~%")
                (force-output)
                (let* ((test-json-dir2 (let ((pos (search "src/main/json" *json-dir*)))
                                         ;; New per-source-set layout: main JSON at <pkg>/src/main/json,
                                         ;; test JSON at <pkg>/src/test/json.
                                         (if pos
                                             (concatenate 'string
                                               (subseq *json-dir* 0 pos)
                                               "src/test/json"
                                               (subseq *json-dir* (+ pos 13)))
                                             *json-dir*)))
                       (test-ns (coerce (read-manifest-field *json-dir* "testModules") 'list))
                       (test-mods (load-modules-from-json test-json-dir2 test-ns))
                       (all-universe (append all-mods test-mods))
                       ;; Filter skip-emit test namespaces (e.g.
                       ;; hydra.test.testEnv): these are type-only stubs whose
                       ;; hand-written per-language counterparts are the
                       ;; source of truth. Mirrors testSkipEmitModuleNames in
                       ;; Hydra.Sources.Test.All and the equivalent filter in
                       ;; heads/python/.../bootstrap.py.
                       (test-mods-to-emit
                         (remove-if
                           (lambda (m)
                             (let* ((ns (hydra_packaging_module-name m))
                                    (ns-str (if (stringp ns) ns
                                                (hydra_packaging_module_name-value ns))))
                               (string= ns-str "hydra.test.testEnv")))
                           test-mods))
                       (out-test (format nil "~A/common-lisp-to-~A/src/test/~A"
                                        *output-base* *target* subdir))
                       (test-start (get-internal-real-time)))
                  (format t "  Loaded ~A test modules.~%" (length test-mods))
                  (format t "~%Mapping test modules to ~A...~%" target-cap)
                  (force-output)
                  (setf test-file-count (generate-sources coder language flags out-test all-universe test-mods-to-emit))
                  (let ((test-secs (/ (- (get-internal-real-time) test-start) internal-time-units-per-second 1.0)))
                    (format t "  Generated ~A test files.~%" test-file-count)
                    (format t "  Time: ~,1Fs~%" test-secs))))

              ;; #473 redirect — run LAST over every generated dir (main + test) so consumer call sites
              ;; hydra_lib_<sub>_ are renamed to hydra_lisp_lib_<sub>_ and the def-module :use token dropped.
              (unless (string= *target* "haskell")
                (redirect-lib-calls (format nil "~A/common-lisp-to-~A/src/main/~A"
                                            *output-base* *target* subdir))
                (when *include-tests*
                  (redirect-lib-calls (format nil "~A/common-lisp-to-~A/src/test/~A"
                                              *output-base* *target* subdir))))

              (format t "~%==========================================~%")
              (format t "Done: ~A main + ~A test files~%" file-count test-file-count)
              (format t "  Output: ~A/common-lisp-to-~A~%" *output-base* *target*)
              (format t "==========================================~%")
              (force-output))))))))

(sb-ext:exit :code 0)
