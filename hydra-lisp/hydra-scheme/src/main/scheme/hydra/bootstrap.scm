;;; Hydra Scheme bootstrap
;;; Loads Hydra modules from JSON and generates code for a target language.
;;;
;;; Usage: guile --no-auto-compile -s bootstrap.scm -- --target <target> --json-dir <path> [OPTIONS]
;;;
;;; Options:
;;;   --target <lang>       Target language (haskell|java|python|clojure|scheme)
;;;   --json-dir <path>     Path to kernel JSON modules
;;;   --output <dir>        Output base directory (default: /tmp/hydra-bootstrapping-demo)
;;;   --kernel-only         Only generate kernel modules
;;;   --include-tests       Also generate test modules

;; IMPORTANT: Import SRFI-9 BEFORE (scheme base) to ensure define-record-type
;; creates procedure accessors (not Guile's syntax-transformer accessors).
(use-modules (srfi srfi-9))
(import (scheme read) (scheme write) (scheme file)
        (scheme process-context) (scheme char) (scheme time))

;; ============================================================================
;; Portable helpers
;; ============================================================================

(define (string-prefix? prefix str)
  "Check if str starts with prefix."
  (let ((plen (string-length prefix))
        (slen (string-length str)))
    (and (>= slen plen)
         (equal? prefix (substring str 0 plen)))))

;; ============================================================================
;; Command-line argument parsing
;; ============================================================================

(define *bootstrap-args*
  (let ((args (command-line)))
    (let ((after-sep (let loop ((rest (cdr args)))
                       (cond
                         ((null? rest) #f)
                         ((equal? (car rest) "--") (cdr rest))
                         (else (loop (cdr rest)))))))
      (or after-sep (cdr args)))))

(define (get-arg name . default)
  (let loop ((rest *bootstrap-args*))
    (cond
      ((null? rest) (if (pair? default) (car default) #f))
      ((equal? (car rest) name)
       (if (pair? (cdr rest)) (cadr rest) #f))
      (else (loop (cdr rest))))))

(define (has-flag name)
  (let loop ((rest *bootstrap-args*))
    (cond
      ((null? rest) #f)
      ((equal? (car rest) name) #t)
      (else (loop (cdr rest))))))

(define *target* (get-arg "--target"))
(define *json-dir* (get-arg "--json-dir"))
(define *output-base* (get-arg "--output" "/tmp/hydra-bootstrapping-demo"))
(define *kernel-only* (has-flag "--kernel-only"))
(define *include-tests* (has-flag "--include-tests"))

(unless (and *target* *json-dir*)
  (display "Usage: guile --no-auto-compile -s bootstrap.scm -- --target <target> --json-dir <path> [OPTIONS]\n")
  (exit 1))

;; ============================================================================
;; Load the Hydra Scheme kernel
;; ============================================================================

;; Determine the directory containing this script (absolute path)
(define *script-dir*
  (let ((args (command-line)))
    (if (pair? args)
        (let* ((script (car args))
               ;; Find last /
               (len (string-length script))
               (last-slash (let loop ((i (- len 1)))
                             (cond
                               ((< i 0) #f)
                               ((char=? (string-ref script i) #\/) i)
                               (else (loop (- i 1)))))))
          (let ((dir (if last-slash
                         (substring script 0 (+ last-slash 1))
                         "./")))
            ;; Make absolute if relative
            (if (char=? (string-ref dir 0) #\/)
                dir
                (string-append (getcwd) "/" dir))))
        (string-append (getcwd) "/"))))

;; Load the kernel loader
(load (string-append *script-dir* "loader.scm"))
;; Re-import SRFI-9 to override (scheme base) define-record-type from loader
(use-modules (srfi srfi-9))
;; Import bitwise operations (arithmetic-shift etc.) needed by literals
(use-modules (srfi srfi-60))
;; Load the JSON reader
(load (string-append *script-dir* "json-reader.scm"))

;; Convert a define-record-type form to alist-based constructor/accessors.
;; Guile's built-in define-record-type creates syntax-transformer accessors
;; that can't be used as first-class values in generated code.
(define (record-type-to-alist-defines form)
  "Convert (define-record-type Name (make-Name f1 f2...) pred? (f1 acc1) (f2 acc2)...)
   to a list of define forms creating alist-based records."
  (let* ((type-name (list-ref form 1))
         (constructor-form (list-ref form 2))
         (constructor-name (if (pair? constructor-form) (car constructor-form) constructor-form))
         (constructor-fields (if (pair? constructor-form) (cdr constructor-form) '()))
         (predicate (list-ref form 3))
         (field-specs (list-tail form 4))
         (field-keywords (map (lambda (f) (symbol->keyword f)) constructor-fields)))
    (append
      ;; Constructor
      (list `(define ,constructor-name
               (lambda ,constructor-fields
                 (list ,@(map (lambda (kw field) `(cons ,kw ,field))
                              field-keywords constructor-fields)))))
      ;; Predicate
      (list `(define ,predicate (lambda (x) (and (pair? x) (pair? (car x))))))
      ;; Accessors
      (map (lambda (spec)
             (let ((field-name (if (pair? spec) (car spec) spec))
                   (accessor-name (if (and (pair? spec) (>= (length spec) 2))
                                      (cadr spec) spec)))
               `(define ,accessor-name
                  (lambda (rec)
                    (cdr (assq ,(symbol->keyword field-name) rec))))))
           field-specs))))

;; Simple coder module loader: strips define-library, converts record types
;; to alist-based definitions. Avoids sanitize/rewrite transformations
;; (which fail on record types with field names like 'cond', 'then', 'else')
;; and avoids Guile's syntax-transformer accessors.
(define (load-coder-module-simple path)
  "Load a generated coder module with alist-based records."
  (when (file-exists? path)
    (call-with-input-file path
      (lambda (port)
        (let loop ()
          (let ((form (read port)))
            (unless (eof-object? form)
              (cond
                ;; Strip define-library: extract and eval (begin ...) body
                ((and (pair? form) (eq? (car form) 'define-library))
                 (let find-begin ((rest (cddr form)))
                   (cond
                     ((null? rest) #t)
                     ((and (pair? (car rest)) (eq? (caar rest) 'begin))
                      (let* ((body-forms (cdar rest))
                             ;; Transform define-record-type to alist defines
                             (expanded (apply append
                               (map (lambda (f)
                                      (if (and (pair? f) (eq? (car f) 'define-record-type)
                                               (>= (length f) 4))
                                          (record-type-to-alist-defines f)
                                          (list f)))
                                    body-forms))))
                        ;; Eval with retry for forward refs
                        (hydra-eval-with-retry expanded)))
                     (else (find-begin (cdr rest))))))
                ;; Non-library forms
                (else (eval form (interaction-environment))))
              (loop))))))))

;; Determine the gen-main base directory
(define *gen-main-base*
  (let* ((main-dir *script-dir*)
         ;; Go from src/main/scheme/hydra/ up to src/ then into gen-main/scheme/hydra/
         (len (string-length main-dir))
         ;; Find "src/main/" and replace with "src/gen-main/"
         (pos (let loop ((i 0))
                (cond
                  ((>= i (- len 8)) #f)
                  ((and (char=? (string-ref main-dir i) #\s)
                        (>= (- len i) 9)
                        (equal? (substring main-dir i (+ i 9)) "src/main/"))
                   i)
                  (else (loop (+ i 1)))))))
    (if pos
        (string-append (substring main-dir 0 pos) "src/gen-main/"
                       (substring main-dir (+ pos 9)))
        ;; Fallback: relative path
        (string-append main-dir "../../gen-main/scheme/hydra/"))))

(display "Loading kernel...\n")
(force-output (current-output-port))

;; Load native libraries FIRST (gen-main modules reference hydra_lib_* symbols)
(display "  Loading native libraries...\n")
(force-output (current-output-port))
;; Load bytevector compatibility shim (provides snap-to-float32)
(hydra-load-native-lib (string-append *script-dir* "../scheme/bytevector.sld"))
(for-each
  (lambda (f)
    (hydra-load-native-lib (string-append *script-dir* "lib/" f)))
  '("equality.scm" "maps.scm" "sets.scm" "lists.scm" "strings.scm"
    "logic.scm" "math.scm" "chars.scm" "eithers.scm" "literals.scm"
    "maybes.scm" "pairs.scm" "regex.scm"))

;; Load core modules via loader
(hydra-load-gen-main *gen-main-base*)

;; Load additional modules needed for bootstrapping (not in the basic loader set)
(define (load-additional-module filename)
  (let ((path (string-append *gen-main-base* filename)))
    (when (file-exists? path)
      (display (string-append "  Loading " filename "...\n"))
      (force-output (current-output-port))
      (hydra-load-file path))))

;; Load modules needed for JSON decode and code generation
(for-each load-additional-module
  '("literals.scm"
    "reflect.scm"
    "languages.scm"
    "parsers.scm"
    "templates.scm"
    "encoding.scm"
    "decoding.scm"
    "extract/helpers.scm"
    "extract/util.scm"
    "extract/json.scm"
    "tarjan.scm"
    "coder_utils.scm"
    "adapt.scm"
    "code_generation.scm"
    ;; Show modules
    "show/core.scm" "show/error/core.scm" "show/errors.scm" "show/graph.scm"
    "show/meta.scm" "show/typing.scm" "show/util.scm" "show/paths.scm"
    ;; Validate
    "validate/core.scm"
    ;; Decode modules (needed by hydra_decode_module_module)
    "decode/paths.scm" "decode/ast.scm" "decode/classes.scm"
    "decode/coders.scm" "decode/context.scm" "decode/core.scm"
    "decode/error/core.scm" "decode/error/checking.scm" "decode/errors.scm"
    "decode/graph.scm" "decode/json/model.scm"
    "decode/module.scm"
    "decode/parsing.scm" "decode/phantoms.scm" "decode/query.scm"
    "decode/relational.scm" "decode/tabular.scm" "decode/testing.scm"
    "decode/topology.scm" "decode/typing.scm" "decode/util.scm"
    "decode/variants.scm"
    ;; Encode modules (may be needed by code generation)
    "encode/paths.scm" "encode/ast.scm" "encode/classes.scm"
    "encode/coders.scm" "encode/context.scm" "encode/core.scm"
    "encode/error/core.scm" "encode/error/checking.scm" "encode/errors.scm"
    "encode/json/model.scm" "encode/module.scm"
    "encode/parsing.scm" "encode/phantoms.scm" "encode/query.scm"
    "encode/relational.scm" "encode/tabular.scm" "encode/testing.scm"
    "encode/topology.scm" "encode/typing.scm" "encode/util.scm"
    "encode/variants.scm"
    "json/parser.scm"
    "json/writer.scm"
    "json/encode.scm"
    "json/decode.scm"
    "json/bootstrap.scm"))

;; Load prims and libraries
(display "  Loading prims...\n")
(force-output (current-output-port))
(hydra-load-native-lib (string-append *script-dir* "prims.scm"))
(hydra-load-native-lib (string-append *script-dir* "lib/libraries.scm"))

;; Load coder modules based on target
(define (load-coder-modules target)
  (let ((coder-files
          (cond
            ((equal? target "haskell")
             '("ext/haskell/syntax.scm" "ext/haskell/operators.scm"
               "ext/haskell/language.scm" "ext/haskell/utils.scm"
               "ext/haskell/serde.scm" "ext/haskell/coder.scm"))
            ((equal? target "java")
             '("ext/java/syntax.scm" "ext/java/language.scm" "ext/java/names.scm"
               "ext/java/environment.scm" "ext/java/utils.scm"
               "ext/java/serde.scm" "ext/java/coder.scm"))
            ((equal? target "python")
             '("ext/python/syntax.scm" "ext/python/language.scm" "ext/python/names.scm"
               "ext/python/environment.scm" "ext/python/utils.scm"
               "ext/python/serde.scm" "ext/python/coder.scm"))
            ((or (equal? target "clojure") (equal? target "scheme")
                 (equal? target "common-lisp") (equal? target "emacs-lisp"))
             '("ext/lisp/syntax.scm" "ext/lisp/language.scm"
               "ext/lisp/serde.scm" "ext/lisp/coder.scm"))
            (else '()))))
    (for-each
      (lambda (f)
        (load-additional-module f))
      coder-files)))

(display "  Loading coder modules...\n")
(force-output (current-output-port))

(load-coder-modules *target*)


(display "Kernel loaded.\n")
(force-output (current-output-port))

;; ============================================================================
;; JSON to Hydra conversion
;; ============================================================================

(define (scheme-to-hydra-json obj)
  "Convert Scheme JSON value (from json-reader) to Hydra JSON value (list-based tagged union)."
  (cond
    ((eq? obj 'json-null) (list 'null '()))
    ((eq? obj #t) (list 'boolean #t))
    ((eq? obj 'json-false) (list 'boolean #f))
    ((eq? obj 'json-empty-object) (list 'object '()))
    ((number? obj) (list 'number (inexact obj)))
    ((string? obj) (list 'string obj))
    ((null? obj) (list 'array '()))  ;; empty JSON arrays
    ((and (pair? obj) (pair? (car obj)) (string? (caar obj)))
     ;; alist (JSON object)
     (list 'object
           (map (lambda (pair)
                  (cons (car pair) (scheme-to-hydra-json (cdr pair))))
                obj)))
    ((list? obj)
     ;; list (JSON array)
     (list 'array (map scheme-to-hydra-json obj)))
    (else (error "Unexpected JSON value type" obj))))

;; ============================================================================
;; Bootstrap graph construction
;; ============================================================================

(define (bootstrap-graph)
  (make-hydra_graph_graph '() '() '() '() '()
    (standard-library)
    '() '()))

(define (bootstrap-schema-map)
  (let loop ((pairs hydra_json_bootstrap_types_by_name) (result '()))
    (if (null? pairs)
        (reverse result)
        (let* ((pair (car pairs))
               (name (car pair))
               (typ (cdr pair))
               (ts (hydra_rewriting_f_type_to_type_scheme typ))
               (stripped (hydra_rewriting_deannotate_type_recursive
                          (hydra_core_type_scheme-type ts))))
          (loop (cdr pairs) (cons (cons name stripped) result))))))

(define (namespace-to-path ns)
  (hydra_code_generation_namespace_to_path ns))

;; ============================================================================
;; Module loading from JSON
;; ============================================================================

(define (load-module-from-json bs-graph schema-map json-dir ns-str)
  (let* ((file-path (string-append json-dir "/" (namespace-to-path ns-str) ".json"))
         (json-obj (json-read-file file-path))
         (hydra-json (scheme-to-hydra-json json-obj))
         (mod-type (list 'variable "hydra.module.Module"))
         (json-result ((((hydra_json_decode_from_json schema-map)
                          "hydra.module.Module") mod-type) hydra-json)))
    (when (eq? (car json-result) 'left)
      (error (string-append "JSON decode error for " ns-str ": ")
             (cadr json-result)))
    (let* ((term (cadr json-result))
           (mod-result ((hydra_decode_module_module bs-graph) term)))
      (when (eq? (car mod-result) 'left)
        (error (string-append "Module decode error for " ns-str ": ")
               (cadr mod-result)))
      (cadr mod-result))))

(define (read-manifest-field json-dir field-name)
  (let* ((manifest-path (string-append json-dir "/manifest.json"))
         (manifest (json-read-file manifest-path)))
    (let loop ((entries manifest))
      (cond
        ((null? entries) '())
        ((equal? (caar entries) field-name) (cdar entries))
        (else (loop (cdr entries)))))))

(define (load-modules-from-json json-dir namespaces)
  (let ((bs-graph (bootstrap-graph))
        (schema-map (bootstrap-schema-map)))
    (map (lambda (ns)
           (display (string-append "  Loaded: " ns "\n"))
           (force-output (current-output-port))
           (load-module-from-json bs-graph schema-map json-dir ns))
         namespaces)))

;; ============================================================================
;; Coder resolution
;; ============================================================================

(define (resolve-coder target)
  (cond
    ((equal? target "python")
     (list hydra_ext_python_coder_module_to_python
           hydra_ext_python_language_python_language
           (list #f #t #t #f)   ; flags: infer=f expand=t hoistCase=t hoistPoly=f
           "python"))
    ((equal? target "java")
     (list hydra_ext_java_coder_module_to_java
           hydra_ext_java_language_java_language
           (list #f #t #f #t)
           "java"))
    ((equal? target "haskell")
     (list hydra_ext_haskell_coder_module_to_haskell
           hydra_ext_haskell_language_haskell_language
           (list #f #f #f #f)
           "haskell"))
    ((or (equal? target "clojure") (equal? target "scheme")
         (equal? target "common-lisp") (equal? target "emacs-lisp"))
     ;; Lisp target uses Lisp coder with dialect parameter
     (let ((mtl hydra_ext_lisp_coder_module_to_lisp)
           (pte hydra_ext_lisp_serde_program_to_expr)
           (dialect (cond ((equal? target "clojure") (list 'clojure '()))
                          ((equal? target "scheme") (list 'scheme '()))
                          ((equal? target "common-lisp") (list 'common_lisp '()))
                          ((equal? target "emacs-lisp") (list 'emacs_lisp '())))))
       (list (lambda (mod)
               (lambda (defs)
                 (lambda (cx)
                   (lambda (g)
                     (let ((result (((((mtl dialect) mod) defs) cx) g)))
                       (if (eq? (car result) 'left)
                           result
                           (let* ((program (cadr result))
                                  (code (hydra_serialization_print_expr
                                          (hydra_serialization_parenthesize
                                            (pte program))))
                                  (ns-val (let ((ns (hydra_module_module-namespace mod)))
                                            (if (string? ns) ns
                                                (hydra_module_namespace-value ns))))
                                  (ext (cond ((equal? target "clojure") "clj")
                                             ((equal? target "scheme") "scm")
                                             ((equal? target "common-lisp") "lisp")
                                             ((equal? target "emacs-lisp") "el")
                                             (else "scm")))
                                  (case-conv (if (equal? target "clojure")
                                                  (list 'camel '())
                                                  (list 'lower_snake '())))
                                  (fp (((hydra_names_namespace_to_file_path case-conv) ext) ns-val)))
                             (list 'right (list (cons fp code))))))))))
             hydra_ext_lisp_language_lisp_language
             (list #f #f #f #f)
             (cond ((equal? target "clojure") "clojure")
                   ((equal? target "scheme") "scheme")
                   ((equal? target "common-lisp") "common-lisp")
                   ((equal? target "emacs-lisp") "emacs-lisp")))))
    (else (error "Unsupported target" target))))

;; ============================================================================
;; Code generation
;; ============================================================================

(define (ensure-directory-exists path)
  "Ensure the directory for a file path exists."
  ;; Extract directory part (everything up to last /)
  (let ((len (string-length path)))
    (let loop ((i (- len 1)))
      (cond
        ((< i 0) #t)
        ((char=? (string-ref path i) #\/)
         (let ((dir (substring path 0 (+ i 1))))
           (system* "mkdir" "-p" dir)))
        (else (loop (- i 1)))))))

(define (write-file-content path content)
  "Write content to a file, ensuring directory exists."
  (ensure-directory-exists path)
  (let ((final-content
          (if (and (> (string-length content) 0)
                   (not (char=? (string-ref content (- (string-length content) 1)) #\newline)))
              (string-append content "\n")
              content)))
    (call-with-output-file path
      (lambda (port)
        (display final-content port)))))

(define (format-time millis)
  (cond
    ((< millis 1000) (string-append (number->string (exact (truncate millis))) "ms"))
    ((< millis 60000) (string-append (number->string (/ (truncate (* millis 0.1)) 100.0)) "s"))
    (else (let ((mins (exact (truncate (/ millis 60000))))
                (secs (/ (truncate (/ (exact (truncate (modulo millis 60000))) 10)) 100.0)))
            (string-append (number->string mins) "m " (number->string secs) "s")))))

(define (current-time-millis)
  (exact (truncate (* (current-jiffy) 1000.0 (/ 1.0 (jiffies-per-second))))))

(define (generate-sources coder language flags out-dir universe-mods mods-to-generate)
  (let* ((bs-graph (bootstrap-graph))
         (cx (make-hydra_context_in_context '() '()))
         (do-infer (list-ref flags 0))
         (do-expand (list-ref flags 1))
         (do-hoist-case (list-ref flags 2))
         (do-hoist-poly (list-ref flags 3))
         (t0 (current-time-millis))
         (result ((((((((((hydra_code_generation_generate_source_files
                            coder) language) do-infer) do-expand) do-hoist-case) do-hoist-poly)
                       bs-graph) universe-mods) mods-to-generate) cx)))
    (when (eq? (car result) 'left)
      (error "Code generation failed" (cadr result)))
    (let* ((files (cadr result))
           (t1 (current-time-millis)))
        (display (string-append "  Code generation took "
                                (format-time (- t1 t0))
                                " for " (number->string (length files)) " files\n"))
        (force-output (current-output-port))
        (for-each
          (lambda (file-pair)
            (let* ((path (string-append out-dir "/" (car file-pair)))
                   (content (cadr file-pair)))
              (write-file-content path content)))
          files)
        (length files))))

;; ============================================================================
;; Main
;; ============================================================================

(let* ((coder-info (resolve-coder *target*))
       (target-cap (string-append
                     (string (char-upcase (string-ref *target* 0)))
                     (substring *target* 1 (string-length *target*)))))
  (let ((coder (list-ref coder-info 0))
        (language (list-ref coder-info 1))
        (flags (list-ref coder-info 2))
        (subdir (list-ref coder-info 3)))

    (display "==========================================\n")
    (display (string-append "Mapping JSON to " target-cap " (via Scheme host)\n"))
    (display "==========================================\n")
    (force-output (current-output-port))

    ;; Load main modules
    (display "\nStep 1: Loading main modules from JSON...\n")
    (force-output (current-output-port))
    (let* ((main-ns (read-manifest-field *json-dir* "mainModules"))
           (eval-ns (read-manifest-field *json-dir* "evalLibModules"))
           (all-ns (append main-ns eval-ns))
           (all-mods (load-modules-from-json *json-dir* all-ns))
           (total-bindings (apply + (map (lambda (m)
                                           (let ((els (hydra_module_module-definitions m)))
                                             (if (list? els) (length els) 0)))
                                         all-mods))))
      (display (string-append "  Loaded " (number->string (length all-mods))
                              " modules (" (number->string total-bindings) " bindings).\n"))
      (force-output (current-output-port))

      ;; Filter if needed
      (let* ((mods-to-generate
               (if *kernel-only*
                   (let ((filtered '()))
                     (for-each
                       (lambda (m)
                         (let* ((ns (hydra_module_module-namespace m))
                                (ns-str (if (string? ns) ns
                                            (if (hydra_module_namespace? ns)
                                                (hydra_module_namespace-value ns)
                                                ns))))
                           (unless (or (string-prefix? "hydra.ext." ns-str)
                                       (string-prefix? "hydra.json.yaml." ns-str))
                             (set! filtered (cons m filtered)))))
                       all-mods)
                     (reverse filtered))
                   all-mods))
             (out-main (string-append *output-base* "/scheme-to-" *target*
                                      "/src/gen-main/" subdir)))

        (when *kernel-only*
          (display (string-append "\nFiltering to kernel modules: "
                                  (number->string (length mods-to-generate))
                                  " of " (number->string (length all-mods)) "\n")))

        (display (string-append "\nMapping " (number->string (length mods-to-generate))
                                " modules to " target-cap "...\n"))
        (display (string-append "  Output: " out-main "\n"))
        (force-output (current-output-port))

        (let* ((main-start (get-internal-real-time))
               (file-count (generate-sources coder language flags out-main all-mods mods-to-generate))
               (main-elapsed (exact->inexact (/ (- (get-internal-real-time) main-start) internal-time-units-per-second))))
          (display (string-append "  Generated " (number->string file-count) " files.\n"))
          (display (string-append "  Time: " (number->string (/ (round (* main-elapsed 10)) 10)) "s\n"))
          (force-output (current-output-port))

          ;; Tests
          (let ((test-count 0))
            (when *include-tests*
              (display "\nLoading test modules from JSON...\n")
              (force-output (current-output-port))
              (let* ((test-json-dir
                       ;; Replace gen-main with gen-test in json-dir
                       (let ((pos (let loop ((i 0))
                                    (cond
                                      ((>= i (- (string-length *json-dir*) 8)) #f)
                                      ((equal? (substring *json-dir* i (+ i 8)) "gen-main") i)
                                      (else (loop (+ i 1)))))))
                         (if pos
                             (string-append (substring *json-dir* 0 pos)
                                            "gen-test"
                                            (substring *json-dir* (+ pos 8)
                                                       (string-length *json-dir*)))
                             *json-dir*)))
                     (test-ns (read-manifest-field *json-dir* "testModules"))
                     (test-mods (load-modules-from-json test-json-dir test-ns))
                     (all-universe (append all-mods test-mods))
                     (out-test (string-append *output-base* "/scheme-to-" *target*
                                              "/src/gen-test/" subdir)))
                (display (string-append "  Loaded " (number->string (length test-mods))
                                        " test modules.\n"))
                (display (string-append "\nMapping test modules to " target-cap "...\n"))
                (force-output (current-output-port))
                (let* ((test-start (get-internal-real-time))
                       (tc (generate-sources coder language flags out-test all-universe test-mods))
                       (test-elapsed (exact->inexact (/ (- (get-internal-real-time) test-start) internal-time-units-per-second))))
                  (set! test-count tc)
                  (display (string-append "  Generated " (number->string test-count)
                                          " test files.\n"))
                  (display (string-append "  Time: " (number->string (/ (round (* test-elapsed 10)) 10)) "s\n"))
                  (force-output (current-output-port)))))

            (display "\n==========================================\n")
            (if (> test-count 0)
                (display (string-append "Done: " (number->string file-count) " main + "
                                        (number->string test-count) " test files\n"))
                (display (string-append "Done: " (number->string file-count) " main files\n")))
            (display (string-append "  Output: " *output-base* "/scheme-to-" *target* "\n"))
            (display "==========================================\n")
            (force-output (current-output-port))))))))

(exit 0)
