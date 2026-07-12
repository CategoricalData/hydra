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

(define (string-replace-first str needle replacement)
  "Replace the first occurrence of NEEDLE in STR with REPLACEMENT (or return STR unchanged)."
  (let* ((nlen (string-length needle))
         (slen (string-length str))
         (pos (let loop ((i 0))
                (cond
                  ((> (+ i nlen) slen) #f)
                  ((equal? (substring str i (+ i nlen)) needle) i)
                  (else (loop (+ i 1)))))))
    (if pos
        (string-append (substring str 0 pos) replacement (substring str (+ pos nlen) slen))
        str)))

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

;; Determine the gen-main base directory: resolves to
;;   <worktree>/dist/scheme/hydra-kernel/src/main/scheme/hydra/
;; given that *script-dir* is
;;   <worktree>/heads/lisp/scheme/src/main/scheme/hydra/
;; (find "heads/lisp/scheme/" and replace with "dist/scheme/hydra-kernel/").
;; Defined here, immediately after *script-dir*, so the runtime loads below (loader.scm etc.,
;; migrated to dist by #434) can reference it.
(define *gen-main-base*
  (let* ((main-dir *script-dir*)
         (len (string-length main-dir))
         (needle "heads/lisp/scheme/")
         (nlen (string-length needle))
         (pos (let loop ((i 0))
                (cond
                  ((> (+ i nlen) len) #f)
                  ((equal? (substring main-dir i (+ i nlen)) needle) i)
                  (else (loop (+ i 1)))))))
    (if pos
        (string-append (substring main-dir 0 pos)
                       "dist/scheme/hydra-kernel/"
                       (substring main-dir (+ pos nlen)))
        ;; Fallback: legacy gen-main location
        (string-append main-dir "../../gen-main/scheme/hydra/"))))

;; Load the kernel loader. Use *gen-main-base* (dist), not *script-dir* (heads): #434 migrated the
;; runtime (loader.scm, json-reader.scm, prims.scm, lib/libraries.scm, scheme/lib/*) out of the head
;; into overlay → dist/scheme/hydra-kernel, leaving only this driver (bootstrap.scm) in the head.
(load (string-append *gen-main-base* "loader.scm"))
;; Re-import SRFI-9 to override (scheme base) define-record-type from loader
(use-modules (srfi srfi-9))
;; Import bitwise operations (arithmetic-shift etc.) needed by literals
(use-modules (srfi srfi-60))
;; Import vlist/vhash bindings used by lib/sets.scm and lib/maps.scm.
;; The bootstrap loader strips define-library wrappers and evaluates body
;; forms in (interaction-environment); without this, vlist-null, vhash-cons
;; etc. would be undefined when those library bodies run. CI's normal R7RS
;; load path picks up the imports from each library's own import block.
(use-modules (ice-9 vlist))
;; #473 redirect helpers: directory walk + whole-file read (Guile-specific; the driver already runs
;; under Guile — see usage banner + (ice-9 vlist) above).
(use-modules (ice-9 ftw) (ice-9 textual-ports))
;; Load the JSON reader (migrated to dist by #434 — see loader.scm note above)
(load (string-append *gen-main-base* "json-reader.scm"))

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


(display "Loading kernel...\n")
(force-output (current-output-port))

;; Load native libraries FIRST (gen-main modules reference hydra_lib_* symbols)
(display "  Loading native libraries...\n")
(force-output (current-output-port))
;; Load bytevector compatibility shim (provides snap-to-float32). Migrated to dist by #434 (see above).
(hydra-load-native-lib (string-append *gen-main-base* "../scheme/bytevector.sld"))
;; POSIX type modules (#494/#498) must load BEFORE the effectful impls below: the system/files impls
;; construct hydra.time.Timespec / hydra.file.* records at load time (e.g. get_time -> make-rec (hydra
;; time) make-hydra_time_timespec ...), so their record constructors must already be bound. These are
;; re-loaded (idempotently) as part of hydra-load-gen-main later; loading the leaf type modules early
;; here is harmless. Dependency order: time (leaf) -> file (imports hydra.time) -> system (imports file).
(for-each
  (lambda (m) (hydra-load-native-lib (string-append *gen-main-base* m)))
  '("core.scm" "time.scm" "file.scm" "system.scm"))
;; #473 Step 0 relocated the native lib impls from hydra/lib/ to hydra/scheme/lib/;
;; #501 relocated them again to hydra/overlay/scheme/lib/ (hydra.overlay.scheme.* namespace).
(for-each
  (lambda (f)
    (hydra-load-native-lib (string-append *gen-main-base* "overlay/scheme/lib/" f)))
  ;; All hydra.overlay.scheme.lib.* impls (the registry references every one's impl procedure).
  ;; Must include the effectful/newer libs (effects/files/system #494/#498, hashing, text) or the
  ;; registry fails with "Unbound variable: hydra_lib_<sub>_<fn>".
  '("chars.scm" "effects.scm" "eithers.scm" "equality.scm" "files.scm" "hashing.scm" "lists.scm"
    "literals.scm" "logic.scm" "maps.scm" "math.scm" "optionals.scm" "pairs.scm" "regex.scm"
    "sets.scm" "strings.scm" "system.scm" "text.scm"))

;; Load core modules via loader
(hydra-load-gen-main *gen-main-base*)

;; Load additional modules needed for bootstrapping (not in the basic loader set)
(define (load-additional-module filename)
  (let ((path (string-append *gen-main-base* filename)))
    (when (file-exists? path)
      (display (string-append "  Loading " filename "...\n"))
      (force-output (current-output-port))
      (hydra-load-file path))))

;; Load modules needed for JSON decode and code generation. Stale entries
;; (extract/helpers, tarjan, coder_utils, code_generation, show/meta) were
;; renamed/removed; current names are codegen.scm, etc. Missing files
;; silently no-op via (file-exists? path) check.
(for-each load-additional-module
  '("literals.scm"
    "reflect.scm"
    "languages.scm"
    "parsers.scm"
    "templates.scm"
    "encoding.scm"
    "decoding.scm"
    "extract/util.scm"
    "extract/json.scm"
    "adapt.scm"
    "codegen.scm"
    "analysis.scm"
    "dependencies.scm"
    "differentiation.scm"
    "environment.scm"
    "predicates.scm"
    "resolution.scm"
    "scoping.scm"
    "strip.scm"
    "variables.scm"
    ;; Show modules
    "show/core.scm" "show/error/core.scm" "show/errors.scm" "show/graph.scm"
    "show/typing.scm" "show/util.scm" "show/paths.scm" "show/variants.scm"
    ;; Validate
    "validate/core.scm" "validate/packaging.scm"
    ;; Decode modules (needed by hydra_decode_packaging_module)
    "decode/paths.scm" "decode/ast.scm" "decode/classes.scm"
    "decode/coders.scm" "decode/context.scm" "decode/core.scm"
    "decode/error/core.scm" "decode/error/checking.scm" "decode/errors.scm"
    "decode/json/model.scm"
    "decode/packaging.scm"
    "decode/parsing.scm" "decode/phantoms.scm" "decode/query.scm"
    "decode/relational.scm" "decode/tabular.scm" "decode/testing.scm"
    "decode/topology.scm" "decode/typing.scm" "decode/util.scm"
    "decode/variants.scm"
    ;; Encode modules (may be needed by code generation)
    "encode/paths.scm" "encode/ast.scm" "encode/classes.scm"
    "encode/coders.scm" "encode/context.scm" "encode/core.scm"
    "encode/error/core.scm" "encode/error/checking.scm" "encode/errors.scm"
    "encode/json/model.scm" "encode/packaging.scm"
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
(hydra-load-native-lib (string-append *gen-main-base* "prims.scm"))
;; #473: the registry (overlay/scheme/libraries.scm) references, per primitive, the PrimitiveDefinition
;; DATA as def:hydra_lib_<sub>_<fn>. Flat-load each generated hydra.lib.* def-module with its exports
;; renamed X -> def:X (hydra-load-def-module), reusing the flat-loaded kernel record constructors so the
;; registry's hydra_packaging_primitive_definition-name accessor accepts them. The `def:` prefix keeps
;; the DATA distinct from the impl PROCEDUREs (hydra_lib_<sub>_<fn>, loaded from overlay/scheme/lib/*).
(for-each
  (lambda (sub)
    (hydra-load-def-module (string-append *gen-main-base* "lib/" sub ".scm")))
  '("chars" "effects" "eithers" "equality" "files" "hashing" "lists" "literals" "logic" "maps"
    "math" "optionals" "pairs" "regex" "sets" "strings" "system" "text"))
(hydra-load-native-lib (string-append *gen-main-base* "overlay/scheme/libraries.scm"))

;; Load coder modules based on target. Coder modules use define-record-type
;; with field names where the accessor needs to be a first-class procedure
;; (e.g. (lambda (v) (record-accessor v))). Guile's built-in
;; define-record-type creates syntax-transformer accessors that fail when
;; used as first-class values. We use load-coder-module-simple, which
;; rewrites define-record-type forms into alist-based defines.
(define (load-coder-modules target)
  (let ((coder-files
          (cond
            ((equal? target "haskell")
             '("haskell/syntax.scm" "haskell/operators.scm"
               "haskell/language.scm" "haskell/environment.scm"
               "haskell/utils.scm"
               "haskell/serde.scm" "haskell/coder.scm"))
            ((equal? target "java")
             '("java/syntax.scm" "java/language.scm" "java/names.scm"
               "java/environment.scm" "java/utils.scm"
               "java/serde.scm" "java/coder.scm"))
            ((equal? target "python")
             '("python/syntax.scm" "python/language.scm" "python/names.scm"
               "python/environment.scm" "python/utils.scm"
               "python/serde.scm" "python/coder.scm"))
            ((or (equal? target "clojure") (equal? target "scheme")
                 (equal? target "common-lisp") (equal? target "emacs-lisp"))
             '("lisp/syntax.scm" "lisp/language.scm"
               "lisp/serde.scm" "lisp/coder.scm"))
            (else '()))))
    (for-each
      (lambda (f)
        (let ((path (string-append *gen-main-base* f)))
          (when (file-exists? path)
            (display (string-append "  Loading " f " (alist-rec)...\n"))
            (force-output (current-output-port))
            (load-coder-module-simple path))))
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
     ;; alist (JSON object); each entry becomes a 2-element Pair list
     ;; per the Hydra JSON model (Value.object = [Pair String Value]).
     (list 'object
           (map (lambda (pair)
                  (list (car pair) (scheme-to-hydra-json (cdr pair))))
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
               (ts (hydra_scoping_f_type_to_type_scheme typ))
               (stripped (hydra_strip_deannotate_type_recursive
                          (hydra_core_type_scheme-body ts))))
          (loop (cdr pairs) (cons (cons name stripped) result))))))

(define (namespace-to-path ns)
  ;; Convert "hydra.foo.bar" to "hydra/foo/bar" for the filesystem path.
  ;; The kernel function hydra_names_module_name_to_file_path requires a
  ;; case_conv + ext argument bundle; for our use (locating the JSON file
  ;; on disk, where path mirrors namespace exactly), we only need '.' -> '/'.
  (let ((len (string-length ns)))
    (let loop ((i 0) (result '()))
      (if (>= i len)
          (list->string (reverse result))
          (let ((c (string-ref ns i)))
            (loop (+ i 1) (cons (if (char=? c #\.) #\/ c) result)))))))

;; ============================================================================
;; Module loading from JSON
;; ============================================================================

(define (load-module-from-json bs-graph schema-map json-dir ns-str)
  (let* ((file-path (string-append json-dir "/" (namespace-to-path ns-str) ".json"))
         (json-obj (json-read-file file-path))
         (hydra-json (scheme-to-hydra-json json-obj))
         (mod-type (list 'variable "hydra.packaging.Module"))
         (json-result ((((hydra_json_decode_from_json schema-map)
                          "hydra.packaging.Module") mod-type) hydra-json)))
    (when (eq? (car json-result) 'left)
      (error (string-append "JSON decode error for " ns-str ": ")
             (cadr json-result)))
    (let* ((term (cadr json-result))
           (mod-result ((hydra_decode_packaging_module bs-graph) term)))
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
           (display (string-append "  Loading: " ns "\n"))
           (force-output (current-output-port))
           (let ((result (load-module-from-json bs-graph schema-map json-dir ns)))
             (display (string-append "  Loaded:  " ns "\n"))
             (force-output (current-output-port))
             result))
         namespaces)))

;; ============================================================================
;; Coder resolution
;; ============================================================================

(define (resolve-coder target)
  (cond
    ((equal? target "python")
     (list hydra_python_coder_module_to_python
           hydra_python_language_python_language
           (list #f #t #t #f)   ; flags: infer=f expand=t hoistCase=t hoistPoly=f
           "python"))
    ((equal? target "java")
     (list hydra_java_coder_module_to_java
           hydra_java_language_java_language
           (list #f #t #f #t)
           "java"))
    ((equal? target "haskell")
     (list hydra_haskell_coder_module_to_haskell
           hydra_haskell_language_haskell_language
           (list #f #f #f #f)
           "haskell"))
    ((or (equal? target "clojure") (equal? target "scheme")
         (equal? target "common-lisp") (equal? target "emacs-lisp"))
     ;; Lisp target uses Lisp coder with dialect parameter
     (let ((mtl hydra_lisp_coder_module_to_lisp)
           (pte hydra_lisp_serde_program_to_expr)
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
                                  (ns-val (let ((mn (hydra_packaging_module-name mod)))
                                            (if (string? mn) mn
                                                (hydra_packaging_module_name-value mn))))
                                  (ext (cond ((equal? target "clojure") "clj")
                                             ((equal? target "scheme") "scm")
                                             ((equal? target "common-lisp") "lisp")
                                             ((equal? target "emacs-lisp") "el")
                                             (else "scm")))
                                  (case-conv (if (equal? target "clojure")
                                                  (list 'camel '())
                                                  (list 'lower_snake '())))
                                  (fp (((hydra_names_module_name_to_file_path case-conv) ext) ns-val)))
                             (list 'right (list (cons fp code))))))))))
             hydra_lisp_language_lisp_language
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
  "Ensure the directory for a file path exists, creating parents as needed.
   Uses Guile's native `mkdir` component-by-component rather than shelling out
   to `mkdir -p`: on macOS `(system* \"mkdir\" ...)` resolving the bare name via
   PATH can crash with SIGSEGV during posix_spawn, silently leaving the deep
   directory (e.g. hydra/test/checking/) uncreated so the subsequent open-file
   fails. Native mkdir avoids the subprocess entirely and is fully portable."
  ;; Extract directory part (everything up to and including the last /).
  (let ((len (string-length path)))
    (let loop ((i (- len 1)))
      (cond
        ((< i 0) #t)
        ((char=? (string-ref path i) #\/)
         (mkdir-recursive (substring path 0 i)))
        (else (loop (- i 1)))))))

(define (mkdir-recursive dir)
  "Create DIR and all missing parent directories. No-op for existing dirs."
  (when (and (> (string-length dir) 0)
             (not (file-exists? dir)))
    ;; Recurse into the parent first, then create this level.
    (let ((slash (string-rindex dir #\/)))
      (when (and slash (> slash 0))
        (mkdir-recursive (substring dir 0 slash))))
    ;; Guard against a race where a sibling write created it meanwhile.
    (unless (file-exists? dir)
      (mkdir dir))))

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
  ;; Generate all modules in a single call. Splitting per-module breaks
  ;; cross-module variable resolution: the kernel computes transitive term
  ;; dependencies of mods-to-generate to build the data graph, and test
  ;; modules contain undeclared cross-references (e.g.
  ;; hydra.test.lib.chars references hydra.test.testGraph.testContext but
  ;; doesn't declare hydra.test.testGraph in dependencies). With the
  ;; vhash maps fix, single-call codegen is fast enough.
  ;; doExpand/doHoistCase/doHoistPoly are derived inside the kernel from
  ;; lang.supportedFeatures; only doInfer is passed in. Generation.hs's
  ;; type signature is stale (declares 4 Bools) but the body takes 1.
  (let* ((bs-graph (bootstrap-graph))
         (cx (make-hydra_typing_inference_context 0 '()))
         (do-infer (list-ref flags 0))
         (t0 (current-time-millis))
         (result (((((((hydra_codegen_generate_source_files
                          coder) language) do-infer)
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

;; #473 Step 0 — lib pass + redirect (mirrors the Java/Python/Scala/Clojure host drivers +
;; bootstrap-from-json/Main.hs). hydra.lib.* primitive IMPLEMENTATIONS were relocated to
;; (hydra scheme lib <sub>); (hydra lib <sub>) is free for the generated PrimitiveDefinition
;; def-modules. The Scheme host must (1) emit the (hydra lib <sub>) def-modules from their LOWERED form
;; (lib pass), and (2) redirect generated consumer imports (hydra lib <sub>) -> (hydra scheme lib <sub>)
;; so they resolve to the relocated impls. Flat call identifiers hydra_lib_<sub>_<fn> stay (resolved via
;; the relocated import's export). See project_473_self_host_lib_pass_gap.
(define lib-subs
  ;; Every hydra.lib.<sub> whose impls are relocated (#473). Must include the effectful/newer libs
  ;; (effects/files/system #494/#498, text) or their consumers keep calling the def-modules
  ;; ("'PrimitiveDefinition' object is not callable" for the python target). Excludes defaults.
  '("chars" "effects" "eithers" "equality" "files" "hashing" "lists" "literals" "logic" "maps"
    "math" "optionals" "pairs" "regex" "sets" "strings" "system" "text"))

(define (lib-module? m)
  (let* ((mn (hydra_packaging_module-name m))
         (ns (if (string? mn) mn (hydra_packaging_module_name-value mn))))
    (and (>= (string-length ns) 10)
         (string=? (substring ns 0 10) "hydra.lib."))))

;; Emit the (hydra lib <sub>) def-modules from their LOWERED form. Lib modules lowered; universe lowers
;; ONLY lib modules (so a lib default-implementation referencing another primitive resolves to it).
(define (run-lib-pass coder language flags out-main all-mods mods-to-generate)
  (let ((lib-mods (map hydra_codegen_lower_primitive_definitions
                       (filter lib-module? mods-to-generate))))
    (when (not (null? lib-mods))
      (let ((lib-universe (map (lambda (m)
                                 (if (lib-module? m)
                                     (hydra_codegen_lower_primitive_definitions m)
                                     m))
                               all-mods)))
        (display (string-append "Lib pass: emitting "
                                (number->string (length lib-mods))
                                " (hydra lib *) definition modules\n"))
        (force-output (current-output-port))
        (generate-sources coder language flags out-main lib-universe lib-mods)))))

;; string-replace-all: replace every occurrence of `from` with `to` in `s`.
(define (string-replace-all s from to)
  ;; Linear-time: accumulate output chunks in a list and concatenate once at the
  ;; end. The previous implementation rebuilt the accumulator with string-append
  ;; on every character, making each call O(n^2); with ~108 passes per file
  ;; (18 lib-subs x 6 delimiters) the redirect pass over the large generated
  ;; test modules (e.g. nominal_types.py ~250KB) spun for over an hour. Copying
  ;; unmatched runs in one substring keeps each call O(n).
  (let ((flen (string-length from))
        (slen (string-length s)))
    (if (= flen 0)
        s
        (let loop ((i 0) (run-start 0) (chunks '()))
          (cond
            ((> (+ i flen) slen)
             (string-concatenate
               (reverse (cons (substring s run-start slen) chunks))))
            ((string=? (substring s i (+ i flen)) from)
             ;; Flush the unmatched run [run-start, i), then emit the replacement.
             (loop (+ i flen)
                   (+ i flen)
                   (cons to (cons (substring s run-start i) chunks))))
            (else (loop (+ i 1) run-start chunks)))))))

;; Rewrite (hydra lib <sub>) -> (hydra scheme lib <sub>) in a consumer file. Primitive NAME strings are
;; dotted "hydra.lib..." (untouched by this space-form rewrite), so no protect/restore is needed.
(define (redirect-scheme s)
  (let loop ((subs lib-subs) (acc s))
    (if (null? subs)
        acc
        (loop (cdr subs)
              (string-replace-all acc
                                  (string-append "(hydra lib " (car subs) ")")
                                  (string-append "(hydra scheme lib " (car subs) ")"))))))

;; Files under hydra/lib/ are the lib-pass def-modules (must keep (hydra lib *)) and the hand-written
;; registry; never redirect these.
(define (lib-def-path? path)
  (let ((needle "/hydra/lib/")
        (plen (string-length path))
        (nlen 11))
    (let loop ((i 0))
      (cond
        ((> (+ i nlen) plen) #f)
        ((string=? (substring path i (+ i nlen)) needle) #t)
        (else (loop (+ i 1)))))))

;; Read an entire file as a string (Guile (ice-9 textual-ports)).
(define (read-file-content path)
  (call-with-input-file path (lambda (port) (get-string-all port))))

;; Does string s contain substring sub?
(define (string-contains-substr? s sub)
  (let ((slen (string-length s)) (blen (string-length sub)))
    (let loop ((i 0))
      (cond
        ((> (+ i blen) slen) #f)
        ((string=? (substring s i (+ i blen)) sub) #t)
        (else (loop (+ i 1)))))))

;; Recursively list regular files under dir (Guile (ice-9 ftw)).
(define (list-files-recursive dir)
  (let ((acc '()))
    (when (file-exists? dir)
      (ftw dir
           (lambda (filename statinfo flag)
             (when (eq? flag 'regular) (set! acc (cons filename acc)))
             #t)))
    acc))

;; Dotted-target (python) redirect: rewrite hydra.lib.<sub> -> hydra.overlay.python.lib.<sub> and
;; hydra.test.test_env -> hydra.overlay.python.test_env in the generated python. Mirrors the
;; Scala/Clojure/CL host fix; redirect-scheme handles only the scheme-target S-expr form.
(define (redirect-python-dotted s)
  ;; Match each delimiter that can follow the module name so both usages (hydra.lib.lists.cons) and
  ;; import lines (import hydra.lib.lists\n) get rewritten. Mirrors the Scala host's redirectDotted.
  ;;
  ;; CRITICAL: protect quoted primitive-NAME string literals. Primitives are registered under their
  ;; canonical name Name("hydra.lib.chars.isAlphaNum"); the runtime looks them up by that exact
  ;; string. Only the dotted CALL paths (hydra.overlay.python.lib.chars.is_alpha_num) get relocated —
  ;; the name literals must stay canonical, or the evaluator can't resolve the primitive and leaves
  ;; the application unreduced (e.g. "expected 'true' but got '(...isAlphaNum @ 97:int32)'"). Use the
  ;; Scala host's sentinel trick: stash `"hydra.lib.` (with the leading quote) before redirecting,
  ;; then restore it afterward. Mirrors Bootstrap.scala redirectDotted.
  (let* ((soh (string (integer->char 1)))
         (sentinel (string-append soh "HYDRA_LIB_NAME_LITERAL" soh))
         (protected (string-replace-all s "\"hydra.lib." (string-append "\"" sentinel)))
         (redirected
           (let loop ((subs lib-subs) (acc protected))
             (if (null? subs)
                 (string-replace-all acc "hydra.test.test_env" "hydra.overlay.python.test_env")
                 (let ((old (string-append "hydra.lib." (car subs)))
                       (new (string-append "hydra.overlay.python.lib." (car subs))))
                   (let dloop ((delims (list "." (string #\newline) " " ")" "," ":")) (a acc))
                     (if (null? delims)
                         (loop (cdr subs) a)
                         (dloop (cdr delims)
                                (string-replace-all a
                                                    (string-append old (car delims))
                                                    (string-append new (car delims)))))))))))
    (string-replace-all redirected sentinel "hydra.lib.")))

;; #473 redirect over a generated dir. For the scheme target, rewrite (hydra lib *) S-expr imports to
;; the relocated impl library; for the python target, rewrite the dotted hydra.lib.* + hydra.test.test_env
;; references to their hydra.overlay.python.* impls.
(define (redirect-lib-calls lang-dir)
  (for-each
    (lambda (path)
      (when (not (lib-def-path? path))
        (let ((s (read-file-content path)))
          (when (and s (or (string-contains-substr? s "(hydra lib ")
                           (string-contains-substr? s "hydra.lib.")
                           (string-contains-substr? s "hydra.test.test")))
            (let ((out (if (string=? *target* "python")
                           (redirect-python-dotted s)
                           (redirect-scheme s))))
              (when (not (string=? out s))
                (write-file-content path out)))))))
    (list-files-recursive lang-dir)))

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
    (let* ((all-ns (read-manifest-field *json-dir* "mainModules"))
           (all-mods (load-modules-from-json *json-dir* all-ns))
           (total-bindings (apply + (map (lambda (m)
                                           (let ((els (hydra_packaging_module-definitions m)))
                                             (if (list? els) (length els) 0)))
                                         all-mods))))
      (display (string-append "  Loaded " (number->string (length all-mods))
                              " modules (" (number->string total-bindings) " bindings).\n"))
      (force-output (current-output-port))

      ;; Filter modules. Note: in scheme's case the universe already comes from
      ;; manifest.mainModules (all the kernel + eval-lib namespaces). There is
      ;; no separate set of "coder packages" to
      ;; exclude here; --kernel-only is therefore a no-op and we generate the
      ;; entire loaded universe. (The Haskell host's bootstrap-from-json sees
      ;; both kernel and ext modules and uses --kernel-only to exclude ext.)
      (let* ((mods-to-generate all-mods)
             (out-main (string-append *output-base* "/scheme-to-" *target*
                                      "/src/main/" subdir)))

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

          ;; #473 lib pass: emit the (hydra lib *) def-modules now (redirect runs LAST, below).
          (when (not (string=? *target* "haskell"))
            (run-lib-pass coder language flags out-main all-mods mods-to-generate))

          ;; Tests
          (let ((test-count 0))
            (when *include-tests*
              (display "\nLoading test modules from JSON...\n")
              (force-output (current-output-port))
              (let* ((test-json-dir
                       ;; Derive the test-json dir from *json-dir*. The new
                       ;; per-source-set layout has main JSON at
                       ;;   <pkg>/src/main/json/...
                       ;; and test JSON at
                       ;;   <pkg>/src/test/json/... .
                       ;; Substitute "src/main/json" with "src/test/json".
                       (let* ((needle "src/main/json")
                              (replacement "src/test/json")
                              (nlen (string-length needle))
                              (jlen (string-length *json-dir*))
                              (pos (let loop ((i 0))
                                     (cond
                                       ((> (+ i nlen) jlen) #f)
                                       ((equal? (substring *json-dir* i (+ i nlen)) needle) i)
                                       (else (loop (+ i 1)))))))
                         (if pos
                             (string-append (substring *json-dir* 0 pos)
                                            replacement
                                            (substring *json-dir* (+ pos nlen) jlen))
                             *json-dir*)))
                     (test-ns (read-manifest-field *json-dir* "testModules"))
                     ;; #546/#547: the kernel test suite references hydra.test.build.* -> hydra.build.*
                     ;; (Option A). Load hydra-build's main modules into the universe and its OWN test
                     ;; modules (hydra.test.build.*, which live in the hydra-build package's test tree,
                     ;; not hydra-kernel's), else cross-host gen fails with
                     ;; "Unknown variable: hydra.test.build.modules.allTests". Mirrors the
                     ;; Java/Scala/Clojure/CL host fix (#553). Derive hydra-build's json dirs by
                     ;; substituting the package name in *json-dir* / test-json-dir.
                     (build-main-json-dir (string-replace-first *json-dir* "hydra-kernel" "hydra-build"))
                     (build-test-json-dir (string-replace-first test-json-dir "hydra-kernel" "hydra-build"))
                     (build-main-ns (read-manifest-field build-main-json-dir "mainModules"))
                     (build-main-mods (load-modules-from-json build-main-json-dir build-main-ns))
                     (build-test-ns (read-manifest-field build-main-json-dir "testModules"))
                     (build-test-mods (if (and build-test-ns (not (null? build-test-ns)))
                                          (load-modules-from-json build-test-json-dir build-test-ns)
                                          '()))
                     (test-mods (append (load-modules-from-json test-json-dir test-ns) build-test-mods))
                     (all-universe (append all-mods build-main-mods test-mods))
                     ;; Filter skip-emit test namespaces (e.g.
                     ;; hydra.test.testEnv): these are type-only stubs whose
                     ;; hand-written per-language counterparts are the source
                     ;; of truth. Mirrors testSkipEmitModuleNames in
                     ;; Hydra.Sources.Test.All and the equivalent filter in
                     ;; heads/python/.../bootstrap.py.
                     (test-mods-to-emit
                       (let loop ((ms test-mods) (out '()))
                         (cond
                           ((null? ms) (reverse out))
                           (else
                             (let* ((m (car ms))
                                    (mn (hydra_packaging_module-name m))
                                    (ns-str (if (string? mn) mn (hydra_packaging_module_name-value mn))))
                               (if (equal? ns-str "hydra.test.testEnv")
                                   (loop (cdr ms) out)
                                   (loop (cdr ms) (cons m out))))))))
                     (out-test (string-append *output-base* "/scheme-to-" *target*
                                              "/src/test/" subdir)))
                (display (string-append "  Loaded " (number->string (length test-mods))
                                        " test modules.\n"))
                (display (string-append "\nMapping test modules to " target-cap "...\n"))
                (force-output (current-output-port))
                (let* ((test-start (get-internal-real-time))
                       (tc (generate-sources coder language flags out-test all-universe test-mods-to-emit))
                       (test-elapsed (exact->inexact (/ (- (get-internal-real-time) test-start) internal-time-units-per-second))))
                  (set! test-count tc)
                  (display (string-append "  Generated " (number->string test-count)
                                          " test files.\n"))
                  (display (string-append "  Time: " (number->string (/ (round (* test-elapsed 10)) 10)) "s\n"))
                  (force-output (current-output-port)))))

            ;; #473 redirect — run LAST over every generated dir (main + test) so consumer imports
            ;; (hydra lib *) are rewritten to (hydra scheme lib *). Skips hydra/lib/ def-modules.
            (when (not (string=? *target* "haskell"))
              (redirect-lib-calls (string-append *output-base* "/scheme-to-" *target* "/src/main/" subdir))
              (when *include-tests*
                (redirect-lib-calls (string-append *output-base* "/scheme-to-" *target* "/src/test/" subdir))))

            (display "\n==========================================\n")
            (if (> test-count 0)
                (display (string-append "Done: " (number->string file-count) " main + "
                                        (number->string test-count) " test files\n"))
                (display (string-append "Done: " (number->string file-count) " main files\n")))
            (display (string-append "  Output: " *output-base* "/scheme-to-" *target* "\n"))
            (display "==========================================\n")
            (force-output (current-output-port))))))))

(exit 0)
