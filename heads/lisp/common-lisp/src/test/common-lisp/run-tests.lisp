;;; Hydra Common Lisp test entry point
;;;
;;; Usage: cd hydra-common-lisp && sbcl --load src/test/common-lisp/run-tests.lisp

(in-package :cl-user)

;; Ensure floats are read as double-precision by default.
;; Generated test data contains decimal literals like 3.141592653589793 without
;; a d0 suffix. Without this setting, SBCL reads them as single-float, losing precision.
(setf *read-default-float-format* 'double-float)

;; Disable IEEE-754 floating-point traps. macOS SBCL leaves these disabled by
;; default, so NaN- and overflow-producing operations return quiet NaN/Inf.
;; Linux SBCL leaves them enabled, so the same operations raise
;; FLOATING-POINT-INVALID-OPERATION at load time when generated test data
;; contains expressions like (sqrt -1) or (log 0). Hydra's IEEE-754 semantics
;; require quiet NaN/Inf, so we explicitly disable the traps here.
#+sbcl
(sb-int:set-floating-point-modes :traps nil)

;; Load cl-ppcre for regex support. Required by the regex test suite —
;; fail loudly if it cannot be loaded, since silently skipping would let
;; regex tests run with broken primitives.
;;
;; Search path priority:
;;   1. ~/quicklisp/dists/quicklisp/software/  (developer/local installs)
;;   2. /usr/share/common-lisp/source/         (Debian/Ubuntu apt package)
(require :asdf)
(let ((trees nil)
      (ql-software (merge-pathnames "quicklisp/dists/quicklisp/software/"
                                    (user-homedir-pathname)))
      (system-cl  #P"/usr/share/common-lisp/source/"))
  (when (probe-file ql-software) (push ql-software trees))
  (when (probe-file system-cl)   (push system-cl trees))
  (when trees
    (funcall (intern "INITIALIZE-SOURCE-REGISTRY" :asdf)
             `(:source-registry
                ,@(mapcar (lambda (d) `(:tree ,d)) trees)
                :inherit-configuration))))
(funcall (intern "LOAD-SYSTEM" :asdf) :cl-ppcre :verbose nil)

;; Determine the base directory (hydra-common-lisp/)
;; *hydra-cl-head* = heads/lisp/common-lisp/ (hand-written sources)
(defvar *hydra-cl-head*
  (make-pathname :directory (butlast (pathname-directory *load-truename*) 3)
                 :defaults *load-truename*))

;; *hydra-cl-pkg* = packages/hydra-lisp/hydra-common-lisp/ (generated content)
;; This is the cwd when invoked from the shared run-tests.sh runner.
(defvar *hydra-cl-pkg* *default-pathname-defaults*)

(defun hydra-path (relative)
  "Resolve a path relative to the package directory (generated content)."
  (merge-pathnames relative *hydra-cl-pkg*))

(defun hydra-head-path (relative)
  "Resolve a path relative to the head directory (hand-written content)."
  (merge-pathnames relative *hydra-cl-head*))

;; ============================================================================
;; 1. Load prelude (alist-based struct system)
;; ============================================================================
(format t "~%Loading prelude...~%")
(load (hydra-head-path "src/main/common-lisp/hydra/prelude.lisp"))

;; ============================================================================
;; 2. Load native primitive libraries
;; ============================================================================
(format t "Loading native libraries...~%")
(dolist (f '("lib/equality.lisp"
             "lib/maps.lisp"
             "lib/sets.lisp"
             "lib/lists.lisp"
             "lib/strings.lisp"
             "lib/logic.lisp"
             "lib/math.lisp"
             "lib/chars.lisp"
             "lib/eithers.lisp"
             "lib/literals.lisp"
             "lib/maybes.lisp"
             "lib/pairs.lisp"
             "lib/regex.lisp"))
  (load (hydra-head-path (concatenate 'string "src/main/common-lisp/hydra/" f))))

;; ============================================================================
;; 3. Load the loader + generated main modules
;; ============================================================================
(format t "Loading generated main modules...~%")
(load (hydra-head-path "src/main/common-lisp/hydra/loader.lisp"))

;; Override *hydra-gen-main-dir* to point at the dist/ generated content.
;; *hydra-cl-head* = heads/lisp/common-lisp/, so ../../../dist = repo-root/dist
(setf *hydra-gen-main-dir*
      (merge-pathnames "../../../dist/common-lisp/hydra-kernel/src/main/common-lisp/hydra/"
                       *hydra-cl-head*))

;; Set function bindings for native library defvars (e.g. hydra_lib_maps_singleton)
;; so they can be called in function position in generated code.
(hydra-set-function-bindings)

(hydra-load-gen-main)

;; Set function bindings again for generated kernel defvars
(hydra-set-function-bindings)

;; ============================================================================
;; 4. Load primitive infrastructure and test runner
;; ============================================================================
(format t "Loading primitive infrastructure...~%")
(load (hydra-head-path "src/main/common-lisp/hydra/prims.lisp"))
(load (hydra-head-path "src/main/common-lisp/hydra/lib/libraries.lisp"))

(format t "Loading test runner...~%")
(load (hydra-head-path "src/test/common-lisp/hydra/test_runner.lisp"))

;; ============================================================================
;; 5. Load generated test data files in dependency order
;; ============================================================================
(format t "Loading generated test data...~%")

(defvar *test-data-base*
  (merge-pathnames "../../../dist/common-lisp/hydra-kernel/src/test/common-lisp/hydra/test/"
                   *hydra-cl-head*))

(defun load-test-file (relative)
  (let ((path (merge-pathnames relative *test-data-base*)))
    (when (probe-file path)
      (hydra-load-file path))))

;; Load test dependency modules (needed by serialization, sorting tests)
(let ((ext-path (merge-pathnames "../../../dist/common-lisp/hydra-kernel/src/main/common-lisp/hydra/haskell/operators.lisp"
                                 *hydra-cl-head*)))
  (when (probe-file ext-path)
    (hydra-load-file ext-path)))

;; Base test types/terms (dependencies first)
(load-test-file "test_types.lisp")
(load-test-file "test_terms.lisp")
(load (hydra-head-path "src/test/common-lisp/hydra/annotation_bindings.lisp"))
(load-test-file "test_graph.lisp")

;; Note: test_graph.lisp is patched by sync-lisp.sh to build a full graph
;; with primitives, annotation bindings, and schema types at load time.

;; Library tests
(dolist (f '("lib/chars.lisp"
             "lib/eithers.lisp"
             "lib/equality.lisp"
             "lib/lists.lisp"
             "lib/literals.lisp"
             "lib/logic.lisp"
             "lib/maps.lisp"
             "lib/math.lisp"
             "lib/maybes.lisp"
             "lib/pairs.lisp"
             "lib/regex.lisp"
             "lib/sets.lisp"
             "lib/strings.lisp"))
  (load-test-file f))

;; Annotation tests
(load-test-file "annotations.lisp")

;; Checking tests
(dolist (f '("checking/fundamentals.lisp"
             "checking/collections.lisp"
             "checking/algebraic_types.lisp"
             "checking/nominal_types.lisp"
             "checking/advanced.lisp"
             "checking/failures.lisp"
             "checking/all.lisp"))
  (load-test-file f))

;; Other test categories
(load-test-file "eta_expansion.lisp")
(load-test-file "formatting.lisp")

;; Hoisting tests
(dolist (f '("hoisting/cases.lisp"
             "hoisting/let.lisp"
             "hoisting/all.lisp"))
  (load-test-file f))

;; Inference tests
(dolist (f '("inference/fundamentals.lisp"
             "inference/algebraic_types.lisp"
             "inference/nominal_types.lisp"
             "inference/algorithm_w.lisp"
             "inference/classes.lisp"
             "inference/failures.lisp"
             "inference/kernel_examples.lisp"
             "inference/all.lisp"))
  (load-test-file f))

;; JSON tests
(dolist (f '("json/coder.lisp"
             "json/parser.lisp"
             "json/roundtrip.lisp"
             "json/writer.lisp"
             "json/yaml.lisp"))
  (load-test-file f))

;; Remaining test categories
(load-test-file "dependencies.lisp")
(load-test-file "differentiation.lisp")
(load-test-file "generation.lisp")
(load-test-file "reduction.lisp")
(load-test-file "rewriting.lisp")
(load-test-file "serialization.lisp")
(load-test-file "sorting.lisp")
(load-test-file "strip.lisp")
(load-test-file "substitution.lisp")
(load-test-file "unification.lisp")

;; Validate tests
(dolist (f '("validate/core.lisp"
             "validate/all.lisp"))
  (load-test-file f))

;; Variables tests
(load-test-file "variables.lisp")

;; Top-level test suite (must be last)
(load-test-file "test_suite.lisp")

;; ============================================================================
;; 6. Run tests
;; ============================================================================
(format t "~%Running tests...~%~%")

(defvar *test-t0* (get-internal-real-time))
(defvar *results* (run-test-group "" hydra_test_test_suite_all_tests))
(defvar *total-ms* (* 1000.0 (/ (- (get-internal-real-time) *test-t0*) internal-time-units-per-second)))

(let* ((pass (first *results*))
       (fail (second *results*))
       (skip (third *results*))
       (benchmark (fourth *results*)))
  (format t "~%========================================~%")
  (format t "Pass: ~A~%" pass)
  (format t "Fail: ~A~%" fail)
  (format t "Skip: ~A~%" skip)
  (when benchmark
    (write-benchmark-json
      (list (cons :path (cdr (assoc :path benchmark)))
            (cons :passed pass)
            (cons :failed fail)
            (cons :skipped skip)
            (cons :total-time-ms (cdr (assoc :total-time-ms benchmark)))
            (cons :subgroups (cdr (assoc :subgroups benchmark))))
      *total-ms*))
  (let ((exit-code (if (> fail 0) 1 0)))
    #+sbcl (sb-ext:exit :code exit-code)
    #+ccl (ccl:quit exit-code)
    #+ecl (ext:quit exit-code)
    #+clisp (ext:exit exit-code)
    #-(or sbcl ccl ecl clisp) (cl-user::quit exit-code)))
