;;; Hydra Common Lisp test entry point
;;;
;;; Usage: cd hydra-common-lisp && sbcl --load src/test/common-lisp/run-tests.lisp

(in-package :cl-user)

;; Ensure floats are read as double-precision by default.
;; Generated test data contains decimal literals like 3.141592653589793 without
;; a d0 suffix. Without this setting, SBCL reads them as single-float, losing precision.
(setf *read-default-float-format* 'double-float)

;; Load cl-ppcre for regex support.
;; Try loading via ASDF directly (avoids quicklisp cache permission issues).
(handler-case
  (progn
    (require :asdf)
    ;; Register the quicklisp software directory so ASDF can find cl-ppcre
    (let ((ql-software (merge-pathnames "quicklisp/dists/quicklisp/software/" (user-homedir-pathname))))
      (when (probe-file ql-software)
        (funcall (intern "INITIALIZE-SOURCE-REGISTRY" :asdf)
                 `(:source-registry (:tree ,ql-software) :inherit-configuration))))
    (funcall (intern "LOAD-SYSTEM" :asdf) :cl-ppcre :verbose nil))
  (error (e) (format t "Warning: Could not load cl-ppcre: ~A~%" e)))

;; Determine the base directory (hydra-common-lisp/)
(defvar *hydra-cl-base*
  (make-pathname :directory (butlast (pathname-directory *load-truename*) 3)
                 :defaults *load-truename*))

(defun hydra-path (relative)
  "Resolve a path relative to the hydra-common-lisp/ directory."
  (merge-pathnames relative *hydra-cl-base*))

;; ============================================================================
;; 1. Load prelude (alist-based struct system)
;; ============================================================================
(format t "~%Loading prelude...~%")
(load (hydra-path "src/main/common-lisp/hydra/prelude.lisp"))

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
  (load (hydra-path (concatenate 'string "src/main/common-lisp/hydra/" f))))

;; ============================================================================
;; 3. Load the loader + generated main modules
;; ============================================================================
(format t "Loading generated main modules...~%")
(load (hydra-path "src/main/common-lisp/hydra/loader.lisp"))

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
(load (hydra-path "src/main/common-lisp/hydra/prims.lisp"))
(load (hydra-path "src/main/common-lisp/hydra/lib/libraries.lisp"))

(format t "Loading test runner...~%")
(load (hydra-path "src/test/common-lisp/hydra/test_runner.lisp"))

;; ============================================================================
;; 5. Load generated test data files in dependency order
;; ============================================================================
(format t "Loading generated test data...~%")

(defvar *test-data-base* (hydra-path "src/gen-test/common-lisp/hydra/test/"))

(defun load-test-file (relative)
  (let ((path (merge-pathnames relative *test-data-base*)))
    (when (probe-file path)
      (hydra-load-file path))))

;; Load test dependency modules (needed by serialization, sorting tests)
(let ((ext-path (hydra-path "src/gen-main/common-lisp/hydra/ext/haskell/operators.lisp")))
  (when (probe-file ext-path)
    (hydra-load-file ext-path)))

;; Base test types/terms (dependencies first)
(load-test-file "test_types.lisp")
(load-test-file "test_terms.lisp")
(load-test-file "test_graph.lisp")

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
             "json/writer.lisp"))
  (load-test-file f))

;; Remaining test categories
(load-test-file "reduction.lisp")
(load-test-file "rewriting.lisp")
(load-test-file "serialization.lisp")
(load-test-file "sorting.lisp")
(load-test-file "substitution.lisp")
(load-test-file "unification.lisp")

;; Top-level test suite (must be last)
(load-test-file "test_suite.lisp")

;; ============================================================================
;; 6. Run tests
;; ============================================================================
(format t "~%Running tests...~%~%")

;; If the full test suite isn't bound, build it dynamically from whatever loaded
(unless (boundp 'hydra_test_test_suite_all_tests)
  (format t "Building test suite dynamically from loaded test modules...~%")
  (let ((subgroups nil))
    (dolist (name '(hydra_test_lib_chars_all_tests
                    hydra_test_lib_eithers_all_tests
                    hydra_test_lib_equality_all_tests
                    hydra_test_lib_lists_all_tests
                    hydra_test_lib_literals_all_tests
                    hydra_test_lib_logic_all_tests
                    hydra_test_lib_maps_all_tests
                    hydra_test_lib_math_all_tests
                    hydra_test_lib_maybes_all_tests
                    hydra_test_lib_pairs_all_tests
                    hydra_test_lib_regex_all_tests
                    hydra_test_lib_sets_all_tests
                    hydra_test_lib_strings_all_tests
                    hydra_test_annotations_all_tests
                    hydra_test_checking_all_all_tests
                    hydra_test_eta_expansion_all_tests
                    hydra_test_formatting_all_tests
                    hydra_test_hoisting_all_all_tests
                    hydra_test_inference_all_all_tests
                    hydra_test_json_coder_all_tests
                    hydra_test_json_parser_all_tests
                    hydra_test_json_roundtrip_all_tests
                    hydra_test_json_writer_all_tests
                    hydra_test_reduction_all_tests
                    hydra_test_rewriting_all_tests
                    hydra_test_serialization_all_tests
                    hydra_test_sorting_all_tests
                    hydra_test_substitution_all_tests
                    hydra_test_unification_all_tests))
      (when (boundp name)
        (push (symbol-value name) subgroups)))
    (setf subgroups (nreverse subgroups))
    (format t "  ~A of 28 test groups loaded~%" (length subgroups))
    (defvar hydra_test_test_suite_all_tests
      (make-test_group "common" nil subgroups nil))))

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
