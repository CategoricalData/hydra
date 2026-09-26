;; #729: the module-grammar rename reordered kernel module names so the package root
;; ("core") comes before the category segment, e.g. (hydra graph) -> (hydra core graph)
;; and (hydra print core) -> (hydra core print model) (the "core" submodule of each
;; subsystem was itself renamed to "model", matching hydra.core -> hydra.core.model).
;; The (hydra overlay scheme ...) imports are unaffected -- overlay modules keep the
;; hydra.overlay.<lang>.* namespace (#501), not hydra.core.overlay.<lang>.*.
(import (scheme base) (scheme write) (scheme cxr) (scheme char)
        (scheme time) (scheme process-context) (scheme file)
        (scheme bytevector)
        (only (guile) mkdir rmdir opendir readdir closedir)  ; #494: directory ops for effectful temp-dir prep
        (hydra core model) (hydra core graph) (hydra core prims)
        (hydra core reduction) (hydra core rewriting) (hydra core print model) (hydra core testing)
        (hydra core formatting) (hydra core sorting) (hydra core serialization)
        (hydra core inference) (hydra core checking) (hydra core hoisting)
        (hydra core unification) (hydra core substitution) (hydra core typing)
        (hydra core dependencies) (hydra core strip) (hydra core variables)
        (hydra core validate model)
        (hydra core json bootstrap) (hydra core json parser) (hydra core json writer)
        (hydra core json encode) (hydra core json decode)
        (hydra core json yaml encode) (hydra core json yaml decode)
        (hydra core encode model)
        (hydra overlay scheme libraries) (hydra overlay scheme lib equality) (hydra overlay scheme lib maps)
        (hydra overlay scheme lib optionals) (hydra overlay scheme lib pairs) (hydra overlay scheme lib sets) (hydra overlay scheme lib lists)
        (hydra overlay scheme lib literals)
        (hydra core test test_graph) (hydra core test test_suite))

(include "src/test/scheme/hydra/test_runner_body.scm")

(let* ((t0 (current-jiffy))
       (results (run-test-group "" hydra_core_test_test_suite_all_tests))
       (total-ms (* 1000.0 (/ (- (current-jiffy) t0) (jiffies-per-second))))
       (pass (car results))
       (fail (cadr results))
       (skip (caddr results))
       (benchmark (if (>= (length results) 4) (list-ref results 3) #f)))
  (display pass) (display " passed, ")
  (display fail) (display " failed, ")
  (display skip) (display " skipped")
  (newline)
  (when benchmark
    (write-benchmark-json
      (list (cons 'path (cdr (assq 'path benchmark)))
            (cons 'passed pass)
            (cons 'failed fail)
            (cons 'skipped skip)
            (cons 'totalTimeMs (cdr (assq 'totalTimeMs benchmark)))
            (cons 'subgroups (cdr (assq 'subgroups benchmark))))
      total-ms))
  (exit (if (> fail 0) 1 0)))
