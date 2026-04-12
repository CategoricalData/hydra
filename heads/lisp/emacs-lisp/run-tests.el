;;; run-tests.el --- Run Hydra Emacs Lisp test suite in batch mode -*- lexical-binding: t -*-

;; Increase recursion limits for reduceTerm on complex test expressions
(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)

;; Load the loader
(let ((loader-path (expand-file-name "src/main/emacs-lisp/hydra/loader.el"
                                      (file-name-directory load-file-name))))
  (load loader-path nil t))

;; Override gen-main and gen-test dirs to point at the dist/ generated content.
(let ((dist-base (expand-file-name "../../../dist/emacs-lisp/hydra-kernel/"
                                    (file-name-directory load-file-name))))
  (setq hydra-gen-main-dir (expand-file-name "src/main/emacs-lisp/hydra/" dist-base))
  (setq hydra-gen-test-dir (expand-file-name "src/test/emacs-lisp/hydra/" dist-base)))

;; Load gen-main
(hydra-load-gen-main)
(hydra-set-function-bindings)

;; Load prims and libraries
(hydra-load-prims-and-libraries)

;; Load annotation bindings (Term AST definitions for kernel annotation functions)
(let ((ann-path (expand-file-name "src/test/emacs-lisp/hydra/annotation_bindings.el"
                                   (file-name-directory load-file-name))))
  (load ann-path nil t))

;; Load test runner (before gen-test so hydra-build-test-graph is available)
(let ((runner-path (expand-file-name "src/test/emacs-lisp/hydra/test_runner.el"
                                      (file-name-directory load-file-name))))
  (load runner-path nil t))

;; Load test data modules (types, terms, graph) first — these provide the test
;; types and terms needed by hydra-ensure-test-graph for schema construction.
(let ((base hydra-gen-test-dir))
  (dolist (f '("test/test_types.el" "test/test_terms.el" "test/test_graph.el"))
    (let ((path (expand-file-name f base)))
      (when (file-exists-p path) (hydra-load-file path)))))
(hydra-set-function-bindings)

;; Build the enhanced test graph BEFORE loading the remaining gen-test modules.
;; Note: test_graph.el also builds the graph (via sync-lisp patch), but
;; hydra-load-gen-test reloads everything, so we ensure the graph is ready.
(hydra-ensure-test-graph)
(setq hydra_test_test_graph_test_graph hydra--test-graph)
(setq hydra_test_test_graph_test_context (hydra-empty-context))

;; Load remaining gen-test modules (test cases that reference the graph)
(hydra-load-gen-test)
(hydra-set-function-bindings)

;; Run tests
(hydra-run-tests)
