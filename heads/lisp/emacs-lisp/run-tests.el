;;; run-tests.el --- Run Hydra Emacs Lisp test suite in batch mode -*- lexical-binding: t -*-

;; Increase recursion limits for reduceTerm on complex test expressions
(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)

;; #434: the hand-written Emacs Lisp runtime (loader.el, lazy.el, prims.el,
;; lib/*.el) now lives in overlay/emacs-lisp/ and is COPIED into the dist tree by
;; the assembler. The head loads it from dist/, never from heads/ or overlay/.
;; Compute the dist base first (honoring the bootstrap demo's HYDRA_LISP_DIST_BASE
;; flat layout), then load the loader from dist.
(let* ((env-base (getenv "HYDRA_LISP_DIST_BASE"))
       (dist-base (if (and env-base (> (length env-base) 0))
                      (file-name-as-directory env-base)
                    (expand-file-name "../../../dist/emacs-lisp/hydra-kernel/"
                                      (file-name-directory load-file-name)))))
  (setq hydra-gen-main-dir (expand-file-name "src/main/emacs-lisp/hydra/" dist-base))
  (setq hydra-gen-test-dir (expand-file-name "src/test/emacs-lisp/hydra/" dist-base))

  ;; Load the loader from the dist copy (it in turn loads lazy.el from its own
  ;; dir, which is now the dist tree).
  (load (expand-file-name "loader.el" hydra-gen-main-dir) nil t)
  ;; In the bootstrap demo (HYDRA_LISP_DIST_BASE set), hand-written files
  ;; live alongside generated files under the same hydra/ tree. Skip the
  ;; top-level ones here; the hand-written runtime under emacs_lisp/lib/ is
  ;; excluded from the gen-main walk unconditionally by the loader (the
  ;; emacs_lisp/lib/ prefix filter in hydra-load-gen-main).
  (when (and env-base (> (length env-base) 0))
    (setq hydra-skip-gen-main-files
          '("loader.el" "lazy.el" "prims.el"))))

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

;; Load test data modules (types, terms, env, graph) first — these provide
;; the test types and terms needed by hydra-ensure-test-graph for schema
;; construction. test_env.el is the hand-written counterpart of the DSL's
;; hydra.test.testEnv stub (filtered from emitted output via
;; testSkipEmitModuleNames); it must load before test_graph.el so the
;; generated (require 'hydra.test.testEnv) resolves.
;; #501: test_env.el now lives in overlay/emacs-lisp/ under the renamed
;; hydra.overlay.emacs_lisp.test namespace and is copied into the dist MAIN tree
;; at overlay/emacs_lisp/test/test_env.el, so it loads from hydra-gen-main-dir
;; rather than the generated test tree. test_types/test_terms/test_graph remain
;; generated test modules in hydra-gen-test-dir.
(let ((base hydra-gen-test-dir))
  (dolist (f '("test/test_types.el" "test/test_terms.el"))
    (let ((path (expand-file-name f base)))
      (when (file-exists-p path) (hydra-load-file path)))))
(let ((env-path (expand-file-name "overlay/emacs_lisp/test/test_env.el" hydra-gen-main-dir)))
  (when (file-exists-p env-path) (hydra-load-file env-path)))
(let ((base hydra-gen-test-dir))
  (dolist (f '("test/test_graph.el"))
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
