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

  ;; #546: the hydra.build.* main + hydra.test.build.* test modules moved to the
  ;; hydra-build package's own dist tree. In the repo layout they live under
  ;; dist/emacs-lisp/hydra-build/...; in the bootstrap-demo flat layout
  ;; (HYDRA_LISP_DIST_BASE set) all packages share one tree, so reuse it.
  (if (and env-base (> (length env-base) 0))
      (progn
        (setq hydra-build-gen-main-dir hydra-gen-main-dir)
        (setq hydra-build-gen-test-dir hydra-gen-test-dir))
    (let ((build-base (expand-file-name "../../../dist/emacs-lisp/hydra-build/"
                                        (file-name-directory load-file-name))))
      (setq hydra-build-gen-main-dir (expand-file-name "src/main/emacs-lisp/hydra/" build-base))
      (setq hydra-build-gen-test-dir (expand-file-name "src/test/emacs-lisp/hydra/" build-base))))

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
;; #546: load hydra-build's main modules (hydra.build.*) from its own dist tree
;; (a no-op re-scan of the same tree in the bootstrap-demo flat layout).
(when (and hydra-build-gen-main-dir
           (not (string= hydra-build-gen-main-dir hydra-gen-main-dir)))
  (hydra-load-gen-main hydra-build-gen-main-dir))
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

;; #546: load hydra-build's test modules (hydra.test.build.*) BEFORE
;; hydra-load-gen-test. The kernel loader list (hydra-load-gen-test) deliberately
;; omits test/build/*.el, but the generated kernel test suite aggregate
;; (test/test_suite.el, loaded inside hydra-load-gen-test) references
;; hydra_test_build_*_all_tests, so those symbols must already be defined.
;; They live in hydra-build's own dist test tree (hydra-build-gen-test-dir) in the
;; normal layout, but in the bootstrap-demo FLAT layout everything shares one tree
;; (hydra-build-gen-test-dir == hydra-gen-test-dir, or is nil). Try both bases so
;; the flat layout is covered too; #444: previously the (not (string= ...)) guard
;; skipped the flat case entirely, leaving the symbols void.
(let ((bases (delete-dups
              (delq nil (list hydra-gen-test-dir hydra-build-gen-test-dir)))))
  (dolist (base bases)
    (dolist (f '("test/build/modules.el"
                 "test/build/reconcile.el"
                 "test/build/routing.el"))
      (let ((path (expand-file-name f base)))
        (when (file-exists-p path) (hydra-load-file path)))))
  (hydra-set-function-bindings))

;; Load remaining gen-test modules (test cases that reference the graph)
(hydra-load-gen-test)
(hydra-set-function-bindings)

;; Run tests
(hydra-run-tests)
