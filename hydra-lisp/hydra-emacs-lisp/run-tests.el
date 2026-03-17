;;; run-tests.el --- Run Hydra Emacs Lisp test suite in batch mode -*- lexical-binding: t -*-

;; Load the loader
(let ((loader-path (expand-file-name "src/main/emacs-lisp/hydra/loader.el"
                                      (file-name-directory load-file-name))))
  (load loader-path nil t))

;; Load gen-main
(hydra-load-gen-main)
(hydra-set-function-bindings)

;; Load prims and libraries
(hydra-load-prims-and-libraries)

;; Load gen-test
(hydra-load-gen-test)
(hydra-set-function-bindings)

;; Load test runner
(let ((runner-path (expand-file-name "src/test/emacs-lisp/hydra/test_runner.el"
                                      (file-name-directory load-file-name))))
  (load runner-path nil t))

;; Run tests
(hydra-run-tests)
