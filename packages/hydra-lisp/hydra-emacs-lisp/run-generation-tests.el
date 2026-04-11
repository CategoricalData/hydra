;;; run-generation-tests.el --- Run Hydra Emacs Lisp generation tests -*- lexical-binding: t -*-

;; Load the loader
(let ((loader-path (expand-file-name "src/main/emacs-lisp/hydra/loader.el"
                                      (file-name-directory load-file-name))))
  (load loader-path nil t))

;; Load gen-main
(hydra-load-gen-main)
(hydra-set-function-bindings)

;; Load prims and libraries
(hydra-load-prims-and-libraries)

;; Load generation test files (suppress redefinition warnings)
(let ((gen-test-dir (expand-file-name "src/gen-test/emacs-lisp/generation/"
                                       (file-name-directory load-file-name))))
  (dolist (f (directory-files-recursively gen-test-dir "\\.el$"))
    (condition-case nil
        (load f nil t)
      (error nil))))

;; Run ERT tests
(let ((results (ert-run-tests-batch-and-exit)))
  results)
