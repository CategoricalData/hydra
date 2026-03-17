;;; debug-hang.el --- Find the hanging inference test -*- lexical-binding: t -*-

(setq max-lisp-eval-depth 10000 max-specpdl-size 10000)
(load (expand-file-name "src/main/emacs-lisp/hydra/loader.el"
                         (file-name-directory load-file-name)) nil t)
(hydra-load-gen-main)
(hydra-set-function-bindings)
(hydra-load-prims-and-libraries)
(hydra-load-gen-test)
(hydra-set-function-bindings)
(load (expand-file-name "src/test/emacs-lisp/hydra/test_runner.el"
                         (file-name-directory load-file-name)) nil t)
(message "DBG: about to build graph")
(hydra-ensure-test-graph)
(message "DBG: graph built")

;; Find and run test #31 (Optional eliminations #4) directly
(defvar hydra--tidx 0)

(defun hydra-find-nth-test (path group n)
  "Find the Nth test case in the tree."
  (let* ((gname (cdr (assq :name group)))
         (full (if (equal path "") gname (format "%s > %s" path gname))))
    (catch 'found
      (dolist (sg (cdr (assq :subgroups group)))
        (let ((r (hydra-find-nth-test full sg n)))
          (when r (throw 'found r))))
      (dolist (tc (cdr (assq :cases group)))
        (setq hydra--tidx (1+ hydra--tidx))
        (when (= hydra--tidx n)
          (throw 'found (cons full tc))))
      nil)))

;; Walk the FULL inference tree and print name BEFORE each test
(defun hydra-walk-run (path group)
  (let* ((gname (cdr (assq :name group)))
         (full (if (equal path "") gname (format "%s > %s" path gname))))
    (dolist (sg (cdr (assq :subgroups group)))
      (hydra-walk-run full sg))
    (dolist (tc (cdr (assq :cases group)))
      (setq hydra--tidx (1+ hydra--tidx))
      (message "T%d: %s > %s" hydra--tidx full (cdr (assq :name tc)))
      (condition-case err
          (hydra-run-test-case full tc)
        (error (message "  ERR: %S" err))))))

;; Time first 10 inference tests to measure byte-compilation speedup
(let ((inf (nth 17 (cdr (assq :subgroups hydra_test_test_suite_all_tests)))))
  (dotimes (i 10)
    (setq hydra--tidx 0)
    (let ((found (hydra-find-nth-test "" inf (1+ i))))
      (when found
        (let ((start (float-time)))
          (condition-case err
              (hydra-run-test-case (car found) (cdr found))
            (error nil))
          (message "T%d: %.3fs  %s" (1+ i) (- (float-time) start)
                   (cdr (assq :name (cdr found))))))))
  (message "Done"))
