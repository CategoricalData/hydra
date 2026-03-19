;;; Run Hydra Common Lisp generation tests
;;;
;;; Usage: cd hydra-common-lisp && sbcl --noinform --non-interactive --no-userinit --load src/test/common-lisp/run-generation-tests.lisp

(in-package :cl-user)
(setf *read-default-float-format* 'double-float)

;; Determine base directory
(defvar *hydra-cl-base*
  (make-pathname :directory (butlast (pathname-directory *load-truename*) 3)
                 :defaults *load-truename*))
(defun hydra-path (relative)
  (merge-pathnames relative *hydra-cl-base*))

;; Load prelude and native libs
(load (hydra-path "src/main/common-lisp/hydra/prelude.lisp"))
(dolist (f '("lib/equality.lisp" "lib/maps.lisp" "lib/sets.lisp" "lib/lists.lisp"
             "lib/strings.lisp" "lib/logic.lisp" "lib/math.lisp" "lib/chars.lisp"
             "lib/eithers.lisp" "lib/literals.lisp" "lib/maybes.lisp" "lib/pairs.lisp"))
  (load (hydra-path (concatenate 'string "src/main/common-lisp/hydra/" f))))

;; Load loader and gen-main
(load (hydra-path "src/main/common-lisp/hydra/loader.lisp"))
(hydra-set-function-bindings)
(hydra-load-gen-main)
(hydra-set-function-bindings)

;; Load prims and libraries
(load (hydra-path "src/main/common-lisp/hydra/prims.lisp"))
(load (hydra-path "src/main/common-lisp/hydra/lib/libraries.lisp"))

;; Collect generation test files
(defvar *gen-test-base* (hydra-path "src/gen-test/common-lisp/generation/"))
(defvar *gen-test-files*
  (let ((pattern (merge-pathnames "**/*.lisp" *gen-test-base*)))
    (sort (directory pattern) #'string< :key #'namestring)))

;; Load all generation test files (suppress style warnings)
(format t "Loading ~A generation test files...~%" (length *gen-test-files*))
(handler-bind ((style-warning #'muffle-warning))
  (dolist (f *gen-test-files*)
    (hydra-load-file f)))
(hydra-set-function-bindings)

;; Discover and run all test-* functions
(defvar *total-pass* 0)
(defvar *total-fail* 0)
(defvar *total-error* 0)

(format t "Running generation tests...~%~%")
(let ((test-syms nil))
  (do-symbols (sym (find-package :cl-user))
    (when (and (fboundp sym)
               (>= (length (symbol-name sym)) 5)
               (string= "TEST-" (subseq (symbol-name sym) 0 5)))
      (push sym test-syms)))
  (setf test-syms (sort test-syms #'string< :key #'symbol-name))
  (dolist (sym test-syms)
    (handler-case
      (progn
        (funcall sym)
        (incf *total-pass*))
      (simple-error (e)
        (format t "FAIL: ~A~%  ~A~%" (symbol-name sym) e)
        (incf *total-fail*))
      (error (e)
        (format t "FAIL: ~A~%  ~A~%" (symbol-name sym) e)
        (incf *total-fail*)))))

(format t "~%========================================~%")
(format t "Generation tests: ~A passed, ~A failed, ~A errors~%"
        *total-pass* *total-fail* *total-error*)

(let ((exit-code (if (> *total-fail* 0) 1 0)))
  #+sbcl (sb-ext:exit :code exit-code)
  #+ccl (ccl:quit exit-code))
