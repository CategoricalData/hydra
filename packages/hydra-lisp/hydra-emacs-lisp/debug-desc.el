;;; debug-desc.el --- Debug getTermDescription -*- lexical-binding: t -*-
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
(hydra-ensure-test-graph)

;; Step 1: Run setTermDescription
(let* ((cx (hydra-empty-context))
       (graph hydra--test-graph)
       (set-fn (list :function (list :primitive "hydra.annotations.setTermDescription")))
       (val (list :maybe (list :literal (list :string "hello"))))
       (term (list :literal (list :string "foo")))
       (set-app (list :application (make-hydra_core_application
                  (list :application (make-hydra_core_application set-fn val))
                  term)))
       (set-result (funcall (funcall (funcall (funcall hydra_reduction_reduce_term cx) graph) t) set-app)))
  (message "SET result tag: %S" (car (cadr set-result)))
  (message "SET is-annotated: %s" (hydra--is-annotated-p (cadr set-result)))

  ;; Step 2: Now manually call getTermDescription on the result
  (let ((annotated-term (cadr set-result)))
    ;; Extract annotations directly
    (message "Annotations in term:")
    (let ((anns (hydra--term-annotations annotated-term)))
      (message "  count: %d" (length anns))
      (dolist (a anns)
        (message "  key: %S" (substring (format "%S" (car a)) 0 (min 120 (length (format "%S" (car a))))))
        (message "  val: %S" (substring (format "%S" (cdr a)) 0 (min 120 (length (format "%S" (cdr a))))))))

    ;; Trace the description key matching
    (let ((anns (hydra--term-annotations annotated-term)))
      (dolist (entry anns)
        (let ((k (car entry)))
          (message "Checking key: tag=%S wrap?=%s" (car k) (eq (car k) :wrap))
          (when (and (consp k) (eq (car k) :wrap))
            (let ((wt (cadr k)))
              (message "  type_name: %S" (hydra_core_wrapped_term-type_name wt))
              (message "  body: %S" (hydra_core_wrapped_term-body wt))
              (let ((b (hydra_core_wrapped_term-body wt)))
                (message "  b tag: %S" (car b))
                (when (and (consp b) (eq (car b) :literal))
                  (message "  literal: %S" (cadr b))
                  (when (and (consp (cadr b)) (eq (car (cadr b)) :string))
                    (message "  string val: %S" (cadr (cadr b)))
                    (message "  matches description: %s" (equal (cadr (cadr b)) "description"))))))))))

    ;; Call the primitive directly — simulate what reducer does
    (message "Direct get (3 args):")
    (let ((get-result (hydra--prim-get-term-description cx graph (list (list :unit) (list :unit) annotated-term))))
      (message "  result: %S" (substring (format "%S" get-result) 0 (min 100 (length (format "%S" get-result)))))
      ;; Also trace the inner logic
      (let* ((peeled annotated-term)
             (cached-anns (hydra--lookup-cached-annotations peeled))
             (term-anns (hydra--term-annotations peeled))
             (anns (if cached-anns (append cached-anns term-anns) term-anns)))
        (message "  cached: %s term-anns: %d total: %d" (if cached-anns "yes" "no") (length term-anns) (length anns))
        (let ((desc-entry (cl-find-if
                            (lambda (e)
                              (let ((k (car e)))
                                (and (consp k) (eq (car k) :wrap)
                                     (let ((wt (cadr k)))
                                       (and (equal (hydra_core_wrapped_term-type_name wt) "hydra.core.Name")
                                            (let ((b (hydra_core_wrapped_term-body wt)))
                                              (and (consp b) (eq (car b) :literal)
                                                   (consp (cadr b)) (eq (car (cadr b)) :string)
                                                   (equal (cadr (cadr b)) "description"))))))))
                            anns)))
          (message "  desc-entry found: %s" (if desc-entry "yes" "no")))))

    ;; Step 3: Now reduce through the full getTermDescription chain
    (message "Via reducer:")
    (let* ((get-fn (list :function (list :primitive "hydra.annotations.getTermDescription")))
           (get-app (list :application (make-hydra_core_application
                      (list :application (make-hydra_core_application
                        (list :application (make-hydra_core_application get-fn (list :unit)))
                        (list :unit)))
                      annotated-term)))
           (get-result (funcall (funcall (funcall (funcall hydra_reduction_reduce_term cx) graph) t) get-app)))
      (message "  result: %S" (substring (format "%S" (cadr get-result)) 0 (min 100 (length (format "%S" (cadr get-result)))))))))
