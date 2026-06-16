;;; Hydra Common Lisp prelude
;;; Compatibility layer for the generated code.
;;; Must be loaded before any generated files.
;;;
;;; The generated CL code has three issues this file addresses:
;;; 1. Records are constructed as alists '((:key . val) ...) but accessed via
;;;    defstruct accessors. We shadow cl:defstruct to create alist-compatible
;;;    accessors with BOTH CamelCase and snake_case names.
;;; 2. (:use :package) directives are not valid CL. We ignore them.
;;; 3. defpackage/in-package directives - we keep everything in one package.

(in-package :cl-user)

;; ============================================================================
;; Camel to snake_case conversion
;; ============================================================================

(defun hydra-camel-to-snake (name-string)
  "Convert CamelCase to snake_case, e.g. 'AnnotatedTerm' -> 'annotated_term'."
  (with-output-to-string (s)
    (loop for i from 0 below (length name-string)
          for c = (char name-string i)
          do (cond
               ((and (upper-case-p c) (> i 0)
                     (or (lower-case-p (char name-string (1- i)))
                         (and (< (1+ i) (length name-string))
                              (lower-case-p (char name-string (1+ i))))))
                (write-char #\_ s)
                (write-char (char-downcase c) s))
               (t (write-char (char-downcase c) s))))))

;; ============================================================================
;; Alist-based struct system
;; ============================================================================
;; We shadow cl:defstruct with a macro that creates alist-compatible functions.
;; Records are alists like '((:body . value) (:annotation . value)).
;;
;; For struct name "AnnotatedTerm" with fields "body" and "annotation":
;; - Creates make-annotatedterm (CL reader upcases)
;; - Creates annotatedterm-body, annotatedterm-annotation (CL reader upcases)
;; - Also creates make-annotated_term, annotated_term-body, etc. (snake_case aliases)
;;
;; Both sets of accessors work on alists.

(defmacro hydra-defstruct (name &rest fields)
  "Define alist-based record type with both CamelCase and snake_case accessors."
  (let* ((name-str (symbol-name name))
         (snake-str (string-upcase (hydra-camel-to-snake name-str)))
         (camel-up (string-upcase name-str)))
    `(progn
       ;; Constructor (CamelCase): (make-AnnotatedTerm :body B :annotation A)
       ;; Also supports positional: (make-AnnotatedTerm B A)
       (defun ,(intern (format nil "MAKE-~A" camel-up)) (&rest args)
         (if (and args (keywordp (first args)))
           ;; Keyword arguments
           (loop for (k v) on args by #'cddr
                 collect (cons k v))
           ;; Positional arguments
           (mapcar #'cons
                   ',(mapcar (lambda (f) (intern (string-upcase (symbol-name f)) :keyword)) fields)
                   args)))

       ;; Accessors (CamelCase)
       ,@(mapcar (lambda (field)
                   (let ((acc-name (intern (format nil "~A-~A" camel-up (string-upcase (symbol-name field))))))
                     `(defun ,acc-name (rec)
                        (cdr (assoc ,(intern (string-upcase (symbol-name field)) :keyword) rec)))))
                 fields)

       ;; Snake_case aliases (only if different from CamelCase)
       ,@(when (string/= camel-up snake-str)
           (cons
             ;; Constructor alias
             `(setf (symbol-function ',(intern (format nil "MAKE-~A" snake-str)))
                    (symbol-function ',(intern (format nil "MAKE-~A" camel-up))))
             ;; Accessor aliases
             (mapcar (lambda (field)
                       (let ((camel-acc (intern (format nil "~A-~A" camel-up (string-upcase (symbol-name field)))))
                             (snake-acc (intern (format nil "~A-~A" snake-str (string-upcase (symbol-name field))))))
                         (when (not (eq camel-acc snake-acc))
                           `(setf (symbol-function ',snake-acc)
                                  (symbol-function ',camel-acc)))))
                     fields)))

       ;; Predicate
       (defun ,(intern (format nil "~A-P" camel-up)) (x) (listp x))
       ,@(when (string/= camel-up snake-str)
           `((defun ,(intern (format nil "~A-P" snake-str)) (x) (listp x))))

       ;; Copy constructor
       (defun ,(intern (format nil "COPY-~A" camel-up)) (x) (copy-alist x))
       ,@(when (string/= camel-up snake-str)
           `((defun ,(intern (format nil "COPY-~A" snake-str)) (x) (copy-alist x))))

       ',name)))

;; ============================================================================
;; Scheme compatibility aliases
;; ============================================================================
;; The generated code uses Scheme-style function names for pattern matching.
;; Define CL equivalents.

(defun equal? (a b) (equal a b))
