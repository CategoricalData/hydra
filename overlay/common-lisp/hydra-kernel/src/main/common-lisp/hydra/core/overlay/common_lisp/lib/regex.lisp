(in-package :cl-user)

;; Regex primitives. Uses cl-ppcre at runtime if available.
;; Symbols are always defined so generated code can reference them.
;;
;; Patterns are Hydra-defined and translingual (docs/specification/regex.md). Each primitive first
;; runs the pattern through hydra.core.parse.regex, then renders the AST to PCRE syntax via
;; hydra.core.print.pcre.regex (cl-ppcre is a full Perl-5-syntax engine), before handing the rendered
;; pattern to the native engine. An ill-formed pattern (rejected by hydra.core.parse.regex) is treated as
;; "no match" -- the same portable-failure convention as an empty match. See issue #603.
;; No package-qualified references for these: they are generated kernel modules, loaded via the
;; rewriting hydra-load-file (hydra-load-gen-main), which strips defpackage/in-package forms --
;; kernel modules never get a real CL package. Their functions (hydra_parse_regex_parse_regex,
;; hydra_print_pcre_regex_print_regex) are globally defined bare symbols by gen-main load time,
;; before this file's functions are ever called.

(defun hydra--ppcre-call (fn-name &rest args)
  (let ((pkg (find-package :cl-ppcre)))
    (unless pkg (error "cl-ppcre is required for regex primitives but is not loaded"))
    (apply (symbol-function (intern fn-name pkg)) args)))

;; (list :given <native-pattern-string>), or (list :none) if the pattern does not parse.
(defun hydra--regex-to-native (pattern)
  (let ((parsed (funcall hydra_parse_regex_parse_regex pattern)))
    (if (eq (car parsed) :given)
        (list :given (funcall hydra_print_pcre_regex_print_regex (cadr parsed)))
        (list :none))))

;; matches :: String -> String -> Bool
(defvar hydra_overlay_common_lisp_lib_regex_matches
  (lambda (pattern)
    (lambda (input)
      (let ((native (hydra--regex-to-native pattern)))
        (if (eq (car native) :none)
            nil
            (let ((scanner (hydra--ppcre-call "CREATE-SCANNER" (concatenate 'string "^(?:" (cadr native) ")$"))))
              (if (hydra--ppcre-call "SCAN" scanner input) t nil)))))))

;; find :: String -> String -> Maybe String
(defvar hydra_overlay_common_lisp_lib_regex_find
  (lambda (pattern)
    (lambda (input)
      (let ((native (hydra--regex-to-native pattern)))
        (if (eq (car native) :none)
            nil
            (multiple-value-bind (start end)
                (hydra--ppcre-call "SCAN" (cadr native) input)
              (if start (subseq input start end) nil)))))))

;; find_all :: String -> String -> [String]
(defvar hydra_overlay_common_lisp_lib_regex_find_all
  (lambda (pattern)
    (lambda (input)
      (let ((native (hydra--regex-to-native pattern)))
        (if (eq (car native) :none)
            nil
            (hydra--ppcre-call "ALL-MATCHES-AS-STRINGS" (cadr native) input))))))

;; replace :: String -> String -> String -> String
(defvar hydra_overlay_common_lisp_lib_regex_replace
  (lambda (pattern)
    (lambda (replacement)
      (lambda (input)
        (let ((native (hydra--regex-to-native pattern)))
          (if (eq (car native) :none)
              input
              (hydra--ppcre-call "REGEX-REPLACE" (cadr native) input replacement)))))))

;; replace_all :: String -> String -> String -> String
(defvar hydra_overlay_common_lisp_lib_regex_replace_all
  (lambda (pattern)
    (lambda (replacement)
      (lambda (input)
        (let ((native (hydra--regex-to-native pattern)))
          (if (eq (car native) :none)
              input
              (hydra--ppcre-call "REGEX-REPLACE-ALL" (cadr native) input replacement)))))))

;; split :: String -> String -> [String]
(defvar hydra_overlay_common_lisp_lib_regex_split
  (lambda (pattern)
    (lambda (input)
      (let ((native (hydra--regex-to-native pattern)))
        (if (eq (car native) :none)
            (list input)
            (let ((parts nil)
                  (start 0)
                  (len (length input))
                  (native-pattern (cadr native)))
              ;; Simple split using SCAN in a loop
              (loop
                (multiple-value-bind (ms me)
                    (hydra--ppcre-call "SCAN" native-pattern input :start start)
                  (unless ms
                    (push (subseq input start len) parts)
                    (return))
                  (push (subseq input start ms) parts)
                  (setf start me)))
              (nreverse parts)))))))
