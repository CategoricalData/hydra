;;; regex.el --- Hydra regex primitives -*- lexical-binding: t; -*-

;; Patterns are Hydra-defined and translingual (docs/specification/regex.md). Each primitive first
;; runs the pattern through hydra.core.parse.regex, then renders the AST to Emacs regexp syntax via
;; hydra.core.print.emacs.regex (which supersedes the old ad-hoc hydra--posix-to-emacs-regex string shim,
;; including its {n,m}-quantifier gap), before handing the rendered pattern to the native engine. An
;; ill-formed pattern (rejected by hydra.core.parse.regex) is treated as "no match" -- the same portable-
;; failure convention as an empty match. See issue #603.
;; No (require ...) for these: they are generated kernel modules, loaded via the rewriting
;; hydra-load-file (hydra-load-gen-main), which strips require/provide forms -- kernel modules are
;; never real Emacs features. Their functions (hydra_parse_regex_parse_regex,
;; hydra_print_emacs_regex_print_regex) are globally defined by gen-main load time, before this file's
;; functions are ever called (see libraries.el's comment on this same guarantee).

;; Returns (given . <native-pattern-string>), or 'none if the pattern does not parse.
;; hydra_parse_regex_parse_regex returns an Optional tagged with the keyword :given (Hydra's native
;; Optional convention; see libraries.el's optional-to-native), not the symbol 'given.
(defun hydra--regex-to-native (pattern)
  (let ((parsed (funcall hydra_parse_regex_parse_regex pattern)))
    (if (eq (car parsed) :given)
        (cons 'given (funcall hydra_print_emacs_regex_print_regex (cadr parsed)))
      'none)))

;; All regex primitives bind case-fold-search to nil. POSIX ERE semantics
;; (which the rest of Hydra follows) treat character classes like [a-z]
;; as case-sensitive; Emacs' default case-fold-search of t would fold them.

;; matches :: String -> String -> Bool
(defvar hydra_overlay_emacs_lisp_lib_regex_matches
  (lambda (pattern)
    (lambda (input)
      (let ((native (hydra--regex-to-native pattern)))
        (if (eq native 'none)
            nil
          (let* ((full-pattern (concat "\\`\\(?:" (cdr native) "\\)\\'"))
                 (case-fold-search nil))
            (if (string-match-p full-pattern input) t nil)))))))

;; find :: String -> String -> Maybe String
(defvar hydra_overlay_emacs_lisp_lib_regex_find
  (lambda (pattern)
    (lambda (input)
      (let ((native (hydra--regex-to-native pattern)))
        (if (eq native 'none)
            nil
          (let ((case-fold-search nil))
            (if (string-match (cdr native) input)
                (match-string 0 input)
              nil)))))))

;; find_all :: String -> String -> [String]
(defvar hydra_overlay_emacs_lisp_lib_regex_find_all
  (lambda (pattern)
    (lambda (input)
      (let ((native (hydra--regex-to-native pattern)))
        (if (eq native 'none)
            nil
          (let ((emacs-pat (cdr native))
                (start 0)
                (results nil)
                (case-fold-search nil))
            (while (string-match emacs-pat input start)
              (push (match-string 0 input) results)
              (setq start (match-end 0))
              ;; Avoid infinite loop on zero-length matches
              (when (= start (match-beginning 0))
                (setq start (1+ start))))
            (nreverse results)))))))

;; replace :: String -> String -> String -> String
;; Replace only the first occurrence
(defvar hydra_overlay_emacs_lisp_lib_regex_replace
  (lambda (pattern)
    (lambda (replacement)
      (lambda (input)
        (let ((native (hydra--regex-to-native pattern)))
          (if (eq native 'none)
              input
            (let ((case-fold-search nil))
              (if (string-match (cdr native) input)
                  (concat (substring input 0 (match-beginning 0))
                          replacement
                          (substring input (match-end 0)))
                input))))))))

;; replace_all :: String -> String -> String -> String
(defvar hydra_overlay_emacs_lisp_lib_regex_replace_all
  (lambda (pattern)
    (lambda (replacement)
      (lambda (input)
        (let ((native (hydra--regex-to-native pattern)))
          (if (eq native 'none)
              input
            (let ((case-fold-search nil))
              (replace-regexp-in-string (cdr native) replacement input t t))))))))

;; split :: String -> String -> [String]
(defvar hydra_overlay_emacs_lisp_lib_regex_split
  (lambda (pattern)
    (lambda (input)
      (let ((native (hydra--regex-to-native pattern)))
        (if (eq native 'none)
            (list input)
          (let ((case-fold-search nil))
            (split-string input (cdr native))))))))

(provide 'hydra.core.lib.regex)
