;;; regex.el --- Hydra regex primitives -*- lexical-binding: t; -*-

;; matches :: String -> String -> Bool
(defvar hydra_lib_regex_matches
  (lambda (pattern)
    (lambda (input)
      (let ((full-pattern (concat "\\`\\(?:" pattern "\\)\\'")))
        (if (string-match-p full-pattern input) t nil)))))

;; find :: String -> String -> Maybe String
(defvar hydra_lib_regex_find
  (lambda (pattern)
    (lambda (input)
      (if (string-match pattern input)
          (match-string 0 input)
        nil))))

;; find_all :: String -> String -> [String]
(defvar hydra_lib_regex_find_all
  (lambda (pattern)
    (lambda (input)
      (let ((start 0)
            (results nil))
        (while (string-match pattern input start)
          (push (match-string 0 input) results)
          (setq start (match-end 0))
          ;; Avoid infinite loop on zero-length matches
          (when (= start (match-beginning 0))
            (setq start (1+ start))))
        (nreverse results)))))

;; replace :: String -> String -> String -> String
;; Replace only the first occurrence
(defvar hydra_lib_regex_replace
  (lambda (pattern)
    (lambda (replacement)
      (lambda (input)
        (if (string-match pattern input)
            (concat (substring input 0 (match-beginning 0))
                    replacement
                    (substring input (match-end 0)))
          input)))))

;; replace_all :: String -> String -> String -> String
(defvar hydra_lib_regex_replace_all
  (lambda (pattern)
    (lambda (replacement)
      (lambda (input)
        (replace-regexp-in-string pattern replacement input t t)))))

;; split :: String -> String -> [String]
(defvar hydra_lib_regex_split
  (lambda (pattern)
    (lambda (input)
      (split-string input pattern))))

(provide 'hydra.lib.regex)
