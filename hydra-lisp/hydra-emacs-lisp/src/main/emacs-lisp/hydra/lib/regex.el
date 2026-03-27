;;; regex.el --- Hydra regex primitives -*- lexical-binding: t; -*-

;; Convert a POSIX ERE pattern to Emacs regex syntax.
;; In POSIX ERE: |, (, ) are special; \|, \(, \) are literal.
;; In Emacs:     |, (, ) are literal; \|, \(, \) are special.
;; So we swap the escaping convention for these three characters,
;; while leaving everything else (including character classes) alone.
(defun hydra--posix-to-emacs-regex (pattern)
  (let ((i 0)
        (len (length pattern))
        (result nil)
        (in-bracket nil))
    (while (< i len)
      (let ((ch (aref pattern i)))
        (cond
         ;; Track character class brackets (don't transform inside them)
         ((and (not in-bracket) (= ch ?\[))
          (push ch result)
          (setq i (1+ i))
          (setq in-bracket t)
          ;; Handle ] as first char in class (literal)
          (when (and (< i len) (= (aref pattern i) ?\]))
            (push (aref pattern i) result)
            (setq i (1+ i)))
          ;; Handle ^ then ] as first chars
          (when (and (< i len) (= (aref pattern i) ?^))
            (push (aref pattern i) result)
            (setq i (1+ i))
            (when (and (< i len) (= (aref pattern i) ?\]))
              (push (aref pattern i) result)
              (setq i (1+ i)))))
         ((and in-bracket (= ch ?\]))
          (push ch result)
          (setq i (1+ i))
          (setq in-bracket nil))
         ;; Inside brackets, pass through unchanged
         (in-bracket
          (push ch result)
          (setq i (1+ i)))
         ;; Backslash escape: check next char
         ((and (= ch ?\\) (< (1+ i) len))
          (let ((next (aref pattern (1+ i))))
            (cond
             ;; \| \( \) in POSIX ERE are literal -> emit the char without backslash
             ((memq next '(?| ?\( ?\)))
              (push next result)
              (setq i (+ i 2)))
             ;; Other escapes pass through as-is
             (t
              (push ch result)
              (push next result)
              (setq i (+ i 2))))))
         ;; Unescaped | ( ) in POSIX ERE are special -> add backslash for Emacs
         ((memq ch '(?| ?\( ?\)))
          (push ?\\ result)
          (push ch result)
          (setq i (1+ i)))
         ;; Everything else passes through
         (t
          (push ch result)
          (setq i (1+ i))))))
    (concat (nreverse result))))

;; find :: String -> String -> Maybe String
(defvar hydra_lib_regex_find
  (lambda (pattern)
    "Find the first substring matching a regex pattern."
    (lambda (input)
      (let ((emacs-pat (hydra--posix-to-emacs-regex pattern)))
        (if (string-match emacs-pat input)
            (match-string 0 input)
          nil)))))

;; find_all :: String -> String -> [String]
(defvar hydra_lib_regex_find_all
  (lambda (pattern)
    "Find all non-overlapping substrings matching a regex pattern."
    (lambda (input)
      (let ((emacs-pat (hydra--posix-to-emacs-regex pattern))
            (start 0)
            (results nil))
        (while (string-match emacs-pat input start)
          (push (match-string 0 input) results)
          (setq start (match-end 0))
          ;; Avoid infinite loop on zero-length matches
          (when (= start (match-beginning 0))
            (setq start (1+ start))))
        (nreverse results)))))

;; matches :: String -> String -> Bool
(defvar hydra_lib_regex_matches
  (lambda (pattern)
    "Check whether an entire string matches a regex pattern."
    (lambda (input)
      (let* ((emacs-pat (hydra--posix-to-emacs-regex pattern))
             (full-pattern (concat "\\`\\(?:" emacs-pat "\\)\\'")))
        (if (string-match-p full-pattern input) t nil)))))

;; replace :: String -> String -> String -> String
;; Replace only the first occurrence
(defvar hydra_lib_regex_replace
  (lambda (pattern)
    "Replace the first occurrence of a regex pattern with a replacement string."
    (lambda (replacement)
      (lambda (input)
        (let ((emacs-pat (hydra--posix-to-emacs-regex pattern)))
          (if (string-match emacs-pat input)
              (concat (substring input 0 (match-beginning 0))
                      replacement
                      (substring input (match-end 0)))
            input))))))

;; replace_all :: String -> String -> String -> String
(defvar hydra_lib_regex_replace_all
  (lambda (pattern)
    "Replace all non-overlapping occurrences of a regex pattern with a replacement string."
    (lambda (replacement)
      (lambda (input)
        (let ((emacs-pat (hydra--posix-to-emacs-regex pattern)))
          (replace-regexp-in-string emacs-pat replacement input t t))))))

;; split :: String -> String -> [String]
(defvar hydra_lib_regex_split
  (lambda (pattern)
    "Split a string by a regex pattern."
    (lambda (input)
      (let ((emacs-pat (hydra--posix-to-emacs-regex pattern)))
        (split-string input emacs-pat)))))

(provide 'hydra.lib.regex)
