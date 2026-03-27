(in-package :cl-user)

;; Regex primitives. Uses cl-ppcre at runtime if available.
;; Symbols are always defined so generated code can reference them.

(defun hydra--ppcre-call (fn-name &rest args)
  (let ((pkg (find-package :cl-ppcre)))
    (unless pkg (error "cl-ppcre is required for regex primitives but is not loaded"))
    (apply (symbol-function (intern fn-name pkg)) args)))

;; find :: String -> String -> Maybe String
;; Find the first substring matching a regex pattern.
(defvar hydra_lib_regex_find
  (lambda (pattern)
    (lambda (input)
      (multiple-value-bind (start end)
          (hydra--ppcre-call "SCAN" pattern input)
        (if start (subseq input start end) nil)))))

;; find_all :: String -> String -> [String]
;; Find all non-overlapping substrings matching a regex pattern.
(defvar hydra_lib_regex_find_all
  (lambda (pattern)
    (lambda (input)
      (hydra--ppcre-call "ALL-MATCHES-AS-STRINGS" pattern input))))

;; matches :: String -> String -> Bool
;; Check whether an entire string matches a regex pattern.
(defvar hydra_lib_regex_matches
  (lambda (pattern)
    (lambda (input)
      (let ((scanner (hydra--ppcre-call "CREATE-SCANNER" (concatenate 'string "^(?:" pattern ")$"))))
        (if (hydra--ppcre-call "SCAN" scanner input) t nil)))))

;; replace :: String -> String -> String -> String
;; Replace the first occurrence of a regex pattern with a replacement string.
(defvar hydra_lib_regex_replace
  (lambda (pattern)
    (lambda (replacement)
      (lambda (input)
        (hydra--ppcre-call "REGEX-REPLACE" pattern input replacement)))))

;; replace_all :: String -> String -> String -> String
;; Replace all non-overlapping occurrences of a regex pattern with a replacement string.
(defvar hydra_lib_regex_replace_all
  (lambda (pattern)
    (lambda (replacement)
      (lambda (input)
        (hydra--ppcre-call "REGEX-REPLACE-ALL" pattern input replacement)))))

;; split :: String -> String -> [String]
;; Split a string by a regex pattern.
(defvar hydra_lib_regex_split
  (lambda (pattern)
    (lambda (input)
      (let ((parts nil)
            (start 0)
            (len (length input)))
        ;; Simple split using SCAN in a loop
        (loop
          (multiple-value-bind (ms me)
              (hydra--ppcre-call "SCAN" pattern input :start start)
            (unless ms
              (push (subseq input start len) parts)
              (return))
            (push (subseq input start ms) parts)
            (setf start me)))
        (nreverse parts)))))
