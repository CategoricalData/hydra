(in-package :cl-user)

;; matches :: String -> String -> Bool
(defvar hydra_lib_regex_matches
  (lambda (pattern)
    (lambda (input)
      (let ((scanner (cl-ppcre:create-scanner (concatenate 'string "^(?:" pattern ")$"))))
        (if (cl-ppcre:scan scanner input) t nil)))))

;; find :: String -> String -> Maybe String
(defvar hydra_lib_regex_find
  (lambda (pattern)
    (lambda (input)
      (multiple-value-bind (start end)
          (cl-ppcre:scan pattern input)
        (if start
            (subseq input start end)
            nil)))))

;; find_all :: String -> String -> [String]
(defvar hydra_lib_regex_find_all
  (lambda (pattern)
    (lambda (input)
      (cl-ppcre:all-matches-as-strings pattern input))))

;; replace :: String -> String -> String -> String
(defvar hydra_lib_regex_replace
  (lambda (pattern)
    (lambda (replacement)
      (lambda (input)
        (cl-ppcre:regex-replace pattern input replacement)))))

;; replace_all :: String -> String -> String -> String
(defvar hydra_lib_regex_replace_all
  (lambda (pattern)
    (lambda (replacement)
      (lambda (input)
        (cl-ppcre:regex-replace-all pattern input replacement)))))

;; split :: String -> String -> [String]
;; Note: cl-ppcre:split drops trailing empty strings by default.
;; We use a manual approach to preserve them, matching Haskell/Java/Python behavior.
(defvar hydra_lib_regex_split
  (lambda (pattern)
    (lambda (input)
      (let ((scanner (cl-ppcre:create-scanner pattern))
            (len (length input))
            (start 0)
            (acc nil))
        (cl-ppcre:do-scans (ms me rs re scanner input)
          (push (subseq input start ms) acc)
          (setf start me))
        (push (subseq input start len) acc)
        (nreverse acc)))))
