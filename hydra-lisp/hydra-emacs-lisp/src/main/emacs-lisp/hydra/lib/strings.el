;;; strings.el --- Hydra string primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun hydra--ensure-multibyte (s)
  "Ensure S is a multibyte string. Decodes UTF-8 if unibyte.
   Uses utf-8-unix to preserve CR characters."
  (if (multibyte-string-p s) s
    (decode-coding-string s 'utf-8-unix)))

;; cat :: [String] -> String
(defvar hydra_lib_strings_cat
  (lambda (strs)
    "Concatenate a list of strings into a single string."
    (apply #'concat strs)))

;; cat2 :: String -> String -> String
(defvar hydra_lib_strings_cat2
  (lambda (a)
    "Concatenate two strings."
    (lambda (b)
      (concat a b))))

;; char_at :: Int -> String -> Int32 (char as codepoint)
(defvar hydra_lib_strings_char_at
  (lambda (n)
    "Get the Unicode code point of the character at a specific index in a string."
    (lambda (s)
      (let ((ms (hydra--ensure-multibyte s)))
        (aref (vconcat (string-to-list ms)) n)))))

;; cons :: Int32 (char as codepoint) -> String -> String
(defvar hydra_lib_strings_cons
  (lambda (c)
    "Prepend a character (as a code point) to a string."
    (lambda (s)
      (concat (char-to-string c) s))))

;; drop :: Int -> String -> String
(defvar hydra_lib_strings_drop
  (lambda (n)
    "Drop the first n characters from a string."
    (lambda (s)
      (if (>= n (length s))
          ""
          (substring s n)))))

;; from_list :: [Int32] -> String (codepoints to string)
(defvar hydra_lib_strings_from_list
  (lambda (codepoints)
    "Convert a list of Unicode code points to a string."
    (apply #'string codepoints)))

;; head :: String -> Int32 (char as codepoint)
(defvar hydra_lib_strings_head
  (lambda (s)
    "Get the first character of a string as a Unicode code point."
    (aref s 0)))

;; intercalate :: String -> [String] -> String
(defvar hydra_lib_strings_intercalate
  (lambda (sep)
    "Join a list of strings with a separator between each element."
    (lambda (strs)
      (mapconcat #'identity strs sep))))

;; is_infix_of :: String -> String -> Bool
(defvar hydra_lib_strings_is_infix_of
  (lambda (needle)
    "Check whether a string is a substring of another."
    (lambda (haystack)
      (if (cl-search needle haystack) t nil))))

;; is_prefix_of :: String -> String -> Bool
(defvar hydra_lib_strings_is_prefix_of
  (lambda (prefix)
    "Check whether a string is a prefix of another."
    (lambda (s)
      (let ((plen (length prefix)))
        (and (<= plen (length s))
             (string= (substring s 0 plen) prefix))))))

;; is_suffix_of :: String -> String -> Bool
(defvar hydra_lib_strings_is_suffix_of
  (lambda (suffix)
    "Check whether a string is a suffix of another."
    (lambda (s)
      (let ((slen (length suffix))
            (len (length s)))
        (and (<= slen len)
             (string= (substring s (- len slen)) suffix))))))

;; length :: String -> Int
(defvar hydra_lib_strings_length
  (lambda (s)
    "Return the length of a string."
    (length (hydra--ensure-multibyte s))))

;; lines :: String -> [String]
;; Haskell semantics: lines "" = [], lines "\n" = [""], lines "a\n" = ["a"]
(defvar hydra_lib_strings_lines
  (lambda (s)
    "Split a string into lines."
    (if (= (length s) 0)
        nil
        (let ((len (length s))
              (acc nil)
              (start 0))
          (let ((i 0))
            (while (< i len)
              (when (= (aref s i) ?\n)
                (push (substring s start i) acc)
                (setq start (1+ i)))
              (setq i (1+ i))))
          ;; Only add trailing segment if string doesn't end with newline
          (when (< start len)
            (push (substring s start len) acc))
          (nreverse acc)))))

;; null :: String -> Bool
(defvar hydra_lib_strings_null
  (lambda (s)
    "Check whether a string is empty."
    (= (length s) 0)))

;; replicate :: Int -> String -> String
(defvar hydra_lib_strings_replicate
  (lambda (n)
    "Replicate a string n times."
    (lambda (s)
      (let ((acc ""))
        (dotimes (_ n acc)
          (setq acc (concat acc s)))))))

;; reverse :: String -> String
(defvar hydra_lib_strings_reverse
  (lambda (s)
    "Reverse a string."
    (reverse s)))

;; split_on :: String -> String -> [String]
;; Haskell semantics: splitOn "" "" = [""], splitOn "" "abc" = ["", "a", "b", "c"]
(defvar hydra_lib_strings_split_on
  (lambda (sep)
    "Split a string on a delimiter string."
    (lambda (s)
      (let ((sep-len (length sep))
            (s-len (length s)))
        (if (= sep-len 0)
            ;; Empty separator: split between each character, with "" prefix
            (cons "" (mapcar #'char-to-string (append s nil)))
            (let ((start 0)
                  (acc nil)
                  (i 0))
              (while (<= (+ i sep-len) s-len)
                (if (string= (substring s i (+ i sep-len)) sep)
                    (progn
                      (push (substring s start i) acc)
                      (setq start (+ i sep-len))
                      (setq i start))
                    (setq i (1+ i))))
              (push (substring s start s-len) acc)
              (nreverse acc)))))))

;; tail :: String -> String
(defvar hydra_lib_strings_tail
  (lambda (s)
    "Get all characters of a string except the first."
    (substring s 1)))

;; take :: Int -> String -> String
(defvar hydra_lib_strings_take
  (lambda (n)
    "Take the first n characters from a string."
    (lambda (s)
      (substring s 0 (min n (length s))))))

;; to_list :: String -> [Int32] (string to codepoints)
(defvar hydra_lib_strings_to_list
  (lambda (s)
    "Convert a string to a list of Unicode code points."
    (string-to-list (hydra--ensure-multibyte s))))

;; to_lower :: String -> String
(defvar hydra_lib_strings_to_lower
  (lambda (s)
    "Convert a string to lowercase."
    (downcase (hydra--ensure-multibyte s))))

;; to_upper :: String -> String
(defvar hydra_lib_strings_to_upper
  (lambda (s)
    "Convert a string to uppercase."
    (upcase (hydra--ensure-multibyte s))))

;; unlines :: [String] -> String
(defvar hydra_lib_strings_unlines
  (lambda (strs)
    "Join a list of strings with newlines, appending a trailing newline."
    (apply #'concat
           (mapcar (lambda (s) (concat s "\n")) strs))))

;; unwords :: [String] -> String
(defvar hydra_lib_strings_unwords
  (lambda (strs)
    "Join a list of strings with spaces."
    (mapconcat #'identity strs " ")))

;; words :: String -> [String]
(defvar hydra_lib_strings_words
  (lambda (s)
    "Split a string into words by whitespace."
    (let ((chars (append s nil))
          (current nil)
          (acc nil))
      (dolist (c chars)
        (if (or (= c ?\s) (= c ?\t) (= c ?\n)
                (= c ?\r) (= c ?\f))
            (when current
              (push (apply #'string (nreverse current)) acc)
              (setq current nil))
            (push c current)))
      (when current
        (push (apply #'string (nreverse current)) acc))
      (nreverse acc))))

(provide 'hydra.lib.strings)
