(in-package :cl-user)

;; cat :: [String] -> String
;; Concatenate a list of strings into a single string.
(defvar hydra_lib_strings_cat
  (lambda (strs)
    (apply #'concatenate 'string strs)))

;; cat2 :: String -> String -> String
;; Concatenate two strings.
(defvar hydra_lib_strings_cat2
  (lambda (a)
    (lambda (b)
      (concatenate 'string a b))))

;; cons :: Int32 (char as codepoint) -> String -> String
;; Prepend a character (as a code point) to a string.
(defvar hydra_lib_strings_cons
  (lambda (c)
    (lambda (s)
      (concatenate 'string (string (code-char c)) s))))

;; drop :: Int -> String -> String
;; Drop the first n characters from a string.
(defvar hydra_lib_strings_drop
  (lambda (n)
    (lambda (s)
      (if (>= n (length s))
          ""
          (subseq s n)))))

;; from_list :: [Int32] -> String (codepoints to string)
;; Convert a list of Unicode code points to a string.
(defvar hydra_lib_strings_from_list
  (lambda (codepoints)
    (coerce (mapcar #'code-char codepoints) 'string)))

;; head :: String -> Int32 (char as codepoint)
;; Get the first character of a string as a Unicode code point.
(defvar hydra_lib_strings_head
  (lambda (s)
    (char-code (char s 0))))

;; intercalate :: String -> [String] -> String
;; Join a list of strings with a separator between each element.
(defvar hydra_lib_strings_intercalate
  (lambda (sep)
    (lambda (strs)
      (if (null strs)
          ""
          (let ((acc (car strs)))
            (dolist (s (cdr strs) acc)
              (setf acc (concatenate 'string acc sep s))))))))

;; is_infix_of :: String -> String -> Bool
;; Check whether a string is a substring of another.
(defvar hydra_lib_strings_is_infix_of
  (lambda (needle)
    (lambda (haystack)
      (if (search needle haystack) t nil))))

;; is_prefix_of :: String -> String -> Bool
;; Check whether a string is a prefix of another.
(defvar hydra_lib_strings_is_prefix_of
  (lambda (prefix)
    (lambda (s)
      (let ((plen (length prefix)))
        (and (<= plen (length s))
             (string= s prefix :end1 plen))))))

;; is_suffix_of :: String -> String -> Bool
;; Check whether a string is a suffix of another.
(defvar hydra_lib_strings_is_suffix_of
  (lambda (suffix)
    (lambda (s)
      (let ((slen (length suffix))
            (len (length s)))
        (and (<= slen len)
             (string= s suffix :start1 (- len slen)))))))

;; length :: String -> Int
;; Return the length of a string.
(defvar hydra_lib_strings_length
  (lambda (s)
    (length s)))

;; lines :: String -> [String]
;; Split a string into lines.
;; Haskell semantics: lines "" = [], lines "\n" = [""], lines "a\n" = ["a"]
(defvar hydra_lib_strings_lines
  (lambda (s)
    (if (zerop (length s))
        nil
        (let ((len (length s))
              (acc nil)
              (start 0))
          (loop for i from 0 below len
                when (char= (char s i) #\Newline)
                do (push (subseq s start i) acc)
                   (setf start (1+ i)))
          ;; Only add trailing segment if string doesn't end with newline
          (when (< start len)
            (push (subseq s start len) acc))
          (nreverse acc)))))

;; maybe_char_at :: Int -> String -> Maybe Int
;; Get the Unicode code point at a specific index, returning Nothing if out of bounds.
(defvar hydra_lib_strings_maybe_char_at
  (lambda (n)
    (lambda (s)
      (if (and (>= n 0) (< n (length s)))
          (list :just (char-code (char s n)))
          (list :nothing)))))

;; null :: String -> Bool
;; Check whether a string is empty.
(defvar hydra_lib_strings_null
  (lambda (s)
    (zerop (length s))))

;; replicate :: Int -> String -> String
;; Repeat a string n times.
(defvar hydra_lib_strings_replicate
  (lambda (n)
    (lambda (s)
      (let ((acc ""))
        (dotimes (i n acc)
          (setf acc (concatenate 'string acc s)))))))

;; reverse :: String -> String
;; Reverse a string.
(defvar hydra_lib_strings_reverse
  (lambda (s)
    (reverse s)))

;; split_on :: String -> String -> [String]
;; Split a string on a delimiter string.
;; Haskell semantics: splitOn "" "" = [""], splitOn "" "abc" = ["", "a", "b", "c"]
(defvar hydra_lib_strings_split_on
  (lambda (sep)
    (lambda (s)
      (let ((sep-len (length sep))
            (s-len (length s)))
        (if (zerop sep-len)
            ;; Empty separator: split between each character, with "" prefix
            (cons "" (map 'list #'string (coerce s 'list)))
            (loop with start = 0
                  with acc = nil
                  for i from 0
                  while (<= (+ i sep-len) s-len)
                  do (when (string= s sep :start1 i :end1 (+ i sep-len))
                       (push (subseq s start i) acc)
                       (setf start (+ i sep-len))
                       (setf i (1- start)))
                  finally (push (subseq s start s-len) acc)
                          (return (nreverse acc))))))))

;; tail :: String -> String
;; Get all characters of a string except the first.
(defvar hydra_lib_strings_tail
  (lambda (s)
    (subseq s 1)))

;; take :: Int -> String -> String
;; Take the first n characters from a string.
(defvar hydra_lib_strings_take
  (lambda (n)
    (lambda (s)
      (subseq s 0 (min n (length s))))))

;; to_list :: String -> [Int32] (string to codepoints)
;; Convert a string to a list of Unicode code points.
(defvar hydra_lib_strings_to_list
  (lambda (s)
    (map 'list #'char-code s)))

;; to_lower :: String -> String
;; Convert a string to lowercase.
(defvar hydra_lib_strings_to_lower
  (lambda (s)
    (string-downcase s)))

;; to_upper :: String -> String
;; Convert a string to uppercase.
(defvar hydra_lib_strings_to_upper
  (lambda (s)
    (string-upcase s)))

;; unlines :: [String] -> String
;; Join a list of strings with newlines, appending a trailing newline.
(defvar hydra_lib_strings_unlines
  (lambda (strs)
    (apply #'concatenate 'string
           (mapcar (lambda (s) (concatenate 'string s (string #\Newline))) strs))))

;; unwords :: [String] -> String
;; Join a list of strings with spaces.
(defvar hydra_lib_strings_unwords
  (lambda (strs)
    (if (null strs)
        ""
        (let ((acc (car strs)))
          (dolist (s (cdr strs) acc)
            (setf acc (concatenate 'string acc " " s)))))))

;; words :: String -> [String]
;; Split a string into words by whitespace.
(defvar hydra_lib_strings_words
  (lambda (s)
    (let ((chars (coerce s 'list))
          (current nil)
          (acc nil))
      (dolist (c chars)
        (if (or (char= c #\Space) (char= c #\Tab) (char= c #\Newline)
                (char= c #\Return) (char= c #\Page))
            (when current
              (push (coerce (nreverse current) 'string) acc)
              (setf current nil))
            (push c current)))
      (when current
        (push (coerce (nreverse current) 'string) acc))
      (nreverse acc))))
