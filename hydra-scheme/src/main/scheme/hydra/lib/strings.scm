(define-library (hydra lib strings)
  (import (scheme base)
          (scheme char))
  (export hydra_lib_strings_cat
          hydra_lib_strings_cat2
          hydra_lib_strings_char_at
          hydra_lib_strings_cons
          hydra_lib_strings_drop
          hydra_lib_strings_from_list
          hydra_lib_strings_head
          hydra_lib_strings_intercalate
          hydra_lib_strings_is_infix_of
          hydra_lib_strings_is_prefix_of
          hydra_lib_strings_is_suffix_of
          hydra_lib_strings_length
          hydra_lib_strings_lines
          hydra_lib_strings_null
          hydra_lib_strings_replicate
          hydra_lib_strings_reverse
          hydra_lib_strings_split_on
          hydra_lib_strings_tail
          hydra_lib_strings_take
          hydra_lib_strings_to_list
          hydra_lib_strings_to_lower
          hydra_lib_strings_to_upper
          hydra_lib_strings_unlines
          hydra_lib_strings_unwords
          hydra_lib_strings_words)
  (begin

    ;; cat :: [String] -> String
    (define hydra_lib_strings_cat
      (lambda (strs)
        (apply string-append strs)))

    ;; cat2 :: String -> String -> String
    (define hydra_lib_strings_cat2
      (lambda (a)
        (lambda (b)
          (string-append a b))))

    ;; Hydra represents characters as integer code points (int32).
    (define (int->char c) (if (char? c) c (integer->char c)))
    (define (char->int c) (if (char? c) (char->integer c) c))

    ;; char_at :: Int -> String -> Char (as int32 code point)
    (define hydra_lib_strings_char_at
      (lambda (n)
        (lambda (s)
          (char->int (string-ref s n)))))

    ;; cons :: Char (as int32) -> String -> String
    (define hydra_lib_strings_cons
      (lambda (c)
        (lambda (s)
          (string-append (string (int->char c)) s))))

    ;; drop :: Int -> String -> String
    (define hydra_lib_strings_drop
      (lambda (n)
        (lambda (s)
          (if (>= n (string-length s))
              ""
              (substring s n (string-length s))))))

    ;; from_list :: [Char (as int32)] -> String
    (define hydra_lib_strings_from_list
      (lambda (chars)
        (list->string (map int->char chars))))

    ;; head :: String -> Char (as int32 code point)
    (define hydra_lib_strings_head
      (lambda (s)
        (char->int (string-ref s 0))))

    ;; intercalate :: String -> [String] -> String
    (define hydra_lib_strings_intercalate
      (lambda (sep)
        (lambda (strs)
          (if (null? strs)
              ""
              (let loop ((rest (cdr strs)) (acc (car strs)))
                (if (null? rest)
                    acc
                    (loop (cdr rest)
                          (string-append acc sep (car rest)))))))))

    ;; isInfixOf :: String -> String -> Bool
    (define hydra_lib_strings_is_infix_of
      (lambda (needle)
        (lambda (haystack)
          (let ((nlen (string-length needle))
                (hlen (string-length haystack)))
            (if (> nlen hlen)
                #f
                (let loop ((i 0))
                  (cond
                    ((> (+ i nlen) hlen) #f)
                    ((string=? (substring haystack i (+ i nlen)) needle) #t)
                    (else (loop (+ i 1))))))))))

    ;; isPrefixOf :: String -> String -> Bool
    (define hydra_lib_strings_is_prefix_of
      (lambda (prefix)
        (lambda (s)
          (let ((plen (string-length prefix)))
            (and (<= plen (string-length s))
                 (string=? (substring s 0 plen) prefix))))))

    ;; isSuffixOf :: String -> String -> Bool
    (define hydra_lib_strings_is_suffix_of
      (lambda (suffix)
        (lambda (s)
          (let ((slen (string-length suffix))
                (len (string-length s)))
            (and (<= slen len)
                 (string=? (substring s (- len slen) len) suffix))))))

    ;; length :: String -> Int
    (define hydra_lib_strings_length
      (lambda (s)
        (string-length s)))

    ;; lines :: String -> [String]
    ;; Haskell behavior: lines "" = [], lines "hello\n" = ["hello"]
    (define hydra_lib_strings_lines
      (lambda (s)
        (let ((len (string-length s)))
          (if (= len 0)
              '()
              (let loop ((i 0) (start 0) (acc '()))
                (cond
                  ((= i len)
                   (reverse (cons (substring s start i) acc)))
                  ((char=? (string-ref s i) #\newline)
                   (if (= (+ i 1) len)
                       ;; Trailing newline: don't add empty string after it
                       (reverse (cons (substring s start i) acc))
                       (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))))
                  (else
                   (loop (+ i 1) start acc))))))))

    ;; null :: String -> Bool
    (define hydra_lib_strings_null
      (lambda (s)
        (= (string-length s) 0)))

    ;; replicate :: Int -> String -> String
    (define hydra_lib_strings_replicate
      (lambda (n)
        (lambda (s)
          (let loop ((k n) (acc ""))
            (if (<= k 0)
                acc
                (loop (- k 1) (string-append acc s)))))))

    ;; reverse :: String -> String
    (define hydra_lib_strings_reverse
      (lambda (s)
        (list->string (reverse (string->list s)))))

    ;; split_on :: String -> String -> [String]
    ;; Haskell behavior: splitOn "" "abc" = ["", "a", "b", "c"]
    ;;                   splitOn "" "" = [""]
    (define hydra_lib_strings_split_on
      (lambda (sep)
        (lambda (s)
          (let ((sep-len (string-length sep))
                (s-len (string-length s)))
            (if (= sep-len 0)
                (if (= s-len 0)
                    (list "")
                    (cons "" (map string (string->list s))))
                (let loop ((i 0) (start 0) (acc '()))
                  (cond
                    ((> (+ i sep-len) s-len)
                     (reverse (cons (substring s start s-len) acc)))
                    ((string=? (substring s i (+ i sep-len)) sep)
                     (loop (+ i sep-len) (+ i sep-len)
                           (cons (substring s start i) acc)))
                    (else
                     (loop (+ i 1) start acc)))))))))

    ;; tail :: String -> String
    (define hydra_lib_strings_tail
      (lambda (s)
        (substring s 1 (string-length s))))

    ;; take :: Int -> String -> String
    (define hydra_lib_strings_take
      (lambda (n)
        (lambda (s)
          (substring s 0 (min n (string-length s))))))

    ;; to_list :: String -> [Char (as int32 code points)]
    (define hydra_lib_strings_to_list
      (lambda (s)
        (map char->int (string->list s))))

    ;; to_lower :: String -> String
    (define hydra_lib_strings_to_lower
      (lambda (s)
        (list->string
          (map char-downcase (string->list s)))))

    ;; to_upper :: String -> String
    (define hydra_lib_strings_to_upper
      (lambda (s)
        (list->string
          (map char-upcase (string->list s)))))

    ;; unlines :: [String] -> String
    (define hydra_lib_strings_unlines
      (lambda (strs)
        (apply string-append (map (lambda (s) (string-append s "\n")) strs))))

    ;; unwords :: [String] -> String
    (define hydra_lib_strings_unwords
      (lambda (strs)
        (if (null? strs)
            ""
            (let loop ((rest (cdr strs)) (acc (car strs)))
              (if (null? rest)
                  acc
                  (loop (cdr rest) (string-append acc " " (car rest))))))))

    ;; words :: String -> [String]
    (define hydra_lib_strings_words
      (lambda (s)
        (let ((chars (string->list s)))
          (let loop ((rest chars) (current '()) (acc '()))
            (cond
              ((null? rest)
               (if (null? current)
                   (reverse acc)
                   (reverse (cons (list->string (reverse current)) acc))))
              ((char-whitespace? (car rest))
               (if (null? current)
                   (loop (cdr rest) '() acc)
                   (loop (cdr rest) '() (cons (list->string (reverse current)) acc))))
              (else
               (loop (cdr rest) (cons (car rest) current) acc)))))))))
