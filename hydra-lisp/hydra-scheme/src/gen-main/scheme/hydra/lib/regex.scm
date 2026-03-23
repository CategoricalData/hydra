(define-library (hydra lib regex)
  (import (scheme base)
          (scheme char))
  (export hydra_lib_regex_matches
          hydra_lib_regex_find
          hydra_lib_regex_find_all
          hydra_lib_regex_replace
          hydra_lib_regex_replace_all
          hydra_lib_regex_split)
  (begin

    ;; Note: R7RS does not include a standard regex library.
    ;; These are stub implementations that use simple string matching.
    ;; A full implementation would require an SRFI or platform-specific library.

    ;; Helper: simple substring search
    (define (string-contains haystack needle)
      (let ((hlen (string-length haystack))
            (nlen (string-length needle)))
        (if (= nlen 0)
            0
            (let loop ((i 0))
              (cond
                ((> (+ i nlen) hlen) #f)
                ((string=? (substring haystack i (+ i nlen)) needle) i)
                (else (loop (+ i 1))))))))

    ;; matches :: String -> String -> Bool
    ;; Stub: exact string equality (no regex parsing in base R7RS)
    (define hydra_lib_regex_matches
      (lambda (pattern)
        (lambda (input)
          (string=? pattern input))))

    ;; find :: String -> String -> Maybe String
    ;; Stub: literal substring search
    (define hydra_lib_regex_find
      (lambda (pattern)
        (lambda (input)
          (let ((pos (string-contains input pattern)))
            (if pos
                (substring input pos (+ pos (string-length pattern)))
                #f)))))

    ;; find_all :: String -> String -> [String]
    ;; Stub: finds all non-overlapping literal occurrences
    (define hydra_lib_regex_find_all
      (lambda (pattern)
        (lambda (input)
          (let ((plen (string-length pattern))
                (ilen (string-length input)))
            (if (= plen 0)
                '()
                (let loop ((i 0) (acc '()))
                  (cond
                    ((> (+ i plen) ilen) (reverse acc))
                    ((string=? (substring input i (+ i plen)) pattern)
                     (loop (+ i plen) (cons pattern acc)))
                    (else (loop (+ i 1) acc)))))))))

    ;; replace :: String -> String -> String -> String
    ;; Stub: replaces first literal occurrence
    (define hydra_lib_regex_replace
      (lambda (pattern)
        (lambda (replacement)
          (lambda (input)
            (let ((pos (string-contains input pattern)))
              (if pos
                  (string-append
                    (substring input 0 pos)
                    replacement
                    (substring input (+ pos (string-length pattern)) (string-length input)))
                  input))))))

    ;; replace_all :: String -> String -> String -> String
    ;; Stub: replaces all literal occurrences
    (define hydra_lib_regex_replace_all
      (lambda (pattern)
        (lambda (replacement)
          (lambda (input)
            (let ((plen (string-length pattern)))
              (if (= plen 0)
                  input
                  (let loop ((s input))
                    (let ((pos (string-contains s pattern)))
                      (if pos
                          (loop (string-append
                                  (substring s 0 pos)
                                  replacement
                                  (substring s (+ pos plen) (string-length s))))
                          s)))))))))

    ;; split :: String -> String -> [String]
    ;; Stub: splits on literal delimiter
    (define hydra_lib_regex_split
      (lambda (pattern)
        (lambda (input)
          (let ((plen (string-length pattern))
                (ilen (string-length input)))
            (if (= plen 0)
                (list input)
                (let loop ((i 0) (start 0) (acc '()))
                  (cond
                    ((> (+ i plen) ilen)
                     (reverse (cons (substring input start ilen) acc)))
                    ((string=? (substring input i (+ i plen)) pattern)
                     (loop (+ i plen) (+ i plen)
                           (cons (substring input start i) acc)))
                    (else
                     (loop (+ i 1) start acc)))))))))))
