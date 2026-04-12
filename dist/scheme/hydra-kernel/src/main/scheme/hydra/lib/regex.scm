(define-library (hydra lib regex)
  (import (scheme base)
          (scheme char)
          (ice-9 regex))
  (export hydra_lib_regex_matches
          hydra_lib_regex_find
          hydra_lib_regex_find_all
          hydra_lib_regex_replace
          hydra_lib_regex_replace_all
          hydra_lib_regex_split)
  (begin

    ;; matches :: String -> String -> Bool
    ;; Full match: pattern must match the entire input
    (define hydra_lib_regex_matches
      (lambda (pattern)
        (lambda (input)
          (let ((m (string-match (string-append "^" pattern "$") input)))
            (if m #t #f)))))

    ;; find :: String -> String -> Maybe String
    ;; Returns (list 'just match) or (list 'nothing)
    (define hydra_lib_regex_find
      (lambda (pattern)
        (lambda (input)
          (let ((m (string-match pattern input)))
            (if m
                (list 'just (match:substring m))
                (list 'nothing))))))

    ;; findAll :: String -> String -> [String]
    (define hydra_lib_regex_find_all
      (lambda (pattern)
        (lambda (input)
          (let loop ((start 0) (acc '()))
            (let ((m (string-match pattern input start)))
              (if (or (not m) (= (match:start m) (match:end m)))
                  (reverse acc)
                  (loop (match:end m) (cons (match:substring m) acc))))))))

    ;; replace :: String -> String -> String -> String
    ;; Replace first occurrence
    (define hydra_lib_regex_replace
      (lambda (pattern)
        (lambda (replacement)
          (lambda (input)
            (let ((m (string-match pattern input)))
              (if m
                  (string-append (match:prefix m) replacement (match:suffix m))
                  input))))))

    ;; replaceAll :: String -> String -> String -> String
    ;; Replace all occurrences
    (define hydra_lib_regex_replace_all
      (lambda (pattern)
        (lambda (replacement)
          (lambda (input)
            (let loop ((remaining input) (acc ""))
              (let ((m (string-match pattern remaining)))
                (if (or (not m) (= (match:start m) (match:end m)))
                    (string-append acc remaining)
                    (loop (match:suffix m)
                          (string-append acc (match:prefix m) replacement)))))))))

    ;; split :: String -> String -> [String]
    (define hydra_lib_regex_split
      (lambda (pattern)
        (lambda (input)
          (let loop ((remaining input) (acc '()))
            (let ((m (string-match pattern remaining)))
              (if (or (not m) (= (match:start m) (match:end m)))
                  (reverse (cons remaining acc))
                  (loop (match:suffix m)
                        (cons (match:prefix m) acc))))))))))
