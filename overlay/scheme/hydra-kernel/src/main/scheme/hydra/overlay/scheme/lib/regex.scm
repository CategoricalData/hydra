(define-library (hydra overlay scheme lib regex)
  (import (scheme base)
          (scheme char)
          (ice-9 regex)
          (hydra parse regex)
          (hydra print posix regex))
  (export hydra_overlay_scheme_lib_regex_matches
          hydra_overlay_scheme_lib_regex_find
          hydra_overlay_scheme_lib_regex_find_all
          hydra_overlay_scheme_lib_regex_replace
          hydra_overlay_scheme_lib_regex_replace_all
          hydra_overlay_scheme_lib_regex_split)
  (begin

    ;; Patterns are Hydra-defined and translingual (docs/specification/regex.md). Each primitive
    ;; first runs the pattern through hydra.parse.regex, then renders the AST to POSIX ERE syntax via
    ;; hydra.print.posix.regex (Guile (ice-9 regex) is POSIX ERE), before handing the rendered
    ;; pattern to the native engine. An ill-formed pattern (rejected by hydra.parse.regex) is treated
    ;; as "no match" -- the same portable-failure convention as an empty match. See issue #603.

    ;; Returns (list 'given <native-pattern-string>), or (list 'none) if the pattern does not parse.
    (define hydra--regex-to-native
      (lambda (pattern)
        (let ((parsed (hydra_parse_regex_parse_regex pattern)))
          (if (eq? (car parsed) 'given)
              (list 'given (hydra_print_posix_regex_print_regex (cadr parsed)))
              (list 'none)))))

    ;; matches :: String -> String -> Bool
    ;; Full match: pattern must match the entire input
    (define hydra_overlay_scheme_lib_regex_matches
      (lambda (pattern)
        (lambda (input)
          (let ((native (hydra--regex-to-native pattern)))
            (if (eq? (car native) 'none)
                #f
                (let ((m (string-match (string-append "^" (cadr native) "$") input)))
                  (if m #t #f)))))))

    ;; find :: String -> String -> Maybe String
    ;; Returns (list 'given match) or (list 'none)
    (define hydra_overlay_scheme_lib_regex_find
      (lambda (pattern)
        (lambda (input)
          (let ((native (hydra--regex-to-native pattern)))
            (if (eq? (car native) 'none)
                (list 'none)
                (let ((m (string-match (cadr native) input)))
                  (if m
                      (list 'given (match:substring m))
                      (list 'none))))))))

    ;; findAll :: String -> String -> [String]
    (define hydra_overlay_scheme_lib_regex_find_all
      (lambda (pattern)
        (lambda (input)
          (let ((native (hydra--regex-to-native pattern)))
            (if (eq? (car native) 'none)
                '()
                (let ((native-pattern (cadr native)))
                  (let loop ((start 0) (acc '()))
                    (let ((m (string-match native-pattern input start)))
                      (if (or (not m) (= (match:start m) (match:end m)))
                          (reverse acc)
                          (loop (match:end m) (cons (match:substring m) acc)))))))))))

    ;; replace :: String -> String -> String -> String
    ;; Replace first occurrence
    (define hydra_overlay_scheme_lib_regex_replace
      (lambda (pattern)
        (lambda (replacement)
          (lambda (input)
            (let ((native (hydra--regex-to-native pattern)))
              (if (eq? (car native) 'none)
                  input
                  (let ((m (string-match (cadr native) input)))
                    (if m
                        (string-append (match:prefix m) replacement (match:suffix m))
                        input))))))))

    ;; replaceAll :: String -> String -> String -> String
    ;; Replace all occurrences
    (define hydra_overlay_scheme_lib_regex_replace_all
      (lambda (pattern)
        (lambda (replacement)
          (lambda (input)
            (let ((native (hydra--regex-to-native pattern)))
              (if (eq? (car native) 'none)
                  input
                  (let ((native-pattern (cadr native)))
                    (let loop ((remaining input) (acc ""))
                      (let ((m (string-match native-pattern remaining)))
                        (if (or (not m) (= (match:start m) (match:end m)))
                            (string-append acc remaining)
                            (loop (match:suffix m)
                                  (string-append acc (match:prefix m) replacement)))))))))))

    ;; split :: String -> String -> [String]
    (define hydra_overlay_scheme_lib_regex_split
      (lambda (pattern)
        (lambda (input)
          (let ((native (hydra--regex-to-native pattern)))
            (if (eq? (car native) 'none)
                (list input)
                (let ((native-pattern (cadr native)))
                  (let loop ((remaining input) (acc '()))
                    (let ((m (string-match native-pattern remaining)))
                      (if (or (not m) (= (match:start m) (match:end m)))
                          (reverse (cons remaining acc))
                          (loop (match:suffix m)
                                (cons (match:prefix m) acc)))))))))))))
