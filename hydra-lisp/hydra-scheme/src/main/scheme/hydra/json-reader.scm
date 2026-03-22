;;; Minimal JSON reader for Scheme
;;; Parses JSON files to native Scheme structures:
;;;   objects -> alists, arrays -> lists, strings -> strings,
;;;   numbers -> numbers, true -> #t, false -> json-false, null -> json-null
;;;
;;; Uses only R7RS-compatible procedures for portability across
;;; Guile, Chibi, Chicken, etc.

(import (scheme base) (scheme read) (scheme write) (scheme file) (scheme char))

(define json-false 'json-false)
(define json-null 'json-null)
(define json-empty-object 'json-empty-object)

(define (json-skip-whitespace port)
  (let loop ()
    (let ((ch (peek-char port)))
      (when (and (not (eof-object? ch))
                 (or (char=? ch #\space)
                     (char=? ch #\tab)
                     (char=? ch #\newline)
                     (char=? ch #\return)))
        (read-char port)
        (loop)))))

(define (json-read-string port)
  (read-char port) ; consume opening "
  (let loop ((acc '()))
    (let ((ch (read-char port)))
      (cond
        ((eof-object? ch) (error "Unterminated JSON string"))
        ((char=? ch #\") (list->string (reverse acc)))
        ((char=? ch #\\)
         (let ((esc (read-char port)))
           (cond
             ((char=? esc #\n) (loop (cons #\newline acc)))
             ((char=? esc #\t) (loop (cons #\tab acc)))
             ((char=? esc #\r) (loop (cons #\return acc)))
             ((char=? esc #\\) (loop (cons #\\ acc)))
             ((char=? esc #\") (loop (cons #\" acc)))
             ((char=? esc #\/) (loop (cons #\/ acc)))
             ((char=? esc #\b) (loop (cons (integer->char 8) acc)))
             ((char=? esc #\f) (loop (cons (integer->char 12) acc)))
             ((char=? esc #\u)
              (let* ((h1 (read-char port))
                     (h2 (read-char port))
                     (h3 (read-char port))
                     (h4 (read-char port))
                     (hex-str (list->string (list h1 h2 h3 h4)))
                     (code (string->number hex-str 16)))
                (loop (cons (integer->char code) acc))))
             (else (loop (cons esc acc))))))
        (else (loop (cons ch acc)))))))

(define (json-number-char? ch)
  (or (char-numeric? ch)
      (char=? ch #\-)
      (char=? ch #\+)
      (char=? ch #\.)
      (char=? ch #\e)
      (char=? ch #\E)))

(define (json-read-number port)
  (let loop ((acc '()))
    (let ((ch (peek-char port)))
      (if (and (not (eof-object? ch)) (json-number-char? ch))
          (begin (read-char port) (loop (cons ch acc)))
          (let* ((s (list->string (reverse acc)))
                 (n (string->number s)))
            (if n n (error "Invalid JSON number" s)))))))

(define (json-read-array port)
  (read-char port) ; consume [
  (json-skip-whitespace port)
  (if (and (not (eof-object? (peek-char port)))
           (char=? (peek-char port) #\]))
      (begin (read-char port) '())
      (let loop ((acc '()))
        (let ((val (json-read-value port)))
          (json-skip-whitespace port)
          (let ((ch (read-char port)))
            (cond
              ((char=? ch #\]) (reverse (cons val acc)))
              ((char=? ch #\,) (loop (cons val acc)))
              (else (error "Expected , or ] in JSON array"))))))))

(define (json-read-object port)
  (read-char port) ; consume {
  (json-skip-whitespace port)
  (if (and (not (eof-object? (peek-char port)))
           (char=? (peek-char port) #\}))
      (begin (read-char port) json-empty-object)
      (let loop ((acc '()))
        (json-skip-whitespace port)
        (let* ((key (json-read-string port)))
          (json-skip-whitespace port)
          (read-char port) ; consume :
          (let ((val (json-read-value port)))
            (json-skip-whitespace port)
            (let ((ch (read-char port)))
              (cond
                ((char=? ch #\}) (reverse (cons (cons key val) acc)))
                ((char=? ch #\,) (loop (cons (cons key val) acc)))
                (else (error "Expected , or } in JSON object")))))))))

(define (json-expect port expected)
  "Read and verify a sequence of expected characters."
  (let loop ((rest (string->list expected)))
    (when (pair? rest)
      (let ((ch (read-char port)))
        (unless (and (not (eof-object? ch)) (char=? ch (car rest)))
          (error "Unexpected character in JSON"))
        (loop (cdr rest))))))

(define (json-read-value port)
  (json-skip-whitespace port)
  (let ((ch (peek-char port)))
    (cond
      ((eof-object? ch) (error "Unexpected end of JSON"))
      ((char=? ch #\") (json-read-string port))
      ((char=? ch #\{) (json-read-object port))
      ((char=? ch #\[) (json-read-array port))
      ((char=? ch #\t) (json-expect port "true") #t)
      ((char=? ch #\f) (json-expect port "false") json-false)
      ((char=? ch #\n) (json-expect port "null") json-null)
      ((or (char-numeric? ch) (char=? ch #\-))
       (json-read-number port))
      (else (error "Unexpected character in JSON" ch)))))

(define (json-read-port port)
  "Read a single JSON value from a port."
  (json-read-value port))

(define (json-read-file path)
  "Read a JSON file and return native Scheme data structures."
  (call-with-input-file path
    (lambda (port)
      (json-read-value port))))
