;;; Minimal JSON reader for Common Lisp (SBCL)
;;; Parses JSON files to native CL structures:
;;;   objects -> alists, arrays -> lists, strings -> strings,
;;;   numbers -> numbers, true/false -> t/nil, null -> :null
(in-package :cl-user)

(defun json-skip-whitespace (stream)
  (loop for ch = (peek-char nil stream nil nil)
        while (and ch (member ch '(#\Space #\Tab #\Newline #\Return)))
        do (read-char stream)))

(defun json-read-string (stream)
  (read-char stream) ; consume opening "
  (with-output-to-string (out)
    (loop for ch = (read-char stream)
          do (cond
               ((char= ch #\") (return))
               ((char= ch #\\)
                (let ((esc (read-char stream)))
                  (case esc
                    (#\b (write-char (code-char 8) out))   ; backspace
                    (#\f (write-char (code-char 12) out))  ; form feed
                    (#\n (write-char #\Newline out))
                    (#\t (write-char #\Tab out))
                    (#\r (write-char #\Return out))
                    (#\\ (write-char #\\ out))
                    (#\" (write-char #\" out))
                    (#\/ (write-char #\/ out))
                    (#\u (let ((code (parse-integer
                                       (concatenate 'string
                                         (string (read-char stream))
                                         (string (read-char stream))
                                         (string (read-char stream))
                                         (string (read-char stream)))
                                       :radix 16)))
                           (write-char (code-char code) out)))
                    (t (write-char esc out)))))
               (t (write-char ch out))))))

(defun json-read-number (stream)
  (let ((chars nil))
    (loop for ch = (peek-char nil stream nil nil)
          while (and ch (or (digit-char-p ch)
                            (member ch '(#\- #\+ #\. #\e #\E))))
          do (push (read-char stream) chars))
    (let ((s (coerce (nreverse chars) 'string)))
      (if (or (find #\. s) (find #\e s) (find #\E s))
          (read-from-string s)
          (parse-integer s)))))

(defun json-read-value (stream)
  (json-skip-whitespace stream)
  (let ((ch (peek-char nil stream nil nil)))
    (cond
      ((null ch) (error "Unexpected end of JSON"))
      ((char= ch #\") (json-read-string stream))
      ((char= ch #\{) (json-read-object stream))
      ((char= ch #\[) (json-read-array stream))
      ((char= ch #\t) (read-char stream) (read-char stream)
       (read-char stream) (read-char stream) t)
      ((char= ch #\f) (read-char stream) (read-char stream)
       (read-char stream) (read-char stream) (read-char stream) :json-false)
      ((char= ch #\n) (read-char stream) (read-char stream)
       (read-char stream) (read-char stream) :null)
      ((or (digit-char-p ch) (char= ch #\-))
       (json-read-number stream))
      (t (error "Unexpected character in JSON: ~A" ch)))))

(defun json-read-array (stream)
  (read-char stream) ; consume [
  (json-skip-whitespace stream)
  (if (char= (peek-char nil stream) #\])
      (progn (read-char stream) nil)
      (let ((result nil))
        (loop
          (push (json-read-value stream) result)
          (json-skip-whitespace stream)
          (let ((ch (read-char stream)))
            (cond
              ((char= ch #\]) (return (nreverse result)))
              ((char= ch #\,) nil)
              (t (error "Expected , or ] in JSON array"))))))))

(defun json-read-object (stream)
  (read-char stream) ; consume {
  (json-skip-whitespace stream)
  (if (char= (peek-char nil stream) #\})
      (progn (read-char stream) :json-empty-object)
      (let ((result nil))
        (loop
          (json-skip-whitespace stream)
          (let ((key (json-read-string stream)))
            (json-skip-whitespace stream)
            (read-char stream) ; consume :
            (let ((val (json-read-value stream)))
              (push (cons key val) result)))
          (json-skip-whitespace stream)
          (let ((ch (read-char stream)))
            (cond
              ((char= ch #\}) (return (nreverse result)))
              ((char= ch #\,) nil)
              (t (error "Expected , or } in JSON object"))))))))

(defun json-read-file (path)
  "Read a JSON file and return native CL data structures."
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (json-read-value stream)))
