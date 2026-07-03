(define-library (hydra overlay scheme lib files)
  ;; The Scheme head is guile-based (see hydra.scheme.lib.maps / .sets, which likewise import
  ;; guile-specific primitives unconditionally, and packages/hydra-lisp/bin/run-tests.sh, which runs
  ;; the suite under guile). Directory operations are not part of R7RS, so they are taken from guile
  ;; here. A chibi-scheme port would substitute (chibi filesystem)'s create-directory / directory-files
  ;; / rename-file / delete-file.
  (import (scheme base)
          (scheme file)
          (scheme write)
          (only (guile)
                mkdir rmdir rename-file copy-file resolve-module eval
                opendir readdir closedir
                stat stat:type stat:size stat:mtime stat:mtimensec
                stat:atime stat:atimensec stat:ctime stat:ctimensec))
  (export hydra_lib_files_copy
          hydra_lib_files_append_file
          hydra_lib_files_create_directory
          hydra_lib_files_exists
          hydra_lib_files_list_directory
          hydra_lib_files_read_file
          hydra_lib_files_remove_directory
          hydra_lib_files_remove_file
          hydra_lib_files_rename
          hydra_lib_files_status
          hydra_lib_files_write_file)
  (begin

    ;; Scheme (R7RS) implementations of hydra.lib.files primitives (#494).
    ;;
    ;; The Hydra type effect<t> is transparent in the Lisp dialects (effect<t> = t), so these
    ;; helpers perform real file I/O eagerly and return a Hydra Either<FileError, T>.
    ;; A recoverable file-system failure becomes (list 'left error); success becomes (list 'right value).
    ;; Mirrors the withFileError/classify pattern of the Haskell reference (Hydra.Haskell.Lib.Files).
    ;;
    ;; Runtime representations (Scheme target):
    ;;   Either        : (list 'left v) | (list 'right v)
    ;;   FileError     : (list '<variant> payload), where <variant> is one of
    ;;                   already_exists | invalid_path | not_found | other | permission_denied
    ;;                   and payload is a FilePath (string) for path-carrying variants, a string for the rest.
    ;;   FilePath      : a bare string (the FilePath wrap is transparent at the value level).
    ;;   unit          : '() (the empty list / nil).
    ;;   binary/bytes  : a Scheme vector of byte ints (e.g. #(65 66)), matching the
    ;;                   hydra.lib.literals binary representation. Helpers below also accept a
    ;;                   list-of-ints or a bytevector defensively, but file I/O converts to a
    ;;                   bytevector internally and readFile returns a vector-of-ints.
    ;;
    ;; All functions are curried, matching the Scheme prim runtime style.
    ;;
    ;; Directory operations are not part of R7RS, so they are taken from guile (the head's Scheme
    ;; implementation; see the imports above and packages/hydra-lisp/bin/run-tests.sh scheme). R7RS
    ;; also lacks a portable way to inspect the errno of a failed operation, so error classification
    ;; is best-effort: it scans the libc strerror text guile attaches to its system-error conditions,
    ;; and any failure it cannot classify becomes FileError.other with the condition's message.

    ;; ---- Helpers (not primitives) ----

    ;; Convert a Hydra binary value to a bytevector for file I/O. The Scheme target represents
    ;; binary as a vector of byte ints (#(65 66)); accept a list-of-ints or an existing bytevector
    ;; too, mirroring the polymorphic normalization in hydra.lib.literals.
    (define (binary->bytevector b)
      (cond
        ((bytevector? b) b)
        ((vector? b)
         (let ((bv (make-bytevector (vector-length b))))
           (do ((i 0 (+ i 1)))
               ((= i (vector-length b)) bv)
             (bytevector-u8-set! bv i (vector-ref b i)))))
        ((list? b)
         (let ((bv (make-bytevector (length b))))
           (let loop ((i 0) (xs b))
             (if (null? xs)
                 bv
                 (begin (bytevector-u8-set! bv i (car xs))
                        (loop (+ i 1) (cdr xs)))))))
        (else (error "binary->bytevector: unsupported binary representation" b))))

    ;; Convert a bytevector to the Hydra binary representation (a vector of byte ints).
    (define (bytevector->binary bv)
      (let ((v (make-vector (bytevector-length bv))))
        (do ((i 0 (+ i 1)))
            ((= i (bytevector-length bv)) v)
          (vector-set! v i (bytevector-u8-ref bv i)))))

    (define (err->message e)
      (cond
        ((error-object? e)
         (let ((m (error-object-message e))
               (irr (error-object-irritants e)))
           (if (null? irr)
               (if (string? m) m "file error")
               (string-append (if (string? m) m "file error")
                              ": "
                              (let ((p (open-output-string)))
                                (write irr p)
                                (get-output-string p))))))
        ((string? e) e)
        (else
         (let ((p (open-output-string)))
           (write e p)
           (get-output-string p)))))

    ;; Classify a raised condition into a FileError, mirroring the Haskell/Scala host's classify.
    ;; R7RS lacks a portable way to inspect a failed operation's errno, so this scans the condition's
    ;; message text for known phrases (best-effort). guile's system-error messages carry the libc
    ;; strerror text ("No such file or directory", "File exists", "Permission denied", ...), which is
    ;; enough to distinguish the FileError variants in practice.
    (define (classify path e)
      (let ((m (string-downcase-ascii (err->message e))))
        (cond
          ((substring? "exist" m)       (list 'already_exists path))
          ((substring? "no such" m)     (list 'not_found path))
          ((substring? "not found" m)   (list 'not_found path))
          ((substring? "not a directory" m) (list 'not_found path))
          ((substring? "permission" m)  (list 'permission_denied path))
          ((substring? "denied" m)      (list 'permission_denied path))
          (else (list 'other (err->message e))))))

    (define (string-downcase-ascii s)
      (string-map (lambda (c)
                    (if (and (char>=? c #\A) (char<=? c #\Z))
                        (integer->char (+ 32 (char->integer c)))
                        c))
                  s))

    (define (substring? needle haystack)
      (let ((nl (string-length needle))
            (hl (string-length haystack)))
        (let loop ((i 0))
          (cond
            ((> (+ i nl) hl) #f)
            ((string=? needle (substring haystack i (+ i nl))) #t)
            (else (loop (+ i 1)))))))

    ;; Run a file-system action, translating any raised condition into the appropriate FileError.
    (define (with-file-error path action)
      (guard (e (#t (list 'left (classify path e))))
        (list 'right (action))))

    ;; Read an entire file and return it as a Hydra binary (a vector of byte ints).
    (define (read-file-bytes path)
      (let ((port (open-binary-input-file path)))
        (let loop ((chunks '()))
          (let ((chunk (read-bytevector 65536 port)))
            (if (eof-object? chunk)
                (begin
                  (close-port port)
                  (bytevector->binary (apply bytevector-append (reverse chunks))))
                (loop (cons chunk chunks)))))))

    ;; Write a Hydra binary value to a file, replacing any existing content.
    (define (write-file-bytes path contents)
      (let ((port (open-binary-output-file path)))
        (write-bytevector (binary->bytevector contents) port)
        (close-port port)))

    ;; Append a Hydra binary value to the end of a file, creating it if absent.
    (define (append-file-bytes path contents)
      ;; R7RS open-binary-output-file truncates; emulate append by reading existing bytes first.
      (let ((existing (if (file-exists? path)
                          (binary->bytevector (read-file-bytes path))
                          (bytevector))))
        (let ((port (open-binary-output-file path)))
          (write-bytevector existing port)
          (write-bytevector (binary->bytevector contents) port)
          (close-port port))))

    ;; List the immediate entries of a directory as bare names (no "." / "..").
    (define (list-dir path)
      (let ((dir (opendir path)))
        (let loop ((acc '()))
          (let ((entry (readdir dir)))
            (if (eof-object? entry)
                (begin (closedir dir) (reverse acc))
                (if (or (string=? entry ".") (string=? entry ".."))
                    (loop acc)
                    (loop (cons entry acc))))))))

    (define (make-directory path recursive)
      (if recursive
          (make-directories-recursive path)
          (mkdir path)))

    ;; mkdir -p emulation: create each path prefix that does not already exist.
    (define (make-directories-recursive path)
      (let ((parts (split-on-slash path)))
        (let loop ((segs parts) (prefix (if (and (> (string-length path) 0)
                                                 (char=? (string-ref path 0) #\/))
                                            "/" "")))
          (if (null? segs)
              #t
              (let ((next (if (string=? prefix "")
                              (car segs)
                              (if (string=? prefix "/")
                                  (string-append "/" (car segs))
                                  (string-append prefix "/" (car segs))))))
                (if (string=? (car segs) "")
                    (loop (cdr segs) prefix)
                    (begin
                      (if (not (file-exists? next)) (mkdir next))
                      (loop (cdr segs) next))))))))

    (define (split-on-slash s)
      (let loop ((chars (string->list s)) (cur '()) (acc '()))
        (cond
          ((null? chars)
           (reverse (cons (list->string (reverse cur)) acc)))
          ((char=? (car chars) #\/)
           (loop (cdr chars) '() (cons (list->string (reverse cur)) acc)))
          (else
           (loop (cdr chars) (cons (car chars) cur) acc)))))

    (define (rename-path source destination)
      (rename-file source destination))

    (define (remove-a-file path)
      (delete-file path))

    ;; The generated (hydra file) record constructor is a macro in the loaded env (see the analogous
    ;; note in hydra.overlay.scheme.lib.system), so FileStatus is built by eval'ing the constructor
    ;; form inside the owning module rather than calling it as a procedure.
    (define (make-rec modname ctor . args)
      (eval (cons ctor (map (lambda (a) (list 'quote a)) args)) (resolve-module modname)))

    (define (path-is-directory? path)
      (eq? (stat:type (stat path)) 'directory))

    ;; Recursively copy a directory tree. destination may already exist (mirrors
    ;; createDirectoryIfMissing in the Haskell reference, Hydra.Haskell.Lib.Files:112).
    (define (copy-directory-recursive source destination)
      (if (not (file-exists? destination)) (mkdir destination))
      (for-each
        (lambda (name)
          (let ((src (string-append source "/" name))
                (dst (string-append destination "/" name)))
            (if (path-is-directory? src)
                (copy-directory-recursive src dst)
                (copy-file src dst))))
        (list-dir source)))

    ;; Recursively remove a directory tree.
    (define (remove-directory-recursive path)
      (for-each
        (lambda (name)
          (let ((full (string-append path "/" name)))
            (if (path-is-directory? full)
                (remove-directory-recursive full)
                (delete-file full))))
        (list-dir path))
      (rmdir path))

    ;; POSIX stat type symbol -> hydra.file.FileType union value.
    (define (file-type type-sym)
      (case type-sym
        ((directory) (list 'directory '()))
        ((symlink) (list 'link '()))
        ((char-special) (list 'character '()))
        ((block-special) (list 'block '()))
        ((fifo) (list 'fifo '()))
        ((socket) (list 'socket '()))
        (else (list 'regular '()))))

    ;; ---- Primitives ----

    ;; appendFile :: FilePath -> binary -> effect<Either<FileError, unit>>
    ;; Append raw bytes to the end of a file, creating it if absent.
    (define hydra_lib_files_append_file
      (lambda (path)
        (lambda (contents)
          (with-file-error path
            (lambda ()
              (append-file-bytes path contents)
              '())))))

    ;; copy :: Bool -> FilePath -> FilePath -> effect<Either<FileError, unit>>
    ;; Copy source to destination; when recursive, source may be a directory whose tree is copied.
    (define hydra_lib_files_copy
      (lambda (recursive)
        (lambda (source)
          (lambda (destination)
            (with-file-error source
              (lambda ()
                (let ((source-is-dir (path-is-directory? source)))
                  (cond
                    ((and recursive source-is-dir) (copy-directory-recursive source destination))
                    (source-is-dir (error "is a directory, but recursive is false" source))
                    (else (copy-file source destination))))
                '()))))))

    ;; createDirectory :: Bool -> FilePath -> effect<Either<FileError, unit>>
    ;; Create a directory; when recursive, create missing parents (mkdir -p).
    (define hydra_lib_files_create_directory
      (lambda (recursive)
        (lambda (path)
          (with-file-error path
            (lambda ()
              (make-directory path recursive)
              '())))))

    ;; exists :: FilePath -> effect<Either<FileError, Bool>>
    ;; Test whether anything exists at the given path (no error on absence).
    (define hydra_lib_files_exists
      (lambda (path)
        (with-file-error path
          (lambda () (file-exists? path)))))

    ;; listDirectory :: FilePath -> effect<Either<FileError, [FilePath]>>
    ;; Returns the bare entry names (not full paths), mirroring the Haskell listDirectory.
    (define hydra_lib_files_list_directory
      (lambda (path)
        (with-file-error path
          (lambda () (list-dir path)))))

    ;; readFile :: FilePath -> effect<Either<FileError, binary>>
    ;; Read the entire contents of a file as raw bytes.
    (define hydra_lib_files_read_file
      (lambda (path)
        (with-file-error path
          (lambda () (read-file-bytes path)))))

    ;; removeDirectory :: Bool -> FilePath -> effect<Either<FileError, unit>>
    ;; Remove a directory; when recursive, remove its entire contents (rm -r); otherwise POSIX rmdir.
    (define hydra_lib_files_remove_directory
      (lambda (recursive)
        (lambda (path)
          (with-file-error path
            (lambda ()
              (if recursive
                  (remove-directory-recursive path)
                  (rmdir path))
              '())))))

    ;; removeFile :: FilePath -> effect<Either<FileError, unit>>
    ;; Remove a file (POSIX unlink).
    (define hydra_lib_files_remove_file
      (lambda (path)
        (with-file-error path
          (lambda () (remove-a-file path) '()))))

    ;; rename :: FilePath -> FilePath -> effect<Either<FileError, unit>>
    ;; Rename or move a file or directory from source to destination.
    (define hydra_lib_files_rename
      (lambda (source)
        (lambda (destination)
          (with-file-error source
            (lambda ()
              (rename-path source destination)
              '())))))

    ;; status :: FilePath -> effect<Either<FileError, FileStatus>>
    ;; Retrieve metadata about the file at path (POSIX stat). Symbolic links are followed.
    (define hydra_lib_files_status
      (lambda (path)
        (with-file-error path
          (lambda ()
            (let ((s (stat path)))
              (make-rec '(hydra file) 'make-hydra_file_file_status
                (file-type (stat:type s))
                (stat:size s)
                (make-rec '(hydra time) 'make-hydra_time_timespec (stat:mtime s) (stat:mtimensec s))
                (list 'given (make-rec '(hydra time) 'make-hydra_time_timespec (stat:atime s) (stat:atimensec s)))
                (list 'given (make-rec '(hydra time) 'make-hydra_time_timespec (stat:ctime s) (stat:ctimensec s)))))))))

    ;; writeFile :: FilePath -> binary -> effect<Either<FileError, unit>>
    ;; Replace the file at path with the raw bytes contents, creating it if necessary.
    (define hydra_lib_files_write_file
      (lambda (path)
        (lambda (contents)
          (with-file-error path
            (lambda ()
              (write-file-bytes path contents)
              '())))))))
