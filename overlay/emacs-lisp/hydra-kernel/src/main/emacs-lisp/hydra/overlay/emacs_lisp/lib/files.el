;;; files.el --- Hydra Emacs Lisp file primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Emacs Lisp implementations of hydra.lib.files primitives (#494).
;;
;; The Hydra type effect<t> is transparent in the Lisp dialects (effect<t> = t), so these
;; helpers perform real file I/O eagerly and return a Hydra Either<FileError, T>.
;; A recoverable file-system failure becomes (list :left error); success becomes (list :right value).
;; Mirrors the withFileError/classify pattern of the Haskell reference (Hydra.Haskell.Lib.Files)
;; and the Clojure/Scheme/Common-Lisp dialect runtimes.
;;
;; Runtime representations (Emacs Lisp target):
;;   Either        : (list :left v) | (list :right v)
;;   FileError     : (list :<variant> payload), where <variant> is one of
;;                   :already_exists | :invalid_path | :not_found | :other | :permission_denied
;;                   and payload is a FilePath (string) for path-carrying variants, a string for the rest.
;;   FilePath      : a bare string (the FilePath wrap is transparent at the value level).
;;   unit          : nil
;;   binary/bytes  : a unibyte string (a sequence of raw bytes). This matches encode-coding-string
;;                   output and the buffer-string of an insert-file-contents-literally read, and is
;;                   accepted directly by write-region. The literals binary helpers (binary_to_bytes,
;;                   binary_to_string) iterate it uniformly via mapcar/vconcat/aref.
;;
;; All functions are curried, matching the emacs-lisp prim runtime style.

;; ---- Helpers (not primitives) ----

(defun hydra-files-message (e)
  "Best-effort human-readable message for an Emacs error/condition object E."
  (let ((m (error-message-string e)))
    (if (or (null m) (string= m "")) (format "%S" e) m)))

;; Classify an Emacs error condition into a FileError, mirroring the Haskell host's classify.
;; Emacs signals `file-already-exists', `file-missing' and the broad `file-error'; finer ENOENT/
;; EACCES distinctions are only available in the textual message, so we inspect it best-effort.
(defun hydra-files-classify (path e)
  (let ((sym (car-safe e))
        (msg (downcase (hydra-files-message e))))
    (cond
     ((eq sym 'file-already-exists) (list :already_exists path))
     ((eq sym 'file-missing) (list :not_found path))
     ((or (string-match-p "already exist" msg) (string-match-p "file exists" msg))
      (list :already_exists path))
     ((or (string-match-p "no such" msg) (string-match-p "does not exist" msg)
          (string-match-p "not found" msg) (string-match-p "nonexistent" msg))
      (list :not_found path))
     ((or (string-match-p "permission" msg) (string-match-p "denied" msg)
          (string-match-p "not permitted" msg))
      (list :permission_denied path))
     (t (list :other (hydra-files-message e))))))

;; Run a file-system action, translating any error into a FileError.
(defmacro hydra-files-with-error (path &rest body)
  (declare (indent 1))
  (let ((p (make-symbol "p")) (e (make-symbol "e")))
    `(let ((,p ,path))
       (condition-case ,e
           (list :right (progn ,@body))
         (error (list :left (hydra-files-classify ,p ,e)))))))

;; ---- Primitives ----

;; appendFile :: FilePath -> binary -> effect<Either<FileError, unit>>
(defvar hydra_overlay_emacs_lisp_lib_files_append_file
  (lambda (path)
    (lambda (contents)
      (hydra-files-with-error path
        (let ((coding-system-for-write 'no-conversion))
          (write-region (string-as-unibyte (concat contents)) nil path t 'silent))
        nil))))

;; createDirectory :: Bool -> FilePath -> effect<Either<FileError, unit>>
;; recursive = mkdir -p (make-directory with PARENTS); non-recursive = plain mkdir, which signals
;; if the parent is missing (file-missing) or the directory already exists (file-already-exists).
(defvar hydra_overlay_emacs_lisp_lib_files_create_directory
  (lambda (recursive)
    (lambda (path)
      (hydra-files-with-error path
        (make-directory path (and recursive t))
        nil))))

;; exists :: FilePath -> effect<Either<FileError, Bool>>
(defvar hydra_overlay_emacs_lisp_lib_files_exists
  (lambda (path)
    (hydra-files-with-error path
      (if (file-exists-p path) t nil))))

;; listDirectory :: FilePath -> effect<Either<FileError, [FilePath]>>
;; Returns the bare entry names (not full paths), excluding "." and "..", mirroring listDirectory.
(defvar hydra_overlay_emacs_lisp_lib_files_list_directory
  (lambda (path)
    (hydra-files-with-error path
      (unless (file-directory-p path)
        (signal 'file-missing (list "no such directory" path)))
      (sort (directory-files path nil
                             ;; match everything except "." and ".."
                             "\\`[^.]\\|\\`\\.[^.]\\|\\`\\.\\.\\.")
            #'string<))))

;; readFile :: FilePath -> effect<Either<FileError, binary>>
;; Reads raw bytes into a unibyte string via a temp buffer + insert-file-contents-literally.
(defvar hydra_overlay_emacs_lisp_lib_files_read_file
  (lambda (path)
    (hydra-files-with-error path
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path)
        (buffer-string)))))

;; removeFile :: FilePath -> effect<Either<FileError, unit>>
(defvar hydra_overlay_emacs_lisp_lib_files_remove_file
  (lambda (path)
    (hydra-files-with-error path
      (unless (file-exists-p path)
        (signal 'file-missing (list "no such file" path)))
      (delete-file path)
      nil)))

;; rename :: FilePath -> FilePath -> effect<Either<FileError, unit>>
(defvar hydra_overlay_emacs_lisp_lib_files_rename
  (lambda (source)
    (lambda (destination)
      (hydra-files-with-error source
        (rename-file source destination t)
        nil))))

;; writeFile :: FilePath -> binary -> effect<Either<FileError, unit>>
(defvar hydra_overlay_emacs_lisp_lib_files_write_file
  (lambda (path)
    (lambda (contents)
      (hydra-files-with-error path
        (let ((coding-system-for-write 'no-conversion))
          (write-region (string-as-unibyte (concat contents)) nil path nil 'silent))
        nil))))

(provide 'hydra.lib.files)

;;; files.el ends here
