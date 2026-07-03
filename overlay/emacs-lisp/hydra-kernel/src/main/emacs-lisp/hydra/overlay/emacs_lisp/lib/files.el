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

;; copy :: Bool -> FilePath -> FilePath -> effect<Either<FileError, unit>>
;; When recursive is true, source may be a directory whose entire tree is copied.
(defvar hydra_overlay_emacs_lisp_lib_files_copy
  (lambda (recursive)
    (lambda (source)
      (lambda (destination)
        (hydra-files-with-error source
          (if (and recursive (file-directory-p source))
              (copy-directory source destination nil t nil)
            (copy-file source destination t))
          nil)))))

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

;; removeDirectory :: Bool -> FilePath -> effect<Either<FileError, unit>>
;; When recursive is false this corresponds to POSIX rmdir: it fails unless empty.
(defvar hydra_overlay_emacs_lisp_lib_files_remove_directory
  (lambda (recursive)
    (lambda (path)
      (hydra-files-with-error path
        (delete-directory path (and recursive t))
        nil))))

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

;; Emacs time value (any Lisp timestamp form: the legacy (HIGH LOW USEC PSEC) list,
;; a (TICKS . HZ) cons, or a plain integer) -> hydra.time.Timespec. time-convert
;; normalizes to a fixed nanosecond-denominator ratio regardless of Emacs version
;; or the specific form file-attributes returns.
(defun hydra-files-timespec (time)
  (let* ((ns-pair (time-convert time 1000000000))
         (ns-total (car ns-pair)))
    (make-hydra_time_timespec
     :seconds (floor ns-total 1000000000)
     :nanoseconds (mod ns-total 1000000000))))

;; status :: FilePath -> effect<Either<FileError, FileStatus>>
;; Retrieve metadata about the file at path (POSIX stat). Symbolic links are followed
;; (file-attributes on a symlink path with ID-FORMAT nil follows the link, matching stat()).
(defvar hydra_overlay_emacs_lisp_lib_files_status
  (lambda (path)
    (hydra-files-with-error path
      (let ((attrs (file-attributes path)))
        (unless attrs
          (signal 'file-missing (list "no such file or directory" path)))
        (let ((file-type (nth 0 attrs)))
          (make-hydra_file_file_status
           :file_type (cond
                       ((eq file-type t) (list :directory nil))
                       ((stringp file-type) (list :link nil))
                       (t (list :regular nil)))
           :size (nth 7 attrs)
           :modification_time (hydra-files-timespec (nth 5 attrs))
           :access_time (list :given (hydra-files-timespec (nth 4 attrs)))
           :status_change_time (list :given (hydra-files-timespec (nth 6 attrs)))))))))

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
