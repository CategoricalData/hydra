(in-package :cl-user)

;; Common Lisp implementations of hydra.lib.files primitives (#494).
;;
;; The Hydra type effect<t> is transparent in the Lisp dialects (effect<t> = t), so these
;; helpers perform real file I/O eagerly and return a Hydra Either<FileError, T>.
;; A recoverable file-system failure becomes (list :left error); success becomes (list :right value).
;; Mirrors the withFileError/classify pattern of the Haskell reference (Hydra.Haskell.Lib.Files)
;; and the Clojure/Scheme dialect runtimes.
;;
;; Runtime representations (Common Lisp target):
;;   Either        : (list :left v) | (list :right v)
;;   FileError     : (list :<variant> payload), where <variant> is one of
;;                   :already_exists | :invalid_path | :not_found | :other | :permission_denied
;;                   and payload is a FilePath (string) for path-carrying variants, a string for the rest.
;;   FilePath      : a bare string (the FilePath wrap is transparent at the value level).
;;   unit          : nil
;;   binary/bytes  : a vector of (unsigned-byte 8). Writes accept any byte sequence (coerced).
;;
;; Portability: this runtime is loaded by the loader BEFORE the test harness pulls in ASDF/UIOP,
;; so it relies only on portable ANSI CL (directory / ensure-directories-exist / delete-file /
;; rename-file / probe-file / with-open-file). All functions are curried, matching the CL prim
;; runtime style.

;; ---- Helpers (not primitives) ----

(defun hydra-files-message (e)
  (let ((m (princ-to-string e)))
    (if (or (null m) (string= m "")) (string (type-of e)) m)))

;; Classify a condition into a FileError, mirroring the Haskell/Scala host's classify.
;; ANSI CL exposes only the broad FILE-ERROR condition (no fine-grained ENOENT/EACCES/EEXIST
;; subclasses), so we inspect the textual message best-effort, defaulting to :other.
(defun hydra-files-classify (path e)
  (let ((msg (string-downcase (hydra-files-message e))))
    (cond
      ((or (search "already exist" msg) (search "exists" msg)) (list :already_exists path))
      ((or (search "does not exist" msg) (search "no such" msg)
           (search "not found" msg) (search "nonexistent" msg)
           (search "deletion" msg)) (list :not_found path))
      ((or (search "permission" msg) (search "denied" msg)
           (search "not permitted" msg) (search "access" msg)) (list :permission_denied path))
      (t (list :other (hydra-files-message e))))))

;; Run a file-system action, translating any FILE-ERROR (or other error) into a FileError.
(defmacro hydra-files-with-error (path &body body)
  (let ((p (gensym)) (e (gensym)))
    `(let ((,p ,path))
       (handler-case
           (list :right (progn ,@body))
         (file-error (,e) (list :left (hydra-files-classify ,p ,e)))
         (error (,e)     (list :left (hydra-files-classify ,p ,e)))))))

;; ---- Primitives ----

;; appendFile :: FilePath -> binary -> effect<Either<FileError, unit>>
(defvar hydra_overlay_common_lisp_lib_files_append_file
  (lambda (path)
    (lambda (contents)
      (hydra-files-with-error path
        (with-open-file (out path :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists :append
                                  :if-does-not-exist :create)
          (write-sequence (coerce contents '(vector (unsigned-byte 8))) out))
        nil))))

;; createDirectory :: Bool -> FilePath -> effect<Either<FileError, unit>>
;; ensure-directories-exist always creates missing parents; when recursive is false we first
;; verify the parent already exists and signal :not_found otherwise (mkdir, not mkdir -p).
(defvar hydra_overlay_common_lisp_lib_files_create_directory
  (lambda (recursive)
    (lambda (path)
      (hydra-files-with-error path
        (let* ((dirpath (if (char= (char path (1- (length path))) #\/)
                            path
                            (concatenate 'string path "/")))
               (pn (pathname dirpath)))
          (unless recursive
            (let ((parent (make-pathname :directory (butlast (pathname-directory pn))
                                         :name nil :type nil)))
              (unless (probe-file parent)
                (error "directory does not exist (no recursive create): ~A" path))))
          (ensure-directories-exist dirpath))
        nil))))

;; exists :: FilePath -> effect<Either<FileError, Bool>>
(defvar hydra_overlay_common_lisp_lib_files_exists
  (lambda (path)
    (hydra-files-with-error path
      (if (probe-file path) t nil))))

;; listDirectory :: FilePath -> effect<Either<FileError, [FilePath]>>
;; Returns the bare entry names (not full paths), mirroring the Haskell listDirectory.
(defvar hydra_overlay_common_lisp_lib_files_list_directory
  (lambda (path)
    (hydra-files-with-error path
      (let* ((dirpath (if (char= (char path (1- (length path))) #\/)
                          path
                          (concatenate 'string path "/")))
             (base (pathname dirpath)))
        (unless (probe-file base)
          (error "no such directory: ~A" path))
        (let ((files (directory (merge-pathnames
                                  (make-pathname :name :wild :type :wild) base)))
              (subdirs (directory (merge-pathnames
                                    (make-pathname :directory '(:relative :wild)) base)))
              (names nil))
          ;; Regular files: file-namestring gives "name.type".
          (dolist (f files)
            (let ((n (file-namestring f)))
              (when (and n (not (string= n ""))) (push n names))))
          ;; Immediate subdirectories: last directory component is the bare name.
          (dolist (d subdirs)
            (let ((comp (car (last (pathname-directory d)))))
              (when (stringp comp) (push comp names))))
          (sort (remove-duplicates names :test #'string=) #'string<))))))

;; readFile :: FilePath -> effect<Either<FileError, binary>>
(defvar hydra_overlay_common_lisp_lib_files_read_file
  (lambda (path)
    (hydra-files-with-error path
      (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
        (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
          (read-sequence buf in)
          buf)))))

;; removeFile :: FilePath -> effect<Either<FileError, unit>>
(defvar hydra_overlay_common_lisp_lib_files_remove_file
  (lambda (path)
    (hydra-files-with-error path
      (unless (probe-file path)
        (error "no such file: ~A" path))
      (delete-file path)
      nil)))

;; rename :: FilePath -> FilePath -> effect<Either<FileError, unit>>
(defvar hydra_overlay_common_lisp_lib_files_rename
  (lambda (source)
    (lambda (destination)
      (hydra-files-with-error source
        ;; rename-file resolves the new name against the old; pass an absolute namestring so the
        ;; destination directory is honored verbatim.
        (rename-file source (merge-pathnames destination))
        nil))))

;; writeFile :: FilePath -> binary -> effect<Either<FileError, unit>>
(defvar hydra_overlay_common_lisp_lib_files_write_file
  (lambda (path)
    (lambda (contents)
      (hydra-files-with-error path
        (with-open-file (out path :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
          (write-sequence (coerce contents '(vector (unsigned-byte 8))) out))
        nil))))
