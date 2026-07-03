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
;; rename-file / probe-file / with-open-file), plus sb-posix (an always-bundled SBCL contrib, no
;; ASDF/UIOP required) for rmdir and stat, which have no portable ANSI CL equivalent. All functions
;; are curried, matching the CL prim runtime style.
#+sbcl (require :sb-posix)

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

;; List a directory's immediate entries as (kind . name) pairs, kind one of :file / :dir.
(defun hydra-files-list-entries (dirpath)
  (let* ((base (pathname dirpath))
         (files (directory (merge-pathnames (make-pathname :name :wild :type :wild) base)))
         (subdirs (directory (merge-pathnames (make-pathname :directory '(:relative :wild)) base)))
         (result nil))
    (dolist (f files)
      (let ((n (file-namestring f)))
        (when (and n (not (string= n ""))) (push (cons :file n) result))))
    (dolist (d subdirs)
      (let ((comp (car (last (pathname-directory d)))))
        (when (stringp comp) (push (cons :dir comp) result))))
    result))

;; Recursively copy a directory tree using only portable ANSI CL.
(defun hydra-files-copy-directory (source destination)
  (let* ((source-dir (if (char= (char source (1- (length source))) #\/)
                         source (concatenate 'string source "/")))
         (dest-dir (if (char= (char destination (1- (length destination))) #\/)
                      destination (concatenate 'string destination "/"))))
    (ensure-directories-exist dest-dir)
    (dolist (entry (hydra-files-list-entries source-dir))
      (let* ((kind (car entry)) (name (cdr entry))
             (src (concatenate 'string source-dir name))
             (dst (concatenate 'string dest-dir name)))
        (if (eq kind :dir)
            (hydra-files-copy-directory src dst)
            (with-open-file (in src :direction :input :element-type '(unsigned-byte 8))
              (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
                (read-sequence buf in)
                (with-open-file (out dst :direction :output :element-type '(unsigned-byte 8)
                                         :if-exists :supersede :if-does-not-exist :create)
                  (write-sequence buf out)))))))))

;; Recursively remove a directory tree using only portable ANSI CL.
(defun hydra-files-remove-directory (path)
  (let ((dirpath (if (char= (char path (1- (length path))) #\/)
                     path (concatenate 'string path "/"))))
    (dolist (entry (hydra-files-list-entries dirpath))
      (let* ((kind (car entry)) (name (cdr entry)) (full (concatenate 'string dirpath name)))
        (if (eq kind :dir)
            (hydra-files-remove-directory full)
            (delete-file full))))
    #+sbcl (sb-posix:rmdir (string-right-trim "/" dirpath))))

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

;; copy :: Bool -> FilePath -> FilePath -> effect<Either<FileError, unit>>
;; When recursive is true, source may be a directory whose entire tree is copied.
(defvar hydra_overlay_common_lisp_lib_files_copy
  (lambda (recursive)
    (lambda (source)
      (lambda (destination)
        (hydra-files-with-error source
          #+sbcl
          (let ((source-is-dir (sb-posix:s-isdir (sb-posix:stat-mode (sb-posix:stat source)))))
            (cond
              ((and recursive source-is-dir) (hydra-files-copy-directory source destination))
              (source-is-dir (error "~A is a directory, but recursive is false" source))
              (t
               (with-open-file (in source :direction :input :element-type '(unsigned-byte 8))
                 (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
                   (read-sequence buf in)
                   (with-open-file (out destination :direction :output :element-type '(unsigned-byte 8)
                                            :if-exists :supersede :if-does-not-exist :create)
                     (write-sequence buf out)))))))
          #-sbcl (error "copy requires SBCL")
          nil)))))

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

;; removeDirectory :: Bool -> FilePath -> effect<Either<FileError, unit>>
;; When recursive is false this corresponds to POSIX rmdir: it fails unless empty.
(defvar hydra_overlay_common_lisp_lib_files_remove_directory
  (lambda (recursive)
    (lambda (path)
      (hydra-files-with-error path
        (if recursive
            (hydra-files-remove-directory path)
            #+sbcl (sb-posix:rmdir (string-right-trim "/" path))
            #-sbcl (error "removeDirectory requires SBCL"))
        nil))))

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

;; Classify an sb-posix stat mode into a hydra.file.FileType union value.
#+sbcl
(defun hydra-files-file-type (mode)
  (cond
    ((sb-posix:s-isdir mode) (list :directory nil))
    ((sb-posix:s-islnk mode) (list :link nil))
    ((sb-posix:s-ischr mode) (list :character nil))
    ((sb-posix:s-isblk mode) (list :block nil))
    ((sb-posix:s-isfifo mode) (list :fifo nil))
    ((sb-posix:s-issock mode) (list :socket nil))
    (t (list :regular nil))))

;; status :: FilePath -> effect<Either<FileError, FileStatus>>
;; Retrieve metadata about the file at path (POSIX stat). Symbolic links are followed.
;; FileStatus is a defstruct (make-hydra_file_file_status :file_type ... :size ... :modification_time
;; ... :access_time ... :status_change_time ...). sb-posix's stat exposes only whole-second
;; timestamps, so nanoseconds is always 0.
(defvar hydra_overlay_common_lisp_lib_files_status
  (lambda (path)
    (hydra-files-with-error path
      #+sbcl
      (let* ((s (sb-posix:stat path))
             (mode (sb-posix:stat-mode s)))
        (make-hydra_file_file_status
          :file_type (hydra-files-file-type mode)
          :size (sb-posix:stat-size s)
          :modification_time (make-hydra_time_timespec :seconds (sb-posix:stat-mtime s) :nanoseconds 0)
          :access_time (list :given (make-hydra_time_timespec :seconds (sb-posix:stat-atime s) :nanoseconds 0))
          :status_change_time (list :given (make-hydra_time_timespec :seconds (sb-posix:stat-ctime s) :nanoseconds 0))))
      #-sbcl (error "status requires SBCL"))))

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
