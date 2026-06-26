(ns hydra.overlay.clojure.lib.files
  (:import [java.io IOException FileNotFoundException]
           [java.nio.file Files Paths LinkOption OpenOption StandardOpenOption
            AccessDeniedException FileAlreadyExistsException NoSuchFileException
            NotDirectoryException InvalidPathException]))

;; Clojure implementations of hydra.lib.files primitives (#494).
;;
;; The Hydra type effect<t> is transparent in the Lisp dialects (effect<t> = t), so these
;; helpers perform real java.nio file I/O eagerly and return a Hydra Either<FileError, T>.
;; A recoverable file-system failure becomes (list :left error); success becomes (list :right value).
;; Mirrors the withFileError/classify pattern of the Haskell reference (Hydra.Haskell.Lib.Files).
;;
;; Runtime representations (Clojure target):
;;   Either        : (list :left v) | (list :right v)
;;   FileError     : (list :<variant> payload), where <variant> is one of
;;                   :already_exists | :invalid_path | :not_found | :other | :permission_denied
;;                   and payload is a FilePath (string) for path-carrying variants, a string for the rest.
;;   FilePath      : a bare string (the FilePath wrap is transparent at the value level).
;;   unit          : nil
;;   binary        : a Clojure vector/seq of byte ints (each 0..255), matching the
;;                   Clojure target's binary representation (see hydra.overlay.clojure.lib.literals).
;;                   Convert to/from a Java byte-array only for the actual java.nio I/O.
;;
;; All functions are curried, matching the Clojure prim runtime style.

;; ---- Helpers (not primitives) ----

;; binary (vector of ints 0..255) -> Java byte-array, for I/O.
(defn- binary->bytes [contents]
  (byte-array (map unchecked-byte contents)))

;; Java byte-array -> binary (vector of unsigned ints 0..255).
(defn- bytes->binary [^bytes ba]
  (vec (map #(bit-and % 0xff) ba)))

(defn- message [^Exception e]
  (let [m (.getMessage e)]
    (if (nil? m) (.getSimpleName (class e)) m)))

;; Classify an IOException into a FileError, mirroring the Haskell/Scala host's classify.
(defn- classify [path ^IOException e]
  (cond
    (instance? FileAlreadyExistsException e) (list :already_exists path)
    (instance? NoSuchFileException e)        (list :not_found path)
    (instance? FileNotFoundException e)      (list :not_found path)
    (instance? NotDirectoryException e)      (list :not_found path)
    (instance? AccessDeniedException e)      (list :permission_denied path)
    :else                                    (list :other (message e))))

;; Run a file-system action, translating any IOException into the appropriate FileError.
(defn- with-file-error [path action]
  (try
    (list :right (action))
    (catch InvalidPathException e (list :left (list :invalid_path (message e))))
    (catch IOException e          (list :left (classify path e)))
    (catch Exception e            (list :left (list :other (message e))))))

(defn- path-of [s] (Paths/get s (into-array String [])))

;; ---- Primitives ----

;; appendFile :: FilePath -> binary -> effect<Either<FileError, unit>>
(def hydra_lib_files_append_file
  "Append raw bytes to the end of a file, creating it if absent."
  (fn [path] (fn [contents]
    (with-file-error path
      (fn []
        (Files/write (path-of path) (binary->bytes contents)
          (into-array OpenOption [StandardOpenOption/CREATE StandardOpenOption/APPEND]))
        nil)))))

;; createDirectory :: Bool -> FilePath -> effect<Either<FileError, unit>>
(def hydra_lib_files_create_directory
  "Create a directory; when recursive, create missing parents (mkdir -p)."
  (fn [recursive] (fn [path]
    (with-file-error path
      (fn []
        (let [p (path-of path)]
          (if recursive
            (Files/createDirectories p (into-array java.nio.file.attribute.FileAttribute []))
            (Files/createDirectory p (into-array java.nio.file.attribute.FileAttribute []))))
        nil)))))

;; exists :: FilePath -> effect<Either<FileError, Bool>>
(def hydra_lib_files_exists
  "Test whether anything exists at the given path (no error on absence)."
  (fn [path]
    (with-file-error path
      (fn [] (Files/exists (path-of path) (into-array LinkOption []))))))

;; listDirectory :: FilePath -> effect<Either<FileError, [FilePath]>>
;; Returns the bare entry names (not full paths), mirroring the Haskell listDirectory.
(def hydra_lib_files_list_directory
  "List the immediate entries of a directory as bare names."
  (fn [path]
    (with-file-error path
      (fn []
        (let [stream (Files/list (path-of path))]
          (try
            (->> (.iterator stream)
                 iterator-seq
                 (mapv (fn [p]
                         (let [n (.getFileName p)]
                           (if (nil? n) (.toString p) (.toString n))))))
            (finally (.close stream))))))))

;; readFile :: FilePath -> effect<Either<FileError, binary>>
(def hydra_lib_files_read_file
  "Read the entire contents of a file as raw bytes."
  (fn [path]
    (with-file-error path
      (fn [] (bytes->binary (Files/readAllBytes (path-of path)))))))

;; removeFile :: FilePath -> effect<Either<FileError, unit>>
(def hydra_lib_files_remove_file
  "Remove a file (POSIX unlink)."
  (fn [path]
    (with-file-error path
      (fn [] (Files/delete (path-of path)) nil))))

;; rename :: FilePath -> FilePath -> effect<Either<FileError, unit>>
(def hydra_lib_files_rename
  "Rename or move a file or directory from source to destination."
  (fn [source] (fn [destination]
    (with-file-error source
      (fn []
        (Files/move (path-of source) (path-of destination)
          (into-array java.nio.file.CopyOption []))
        nil)))))

;; writeFile :: FilePath -> binary -> effect<Either<FileError, unit>>
(def hydra_lib_files_write_file
  "Replace the file at path with the raw bytes contents, creating it if necessary."
  (fn [path] (fn [contents]
    (with-file-error path
      (fn []
        (Files/write (path-of path) (binary->bytes contents) (into-array OpenOption []))
        nil)))))
