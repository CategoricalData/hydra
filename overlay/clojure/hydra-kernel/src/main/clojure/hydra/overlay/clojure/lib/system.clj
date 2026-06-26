(ns hydra.overlay.clojure.lib.system
  (:require [hydra.system :refer :all]
            [hydra.time :refer :all])
  (:import [java.io ByteArrayOutputStream IOException InputStream File]))

;; Clojure implementations of hydra.lib.system primitives (#498).
;;
;; The Hydra type effect<t> is transparent in the Lisp dialects (effect<t> = t), so these helpers
;; perform their work eagerly. Fallible primitives return a Hydra Either: success is (list :right v),
;; a launch failure is (list :left error). Mirrors the classify pattern of the Haskell/Java/Scala hosts.
;;
;; Runtime representations (Clojure target):
;;   Either              : (list :left v) | (list :right v)
;;   Optional            : (list :given v) | (list :none)
;;   SystemError         : (list :<variant> payload), variant one of
;;                         :command_not_found | :permission_denied | :invalid_working_directory
;;                         | :interrupted | :other; payload a FilePath (string) or message string,
;;                         and () for the nullary :interrupted.
;;   Command             : a hydra_system_command defrecord {:program :arguments :working_directory :environment}
;;   ProcessResult       : a hydra_system_process_result defrecord {:exit_code :stdout :stderr}
;;   FilePath/StatusCode/EnvironmentVariable : transparent (bare string / int / string)
;;   binary              : a vector of ints 0..255 (matches hydra.overlay.clojure.lib.files / literals)
;;   unit                : nil
;;
;; All functions are curried, matching the Clojure prim runtime style.

;; ---- Helpers (not primitives) ----

(defn- message [^Exception e]
  (let [m (.getMessage e)]
    (if (nil? m) (.getSimpleName (class e)) m)))

;; Java byte-array -> binary (vector of unsigned ints 0..255).
(defn- bytes->binary [^bytes ba]
  (vec (map #(bit-and % 0xff) ba)))

;; Read an InputStream fully into a Java byte-array.
(defn- read-all [^InputStream in]
  (let [buffer (ByteArrayOutputStream.)
        chunk (byte-array 8192)]
    (loop []
      (let [n (.read in chunk)]
        (when (not= n -1)
          (.write buffer chunk 0 n)
          (recur))))
    (.toByteArray buffer)))

;; Classify a process-launch IOException into a SystemError.
(defn- classify [program ^IOException e]
  (let [lower (.toLowerCase (message e))]
    (cond
      (or (.contains lower "error=2") (.contains lower "no such file"))     (list :command_not_found program)
      (or (.contains lower "error=13") (.contains lower "permission denied")) (list :permission_denied program)
      (or (.contains lower "error=20") (.contains lower "not a directory"))  (list :invalid_working_directory program)
      :else                                                                  (list :other (message e)))))

;; ---- Primitives ----

;; execute :: Command -> effect<Either<SystemError, ProcessResult>>
(def hydra_lib_system_execute
  "Run a program to completion, capturing stdout/stderr (binary) and the exit code."
  (fn [command]
    (let [program (:program command)
          args (:arguments command)
          wd (:working_directory command)
          env (:environment command)
          argv (java.util.ArrayList. ^java.util.Collection (cons program args))
          pb (ProcessBuilder. argv)]
      (when (= (first wd) :given)
        (.directory pb (File. ^String (second wd))))
      (when (= (first env) :given)
        (let [pe (.environment pb)]
          (.clear pe)
          (doseq [[k v] (second env)] (.put pe k v))))
      (try
        (let [process (.start pb)
              out (read-all (.getInputStream process))
              err (read-all (.getErrorStream process))
              code (.waitFor process)]
          (list :right (->hydra_system_process_result code (bytes->binary out) (bytes->binary err))))
        (catch IOException e (list :left (classify program e)))
        (catch InterruptedException _e
          (.interrupt (Thread/currentThread))
          (list :left (list :interrupted)))))))

;; exit :: StatusCode -> effect<unit>
(def hydra_lib_system_exit
  "Terminate the current process immediately with the given status. Does not return."
  (fn [code]
    (System/exit code)
    nil))

;; getEnvironment :: effect<Map<EnvironmentVariable, string>>
;; A nullary effect: generated consumer code references this as a bare value (the effect result), so it
;; is a plain def, not a thunk. (The reducer registry wraps it in a thunk; see register-system.)
(def hydra_lib_system_get_environment
  "Return the entire environment of the current process as a map from variable name to value."
  (into {} (System/getenv)))

;; getEnvironmentVariable :: EnvironmentVariable -> effect<Optional<string>>
(def hydra_lib_system_get_environment_variable
  "Return the value of the named environment variable, or none if it is not set."
  (fn [name]
    (let [v (System/getenv ^String name)]
      (if (nil? v) (list :none) (list :given v)))))

;; getTime :: effect<Timespec>  (nullary effect: a bare value, see getEnvironment note)
(def hydra_lib_system_get_time
  "Return the current wall-clock time as a Timespec (seconds and nanoseconds since the Unix epoch)."
  (let [now (java.time.Instant/now)]
    (->hydra_time_timespec (.getEpochSecond now) (long (.getNano now)))))

;; getWorkingDirectory :: effect<Either<SystemError, FilePath>>  (nullary effect: a bare value)
(def hydra_lib_system_get_working_directory
  "Return the current working directory as a FilePath (string)."
  (let [cwd (System/getProperty "user.dir")]
    (if (nil? cwd)
      (list :left (list :other "user.dir is not set"))
      (list :right cwd))))
