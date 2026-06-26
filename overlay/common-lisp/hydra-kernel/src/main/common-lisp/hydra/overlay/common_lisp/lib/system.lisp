(in-package :cl-user)

;; Common Lisp implementations of hydra.lib.system primitives (#498).
;;
;; effect<t> is transparent in the Lisp dialects (effect<t> = t). Fallible primitives return a Hydra
;; Either: success is (list :right v), a launch failure is (list :left error). Nullary effects are bare
;; values. Mirrors the classify pattern of the other hosts.
;;
;; Runtime representations (Common Lisp target):
;;   Either              : (list :left v) | (list :right v)
;;   Optional            : (list :given v) | (list :none)
;;   SystemError         : (list :<variant> payload), variant one of command_not_found |
;;                         permission_denied | invalid_working_directory | interrupted | other.
;;   Command/ProcessResult : defstructs (make-hydra_system_command / hydra_system_command-program ...).
;;   FilePath/StatusCode/EnvironmentVariable : transparent (bare string / integer / string).
;;   binary              : a vector of (unsigned-byte 8); Timespec : a defstruct.
;;
;; Process execution uses sb-ext:run-program (SBCL is the Common Lisp head; see math.lisp / text.lisp,
;; which likewise use #+sbcl features). The program is run directly (no shell); stdout and stderr are
;; captured to separate string streams as raw bytes, and the exit code is read from the process.

;; ---- Helpers (not primitives) ----

(defun hydra-system-message (e)
  (let ((m (princ-to-string e)))
    (if (or (null m) (string= m "")) (string (type-of e)) m)))

;; Read an open byte stream fully into a (vector (unsigned-byte 8)).
(defun hydra-system-read-bytes (stream)
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (when stream
      (loop for b = (read-byte stream nil nil)
            while b do (vector-push-extend b out)))
    (coerce out '(vector (unsigned-byte 8)))))

;; Classify a launch failure into a SystemError (best-effort by message text).
(defun hydra-system-classify (program e)
  (let ((msg (string-downcase (hydra-system-message e))))
    (cond
      ((or (search "no such" msg) (search "not found" msg) (search "does not exist" msg))
       (list :command_not_found program))
      ((or (search "permission" msg) (search "denied" msg))
       (list :permission_denied program))
      ((search "not a directory" msg)
       (list :invalid_working_directory program))
      (t (list :other (hydra-system-message e))))))

;; ---- Primitives ----

;; execute :: Command -> effect<Either<SystemError, ProcessResult>>
(defvar hydra_overlay_common_lisp_lib_system_execute
  (lambda (command)
    (let ((program (hydra_system_command-program command))
          (args (hydra_system_command-arguments command))
          (wd (hydra_system_command-working_directory command))
          (env (hydra_system_command-environment command)))
      #+sbcl
      (handler-case
          (let* ((out-stream (make-string-output-stream))
                 (err-stream (make-string-output-stream))
                 (proc (sb-ext:run-program
                         program (mapcar #'princ-to-string args)
                         :search (not (find #\/ program))
                         :directory (when (eq (car wd) :given) (cadr wd))
                         :environment (if (eq (car env) :given)
                                          (mapcar (lambda (kv) (format nil "~a=~a" (car kv) (cdr kv)))
                                                  (cadr env))
                                          (sb-ext:posix-environ))
                         :output out-stream
                         :error err-stream
                         :wait t)))
            (list :right
              (make-hydra_system_process_result
                :exit_code (sb-ext:process-exit-code proc)
                :stdout (sb-ext:string-to-octets (get-output-stream-string out-stream))
                :stderr (sb-ext:string-to-octets (get-output-stream-string err-stream)))))
        (error (e) (list :left (hydra-system-classify program e))))
      #-sbcl
      (list :left (list :other "hydra.lib.system.execute requires SBCL")))))

;; exit :: StatusCode -> effect<unit>
(defvar hydra_overlay_common_lisp_lib_system_exit
  (lambda (code)
    #+sbcl (sb-ext:exit :code code)
    #-sbcl (cl-user::quit)))

;; getEnvironment / getTime / getWorkingDirectory are NULLARY effects: generated consumer code references
;; the bare symbol AS the effect's result, so each is a defvar holding the value. This file is loaded
;; AFTER the generated type defstructs (see run-tests.lisp ordering) so make-hydra_time_timespec is bound.
;; A map is an alist of (key . value) pairs (accepted by hydra.lib.maps).
(defvar hydra_overlay_common_lisp_lib_system_get_environment
  #+sbcl
  (mapcar (lambda (entry)
            (let ((eq (position #\= entry)))
              (if eq
                  (cons (subseq entry 0 eq) (subseq entry (1+ eq)))
                  (cons entry ""))))
          (sb-ext:posix-environ))
  #-sbcl nil)

;; getEnvironmentVariable :: EnvironmentVariable -> effect<Optional<string>>
(defvar hydra_overlay_common_lisp_lib_system_get_environment_variable
  (lambda (name)
    (let ((v (sb-ext:posix-getenv name)))
      (if v (list :given v) (list :none)))))

;; getTime :: effect<Timespec>  (nullary effect)
(defvar hydra_overlay_common_lisp_lib_system_get_time
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day)
    (make-hydra_time_timespec :seconds sec :nanoseconds (* usec 1000))))

;; getWorkingDirectory :: effect<Either<SystemError, FilePath>>  (nullary effect)
(defvar hydra_overlay_common_lisp_lib_system_get_working_directory
  (list :right (namestring (truename "."))))
