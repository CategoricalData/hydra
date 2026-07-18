;;; system.el --- Hydra Emacs Lisp system primitives -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Emacs Lisp implementations of hydra.lib.system primitives (#498).
;;
;; effect<t> is transparent in the Lisp dialects (effect<t> = t). Fallible primitives return a Hydra
;; Either: success is (list :right v), a launch failure is (list :left error). Nullary effects are bare
;; values. Mirrors the classify pattern of the other hosts.
;;
;; Runtime representations (Emacs Lisp target):
;;   Either              : (list :left v) | (list :right v)
;;   Optional            : (list :given v) | (list :none)
;;   SystemError         : (list :<variant> payload), variant one of command_not_found |
;;                         permission_denied | invalid_working_directory | interrupted | other.
;;   Command/ProcessResult : cl-defstructs (make-hydra_system_command / hydra_system_command-program ...).
;;   FilePath/StatusCode/EnvironmentVariable : transparent (bare string / integer / string).
;;   binary              : a unibyte string; Timespec : a cl-defstruct.
;;
;; Process execution uses call-process (no shell): the program is run directly with stdout captured to a
;; buffer and stderr to a temp file, and the exit code is the call-process return value. binary stdout/
;; stderr are unibyte strings.

;; ---- Helpers (not primitives) ----

(defun hydra-system-message (e)
  "Best-effort human-readable message for an Emacs error/condition object E."
  (let ((m (error-message-string e)))
    (if (or (null m) (string= m "")) (format "%S" e) m)))

;; Classify a launch failure into a SystemError (best-effort by message text).
(defun hydra-system-classify (program e)
  (let ((msg (downcase (hydra-system-message e))))
    (cond
     ((or (string-match-p "no such" msg) (string-match-p "not found" msg)
          (string-match-p "executable" msg) (string-match-p "does not exist" msg))
      (list :command_not_found program))
     ((or (string-match-p "permission" msg) (string-match-p "denied" msg))
      (list :permission_denied program))
     ((string-match-p "not a directory" msg)
      (list :invalid_working_directory program))
     (t (list :other (hydra-system-message e))))))

;; ---- Primitives ----

;; execute :: Command -> effect<Either<SystemError, ProcessResult>>
(defvar hydra_overlay_emacs_lisp_lib_system_execute
  (lambda (command)
    (let ((program (hydra_system_command-program command))
          (args (hydra_system_command-arguments command))
          (wd (hydra_system_command-working_directory command))
          (env (hydra_system_command-environment command)))
      (condition-case e
          (let* ((errfile (make-temp-file "hydra-sys-"))
                 (default-directory (if (eq (car wd) :given) (cadr wd) default-directory))
                 (process-environment (if (eq (car env) :given)
                                          (mapcar (lambda (kv) (format "%s=%s" (car kv) (cdr kv)))
                                                  (cadr env))
                                        process-environment))
                 (out (with-temp-buffer
                        (set-buffer-multibyte nil)
                        (let ((code (apply #'call-process program nil
                                           (list (current-buffer) errfile) nil
                                           (mapcar (lambda (a) (format "%s" a)) args))))
                          (cons code (buffer-string)))))
                 (errtxt (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (insert-file-contents-literally errfile)
                           (buffer-string))))
            (delete-file errfile)
            (let ((code (car out)))
              ;; call-process returns a signal-description string if the process was killed by a signal.
              (if (stringp code)
                  (list :left (list :interrupted))
                (list :right
                      (make-hydra_system_process_result
                       :exit_code code
                       :stdout (cdr out)
                       :stderr errtxt)))))
        (error (list :left (hydra-system-classify program e)))))))

;; exit :: StatusCode -> effect<unit>
(defvar hydra_overlay_emacs_lisp_lib_system_exit
  (lambda (code) (kill-emacs code)))

;; getEnvironment / getTime / getWorkingDirectory are NULLARY effects: generated consumer code references
;; the bare symbol AS the effect's result, so each is a defvar holding the value.

;; getEnvironment :: effect<Map<EnvironmentVariable, string>>  (alist of (key . value), accepted by hydra.lib.maps)
(defvar hydra_overlay_emacs_lisp_lib_system_get_environment
  (mapcar (lambda (entry)
            (let ((eq (string-match "=" entry)))
              (if eq
                  (cons (substring entry 0 eq) (substring entry (1+ eq)))
                (cons entry ""))))
          process-environment))

;; getEnvironmentVariable :: EnvironmentVariable -> effect<Optional<string>>
(defvar hydra_overlay_emacs_lisp_lib_system_get_environment_variable
  (lambda (name)
    (let ((v (getenv name)))
      (if v (list :given v) (list :none)))))

;; getTime :: effect<Timespec>  (nullary effect)
(defvar hydra_overlay_emacs_lisp_lib_system_get_time
  (let ((now (current-time)))
    ;; current-time = (HIGH LOW USEC PSEC); seconds = HIGH*65536 + LOW.
    (make-hydra_time_timespec
     :seconds (+ (* (nth 0 now) 65536) (nth 1 now))
     :nanoseconds (* (nth 2 now) 1000))))

;; getWorkingDirectory :: effect<Either<SystemError, FilePath>>  (nullary effect)
(defvar hydra_overlay_emacs_lisp_lib_system_get_working_directory
  (list :right (expand-file-name default-directory)))

;; readStdin :: effect<Either<SystemError, binary>>  (nullary effect)
;; Batch-mode Emacs has no direct binary-stdin primitive, so this reads the process's own stdin via
;; the /dev/stdin special file (as insert-file-contents-literally, to avoid encoding translation),
;; producing a unibyte string -- the binary representation used throughout this host.
(defvar hydra_overlay_emacs_lisp_lib_system_read_stdin
  (condition-case e
      (list :right
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally "/dev/stdin")
              (buffer-string)))
    (error (list :left (list :other (hydra-system-message e))))))

;; writeStderr :: binary -> effect<Either<SystemError, unit>>
;; Writes to the /dev/stderr special file rather than send-string-to-terminal, so output still goes
;; to fd 2 (and not the controlling terminal) when stderr has been redirected or piped.
(defvar hydra_overlay_emacs_lisp_lib_system_write_stderr
  (lambda (bytes)
    (condition-case e
        (progn
          (write-region bytes nil "/dev/stderr" t 'silent)
          (list :right nil))
      (error (list :left (list :other (hydra-system-message e)))))))

;; writeStdout :: binary -> effect<Either<SystemError, unit>>
(defvar hydra_overlay_emacs_lisp_lib_system_write_stdout
  (lambda (bytes)
    (condition-case e
        (progn
          (princ bytes)
          (list :right nil))
      (error (list :left (list :other (hydra-system-message e)))))))

(provide 'hydra-emacs-lisp-system)
