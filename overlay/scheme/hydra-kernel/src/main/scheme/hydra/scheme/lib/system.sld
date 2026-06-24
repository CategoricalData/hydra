(define-library (hydra scheme lib system)
  ;; The Scheme head is guile-based. Process execution, environment access, and the clock are not part
  ;; of R7RS, so they are taken from guile here (mirroring how hydra.scheme.lib.files takes directory
  ;; operations from guile). For #498.
  ;; NB: the generated record types (hydra system) / (hydra time) are NOT imported here. Importing them
  ;; into this define-library segfaults guile when loaded in the full run-tests.scm module graph (#498).
  ;; Moreover the scheme head's form-rewriting flat loader turns each generated record constructor and
  ;; accessor into a MACRO (syntax-transformer), so they cannot be called via module-ref. They are instead
  ;; invoked by eval'ing the constructor/accessor form inside the owning module (see make-rec / rec-ref),
  ;; which works once those modules are loaded (they are, via run-tests.scm).
  ;; NB2: a replacement environment is applied by running the program under `env -i NAME=VAL ...` rather
  ;; than mutating guile's global `environ` around the open-pipe* call -- the latter SIGSEGVs guile inside
  ;; the test process (#498). The working directory still uses chdir within a dynamic-wind, which is safe.
  (import (scheme base)
          (scheme write)
          (only (guile)
                getcwd chdir access? X_OK getenv environ
                gettimeofday mkstemp! port-filename status:exit-val OPEN_READ
                with-error-to-file delete-file file-exists? string-index
                primitive-exit open-file resolve-module eval)
          (only (ice-9 popen) open-pipe* close-pipe)
          (only (ice-9 binary-ports) get-bytevector-all)
          (only (rnrs bytevectors) bytevector-length bytevector-u8-ref make-bytevector))
  (export hydra_lib_system_execute
          hydra_lib_system_exit
          hydra_lib_system_get_environment
          hydra_lib_system_get_environment_variable
          hydra_lib_system_get_time
          hydra_lib_system_get_working_directory)
  (begin

    ;; Scheme (guile) implementations of hydra.lib.system primitives (#498).
    ;;
    ;; effect<t> is transparent in the Lisp dialects (effect<t> = t). Fallible primitives return a Hydra
    ;; Either: success is (list 'right v), a launch failure is (list 'left error). Nullary effects are
    ;; bare values. Mirrors the classify pattern of the other hosts.
    ;;
    ;; Runtime representations (Scheme target):
    ;;   Either              : (list 'left v) | (list 'right v)
    ;;   Optional            : (list 'given v) | (list 'none)
    ;;   SystemError         : (list '<variant> payload), variant one of command_not_found |
    ;;                         permission_denied | invalid_working_directory | interrupted | other.
    ;;   Command/ProcessResult : Scheme records (make-hydra_system_command / accessors) from (hydra system).
    ;;   FilePath/StatusCode/EnvironmentVariable : transparent (bare string / int / string).
    ;;   binary              : a vector of byte ints; Timespec : a (hydra time) record.
    ;;
    ;; execute runs the program directly via guile open-pipe* (no shell), capturing stdout as binary,
    ;; stderr separately via a temp file, and the exit code. It pre-checks the program with access? so a
    ;; missing/non-executable program is left(command_not_found)/left(permission_denied) rather than the
    ;; exit-127 that exec failure would otherwise produce.

    ;; ---- Helpers (not primitives) ----

    ;; The generated record constructors/accessors are macros in the loaded env (see module note above),
    ;; so they are invoked by eval'ing the form inside the owning module rather than called as procedures.
    ;; make-rec: (make-rec '(hydra system) 'make-X a b ...) constructs a record from runtime values.
    ;; rec-ref:  (rec-ref '(hydra system) 'X-field rec) reads a field of a record.
    (define (make-rec modname ctor . args)
      (eval (cons ctor (map (lambda (a) (list 'quote a)) args)) (resolve-module modname)))
    (define (rec-ref modname acc rec)
      (eval (list acc (list 'quote rec)) (resolve-module modname)))

    (define (bytevector->binary bv)
      (let ((v (make-vector (bytevector-length bv))))
        (do ((i 0 (+ i 1)))
            ((= i (bytevector-length bv)) v)
          (vector-set! v i (bytevector-u8-ref bv i)))))

    ;; Resolve the program to an executable path, or #f if not found / not executable.
    ;; A program containing a slash is checked directly; otherwise PATH is searched.
    (define (resolve-executable program)
      (if (string-index program #\/)
          (if (access? program X_OK) program #f)
          (let loop ((dirs (split-path (or (getenv "PATH") ""))))
            (if (null? dirs)
                #f
                (let ((candidate (string-append (car dirs) "/" program)))
                  (if (access? candidate X_OK) candidate (loop (cdr dirs))))))))

    (define (split-path s)
      (let loop ((chars (string->list s)) (cur '()) (acc '()))
        (cond
          ((null? chars) (reverse (cons (list->string (reverse cur)) acc)))
          ((char=? (car chars) #\:) (loop (cdr chars) '() (cons (list->string (reverse cur)) acc)))
          (else (loop (cdr chars) (cons (car chars) cur) acc)))))

    ;; Build the (program . args) actually handed to open-pipe*. When a replacement environment is given,
    ;; wrap the invocation in `/usr/bin/env -i NAME=VAL ... program args` so the child sees exactly that
    ;; environment, without mutating the parent process's global environ (which crashes guile, see NB2).
    (define (env-wrap program args env)
      (if (eq? (car env) 'given)
          (cons "/usr/bin/env"
                (append (cons "-i"
                              (map (lambda (kv) (string-append (car kv) "=" (cdr kv))) (cadr env)))
                        (cons program args)))
          (cons program args)))

    ;; ---- Primitives ----

    ;; execute :: Command -> effect<Either<SystemError, ProcessResult>>
    (define hydra_lib_system_execute
      (lambda (command)
        (let ((program (rec-ref '(hydra system) 'hydra_system_command-program command))
              (args (rec-ref '(hydra system) 'hydra_system_command-arguments command))
              (wd (rec-ref '(hydra system) 'hydra_system_command-working_directory command))
              (env (rec-ref '(hydra system) 'hydra_system_command-environment command)))
          (if (not (resolve-executable program))
              (list 'left (list 'command_not_found program))
              (let ((invocation (env-wrap program args env))
                    (saved-cwd (getcwd))
                    (errfile (let ((p (mkstemp! (string-copy "/tmp/hydra-sys-XXXXXX"))))
                               (let ((name (port-filename p))) (close-port p) name))))
                (dynamic-wind
                  (lambda ()
                    (when (eq? (car wd) 'given) (chdir (cadr wd))))
                  (lambda ()
                    (let* ((port (with-error-to-file errfile
                                   (lambda () (apply open-pipe* OPEN_READ (car invocation) (cdr invocation)))))
                           (out (get-bytevector-all port))
                           (st (close-pipe port))
                           (errbv (call-with-port (open-file errfile "rb") get-bytevector-all)))
                      (list 'right
                        (make-rec '(hydra system) 'make-hydra_system_process_result
                          (status:exit-val st)
                          (if (eof-object? out) #() (bytevector->binary out))
                          (if (eof-object? errbv) #() (bytevector->binary errbv))))))
                  (lambda ()
                    (chdir saved-cwd)
                    (when (file-exists? errfile) (delete-file errfile)))))))))

    ;; exit :: StatusCode -> effect<unit>
    (define hydra_lib_system_exit
      (lambda (code) (primitive-exit code)))

    ;; getEnvironment :: effect<Map<EnvironmentVariable, string>>  (nullary effect: a bare value)
    ;; A map is an alist of (key . value) pairs (accepted by hydra.lib.maps).
    (define hydra_lib_system_get_environment
      (map (lambda (entry)
             (let ((eq (string-index entry #\=)))
               (if eq
                   (cons (substring entry 0 eq) (substring entry (+ eq 1) (string-length entry)))
                   (cons entry ""))))
           (environ)))

    ;; getEnvironmentVariable :: EnvironmentVariable -> effect<Optional<string>>
    (define hydra_lib_system_get_environment_variable
      (lambda (name)
        (let ((v (getenv name)))
          (if v (list 'given v) (list 'none)))))

    ;; getTime :: effect<Timespec>  (nullary effect: a bare value)
    (define hydra_lib_system_get_time
      (let ((tv (gettimeofday)))
        (make-rec '(hydra time) 'make-hydra_time_timespec (car tv) (* (cdr tv) 1000))))

    ;; getWorkingDirectory :: effect<Either<SystemError, FilePath>>  (nullary effect: a bare value)
    (define hydra_lib_system_get_working_directory
      (list 'right (getcwd)))))
