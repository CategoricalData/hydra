;;; Cross-host inference benchmark — Common Lisp runner.
;;;
;;; Loads the synthetic hydra.bench.<series> workload from the kernel JSON,
;;; takes prefixes of the chained walker definitions, and times
;;; hydra_codegen_infer_modules_given on each prefix. Emits a JSON array
;;; describing {host, n, elapsed_seconds, ok} per prefix size.
;;;
;;; Usage:
;;;   sbcl --script inference-bench.lisp --json-dir <path> [--sizes 0,10,25,50] \
;;;        [--namespace hydra.bench.linearChain] [--out path/to/result.json]
;;;
;;; This script is invoked by bin/run-inference-bench.sh, which dispatches
;;; to per-host runners and aggregates results.

(in-package :cl-user)

(setf *read-default-float-format* 'double-float)

;; Disable IEEE-754 traps for compatibility with generated kernel code that
;; may evaluate NaN-producing expressions during inference.
#+sbcl (sb-int:set-floating-point-modes :traps nil)

;; --- Argument parsing ---
(defvar *bench-args*
  (let ((args (or sb-ext:*posix-argv* nil)))
    (or (cdr (member "--" args :test #'string=)) args)))

(defun get-arg (name &optional default)
  (let ((pos (position name *bench-args* :test #'string=)))
    (if (and pos (< (1+ pos) (length *bench-args*)))
        (nth (1+ pos) *bench-args*)
        default)))

(defvar *json-dir* (get-arg "--json-dir"))
(defvar *sizes-str* (get-arg "--sizes" "0,10,25,50,100"))
(defvar *namespace* (get-arg "--namespace" "hydra.bench.linearChain"))
(defvar *out-path* (get-arg "--out"))

(unless *json-dir*
  (format *error-output* "Usage: sbcl --script inference-bench.lisp --json-dir <path> [opts]~%")
  (sb-ext:exit :code 1))

(defun parse-sizes (s)
  (let ((parts (loop for start = 0 then (1+ comma)
                     for comma = (position #\, s :start start)
                     collect (subseq s start (or comma (length s)))
                     while comma)))
    (sort (remove-duplicates
            (mapcar (lambda (p) (parse-integer (string-trim " " p)))
                    (remove "" parts :test #'string=))
            :test #'=)
          #'<)))

(defvar *sizes* (parse-sizes *sizes-str*))

;; --- Load the Hydra kernel ---
;; Resolve heads/lisp/common-lisp/ from this script's location:
;; heads/lisp/common-lisp/bin/inference-bench.lisp -> ../
(defvar *hydra-cl-head*
  (let ((dir (pathname-directory (truename *load-truename*))))
    (make-pathname :directory (butlast dir))))

(defun head-path (rel)
  (merge-pathnames rel *hydra-cl-head*))

(format *error-output* "Loading prelude...~%")
(force-output *error-output*)
(load (head-path "src/main/common-lisp/hydra/prelude.lisp"))

(format *error-output* "Loading native libraries...~%")
(dolist (f '("lib/equality.lisp" "lib/maps.lisp" "lib/sets.lisp" "lib/lists.lisp"
             "lib/strings.lisp" "lib/logic.lisp" "lib/math.lisp" "lib/chars.lisp"
             "lib/eithers.lisp" "lib/literals.lisp" "lib/maybes.lisp"
             "lib/pairs.lisp" "lib/regex.lisp"))
  (load (head-path (concatenate 'string "src/main/common-lisp/hydra/" f))))

(format *error-output* "Loading loader and generated kernel...~%")
(load (head-path "src/main/common-lisp/hydra/loader.lisp"))
(load (head-path "src/main/common-lisp/hydra/json-reader.lisp"))

;; Point gen-main at the dist/ kernel.
(setf *hydra-gen-main-dir*
      (merge-pathnames "../../../dist/common-lisp/hydra-kernel/src/main/common-lisp/hydra/"
                       *hydra-cl-head*))

(hydra-load-gen-main)
(hydra-load-prims-and-libraries)
(hydra-set-function-bindings)
(format *error-output* "Kernel loaded.~%")
(force-output *error-output*)

;; --- JSON to Hydra conversion (mirrors bootstrap.lisp) ---
(defun cl-to-hydra-json (obj)
  (cond
    ((eq obj :null) (list :null nil))
    ((eq obj t) (list :boolean t))
    ((eq obj :json-false) (list :boolean nil))
    ((eq obj :json-empty-object) (list :object nil))
    ((null obj) (list :array nil))
    ((numberp obj) (list :number (coerce obj 'double-float)))
    ((stringp obj) (list :string obj))
    ((and (consp obj) (consp (car obj)) (stringp (caar obj)))
     (list :object
           (mapcar (lambda (pair) (cons (car pair) (cl-to-hydra-json (cdr pair)))) obj)))
    ((listp obj) (list :array (mapcar #'cl-to-hydra-json obj)))
    (t (error "Unexpected JSON value type: ~A" (type-of obj)))))

(defun bootstrap-graph ()
  (funcall 'make-hydra_graph_graph nil nil nil nil nil
           (standard-library) nil nil))

(defun bootstrap-schema-map ()
  (let ((result nil))
    (dolist (pair (symbol-value 'hydra_json_bootstrap_types_by_name))
      (let* ((name (car pair))
             (typ (cdr pair))
             (ts (funcall (symbol-value 'hydra_scoping_f_type_to_type_scheme) typ))
             (stripped (funcall (symbol-value 'hydra_strip_deannotate_type_recursive)
                                (funcall 'hydra_core_type_scheme-body ts))))
        (push (cons name stripped) result)))
    (nreverse result)))

(defun namespace-to-path (ns)
  (funcall (symbol-value 'hydra_codegen_namespace_to_path) ns))

(defun load-module-from-json (bs-graph schema-map ns-str json-dir)
  (let* ((file-path (format nil "~A/~A.json" json-dir (namespace-to-path ns-str)))
         (json-obj (json-read-file file-path))
         (hydra-json (cl-to-hydra-json json-obj))
         (mod-type (list :variable "hydra.packaging.Module"))
         (json-result (funcall (funcall (funcall (funcall
                        (symbol-value 'hydra_json_decode_from_json) schema-map)
                        "hydra.packaging.Module") mod-type) hydra-json)))
    (when (eq (first json-result) :left)
      (error "JSON decode error for ~A: ~A" ns-str (second json-result)))
    (let* ((term (second json-result))
           (mod-result (funcall (funcall
                         (symbol-value 'hydra_decode_packaging_module) bs-graph) term)))
      (when (eq (first mod-result) :left)
        (error "Module decode error for ~A: ~A" ns-str (second mod-result)))
      (second mod-result))))

(defun read-manifest-field (json-dir field-name)
  (let* ((manifest-path (format nil "~A/manifest.json" json-dir))
         (manifest (json-read-file manifest-path))
         (entry (assoc field-name manifest :test #'string=)))
    (if entry (cdr entry) nil)))

(defun load-modules-from-json (json-dir namespaces)
  (let ((bs-graph (bootstrap-graph))
        (schema-map (bootstrap-schema-map)))
    (mapcar (lambda (ns) (load-module-from-json bs-graph schema-map ns json-dir))
            namespaces)))

;; --- Bench-specific logic ---

(defun module-ns-str (m)
  "Extract a module's namespace as a string."
  (let ((ns (cdr (assoc :namespace m))))
    (if (stringp ns) ns (cdr (assoc :value ns)))))

(defun find-bench-module (universe ns-str)
  (or (find-if (lambda (m) (string= (module-ns-str m) ns-str)) universe)
      (error "Bench module ~A not found in universe. Run /sync-haskell() to regenerate." ns-str)))

(defun last-dot-segment (s)
  (let ((ix (position #\. s :from-end t)))
    (if ix (subseq s (1+ ix)) s)))

(defun rename-term-def (td target-ns)
  "Rename a TermDefinition's name into target-ns (a string)."
  (let* ((orig-name (cdr (assoc :name td)))
         (name-str (if (stringp orig-name) orig-name (cdr (assoc :value orig-name))))
         (local (last-dot-segment name-str))
         (new-name (concatenate 'string target-ns "." local)))
    ;; Build a new term_definition alist with the renamed name.
    (mapcar (lambda (pair)
              (if (eq (car pair) :name)
                  (cons :name (if (stringp orig-name)
                                  new-name
                                  (list (cons :value new-name))))
                  pair))
            td)))

(defun make-synthetic-module (bench-mod n)
  "Build a target module containing the first n walker defs renamed into z.bench.scaling."
  (let* ((target-ns-str "z.bench.scaling")
         (target-ns (list (cons :value target-ns-str)))
         (orig-defs (cdr (assoc :definitions bench-mod)))
         (take-defs (subseq orig-defs 0 (min n (length orig-defs))))
         ;; Each definition is either (:term td) or (:type td); we rename term defs only.
         (renamed
          (mapcar (lambda (d)
                    (cond
                      ((and (consp d) (eq (first d) :term))
                       (list :term (rename-term-def (second d) target-ns-str)))
                      (t d)))
                  take-defs))
         (bench-ns (cdr (assoc :namespace bench-mod)))
         (bench-deps (cdr (assoc :dependencies bench-mod)))
         (description (cdr (assoc :description bench-mod))))
    (list (cons :description description)
          (cons :namespace target-ns)
          (cons :dependencies (cons bench-ns bench-deps))
          (cons :definitions renamed))))

(defun time-inference (universe target)
  "Time hydra_codegen_infer_modules_given universe+target target. Returns (elapsed ok err)."
  (let* ((bs-graph (bootstrap-graph))
         (cx (funcall 'make-hydra_context_context nil nil nil))
         (universe-plus (append universe (list target)))
         (targets (list target))
         (t0 (get-internal-real-time))
         (result (handler-case
                     (funcall (funcall (funcall (funcall
                       (symbol-value 'hydra_codegen_infer_modules_given)
                       cx) bs-graph) universe-plus) targets)
                   (error (e)
                     (list :left (format nil "exception: ~A" e)))))
         (t1 (get-internal-real-time))
         (elapsed (/ (- t1 t0) (float internal-time-units-per-second))))
    (let ((ok (eq (first result) :right))
          (err (if (eq (first result) :left)
                   (let ((s (format nil "~A" (second result))))
                     (subseq s 0 (min 200 (length s))))
                   "")))
      (list elapsed ok err))))

;; --- JSON output ---

(defun json-escape (s)
  (with-output-to-string (out)
    (loop for c across s do
          (case c
            (#\\ (write-string "\\\\" out))
            (#\" (write-string "\\\"" out))
            (#\Newline (write-string "\\n" out))
            (#\Return (write-string "\\r" out))
            (#\Tab (write-string "\\t" out))
            (t (write-char c out))))))

(defun result-json (host nsv n elapsed ok err)
  (format nil "  {~%    \"host\": \"~A\",~%    \"namespace\": \"~A\",~%    \"n\": ~A,~%    \"elapsed_seconds\": ~,6F,~%    \"ok\": ~A,~%    \"error\": ~A~%  }"
          host nsv n elapsed
          (if ok "true" "false")
          (if ok "null" (format nil "\"~A\"" (json-escape err)))))

;; --- Main ---

(format *error-output* "Loading universe from ~A ...~%" *json-dir*)
(force-output *error-output*)
(let* ((t-load-0 (get-internal-real-time))
       (main-ns (coerce (read-manifest-field *json-dir* "mainModules") 'list))
       (universe (load-modules-from-json *json-dir* main-ns))
       (t-load-1 (get-internal-real-time)))
  (format *error-output* "  loaded ~A modules (~,1Fs)~%"
          (length universe)
          (/ (- t-load-1 t-load-0) (float internal-time-units-per-second)))
  (force-output *error-output*)
  (let* ((bench (find-bench-module universe *namespace*))
         (avail (length (cdr (assoc :definitions bench)))))
    (format *error-output* "Bench workload ~A: ~A definitions available~%" *namespace* avail)
    (force-output *error-output*)
    (let ((results nil)
          (host "common-lisp"))
      (dolist (n *sizes*)
        (cond
          ((> n avail)
           (format *error-output* "  skipping n=~A (only ~A defs available)~%" n avail))
          (t
           (let* ((target (make-synthetic-module bench n))
                  (timing (time-inference universe target))
                  (elapsed (first timing))
                  (ok (second timing))
                  (err (third timing))
                  (status (if ok "OK" (format nil "FAIL: ~A" err))))
             (format *error-output* "  n=~3D: ~6,2Fs ~A~%" n elapsed status)
             (force-output *error-output*)
             (push (list host *namespace* n elapsed ok err) results)))))
      (let ((json-body
             (format nil "[~%~{~A~^,~%~}~%]~%"
                     (mapcar (lambda (r) (apply #'result-json r))
                             (nreverse results)))))
        (cond
          (*out-path*
           (with-open-file (out *out-path* :direction :output :if-exists :supersede
                                          :external-format :utf-8)
             (write-string json-body out))
           (format *error-output* "Wrote ~A~%" *out-path*))
          (t (write-string json-body)))
        (sb-ext:exit :code (if (every (lambda (r) (fifth r))
                                      (nreverse (copy-list results)))
                               0 1))))))
