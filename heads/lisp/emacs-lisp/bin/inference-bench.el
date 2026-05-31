;;; inference-bench.el --- Cross-host inference benchmark — Emacs Lisp runner -*- lexical-binding: t -*-
;;;
;;; Loads the synthetic hydra.bench.<series> workload from the kernel JSON,
;;; takes prefixes of the chained walker definitions, and times
;;; hydra_codegen_infer_modules_given on each prefix. Emits a JSON array
;;; describing {host, n, elapsed_seconds, ok} per prefix size.
;;;
;;; Usage:
;;;   emacs --batch --no-init-file --script inference-bench.el -- \
;;;       --json-dir <path> --bench-json-dir <path> \
;;;       [--sizes 0,10,25,50] [--namespace hydra.bench.linearChain] \
;;;       [--out path/to/result.json]
;;;
;;; This script is invoked by bin/run-inference-bench.sh, which dispatches
;;; to per-host runners and aggregates results.

(require 'cl-lib)

;; Increase limits for deeply nested generated code.
(setq max-lisp-eval-depth 100000)
(setq max-specpdl-size 100000)

;; --- Argument parsing ---
(defvar bench-args (or command-line-args-left nil)
  "Arguments after -- on the command line.")
(setq command-line-args-left nil)

(defun bench-get-arg (name &optional default)
  (let ((pos (cl-position name bench-args :test #'string=)))
    (if (and pos (< (1+ pos) (length bench-args)))
        (nth (1+ pos) bench-args)
      default)))

(defvar bench-json-dir-arg (bench-get-arg "--json-dir"))
(defvar bench-bench-json-dir (bench-get-arg "--bench-json-dir"))
(defvar bench-sizes-str (bench-get-arg "--sizes" "0,10,25,50,100"))
(defvar bench-namespace (bench-get-arg "--namespace" "hydra.bench.linearChain"))
(defvar bench-out-path (bench-get-arg "--out"))

(unless bench-json-dir-arg
  (princ "Usage: emacs --batch --script inference-bench.el -- --json-dir <path> [opts]\n")
  (kill-emacs 1))

(defun bench-parse-sizes (s)
  (sort (cl-remove-duplicates
         (mapcar #'string-to-number
                 (cl-remove-if (lambda (p) (string= p ""))
                               (split-string s ",")))
         :test #'=)
        #'<))

(defvar bench-sizes (bench-parse-sizes bench-sizes-str))

;; --- Load the Hydra Emacs Lisp kernel ---
;; Resolve heads/lisp/emacs-lisp/ from this script's location:
;; heads/lisp/emacs-lisp/bin/inference-bench.el -> ../
(defvar hydra-el-head
  (let ((script (or load-file-name buffer-file-name
                    (expand-file-name "inference-bench.el" default-directory))))
    (file-name-directory (directory-file-name (file-name-directory script)))))

(defun head-path (rel)
  (expand-file-name rel hydra-el-head))

(princ "Loading loader and native libraries...\n")
(load (head-path "src/main/emacs-lisp/hydra/loader.el") nil t)

;; Point gen-main at the dist/ kernel.
;; hydra-el-head = heads/lisp/emacs-lisp/; three "../" climbs to repo root.
(let ((dist-base (expand-file-name "../../../dist/emacs-lisp/hydra-kernel/" hydra-el-head)))
  (setq hydra-gen-main-dir (expand-file-name "src/main/emacs-lisp/hydra/" dist-base))
  (setq hydra-gen-test-dir (expand-file-name "src/test/emacs-lisp/hydra/" dist-base)))

(princ (format "  json-dir: %s\n" bench-json-dir-arg))
(princ (format "  gen-main: %s\n" hydra-gen-main-dir))

(princ "Loading generated kernel (this can take a few minutes)...\n")
(hydra-load-gen-main)
(hydra-load-prims-and-libraries)
(hydra-set-function-bindings)
(princ "Kernel loaded.\n")

;; --- JSON to Hydra conversion (mirrors bootstrap.el) ---
(defun bench-read-json-file (path)
  (with-temp-buffer
    (set-buffer-multibyte t)
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (json-parse-string (buffer-string)
                       :object-type 'hash-table
                       :null-object :null
                       :false-object :json-false)))

(defun bench-json-to-hydra (obj)
  (cond
   ((eq obj :null) (list :null nil))
   ((eq obj t) (list :boolean t))
   ((eq obj :json-false) (list :boolean nil))
   ((numberp obj) (list :number (float obj)))
   ((stringp obj) (list :string obj))
   ((hash-table-p obj)
    (let ((pairs nil))
      (maphash (lambda (k v) (push (cons k (bench-json-to-hydra v)) pairs)) obj)
      (list :object (nreverse pairs))))
   ((vectorp obj)
    (list :array (mapcar #'bench-json-to-hydra (append obj nil))))
   (t (error "Unexpected JSON value type: %s" (type-of obj)))))

(defun bench-bootstrap-graph ()
  (funcall 'make-hydra_graph_graph nil nil nil nil nil
           (standard-library) nil nil))

(defun bench-bootstrap-schema-map ()
  (let ((result nil))
    (dolist (pair (symbol-value 'hydra_json_bootstrap_types_by_name))
      (let* ((name (car pair))
             (typ (cdr pair))
             (ts (funcall (symbol-value 'hydra_scoping_f_type_to_type_scheme) typ))
             (stripped (funcall (symbol-value 'hydra_strip_deannotate_type_recursive)
                                (funcall 'hydra_core_type_scheme-body ts))))
        (push (cons name stripped) result)))
    (nreverse result)))

(defun bench-namespace-to-path (ns)
  (funcall (symbol-value 'hydra_codegen_module_name_to_path) ns))

(defun bench-load-module-from-json (bs-graph schema-map ns-str json-dir)
  (let* ((file-path (format "%s/%s.json" json-dir (bench-namespace-to-path ns-str)))
         (json-obj (bench-read-json-file file-path))
         (hydra-json (bench-json-to-hydra json-obj))
         (mod-type (list :variable "hydra.packaging.Module"))
         (json-result (funcall (funcall (funcall (funcall
                        (symbol-value 'hydra_json_decode_from_json) schema-map)
                        "hydra.packaging.Module") mod-type) hydra-json)))
    (when (eq (car json-result) :left)
      (error "JSON decode error for %s: %s" ns-str (cadr json-result)))
    (let* ((term (cadr json-result))
           (mod-result (funcall (funcall
                         (symbol-value 'hydra_decode_packaging_module) bs-graph) term)))
      (when (eq (car mod-result) :left)
        (error "Module decode error for %s: %s" ns-str (cadr mod-result)))
      (cadr mod-result))))

(defun bench-read-manifest-field (json-dir field-name)
  (let* ((manifest-path (format "%s/manifest.json" json-dir))
         (manifest (bench-read-json-file manifest-path)))
    (if (hash-table-p manifest)
        (let ((val (gethash field-name manifest)))
          (if (vectorp val) (append val nil) val))
      nil)))

(defun bench-load-modules-from-json (json-dir namespaces)
  (let ((bs-graph (bench-bootstrap-graph))
        (schema-map (bench-bootstrap-schema-map)))
    (mapcar (lambda (ns) (bench-load-module-from-json bs-graph schema-map ns json-dir))
            namespaces)))

;; --- Bench-specific logic ---

(defun bench-module-ns-str (m)
  (let ((ns (cdr (assoc :namespace m))))
    (if (stringp ns) ns (cdr (assoc :value ns)))))

(defun bench-find-bench-module (universe ns-str)
  (or (cl-find-if (lambda (m) (string= (bench-module-ns-str m) ns-str)) universe)
      (error "Bench module %s not found in universe. Run /sync-bench() to regenerate." ns-str)))

(defun bench-last-dot-segment (s)
  (let ((ix (cl-position ?. s :from-end t)))
    (if ix (substring s (1+ ix)) s)))

(defun bench-rename-term-def (td target-ns)
  (let* ((orig-name (cdr (assoc :name td)))
         (name-str (if (stringp orig-name) orig-name (cdr (assoc :value orig-name))))
         (local (bench-last-dot-segment name-str))
         (new-name (concat target-ns "." local)))
    (mapcar (lambda (pair)
              (if (eq (car pair) :name)
                  (cons :name (if (stringp orig-name)
                                  new-name
                                (list (cons :value new-name))))
                pair))
            td)))

(defun bench-make-synthetic-module (bench-mod n)
  (let* ((target-ns-str "z.bench.scaling")
         (target-ns (list (cons :value target-ns-str)))
         (orig-defs (cdr (assoc :definitions bench-mod)))
         (take-defs (cl-subseq orig-defs 0 (min n (length orig-defs))))
         (renamed
          (mapcar (lambda (d)
                    (cond
                     ((and (consp d) (eq (car d) :term))
                      (list :term (bench-rename-term-def (cadr d) target-ns-str)))
                     (t d)))
                  take-defs))
         (bench-ns (cdr (assoc :namespace bench-mod)))
         (bench-deps (cdr (assoc :dependencies bench-mod)))
         (description (cdr (assoc :description bench-mod))))
    (list (cons :description description)
          (cons :namespace target-ns)
          (cons :dependencies (cons bench-ns bench-deps))
          (cons :definitions renamed))))

(defun bench-time-inference (universe target)
  "Time hydra_codegen_infer_modules_given. Returns (elapsed ok err)."
  (let* ((bs-graph (bench-bootstrap-graph))
         (cx (funcall 'make-hydra_typing_inference_context 0 nil))
         (universe-plus (append universe (list target)))
         (targets (list target))
         (t0 (float-time))
         (result (condition-case e
                     (funcall (funcall (funcall (funcall
                               (symbol-value 'hydra_codegen_infer_modules_given)
                               cx) bs-graph) universe-plus) targets)
                   (error (list :left (format "exception: %s" e)))))
         (t1 (float-time))
         (elapsed (- t1 t0)))
    (let ((ok (eq (car result) :right))
          (err (if (eq (car result) :left)
                   (let ((s (format "%s" (cadr result))))
                     (substring s 0 (min 200 (length s))))
                 "")))
      (list elapsed ok err))))

;; --- JSON output ---

(defun bench-json-escape (s)
  (let ((out (make-string 0 ?x)))
    (dolist (c (string-to-list s) out)
      (setq out (concat out
                        (cond
                         ((eq c ?\\) "\\\\")
                         ((eq c ?\") "\\\"")
                         ((eq c ?\n) "\\n")
                         ((eq c ?\r) "\\r")
                         ((eq c ?\t) "\\t")
                         (t (char-to-string c))))))))

(defun bench-result-json (host nsv n elapsed ok err)
  (format "  {\n    \"host\": \"%s\",\n    \"namespace\": \"%s\",\n    \"n\": %d,\n    \"elapsed_seconds\": %.6f,\n    \"ok\": %s,\n    \"error\": %s\n  }"
          host nsv n elapsed
          (if ok "true" "false")
          (if ok "null" (format "\"%s\"" (bench-json-escape err)))))

;; --- Main ---

(princ (format "Loading universe from %s ...\n" bench-json-dir-arg))
(when bench-bench-json-dir
  (princ (format "  + bench modules from %s\n" bench-bench-json-dir)))

(let* ((t-load-0 (float-time))
       (main-ns (bench-read-manifest-field bench-json-dir-arg "mainModules"))
       (universe (bench-load-modules-from-json bench-json-dir-arg main-ns))
       (universe (if (and bench-bench-json-dir
                          (file-exists-p (format "%s/manifest.json" bench-bench-json-dir)))
                     (append universe
                             (bench-load-modules-from-json bench-bench-json-dir
                                (bench-read-manifest-field bench-bench-json-dir "mainModules")))
                   universe))
       (t-load-1 (float-time)))
  (princ (format "  loaded %d modules (%.1fs)\n"
                 (length universe) (- t-load-1 t-load-0)))
  (let* ((bench (bench-find-bench-module universe bench-namespace))
         (avail (length (cdr (assoc :definitions bench)))))
    (princ (format "Bench workload %s: %d definitions available\n" bench-namespace avail))
    (let ((results nil)
          (host "emacs-lisp"))
      (cl-labels ((write-results ()
                    (when bench-out-path
                      (let* ((ordered (reverse results))
                             (body (concat "[\n"
                                           (mapconcat (lambda (r) (apply #'bench-result-json r))
                                                      ordered ",\n")
                                           "\n]\n")))
                        (with-temp-buffer
                          (insert body)
                          (let ((coding-system-for-write 'utf-8-unix))
                            (write-region (point-min) (point-max) bench-out-path nil 'silent)))))))
        (dolist (n bench-sizes)
          (cond
           ((> n avail)
            (princ (format "  skipping n=%d (only %d defs available)\n" n avail)))
           (t
            (let* ((target (bench-make-synthetic-module bench n))
                   (timing (bench-time-inference universe target))
                   (elapsed (nth 0 timing))
                   (ok (nth 1 timing))
                   (err (nth 2 timing))
                   (status (if ok "OK" (format "FAIL: %s" err))))
              (princ (format "  n=%3d: %6.2fs %s\n" n elapsed status))
              (push (list host bench-namespace n elapsed ok err) results)
              ;; Write incrementally so partial results survive timeouts.
              (write-results)))))
        (cond
         (bench-out-path
          (princ (format "Wrote %s\n" bench-out-path)))
         (t (princ (concat "[\n"
                           (mapconcat (lambda (r) (apply #'bench-result-json r))
                                      (reverse results) ",\n")
                           "\n]\n"))))
        (kill-emacs (if (cl-every (lambda (r) (nth 4 r)) (reverse results)) 0 1))))))
