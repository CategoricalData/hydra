;;; Hydra Scheme loader
;;; Loads generated files by stripping define-library wrappers, qualifying all
;;; top-level definitions with the module name, and rewriting intra-module
;;; references. Evaluates in the interaction-environment.
;;; Uses retry passes to handle forward references (like CL's loader).

(import (scheme base) (scheme read) (scheme eval) (scheme file)
        (scheme write) (scheme process-context))
(use-modules (system base compile))

;; ============================================================================
;; Bytecode compilation
;; ============================================================================

(define (compile-define-value form)
  "If form is (define name (lambda ...)), try to compile the lambda to bytecode.
   Falls back to the original form if compilation fails."
  (if (and (pair? form) (eq? (car form) 'define)
           (pair? (cdr form)) (symbol? (cadr form))
           (pair? (cddr form)) (null? (cdddr form))
           (pair? (caddr form)) (eq? (car (caddr form)) 'lambda))
      (catch #t
        (lambda ()
          (let ((compiled (compile (caddr form) #:env (current-module))))
            `(define ,(cadr form) ,compiled)))
        (lambda args form))
      form))

;; ============================================================================
;; Module name to prefix conversion
;; ============================================================================

(define (library-name->prefix parts)
  "Convert library name parts like (hydra formatting) to prefix string 'hydra_formatting'."
  (let loop ((rest parts) (acc ""))
    (if (null? rest)
        acc
        (loop (cdr rest)
              (string-append acc
                (if (string=? acc "") "" "_")
                (symbol->string (car rest)))))))

(define (qualify-name prefix name)
  "Create a qualified symbol: prefix_name."
  (string->symbol (string-append prefix "_" (symbol->string name))))

;; ============================================================================
;; Symbol renaming: build rename map and rewrite forms
;; ============================================================================

(define (name-already-qualified? name prefix)
  "Check if a symbol name already starts with the prefix."
  (let ((name-str (symbol->string name))
        (prefix-str (string-append prefix "_")))
    (and (>= (string-length name-str) (string-length prefix-str))
         (string=? prefix-str (substring name-str 0 (string-length prefix-str))))))

(define (extract-define-name form)
  "Extract the name from a define form, or #f if not a define."
  (and (pair? form)
       (eq? (car form) 'define)
       (pair? (cdr form))
       (cond
         ((symbol? (cadr form)) (cadr form))
         ((and (pair? (cadr form)) (symbol? (caadr form))) (caadr form))
         (else #f))))

(define (extract-record-predicate form)
  "Extract the predicate name from a define-record-type form, or #f.
   Form: (define-record-type Name (constructor ...) predicate? (field accessor) ...)"
  (and (pair? form)
       (eq? (car form) 'define-record-type)
       (>= (length form) 4)
       (let ((pred (list-ref form 3)))
         (and (symbol? pred) pred))))

(define *scheme-builtins*
  '(symbol? number? string? pair? null? boolean? char? vector? procedure?
    list? integer? zero? positive? negative? exact? inexact? complex? real? rational?
    list map for-each append reverse length member assoc
    not and or if cond case when unless begin let let* letrec letrec* do
    define define-record-type lambda set! quote
    car cdr cons cadr caddr caar cdar cddr caadr cdadr caddr
    + - * / = < > <= >= min max abs
    eq? eql? equal? string=? char=?
    values call-with-values apply
    error guard raise with-exception-handler
    display write newline read
    open-input-file open-output-file close-port
    string-append string-length string-ref substring number->string string->number
    make-string string->list list->string string-copy
    exact truncate floor ceiling round inexact
    expt sqrt log exp sin cos tan asin acos atan))

(define (is-scheme-builtin? sym)
  (memq sym *scheme-builtins*))

(define (build-rename-map forms prefix)
  "Build an alist mapping short names to qualified names for all define and
   define-record-type predicate forms.
   Names that shadow Scheme builtins are excluded from the rename map but
   tracked separately in the builtin-shadows list for definition-only renaming."
  (let loop ((rest forms) (acc '()))
    (if (null? rest)
        acc
        (let* ((form (car rest))
               (name (extract-define-name form))
               (pred (extract-record-predicate form)))
          (let ((acc2 (if (and name (not (name-already-qualified? name prefix))
                              (not (is-scheme-builtin? name)))
                         (cons (cons name (qualify-name prefix name)) acc)
                         acc)))
            (let ((acc3 (if (and pred (is-scheme-builtin? pred)
                                (not (name-already-qualified? pred prefix)))
                           (cons (cons pred (qualify-name prefix pred)) acc2)
                           acc2)))
              (loop (cdr rest) acc3)))))))

(define (build-builtin-shadow-map forms prefix)
  "Build a rename map for definitions that shadow Scheme builtins.
   These are renamed at the definition site only, not at usage sites."
  (let loop ((rest forms) (acc '()))
    (if (null? rest) acc
        (let* ((form (car rest))
               (name (extract-define-name form)))
          (loop (cdr rest)
                (if (and name (is-scheme-builtin? name) (not (name-already-qualified? name prefix)))
                    (cons (cons name (qualify-name prefix name)) acc)
                    acc))))))

(define (rename-definition-only form shadow-map)
  "Rename just the definition name (not body references) for builtin-shadowing definitions."
  (if (or (not (pair? form)) (null? shadow-map))
      form
      (cond
        ((eq? (car form) 'define)
         (let* ((name (extract-define-name form))
                (entry (and name (assq name shadow-map))))
           (if entry
               (cons 'define (cons (cdr entry) (cddr form)))
               form)))
        (else form))))

(define (rename-lookup sym renames)
  "Look up a symbol in the rename map. Returns the renamed symbol or #f."
  (let loop ((rest renames))
    (cond
      ((null? rest) #f)
      ((eq? sym (caar rest)) (cdar rest))
      (else (loop (cdr rest))))))

(define (rewrite-form form renames shadowed)
  "Rewrite all symbol references in form according to renames, respecting shadowed bindings.
   shadowed is a list of symbols that are locally bound and should not be renamed."
  (cond
    ;; Atoms
    ((symbol? form)
     (if (memq form shadowed)
         form
         (or (rename-lookup form renames) form)))
    ((not (pair? form)) form)
    ;; Special forms that introduce bindings
    ((eq? (car form) 'lambda)
     ;; (lambda (params...) body...)
     (let* ((params (cadr form))
            (param-names (if (pair? params)
                            (let loop ((p params) (acc '()))
                              (cond
                                ((null? p) acc)
                                ((symbol? p) (cons p acc))  ;; rest arg
                                ((pair? p) (loop (cdr p) (cons (car p) acc)))
                                (else acc)))
                            (if (symbol? params) (list params) '())))
            (new-shadowed (append param-names shadowed)))
       (cons 'lambda
             (cons params
                   (map (lambda (b) (rewrite-form b renames new-shadowed))
                        (cddr form))))))
    ((eq? (car form) 'let)
     ;; (let ((var expr) ...) body...) or named let (let name ((var expr) ...) body...)
     (if (and (pair? (cdr form)) (symbol? (cadr form)))
         ;; Named let: (let name ((var expr) ...) body...)
         (let* ((name (cadr form))
                (bindings (caddr form))
                (body (cdddr form))
                (bind-names (map car bindings))
                (new-shadowed (cons name (append bind-names shadowed))))
           (cons 'let
                 (cons name
                       (cons (map (lambda (b)
                                    (list (car b)
                                          (rewrite-form (cadr b) renames new-shadowed)))
                                  bindings)
                             (map (lambda (b) (rewrite-form b renames new-shadowed))
                                  body)))))
         ;; Regular let
         (let* ((bindings (cadr form))
                (body (cddr form))
                (bind-names (map car bindings))
                (new-shadowed (append bind-names shadowed)))
           (cons 'let
                 (cons (map (lambda (b)
                              (list (car b)
                                    (rewrite-form (cadr b) renames shadowed)))
                            bindings)
                       (map (lambda (b) (rewrite-form b renames new-shadowed))
                            body))))))
    ((eq? (car form) 'let*)
     ;; let* bindings are sequential
     (let* ((bindings (cadr form))
            (body (cddr form)))
       (let bind-loop ((rest bindings) (cur-shadowed shadowed) (new-bindings '()))
         (if (null? rest)
             (cons 'let*
                   (cons (reverse new-bindings)
                         (map (lambda (b) (rewrite-form b renames cur-shadowed))
                              body)))
             (let* ((b (car rest))
                    (bname (car b))
                    (bexpr (rewrite-form (cadr b) renames cur-shadowed)))
               (bind-loop (cdr rest)
                          (cons bname cur-shadowed)
                          (cons (list bname bexpr) new-bindings)))))))
    ((eq? (car form) 'letrec)
     (let* ((bindings (cadr form))
            (body (cddr form))
            (bind-names (map car bindings))
            (new-shadowed (append bind-names shadowed)))
       (cons 'letrec
             (cons (map (lambda (b)
                          (list (car b)
                                (rewrite-form (cadr b) renames new-shadowed)))
                        bindings)
                   (map (lambda (b) (rewrite-form b renames new-shadowed))
                        body)))))
    ((eq? (car form) 'define)
     ;; (define name expr) or (define (name args...) body...)
     (cond
       ((and (pair? (cdr form)) (symbol? (cadr form)))
        (let ((qname (or (rename-lookup (cadr form) renames) (cadr form))))
          (cons 'define
                (cons qname
                      (map (lambda (b) (rewrite-form b renames shadowed))
                           (cddr form))))))
       ((and (pair? (cdr form)) (pair? (cadr form)))
        (let* ((name (caadr form))
               (params (cdadr form))
               (qname (or (rename-lookup name renames) name))
               (param-names (let loop ((p params) (acc '()))
                              (cond
                                ((null? p) acc)
                                ((symbol? p) (cons p acc))
                                ((pair? p) (loop (cdr p) (cons (car p) acc)))
                                (else acc))))
               (new-shadowed (append param-names shadowed)))
          (cons 'define
                (cons (cons qname params)
                      (map (lambda (b) (rewrite-form b renames new-shadowed))
                           (cddr form))))))
       (else form)))
    ((eq? (car form) 'define-record-type)
     ;; (define-record-type Name (constructor fields...) predicate? (field accessor) ...)
     ;; Rename predicate if it's in the rename map
     (if (>= (length form) 4)
         (let* ((type-name (list-ref form 1))
                (constructor (list-ref form 2))
                (predicate (list-ref form 3))
                (field-specs (list-tail form 4))
                (new-pred (if (symbol? predicate)
                             (or (rename-lookup predicate renames) predicate)
                             predicate)))
           (append (list 'define-record-type type-name constructor new-pred) field-specs))
         form))
    ((memq (car form) '(quote))
     form)  ;; Don't rewrite inside quotes
    ((eq? (car form) 'cond)
     (cons 'cond
           (map (lambda (clause)
                  (map (lambda (e) (rewrite-form e renames shadowed)) clause))
                (cdr form))))
    (else
     ;; General form: rewrite all sub-expressions
     (map (lambda (sub) (rewrite-form sub renames shadowed)) form))))

;; ============================================================================
;; Symbol sanitization: replace primed identifiers with underscored ones
;; ============================================================================

;; The code generator applies sanitizeWithUnderscores to variable references
;; (converting t' to t_) but not to lambda parameters. This creates mismatches.
;; We fix this by sanitizing all primed symbols throughout the form.

(define (sanitize-primed-symbol sym)
  "If sym ends with a single-quote, replace it with underscore."
  (let* ((s (symbol->string sym))
         (len (string-length s)))
    (if (and (> len 1) (char=? (string-ref s (- len 1)) #\'))
        (string->symbol (string-append (substring s 0 (- len 1)) "_"))
        sym)))

(define (sanitize-primed-forms form)
  "Walk form and replace all primed symbols with underscored equivalents."
  (cond
    ((symbol? form) (sanitize-primed-symbol form))
    ((not (pair? form)) form)
    ((eq? (car form) 'quote) form)
    (else (map sanitize-primed-forms form))))

;; ============================================================================
;; Reserved-word parameter sanitization
;; ============================================================================

;; The code generator applies sanitizeWithUnderscores to variable references
;; (appending _ to reserved words like "t"), but lambda parameters only get
;; convertCaseCamelToLowerSnake. We fix this by renaming lambda params and
;; let binding names that are Lisp reserved words.

(define *lisp-reserved-words*
  '(t nil lambda list cond case let fn def defn defun defvar defmacro
    setq setf quote and or not when unless do loop if else
    progn begin car cdr cons first second third rest
    apply funcall eval load require
    define define-record-type syntax-rules))

(define (is-lisp-reserved? sym)
  (memq sym *lisp-reserved-words*))

(define (sanitize-reserved-param sym)
  "If sym is a Lisp reserved word, append underscore."
  (if (is-lisp-reserved? sym)
      (string->symbol (string-append (symbol->string sym) "_"))
      sym))

(define (rename-symbol-in-form form old-sym new-sym)
  "Rename all free occurrences of old-sym to new-sym in form,
   respecting lambda and let binding scopes."
  (cond
    ((eq? form old-sym) new-sym)
    ((not (pair? form)) form)
    ((eq? (car form) 'quote) form)
    ((eq? (car form) 'lambda)
     (let* ((params (cadr form))
            (param-list (cond ((pair? params) params)
                              ((symbol? params) (list params))
                              (else '()))))
       (if (memq old-sym param-list)
           form  ;; shadowed by lambda param
           (cons 'lambda
                 (cons params
                       (map (lambda (b) (rename-symbol-in-form b old-sym new-sym))
                            (cddr form)))))))
    (else
     (map (lambda (sub) (rename-symbol-in-form sub old-sym new-sym)) form))))

(define (sanitize-reserved-params form)
  "Walk form and rename lambda parameters that are Lisp reserved words
   by appending underscore. Also renames all references in the body."
  (cond
    ((not (pair? form)) form)
    ((eq? (car form) 'quote) form)
    ((eq? (car form) 'lambda)
     ;; (lambda (params...) body...)
     (let* ((params (cadr form))
            (body (cddr form))
            (renames '())
            (new-params
              (if (pair? params)
                  (map (lambda (p)
                         (if (and (symbol? p) (is-lisp-reserved? p))
                             (let ((new-p (sanitize-reserved-param p)))
                               (set! renames (cons (cons p new-p) renames))
                               new-p)
                             p))
                       params)
                  (if (and (symbol? params) (is-lisp-reserved? params))
                      (begin
                        (set! renames (cons (cons params (sanitize-reserved-param params)) renames))
                        (sanitize-reserved-param params))
                      params))))
       (if (null? renames)
           ;; No changes needed, just recurse into body
           (cons 'lambda (cons params (map sanitize-reserved-params body)))
           ;; Rename params in body, then recurse into the renamed body
           (let ((renamed-body
                   (let rename-loop ((pairs renames) (b body))
                     (if (null? pairs) b
                         (rename-loop (cdr pairs)
                                      (map (lambda (f)
                                             (rename-symbol-in-form f (caar pairs) (cdar pairs)))
                                           b))))))
             (cons 'lambda (cons new-params (map sanitize-reserved-params renamed-body)))))))
    (else
     (map sanitize-reserved-params form))))

;; ============================================================================
;; Letrec fixpoint transformation
;; ============================================================================

;; The Lisp code generator translates Haskell's recursive `let x = f x in body`
;; as `((lambda (x) body) (f x))`. In Haskell this works because let is lazy,
;; but in Scheme x is unbound when evaluating (f x). We detect this pattern and
;; transform it to (letrec ((x init)) body) which provides recursive binding.

(define (has-free-ref? form sym bound)
  "Check if form contains a free reference to sym, given a set of bound variables."
  (cond
    ((eq? form sym) (not (memq sym bound)))
    ((not (pair? form)) #f)
    ((eq? (car form) 'quote) #f)
    ((eq? (car form) 'lambda)
     (let* ((params (cadr form))
            (param-names (if (pair? params)
                            (let loop ((p params) (acc '()))
                              (cond
                                ((null? p) acc)
                                ((symbol? p) (cons p acc))
                                ((pair? p) (loop (cdr p) (cons (car p) acc)))
                                (else acc)))
                            (if (symbol? params) (list params) '())))
            (new-bound (append param-names bound)))
       (let body-loop ((rest (cddr form)))
         (cond
           ((null? rest) #f)
           ((has-free-ref? (car rest) sym new-bound) #t)
           (else (body-loop (cdr rest)))))))
    (else
     (or (has-free-ref? (car form) sym bound)
         (has-free-ref? (cdr form) sym bound)))))

(define (fix-letrec form)
  "Walk form and transform self-referential ((lambda (X) BODY) INIT) into
   (letrec ((X INIT)) BODY). Also handles the Y-combinator pattern where
   BODY is (X ...).
   Tracks outer-bound variables to avoid false positives where a same-named
   variable from an enclosing scope is mistaken for a self-reference."
  (fix-letrec-inner form '()))

(define (fix-letrec-inner form outer-bound)
  (cond
    ((not (pair? form)) form)
    ((eq? (car form) 'quote) form)
    ;; Check for ((lambda (X) BODY...) INIT) pattern
    ((and (pair? (car form))
          (eq? (caar form) 'lambda)
          (pair? (cdr form))
          (null? (cddr form))  ;; exactly one argument
          (pair? (cadar form))
          (null? (cdr (cadar form)))  ;; exactly one parameter
          (symbol? (caadar form)))
     (let* ((param (caadar form))
            (body-forms (cddar form))
            (init (cadr form)))
       (if (and
             ;; Only trigger if param is NOT already bound by an outer scope
             (not (memq param outer-bound))
             ;; Never trigger when init is just a bare symbol (including param itself).
             ;; ((lambda (x) body) x) is identity substitution, not self-reference.
             ;; ((lambda (x) body) y) where y is a variable is also not recursive.
             (not (symbol? init))
             (or
               ;; Pattern 1: init has a free reference to param
               (has-free-ref? init param '())
               ;; Pattern 2: body is (param ...) — Y-combinator fixpoint
               (and (= (length body-forms) 1)
                    (pair? (car body-forms))
                    (eq? (caar body-forms) param))))
           ;; Transform to letrec with lambda wrapper
           (let ((fixed-init (fix-letrec-inner init outer-bound))
                 (fixed-body (map (lambda (f) (fix-letrec-inner f outer-bound)) body-forms)))
             (if (= (length fixed-body) 1)
                 `(letrec ((,param (lambda args (apply ,fixed-init args))))
                    ,(car fixed-body))
                 `(letrec ((,param (lambda args (apply ,fixed-init args))))
                    (begin ,@fixed-body))))
           ;; Not self-referential, recurse normally
           ;; Track the lambda parameter as outer-bound for inner expressions
           (let ((new-bound (cons param outer-bound)))
             (cons (cons 'lambda
                         (cons (cadar form)
                               (map (lambda (f) (fix-letrec-inner f new-bound)) body-forms)))
                   (list (fix-letrec-inner init outer-bound)))))))
    ;; Track lambda parameters for other lambdas too
    ((eq? (car form) 'lambda)
     (let* ((params (cadr form))
            (param-list (cond ((pair? params) params)
                              ((symbol? params) (list params))
                              (else '())))
            (new-bound (append param-list outer-bound)))
       (cons 'lambda
             (cons params
                   (map (lambda (f) (fix-letrec-inner f new-bound)) (cddr form))))))
    (else
     (map (lambda (f) (fix-letrec-inner f outer-bound)) form))))

;; ============================================================================
;; Short-circuit if_else transformation
;; ============================================================================

;; The code generator translates Haskell's `if cond then else` as eager curried
;; calls: (((hydra_lib_logic_if_else cond) then) else). In strict Scheme both
;; branches are evaluated, causing infinite recursion for recursive functions
;; that use if_else for termination. We transform these into Scheme's native
;; short-circuiting (if ...) special form, matching what the CL loader does.
;;
;; Also fixes the cond case-dispatch pattern where else branches incorrectly
;; apply a result value as a function with the dispatch variable.

(define (last-element lst)
  "Return the last element of a proper list."
  (if (null? (cdr lst)) (car lst) (last-element (cdr lst))))

(define (drop-last lst)
  "Drop the last element of a list, returning the remaining elements as a form.
   For 2-element list (A B), returns A (the function position).
   For 3+ element list (A B C), returns (A B) (function + remaining args)."
  (cond
    ((null? (cdr lst)) (car lst))
    ((null? (cddr lst)) (car lst))     ;; (A B) -> A
    (else (let loop ((rest lst) (acc '()))
            (if (null? (cddr rest))
                (reverse (cons (car rest) acc))
                (loop (cdr rest) (cons (car rest) acc)))))))

(define (else-head-is-function? head dispatch-var)
  "Check if the head of a cond else expression is a function that legitimately
   takes dispatch-var as an argument. Returns #t for known function patterns,
   #f otherwise (meaning it's safe to strip dispatch-var).
   dispatch-var is the common dispatch variable from the non-else branches."
  (cond
    ;; Literal values are never functions
    ((boolean? head) #f)
    ((number? head) #f)
    ((string? head) #f)
    ((and (pair? head) (eq? (car head) 'quote)) #f)
    ;; Bare symbol that IS the dispatch variable — it's the default case returning
    ;; the original value unchanged. Safe to strip.
    ((and (symbol? head) (eq? head dispatch-var)) #f)
    ;; Other bare symbol — could be a function variable like dflt, recurse, etc.
    ((symbol? head) #t)
    ;; Not a pair and not a symbol — safe
    ((not (pair? head)) #f)
    ;; Lambda is obviously a function
    ((and (pair? head) (eq? (car head) 'lambda)) #t)
    ;; list, cons, make-* constructors produce values
    ((and (symbol? (car head)) (eq? (car head) 'list)) #f)
    ((and (symbol? (car head)) (eq? (car head) 'cons)) #f)
    ((and (symbol? (car head))
          (let ((s (symbol->string (car head))))
            (and (> (string-length s) 5)
                 (string=? "make-" (substring s 0 5)))))
     #f)
    ;; Default: assume function call results could be either.
    ;; Most generated else heads are value-constructing expressions (list ..., make-* ..., etc.)
    ;; so defaulting to #f (strip) is safe for the common case.
    (else #f)))

(define (find-dispatch-var clauses)
  "Find the common dispatch variable in a cond's case-dispatch pattern.
   Returns the variable symbol if found, #f otherwise.
   Each non-else clause should look like: (TEST ((lambda (PARAM) BODY) DISPVAR))"
  (let loop ((cls clauses) (found-var #f))
    (cond
      ((null? cls) found-var)
      ;; Skip else clause
      ((and (pair? (car cls)) (eq? (caar cls) 'else))
       (loop (cdr cls) found-var))
      ;; Check normal clause: (TEST EXPR)
      ((and (pair? (car cls))
            (>= (length (car cls)) 2))
       (let* ((clause (car cls))
              (expr (cadr clause)))  ;; The result expression
         ;; Should be ((lambda (PARAM) BODY) DISPVAR)
         (if (and (pair? expr)
                  (pair? (cdr expr))
                  (null? (cddr expr))         ;; exactly 2 elements
                  (pair? (car expr))           ;; first is a subexpr
                  (eq? (caar expr) 'lambda)   ;; first is a lambda
                  (symbol? (cadr expr)))       ;; second is a symbol (DISPVAR)
             (let ((var (cadr expr)))
               (if (or (not found-var) (eq? found-var var))
                   (loop (cdr cls) var)
                   #f))  ;; Inconsistent vars, abort
             ;; Not the expected pattern — might still have the var elsewhere
             (loop (cdr cls) found-var))))
      (else (loop (cdr cls) found-var)))))

(define (fix-if-else form)
  "Walk form and transform (((hydra_lib_logic_if_else C) T) E) into (if C T E).
   Also transforms ((hydra_lib_logic_and A) B) -> (and A B) and
   ((hydra_lib_logic_or A) B) -> (or A B) for short-circuit evaluation.
   Also fixes cond else clauses where literals are called as functions:
   (else (LITERAL expr)) -> (else LITERAL) when LITERAL is #t, #f, a number, or '()."
  (cond
    ((not (pair? form)) form)
    ((eq? (car form) 'quote) form)
    ;; Detect ((hydra_lib_logic_and A) B) -> (and A B)
    ;; and ((hydra_lib_logic_or A) B) -> (or A B)
    ;; Structure: form = (X B) where X = (hydra_lib_logic_and A)
    ((and (pair? (car form))
          (pair? (cdr form))
          (null? (cddr form))                 ;; exactly one arg (B)
          (symbol? (caar form))
          (or (eq? (caar form) 'hydra_lib_logic_and)
              (eq? (caar form) 'hydra_lib_logic_or))
          (pair? (cdar form))
          (null? (cddar form)))               ;; exactly one arg for X (A)
     (let ((op (if (eq? (caar form) 'hydra_lib_logic_and) 'and 'or))
           (a (fix-if-else (cadar form)))
           (b (fix-if-else (cadr form))))
       (list op a b)))
    ;; Detect (((hydra_lib_logic_if_else C) T) E)
    ;; Structure: form = (X E) where X = (Y T) where Y = (hydra_lib_logic_if_else C)
    ;; So: form = (((hydra_lib_logic_if_else C) T) E)
    ;;   car form = ((hydra_lib_logic_if_else C) T)
    ;;   caar form = (hydra_lib_logic_if_else C)
    ;;   caaar form = hydra_lib_logic_if_else  (a symbol)
    ((and (pair? (car form))                  ;; form = (X E ...)
          (pair? (cdr form))
          (null? (cddr form))                 ;; exactly one arg (E)
          (pair? (caar form))                 ;; X = (Y T ...)
          (pair? (cdar form))
          (null? (cddar form))                ;; exactly one arg for X (T)
          (symbol? (caaar form))              ;; Y starts with a symbol
          (eq? (caaar form) 'hydra_lib_logic_if_else)
          (pair? (cdaar form))                ;; Y has an arg (C)
          (null? (cddaar form)))              ;; exactly one arg for Y
     (let ((cond-form (fix-if-else (cadaar form)))         ;; C
           (then-form (fix-if-else (cadar form)))          ;; T
           (else-form (fix-if-else (cadr form))))          ;; E
       (list 'if cond-form then-form else-form)))
    ;; Fix cond clauses for the generated case-dispatch pattern.
    ;; Generated code has: (cond ((equal? X 'tag) ((lambda (b) ...) VAR)) ... (else (EXPR VAR)))
    ;; The else branch incorrectly calls EXPR as a function with VAR.
    ;; We detect the pattern by checking if non-else branches apply a lambda to a common variable,
    ;; and if the else branch applies a non-lambda to that same variable.
    ;; Lambda: recurse into body forms only, NOT the parameter list
    ((eq? (car form) 'lambda)
     (if (and (pair? (cdr form)) (pair? (cddr form)))
         (cons* 'lambda (cadr form) (map fix-if-else (cddr form)))
         form))
    ;; Letrec: recurse into binding values and body, not binding names
    ((eq? (car form) 'letrec)
     (if (and (pair? (cdr form)) (pair? (cadr form)))
         (cons* 'letrec
                (map (lambda (binding)
                       (if (and (pair? binding) (pair? (cdr binding)))
                           (cons (car binding) (map fix-if-else (cdr binding)))
                           binding))
                     (cadr form))
                (map fix-if-else (cddr form)))
         form))
    ((eq? (car form) 'cond)
     (let* ((clauses (cdr form))
            ;; Find the common "dispatch variable" from non-else branches
            ;; Each branch should be: (TEST ((lambda (PARAM) BODY) DISPVAR))
            (dispatch-var (find-dispatch-var clauses))
            (has-else? (and (pair? clauses)
                            (let ((last-clause (let lp ((c clauses))
                                                 (if (null? (cdr c)) (car c) (lp (cdr c))))))
                              (and (pair? last-clause) (eq? (car last-clause) 'else))))))
       (let ((fixed-clauses
               (map (lambda (clause)
                      (if (and dispatch-var
                               (pair? clause)
                               (eq? (car clause) 'else)
                               (pair? (cdr clause))
                               (null? (cddr clause))          ;; (else SINGLE-EXPR)
                               (pair? (cadr clause))          ;; expr is a pair (EXPR VAR)
                               (let ((expr (cadr clause)))
                                 (and (pair? expr)
                                      (pair? (cdr expr))
                                      ;; Check that the last arg matches dispatch-var
                                      (eq? (last-element expr) dispatch-var)
                                      ;; And the head is NOT a lambda (lambdas are legitimate)
                                      (not (and (pair? (car expr))
                                                (eq? (caar expr) 'lambda))))))
                          ;; (else (VALUE dispatch-var)) -> (else VALUE)
                          (list 'else (fix-if-else (drop-last (cadr clause))))
                          ;; Normal clause: recurse into all exprs
                          (map fix-if-else clause)))
                    clauses)))
         ;; If there's a dispatch-var pattern but no else clause,
         ;; add a default else that raises an error (exhaustive match in Haskell
         ;; but Scheme cond returns void without else).
         (if (and dispatch-var (not has-else?))
             (cons 'cond (append fixed-clauses
                                 (list (list 'else
                                             (list 'error
                                                   (list 'string-append
                                                         "non-exhaustive pattern match on: "
                                                         (list 'let (list (list 'tmp dispatch-var))
                                                               (list 'if (list 'pair? 'tmp)
                                                                     (list 'symbol->string (list 'car 'tmp))
                                                                     (list 'let (list (list 'p (list 'open-output-string)))
                                                                           (list 'write 'tmp 'p)
                                                                           (list 'get-output-string 'p))))))))))
             (cons 'cond fixed-clauses)))))
    (else
     (map fix-if-else form))))

;; ============================================================================
;; Fix quoted map alists
;; ============================================================================

;; The code generator produces map literals as: (list 'map '((KEY . VAL) ...))
;; where KEY and VAL are (list 'literal ...) forms.  The Scheme quote prevents
;; these inner (list ...) forms from being evaluated, leaving raw symbols.
;; We detect quoted alists whose entries contain (list ...) sub-forms and
;; convert them to (list (cons KEY VAL) ...) so each entry is evaluated.

(define (quoted-alist-needs-eval? datum)
  "Check if datum is a list of dotted pairs containing (list ...) forms."
  (and (pair? datum)
       (let check ((entries datum))
         (or (null? entries)
             (and (pair? entries)
                  (pair? (car entries))
                  ;; Check that key or value is a (list ...) form
                  (let ((k (caar entries))
                        (v (cdar entries)))
                    (or (and (pair? k) (eq? (car k) 'list))
                        (and (pair? v) (eq? (car v) 'list))))
                  (check (cdr entries)))))))

(define (requote-datum datum)
  "Convert a quoted datum back to a form that evaluates to the same structure.
   (list (quote sym) ...) -> (list 'sym ...) i.e. re-wrap as executable."
  (cond
    ((and (pair? datum) (eq? (car datum) 'list))
     ;; (list (quote sym) args...) -> (list (quote sym) requoted-args...)
     (cons 'list (map requote-datum (cdr datum))))
    ((and (pair? datum) (eq? (car datum) 'quote))
     ;; (quote X) -> (quote X) -- keep as-is, it's already a quote form
     datum)
    ((pair? datum)
     ;; Unknown pair - requote both sides
     (cons (requote-datum (car datum)) (requote-datum (cdr datum))))
    ((symbol? datum) (list 'quote datum))
    ((string? datum) datum)
    ((number? datum) datum)
    ((boolean? datum) datum)
    ((null? datum) (list 'quote '()))
    (else datum)))

(define (unquote-alist-entry entry)
  "Convert a quoted alist entry ((list ...) . (list ...)) to (cons KEY VAL)."
  (list 'cons (requote-datum (car entry)) (requote-datum (cdr entry))))

(define (fix-quoted-maps form)
  "Walk form and fix quoted map alists so inner (list ...) forms are evaluated."
  (cond
    ((not (pair? form)) form)
    ;; Detect (quote ALIST) where ALIST is alist with (list ...) entries
    ((and (eq? (car form) 'quote)
          (pair? (cdr form))
          (null? (cddr form))
          (pair? (cadr form))
          (quoted-alist-needs-eval? (cadr form)))
     ;; Convert '((K . V) ...) to (list (cons K' V') ...)
     (cons 'list (map unquote-alist-entry (cadr form))))
    ((eq? (car form) 'quote) form)
    (else (map fix-quoted-maps form))))

;; ============================================================================
;; Record accessor aliasing
;; ============================================================================

;; The code generator produces lowercase accessor names like (application-function v)
;; but Scheme's define-record-type creates capitalized ones like Application-function.
;; We extract record type definitions and create lowercase aliases.

(define (lowercase-string s)
  "Convert a string to lowercase."
  (let* ((len (string-length s))
         (out (make-string len)))
    (let loop ((i 0))
      (if (>= i len) out
          (begin
            (string-set! out i (char-downcase (string-ref s i)))
            (loop (+ i 1)))))))

(define (camel-to-snake s)
  "Convert CamelCase to snake_case: 'TypeScheme' -> 'type_scheme'."
  (let* ((len (string-length s)))
    (let loop ((i 0) (acc '()))
      (if (>= i len)
          (list->string (reverse acc))
          (let ((ch (string-ref s i)))
            (if (and (char-upper-case? ch) (> i 0))
                (loop (+ i 1) (cons (char-downcase ch) (cons #\_ acc)))
                (loop (+ i 1) (cons (char-downcase ch) acc))))))))

(define (extract-record-aliases form)
  "From a define-record-type form, return a list of (snake_case-accessor . actual-accessor) pairs.
   Also includes constructor, predicate aliases.
   Form: (define-record-type TypeName (make-TypeName fields...) predicate? (field Accessor) ...)"
  (if (and (pair? form)
           (eq? (car form) 'define-record-type)
           (>= (length form) 4))
      (let* ((type-name (list-ref form 1))
             (constructor-form (list-ref form 2))
             (predicate (list-ref form 3))
             (field-specs (list-tail form 4))
             (type-str (symbol->string type-name))
             (snake-type (camel-to-snake type-str))
             ;; Build accessor aliases: TypeName-field -> type_name-field
             (accessor-aliases
               (let loop ((specs field-specs) (acc '()))
                 (if (null? specs) acc
                     (if (and (pair? (car specs)) (>= (length (car specs)) 2))
                         (let* ((field-name (symbol->string (car (car specs))))
                                (accessor (cadr (car specs)))
                                (acc-str (symbol->string accessor))
                                ;; The code generator produces: snake_type-field_name
                                (snake-acc (string->symbol
                                             (string-append snake-type "-" field-name))))
                           (if (eq? snake-acc accessor)
                               (loop (cdr specs) acc)  ;; already matches
                               (loop (cdr specs) (cons (cons snake-acc accessor) acc))))
                         (loop (cdr specs) acc)))))
             ;; Build constructor alias: make-type_name for make-TypeName
             (constructor-name (if (pair? constructor-form) (car constructor-form) #f))
             (constructor-alias
               (if constructor-name
                   (let* ((snake-cstr (string->symbol
                                        (string-append "make-" snake-type))))
                     (if (eq? snake-cstr constructor-name) '()
                         (list (cons snake-cstr constructor-name))))
                   '()))
             ;; Build predicate alias: type_name? for typeName?
             (predicate-alias
               (if (symbol? predicate)
                   (let* ((snake-pred (string->symbol
                                        (string-append snake-type "?"))))
                     (if (eq? snake-pred predicate) '()
                         (list (cons snake-pred predicate))))
                   '())))
        (append accessor-aliases constructor-alias predicate-alias))
      '()))

(define (install-record-aliases forms)
  "Extract all record type aliases from forms and install them."
  (for-each
    (lambda (form)
      (let ((aliases (extract-record-aliases form)))
        (for-each
          (lambda (pair)
            (guard (exn (#t #t))  ;; ignore errors if original not defined yet
              (let ((val (eval (cdr pair) (interaction-environment))))
                (eval `(define ,(car pair) ,val) (interaction-environment)))))
          aliases)))
    forms))

;; ============================================================================
;; File loading with retry
;; ============================================================================

(define (hydra-load-file path)
  "Load a generated Scheme file, stripping define-library wrapper.
   Generated code uses fully-qualified names, so no renaming is needed.
   The transformation passes (sanitize, fix-letrec, fix-if-else, fix-quoted-maps)
   are now no-ops for generated code but are retained for safety.
   Uses retry passes for forward references."
  (call-with-input-file path
    (lambda (port)
      (let loop ()
        (let ((form (read port)))
          (unless (eof-object? form)
            (cond
              ;; Strip define-library: extract (begin ...) body forms
              ((and (pair? form) (eq? (car form) 'define-library))
               (let* ((lib-name (cadr form))
                      (prefix (library-name->prefix lib-name)))
                 (let find-begin ((rest (cddr form)))
                   (cond
                     ((null? rest) #t)
                     ((and (pair? (car rest)) (eq? (caar rest) 'begin))
                      (let* ((body-forms (map sanitize-reserved-params (map sanitize-primed-forms (cdar rest))))
                             (renames (build-rename-map body-forms prefix))
                             (shadow-map (build-builtin-shadow-map body-forms prefix))
                             (rewritten (map (lambda (f)
                                              (if (and (pair? f)
                                                       (memq (car f) '(define define-record-type)))
                                                  (rewrite-form f renames '())
                                                  f))
                                            body-forms))
                             ;; Rename definitions that shadow builtins (definition name only)
                             (shadow-fixed (map (lambda (f) (rename-definition-only f shadow-map)) rewritten))
                             (fixed (map fix-letrec shadow-fixed))
                             (if-fixed (map fix-if-else fixed))
                             (map-fixed (map fix-quoted-maps if-fixed))
                             )
                        (hydra-eval-with-retry map-fixed)))
                     (else (find-begin (cdr rest)))))))
              ;; Non-library forms: evaluate directly
              (else (eval form (interaction-environment))))
            (loop)))))))

(define (hydra-eval-with-retry forms)
  "Evaluate forms with retry passes for forward references."
  (let ((failed '()))
    ;; First pass
    (for-each
      (lambda (f)
        (guard (exn (#t (set! failed (cons f failed))))
          (eval f (interaction-environment))
          ;; After successfully evaluating a define-record-type, install lowercase aliases
          (when (and (pair? f) (eq? (car f) 'define-record-type))
            (install-record-aliases (list f)))))
      forms)
    ;; Retry failed forms up to 10 times
    (set! failed (reverse failed))
    (let retry ((pass 0) (remaining failed))
      (if (and (pair? remaining) (< pass 10))
        (let ((still-failed '()))
          (for-each
            (lambda (f)
              (guard (exn (#t (set! still-failed (cons f still-failed))))
                (eval f (interaction-environment))))
            remaining)
          (retry (+ pass 1) (reverse still-failed)))
        ;; Report remaining failures and raise.
        (when (pair? remaining)
          (display (string-append "  FAIL: " (number->string (length remaining)) " form(s) failed after retries\n"))
          (force-output)
          (for-each
            (lambda (f)
              (guard (exn (#t #t))
                (guard (exn2 (#t
                  (display "  LOAD ERROR: ")
                  (guard (exn3 (#t (display "???\n") (force-output)))
                    (let ((op (open-output-string)))
                      (write f op)
                      (let ((s (get-output-string op)))
                        (display (if (> (string-length s) 300) (substring s 0 300) s))))
                    (display "\n  EXCEPTION: ")
                    (let ((op (open-output-string)))
                      (write exn2 op)
                      (let ((s (get-output-string op)))
                        (display (if (> (string-length s) 300) (substring s 0 300) s)))))
                  (newline)
                  (force-output)))
                (eval f (interaction-environment)))))
            remaining)
          (error "hydra-eval-with-retry: unresolved forms after 10 retries"))))))

(define (hydra-load-native-lib path)
  "Load a native library file, stripping define-library wrapper but NOT renaming definitions.
   Native libs already use fully-qualified names for their exports and may have
   internal helpers that should stay unprefixed."
  (call-with-input-file path
    (lambda (port)
      (let loop ()
        (let ((form (read port)))
          (unless (eof-object? form)
            (cond
              ;; Strip define-library: extract (begin ...) body forms
              ((and (pair? form) (eq? (car form) 'define-library))
               (let find-begin ((rest (cddr form)))
                 (cond
                   ((null? rest) #t)
                   ((and (pair? (car rest)) (eq? (caar rest) 'begin))
                    (hydra-eval-with-retry (cdar rest)))
                   (else (find-begin (cdr rest))))))
              ;; Non-library forms: evaluate directly
              (else (eval form (interaction-environment))))
            (loop)))))))

(define (hydra-load-file-if-exists path)
  "Load file if it exists, silently skip otherwise."
  (when (file-exists? path)
    (hydra-load-file path)))

;; ============================================================================
;; Gen-main loading
;; ============================================================================

(define (hydra-load-gen-main base)
  "Load all generated main modules in dependency order."
  (let ((files '(
           ;; Core types
           "core.scm"
           "error/core.scm"
           "error/checking.scm"
           "error/packaging.scm"
           "context.scm"
           "graph.scm"
           "packaging.scm"
           "ast.scm"
           "coders.scm"
           "phantoms.scm"
           "parsing.scm"
           "query.scm"
           "relational.scm"
           "tabular.scm"
           "testing.scm"
           "topology.scm"
           "typing.scm"
           "util.scm"
           "variants.scm"
           "json/model.scm"
           "classes.scm"
           "constants.scm"
           "paths.scm"
           ;; Core operations
           "formatting.scm"
           "rewriting.scm"
           "sorting.scm"
           "names.scm"
           "arity.scm"
           "lexical.scm"
           "literals.scm"
           "reflect.scm"
           "languages.scm"
           "parsers.scm"
           "templates.scm"
           "strip.scm"
           "variables.scm"
           "scoping.scm"
           "dependencies.scm"
           "predicates.scm"
           "resolution.scm"
           "analysis.scm"
           "environment.scm"
           ;; Encoding/decoding
           "encoding.scm"
           "decoding.scm"
           "codegen.scm"
           "hoisting.scm"
           "show/core.scm"
           "show/error/core.scm"
           "show/errors.scm"
           "show/error/packaging.scm"
           "validate/core.scm"
           "validate/packaging.scm"
           "encode/core.scm"
           "encode/error/core.scm"
           "encode/error/checking.scm"
           "encode/errors.scm"
           "encode/packaging.scm"
           "decode/core.scm"
           "decode/error/core.scm"
           "decode/error/checking.scm"
           "decode/errors.scm"
           "decode/packaging.scm"
           "extract/core.scm"
           "extract/json.scm"
           ;; Higher-level operations
           "substitution.scm"
           "annotations.scm"
           "unification.scm"
           "inference.scm"
           "checking.scm"
           ;; Serialization
           "serialization.scm"
           ;; Reduction (needs most things)
           "reduction.scm"
           ;; JSON
           "json/coder.scm"
           )))
    (for-each
      (lambda (f)
        (let ((path (string-append base "/" f)))
          (when (file-exists? path)
            (display (string-append "  Loading " f "...\n"))
            (hydra-load-file path))))
      files)))
