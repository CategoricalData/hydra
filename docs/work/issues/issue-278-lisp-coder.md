# Issue #278: Create a Lisp Coder

> **GitHub Issue**: [#278 - Create a Lisp coder](https://github.com/CategoricalData/hydra/issues/278)
>
> **Status**: Open (research phase)
>
> **Created**: March 11, 2026
>
> **Category**: Code Generation

## Executive Summary

Create support for multiple Lisp dialects in Hydra, initially targeting
Clojure, Emacs Lisp, Common Lisp, and Scheme (R7RS).
The approach is to design either a shared or per-dialect syntax model,
build a Hydra-to-Lisp coder, and implement serialization to concrete syntax.

## Target Dialects

1. **Clojure** -- Most widely used modern Lisp. JVM-based. Immutable-first, persistent data structures.
2. **Emacs Lisp** -- Huge installed base. Dynamically typed, Lisp-2.
3. **Common Lisp** -- ANSI standard Lisp. Lisp-2, CLOS, conditions system.
4. **Scheme (R7RS)** -- Minimalist, Lisp-1. Required tail call optimization.

## Syntax Feature Matrix

The cells show concrete syntax for each dialect. Empty cells indicate the feature is unsupported or has no standard form.

### Literals

| Feature | Clojure | Emacs Lisp | Common Lisp | Scheme R7RS | Notes |
|---------|---------|------------|-------------|-------------|-------|
| Integer | `42`, `0xff`, `42N` | `42`, `#xff` | `42`, `#xff` | `42`, `#xff` | Clojure has BigInt suffix `N` |
| Float | `3.14`, `3.14M` | `3.14` | `3.14d0`, `3.14f0` | `3.14` | CL distinguishes float precisions |
| Boolean | `true`, `false` | `t`, `nil` | `t`, `nil` | `#t`, `#f` | Elisp/CL: nil is false + empty list |
| String | `"hello"` | `"hello"` | `"hello"` | `"hello"` | Escape sequences vary slightly |
| Character | `\a`, `\newline` | `?a`, `?\n` | `#\a`, `#\Newline` | `#\a`, `#\newline` | All four differ |
| Nil/null | `nil` | `nil` | `nil` | `'()` | Scheme: empty list and false are distinct |
| Keyword | `:foo`, `::foo` | `:foo` | `:foo` | -- | Scheme has no standard keywords |
| Symbol | `foo`, `ns/foo` | `foo` | `foo`, `pkg:sym` | `foo` | CL upcases by default |

### Collections

| Feature | Clojure | Emacs Lisp | Common Lisp | Scheme R7RS | Notes |
|---------|---------|------------|-------------|-------------|-------|
| List | `'(1 2 3)` | `'(1 2 3)` | `'(1 2 3)` | `'(1 2 3)` | Universal |
| Dotted pair | -- | `'(1 . 2)` | `'(1 . 2)` | `'(1 . 2)` | Clojure has no cons cells |
| Vector | `[1 2 3]` | `[1 2 3]` | `#(1 2 3)` | `#(1 2 3)` | Clojure/Elisp use `[]` |
| Hash map | `{:a 1 :b 2}` | alist or `#s(hash-table ...)` | alist, `make-hash-table` | alist | Only Clojure has map literals |
| Set | `#{1 2 3}` | -- | -- | -- | Only Clojure has set literals |

### Function Definition

| Feature | Clojure | Emacs Lisp | Common Lisp | Scheme R7RS | Notes |
|---------|---------|------------|-------------|-------------|-------|
| Named fn | `(defn f [x] ...)` | `(defun f (x) ...)` | `(defun f (x) ...)` | `(define (f x) ...)` | Clojure uses vector for params |
| Lambda | `(fn [x] ...)` | `(lambda (x) ...)` | `(lambda (x) ...)` | `(lambda (x) ...)` | Clojure: `fn`, others: `lambda` |
| Lambda shorthand | `#(+ %1 %2)` | -- | -- | -- | Clojure only |
| Multi-arity | `(defn f ([x] ...) ([x y] ...))` | `&optional` | `&optional` | `(case-lambda ...)` | All differ |
| Rest params | `[x & rest]` | `(x &rest r)` | `(x &rest r)` | `(x . rest)` | Clojure uses `&`, Scheme uses `.` |
| Function namespace | Lisp-1 | Lisp-1 | Lisp-2 | Lisp-1 | CL separates fn/var namespaces |

### Variable Binding

| Feature | Clojure | Emacs Lisp | Common Lisp | Scheme R7RS | Notes |
|---------|---------|------------|-------------|-------------|-------|
| Let | `(let [x 1 y 2] ...)` | `(let ((x 1) (y 2)) ...)` | `(let ((x 1) (y 2)) ...)` | `(let ((x 1) (y 2)) ...)` | Clojure uses vector, is sequential |
| Let* | `let` is already sequential | `(let* ((x 1) ...) ...)` | `(let* ((x 1) ...) ...)` | `(let* ((x 1) ...) ...)` | Clojure's `let` = `let*` |
| Destructuring | `(let [{:keys [a b]} m] ...)` | `(pcase-let ...)` | `(destructuring-bind ...)` | -- | Varies widely |
| Global def | `(def x 1)` | `(defvar x 1)` | `(defvar x 1)` | `(define x 1)` | |
| Constant | `(def ^:const x 1)` | `(defconst x 1)` | `(defconstant x 1)` | -- | Scheme has no constant form |

### Control Flow

| Feature | Clojure | Emacs Lisp | Common Lisp | Scheme R7RS | Notes |
|---------|---------|------------|-------------|-------------|-------|
| If | `(if t a b)` | `(if t a b ...)` | `(if t a b)` | `(if t a b)` | Elisp allows implicit progn in else |
| Cond | `(cond t1 e1 :else d)` | `(cond (t1 e1) (t d))` | `(cond (t1 e1) (t d))` | `(cond (t1 e1) (else d))` | Clojure uses flat pairs |
| Case | `(case x :a 1 :b 2 d)` | `(pcase x ...)` | `(case x (k1 b1) ...)` | `(case x ((d1) b1) ...)` | Constant dispatch only (except Elisp pcase) |
| And/or/not | `(and a b)` | `(and a b)` | `(and a b)` | `(and a b)` | Universal, short-circuiting |

### Data Types

| Feature | Clojure | Emacs Lisp | Common Lisp | Scheme R7RS | Notes |
|---------|---------|------------|-------------|-------------|-------|
| Record/struct | `(defrecord P [x y])` | `(cl-defstruct p x y)` | `(defstruct p x y)` | `(define-record-type ...)` | All differ in syntax |
| Sum types | -- | -- | -- | -- | None have native sum types |
| Type hints | `^String x` | -- | `(declare (type ...))` | -- | Clojure: Java interop hints |
| Newtype | `(defrecord W [value])` | `(cl-defstruct w value)` | `(defstruct w value)` | `(define-record-type ...)` | Single-field struct |

### Module System

| Feature | Clojure | Emacs Lisp | Common Lisp | Scheme R7RS | Notes |
|---------|---------|------------|-------------|-------------|-------|
| Declaration | `(ns my.core)` | `(provide 'my)` | `(defpackage :my ...)` | `(define-library ...)` | Elisp has no formal modules |
| Import | `(:require [x :as y])` | `(require 'x)` | `(:use :x)`, `(:import-from ...)` | `(import (x))` | |
| Export | public by default | convention only | `(:export :foo)` | `(export foo)` | Clojure: `defn-` for private |

### Other

| Feature | Clojure | Emacs Lisp | Common Lisp | Scheme R7RS | Notes |
|---------|---------|------------|-------------|-------------|-------|
| Line comment | `;` | `;` | `;` | `;` | Universal |
| Block comment | `(comment ...)` | -- | `#\| ... \|#` | `#\| ... \|#` | Clojure `comment` returns nil |
| Form comment | `#_ form` | -- | -- | `#; datum` | Discard next form |
| Quote | `'x` | `'x` | `'x` | `'x` | Universal |
| Quasiquote | `` `(~x ~@xs)`` | `` `(,x ,@xs)`` | `` `(,x ,@xs)`` | `` `(,x ,@xs)`` | Clojure uses `~` not `,` |
| Macro | `(defmacro f [x] ...)` | `(defmacro f (x) ...)` | `(defmacro f (x) ...)` | `(define-syntax ...)` | Scheme: hygienic only |
| Docstring | `(defn f "doc" [x] ...)` | `(defun f (x) "doc" ...)` | `(defun f (x) "doc" ...)` | -- | Scheme has no docstrings |
| TCO | `(recur ...)` only | not guaranteed | not guaranteed | required by spec | Clojure: explicit recur |
| Multiple values | -- | `(cl-values ...)` | `(values 1 2)` | `(values 1 2)` | Clojure: return a vector |

### Structural Summary

| Property | Clojure | Emacs Lisp | Common Lisp | Scheme R7RS |
|----------|---------|------------|-------------|-------------|
| Function namespace | Lisp-1 | Lisp-1 | Lisp-2 | Lisp-1 |
| Immutable by default | yes | no | no | no |
| Nil punning | partial | full | full | none |
| Let binding syntax | `[x 1 y 2]` | `((x 1) (y 2))` | `((x 1) (y 2))` | `((x 1) (y 2))` |
| Param list syntax | `[x y]` | `(x y)` | `(x y)` | `(x y)` |
| Macro hygiene | manual | manual | manual | automatic |
