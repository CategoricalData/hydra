# Note: this is an automatically generated file. Do not edit.

r"""Language constraints and reserved words for Lisp (covering Clojure, Emacs Lisp, Common Lisp, and Scheme)."""

from __future__ import annotations
from functools import lru_cache
from typing import TypeVar, cast
import hydra.coders
import hydra.core
import hydra.lib.lists
import hydra.lib.sets
import hydra.variants

T0 = TypeVar("T0")

@lru_cache(1)
def lisp_language() -> hydra.coders.Language:
    r"""Language constraints for Lisp."""

    @lru_cache(1)
    def elimination_variants() -> frozenset[hydra.variants.EliminationVariant]:
        return hydra.lib.sets.from_list((hydra.variants.EliminationVariant.RECORD, hydra.variants.EliminationVariant.UNION, hydra.variants.EliminationVariant.WRAP))
    @lru_cache(1)
    def literal_variants() -> frozenset[hydra.variants.LiteralVariant]:
        return hydra.lib.sets.from_list((hydra.variants.LiteralVariant.BINARY, hydra.variants.LiteralVariant.BOOLEAN, hydra.variants.LiteralVariant.FLOAT, hydra.variants.LiteralVariant.INTEGER, hydra.variants.LiteralVariant.STRING))
    @lru_cache(1)
    def float_types() -> frozenset[hydra.core.FloatType]:
        return hydra.lib.sets.from_list((hydra.core.FloatType.BIGFLOAT, hydra.core.FloatType.FLOAT64))
    @lru_cache(1)
    def function_variants() -> frozenset[hydra.variants.FunctionVariant]:
        return hydra.lib.sets.from_list((hydra.variants.FunctionVariant.ELIMINATION, hydra.variants.FunctionVariant.LAMBDA))
    @lru_cache(1)
    def integer_types() -> frozenset[hydra.core.IntegerType]:
        return hydra.lib.sets.from_list((hydra.core.IntegerType.BIGINT,))
    @lru_cache(1)
    def term_variants() -> frozenset[hydra.variants.TermVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TermVariant.ANNOTATED, hydra.variants.TermVariant.APPLICATION, hydra.variants.TermVariant.EITHER, hydra.variants.TermVariant.FUNCTION, hydra.variants.TermVariant.LET, hydra.variants.TermVariant.LIST, hydra.variants.TermVariant.LITERAL, hydra.variants.TermVariant.MAP, hydra.variants.TermVariant.MAYBE, hydra.variants.TermVariant.PAIR, hydra.variants.TermVariant.RECORD, hydra.variants.TermVariant.SET, hydra.variants.TermVariant.UNION, hydra.variants.TermVariant.UNIT, hydra.variants.TermVariant.VARIABLE, hydra.variants.TermVariant.WRAP))
    @lru_cache(1)
    def type_variants() -> frozenset[hydra.variants.TypeVariant]:
        return hydra.lib.sets.from_list((hydra.variants.TypeVariant.ANNOTATED, hydra.variants.TypeVariant.APPLICATION, hydra.variants.TypeVariant.EITHER, hydra.variants.TypeVariant.FUNCTION, hydra.variants.TypeVariant.FORALL, hydra.variants.TypeVariant.LIST, hydra.variants.TypeVariant.LITERAL, hydra.variants.TypeVariant.MAP, hydra.variants.TypeVariant.MAYBE, hydra.variants.TypeVariant.PAIR, hydra.variants.TypeVariant.RECORD, hydra.variants.TypeVariant.SET, hydra.variants.TypeVariant.UNION, hydra.variants.TypeVariant.UNIT, hydra.variants.TypeVariant.VARIABLE, hydra.variants.TypeVariant.VOID, hydra.variants.TypeVariant.WRAP))
    def type_predicate(_: T0) -> bool:
        return True
    return hydra.coders.Language(hydra.coders.LanguageName("hydra.ext.lisp"), hydra.coders.LanguageConstraints(elimination_variants(), literal_variants(), float_types(), function_variants(), integer_types(), term_variants(), type_variants(), (lambda x1: type_predicate(x1))))

@lru_cache(1)
def lisp_reserved_words() -> frozenset[str]:
    r"""A set of reserved words across all four Lisp dialects."""

    # Clojure special forms and reserved symbols.
    clojure_keywords = ("def", "defn", "defn-", "defmacro", "defrecord", "deftype", "defprotocol", "defmulti", "defmethod", "fn", "let", "loop", "recur", "if", "do", "cond", "case", "when", "when-not", "when-let", "if-let", "and", "or", "not", "nil", "true", "false", "throw", "try", "catch", "finally", "quote", "var", "ns", "require", "import", "use", "in-ns", "refer", "new", "set!", "monitor-enter", "monitor-exit")
    # Emacs Lisp special forms and reserved symbols.
    emacs_lisp_keywords = ("defun", "defvar", "defconst", "defmacro", "defsubst", "defadvice", "defcustom", "defgroup", "lambda", "let", "let*", "if", "cond", "progn", "prog1", "prog2", "while", "dolist", "dotimes", "and", "or", "not", "nil", "t", "quote", "function", "setq", "setq-default", "require", "provide", "condition-case", "unwind-protect", "save-excursion", "save-restriction", "catch", "throw", "interactive", "cl-defstruct", "cl-case", "cl-loop", "cl-labels", "cl-flet", "pcase", "pcase-let", "seq-let")
    # Common Lisp special operators, macros, and reserved symbols.
    common_lisp_keywords = ("defun", "defvar", "defparameter", "defconstant", "defmacro", "defgeneric", "defmethod", "defclass", "defstruct", "deftype", "defpackage", "defsetf", "lambda", "let", "let*", "flet", "labels", "macrolet", "symbol-macrolet", "if", "cond", "case", "typecase", "etypecase", "ecase", "progn", "prog1", "prog2", "block", "return-from", "tagbody", "go", "and", "or", "not", "nil", "t", "quote", "function", "setq", "setf", "do", "do*", "dolist", "dotimes", "loop", "values", "multiple-value-bind", "multiple-value-list", "the", "declare", "declaim", "proclaim", "in-package", "use-package", "export", "import", "intern", "handler-case", "handler-bind", "restart-case", "condition", "unwind-protect", "catch", "throw", "eval-when", "load-time-value", "locally", "the")
    # Scheme R7RS keywords and special forms.
    scheme_keywords = ("define", "define-syntax", "define-record-type", "define-library", "define-values", "lambda", "let", "let*", "letrec", "letrec*", "let-values", "let*-values", "if", "cond", "case", "when", "unless", "and", "or", "not", "begin", "do", "quote", "quasiquote", "unquote", "unquote-splicing", "set!", "import", "export", "include", "include-ci", "syntax-rules", "syntax-case", "with-syntax", "call-with-current-continuation", "call/cc", "call-with-values", "values", "dynamic-wind", "guard", "parameterize", "else")
    # Reserved words specific to Hydra-Lisp code generation.
    hydra_lisp_keywords = ("Node",)
    return hydra.lib.sets.from_list(hydra.lib.lists.concat((clojure_keywords, emacs_lisp_keywords, common_lisp_keywords, scheme_keywords, hydra_lisp_keywords)))
