package hydra.lisp.language

import hydra.coders.*

import hydra.core.*

import hydra.variants.*

lazy val lispLanguage: hydra.coders.Language = {
  lazy val eliminationVariants: scala.collection.immutable.Set[hydra.variants.EliminationVariant] = hydra.lib.sets.fromList[hydra.variants.EliminationVariant](Seq(hydra.variants.EliminationVariant.record,
     hydra.variants.EliminationVariant.union, hydra.variants.EliminationVariant.wrap))
  lazy val literalVariants: scala.collection.immutable.Set[hydra.variants.LiteralVariant] = hydra.lib.sets.fromList[hydra.variants.LiteralVariant](Seq(hydra.variants.LiteralVariant.binary,
     hydra.variants.LiteralVariant.boolean, hydra.variants.LiteralVariant.float, hydra.variants.LiteralVariant.integer,
     hydra.variants.LiteralVariant.string))
  lazy val floatTypes: scala.collection.immutable.Set[hydra.core.FloatType] = hydra.lib.sets.fromList[hydra.core.FloatType](Seq(hydra.core.FloatType.bigfloat,
     hydra.core.FloatType.float64))
  lazy val functionVariants: scala.collection.immutable.Set[hydra.variants.FunctionVariant] = hydra.lib.sets.fromList[hydra.variants.FunctionVariant](Seq(hydra.variants.FunctionVariant.elimination,
     hydra.variants.FunctionVariant.lambda))
  lazy val integerTypes: scala.collection.immutable.Set[hydra.core.IntegerType] = hydra.lib.sets.fromList[hydra.core.IntegerType](Seq(hydra.core.IntegerType.bigint))
  lazy val termVariants: scala.collection.immutable.Set[hydra.variants.TermVariant] = hydra.lib.sets.fromList[hydra.variants.TermVariant](Seq(hydra.variants.TermVariant.annotated,
     hydra.variants.TermVariant.application, hydra.variants.TermVariant.either, hydra.variants.TermVariant.cases,
     hydra.variants.TermVariant.lambda, hydra.variants.TermVariant.project, hydra.variants.TermVariant.unwrap,
     hydra.variants.TermVariant.typeApplication, hydra.variants.TermVariant.typeLambda,
     hydra.variants.TermVariant.let, hydra.variants.TermVariant.list, hydra.variants.TermVariant.literal,
     hydra.variants.TermVariant.map, hydra.variants.TermVariant.maybe, hydra.variants.TermVariant.pair,
     hydra.variants.TermVariant.record, hydra.variants.TermVariant.set, hydra.variants.TermVariant.inject,
     hydra.variants.TermVariant.unit, hydra.variants.TermVariant.variable, hydra.variants.TermVariant.wrap))
  lazy val typeVariants: scala.collection.immutable.Set[hydra.variants.TypeVariant] = hydra.lib.sets.fromList[hydra.variants.TypeVariant](Seq(hydra.variants.TypeVariant.annotated,
     hydra.variants.TypeVariant.application, hydra.variants.TypeVariant.either, hydra.variants.TypeVariant.function,
     hydra.variants.TypeVariant.forall, hydra.variants.TypeVariant.list, hydra.variants.TypeVariant.literal,
     hydra.variants.TypeVariant.map, hydra.variants.TypeVariant.maybe, hydra.variants.TypeVariant.pair,
     hydra.variants.TypeVariant.record, hydra.variants.TypeVariant.set, hydra.variants.TypeVariant.union,
     hydra.variants.TypeVariant.unit, hydra.variants.TypeVariant.variable, hydra.variants.TypeVariant.void,
     hydra.variants.TypeVariant.wrap))
  def typePredicate[T0](_x: T0): Boolean = true
  hydra.coders.Language("hydra.lisp", hydra.coders.LanguageConstraints(eliminationVariants,
     literalVariants, floatTypes, functionVariants, integerTypes, termVariants, typeVariants,
     typePredicate))
}

lazy val lispReservedWords: scala.collection.immutable.Set[scala.Predef.String] = {
  lazy val clojureKeywords: Seq[scala.Predef.String] = Seq("def", "defn", "defn-",
     "defmacro", "defrecord", "deftype", "defprotocol", "defmulti", "defmethod", "fn",
     "let", "loop", "recur", "if", "do", "cond", "case", "when", "when-not", "when-let",
     "if-let", "and", "or", "not", "nil", "true", "false", "throw", "try", "catch",
     "finally", "quote", "var", "ns", "require", "import", "use", "in-ns", "refer",
     "new", "set!", "monitor-enter", "monitor-exit")
  lazy val emacsLispKeywords: Seq[scala.Predef.String] = Seq("defun", "defvar", "defconst",
     "defmacro", "defsubst", "defadvice", "defcustom", "defgroup", "lambda", "let",
     "let*", "if", "cond", "progn", "prog1", "prog2", "while", "dolist", "dotimes",
     "and", "or", "not", "nil", "t", "quote", "function", "setq", "setq-default",
     "require", "provide", "condition-case", "unwind-protect", "save-excursion", "save-restriction",
     "catch", "throw", "interactive", "cl-defstruct", "cl-case", "cl-loop", "cl-labels",
     "cl-flet", "pcase", "pcase-let", "seq-let")
  lazy val commonLispKeywords: Seq[scala.Predef.String] = Seq("defun", "defvar", "defparameter",
     "defconstant", "defmacro", "defgeneric", "defmethod", "defclass", "defstruct",
     "deftype", "defpackage", "defsetf", "lambda", "let", "let*", "flet", "labels",
     "macrolet", "symbol-macrolet", "if", "cond", "case", "typecase", "etypecase",
     "ecase", "progn", "prog1", "prog2", "block", "return-from", "tagbody", "go",
     "and", "or", "not", "nil", "t", "quote", "function", "setq", "setf", "do", "do*",
     "dolist", "dotimes", "loop", "values", "multiple-value-bind", "multiple-value-list",
     "the", "declare", "declaim", "proclaim", "in-package", "use-package", "export",
     "import", "intern", "handler-case", "handler-bind", "restart-case", "condition",
     "unwind-protect", "catch", "throw", "eval-when", "load-time-value", "locally",
     "the", "pi")
  lazy val schemeKeywords: Seq[scala.Predef.String] = Seq("define", "define-syntax",
     "define-record-type", "define-library", "define-values", "lambda", "let", "let*",
     "letrec", "letrec*", "let-values", "let*-values", "if", "cond", "case", "when",
     "unless", "and", "or", "not", "begin", "do", "quote", "quasiquote", "unquote",
     "unquote-splicing", "set!", "import", "export", "include", "include-ci", "syntax-rules",
     "syntax-case", "with-syntax", "call-with-current-continuation", "call/cc", "call-with-values",
     "values", "dynamic-wind", "guard", "parameterize", "else")
  lazy val hydraLispKeywords: Seq[scala.Predef.String] = Seq("Node")
  hydra.lib.sets.fromList[scala.Predef.String](hydra.lib.lists.concat[scala.Predef.String](Seq(clojureKeywords,
     emacsLispKeywords, commonLispKeywords, schemeKeywords, hydraLispKeywords)))
}
