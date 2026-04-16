// Note: this is an automatically generated file. Do not edit.

package hydra.lisp;

/**
 * Lisp serializer: converts Lisp AST to concrete syntax for Clojure, Emacs Lisp, Common Lisp, or Scheme
 */
public interface Serde {
  static hydra.ast.Expr andExpressionToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.AndExpression andExpr) {
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
      java.util.Arrays.asList(hydra.Serialization.cst("and")),
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.lisp.Serde.expressionToExpr(
          d,
          v1)),
        (andExpr).expressions))));
  }

  static hydra.ast.Expr applicationToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.Application app) {
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> args = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.lisp.Serde.expressionToExpr(
        d,
        v1)),
      (app).arguments));
    hydra.lisp.syntax.Expression funExpr = (app).function;
    hydra.ast.Expr fun = hydra.lisp.Serde.expressionToExpr(
      d,
      funExpr);
    Boolean needsFuncall = (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.lisp.syntax.Dialect instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.lisp.syntax.Dialect.EmacsLisp u) {
        return (funExpr).accept(new hydra.lisp.syntax.Expression.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.lisp.syntax.Expression instance) {
            return true;
          }

          @Override
          public Boolean visit(hydra.lisp.syntax.Expression.Variable s) {
            return false;
          }
        });
      }
    });
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> allParts = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      needsFuncall,
      () -> hydra.lib.lists.Concat2.apply(
        java.util.Arrays.asList(
          hydra.Serialization.cst("funcall"),
          fun),
        args.get()),
      () -> hydra.lib.lists.Concat2.apply(
        java.util.Arrays.asList(fun),
        args.get())));
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(allParts.get()));
  }

  static hydra.ast.Expr caseExpressionToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.CaseExpression caseExpr) {
    java.util.List<hydra.lisp.syntax.CaseClause> clauses = (caseExpr).clauses;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> clauseExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.lisp.syntax.CaseClause, hydra.ast.Expr>) (c -> hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
        hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.lisp.Serde.expressionToExpr(
            d,
            v1)),
          (c).keys))),
        hydra.lisp.Serde.expressionToExpr(
          d,
          (c).body))))),
      clauses));
    hydra.util.Maybe<hydra.lisp.syntax.Expression> dflt = (caseExpr).default_;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> defaultPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (java.util.List<hydra.ast.Expr>) (java.util.Collections.<hydra.ast.Expr>emptyList()),
      (java.util.function.Function<hydra.lisp.syntax.Expression, java.util.List<hydra.ast.Expr>>) (e -> java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
        hydra.Serialization.cst("else"),
        hydra.lisp.Serde.expressionToExpr(
          d,
          e)))))),
      dflt));
    hydra.ast.Expr scrutinee = hydra.lisp.Serde.expressionToExpr(
      d,
      (caseExpr).scrutinee);
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
      java.util.Arrays.asList(
        hydra.Serialization.cst("case"),
        scrutinee),
      clauseExprs.get(),
      defaultPart.get()))));
  }

  static hydra.ast.Expr commentToExpr(hydra.lisp.syntax.Comment c) {
    String text = (c).text;
    return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
      "; ",
      text));
  }

  static hydra.ast.Expr condExpressionToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.CondExpression condExpr) {
    java.util.List<hydra.lisp.syntax.CondClause> clauses = (condExpr).clauses;
    hydra.util.Maybe<hydra.lisp.syntax.Expression> dflt = (condExpr).default_;
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        hydra.util.Lazy<java.util.List<hydra.ast.Expr>> clauseExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.lisp.syntax.CondClause, java.util.List<hydra.ast.Expr>>) (c -> java.util.Arrays.asList(
            hydra.lisp.Serde.expressionToExpr(
              d,
              (c).condition),
            hydra.lisp.Serde.expressionToExpr(
              d,
              (c).body))),
          clauses)));
        hydra.util.Lazy<java.util.List<hydra.ast.Expr>> defaultPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
          () -> (java.util.List<hydra.ast.Expr>) (java.util.Collections.<hydra.ast.Expr>emptyList()),
          (java.util.function.Function<hydra.lisp.syntax.Expression, java.util.List<hydra.ast.Expr>>) (e -> java.util.Arrays.asList(
            hydra.Serialization.cst(":else"),
            hydra.lisp.Serde.expressionToExpr(
              d,
              e))),
          dflt));
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(hydra.Serialization.cst("cond")),
          clauseExprs.get(),
          defaultPart.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        hydra.util.Lazy<java.util.List<hydra.ast.Expr>> clauseExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.lisp.syntax.CondClause, hydra.ast.Expr>) (c -> hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.lisp.Serde.expressionToExpr(
              d,
              (c).condition),
            hydra.lisp.Serde.expressionToExpr(
              d,
              (c).body))))),
          clauses));
        hydra.util.Lazy<java.util.List<hydra.ast.Expr>> defaultPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
          () -> (java.util.List<hydra.ast.Expr>) (java.util.Collections.<hydra.ast.Expr>emptyList()),
          (java.util.function.Function<hydra.lisp.syntax.Expression, java.util.List<hydra.ast.Expr>>) (e -> java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.Serialization.cst("t"),
            hydra.lisp.Serde.expressionToExpr(
              d,
              e)))))),
          dflt));
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(hydra.Serialization.cst("cond")),
          clauseExprs.get(),
          defaultPart.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        hydra.util.Lazy<java.util.List<hydra.ast.Expr>> clauseExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.lisp.syntax.CondClause, hydra.ast.Expr>) (c -> hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.lisp.Serde.expressionToExpr(
              d,
              (c).condition),
            hydra.lisp.Serde.expressionToExpr(
              d,
              (c).body))))),
          clauses));
        hydra.util.Lazy<java.util.List<hydra.ast.Expr>> defaultPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
          () -> (java.util.List<hydra.ast.Expr>) (java.util.Collections.<hydra.ast.Expr>emptyList()),
          (java.util.function.Function<hydra.lisp.syntax.Expression, java.util.List<hydra.ast.Expr>>) (e -> java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.Serialization.cst("t"),
            hydra.lisp.Serde.expressionToExpr(
              d,
              e)))))),
          dflt));
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(hydra.Serialization.cst("cond")),
          clauseExprs.get(),
          defaultPart.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        hydra.util.Lazy<java.util.List<hydra.ast.Expr>> clauseExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.lisp.syntax.CondClause, hydra.ast.Expr>) (c -> hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.lisp.Serde.expressionToExpr(
              d,
              (c).condition),
            hydra.lisp.Serde.expressionToExpr(
              d,
              (c).body))))),
          clauses));
        hydra.util.Lazy<java.util.List<hydra.ast.Expr>> defaultPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
          () -> (java.util.List<hydra.ast.Expr>) (java.util.Collections.<hydra.ast.Expr>emptyList()),
          (java.util.function.Function<hydra.lisp.syntax.Expression, java.util.List<hydra.ast.Expr>>) (e -> java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.Serialization.cst("else"),
            hydra.lisp.Serde.expressionToExpr(
              d,
              e)))))),
          dflt));
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(hydra.Serialization.cst("cond")),
          clauseExprs.get(),
          defaultPart.get()))));
      }
    });
  }

  static hydra.ast.Expr constantDefinitionToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.ConstantDefinition cdef) {
    hydra.ast.Expr name = hydra.lisp.Serde.symbolToExpr((cdef).name);
    hydra.ast.Expr value = hydra.lisp.Serde.expressionToExpr(
      d,
      (cdef).value);
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst(hydra.lisp.Serde.defconstKeyword(d)),
      name,
      value)));
  }

  static String defKeyword(hydra.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public String visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return "def";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return "defvar";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return "cl:defvar";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return "define";
      }
    });
  }

  static String defconstKeyword(hydra.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public String visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return "def";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return "defconst";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return "cl:defconstant";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return "define";
      }
    });
  }

  static String defnKeyword(hydra.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public String visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return "defn";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return "defun";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return "cl:defun";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return "define";
      }
    });
  }

  static String defrecordKeyword(hydra.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public String visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return "defrecord";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return "cl-defstruct";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return "cl:defstruct";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return "define-record-type";
      }
    });
  }

  static hydra.ast.Expr doExpressionToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.DoExpression doExpr) {
    String kw = (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public String visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return "do";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return "progn";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return "progn";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return "begin";
      }
    });
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
      java.util.Arrays.asList(hydra.Serialization.cst(kw)),
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.lisp.Serde.expressionToExpr(
          d,
          v1)),
        (doExpr).expressions))));
  }

  static hydra.ast.Expr docstringToExpr(hydra.lisp.syntax.Docstring ds) {
    return hydra.Serialization.cst(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      ";; ",
      (ds).value)));
  }

  static hydra.ast.Expr exportDeclarationToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.ExportDeclaration edecl) {
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> syms = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      hydra.lisp.Serde::symbolToExpr,
      (edecl).symbols));
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.cst("");
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.ast.Expr, hydra.ast.Expr>) (s -> hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.Serialization.cst("provide"),
            hydra.Serialization.noSep(java.util.Arrays.asList(
              hydra.Serialization.cst("'"),
              s)))))),
          syms.get()));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
          java.util.Arrays.asList(hydra.Serialization.cst(":export")),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.ast.Expr, hydra.ast.Expr>) (s -> hydra.Serialization.noSep(java.util.Arrays.asList(
              hydra.Serialization.cst(":"),
              s))),
            syms.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
          java.util.Arrays.asList(hydra.Serialization.cst("export")),
          syms.get())));
      }
    });
  }

  static hydra.ast.Expr expressionToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.Expression expr) {
    return (expr).accept(new hydra.lisp.syntax.Expression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.Application a) {
        return hydra.lisp.Serde.applicationToExpr(
          d,
          (a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.Lambda l) {
        return hydra.lisp.Serde.lambdaToExpr(
          d,
          (l).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.Let l) {
        return hydra.lisp.Serde.letExpressionToExpr(
          d,
          (l).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.If i) {
        return hydra.lisp.Serde.ifExpressionToExpr(
          d,
          (i).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.Cond c) {
        return hydra.lisp.Serde.condExpressionToExpr(
          d,
          (c).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.Case c) {
        return hydra.lisp.Serde.caseExpressionToExpr(
          d,
          (c).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.And a) {
        return hydra.lisp.Serde.andExpressionToExpr(
          d,
          (a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.Or o) {
        return hydra.lisp.Serde.orExpressionToExpr(
          d,
          (o).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.Not n) {
        return hydra.lisp.Serde.notExpressionToExpr(
          d,
          (n).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.Do e) {
        return hydra.lisp.Serde.doExpressionToExpr(
          d,
          (e).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.Begin e) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
          java.util.Arrays.asList(hydra.Serialization.cst("begin")),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.lisp.Serde.expressionToExpr(
              d,
              v1)),
            (e).value.expressions))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.Variable v) {
        return hydra.lisp.Serde.variableReferenceToExpr(
          d,
          (v).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.Literal l) {
        return hydra.lisp.Serde.literalToExpr(
          d,
          (l).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.List l) {
        return hydra.lisp.Serde.listLiteralToExpr(
          d,
          (l).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.Vector v) {
        return hydra.lisp.Serde.vectorLiteralToExpr(
          d,
          (v).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.Map m) {
        return hydra.lisp.Serde.mapLiteralToExpr(
          d,
          (m).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.Set s) {
        return hydra.lisp.Serde.setLiteralToExpr(
          d,
          (s).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.Cons c) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("cons"),
          hydra.lisp.Serde.expressionToExpr(
            d,
            (c).value.head),
          hydra.lisp.Serde.expressionToExpr(
            d,
            (c).value.tail))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.DottedPair p) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.lisp.Serde.expressionToExpr(
            d,
            (p).value.car),
          hydra.Serialization.cst("."),
          hydra.lisp.Serde.expressionToExpr(
            d,
            (p).value.cdr))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.FieldAccess fa) {
        return hydra.lisp.Serde.fieldAccessToExpr(
          d,
          (fa).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.TypeAnnotation ta) {
        return hydra.lisp.Serde.expressionToExpr(
          d,
          (ta).value.expression);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.Quote q) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("'"),
          hydra.lisp.Serde.expressionToExpr(
            d,
            (q).value.body)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.Quasiquote q) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("`"),
          hydra.lisp.Serde.expressionToExpr(
            d,
            (q).value.body)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.Unquote u) {
        return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
            return hydra.Serialization.noSep(java.util.Arrays.asList(
              hydra.Serialization.cst("~"),
              hydra.lisp.Serde.expressionToExpr(
                d,
                (u).value.body)));
          }

          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
            return hydra.Serialization.noSep(java.util.Arrays.asList(
              hydra.Serialization.cst(","),
              hydra.lisp.Serde.expressionToExpr(
                d,
                (u).value.body)));
          }

          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
            return hydra.Serialization.noSep(java.util.Arrays.asList(
              hydra.Serialization.cst(","),
              hydra.lisp.Serde.expressionToExpr(
                d,
                (u).value.body)));
          }

          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
            return hydra.Serialization.noSep(java.util.Arrays.asList(
              hydra.Serialization.cst(","),
              hydra.lisp.Serde.expressionToExpr(
                d,
                (u).value.body)));
          }
        });
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.SplicingUnquote su) {
        return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
            return hydra.Serialization.noSep(java.util.Arrays.asList(
              hydra.Serialization.cst("~@"),
              hydra.lisp.Serde.expressionToExpr(
                d,
                (su).value.body)));
          }

          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
            return hydra.Serialization.noSep(java.util.Arrays.asList(
              hydra.Serialization.cst(",@"),
              hydra.lisp.Serde.expressionToExpr(
                d,
                (su).value.body)));
          }

          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
            return hydra.Serialization.noSep(java.util.Arrays.asList(
              hydra.Serialization.cst(",@"),
              hydra.lisp.Serde.expressionToExpr(
                d,
                (su).value.body)));
          }

          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
            return hydra.Serialization.noSep(java.util.Arrays.asList(
              hydra.Serialization.cst(",@"),
              hydra.lisp.Serde.expressionToExpr(
                d,
                (su).value.body)));
          }
        });
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Expression.SExpression s) {
        return hydra.lisp.Serde.sExpressionToExpr((s).value);
      }
    });
  }

  static hydra.ast.Expr falseExpr(hydra.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.cst("false");
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.cst("nil");
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.cst("cl:nil");
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.cst("#f");
      }
    });
  }

  static hydra.ast.Expr fieldAccessToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.FieldAccess fa) {
    hydra.ast.Expr field = hydra.lisp.Serde.symbolToExpr((fa).field);
    hydra.ast.Expr rtype = hydra.lisp.Serde.symbolToExpr((fa).recordType);
    hydra.ast.Expr target = hydra.lisp.Serde.expressionToExpr(
      d,
      (fa).target);
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.noSep(java.util.Arrays.asList(
            hydra.Serialization.cst(":"),
            field)),
          target)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.noSep(java.util.Arrays.asList(
            rtype,
            hydra.Serialization.cst("-"),
            field)),
          target)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.noSep(java.util.Arrays.asList(
            rtype,
            hydra.Serialization.cst("-"),
            field)),
          target)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.noSep(java.util.Arrays.asList(
            rtype,
            hydra.Serialization.cst("-"),
            field)),
          target)));
      }
    });
  }

  static String formatLispFloat(hydra.lisp.syntax.Dialect d, java.math.BigDecimal v) {
    String s = hydra.lib.literals.ShowBigfloat.apply(v);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        s,
        "NaN"),
      () -> (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
        @Override
        public String visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
          return "Double/NaN";
        }

        @Override
        public String visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
          return "+nan.0";
        }

        @Override
        public String visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
          return "+hydra-nan+";
        }

        @Override
        public String visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
          return "0.0e+NaN";
        }
      }),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          s,
          "Infinity"),
        () -> (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
          @Override
          public String visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
            return "Double/POSITIVE_INFINITY";
          }

          @Override
          public String visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
            return "+inf.0";
          }

          @Override
          public String visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
            return "+hydra-pos-inf+";
          }

          @Override
          public String visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
            return "1.0e+INF";
          }
        }),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            s,
            "-Infinity"),
          () -> (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
            @Override
            public String visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
              return "Double/NEGATIVE_INFINITY";
            }

            @Override
            public String visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
              return "-inf.0";
            }

            @Override
            public String visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
              return "+hydra-neg-inf+";
            }

            @Override
            public String visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
              return "-1.0e+INF";
            }
          }),
          () -> s)));
  }

  static hydra.ast.Expr functionDefinitionToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.FunctionDefinition fdef) {
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> body = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.lisp.Serde.expressionToExpr(
        d,
        v1)),
      (fdef).body));
    hydra.ast.Expr name = hydra.lisp.Serde.symbolToExpr((fdef).name);
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> params = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      hydra.lisp.Serde::symbolToExpr,
      (fdef).params));
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(
            hydra.Serialization.cst("defn"),
            name),
          java.util.Arrays.asList(hydra.Serialization.brackets(
            hydra.Serialization.squareBrackets(),
            hydra.Serialization.inlineStyle(),
            hydra.Serialization.spaceSep(params.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(
            hydra.Serialization.cst("defun"),
            name),
          java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.spaceSep(params.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(
            hydra.Serialization.cst("defun"),
            name),
          java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.spaceSep(params.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(hydra.Serialization.cst("define")),
          java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
            java.util.Arrays.asList(name),
            params.get())))),
          body.get()))));
      }
    });
  }

  static hydra.ast.Expr ifExpressionToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.IfExpression ifExpr) {
    hydra.ast.Expr cond = hydra.lisp.Serde.expressionToExpr(
      d,
      (ifExpr).condition);
    hydra.util.Maybe<hydra.lisp.syntax.Expression> else_ = (ifExpr).else_;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> elsePart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (java.util.List<hydra.ast.Expr>) (java.util.Collections.<hydra.ast.Expr>emptyList()),
      (java.util.function.Function<hydra.lisp.syntax.Expression, java.util.List<hydra.ast.Expr>>) (e -> java.util.Arrays.asList(hydra.lisp.Serde.expressionToExpr(
        d,
        e))),
      else_));
    hydra.ast.Expr then = hydra.lisp.Serde.expressionToExpr(
      d,
      (ifExpr).then);
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
      java.util.Arrays.asList(
        hydra.Serialization.cst("if"),
        cond,
        then),
      elsePart.get()))));
  }

  static hydra.ast.Expr importDeclarationToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.ImportDeclaration idecl) {
    String modName = (idecl).module.value;
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst(":require"),
          hydra.Serialization.brackets(
            hydra.Serialization.squareBrackets(),
            hydra.Serialization.inlineStyle(),
            hydra.Serialization.spaceSep(java.util.Arrays.asList(hydra.Serialization.cst(modName)))))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("require"),
          hydra.Serialization.noSep(java.util.Arrays.asList(
            hydra.Serialization.cst("'"),
            hydra.Serialization.cst(modName))))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst(":use"),
          hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
            ":",
            modName)))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("import"),
          hydra.Serialization.parens(hydra.Serialization.cst(modName)))));
      }
    });
  }

  static hydra.ast.Expr keywordToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.Keyword k) {
    String name = (k).name;
    hydra.util.Maybe<String> ns = (k).namespace;
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr otherwise(hydra.lisp.syntax.Dialect instance) {
        return hydra.Serialization.cst(hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.lib.strings.Cat2.apply(
            ":",
            name),
          (java.util.function.Function<String, String>) (n -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            n,
            "/:",
            name))),
          ns));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("'"),
          hydra.Serialization.cst(name)));
      }
    });
  }

  static String lambdaKeyword(hydra.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public String visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return "fn";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return "lambda";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return "cl:lambda";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return "lambda";
      }
    });
  }

  static hydra.ast.Expr lambdaToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.Lambda lam) {
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> body = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.lisp.Serde.expressionToExpr(
        d,
        v1)),
      (lam).body));
    String kw = hydra.lisp.Serde.lambdaKeyword(d);
    hydra.util.Maybe<hydra.lisp.syntax.Symbol> mname = (lam).name;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> params = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      hydra.lisp.Serde::symbolToExpr,
      (lam).params));
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
            java.util.Arrays.asList(hydra.Serialization.cst(kw)),
            java.util.Arrays.asList(hydra.Serialization.brackets(
              hydra.Serialization.squareBrackets(),
              hydra.Serialization.inlineStyle(),
              hydra.Serialization.spaceSep(params.get()))),
            body.get())))),
          (java.util.function.Function<hydra.lisp.syntax.Symbol, hydra.ast.Expr>) (sym -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
            java.util.Arrays.asList(
              hydra.Serialization.cst(kw),
              hydra.lisp.Serde.symbolToExpr(sym)),
            java.util.Arrays.asList(hydra.Serialization.brackets(
              hydra.Serialization.squareBrackets(),
              hydra.Serialization.inlineStyle(),
              hydra.Serialization.spaceSep(params.get()))),
            body.get()))))),
          mname);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(hydra.Serialization.cst(kw)),
          java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.spaceSep(params.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(hydra.Serialization.cst(kw)),
          java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.spaceSep(params.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(hydra.Serialization.cst(kw)),
          java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.spaceSep(params.get()))),
          body.get()))));
      }
    });
  }

  static hydra.ast.Expr letExpressionToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.LetExpression letExpr) {
    java.util.List<hydra.lisp.syntax.LetBinding> bindings = (letExpr).bindings;
    hydra.util.Lazy<java.util.List<hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>>> bindingPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.lisp.syntax.LetBinding, hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>>) (b -> (b).accept(new hydra.lisp.syntax.LetBinding.PartialVisitor<>() {
        @Override
        public hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr> visit(hydra.lisp.syntax.LetBinding.Simple sb) {
          return (hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>) ((hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>) (new hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>(hydra.lisp.Serde.symbolToExpr((sb).value.name), hydra.lisp.Serde.expressionToExpr(
            d,
            (sb).value.value))));
        }

        @Override
        public hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr> visit(hydra.lisp.syntax.LetBinding.Destructuring ignored) {
          return (hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>) ((hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>) (new hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>(hydra.Serialization.cst("<destructuring>"), hydra.Serialization.cst("<destructuring>"))));
        }
      })),
      bindings));
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> body = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.lisp.Serde.expressionToExpr(
        d,
        v1)),
      (letExpr).body));
    hydra.lisp.syntax.LetKind kind = (letExpr).kind;
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return (kind).accept(new hydra.lisp.syntax.LetKind.PartialVisitor<>() {
          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.LetKind.Recursive _2) {
            return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              java.util.Arrays.asList(hydra.Serialization.cst("let")),
              java.util.Arrays.asList(hydra.Serialization.brackets(
                hydra.Serialization.squareBrackets(),
                hydra.Serialization.inlineStyle(),
                hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>, java.util.List<hydra.ast.Expr>>) (p -> java.util.Arrays.asList(
                    hydra.lib.pairs.First.apply(p),
                    hydra.lib.pairs.Second.apply(p))),
                  bindingPairs.get()))))),
              body.get()))));
          }

          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.LetKind.Parallel _2) {
            return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              java.util.Arrays.asList(hydra.Serialization.cst("let")),
              java.util.Arrays.asList(hydra.Serialization.brackets(
                hydra.Serialization.squareBrackets(),
                hydra.Serialization.inlineStyle(),
                hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>, java.util.List<hydra.ast.Expr>>) (p -> java.util.Arrays.asList(
                    hydra.lib.pairs.First.apply(p),
                    hydra.lib.pairs.Second.apply(p))),
                  bindingPairs.get()))))),
              body.get()))));
          }

          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.LetKind.Sequential _2) {
            return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              java.util.Arrays.asList(hydra.Serialization.cst("let")),
              java.util.Arrays.asList(hydra.Serialization.brackets(
                hydra.Serialization.squareBrackets(),
                hydra.Serialization.inlineStyle(),
                hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>, java.util.List<hydra.ast.Expr>>) (p -> java.util.Arrays.asList(
                    hydra.lib.pairs.First.apply(p),
                    hydra.lib.pairs.Second.apply(p))),
                  bindingPairs.get()))))),
              body.get()))));
          }
        });
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        hydra.util.Lazy<java.util.List<hydra.ast.Expr>> bindingExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>, hydra.ast.Expr>) (p -> hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.lib.pairs.First.apply(p),
            hydra.lib.pairs.Second.apply(p))))),
          bindingPairs.get()));
        String kw = (kind).accept(new hydra.lisp.syntax.LetKind.PartialVisitor<>() {
          @Override
          public String visit(hydra.lisp.syntax.LetKind.Parallel _2) {
            return "let";
          }

          @Override
          public String visit(hydra.lisp.syntax.LetKind.Sequential _2) {
            return "let*";
          }

          @Override
          public String visit(hydra.lisp.syntax.LetKind.Recursive _2) {
            return "letrec";
          }
        });
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(hydra.Serialization.cst(kw)),
          java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.spaceSep(bindingExprs.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        hydra.util.Lazy<java.util.List<hydra.ast.Expr>> bindingExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>, hydra.ast.Expr>) (p -> hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.lib.pairs.First.apply(p),
            hydra.lib.pairs.Second.apply(p))))),
          bindingPairs.get()));
        String kw = (kind).accept(new hydra.lisp.syntax.LetKind.PartialVisitor<>() {
          @Override
          public String visit(hydra.lisp.syntax.LetKind.Parallel _2) {
            return "let";
          }

          @Override
          public String visit(hydra.lisp.syntax.LetKind.Sequential _2) {
            return "let*";
          }

          @Override
          public String visit(hydra.lisp.syntax.LetKind.Recursive _2) {
            return "letrec";
          }
        });
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(hydra.Serialization.cst(kw)),
          java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.spaceSep(bindingExprs.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        hydra.util.Lazy<java.util.List<hydra.ast.Expr>> bindingExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>, hydra.ast.Expr>) (p -> hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.lib.pairs.First.apply(p),
            hydra.lib.pairs.Second.apply(p))))),
          bindingPairs.get()));
        String kw = (kind).accept(new hydra.lisp.syntax.LetKind.PartialVisitor<>() {
          @Override
          public String visit(hydra.lisp.syntax.LetKind.Parallel _2) {
            return "let";
          }

          @Override
          public String visit(hydra.lisp.syntax.LetKind.Sequential _2) {
            return "let*";
          }

          @Override
          public String visit(hydra.lisp.syntax.LetKind.Recursive _2) {
            return "letrec";
          }
        });
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(hydra.Serialization.cst(kw)),
          java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.spaceSep(bindingExprs.get()))),
          body.get()))));
      }
    });
  }

  static String listKeyword(hydra.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public String visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return "list";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return "list";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return "cl:list";
      }

      @Override
      public String visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return "list";
      }
    });
  }

  static hydra.ast.Expr listLiteralToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.ListLiteral ll) {
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> elems = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.lisp.Serde.expressionToExpr(
        d,
        v1)),
      (ll).elements));
    Boolean quoted = (ll).quoted;
    return hydra.lib.logic.IfElse.lazy(
      quoted,
      () -> hydra.Serialization.noSep(java.util.Arrays.asList(
        hydra.Serialization.cst("'"),
        hydra.Serialization.parens(hydra.Serialization.spaceSep(elems.get())))),
      () -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
        java.util.Arrays.asList(hydra.Serialization.cst(hydra.lisp.Serde.listKeyword(d))),
        elems.get()))));
  }

  static hydra.ast.Expr literalToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.Literal lit) {
    return (lit).accept(new hydra.lisp.syntax.Literal.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Literal.Integer_ i) {
        return hydra.Serialization.cst(hydra.lib.literals.ShowBigint.apply((i).value.value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Literal.Float_ f) {
        return hydra.Serialization.cst(hydra.lisp.Serde.formatLispFloat(
          d,
          (f).value.value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Literal.String_ s) {
        String e1 = hydra.lib.strings.Intercalate.apply(
          "\\\\",
          hydra.lib.strings.SplitOn.apply(
            "\\",
            (s).value));
        return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
            String escaped = hydra.lib.strings.Intercalate.apply(
              "\\\"",
              hydra.lib.strings.SplitOn.apply(
                "\"",
                e1));
            return hydra.Serialization.cst(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "\"",
              escaped,
              "\"")));
          }

          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
            String e2 = hydra.lib.strings.Intercalate.apply(
              "\\n",
              hydra.lib.strings.SplitOn.apply(
                hydra.lib.strings.FromList.apply(java.util.Arrays.asList(10)),
                e1));
            String e3 = hydra.lib.strings.Intercalate.apply(
              "\\r",
              hydra.lib.strings.SplitOn.apply(
                hydra.lib.strings.FromList.apply(java.util.Arrays.asList(13)),
                e2));
            String e4 = hydra.lib.strings.Intercalate.apply(
              "\\t",
              hydra.lib.strings.SplitOn.apply(
                hydra.lib.strings.FromList.apply(java.util.Arrays.asList(9)),
                e3));
            String escaped = hydra.lib.strings.Intercalate.apply(
              "\\\"",
              hydra.lib.strings.SplitOn.apply(
                "\"",
                e4));
            return hydra.Serialization.cst(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "\"",
              escaped,
              "\"")));
          }

          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
            String e2 = hydra.lib.strings.Intercalate.apply(
              "\\n",
              hydra.lib.strings.SplitOn.apply(
                hydra.lib.strings.FromList.apply(java.util.Arrays.asList(10)),
                e1));
            String e3 = hydra.lib.strings.Intercalate.apply(
              "\\r",
              hydra.lib.strings.SplitOn.apply(
                hydra.lib.strings.FromList.apply(java.util.Arrays.asList(13)),
                e2));
            String e4 = hydra.lib.strings.Intercalate.apply(
              "\\t",
              hydra.lib.strings.SplitOn.apply(
                hydra.lib.strings.FromList.apply(java.util.Arrays.asList(9)),
                e3));
            String escaped = hydra.lib.strings.Intercalate.apply(
              "\\\"",
              hydra.lib.strings.SplitOn.apply(
                "\"",
                e4));
            return hydra.Serialization.cst(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "\"",
              escaped,
              "\"")));
          }

          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
            String e2 = hydra.lib.strings.Intercalate.apply(
              "\\n",
              hydra.lib.strings.SplitOn.apply(
                hydra.lib.strings.FromList.apply(java.util.Arrays.asList(10)),
                e1));
            String e3 = hydra.lib.strings.Intercalate.apply(
              "\\r",
              hydra.lib.strings.SplitOn.apply(
                hydra.lib.strings.FromList.apply(java.util.Arrays.asList(13)),
                e2));
            String e4 = hydra.lib.strings.Intercalate.apply(
              "\\t",
              hydra.lib.strings.SplitOn.apply(
                hydra.lib.strings.FromList.apply(java.util.Arrays.asList(9)),
                e3));
            String escaped = hydra.lib.strings.Intercalate.apply(
              "\\\"",
              hydra.lib.strings.SplitOn.apply(
                "\"",
                e4));
            return hydra.Serialization.cst(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "\"",
              escaped,
              "\"")));
          }
        });
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Literal.Character_ c) {
        String ch = (c).value.value;
        return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
            return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
              "\\",
              ch));
          }

          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
            return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
              "?",
              ch));
          }

          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
            return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
              "#\\",
              ch));
          }

          @Override
          public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
            return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
              "#\\",
              ch));
          }
        });
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Literal.Boolean_ b) {
        return hydra.lib.logic.IfElse.lazy(
          (b).value,
          () -> hydra.lisp.Serde.trueExpr(d),
          () -> hydra.lisp.Serde.falseExpr(d));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Literal.Nil ignored) {
        return hydra.lisp.Serde.nilExpr(d);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Literal.Keyword k) {
        return hydra.lisp.Serde.keywordToExpr(
          d,
          (k).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Literal.Symbol s) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("'"),
          hydra.lisp.Serde.symbolToExpr((s).value)));
      }
    });
  }

  static hydra.ast.Expr macroDefinitionToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.MacroDefinition mdef) {
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> body = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.lisp.Serde.expressionToExpr(
        d,
        v1)),
      (mdef).body));
    hydra.ast.Expr name = hydra.lisp.Serde.symbolToExpr((mdef).name);
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> params = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      hydra.lisp.Serde::symbolToExpr,
      (mdef).params));
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(
            hydra.Serialization.cst("defmacro"),
            name),
          java.util.Arrays.asList(hydra.Serialization.brackets(
            hydra.Serialization.squareBrackets(),
            hydra.Serialization.inlineStyle(),
            hydra.Serialization.spaceSep(params.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(
            hydra.Serialization.cst("defmacro"),
            name),
          java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.spaceSep(params.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(
            hydra.Serialization.cst("defmacro"),
            name),
          java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.spaceSep(params.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(
            hydra.Serialization.cst("define-syntax"),
            name),
          body.get()))));
      }
    });
  }

  static hydra.ast.Expr mapLiteralToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.MapLiteral ml) {
    java.util.List<hydra.lisp.syntax.MapEntry> entries = (ml).entries;
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.brackets(
          hydra.Serialization.curlyBraces(),
          hydra.Serialization.inlineStyle(),
          hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.lisp.syntax.MapEntry, java.util.List<hydra.ast.Expr>>) (e -> java.util.Arrays.asList(
              hydra.lisp.Serde.expressionToExpr(
                d,
                (e).key),
              hydra.lisp.Serde.expressionToExpr(
                d,
                (e).value))),
            entries))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("'"),
          hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.lisp.syntax.MapEntry, hydra.ast.Expr>) (e -> hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
              hydra.lisp.Serde.expressionToExpr(
                d,
                (e).key),
              hydra.Serialization.cst("."),
              hydra.lisp.Serde.expressionToExpr(
                d,
                (e).value))))),
            entries)))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("'"),
          hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.lisp.syntax.MapEntry, hydra.ast.Expr>) (e -> hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
              hydra.lisp.Serde.expressionToExpr(
                d,
                (e).key),
              hydra.Serialization.cst("."),
              hydra.lisp.Serde.expressionToExpr(
                d,
                (e).value))))),
            entries)))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
          java.util.Arrays.asList(hydra.Serialization.cst("list")),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.lisp.syntax.MapEntry, hydra.ast.Expr>) (e -> hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
              hydra.Serialization.cst("cons"),
              hydra.lisp.Serde.expressionToExpr(
                d,
                (e).key),
              hydra.lisp.Serde.expressionToExpr(
                d,
                (e).value))))),
            entries))));
      }
    });
  }

  static hydra.ast.Expr moduleDeclarationToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.ModuleDeclaration mdecl) {
    String name = (mdecl).name.value;
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("ns"),
          hydra.Serialization.cst(name))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.newlineSep(java.util.Arrays.asList(
          hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.Serialization.cst("require"),
            hydra.Serialization.noSep(java.util.Arrays.asList(
              hydra.Serialization.cst("'"),
              hydra.Serialization.cst("cl-lib")))))),
          hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.Serialization.cst("provide"),
            hydra.Serialization.noSep(java.util.Arrays.asList(
              hydra.Serialization.cst("'"),
              hydra.Serialization.cst(name))))))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.newlineSep(java.util.Arrays.asList(
          hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.Serialization.cst("defpackage"),
            hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
              ":",
              name))))),
          hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.Serialization.cst("in-package"),
            hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
              ":",
              name)))))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("define-library"),
          hydra.Serialization.parens(hydra.Serialization.cst(name)))));
      }
    });
  }

  static hydra.ast.Expr nilExpr(hydra.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.cst("nil");
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.cst("nil");
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.cst("cl:nil");
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.cst("'()");
      }
    });
  }

  static hydra.ast.Expr notExpressionToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.NotExpression notExpr) {
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst("not"),
      hydra.lisp.Serde.expressionToExpr(
        d,
        (notExpr).expression))));
  }

  static hydra.ast.Expr orExpressionToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.OrExpression orExpr) {
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
      java.util.Arrays.asList(hydra.Serialization.cst("or")),
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.lisp.Serde.expressionToExpr(
          d,
          v1)),
        (orExpr).expressions))));
  }

  static hydra.ast.Expr programToExpr(hydra.lisp.syntax.Program prog) {
    hydra.lisp.syntax.Dialect d = (prog).dialect;
    java.util.List<hydra.lisp.syntax.ExportDeclaration> exports = (prog).exports;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> exportSyms = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.lisp.syntax.ExportDeclaration, java.util.List<hydra.ast.Expr>>) (edecl -> hydra.lib.lists.Map.apply(
        hydra.lisp.Serde::symbolToExpr,
        (edecl).symbols)),
      exports)));
    java.util.List<hydra.lisp.syntax.TopLevelFormWithComments> forms = (prog).forms;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> formPart = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.lisp.syntax.TopLevelFormWithComments, hydra.ast.Expr>) (v1 -> hydra.lisp.Serde.topLevelFormWithCommentsToExpr(
        d,
        v1)),
      forms));
    java.util.List<hydra.lisp.syntax.ImportDeclaration> imports = (prog).imports;
    hydra.util.Lazy<java.util.List<String>> importNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.lisp.syntax.ImportDeclaration, String>) (idecl -> (idecl).module.value),
      imports));
    hydra.util.Maybe<hydra.lisp.syntax.ModuleDeclaration> modDecl = (prog).module;
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.Serialization.doubleNewlineSep(formPart.get()),
          (java.util.function.Function<hydra.lisp.syntax.ModuleDeclaration, hydra.ast.Expr>) (m -> {
            hydra.util.Lazy<java.util.List<hydra.ast.Expr>> varNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.lisp.syntax.TopLevelFormWithComments, java.util.List<hydra.ast.Expr>>) (fwc -> {
                hydra.lisp.syntax.TopLevelForm form = (fwc).form;
                return (form).accept(new hydra.lisp.syntax.TopLevelForm.PartialVisitor<>() {
                  @Override
                  public java.util.List<hydra.ast.Expr> otherwise(hydra.lisp.syntax.TopLevelForm instance) {
                    return (java.util.List<hydra.ast.Expr>) (java.util.Collections.<hydra.ast.Expr>emptyList());
                  }

                  @Override
                  public java.util.List<hydra.ast.Expr> visit(hydra.lisp.syntax.TopLevelForm.Variable vd) {
                    return java.util.Arrays.asList(hydra.lisp.Serde.symbolToExpr((vd).value.name));
                  }

                  @Override
                  public java.util.List<hydra.ast.Expr> visit(hydra.lisp.syntax.TopLevelForm.Function fd) {
                    return java.util.Arrays.asList(hydra.lisp.Serde.symbolToExpr((fd).value.name));
                  }
                });
              }),
              forms)));
            hydra.util.Lazy<java.util.List<hydra.ast.Expr>> declareForm = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.lists.Null.apply(varNames.get()),
              () -> (java.util.List<hydra.ast.Expr>) (java.util.Collections.<hydra.ast.Expr>emptyList()),
              () -> java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
                java.util.Arrays.asList(hydra.Serialization.cst("declare")),
                varNames.get()))))));
            String nameStr = (m).name.value;
            hydra.util.Lazy<java.util.List<hydra.ast.Expr>> requireClauses = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<String, hydra.ast.Expr>) (imp -> hydra.Serialization.brackets(
                hydra.Serialization.squareBrackets(),
                hydra.Serialization.inlineStyle(),
                hydra.Serialization.spaceSep(java.util.Arrays.asList(
                  hydra.Serialization.cst(imp),
                  hydra.Serialization.cst(":refer"),
                  hydra.Serialization.cst(":all"))))),
              importNames.get()));
            hydra.util.Lazy<hydra.ast.Expr> nsForm = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.lists.Null.apply(requireClauses.get()),
              () -> hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
                hydra.Serialization.cst("ns"),
                hydra.Serialization.cst(nameStr)))),
              () -> hydra.Serialization.parens(hydra.Serialization.newlineSep(java.util.Arrays.asList(
                hydra.Serialization.spaceSep(java.util.Arrays.asList(
                  hydra.Serialization.cst("ns"),
                  hydra.Serialization.cst(nameStr))),
                hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
                  java.util.Arrays.asList(hydra.Serialization.cst("  (:require")),
                  requireClauses.get())),
                hydra.Serialization.cst(")"))))));
            return hydra.Serialization.doubleNewlineSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              java.util.Arrays.asList(nsForm.get()),
              declareForm.get(),
              formPart.get())));
          }),
          modDecl);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.Serialization.doubleNewlineSep(formPart.get()),
          (java.util.function.Function<hydra.lisp.syntax.ModuleDeclaration, hydra.ast.Expr>) (m -> {
            String nameStr = (m).name.value;
            hydra.ast.Expr provideForm = hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
              hydra.Serialization.cst("provide"),
              hydra.Serialization.noSep(java.util.Arrays.asList(
                hydra.Serialization.cst("'"),
                hydra.Serialization.cst(nameStr))))));
            hydra.ast.Expr requireClLib = hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
              hydra.Serialization.cst("require"),
              hydra.Serialization.noSep(java.util.Arrays.asList(
                hydra.Serialization.cst("'"),
                hydra.Serialization.cst("cl-lib"))))));
            hydra.util.Lazy<java.util.List<hydra.ast.Expr>> requireImports = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<String, hydra.ast.Expr>) (imp -> hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
                hydra.Serialization.cst("require"),
                hydra.Serialization.noSep(java.util.Arrays.asList(
                  hydra.Serialization.cst("'"),
                  hydra.Serialization.cst(imp))))))),
              importNames.get()));
            return hydra.Serialization.doubleNewlineSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              java.util.Arrays.asList(requireClLib),
              requireImports.get(),
              formPart.get(),
              java.util.Arrays.asList(provideForm))));
          }),
          modDecl);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.Serialization.doubleNewlineSep(formPart.get()),
          (java.util.function.Function<hydra.lisp.syntax.ModuleDeclaration, hydra.ast.Expr>) (m -> {
            String nameStr = (m).name.value;
            String colonName = hydra.lib.strings.Cat2.apply(
              ":",
              nameStr);
            hydra.util.Lazy<java.util.List<hydra.ast.Expr>> exportClause = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.lists.Null.apply(exportSyms.get()),
              () -> (java.util.List<hydra.ast.Expr>) (java.util.Collections.<hydra.ast.Expr>emptyList()),
              () -> java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
                java.util.Arrays.asList(hydra.Serialization.cst(":export")),
                hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.ast.Expr, hydra.ast.Expr>) (s -> hydra.Serialization.noSep(java.util.Arrays.asList(
                    hydra.Serialization.cst(":"),
                    s))),
                  exportSyms.get())))))));
            hydra.util.Lazy<hydra.ast.Expr> useClause = new hydra.util.Lazy<>(() -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
              java.util.Arrays.asList(
                hydra.Serialization.cst(":use"),
                hydra.Serialization.cst(":cl")),
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<String, hydra.ast.Expr>) (imp -> hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
                  ":",
                  imp))),
                importNames.get())))));
            hydra.util.Lazy<hydra.ast.Expr> defpkgForm = new hydra.util.Lazy<>(() -> hydra.Serialization.parens(hydra.Serialization.newlineSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              java.util.Arrays.asList(hydra.Serialization.spaceSep(java.util.Arrays.asList(
                hydra.Serialization.cst("defpackage"),
                hydra.Serialization.cst(colonName)))),
              java.util.Arrays.asList(useClause.get()),
              exportClause.get())))));
            hydra.ast.Expr inpkgForm = hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
              hydra.Serialization.cst("in-package"),
              hydra.Serialization.cst(colonName))));
            return hydra.Serialization.doubleNewlineSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              java.util.Arrays.asList(
                defpkgForm.get(),
                inpkgForm),
              formPart.get())));
          }),
          modDecl);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.Serialization.doubleNewlineSep(formPart.get()),
          (java.util.function.Function<hydra.lisp.syntax.ModuleDeclaration, hydra.ast.Expr>) (m -> {
            hydra.util.Lazy<java.util.List<hydra.ast.Expr>> domainImportExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.lisp.syntax.ImportDeclaration, hydra.ast.Expr>) (idecl -> {
                String nsName = (idecl).module.value;
                hydra.util.Lazy<java.util.List<String>> nsParts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                  (java.util.function.Function<String, String>) (p -> hydra.Formatting.convertCaseCamelToLowerSnake(p)),
                  hydra.lib.strings.SplitOn.apply(
                    ".",
                    nsName)));
                return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
                  (java.util.function.Function<String, hydra.ast.Expr>) (p -> hydra.Serialization.cst(p)),
                  nsParts.get())));
              }),
              imports));
            hydra.ast.Expr schemeBaseExpr = hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
              hydra.Serialization.cst("scheme"),
              hydra.Serialization.cst("base"))));
            hydra.util.Lazy<java.util.List<hydra.ast.Expr>> allImportExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
              java.util.Arrays.asList(schemeBaseExpr),
              domainImportExprs.get()));
            hydra.util.Lazy<hydra.ast.Expr> beginClause = new hydra.util.Lazy<>(() -> hydra.Serialization.parens(hydra.Serialization.newlineSep(hydra.lib.lists.Concat2.apply(
              java.util.Arrays.asList(hydra.Serialization.cst("begin")),
              formPart.get()))));
            hydra.util.Lazy<java.util.List<hydra.ast.Expr>> exportClauses = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.lisp.syntax.ExportDeclaration, hydra.ast.Expr>) (edecl -> hydra.lisp.Serde.exportDeclarationToExpr(
                d,
                edecl)),
              exports));
            hydra.util.Lazy<hydra.ast.Expr> importClause = new hydra.util.Lazy<>(() -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
              java.util.Arrays.asList(hydra.Serialization.cst("import")),
              allImportExprs.get()))));
            String nameStr = (m).name.value;
            hydra.util.Lazy<java.util.List<String>> nameParts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<String, String>) (p -> hydra.Formatting.convertCaseCamelToLowerSnake(p)),
              hydra.lib.strings.SplitOn.apply(
                ".",
                nameStr)));
            hydra.util.Lazy<hydra.ast.Expr> nameExpr = new hydra.util.Lazy<>(() -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
              (java.util.function.Function<String, hydra.ast.Expr>) (p -> hydra.Serialization.cst(p)),
              nameParts.get()))));
            return hydra.Serialization.parens(hydra.Serialization.newlineSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
              java.util.Arrays.asList(hydra.Serialization.spaceSep(java.util.Arrays.asList(
                hydra.Serialization.cst("define-library"),
                nameExpr.get()))),
              exportClauses.get(),
              java.util.Arrays.asList(importClause.get()),
              java.util.Arrays.asList(beginClause.get())))));
          }),
          modDecl);
      }
    });
  }

  static hydra.ast.Expr recordTypeDefinitionToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.RecordTypeDefinition rdef) {
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> fields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.lisp.syntax.FieldDefinition, hydra.ast.Expr>) (f -> hydra.lisp.Serde.symbolToExpr((f).name)),
      (rdef).fields));
    hydra.ast.Expr name = hydra.lisp.Serde.symbolToExpr((rdef).name);
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        hydra.ast.Expr defrecordForm = hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("defrecord"),
          name,
          hydra.Serialization.brackets(
            hydra.Serialization.squareBrackets(),
            hydra.Serialization.inlineStyle(),
            hydra.Serialization.spaceSep(fields.get())))));
        hydra.util.Lazy<java.util.List<String>> fieldNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.lisp.syntax.FieldDefinition, String>) (f -> (f).name.value),
          (rdef).fields));
        String nameStr = (rdef).name.value;
        hydra.util.Lazy<hydra.ast.Expr> makeAlias = new hydra.util.Lazy<>(() -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(
            hydra.Serialization.cst("defn"),
            hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
              "make-",
              nameStr))),
          java.util.Arrays.asList(hydra.Serialization.brackets(
            hydra.Serialization.squareBrackets(),
            hydra.Serialization.inlineStyle(),
            hydra.Serialization.spaceSep(fields.get()))),
          java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
            java.util.Arrays.asList(hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
              "->",
              nameStr))),
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<String, hydra.ast.Expr>) (fn -> hydra.Serialization.cst(fn)),
              fieldNames.get()))))))))));
        return hydra.Serialization.newlineSep(java.util.Arrays.asList(
          defrecordForm,
          makeAlias.get()));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(
            hydra.Serialization.cst("cl-defstruct"),
            name),
          fields.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(
            hydra.Serialization.cst("cl:defstruct"),
            name),
          fields.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        hydra.util.Lazy<java.util.List<String>> fieldNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.lisp.syntax.FieldDefinition, String>) (f -> (f).name.value),
          (rdef).fields));
        String nameStr = (rdef).name.value;
        hydra.util.Lazy<java.util.List<hydra.ast.Expr>> accessors = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<String, hydra.ast.Expr>) (fn -> hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.Serialization.cst(fn),
            hydra.Serialization.cst(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              nameStr,
              "-",
              fn))))))),
          fieldNames.get()));
        hydra.util.Lazy<hydra.ast.Expr> constructor = new hydra.util.Lazy<>(() -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
          java.util.Arrays.asList(hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
            "make-",
            nameStr))),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<String, hydra.ast.Expr>) (fn -> hydra.Serialization.cst(fn)),
            fieldNames.get())))));
        hydra.ast.Expr predicate = hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
          nameStr,
          "?"));
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(
            hydra.Serialization.cst("define-record-type"),
            name,
            constructor.get(),
            predicate),
          accessors.get()))));
      }
    });
  }

  static hydra.ast.Expr sExpressionToExpr(hydra.lisp.syntax.SExpression sexpr) {
    return (sexpr).accept(new hydra.lisp.syntax.SExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.SExpression.Atom a) {
        return hydra.Serialization.cst((a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.SExpression.List elems) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.lisp.Serde::sExpressionToExpr,
          (elems).value)));
      }
    });
  }

  static hydra.ast.Expr setLiteralToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.SetLiteral sl) {
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> elems = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.lisp.Serde.expressionToExpr(
        d,
        v1)),
      (sl).elements));
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("#"),
          hydra.Serialization.brackets(
            hydra.Serialization.curlyBraces(),
            hydra.Serialization.inlineStyle(),
            hydra.Serialization.spaceSep(elems.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
          java.util.Arrays.asList(hydra.Serialization.cst("list")),
          elems.get())));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
          java.util.Arrays.asList(hydra.Serialization.cst("cl:list")),
          elems.get())));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
          java.util.Arrays.asList(hydra.Serialization.cst("list")),
          elems.get())));
      }
    });
  }

  static hydra.ast.Expr symbolToExpr(hydra.lisp.syntax.Symbol s) {
    return hydra.Serialization.cst((s).value);
  }

  static hydra.ast.Expr topLevelFormToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.TopLevelForm form) {
    return (form).accept(new hydra.lisp.syntax.TopLevelForm.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.TopLevelForm.Function f) {
        return hydra.lisp.Serde.functionDefinitionToExpr(
          d,
          (f).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.TopLevelForm.Variable v) {
        return hydra.lisp.Serde.variableDefinitionToExpr(
          d,
          (v).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.TopLevelForm.Constant c) {
        return hydra.lisp.Serde.constantDefinitionToExpr(
          d,
          (c).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.TopLevelForm.RecordType r) {
        return hydra.lisp.Serde.recordTypeDefinitionToExpr(
          d,
          (r).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.TopLevelForm.Macro m) {
        return hydra.lisp.Serde.macroDefinitionToExpr(
          d,
          (m).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.TopLevelForm.Expression e) {
        return hydra.lisp.Serde.expressionToExpr(
          d,
          (e).value);
      }
    });
  }

  static hydra.ast.Expr topLevelFormWithCommentsToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.TopLevelFormWithComments fwc) {
    hydra.util.Maybe<hydra.lisp.syntax.Comment> mcomment = (fwc).comment;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> commentPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (java.util.List<hydra.ast.Expr>) (java.util.Collections.<hydra.ast.Expr>emptyList()),
      (java.util.function.Function<hydra.lisp.syntax.Comment, java.util.List<hydra.ast.Expr>>) (c -> java.util.Arrays.asList(hydra.lisp.Serde.commentToExpr(c))),
      mcomment));
    hydra.util.Maybe<hydra.lisp.syntax.Docstring> mdoc = (fwc).doc;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> docPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (java.util.List<hydra.ast.Expr>) (java.util.Collections.<hydra.ast.Expr>emptyList()),
      (java.util.function.Function<hydra.lisp.syntax.Docstring, java.util.List<hydra.ast.Expr>>) (ds -> java.util.Arrays.asList(hydra.lisp.Serde.docstringToExpr(ds))),
      mdoc));
    hydra.lisp.syntax.TopLevelForm form = (fwc).form;
    hydra.ast.Expr formExpr = hydra.lisp.Serde.topLevelFormToExpr(
      d,
      form);
    return hydra.Serialization.newlineSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
      commentPart.get(),
      docPart.get(),
      java.util.Arrays.asList(formExpr))));
  }

  static hydra.ast.Expr trueExpr(hydra.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.cst("true");
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.cst("t");
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.cst("cl:t");
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.cst("#t");
      }
    });
  }

  static hydra.ast.Expr variableDefinitionToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.VariableDefinition vdef) {
    hydra.ast.Expr name = hydra.lisp.Serde.symbolToExpr((vdef).name);
    hydra.ast.Expr value = hydra.lisp.Serde.expressionToExpr(
      d,
      (vdef).value);
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst(hydra.lisp.Serde.defKeyword(d)),
      name,
      value)));
  }

  static hydra.ast.Expr variableReferenceToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.VariableReference vref) {
    Boolean isFnNs = (vref).functionNamespace;
    hydra.ast.Expr name = hydra.lisp.Serde.symbolToExpr((vref).name);
    return hydra.lib.logic.IfElse.lazy(
      isFnNs,
      () -> (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
        @Override
        public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
          return hydra.Serialization.noSep(java.util.Arrays.asList(
            hydra.Serialization.cst("#'"),
            name));
        }

        @Override
        public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
          return name;
        }

        @Override
        public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
          return name;
        }

        @Override
        public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
          return name;
        }
      }),
      () -> name);
  }

  static hydra.ast.Expr vectorLiteralToExpr(hydra.lisp.syntax.Dialect d, hydra.lisp.syntax.VectorLiteral vl) {
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> elems = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.lisp.Serde.expressionToExpr(
        d,
        v1)),
      (vl).elements));
    return (d).accept(new hydra.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.brackets(
          hydra.Serialization.squareBrackets(),
          hydra.Serialization.inlineStyle(),
          hydra.Serialization.spaceSep(elems.get()));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.brackets(
          hydra.Serialization.squareBrackets(),
          hydra.Serialization.inlineStyle(),
          hydra.Serialization.spaceSep(elems.get()));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("#"),
          hydra.Serialization.parens(hydra.Serialization.spaceSep(elems.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("#"),
          hydra.Serialization.parens(hydra.Serialization.spaceSep(elems.get()))));
      }
    });
  }
}
