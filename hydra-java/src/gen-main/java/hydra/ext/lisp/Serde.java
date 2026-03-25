// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp;

/**
 * Lisp serializer: converts Lisp AST to concrete syntax for Clojure, Emacs Lisp, Common Lisp, or Scheme
 */
public interface Serde {
  static hydra.ast.Expr andExpressionToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.AndExpression andExpr) {
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
      hydra.util.ConsList.of(hydra.Serialization.cst("and")),
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.ext.lisp.Serde.expressionToExpr(
          d,
          v1)),
        (andExpr).expressions))));
  }

  static hydra.ast.Expr applicationToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.Application app) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> args = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.ext.lisp.Serde.expressionToExpr(
        d,
        v1)),
      (app).arguments));
    hydra.ext.lisp.syntax.Expression funExpr = (app).function;
    hydra.ast.Expr fun = hydra.ext.lisp.Serde.expressionToExpr(
      d,
      funExpr);
    Boolean needsFuncall = (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.ext.lisp.syntax.Dialect instance) {
        return false;
      }

      @Override
      public Boolean visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp u) {
        return (funExpr).accept(new hydra.ext.lisp.syntax.Expression.PartialVisitor<>() {
          @Override
          public Boolean otherwise(hydra.ext.lisp.syntax.Expression instance) {
            return true;
          }

          @Override
          public Boolean visit(hydra.ext.lisp.syntax.Expression.Variable s) {
            return false;
          }
        });
      }
    });
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> allParts = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      needsFuncall,
      () -> hydra.lib.lists.Concat2.apply(
        hydra.util.ConsList.of(
          hydra.Serialization.cst("funcall"),
          fun),
        args.get()),
      () -> hydra.lib.lists.Concat2.apply(
        hydra.util.ConsList.of(fun),
        args.get())));
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(allParts.get()));
  }

  static hydra.ast.Expr caseExpressionToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.CaseExpression caseExpr) {
    hydra.util.ConsList<hydra.ext.lisp.syntax.CaseClause> clauses = (caseExpr).clauses;
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> clauseExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.lisp.syntax.CaseClause, hydra.ast.Expr>) (c -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
        hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.ext.lisp.Serde.expressionToExpr(
            d,
            v1)),
          (c).keys))),
        hydra.ext.lisp.Serde.expressionToExpr(
          d,
          (c).body))))),
      clauses));
    hydra.util.Maybe<hydra.ext.lisp.syntax.Expression> dflt = (caseExpr).default_;
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> defaultPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (hydra.util.ConsList<hydra.ast.Expr>) (hydra.util.ConsList.<hydra.ast.Expr>empty()),
      (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.util.ConsList<hydra.ast.Expr>>) (e -> hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
        hydra.Serialization.cst("else"),
        hydra.ext.lisp.Serde.expressionToExpr(
          d,
          e)))))),
      dflt));
    hydra.ast.Expr scrutinee = hydra.ext.lisp.Serde.expressionToExpr(
      d,
      (caseExpr).scrutinee);
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
      hydra.util.ConsList.of(
        hydra.Serialization.cst("case"),
        scrutinee),
      clauseExprs.get(),
      defaultPart.get()))));
  }

  static hydra.ast.Expr commentToExpr(hydra.ext.lisp.syntax.Comment c) {
    String text = (c).text;
    return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
      "; ",
      text));
  }

  static hydra.ast.Expr condExpressionToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.CondExpression condExpr) {
    hydra.util.ConsList<hydra.ext.lisp.syntax.CondClause> clauses = (condExpr).clauses;
    hydra.util.Maybe<hydra.ext.lisp.syntax.Expression> dflt = (condExpr).default_;
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> clauseExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.ext.lisp.syntax.CondClause, hydra.util.ConsList<hydra.ast.Expr>>) (c -> hydra.util.ConsList.of(
            hydra.ext.lisp.Serde.expressionToExpr(
              d,
              (c).condition),
            hydra.ext.lisp.Serde.expressionToExpr(
              d,
              (c).body))),
          clauses)));
        hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> defaultPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
          () -> (hydra.util.ConsList<hydra.ast.Expr>) (hydra.util.ConsList.<hydra.ast.Expr>empty()),
          (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.util.ConsList<hydra.ast.Expr>>) (e -> hydra.util.ConsList.of(
            hydra.Serialization.cst(":else"),
            hydra.ext.lisp.Serde.expressionToExpr(
              d,
              e))),
          dflt));
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(hydra.Serialization.cst("cond")),
          clauseExprs.get(),
          defaultPart.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> clauseExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.ext.lisp.syntax.CondClause, hydra.ast.Expr>) (c -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
            hydra.ext.lisp.Serde.expressionToExpr(
              d,
              (c).condition),
            hydra.ext.lisp.Serde.expressionToExpr(
              d,
              (c).body))))),
          clauses));
        hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> defaultPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
          () -> (hydra.util.ConsList<hydra.ast.Expr>) (hydra.util.ConsList.<hydra.ast.Expr>empty()),
          (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.util.ConsList<hydra.ast.Expr>>) (e -> hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
            hydra.Serialization.cst("t"),
            hydra.ext.lisp.Serde.expressionToExpr(
              d,
              e)))))),
          dflt));
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(hydra.Serialization.cst("cond")),
          clauseExprs.get(),
          defaultPart.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> clauseExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.ext.lisp.syntax.CondClause, hydra.ast.Expr>) (c -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
            hydra.ext.lisp.Serde.expressionToExpr(
              d,
              (c).condition),
            hydra.ext.lisp.Serde.expressionToExpr(
              d,
              (c).body))))),
          clauses));
        hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> defaultPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
          () -> (hydra.util.ConsList<hydra.ast.Expr>) (hydra.util.ConsList.<hydra.ast.Expr>empty()),
          (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.util.ConsList<hydra.ast.Expr>>) (e -> hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
            hydra.Serialization.cst("t"),
            hydra.ext.lisp.Serde.expressionToExpr(
              d,
              e)))))),
          dflt));
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(hydra.Serialization.cst("cond")),
          clauseExprs.get(),
          defaultPart.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> clauseExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.ext.lisp.syntax.CondClause, hydra.ast.Expr>) (c -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
            hydra.ext.lisp.Serde.expressionToExpr(
              d,
              (c).condition),
            hydra.ext.lisp.Serde.expressionToExpr(
              d,
              (c).body))))),
          clauses));
        hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> defaultPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
          () -> (hydra.util.ConsList<hydra.ast.Expr>) (hydra.util.ConsList.<hydra.ast.Expr>empty()),
          (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.util.ConsList<hydra.ast.Expr>>) (e -> hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
            hydra.Serialization.cst("else"),
            hydra.ext.lisp.Serde.expressionToExpr(
              d,
              e)))))),
          dflt));
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(hydra.Serialization.cst("cond")),
          clauseExprs.get(),
          defaultPart.get()))));
      }
    });
  }

  static hydra.ast.Expr constantDefinitionToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.ConstantDefinition cdef) {
    hydra.ast.Expr name = hydra.ext.lisp.Serde.symbolToExpr((cdef).name);
    hydra.ast.Expr value = hydra.ext.lisp.Serde.expressionToExpr(
      d,
      (cdef).value);
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
      hydra.Serialization.cst(hydra.ext.lisp.Serde.defconstKeyword(d)),
      name,
      value)));
  }

  static String defKeyword(hydra.ext.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return "def";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return "defvar";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return "cl:defvar";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return "define";
      }
    });
  }

  static String defconstKeyword(hydra.ext.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return "def";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return "defconst";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return "cl:defconstant";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return "define";
      }
    });
  }

  static String defnKeyword(hydra.ext.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return "defn";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return "defun";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return "cl:defun";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return "define";
      }
    });
  }

  static String defrecordKeyword(hydra.ext.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return "defrecord";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return "cl-defstruct";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return "cl:defstruct";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return "define-record-type";
      }
    });
  }

  static hydra.ast.Expr doExpressionToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.DoExpression doExpr) {
    String kw = (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return "do";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return "progn";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return "progn";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return "begin";
      }
    });
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
      hydra.util.ConsList.of(hydra.Serialization.cst(kw)),
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.ext.lisp.Serde.expressionToExpr(
          d,
          v1)),
        (doExpr).expressions))));
  }

  static hydra.ast.Expr docstringToExpr(hydra.ext.lisp.syntax.Docstring ds) {
    return hydra.Serialization.cst(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      ";; ",
      (ds).value)));
  }

  static hydra.ast.Expr exportDeclarationToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.ExportDeclaration edecl) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> syms = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      hydra.ext.lisp.Serde::symbolToExpr,
      (edecl).symbols));
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.cst("");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.ast.Expr, hydra.ast.Expr>) (s -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
            hydra.Serialization.cst("provide"),
            hydra.Serialization.noSep(hydra.util.ConsList.of(
              hydra.Serialization.cst("'"),
              s)))))),
          syms.get()));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
          hydra.util.ConsList.of(hydra.Serialization.cst(":export")),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.ast.Expr, hydra.ast.Expr>) (s -> hydra.Serialization.noSep(hydra.util.ConsList.of(
              hydra.Serialization.cst(":"),
              s))),
            syms.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
          hydra.util.ConsList.of(hydra.Serialization.cst("export")),
          syms.get())));
      }
    });
  }

  static hydra.ast.Expr expressionToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.Expression expr) {
    return (expr).accept(new hydra.ext.lisp.syntax.Expression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.Application a) {
        return hydra.ext.lisp.Serde.applicationToExpr(
          d,
          (a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.Lambda l) {
        return hydra.ext.lisp.Serde.lambdaToExpr(
          d,
          (l).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.Let l) {
        return hydra.ext.lisp.Serde.letExpressionToExpr(
          d,
          (l).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.If i) {
        return hydra.ext.lisp.Serde.ifExpressionToExpr(
          d,
          (i).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.Cond c) {
        return hydra.ext.lisp.Serde.condExpressionToExpr(
          d,
          (c).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.Case c) {
        return hydra.ext.lisp.Serde.caseExpressionToExpr(
          d,
          (c).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.And a) {
        return hydra.ext.lisp.Serde.andExpressionToExpr(
          d,
          (a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.Or o) {
        return hydra.ext.lisp.Serde.orExpressionToExpr(
          d,
          (o).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.Not n) {
        return hydra.ext.lisp.Serde.notExpressionToExpr(
          d,
          (n).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.Do e) {
        return hydra.ext.lisp.Serde.doExpressionToExpr(
          d,
          (e).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.Begin e) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
          hydra.util.ConsList.of(hydra.Serialization.cst("begin")),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.ext.lisp.Serde.expressionToExpr(
              d,
              v1)),
            (e).value.expressions))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.Variable v) {
        return hydra.ext.lisp.Serde.variableReferenceToExpr(
          d,
          (v).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.Literal l) {
        return hydra.ext.lisp.Serde.literalToExpr(
          d,
          (l).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.List l) {
        return hydra.ext.lisp.Serde.listLiteralToExpr(
          d,
          (l).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.Vector v) {
        return hydra.ext.lisp.Serde.vectorLiteralToExpr(
          d,
          (v).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.Map m) {
        return hydra.ext.lisp.Serde.mapLiteralToExpr(
          d,
          (m).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.Set s) {
        return hydra.ext.lisp.Serde.setLiteralToExpr(
          d,
          (s).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.Cons c) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("cons"),
          hydra.ext.lisp.Serde.expressionToExpr(
            d,
            (c).value.head),
          hydra.ext.lisp.Serde.expressionToExpr(
            d,
            (c).value.tail))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.DottedPair p) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.ext.lisp.Serde.expressionToExpr(
            d,
            (p).value.car),
          hydra.Serialization.cst("."),
          hydra.ext.lisp.Serde.expressionToExpr(
            d,
            (p).value.cdr))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.FieldAccess fa) {
        return hydra.ext.lisp.Serde.fieldAccessToExpr(
          d,
          (fa).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.TypeAnnotation ta) {
        return hydra.ext.lisp.Serde.expressionToExpr(
          d,
          (ta).value.expression);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.Quote q) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("'"),
          hydra.ext.lisp.Serde.expressionToExpr(
            d,
            (q).value.body)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.Quasiquote q) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("`"),
          hydra.ext.lisp.Serde.expressionToExpr(
            d,
            (q).value.body)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.Unquote u) {
        return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
            return hydra.Serialization.noSep(hydra.util.ConsList.of(
              hydra.Serialization.cst("~"),
              hydra.ext.lisp.Serde.expressionToExpr(
                d,
                (u).value.body)));
          }

          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
            return hydra.Serialization.noSep(hydra.util.ConsList.of(
              hydra.Serialization.cst(","),
              hydra.ext.lisp.Serde.expressionToExpr(
                d,
                (u).value.body)));
          }

          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
            return hydra.Serialization.noSep(hydra.util.ConsList.of(
              hydra.Serialization.cst(","),
              hydra.ext.lisp.Serde.expressionToExpr(
                d,
                (u).value.body)));
          }

          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
            return hydra.Serialization.noSep(hydra.util.ConsList.of(
              hydra.Serialization.cst(","),
              hydra.ext.lisp.Serde.expressionToExpr(
                d,
                (u).value.body)));
          }
        });
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.SplicingUnquote su) {
        return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
            return hydra.Serialization.noSep(hydra.util.ConsList.of(
              hydra.Serialization.cst("~@"),
              hydra.ext.lisp.Serde.expressionToExpr(
                d,
                (su).value.body)));
          }

          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
            return hydra.Serialization.noSep(hydra.util.ConsList.of(
              hydra.Serialization.cst(",@"),
              hydra.ext.lisp.Serde.expressionToExpr(
                d,
                (su).value.body)));
          }

          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
            return hydra.Serialization.noSep(hydra.util.ConsList.of(
              hydra.Serialization.cst(",@"),
              hydra.ext.lisp.Serde.expressionToExpr(
                d,
                (su).value.body)));
          }

          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
            return hydra.Serialization.noSep(hydra.util.ConsList.of(
              hydra.Serialization.cst(",@"),
              hydra.ext.lisp.Serde.expressionToExpr(
                d,
                (su).value.body)));
          }
        });
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Expression.SExpression s) {
        return hydra.ext.lisp.Serde.sExpressionToExpr((s).value);
      }
    });
  }

  static hydra.ast.Expr falseExpr(hydra.ext.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.cst("false");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.cst("nil");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.cst("cl:nil");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.cst("#f");
      }
    });
  }

  static hydra.ast.Expr fieldAccessToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.FieldAccess fa) {
    hydra.ast.Expr field = hydra.ext.lisp.Serde.symbolToExpr((fa).field);
    hydra.ast.Expr rtype = hydra.ext.lisp.Serde.symbolToExpr((fa).recordType);
    hydra.ast.Expr target = hydra.ext.lisp.Serde.expressionToExpr(
      d,
      (fa).target);
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.Serialization.noSep(hydra.util.ConsList.of(
            hydra.Serialization.cst(":"),
            field)),
          target)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.Serialization.noSep(hydra.util.ConsList.of(
            rtype,
            hydra.Serialization.cst("-"),
            field)),
          target)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.Serialization.noSep(hydra.util.ConsList.of(
            rtype,
            hydra.Serialization.cst("-"),
            field)),
          target)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.Serialization.noSep(hydra.util.ConsList.of(
            rtype,
            hydra.Serialization.cst("-"),
            field)),
          target)));
      }
    });
  }

  static hydra.ast.Expr functionDefinitionToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.FunctionDefinition fdef) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> body = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.ext.lisp.Serde.expressionToExpr(
        d,
        v1)),
      (fdef).body));
    hydra.ast.Expr name = hydra.ext.lisp.Serde.symbolToExpr((fdef).name);
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> params = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      hydra.ext.lisp.Serde::symbolToExpr,
      (fdef).params));
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(
            hydra.Serialization.cst("defn"),
            name),
          hydra.util.ConsList.of(hydra.Serialization.brackets(
            hydra.Serialization.squareBrackets(),
            hydra.Serialization.inlineStyle(),
            hydra.Serialization.spaceSep(params.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(
            hydra.Serialization.cst("defun"),
            name),
          hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.spaceSep(params.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(
            hydra.Serialization.cst("defun"),
            name),
          hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.spaceSep(params.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(hydra.Serialization.cst("define")),
          hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
            hydra.util.ConsList.of(name),
            params.get())))),
          body.get()))));
      }
    });
  }

  static hydra.ast.Expr ifExpressionToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.IfExpression ifExpr) {
    hydra.ast.Expr cond = hydra.ext.lisp.Serde.expressionToExpr(
      d,
      (ifExpr).condition);
    hydra.util.Maybe<hydra.ext.lisp.syntax.Expression> else_ = (ifExpr).else_;
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> elsePart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (hydra.util.ConsList<hydra.ast.Expr>) (hydra.util.ConsList.<hydra.ast.Expr>empty()),
      (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.util.ConsList<hydra.ast.Expr>>) (e -> hydra.util.ConsList.of(hydra.ext.lisp.Serde.expressionToExpr(
        d,
        e))),
      else_));
    hydra.ast.Expr then = hydra.ext.lisp.Serde.expressionToExpr(
      d,
      (ifExpr).then);
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
      hydra.util.ConsList.of(
        hydra.Serialization.cst("if"),
        cond,
        then),
      elsePart.get()))));
  }

  static hydra.ast.Expr importDeclarationToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.ImportDeclaration idecl) {
    String modName = (idecl).module.value;
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.Serialization.cst(":require"),
          hydra.Serialization.brackets(
            hydra.Serialization.squareBrackets(),
            hydra.Serialization.inlineStyle(),
            hydra.Serialization.spaceSep(hydra.util.ConsList.of(hydra.Serialization.cst(modName)))))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("require"),
          hydra.Serialization.noSep(hydra.util.ConsList.of(
            hydra.Serialization.cst("'"),
            hydra.Serialization.cst(modName))))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.Serialization.cst(":use"),
          hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
            ":",
            modName)))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("import"),
          hydra.Serialization.parens(hydra.Serialization.cst(modName)))));
      }
    });
  }

  static hydra.ast.Expr keywordToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.Keyword k) {
    String name = (k).name;
    hydra.util.Maybe<String> ns = (k).namespace;
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr otherwise(hydra.ext.lisp.syntax.Dialect instance) {
        return hydra.Serialization.cst(hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.lib.strings.Cat2.apply(
            ":",
            name),
          (java.util.function.Function<String, String>) (n -> hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
            n,
            "/:",
            name))),
          ns));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("'"),
          hydra.Serialization.cst(name)));
      }
    });
  }

  static String lambdaKeyword(hydra.ext.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return "fn";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return "lambda";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return "cl:lambda";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return "lambda";
      }
    });
  }

  static hydra.ast.Expr lambdaToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.Lambda lam) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> body = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.ext.lisp.Serde.expressionToExpr(
        d,
        v1)),
      (lam).body));
    String kw = hydra.ext.lisp.Serde.lambdaKeyword(d);
    hydra.util.Maybe<hydra.ext.lisp.syntax.Symbol> mname = (lam).name;
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> params = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      hydra.ext.lisp.Serde::symbolToExpr,
      (lam).params));
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
            hydra.util.ConsList.of(hydra.Serialization.cst(kw)),
            hydra.util.ConsList.of(hydra.Serialization.brackets(
              hydra.Serialization.squareBrackets(),
              hydra.Serialization.inlineStyle(),
              hydra.Serialization.spaceSep(params.get()))),
            body.get())))),
          (java.util.function.Function<hydra.ext.lisp.syntax.Symbol, hydra.ast.Expr>) (sym -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
            hydra.util.ConsList.of(
              hydra.Serialization.cst(kw),
              hydra.ext.lisp.Serde.symbolToExpr(sym)),
            hydra.util.ConsList.of(hydra.Serialization.brackets(
              hydra.Serialization.squareBrackets(),
              hydra.Serialization.inlineStyle(),
              hydra.Serialization.spaceSep(params.get()))),
            body.get()))))),
          mname);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(hydra.Serialization.cst(kw)),
          hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.spaceSep(params.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(hydra.Serialization.cst(kw)),
          hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.spaceSep(params.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(hydra.Serialization.cst(kw)),
          hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.spaceSep(params.get()))),
          body.get()))));
      }
    });
  }

  static hydra.ast.Expr letExpressionToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.LetExpression letExpr) {
    hydra.util.ConsList<hydra.ext.lisp.syntax.LetBinding> bindings = (letExpr).bindings;
    hydra.util.Lazy<hydra.util.ConsList<hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>>> bindingPairs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.lisp.syntax.LetBinding, hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>>) (b -> (b).accept(new hydra.ext.lisp.syntax.LetBinding.PartialVisitor<>() {
        @Override
        public hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr> visit(hydra.ext.lisp.syntax.LetBinding.Simple sb) {
          return (hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>) ((hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>) (new hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>(hydra.ext.lisp.Serde.symbolToExpr((sb).value.name), hydra.ext.lisp.Serde.expressionToExpr(
            d,
            (sb).value.value))));
        }

        @Override
        public hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr> visit(hydra.ext.lisp.syntax.LetBinding.Destructuring ignored) {
          return (hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>) ((hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>) (new hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>(hydra.Serialization.cst("<destructuring>"), hydra.Serialization.cst("<destructuring>"))));
        }
      })),
      bindings));
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> body = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.ext.lisp.Serde.expressionToExpr(
        d,
        v1)),
      (letExpr).body));
    hydra.ext.lisp.syntax.LetKind kind = (letExpr).kind;
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return (kind).accept(new hydra.ext.lisp.syntax.LetKind.PartialVisitor<>() {
          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.LetKind.Recursive _2) {
            return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
              hydra.util.ConsList.of(hydra.Serialization.cst("let")),
              hydra.util.ConsList.of(hydra.Serialization.brackets(
                hydra.Serialization.squareBrackets(),
                hydra.Serialization.inlineStyle(),
                hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>, hydra.util.ConsList<hydra.ast.Expr>>) (p -> hydra.util.ConsList.of(
                    hydra.lib.pairs.First.apply(p),
                    hydra.lib.pairs.Second.apply(p))),
                  bindingPairs.get()))))),
              body.get()))));
          }

          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.LetKind.Parallel _2) {
            return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
              hydra.util.ConsList.of(hydra.Serialization.cst("let")),
              hydra.util.ConsList.of(hydra.Serialization.brackets(
                hydra.Serialization.squareBrackets(),
                hydra.Serialization.inlineStyle(),
                hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>, hydra.util.ConsList<hydra.ast.Expr>>) (p -> hydra.util.ConsList.of(
                    hydra.lib.pairs.First.apply(p),
                    hydra.lib.pairs.Second.apply(p))),
                  bindingPairs.get()))))),
              body.get()))));
          }

          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.LetKind.Sequential _2) {
            return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
              hydra.util.ConsList.of(hydra.Serialization.cst("let")),
              hydra.util.ConsList.of(hydra.Serialization.brackets(
                hydra.Serialization.squareBrackets(),
                hydra.Serialization.inlineStyle(),
                hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>, hydra.util.ConsList<hydra.ast.Expr>>) (p -> hydra.util.ConsList.of(
                    hydra.lib.pairs.First.apply(p),
                    hydra.lib.pairs.Second.apply(p))),
                  bindingPairs.get()))))),
              body.get()))));
          }
        });
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> bindingExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>, hydra.ast.Expr>) (p -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
            hydra.lib.pairs.First.apply(p),
            hydra.lib.pairs.Second.apply(p))))),
          bindingPairs.get()));
        String kw = (kind).accept(new hydra.ext.lisp.syntax.LetKind.PartialVisitor<>() {
          @Override
          public String visit(hydra.ext.lisp.syntax.LetKind.Parallel _2) {
            return "let";
          }

          @Override
          public String visit(hydra.ext.lisp.syntax.LetKind.Sequential _2) {
            return "let*";
          }

          @Override
          public String visit(hydra.ext.lisp.syntax.LetKind.Recursive _2) {
            return "letrec";
          }
        });
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(hydra.Serialization.cst(kw)),
          hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.spaceSep(bindingExprs.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> bindingExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>, hydra.ast.Expr>) (p -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
            hydra.lib.pairs.First.apply(p),
            hydra.lib.pairs.Second.apply(p))))),
          bindingPairs.get()));
        String kw = (kind).accept(new hydra.ext.lisp.syntax.LetKind.PartialVisitor<>() {
          @Override
          public String visit(hydra.ext.lisp.syntax.LetKind.Parallel _2) {
            return "let";
          }

          @Override
          public String visit(hydra.ext.lisp.syntax.LetKind.Sequential _2) {
            return "let*";
          }

          @Override
          public String visit(hydra.ext.lisp.syntax.LetKind.Recursive _2) {
            return "letrec";
          }
        });
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(hydra.Serialization.cst(kw)),
          hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.spaceSep(bindingExprs.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> bindingExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.ast.Expr, hydra.ast.Expr>, hydra.ast.Expr>) (p -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
            hydra.lib.pairs.First.apply(p),
            hydra.lib.pairs.Second.apply(p))))),
          bindingPairs.get()));
        String kw = (kind).accept(new hydra.ext.lisp.syntax.LetKind.PartialVisitor<>() {
          @Override
          public String visit(hydra.ext.lisp.syntax.LetKind.Parallel _2) {
            return "let";
          }

          @Override
          public String visit(hydra.ext.lisp.syntax.LetKind.Sequential _2) {
            return "let*";
          }

          @Override
          public String visit(hydra.ext.lisp.syntax.LetKind.Recursive _2) {
            return "letrec";
          }
        });
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(hydra.Serialization.cst(kw)),
          hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.spaceSep(bindingExprs.get()))),
          body.get()))));
      }
    });
  }

  static String listKeyword(hydra.ext.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return "list";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return "list";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return "cl:list";
      }

      @Override
      public String visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return "list";
      }
    });
  }

  static hydra.ast.Expr listLiteralToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.ListLiteral ll) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> elems = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.ext.lisp.Serde.expressionToExpr(
        d,
        v1)),
      (ll).elements));
    Boolean quoted = (ll).quoted;
    return hydra.lib.logic.IfElse.lazy(
      quoted,
      () -> hydra.Serialization.noSep(hydra.util.ConsList.of(
        hydra.Serialization.cst("'"),
        hydra.Serialization.parens(hydra.Serialization.spaceSep(elems.get())))),
      () -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
        hydra.util.ConsList.of(hydra.Serialization.cst(hydra.ext.lisp.Serde.listKeyword(d))),
        elems.get()))));
  }

  static hydra.ast.Expr literalToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.Literal lit) {
    return (lit).accept(new hydra.ext.lisp.syntax.Literal.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Literal.Integer_ i) {
        return hydra.Serialization.cst(hydra.lib.literals.ShowBigint.apply((i).value.value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Literal.Float_ f) {
        return hydra.Serialization.cst(hydra.lib.literals.ShowBigfloat.apply((f).value.value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Literal.String_ s) {
        String e1 = hydra.lib.strings.Intercalate.apply(
          "\\\\",
          hydra.lib.strings.SplitOn.apply(
            "\\",
            (s).value));
        return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
            String escaped = hydra.lib.strings.Intercalate.apply(
              "\\\"",
              hydra.lib.strings.SplitOn.apply(
                "\"",
                e1));
            return hydra.Serialization.cst(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "\"",
              escaped,
              "\"")));
          }

          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
            String e2 = hydra.lib.strings.Intercalate.apply(
              "\\n",
              hydra.lib.strings.SplitOn.apply(
                hydra.lib.strings.FromList.apply(hydra.util.ConsList.of(10)),
                e1));
            String e3 = hydra.lib.strings.Intercalate.apply(
              "\\r",
              hydra.lib.strings.SplitOn.apply(
                hydra.lib.strings.FromList.apply(hydra.util.ConsList.of(13)),
                e2));
            String e4 = hydra.lib.strings.Intercalate.apply(
              "\\t",
              hydra.lib.strings.SplitOn.apply(
                hydra.lib.strings.FromList.apply(hydra.util.ConsList.of(9)),
                e3));
            String escaped = hydra.lib.strings.Intercalate.apply(
              "\\\"",
              hydra.lib.strings.SplitOn.apply(
                "\"",
                e4));
            return hydra.Serialization.cst(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "\"",
              escaped,
              "\"")));
          }

          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
            String e2 = hydra.lib.strings.Intercalate.apply(
              "\\n",
              hydra.lib.strings.SplitOn.apply(
                hydra.lib.strings.FromList.apply(hydra.util.ConsList.of(10)),
                e1));
            String e3 = hydra.lib.strings.Intercalate.apply(
              "\\r",
              hydra.lib.strings.SplitOn.apply(
                hydra.lib.strings.FromList.apply(hydra.util.ConsList.of(13)),
                e2));
            String e4 = hydra.lib.strings.Intercalate.apply(
              "\\t",
              hydra.lib.strings.SplitOn.apply(
                hydra.lib.strings.FromList.apply(hydra.util.ConsList.of(9)),
                e3));
            String escaped = hydra.lib.strings.Intercalate.apply(
              "\\\"",
              hydra.lib.strings.SplitOn.apply(
                "\"",
                e4));
            return hydra.Serialization.cst(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "\"",
              escaped,
              "\"")));
          }

          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
            String e2 = hydra.lib.strings.Intercalate.apply(
              "\\n",
              hydra.lib.strings.SplitOn.apply(
                hydra.lib.strings.FromList.apply(hydra.util.ConsList.of(10)),
                e1));
            String e3 = hydra.lib.strings.Intercalate.apply(
              "\\r",
              hydra.lib.strings.SplitOn.apply(
                hydra.lib.strings.FromList.apply(hydra.util.ConsList.of(13)),
                e2));
            String e4 = hydra.lib.strings.Intercalate.apply(
              "\\t",
              hydra.lib.strings.SplitOn.apply(
                hydra.lib.strings.FromList.apply(hydra.util.ConsList.of(9)),
                e3));
            String escaped = hydra.lib.strings.Intercalate.apply(
              "\\\"",
              hydra.lib.strings.SplitOn.apply(
                "\"",
                e4));
            return hydra.Serialization.cst(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "\"",
              escaped,
              "\"")));
          }
        });
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Literal.Character_ c) {
        String ch = (c).value.value;
        return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
            return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
              "\\",
              ch));
          }

          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
            return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
              "?",
              ch));
          }

          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
            return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
              "#\\",
              ch));
          }

          @Override
          public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
            return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
              "#\\",
              ch));
          }
        });
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Literal.Boolean_ b) {
        return hydra.lib.logic.IfElse.lazy(
          (b).value,
          () -> hydra.ext.lisp.Serde.trueExpr(d),
          () -> hydra.ext.lisp.Serde.falseExpr(d));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Literal.Nil ignored) {
        return hydra.ext.lisp.Serde.nilExpr(d);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Literal.Keyword k) {
        return hydra.ext.lisp.Serde.keywordToExpr(
          d,
          (k).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Literal.Symbol s) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("'"),
          hydra.ext.lisp.Serde.symbolToExpr((s).value)));
      }
    });
  }

  static hydra.ast.Expr macroDefinitionToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.MacroDefinition mdef) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> body = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.ext.lisp.Serde.expressionToExpr(
        d,
        v1)),
      (mdef).body));
    hydra.ast.Expr name = hydra.ext.lisp.Serde.symbolToExpr((mdef).name);
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> params = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      hydra.ext.lisp.Serde::symbolToExpr,
      (mdef).params));
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(
            hydra.Serialization.cst("defmacro"),
            name),
          hydra.util.ConsList.of(hydra.Serialization.brackets(
            hydra.Serialization.squareBrackets(),
            hydra.Serialization.inlineStyle(),
            hydra.Serialization.spaceSep(params.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(
            hydra.Serialization.cst("defmacro"),
            name),
          hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.spaceSep(params.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(
            hydra.Serialization.cst("defmacro"),
            name),
          hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.spaceSep(params.get()))),
          body.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(
            hydra.Serialization.cst("define-syntax"),
            name),
          body.get()))));
      }
    });
  }

  static hydra.ast.Expr mapLiteralToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.MapLiteral ml) {
    hydra.util.ConsList<hydra.ext.lisp.syntax.MapEntry> entries = (ml).entries;
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.brackets(
          hydra.Serialization.curlyBraces(),
          hydra.Serialization.inlineStyle(),
          hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.ext.lisp.syntax.MapEntry, hydra.util.ConsList<hydra.ast.Expr>>) (e -> hydra.util.ConsList.of(
              hydra.ext.lisp.Serde.expressionToExpr(
                d,
                (e).key),
              hydra.ext.lisp.Serde.expressionToExpr(
                d,
                (e).value))),
            entries))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("'"),
          hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.ext.lisp.syntax.MapEntry, hydra.ast.Expr>) (e -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
              hydra.ext.lisp.Serde.expressionToExpr(
                d,
                (e).key),
              hydra.Serialization.cst("."),
              hydra.ext.lisp.Serde.expressionToExpr(
                d,
                (e).value))))),
            entries)))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("'"),
          hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.ext.lisp.syntax.MapEntry, hydra.ast.Expr>) (e -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
              hydra.ext.lisp.Serde.expressionToExpr(
                d,
                (e).key),
              hydra.Serialization.cst("."),
              hydra.ext.lisp.Serde.expressionToExpr(
                d,
                (e).value))))),
            entries)))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
          hydra.util.ConsList.of(hydra.Serialization.cst("list")),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.ext.lisp.syntax.MapEntry, hydra.ast.Expr>) (e -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
              hydra.Serialization.cst("cons"),
              hydra.ext.lisp.Serde.expressionToExpr(
                d,
                (e).key),
              hydra.ext.lisp.Serde.expressionToExpr(
                d,
                (e).value))))),
            entries))));
      }
    });
  }

  static hydra.ast.Expr moduleDeclarationToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.ModuleDeclaration mdecl) {
    String name = (mdecl).name.value;
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("ns"),
          hydra.Serialization.cst(name))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.newlineSep(hydra.util.ConsList.of(
          hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
            hydra.Serialization.cst("require"),
            hydra.Serialization.noSep(hydra.util.ConsList.of(
              hydra.Serialization.cst("'"),
              hydra.Serialization.cst("cl-lib")))))),
          hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
            hydra.Serialization.cst("provide"),
            hydra.Serialization.noSep(hydra.util.ConsList.of(
              hydra.Serialization.cst("'"),
              hydra.Serialization.cst(name))))))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.newlineSep(hydra.util.ConsList.of(
          hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
            hydra.Serialization.cst("defpackage"),
            hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
              ":",
              name))))),
          hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
            hydra.Serialization.cst("in-package"),
            hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
              ":",
              name)))))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("define-library"),
          hydra.Serialization.parens(hydra.Serialization.cst(name)))));
      }
    });
  }

  static hydra.ast.Expr nilExpr(hydra.ext.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.cst("nil");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.cst("nil");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.cst("cl:nil");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.cst("'()");
      }
    });
  }

  static hydra.ast.Expr notExpressionToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.NotExpression notExpr) {
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
      hydra.Serialization.cst("not"),
      hydra.ext.lisp.Serde.expressionToExpr(
        d,
        (notExpr).expression))));
  }

  static hydra.ast.Expr orExpressionToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.OrExpression orExpr) {
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
      hydra.util.ConsList.of(hydra.Serialization.cst("or")),
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.ext.lisp.Serde.expressionToExpr(
          d,
          v1)),
        (orExpr).expressions))));
  }

  static hydra.ast.Expr programToExpr(hydra.ext.lisp.syntax.Program prog) {
    hydra.ext.lisp.syntax.Dialect d = (prog).dialect;
    hydra.util.ConsList<hydra.ext.lisp.syntax.ExportDeclaration> exports = (prog).exports;
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> exportSyms = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.lisp.syntax.ExportDeclaration, hydra.util.ConsList<hydra.ast.Expr>>) (edecl -> hydra.lib.lists.Map.apply(
        hydra.ext.lisp.Serde::symbolToExpr,
        (edecl).symbols)),
      exports)));
    hydra.util.ConsList<hydra.ext.lisp.syntax.TopLevelFormWithComments> forms = (prog).forms;
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> formPart = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.lisp.syntax.TopLevelFormWithComments, hydra.ast.Expr>) (v1 -> hydra.ext.lisp.Serde.topLevelFormWithCommentsToExpr(
        d,
        v1)),
      forms));
    hydra.util.ConsList<hydra.ext.lisp.syntax.ImportDeclaration> imports = (prog).imports;
    hydra.util.Lazy<hydra.util.ConsList<String>> importNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.lisp.syntax.ImportDeclaration, String>) (idecl -> (idecl).module.value),
      imports));
    hydra.util.Maybe<hydra.ext.lisp.syntax.ModuleDeclaration> modDecl = (prog).module;
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.Serialization.doubleNewlineSep(formPart.get()),
          (java.util.function.Function<hydra.ext.lisp.syntax.ModuleDeclaration, hydra.ast.Expr>) (m -> {
            hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> varNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.ext.lisp.syntax.TopLevelFormWithComments, hydra.util.ConsList<hydra.ast.Expr>>) (fwc -> {
                hydra.ext.lisp.syntax.TopLevelForm form = (fwc).form;
                return (form).accept(new hydra.ext.lisp.syntax.TopLevelForm.PartialVisitor<>() {
                  @Override
                  public hydra.util.ConsList<hydra.ast.Expr> otherwise(hydra.ext.lisp.syntax.TopLevelForm instance) {
                    return (hydra.util.ConsList<hydra.ast.Expr>) (hydra.util.ConsList.<hydra.ast.Expr>empty());
                  }

                  @Override
                  public hydra.util.ConsList<hydra.ast.Expr> visit(hydra.ext.lisp.syntax.TopLevelForm.Variable vd) {
                    return hydra.util.ConsList.of(hydra.ext.lisp.Serde.symbolToExpr((vd).value.name));
                  }

                  @Override
                  public hydra.util.ConsList<hydra.ast.Expr> visit(hydra.ext.lisp.syntax.TopLevelForm.Function fd) {
                    return hydra.util.ConsList.of(hydra.ext.lisp.Serde.symbolToExpr((fd).value.name));
                  }
                });
              }),
              forms)));
            hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> declareForm = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.lists.Null.apply(varNames.get()),
              () -> (hydra.util.ConsList<hydra.ast.Expr>) (hydra.util.ConsList.<hydra.ast.Expr>empty()),
              () -> hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
                hydra.util.ConsList.of(hydra.Serialization.cst("declare")),
                varNames.get()))))));
            String nameStr = (m).name.value;
            hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> requireClauses = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<String, hydra.ast.Expr>) (imp -> hydra.Serialization.brackets(
                hydra.Serialization.squareBrackets(),
                hydra.Serialization.inlineStyle(),
                hydra.Serialization.spaceSep(hydra.util.ConsList.of(
                  hydra.Serialization.cst(imp),
                  hydra.Serialization.cst(":refer"),
                  hydra.Serialization.cst(":all"))))),
              importNames.get()));
            hydra.util.Lazy<hydra.ast.Expr> nsForm = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.lists.Null.apply(requireClauses.get()),
              () -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
                hydra.Serialization.cst("ns"),
                hydra.Serialization.cst(nameStr)))),
              () -> hydra.Serialization.parens(hydra.Serialization.newlineSep(hydra.util.ConsList.of(
                hydra.Serialization.spaceSep(hydra.util.ConsList.of(
                  hydra.Serialization.cst("ns"),
                  hydra.Serialization.cst(nameStr))),
                hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
                  hydra.util.ConsList.of(hydra.Serialization.cst("  (:require")),
                  requireClauses.get())),
                hydra.Serialization.cst(")"))))));
            return hydra.Serialization.doubleNewlineSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
              hydra.util.ConsList.of(nsForm.get()),
              declareForm.get(),
              formPart.get())));
          }),
          modDecl);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.Serialization.doubleNewlineSep(formPart.get()),
          (java.util.function.Function<hydra.ext.lisp.syntax.ModuleDeclaration, hydra.ast.Expr>) (m -> {
            String nameStr = (m).name.value;
            hydra.ast.Expr provideForm = hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
              hydra.Serialization.cst("provide"),
              hydra.Serialization.noSep(hydra.util.ConsList.of(
                hydra.Serialization.cst("'"),
                hydra.Serialization.cst(nameStr))))));
            hydra.ast.Expr requireClLib = hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
              hydra.Serialization.cst("require"),
              hydra.Serialization.noSep(hydra.util.ConsList.of(
                hydra.Serialization.cst("'"),
                hydra.Serialization.cst("cl-lib"))))));
            hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> requireImports = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<String, hydra.ast.Expr>) (imp -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
                hydra.Serialization.cst("require"),
                hydra.Serialization.noSep(hydra.util.ConsList.of(
                  hydra.Serialization.cst("'"),
                  hydra.Serialization.cst(imp))))))),
              importNames.get()));
            return hydra.Serialization.doubleNewlineSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
              hydra.util.ConsList.of(requireClLib),
              requireImports.get(),
              formPart.get(),
              hydra.util.ConsList.of(provideForm))));
          }),
          modDecl);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.Serialization.doubleNewlineSep(formPart.get()),
          (java.util.function.Function<hydra.ext.lisp.syntax.ModuleDeclaration, hydra.ast.Expr>) (m -> {
            String nameStr = (m).name.value;
            String colonName = hydra.lib.strings.Cat2.apply(
              ":",
              nameStr);
            hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> exportClause = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.lists.Null.apply(exportSyms.get()),
              () -> (hydra.util.ConsList<hydra.ast.Expr>) (hydra.util.ConsList.<hydra.ast.Expr>empty()),
              () -> hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
                hydra.util.ConsList.of(hydra.Serialization.cst(":export")),
                hydra.lib.lists.Map.apply(
                  (java.util.function.Function<hydra.ast.Expr, hydra.ast.Expr>) (s -> hydra.Serialization.noSep(hydra.util.ConsList.of(
                    hydra.Serialization.cst(":"),
                    s))),
                  exportSyms.get())))))));
            hydra.util.Lazy<hydra.ast.Expr> useClause = new hydra.util.Lazy<>(() -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
              hydra.util.ConsList.of(
                hydra.Serialization.cst(":use"),
                hydra.Serialization.cst(":cl")),
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<String, hydra.ast.Expr>) (imp -> hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
                  ":",
                  imp))),
                importNames.get())))));
            hydra.util.Lazy<hydra.ast.Expr> defpkgForm = new hydra.util.Lazy<>(() -> hydra.Serialization.parens(hydra.Serialization.newlineSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
              hydra.util.ConsList.of(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
                hydra.Serialization.cst("defpackage"),
                hydra.Serialization.cst(colonName)))),
              hydra.util.ConsList.of(useClause.get()),
              exportClause.get())))));
            hydra.ast.Expr inpkgForm = hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
              hydra.Serialization.cst("in-package"),
              hydra.Serialization.cst(colonName))));
            return hydra.Serialization.doubleNewlineSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
              hydra.util.ConsList.of(
                defpkgForm.get(),
                inpkgForm),
              formPart.get())));
          }),
          modDecl);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.Serialization.doubleNewlineSep(formPart.get()),
          (java.util.function.Function<hydra.ext.lisp.syntax.ModuleDeclaration, hydra.ast.Expr>) (m -> {
            hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> domainImportExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.ext.lisp.syntax.ImportDeclaration, hydra.ast.Expr>) (idecl -> {
                String nsName = (idecl).module.value;
                hydra.util.Lazy<hydra.util.ConsList<String>> nsParts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
                  (java.util.function.Function<String, String>) (p -> hydra.Formatting.convertCaseCamelToLowerSnake(p)),
                  hydra.lib.strings.SplitOn.apply(
                    ".",
                    nsName)));
                return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
                  (java.util.function.Function<String, hydra.ast.Expr>) (p -> hydra.Serialization.cst(p)),
                  nsParts.get())));
              }),
              imports));
            hydra.ast.Expr schemeBaseExpr = hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
              hydra.Serialization.cst("scheme"),
              hydra.Serialization.cst("base"))));
            hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> allImportExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
              hydra.util.ConsList.of(schemeBaseExpr),
              domainImportExprs.get()));
            hydra.util.Lazy<hydra.ast.Expr> beginClause = new hydra.util.Lazy<>(() -> hydra.Serialization.parens(hydra.Serialization.newlineSep(hydra.lib.lists.Concat2.apply(
              hydra.util.ConsList.of(hydra.Serialization.cst("begin")),
              formPart.get()))));
            hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> exportClauses = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.ext.lisp.syntax.ExportDeclaration, hydra.ast.Expr>) (edecl -> hydra.ext.lisp.Serde.exportDeclarationToExpr(
                d,
                edecl)),
              exports));
            hydra.util.Lazy<hydra.ast.Expr> importClause = new hydra.util.Lazy<>(() -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
              hydra.util.ConsList.of(hydra.Serialization.cst("import")),
              allImportExprs.get()))));
            String nameStr = (m).name.value;
            hydra.util.Lazy<hydra.util.ConsList<String>> nameParts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              (java.util.function.Function<String, String>) (p -> hydra.Formatting.convertCaseCamelToLowerSnake(p)),
              hydra.lib.strings.SplitOn.apply(
                ".",
                nameStr)));
            hydra.util.Lazy<hydra.ast.Expr> nameExpr = new hydra.util.Lazy<>(() -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
              (java.util.function.Function<String, hydra.ast.Expr>) (p -> hydra.Serialization.cst(p)),
              nameParts.get()))));
            return hydra.Serialization.parens(hydra.Serialization.newlineSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
              hydra.util.ConsList.of(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
                hydra.Serialization.cst("define-library"),
                nameExpr.get()))),
              exportClauses.get(),
              hydra.util.ConsList.of(importClause.get()),
              hydra.util.ConsList.of(beginClause.get())))));
          }),
          modDecl);
      }
    });
  }

  static hydra.ast.Expr recordTypeDefinitionToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.RecordTypeDefinition rdef) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> fields = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.lisp.syntax.FieldDefinition, hydra.ast.Expr>) (f -> hydra.ext.lisp.Serde.symbolToExpr((f).name)),
      (rdef).fields));
    hydra.ast.Expr name = hydra.ext.lisp.Serde.symbolToExpr((rdef).name);
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        hydra.ast.Expr defrecordForm = hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("defrecord"),
          name,
          hydra.Serialization.brackets(
            hydra.Serialization.squareBrackets(),
            hydra.Serialization.inlineStyle(),
            hydra.Serialization.spaceSep(fields.get())))));
        hydra.util.Lazy<hydra.util.ConsList<String>> fieldNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.ext.lisp.syntax.FieldDefinition, String>) (f -> (f).name.value),
          (rdef).fields));
        String nameStr = (rdef).name.value;
        hydra.util.Lazy<hydra.ast.Expr> makeAlias = new hydra.util.Lazy<>(() -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(
            hydra.Serialization.cst("defn"),
            hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
              "make-",
              nameStr))),
          hydra.util.ConsList.of(hydra.Serialization.brackets(
            hydra.Serialization.squareBrackets(),
            hydra.Serialization.inlineStyle(),
            hydra.Serialization.spaceSep(fields.get()))),
          hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
            hydra.util.ConsList.of(hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
              "->",
              nameStr))),
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<String, hydra.ast.Expr>) (fn -> hydra.Serialization.cst(fn)),
              fieldNames.get()))))))))));
        return hydra.Serialization.newlineSep(hydra.util.ConsList.of(
          defrecordForm,
          makeAlias.get()));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(
            hydra.Serialization.cst("cl-defstruct"),
            name),
          fields.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(
            hydra.Serialization.cst("cl:defstruct"),
            name),
          fields.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        hydra.util.Lazy<hydra.util.ConsList<String>> fieldNames = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.ext.lisp.syntax.FieldDefinition, String>) (f -> (f).name.value),
          (rdef).fields));
        String nameStr = (rdef).name.value;
        hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> accessors = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<String, hydra.ast.Expr>) (fn -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
            hydra.Serialization.cst(fn),
            hydra.Serialization.cst(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              nameStr,
              "-",
              fn))))))),
          fieldNames.get()));
        hydra.util.Lazy<hydra.ast.Expr> constructor = new hydra.util.Lazy<>(() -> hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
          hydra.util.ConsList.of(hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
            "make-",
            nameStr))),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<String, hydra.ast.Expr>) (fn -> hydra.Serialization.cst(fn)),
            fieldNames.get())))));
        hydra.ast.Expr predicate = hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
          nameStr,
          "?"));
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
          hydra.util.ConsList.of(
            hydra.Serialization.cst("define-record-type"),
            name,
            constructor.get(),
            predicate),
          accessors.get()))));
      }
    });
  }

  static hydra.ast.Expr sExpressionToExpr(hydra.ext.lisp.syntax.SExpression sexpr) {
    return (sexpr).accept(new hydra.ext.lisp.syntax.SExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.SExpression.Atom a) {
        return hydra.Serialization.cst((a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.SExpression.List elems) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.ext.lisp.Serde::sExpressionToExpr,
          (elems).value)));
      }
    });
  }

  static hydra.ast.Expr setLiteralToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.SetLiteral sl) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> elems = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.ext.lisp.Serde.expressionToExpr(
        d,
        v1)),
      (sl).elements));
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("#"),
          hydra.Serialization.brackets(
            hydra.Serialization.curlyBraces(),
            hydra.Serialization.inlineStyle(),
            hydra.Serialization.spaceSep(elems.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
          hydra.util.ConsList.of(hydra.Serialization.cst("list")),
          elems.get())));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
          hydra.util.ConsList.of(hydra.Serialization.cst("cl:list")),
          elems.get())));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.lib.lists.Concat2.apply(
          hydra.util.ConsList.of(hydra.Serialization.cst("list")),
          elems.get())));
      }
    });
  }

  static hydra.ast.Expr symbolToExpr(hydra.ext.lisp.syntax.Symbol s) {
    return hydra.Serialization.cst((s).value);
  }

  static hydra.ast.Expr topLevelFormToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.TopLevelForm form) {
    return (form).accept(new hydra.ext.lisp.syntax.TopLevelForm.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.TopLevelForm.Function f) {
        return hydra.ext.lisp.Serde.functionDefinitionToExpr(
          d,
          (f).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.TopLevelForm.Variable v) {
        return hydra.ext.lisp.Serde.variableDefinitionToExpr(
          d,
          (v).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.TopLevelForm.Constant c) {
        return hydra.ext.lisp.Serde.constantDefinitionToExpr(
          d,
          (c).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.TopLevelForm.RecordType r) {
        return hydra.ext.lisp.Serde.recordTypeDefinitionToExpr(
          d,
          (r).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.TopLevelForm.Macro m) {
        return hydra.ext.lisp.Serde.macroDefinitionToExpr(
          d,
          (m).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.TopLevelForm.Expression e) {
        return hydra.ext.lisp.Serde.expressionToExpr(
          d,
          (e).value);
      }
    });
  }

  static hydra.ast.Expr topLevelFormWithCommentsToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.TopLevelFormWithComments fwc) {
    hydra.util.Maybe<hydra.ext.lisp.syntax.Comment> mcomment = (fwc).comment;
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> commentPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (hydra.util.ConsList<hydra.ast.Expr>) (hydra.util.ConsList.<hydra.ast.Expr>empty()),
      (java.util.function.Function<hydra.ext.lisp.syntax.Comment, hydra.util.ConsList<hydra.ast.Expr>>) (c -> hydra.util.ConsList.of(hydra.ext.lisp.Serde.commentToExpr(c))),
      mcomment));
    hydra.util.Maybe<hydra.ext.lisp.syntax.Docstring> mdoc = (fwc).doc;
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> docPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (hydra.util.ConsList<hydra.ast.Expr>) (hydra.util.ConsList.<hydra.ast.Expr>empty()),
      (java.util.function.Function<hydra.ext.lisp.syntax.Docstring, hydra.util.ConsList<hydra.ast.Expr>>) (ds -> hydra.util.ConsList.of(hydra.ext.lisp.Serde.docstringToExpr(ds))),
      mdoc));
    hydra.ext.lisp.syntax.TopLevelForm form = (fwc).form;
    hydra.ast.Expr formExpr = hydra.ext.lisp.Serde.topLevelFormToExpr(
      d,
      form);
    return hydra.Serialization.newlineSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
      commentPart.get(),
      docPart.get(),
      hydra.util.ConsList.of(formExpr))));
  }

  static hydra.ast.Expr trueExpr(hydra.ext.lisp.syntax.Dialect d) {
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.cst("true");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.cst("t");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.cst("cl:t");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.cst("#t");
      }
    });
  }

  static hydra.ast.Expr variableDefinitionToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.VariableDefinition vdef) {
    hydra.ast.Expr name = hydra.ext.lisp.Serde.symbolToExpr((vdef).name);
    hydra.ast.Expr value = hydra.ext.lisp.Serde.expressionToExpr(
      d,
      (vdef).value);
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
      hydra.Serialization.cst(hydra.ext.lisp.Serde.defKeyword(d)),
      name,
      value)));
  }

  static hydra.ast.Expr variableReferenceToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.VariableReference vref) {
    Boolean isFnNs = (vref).functionNamespace;
    hydra.ast.Expr name = hydra.ext.lisp.Serde.symbolToExpr((vref).name);
    return hydra.lib.logic.IfElse.lazy(
      isFnNs,
      () -> (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
        @Override
        public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
          return hydra.Serialization.noSep(hydra.util.ConsList.of(
            hydra.Serialization.cst("#'"),
            name));
        }

        @Override
        public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
          return name;
        }

        @Override
        public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
          return name;
        }

        @Override
        public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
          return name;
        }
      }),
      () -> name);
  }

  static hydra.ast.Expr vectorLiteralToExpr(hydra.ext.lisp.syntax.Dialect d, hydra.ext.lisp.syntax.VectorLiteral vl) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> elems = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.lisp.syntax.Expression, hydra.ast.Expr>) (v1 -> hydra.ext.lisp.Serde.expressionToExpr(
        d,
        v1)),
      (vl).elements));
    return (d).accept(new hydra.ext.lisp.syntax.Dialect.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Clojure ignored) {
        return hydra.Serialization.brackets(
          hydra.Serialization.squareBrackets(),
          hydra.Serialization.inlineStyle(),
          hydra.Serialization.spaceSep(elems.get()));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.EmacsLisp ignored) {
        return hydra.Serialization.brackets(
          hydra.Serialization.squareBrackets(),
          hydra.Serialization.inlineStyle(),
          hydra.Serialization.spaceSep(elems.get()));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.CommonLisp ignored) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("#"),
          hydra.Serialization.parens(hydra.Serialization.spaceSep(elems.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.lisp.syntax.Dialect.Scheme ignored) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("#"),
          hydra.Serialization.parens(hydra.Serialization.spaceSep(elems.get()))));
      }
    });
  }
}
