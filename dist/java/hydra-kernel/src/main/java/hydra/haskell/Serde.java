// Note: this is an automatically generated file. Do not edit.

package hydra.haskell;

/**
 * Haskell operator precendence and associativity are drawn from:
 * https://self-learning-java-tutorial.blogspot.com/2016/04/haskell-operator-precedence.html
 * Other operators were investigated using GHCi, e.g. ":info (-&gt;)"
 * Operator names are drawn (loosely) from:
 * https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators
 */
public interface Serde {
  static hydra.ast.Expr alternativeToExpr(hydra.haskell.syntax.Alternative alt) {
    return hydra.Serialization.structuralSpaceSep(java.util.Arrays.asList(
      hydra.haskell.Serde.patternToExpr((alt).pattern),
      hydra.Serialization.cst("->"),
      hydra.haskell.Serde.caseRhsToExpr((alt).rhs)));
  }

  static hydra.ast.Expr applicationExpressionToExpr(hydra.haskell.syntax.ApplicationExpression app) {
    return hydra.Serialization.ifx(
      hydra.haskell.Operators.appOp(),
      hydra.haskell.Serde.expressionToExpr((app).function),
      hydra.haskell.Serde.expressionToExpr((app).argument));
  }

  static hydra.ast.Expr applicationPatternToExpr(hydra.haskell.syntax.ApplicationPattern appPat) {
    hydra.haskell.syntax.Name name = (appPat).name;
    java.util.List<hydra.haskell.syntax.Pattern> pats = (appPat).args;
    return hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
      hydra.haskell.Serde.nameToExpr(name),
      hydra.lib.lists.Map.apply(
        hydra.haskell.Serde::patternToExpr,
        pats)));
  }

  static hydra.ast.Expr assertionToExpr(hydra.haskell.syntax.Assertion sert) {
    return (sert).accept(new hydra.haskell.syntax.Assertion.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Assertion.Class_ cls) {
        return hydra.haskell.Serde.classAssertionToExpr((cls).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Assertion.Tuple serts) {
        return hydra.Serialization.parenList(
          false,
          hydra.lib.lists.Map.apply(
            hydra.haskell.Serde::assertionToExpr,
            (serts).value));
      }
    });
  }

  static hydra.ast.Expr caseExpressionToExpr(hydra.haskell.syntax.CaseExpression caseExpr) {
    java.util.List<hydra.haskell.syntax.Alternative> alts = (caseExpr).alternatives;
    hydra.haskell.syntax.Expression cs = (caseExpr).case_;
    hydra.ast.Expr lhs = hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst("case"),
      hydra.haskell.Serde.expressionToExpr(cs)));
    hydra.ast.Op ofOp = new hydra.ast.Op(new hydra.ast.Symbol("of"), new hydra.ast.Padding(new hydra.ast.Ws.Space(), new hydra.ast.Ws.BreakAndIndent("  ")), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None());
    hydra.util.Lazy<hydra.ast.Expr> rhs = new hydra.util.Lazy<>(() -> hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
      hydra.haskell.Serde::alternativeToExpr,
      alts)));
    return hydra.Serialization.ifx(
      ofOp,
      lhs,
      rhs.get());
  }

  static hydra.ast.Expr caseRhsToExpr(hydra.haskell.syntax.CaseRhs rhs) {
    return hydra.haskell.Serde.expressionToExpr((rhs).value);
  }

  static hydra.ast.Expr classAssertionToExpr(hydra.haskell.syntax.ClassAssertion clsAsrt) {
    hydra.haskell.syntax.Name name = (clsAsrt).name;
    java.util.List<hydra.haskell.syntax.Type> types = (clsAsrt).types;
    return hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
      hydra.haskell.Serde.nameToExpr(name),
      java.util.Arrays.asList(hydra.Serialization.commaSep(
        hydra.Serialization.halfBlockStyle(),
        hydra.lib.lists.Map.apply(
          hydra.haskell.Serde::typeToExpr,
          types)))));
  }

  static hydra.ast.Expr constructRecordExpressionToExpr(hydra.haskell.syntax.ConstructRecordExpression constructRecord) {
    java.util.function.Function<hydra.haskell.syntax.FieldUpdate, hydra.ast.Expr> fromUpdate = (java.util.function.Function<hydra.haskell.syntax.FieldUpdate, hydra.ast.Expr>) (update -> {
      hydra.haskell.syntax.Name fn = (update).name;
      hydra.haskell.syntax.Expression val = (update).value;
      return hydra.Serialization.ifx(
        hydra.haskell.Operators.defineOp(),
        hydra.haskell.Serde.nameToExpr(fn),
        hydra.haskell.Serde.expressionToExpr(val));
    });
    java.util.List<hydra.haskell.syntax.FieldUpdate> updates = (constructRecord).fields;
    hydra.util.Lazy<hydra.ast.Expr> body = new hydra.util.Lazy<>(() -> hydra.Serialization.commaSep(
      hydra.Serialization.halfBlockStyle(),
      hydra.lib.lists.Map.apply(
        fromUpdate,
        updates)));
    hydra.haskell.syntax.Name name = (constructRecord).name;
    return hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
      hydra.haskell.Serde.nameToExpr(name),
      java.util.Arrays.asList(hydra.Serialization.brackets(
        hydra.Serialization.curlyBraces(),
        hydra.Serialization.halfBlockStyle(),
        body.get()))));
  }

  static hydra.ast.Expr constructorToExpr(hydra.haskell.syntax.Constructor cons) {
    return (cons).accept(new hydra.haskell.syntax.Constructor.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Constructor.Ordinary ord) {
        hydra.haskell.syntax.Name name = (ord).value.name;
        java.util.List<hydra.haskell.syntax.Type> types = (ord).value.fields;
        return hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
          hydra.haskell.Serde.nameToExpr(name),
          java.util.Arrays.asList(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
            hydra.haskell.Serde::typeToExpr,
            types)))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Constructor.Record rec) {
        java.util.List<hydra.haskell.syntax.FieldWithComments> fields = (rec).value.fields;
        hydra.haskell.syntax.Name name = (rec).value.name;
        return hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
          hydra.haskell.Serde.nameToExpr(name),
          java.util.Arrays.asList(hydra.Serialization.curlyBracesList(
            (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
            hydra.Serialization.halfBlockStyle(),
            hydra.lib.lists.Map.apply(
              hydra.haskell.Serde::fieldWithCommentsToExpr,
              fields)))));
      }
    });
  }

  static hydra.ast.Expr constructorWithCommentsToExpr(hydra.haskell.syntax.ConstructorWithComments consWithComments) {
    hydra.haskell.syntax.Constructor body = (consWithComments).body;
    hydra.util.Maybe<String> mc = (consWithComments).comments;
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.haskell.Serde.constructorToExpr(body),
      (java.util.function.Function<String, hydra.ast.Expr>) (c -> hydra.Serialization.newlineSep(hydra.lib.lists.Cons.apply(
        hydra.Serialization.cst(hydra.haskell.Serde.toHaskellComments(c)),
        java.util.Arrays.asList(hydra.haskell.Serde.constructorToExpr(body))))),
      mc);
  }

  static hydra.ast.Expr dataOrNewtypeToExpr(hydra.haskell.syntax.DataOrNewtype kw) {
    return (kw).accept(new hydra.haskell.syntax.DataOrNewtype.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.DataOrNewtype.Data ignored) {
        return hydra.Serialization.cst("data");
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.DataOrNewtype.Newtype ignored) {
        return hydra.Serialization.cst("newtype");
      }
    });
  }

  static hydra.ast.Expr declarationHeadToExpr(hydra.haskell.syntax.DeclarationHead hd) {
    return (hd).accept(new hydra.haskell.syntax.DeclarationHead.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.DeclarationHead.Application appHead) {
        hydra.haskell.syntax.DeclarationHead fun = (appHead).value.function;
        hydra.haskell.syntax.Variable op = (appHead).value.operand;
        return hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
          hydra.haskell.Serde.declarationHeadToExpr(fun),
          java.util.Arrays.asList(hydra.haskell.Serde.variableToExpr(op))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.DeclarationHead.Simple name) {
        return hydra.haskell.Serde.nameToExpr((name).value);
      }
    });
  }

  static hydra.ast.Expr declarationToExpr(hydra.haskell.syntax.Declaration decl) {
    return (decl).accept(new hydra.haskell.syntax.Declaration.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Declaration.Data dataDecl) {
        java.util.List<hydra.haskell.syntax.ConstructorWithComments> cons = (dataDecl).value.constructors;
        hydra.util.Lazy<hydra.ast.Expr> constructors = new hydra.util.Lazy<>(() -> hydra.Serialization.orSep(
          hydra.Serialization.halfBlockStyle(),
          hydra.lib.lists.Map.apply(
            hydra.haskell.Serde::constructorWithCommentsToExpr,
            cons)));
        java.util.List<hydra.haskell.syntax.Deriving> deriv = (dataDecl).value.deriving;
        hydra.util.Lazy<java.util.List<hydra.haskell.syntax.Name>> derivCat = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          wrapped -> (wrapped).value,
          deriv)));
        hydra.util.Lazy<java.util.List<hydra.ast.Expr>> derivingClause = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(derivCat.get()),
          () -> (java.util.List<hydra.ast.Expr>) (java.util.Collections.<hydra.ast.Expr>emptyList()),
          () -> java.util.Arrays.asList(hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
            hydra.Serialization.cst("deriving"),
            java.util.Arrays.asList(hydra.Serialization.parenList(
              false,
              hydra.lib.lists.Map.apply(
                hydra.haskell.Serde::nameToExpr,
                derivCat.get()))))))));
        hydra.haskell.syntax.DeclarationHead hd = (dataDecl).value.head;
        hydra.haskell.syntax.DataOrNewtype kw = (dataDecl).value.keyword;
        hydra.util.Lazy<java.util.List<hydra.ast.Expr>> mainParts = new hydra.util.Lazy<>(() -> java.util.Arrays.asList(
          hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
            hydra.haskell.Serde.dataOrNewtypeToExpr(kw),
            hydra.lib.lists.Cons.apply(
              hydra.haskell.Serde.declarationHeadToExpr(hd),
              java.util.Arrays.asList(hydra.Serialization.cst("="))))),
          constructors.get()));
        return hydra.Serialization.indentBlock(hydra.lib.lists.Concat2.apply(
          mainParts.get(),
          derivingClause.get()));
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Declaration.Type typeDecl) {
        hydra.haskell.syntax.DeclarationHead hd = (typeDecl).value.name;
        hydra.haskell.syntax.Type typ = (typeDecl).value.type;
        return hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
          hydra.Serialization.cst("type"),
          hydra.lib.lists.Cons.apply(
            hydra.haskell.Serde.declarationHeadToExpr(hd),
            hydra.lib.lists.Cons.apply(
              hydra.Serialization.cst("="),
              java.util.Arrays.asList(hydra.haskell.Serde.typeToExpr(typ))))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Declaration.ValueBinding vb) {
        return hydra.haskell.Serde.valueBindingToExpr((vb).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Declaration.TypedBinding typedBinding) {
        hydra.haskell.syntax.TypeSignature typeSig = (typedBinding).value.typeSignature;
        hydra.haskell.syntax.Type htype = (typeSig).type;
        hydra.haskell.syntax.Name name = (typeSig).name;
        hydra.haskell.syntax.ValueBinding vb = (typedBinding).value.valueBinding;
        return hydra.Serialization.newlineSep(hydra.lib.lists.Cons.apply(
          hydra.Serialization.structuralSpaceSep(java.util.Arrays.asList(
            hydra.haskell.Serde.nameToExpr(name),
            hydra.Serialization.cst("::"),
            hydra.haskell.Serde.typeToExpr(htype))),
          java.util.Arrays.asList(hydra.haskell.Serde.valueBindingToExpr(vb))));
      }
    });
  }

  static hydra.ast.Expr declarationWithCommentsToExpr(hydra.haskell.syntax.DeclarationWithComments declWithComments) {
    hydra.haskell.syntax.Declaration body = (declWithComments).body;
    hydra.util.Maybe<String> mc = (declWithComments).comments;
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.haskell.Serde.declarationToExpr(body),
      (java.util.function.Function<String, hydra.ast.Expr>) (c -> hydra.Serialization.newlineSep(hydra.lib.lists.Cons.apply(
        hydra.Serialization.cst(hydra.haskell.Serde.toHaskellComments(c)),
        java.util.Arrays.asList(hydra.haskell.Serde.declarationToExpr(body))))),
      mc);
  }

  static hydra.ast.Expr expressionToExpr(hydra.haskell.syntax.Expression expr) {
    return (expr).accept(new hydra.haskell.syntax.Expression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Expression.Application app) {
        return hydra.haskell.Serde.applicationExpressionToExpr((app).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Expression.Case cases) {
        return hydra.haskell.Serde.caseExpressionToExpr((cases).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Expression.ConstructRecord r) {
        return hydra.haskell.Serde.constructRecordExpressionToExpr((r).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Expression.Do statements) {
        return hydra.Serialization.indentBlock(hydra.lib.lists.Cons.apply(
          hydra.Serialization.cst("do"),
          hydra.lib.lists.Map.apply(
            hydra.haskell.Serde::statementToExpr,
            (statements).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Expression.If ifte) {
        return hydra.haskell.Serde.ifExpressionToExpr((ifte).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Expression.Literal lit) {
        return hydra.haskell.Serde.literalToExpr((lit).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Expression.Lambda lam) {
        return hydra.Serialization.parenthesize(hydra.haskell.Serde.lambdaExpressionToExpr((lam).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Expression.Let letExpr) {
        java.util.List<hydra.haskell.syntax.LocalBinding> bindings = (letExpr).value.bindings;
        java.util.function.Function<hydra.haskell.syntax.LocalBinding, hydra.ast.Expr> encodeBinding = (java.util.function.Function<hydra.haskell.syntax.LocalBinding, hydra.ast.Expr>) (binding -> hydra.Serialization.indentSubsequentLines(
          "    ",
          hydra.haskell.Serde.localBindingToExpr(binding)));
        hydra.haskell.syntax.Expression inner = (letExpr).value.inner;
        return hydra.Serialization.indentBlock(hydra.lib.lists.Cons.apply(
          hydra.Serialization.cst(""),
          hydra.lib.lists.Cons.apply(
            hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
              hydra.Serialization.cst("let"),
              java.util.Arrays.asList(hydra.Serialization.customIndentBlock(
                "    ",
                hydra.lib.lists.Map.apply(
                  encodeBinding,
                  bindings))))),
            java.util.Arrays.asList(hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
              hydra.Serialization.cst("in"),
              java.util.Arrays.asList(hydra.haskell.Serde.expressionToExpr(inner))))))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Expression.List exprs) {
        return hydra.Serialization.bracketList(
          hydra.Serialization.halfBlockStyle(),
          hydra.lib.lists.Map.apply(
            hydra.haskell.Serde::expressionToExpr,
            (exprs).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Expression.Parens expr_) {
        return hydra.Serialization.parenthesize(hydra.haskell.Serde.expressionToExpr((expr_).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Expression.Tuple exprs) {
        return hydra.Serialization.parenList(
          false,
          hydra.lib.lists.Map.apply(
            hydra.haskell.Serde::expressionToExpr,
            (exprs).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Expression.Variable name) {
        return hydra.haskell.Serde.nameToExpr((name).value);
      }
    });
  }

  static hydra.ast.Expr fieldToExpr(hydra.haskell.syntax.Field field) {
    hydra.haskell.syntax.Name name = (field).name;
    hydra.haskell.syntax.Type typ = (field).type;
    return hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
      hydra.haskell.Serde.nameToExpr(name),
      hydra.lib.lists.Cons.apply(
        hydra.Serialization.cst("::"),
        java.util.Arrays.asList(hydra.haskell.Serde.typeToExpr(typ)))));
  }

  static hydra.ast.Expr fieldWithCommentsToExpr(hydra.haskell.syntax.FieldWithComments fieldWithComments) {
    hydra.haskell.syntax.Field field = (fieldWithComments).field;
    hydra.util.Maybe<String> mc = (fieldWithComments).comments;
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.haskell.Serde.fieldToExpr(field),
      (java.util.function.Function<String, hydra.ast.Expr>) (c -> hydra.Serialization.newlineSep(hydra.lib.lists.Cons.apply(
        hydra.Serialization.cst(hydra.haskell.Serde.toHaskellComments(c)),
        java.util.Arrays.asList(hydra.haskell.Serde.fieldToExpr(field))))),
      mc);
  }

  static hydra.ast.Expr ifExpressionToExpr(hydra.haskell.syntax.IfExpression ifExpr) {
    hydra.haskell.syntax.Expression eelse = (ifExpr).else_;
    hydra.haskell.syntax.Expression ethen = (ifExpr).then;
    hydra.util.Lazy<hydra.ast.Expr> body = new hydra.util.Lazy<>(() -> hydra.Serialization.newlineSep(hydra.lib.lists.Cons.apply(
      hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
        hydra.Serialization.cst("then"),
        java.util.Arrays.asList(hydra.haskell.Serde.expressionToExpr(ethen)))),
      java.util.Arrays.asList(hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
        hydra.Serialization.cst("else"),
        java.util.Arrays.asList(hydra.haskell.Serde.expressionToExpr(eelse))))))));
    hydra.haskell.syntax.Expression eif = (ifExpr).condition;
    hydra.ast.Op ifOp = new hydra.ast.Op(new hydra.ast.Symbol(""), new hydra.ast.Padding(new hydra.ast.Ws.None(), new hydra.ast.Ws.BreakAndIndent("  ")), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None());
    return hydra.Serialization.ifx(
      ifOp,
      hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
        hydra.Serialization.cst("if"),
        java.util.Arrays.asList(hydra.haskell.Serde.expressionToExpr(eif)))),
      body.get());
  }

  static hydra.ast.Expr importExportSpecToExpr(hydra.haskell.syntax.ImportExportSpec spec) {
    return hydra.haskell.Serde.nameToExpr((spec).name);
  }

  static hydra.ast.Expr importToExpr(hydra.haskell.syntax.Import import_) {
    java.util.function.Function<hydra.haskell.syntax.SpecImport, hydra.ast.Expr> hidingSec = (java.util.function.Function<hydra.haskell.syntax.SpecImport, hydra.ast.Expr>) (spec -> (spec).accept(new hydra.haskell.syntax.SpecImport.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.SpecImport.Hiding names) {
        return hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
          hydra.Serialization.cst("hiding "),
          java.util.Arrays.asList(hydra.Serialization.parens(hydra.Serialization.commaSep(
            hydra.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.haskell.Serde::importExportSpecToExpr,
              (names).value))))));
      }
    }));
    hydra.util.Maybe<hydra.haskell.syntax.ModuleName> mod = (import_).as;
    hydra.haskell.syntax.ModuleName modName = (import_).module;
    hydra.util.Maybe<hydra.haskell.syntax.SpecImport> mspec = (import_).spec;
    String name = (modName).value;
    Boolean qual = (import_).qualified;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> parts = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.util.Maybe.just(hydra.Serialization.cst("import")),
      hydra.lib.logic.IfElse.lazy(
        qual,
        () -> hydra.util.Maybe.just(hydra.Serialization.cst("qualified")),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing())),
      hydra.util.Maybe.just(hydra.Serialization.cst(name)),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.haskell.syntax.ModuleName, hydra.ast.Expr>) (m -> hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
          "as ",
          (m).value))),
        mod),
      hydra.lib.maybes.Map.apply(
        hidingSec,
        mspec))));
    return hydra.Serialization.spaceSep(parts.get());
  }

  static hydra.ast.Expr lambdaExpressionToExpr(hydra.haskell.syntax.LambdaExpression lambdaExpr) {
    java.util.List<hydra.haskell.syntax.Pattern> bindings = (lambdaExpr).bindings;
    hydra.haskell.syntax.Expression inner = (lambdaExpr).inner;
    hydra.ast.Expr body = hydra.haskell.Serde.expressionToExpr(inner);
    hydra.util.Lazy<hydra.ast.Expr> head = new hydra.util.Lazy<>(() -> hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
      hydra.haskell.Serde::patternToExpr,
      bindings)));
    return hydra.Serialization.ifx(
      hydra.haskell.Operators.lambdaOp(),
      hydra.Serialization.prefix(
        "\\",
        head.get()),
      body);
  }

  static hydra.ast.Expr literalToExpr(hydra.haskell.syntax.Literal lit) {
    java.util.function.Function<Boolean, java.util.function.Function<String, String>> parensIfNeg = (java.util.function.Function<Boolean, java.util.function.Function<String, String>>) (b -> (java.util.function.Function<String, String>) (e -> hydra.lib.logic.IfElse.lazy(
      b,
      () -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
        "(",
        e,
        ")")),
      () -> e)));
    return hydra.Serialization.cst((lit).accept(new hydra.haskell.syntax.Literal.PartialVisitor<>() {
      @Override
      public String visit(hydra.haskell.syntax.Literal.Char c) {
        return hydra.lib.literals.ShowString.apply(hydra.lib.literals.ShowUint16.apply((c).value));
      }

      @Override
      public String visit(hydra.haskell.syntax.Literal.Double_ d) {
        return hydra.haskell.Serde.literalToExpr_showFloat(
          parensIfNeg,
          (java.util.function.Function<Double, String>) (v -> hydra.lib.literals.ShowFloat64.apply(v)),
          (d).value);
      }

      @Override
      public String visit(hydra.haskell.syntax.Literal.Float_ f) {
        return hydra.haskell.Serde.literalToExpr_showFloat(
          parensIfNeg,
          (java.util.function.Function<Float, String>) (v -> hydra.lib.literals.ShowFloat32.apply(v)),
          (f).value);
      }

      @Override
      public String visit(hydra.haskell.syntax.Literal.Int i) {
        return (parensIfNeg).apply(hydra.lib.equality.Lt.apply(
          (i).value,
          0)).apply(hydra.lib.literals.ShowInt32.apply((i).value));
      }

      @Override
      public String visit(hydra.haskell.syntax.Literal.Integer_ i) {
        return (parensIfNeg).apply(hydra.lib.equality.Lt.apply(
          (i).value,
          new java.math.BigInteger("0"))).apply(hydra.lib.literals.ShowBigint.apply((i).value));
      }

      @Override
      public String visit(hydra.haskell.syntax.Literal.String_ s) {
        return hydra.lib.literals.ShowString.apply((s).value);
      }
    }));
  }

  static <T0> String literalToExpr_showFloat(java.util.function.Function<Boolean, java.util.function.Function<String, String>> parensIfNeg, java.util.function.Function<T0, String> showFn, T0 v) {
    String raw = (showFn).apply(v);
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        raw,
        "NaN"),
      () -> "(0/0)",
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          raw,
          "Infinity"),
        () -> "(1/0)",
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            raw,
            "-Infinity"),
          () -> "(-(1/0))",
          () -> (parensIfNeg).apply(hydra.lib.equality.Equal.apply(
            hydra.lib.strings.CharAt.apply(
              0,
              raw),
            45)).apply(raw))));
  }

  static hydra.ast.Expr localBindingToExpr(hydra.haskell.syntax.LocalBinding binding) {
    return (binding).accept(new hydra.haskell.syntax.LocalBinding.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.LocalBinding.Signature ts) {
        return hydra.haskell.Serde.typeSignatureToExpr((ts).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.LocalBinding.Value vb) {
        return hydra.haskell.Serde.valueBindingToExpr((vb).value);
      }
    });
  }

  static hydra.ast.Expr moduleHeadToExpr(hydra.haskell.syntax.ModuleHead moduleHead) {
    hydra.haskell.syntax.ModuleName modName = (moduleHead).name;
    String mname = (modName).value;
    hydra.util.Lazy<hydra.ast.Expr> head = new hydra.util.Lazy<>(() -> hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
      hydra.Serialization.cst("module"),
      hydra.lib.lists.Cons.apply(
        hydra.Serialization.cst(mname),
        java.util.Arrays.asList(hydra.Serialization.cst("where"))))));
    hydra.util.Maybe<String> mc = (moduleHead).comments;
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> head.get(),
      (java.util.function.Function<String, hydra.ast.Expr>) (c -> hydra.Serialization.newlineSep(hydra.lib.lists.Cons.apply(
        hydra.Serialization.cst(hydra.haskell.Serde.toHaskellComments(c)),
        hydra.lib.lists.Cons.apply(
          hydra.Serialization.cst(""),
          java.util.Arrays.asList(head.get()))))),
      mc);
  }

  static hydra.ast.Expr moduleToExpr(hydra.haskell.syntax.Module module) {
    java.util.List<hydra.haskell.syntax.DeclarationWithComments> decls = (module).declarations;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> declLines = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      hydra.haskell.Serde::declarationWithCommentsToExpr,
      decls));
    hydra.util.Maybe<hydra.haskell.syntax.ModuleHead> mh = (module).head;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> headerLine = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (java.util.List<hydra.ast.Expr>) (java.util.Collections.<hydra.ast.Expr>emptyList()),
      (java.util.function.Function<hydra.haskell.syntax.ModuleHead, java.util.List<hydra.ast.Expr>>) (h -> java.util.Arrays.asList(hydra.haskell.Serde.moduleHeadToExpr(h))),
      mh));
    java.util.List<hydra.haskell.syntax.Import> imports = (module).imports;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> importLines = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(imports),
      () -> (java.util.List<hydra.ast.Expr>) (java.util.Collections.<hydra.ast.Expr>emptyList()),
      () -> java.util.Arrays.asList(hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
        hydra.haskell.Serde::importToExpr,
        imports)))));
    java.util.List<hydra.ast.Expr> warning = java.util.Arrays.asList(hydra.Serialization.cst(hydra.haskell.Serde.toSimpleComments(hydra.Constants.warningAutoGeneratedFile())));
    return hydra.Serialization.doubleNewlineSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
      warning,
      headerLine.get(),
      importLines.get(),
      declLines.get())));
  }

  static hydra.ast.Expr nameToExpr(hydra.haskell.syntax.Name name) {
    return hydra.Serialization.cst((name).accept(new hydra.haskell.syntax.Name.PartialVisitor<>() {
      @Override
      public String visit(hydra.haskell.syntax.Name.Implicit qn) {
        return hydra.lib.strings.Cat2.apply(
          "?",
          hydra.haskell.Serde.writeQualifiedName((qn).value));
      }

      @Override
      public String visit(hydra.haskell.syntax.Name.Normal qn) {
        return hydra.haskell.Serde.writeQualifiedName((qn).value);
      }

      @Override
      public String visit(hydra.haskell.syntax.Name.Parens qn) {
        return hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "(",
          hydra.haskell.Serde.writeQualifiedName((qn).value),
          ")"));
      }
    }));
  }

  static hydra.ast.Expr patternToExpr(hydra.haskell.syntax.Pattern pat) {
    return (pat).accept(new hydra.haskell.syntax.Pattern.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Pattern.Application app) {
        return hydra.haskell.Serde.applicationPatternToExpr((app).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Pattern.List pats) {
        return hydra.Serialization.bracketList(
          hydra.Serialization.halfBlockStyle(),
          hydra.lib.lists.Map.apply(
            hydra.haskell.Serde::patternToExpr,
            (pats).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Pattern.Literal lit) {
        return hydra.haskell.Serde.literalToExpr((lit).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Pattern.Name name) {
        return hydra.haskell.Serde.nameToExpr((name).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Pattern.Parens pat_) {
        return hydra.Serialization.parenthesize(hydra.haskell.Serde.patternToExpr((pat_).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Pattern.Tuple pats) {
        return hydra.Serialization.parenList(
          false,
          hydra.lib.lists.Map.apply(
            hydra.haskell.Serde::patternToExpr,
            (pats).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Pattern.Wildcard ignored) {
        return hydra.Serialization.cst("_");
      }
    });
  }

  static hydra.ast.Expr rightHandSideToExpr(hydra.haskell.syntax.RightHandSide rhs) {
    return hydra.haskell.Serde.expressionToExpr((rhs).value);
  }

  static hydra.ast.Expr statementToExpr(hydra.haskell.syntax.Statement stmt) {
    return hydra.haskell.Serde.expressionToExpr((stmt).value);
  }

  static String toHaskellComments(String c) {
    return hydra.lib.strings.Intercalate.apply(
      "\n",
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<String, String>) (s -> hydra.lib.strings.Cat2.apply(
          "-- | ",
          s)),
        hydra.lib.strings.Lines.apply(c)));
  }

  static String toSimpleComments(String c) {
    return hydra.lib.strings.Intercalate.apply(
      "\n",
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<String, String>) (s -> hydra.lib.strings.Cat2.apply(
          "-- ",
          s)),
        hydra.lib.strings.Lines.apply(c)));
  }

  static hydra.ast.Expr typeSignatureToExpr(hydra.haskell.syntax.TypeSignature typeSig) {
    hydra.haskell.syntax.Name name = (typeSig).name;
    hydra.ast.Expr nameExpr = hydra.haskell.Serde.nameToExpr(name);
    hydra.haskell.syntax.Type typ = (typeSig).type;
    hydra.ast.Expr typeExpr = hydra.haskell.Serde.typeToExpr(typ);
    hydra.ast.Expr inlineSig = hydra.Serialization.structuralSpaceSep(java.util.Arrays.asList(
      nameExpr,
      hydra.Serialization.cst("::"),
      typeExpr));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Gt.apply(
        hydra.Serialization.expressionLength(inlineSig),
        120),
      () -> hydra.Serialization.newlineSep(java.util.Arrays.asList(
        hydra.Serialization.spaceSep(java.util.Arrays.asList(
          nameExpr,
          hydra.Serialization.cst("::"))),
        hydra.Serialization.tabIndent(typeExpr))),
      () -> inlineSig);
  }

  static hydra.ast.Expr typeToExpr(hydra.haskell.syntax.Type htype) {
    return (htype).accept(new hydra.haskell.syntax.Type.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Type.Application appType) {
        hydra.haskell.syntax.Type lhs = (appType).value.context;
        hydra.haskell.syntax.Type rhs = (appType).value.argument;
        return hydra.Serialization.ifx(
          hydra.haskell.Operators.appOp(),
          hydra.haskell.Serde.typeToExpr(lhs),
          hydra.haskell.Serde.typeToExpr(rhs));
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Type.Ctx ctxType) {
        hydra.haskell.syntax.Assertion ctx = (ctxType).value.ctx;
        hydra.haskell.syntax.Type typ = (ctxType).value.type;
        return hydra.Serialization.ifx(
          hydra.haskell.Operators.assertOp(),
          hydra.haskell.Serde.assertionToExpr(ctx),
          hydra.haskell.Serde.typeToExpr(typ));
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Type.Function funType) {
        hydra.haskell.syntax.Type cod = (funType).value.codomain;
        hydra.haskell.syntax.Type dom = (funType).value.domain;
        return hydra.Serialization.ifx(
          hydra.haskell.Operators.arrowOp(),
          hydra.haskell.Serde.typeToExpr(dom),
          hydra.haskell.Serde.typeToExpr(cod));
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Type.List htype_) {
        return hydra.Serialization.bracketList(
          hydra.Serialization.inlineStyle(),
          java.util.Arrays.asList(hydra.haskell.Serde.typeToExpr((htype_).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Type.Tuple types) {
        return hydra.Serialization.parenList(
          false,
          hydra.lib.lists.Map.apply(
            hydra.haskell.Serde::typeToExpr,
            (types).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.Type.Variable name) {
        return hydra.haskell.Serde.nameToExpr((name).value);
      }
    });
  }

  static hydra.ast.Expr valueBindingToExpr(hydra.haskell.syntax.ValueBinding vb) {
    return (vb).accept(new hydra.haskell.syntax.ValueBinding.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.haskell.syntax.ValueBinding.Simple simpleVB) {
        hydra.haskell.syntax.Pattern pat = (simpleVB).value.pattern;
        hydra.ast.Expr lhsExpr = hydra.haskell.Serde.patternToExpr(pat);
        hydra.haskell.syntax.RightHandSide rhs = (simpleVB).value.rhs;
        hydra.ast.Expr rhsExpr = hydra.haskell.Serde.rightHandSideToExpr(rhs);
        hydra.ast.Expr inlineBody = hydra.Serialization.structuralSpaceSep(java.util.Arrays.asList(
          lhsExpr,
          hydra.Serialization.cst("="),
          rhsExpr));
        hydra.util.Lazy<hydra.ast.Expr> body = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Gt.apply(
            hydra.Serialization.expressionLength(inlineBody),
            120),
          () -> hydra.Serialization.newlineSep(java.util.Arrays.asList(
            hydra.Serialization.spaceSep(java.util.Arrays.asList(
              lhsExpr,
              hydra.Serialization.cst("="))),
            hydra.Serialization.tabIndent(rhsExpr))),
          () -> inlineBody));
        hydra.util.Maybe<hydra.haskell.syntax.LocalBindings> local = (simpleVB).value.localBindings;
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> body.get(),
          (java.util.function.Function<hydra.haskell.syntax.LocalBindings, hydra.ast.Expr>) (localBindings -> {
            java.util.List<hydra.haskell.syntax.LocalBinding> bindings = (localBindings).value;
            return hydra.Serialization.indentBlock(hydra.lib.lists.Cons.apply(
              body.get(),
              java.util.Arrays.asList(hydra.Serialization.indentBlock(hydra.lib.lists.Cons.apply(
                hydra.Serialization.cst("where"),
                hydra.lib.lists.Map.apply(
                  hydra.haskell.Serde::localBindingToExpr,
                  bindings))))));
          }),
          local);
      }
    });
  }

  static hydra.ast.Expr variableToExpr(hydra.haskell.syntax.Variable variable) {
    return hydra.haskell.Serde.nameToExpr((variable).value);
  }

  static String writeQualifiedName(hydra.haskell.syntax.QualifiedName qname) {
    java.util.function.Function<hydra.haskell.syntax.NamePart, String> h = (java.util.function.Function<hydra.haskell.syntax.NamePart, String>) (namePart -> (namePart).value);
    java.util.List<hydra.haskell.syntax.NamePart> qualifiers = (qname).qualifiers;
    hydra.haskell.syntax.NamePart unqual = (qname).unqualified;
    hydra.util.Lazy<java.util.List<String>> allParts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
      hydra.lib.lists.Map.apply(
        h,
        qualifiers),
      java.util.Arrays.asList((h).apply(unqual))));
    return hydra.lib.strings.Intercalate.apply(
      ".",
      allParts.get());
  }
}
