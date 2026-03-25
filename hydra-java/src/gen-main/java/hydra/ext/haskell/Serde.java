// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell;

/**
 * Haskell operator precendence and associativity are drawn from:
 * https://self-learning-java-tutorial.blogspot.com/2016/04/haskell-operator-precedence.html
 * Other operators were investigated using GHCi, e.g. ":info (-&gt;)"
 * Operator names are drawn (loosely) from:
 * https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators
 */
public interface Serde {
  static hydra.ast.Expr alternativeToExpr(hydra.ext.haskell.syntax.Alternative alt) {
    return hydra.Serialization.structuralSpaceSep(hydra.util.ConsList.of(
      hydra.ext.haskell.Serde.patternToExpr((alt).pattern),
      hydra.Serialization.cst("->"),
      hydra.ext.haskell.Serde.caseRhsToExpr((alt).rhs)));
  }

  static hydra.ast.Expr applicationExpressionToExpr(hydra.ext.haskell.syntax.ApplicationExpression app) {
    return hydra.Serialization.ifx(
      hydra.ext.haskell.Operators.appOp(),
      hydra.ext.haskell.Serde.expressionToExpr((app).function),
      hydra.ext.haskell.Serde.expressionToExpr((app).argument));
  }

  static hydra.ast.Expr applicationPatternToExpr(hydra.ext.haskell.syntax.ApplicationPattern appPat) {
    hydra.ext.haskell.syntax.Name name = (appPat).name;
    hydra.util.ConsList<hydra.ext.haskell.syntax.Pattern> pats = (appPat).args;
    return hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
      hydra.ext.haskell.Serde.nameToExpr(name),
      hydra.lib.lists.Map.apply(
        hydra.ext.haskell.Serde::patternToExpr,
        pats)));
  }

  static hydra.ast.Expr assertionToExpr(hydra.ext.haskell.syntax.Assertion sert) {
    return (sert).accept(new hydra.ext.haskell.syntax.Assertion.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Assertion.Class_ cls) {
        return hydra.ext.haskell.Serde.classAssertionToExpr((cls).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Assertion.Tuple serts) {
        return hydra.Serialization.parenList(
          false,
          hydra.lib.lists.Map.apply(
            hydra.ext.haskell.Serde::assertionToExpr,
            (serts).value));
      }
    });
  }

  static hydra.ast.Expr caseExpressionToExpr(hydra.ext.haskell.syntax.CaseExpression caseExpr) {
    hydra.util.ConsList<hydra.ext.haskell.syntax.Alternative> alts = (caseExpr).alternatives;
    hydra.ext.haskell.syntax.Expression cs = (caseExpr).case_;
    hydra.ast.Expr lhs = hydra.Serialization.spaceSep(hydra.util.ConsList.of(
      hydra.Serialization.cst("case"),
      hydra.ext.haskell.Serde.expressionToExpr(cs)));
    hydra.ast.Op ofOp = new hydra.ast.Op(new hydra.ast.Symbol("of"), new hydra.ast.Padding(new hydra.ast.Ws.Space(), new hydra.ast.Ws.BreakAndIndent("  ")), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None());
    hydra.util.Lazy<hydra.ast.Expr> rhs = new hydra.util.Lazy<>(() -> hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
      hydra.ext.haskell.Serde::alternativeToExpr,
      alts)));
    return hydra.Serialization.ifx(
      ofOp,
      lhs,
      rhs.get());
  }

  static hydra.ast.Expr caseRhsToExpr(hydra.ext.haskell.syntax.CaseRhs rhs) {
    return hydra.ext.haskell.Serde.expressionToExpr((rhs).value);
  }

  static hydra.ast.Expr classAssertionToExpr(hydra.ext.haskell.syntax.ClassAssertion clsAsrt) {
    hydra.ext.haskell.syntax.Name name = (clsAsrt).name;
    hydra.util.ConsList<hydra.ext.haskell.syntax.Type> types = (clsAsrt).types;
    return hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
      hydra.ext.haskell.Serde.nameToExpr(name),
      hydra.util.ConsList.of(hydra.Serialization.commaSep(
        hydra.Serialization.halfBlockStyle(),
        hydra.lib.lists.Map.apply(
          hydra.ext.haskell.Serde::typeToExpr,
          types)))));
  }

  static hydra.ast.Expr constructRecordExpressionToExpr(hydra.ext.haskell.syntax.ConstructRecordExpression constructRecord) {
    java.util.function.Function<hydra.ext.haskell.syntax.FieldUpdate, hydra.ast.Expr> fromUpdate = (java.util.function.Function<hydra.ext.haskell.syntax.FieldUpdate, hydra.ast.Expr>) (update -> {
      hydra.ext.haskell.syntax.Name fn = (update).name;
      hydra.ext.haskell.syntax.Expression val = (update).value;
      return hydra.Serialization.ifx(
        hydra.ext.haskell.Operators.defineOp(),
        hydra.ext.haskell.Serde.nameToExpr(fn),
        hydra.ext.haskell.Serde.expressionToExpr(val));
    });
    hydra.util.ConsList<hydra.ext.haskell.syntax.FieldUpdate> updates = (constructRecord).fields;
    hydra.util.Lazy<hydra.ast.Expr> body = new hydra.util.Lazy<>(() -> hydra.Serialization.commaSep(
      hydra.Serialization.halfBlockStyle(),
      hydra.lib.lists.Map.apply(
        fromUpdate,
        updates)));
    hydra.ext.haskell.syntax.Name name = (constructRecord).name;
    return hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
      hydra.ext.haskell.Serde.nameToExpr(name),
      hydra.util.ConsList.of(hydra.Serialization.brackets(
        hydra.Serialization.curlyBraces(),
        hydra.Serialization.halfBlockStyle(),
        body.get()))));
  }

  static hydra.ast.Expr constructorToExpr(hydra.ext.haskell.syntax.Constructor cons) {
    return (cons).accept(new hydra.ext.haskell.syntax.Constructor.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Constructor.Ordinary ord) {
        hydra.ext.haskell.syntax.Name name = (ord).value.name;
        hydra.util.ConsList<hydra.ext.haskell.syntax.Type> types = (ord).value.fields;
        return hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
          hydra.ext.haskell.Serde.nameToExpr(name),
          hydra.util.ConsList.of(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
            hydra.ext.haskell.Serde::typeToExpr,
            types)))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Constructor.Record rec) {
        hydra.util.ConsList<hydra.ext.haskell.syntax.FieldWithComments> fields = (rec).value.fields;
        hydra.ext.haskell.syntax.Name name = (rec).value.name;
        return hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
          hydra.ext.haskell.Serde.nameToExpr(name),
          hydra.util.ConsList.of(hydra.Serialization.curlyBracesList(
            (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
            hydra.Serialization.halfBlockStyle(),
            hydra.lib.lists.Map.apply(
              hydra.ext.haskell.Serde::fieldWithCommentsToExpr,
              fields)))));
      }
    });
  }

  static hydra.ast.Expr constructorWithCommentsToExpr(hydra.ext.haskell.syntax.ConstructorWithComments consWithComments) {
    hydra.ext.haskell.syntax.Constructor body = (consWithComments).body;
    hydra.util.Maybe<String> mc = (consWithComments).comments;
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.ext.haskell.Serde.constructorToExpr(body),
      (java.util.function.Function<String, hydra.ast.Expr>) (c -> hydra.Serialization.newlineSep(hydra.lib.lists.Cons.apply(
        hydra.Serialization.cst(hydra.ext.haskell.Serde.toHaskellComments(c)),
        hydra.util.ConsList.of(hydra.ext.haskell.Serde.constructorToExpr(body))))),
      mc);
  }

  static hydra.ast.Expr dataOrNewtypeToExpr(hydra.ext.haskell.syntax.DataOrNewtype kw) {
    return (kw).accept(new hydra.ext.haskell.syntax.DataOrNewtype.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.DataOrNewtype.Data ignored) {
        return hydra.Serialization.cst("data");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.DataOrNewtype.Newtype ignored) {
        return hydra.Serialization.cst("newtype");
      }
    });
  }

  static hydra.ast.Expr declarationHeadToExpr(hydra.ext.haskell.syntax.DeclarationHead hd) {
    return (hd).accept(new hydra.ext.haskell.syntax.DeclarationHead.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.DeclarationHead.Application appHead) {
        hydra.ext.haskell.syntax.DeclarationHead fun = (appHead).value.function;
        hydra.ext.haskell.syntax.Variable op = (appHead).value.operand;
        return hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
          hydra.ext.haskell.Serde.declarationHeadToExpr(fun),
          hydra.util.ConsList.of(hydra.ext.haskell.Serde.variableToExpr(op))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.DeclarationHead.Simple name) {
        return hydra.ext.haskell.Serde.nameToExpr((name).value);
      }
    });
  }

  static hydra.ast.Expr declarationToExpr(hydra.ext.haskell.syntax.Declaration decl) {
    return (decl).accept(new hydra.ext.haskell.syntax.Declaration.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Declaration.Data dataDecl) {
        hydra.util.ConsList<hydra.ext.haskell.syntax.ConstructorWithComments> cons = (dataDecl).value.constructors;
        hydra.util.Lazy<hydra.ast.Expr> constructors = new hydra.util.Lazy<>(() -> hydra.Serialization.orSep(
          hydra.Serialization.halfBlockStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.haskell.Serde::constructorWithCommentsToExpr,
            cons)));
        hydra.util.ConsList<hydra.ext.haskell.syntax.Deriving> deriv = (dataDecl).value.deriving;
        hydra.util.Lazy<hydra.util.ConsList<hydra.ext.haskell.syntax.Name>> derivCat = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          wrapped -> (wrapped).value,
          deriv)));
        hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> derivingClause = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(derivCat.get()),
          () -> (hydra.util.ConsList<hydra.ast.Expr>) (hydra.util.ConsList.<hydra.ast.Expr>empty()),
          () -> hydra.util.ConsList.of(hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
            hydra.Serialization.cst("deriving"),
            hydra.util.ConsList.of(hydra.Serialization.parenList(
              false,
              hydra.lib.lists.Map.apply(
                hydra.ext.haskell.Serde::nameToExpr,
                derivCat.get()))))))));
        hydra.ext.haskell.syntax.DeclarationHead hd = (dataDecl).value.head;
        hydra.ext.haskell.syntax.DataOrNewtype kw = (dataDecl).value.keyword;
        hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> mainParts = new hydra.util.Lazy<>(() -> hydra.util.ConsList.of(
          hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
            hydra.ext.haskell.Serde.dataOrNewtypeToExpr(kw),
            hydra.lib.lists.Cons.apply(
              hydra.ext.haskell.Serde.declarationHeadToExpr(hd),
              hydra.util.ConsList.of(hydra.Serialization.cst("="))))),
          constructors.get()));
        return hydra.Serialization.indentBlock(hydra.lib.lists.Concat2.apply(
          mainParts.get(),
          derivingClause.get()));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Declaration.Type typeDecl) {
        hydra.ext.haskell.syntax.DeclarationHead hd = (typeDecl).value.name;
        hydra.ext.haskell.syntax.Type typ = (typeDecl).value.type;
        return hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
          hydra.Serialization.cst("type"),
          hydra.lib.lists.Cons.apply(
            hydra.ext.haskell.Serde.declarationHeadToExpr(hd),
            hydra.lib.lists.Cons.apply(
              hydra.Serialization.cst("="),
              hydra.util.ConsList.of(hydra.ext.haskell.Serde.typeToExpr(typ))))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Declaration.ValueBinding vb) {
        return hydra.ext.haskell.Serde.valueBindingToExpr((vb).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Declaration.TypedBinding typedBinding) {
        hydra.ext.haskell.syntax.TypeSignature typeSig = (typedBinding).value.typeSignature;
        hydra.ext.haskell.syntax.Type htype = (typeSig).type;
        hydra.ext.haskell.syntax.Name name = (typeSig).name;
        hydra.ext.haskell.syntax.ValueBinding vb = (typedBinding).value.valueBinding;
        return hydra.Serialization.newlineSep(hydra.lib.lists.Cons.apply(
          hydra.Serialization.structuralSpaceSep(hydra.util.ConsList.of(
            hydra.ext.haskell.Serde.nameToExpr(name),
            hydra.Serialization.cst("::"),
            hydra.ext.haskell.Serde.typeToExpr(htype))),
          hydra.util.ConsList.of(hydra.ext.haskell.Serde.valueBindingToExpr(vb))));
      }
    });
  }

  static hydra.ast.Expr declarationWithCommentsToExpr(hydra.ext.haskell.syntax.DeclarationWithComments declWithComments) {
    hydra.ext.haskell.syntax.Declaration body = (declWithComments).body;
    hydra.util.Maybe<String> mc = (declWithComments).comments;
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.ext.haskell.Serde.declarationToExpr(body),
      (java.util.function.Function<String, hydra.ast.Expr>) (c -> hydra.Serialization.newlineSep(hydra.lib.lists.Cons.apply(
        hydra.Serialization.cst(hydra.ext.haskell.Serde.toHaskellComments(c)),
        hydra.util.ConsList.of(hydra.ext.haskell.Serde.declarationToExpr(body))))),
      mc);
  }

  static hydra.ast.Expr expressionToExpr(hydra.ext.haskell.syntax.Expression expr) {
    return (expr).accept(new hydra.ext.haskell.syntax.Expression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Expression.Application app) {
        return hydra.ext.haskell.Serde.applicationExpressionToExpr((app).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Expression.Case cases) {
        return hydra.ext.haskell.Serde.caseExpressionToExpr((cases).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Expression.ConstructRecord r) {
        return hydra.ext.haskell.Serde.constructRecordExpressionToExpr((r).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Expression.Do statements) {
        return hydra.Serialization.indentBlock(hydra.lib.lists.Cons.apply(
          hydra.Serialization.cst("do"),
          hydra.lib.lists.Map.apply(
            hydra.ext.haskell.Serde::statementToExpr,
            (statements).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Expression.If ifte) {
        return hydra.ext.haskell.Serde.ifExpressionToExpr((ifte).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Expression.Literal lit) {
        return hydra.ext.haskell.Serde.literalToExpr((lit).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Expression.Lambda lam) {
        return hydra.Serialization.parenthesize(hydra.ext.haskell.Serde.lambdaExpressionToExpr((lam).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Expression.Let letExpr) {
        hydra.util.ConsList<hydra.ext.haskell.syntax.LocalBinding> bindings = (letExpr).value.bindings;
        java.util.function.Function<hydra.ext.haskell.syntax.LocalBinding, hydra.ast.Expr> encodeBinding = (java.util.function.Function<hydra.ext.haskell.syntax.LocalBinding, hydra.ast.Expr>) (binding -> hydra.Serialization.indentSubsequentLines(
          "    ",
          hydra.ext.haskell.Serde.localBindingToExpr(binding)));
        hydra.ext.haskell.syntax.Expression inner = (letExpr).value.inner;
        return hydra.Serialization.indentBlock(hydra.lib.lists.Cons.apply(
          hydra.Serialization.cst(""),
          hydra.lib.lists.Cons.apply(
            hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
              hydra.Serialization.cst("let"),
              hydra.util.ConsList.of(hydra.Serialization.customIndentBlock(
                "    ",
                hydra.lib.lists.Map.apply(
                  encodeBinding,
                  bindings))))),
            hydra.util.ConsList.of(hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
              hydra.Serialization.cst("in"),
              hydra.util.ConsList.of(hydra.ext.haskell.Serde.expressionToExpr(inner))))))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Expression.List exprs) {
        return hydra.Serialization.bracketList(
          hydra.Serialization.halfBlockStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.haskell.Serde::expressionToExpr,
            (exprs).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Expression.Parens expr_) {
        return hydra.Serialization.parenthesize(hydra.ext.haskell.Serde.expressionToExpr((expr_).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Expression.Tuple exprs) {
        return hydra.Serialization.parenList(
          false,
          hydra.lib.lists.Map.apply(
            hydra.ext.haskell.Serde::expressionToExpr,
            (exprs).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Expression.Variable name) {
        return hydra.ext.haskell.Serde.nameToExpr((name).value);
      }
    });
  }

  static hydra.ast.Expr fieldToExpr(hydra.ext.haskell.syntax.Field field) {
    hydra.ext.haskell.syntax.Name name = (field).name;
    hydra.ext.haskell.syntax.Type typ = (field).type;
    return hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
      hydra.ext.haskell.Serde.nameToExpr(name),
      hydra.lib.lists.Cons.apply(
        hydra.Serialization.cst("::"),
        hydra.util.ConsList.of(hydra.ext.haskell.Serde.typeToExpr(typ)))));
  }

  static hydra.ast.Expr fieldWithCommentsToExpr(hydra.ext.haskell.syntax.FieldWithComments fieldWithComments) {
    hydra.ext.haskell.syntax.Field field = (fieldWithComments).field;
    hydra.util.Maybe<String> mc = (fieldWithComments).comments;
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.ext.haskell.Serde.fieldToExpr(field),
      (java.util.function.Function<String, hydra.ast.Expr>) (c -> hydra.Serialization.newlineSep(hydra.lib.lists.Cons.apply(
        hydra.Serialization.cst(hydra.ext.haskell.Serde.toHaskellComments(c)),
        hydra.util.ConsList.of(hydra.ext.haskell.Serde.fieldToExpr(field))))),
      mc);
  }

  static hydra.ast.Expr ifExpressionToExpr(hydra.ext.haskell.syntax.IfExpression ifExpr) {
    hydra.ext.haskell.syntax.Expression eelse = (ifExpr).else_;
    hydra.ext.haskell.syntax.Expression ethen = (ifExpr).then;
    hydra.util.Lazy<hydra.ast.Expr> body = new hydra.util.Lazy<>(() -> hydra.Serialization.newlineSep(hydra.lib.lists.Cons.apply(
      hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
        hydra.Serialization.cst("then"),
        hydra.util.ConsList.of(hydra.ext.haskell.Serde.expressionToExpr(ethen)))),
      hydra.util.ConsList.of(hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
        hydra.Serialization.cst("else"),
        hydra.util.ConsList.of(hydra.ext.haskell.Serde.expressionToExpr(eelse))))))));
    hydra.ext.haskell.syntax.Expression eif = (ifExpr).condition;
    hydra.ast.Op ifOp = new hydra.ast.Op(new hydra.ast.Symbol(""), new hydra.ast.Padding(new hydra.ast.Ws.None(), new hydra.ast.Ws.BreakAndIndent("  ")), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None());
    return hydra.Serialization.ifx(
      ifOp,
      hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
        hydra.Serialization.cst("if"),
        hydra.util.ConsList.of(hydra.ext.haskell.Serde.expressionToExpr(eif)))),
      body.get());
  }

  static hydra.ast.Expr importExportSpecToExpr(hydra.ext.haskell.syntax.ImportExportSpec spec) {
    return hydra.ext.haskell.Serde.nameToExpr((spec).name);
  }

  static hydra.ast.Expr importToExpr(hydra.ext.haskell.syntax.Import import_) {
    java.util.function.Function<hydra.ext.haskell.syntax.SpecImport, hydra.ast.Expr> hidingSec = (java.util.function.Function<hydra.ext.haskell.syntax.SpecImport, hydra.ast.Expr>) (spec -> (spec).accept(new hydra.ext.haskell.syntax.SpecImport.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.SpecImport.Hiding names) {
        return hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
          hydra.Serialization.cst("hiding "),
          hydra.util.ConsList.of(hydra.Serialization.parens(hydra.Serialization.commaSep(
            hydra.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.ext.haskell.Serde::importExportSpecToExpr,
              (names).value))))));
      }
    }));
    hydra.util.Maybe<hydra.ext.haskell.syntax.ModuleName> mod = (import_).as;
    hydra.ext.haskell.syntax.ModuleName modName = (import_).module;
    hydra.util.Maybe<hydra.ext.haskell.syntax.SpecImport> mspec = (import_).spec;
    String name = (modName).value;
    Boolean qual = (import_).qualified;
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> parts = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.util.Maybe.just(hydra.Serialization.cst("import")),
      hydra.lib.logic.IfElse.lazy(
        qual,
        () -> hydra.util.Maybe.just(hydra.Serialization.cst("qualified")),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing())),
      hydra.util.Maybe.just(hydra.Serialization.cst(name)),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.ext.haskell.syntax.ModuleName, hydra.ast.Expr>) (m -> hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
          "as ",
          (m).value))),
        mod),
      hydra.lib.maybes.Map.apply(
        hidingSec,
        mspec))));
    return hydra.Serialization.spaceSep(parts.get());
  }

  static hydra.ast.Expr lambdaExpressionToExpr(hydra.ext.haskell.syntax.LambdaExpression lambdaExpr) {
    hydra.util.ConsList<hydra.ext.haskell.syntax.Pattern> bindings = (lambdaExpr).bindings;
    hydra.ext.haskell.syntax.Expression inner = (lambdaExpr).inner;
    hydra.ast.Expr body = hydra.ext.haskell.Serde.expressionToExpr(inner);
    hydra.util.Lazy<hydra.ast.Expr> head = new hydra.util.Lazy<>(() -> hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
      hydra.ext.haskell.Serde::patternToExpr,
      bindings)));
    return hydra.Serialization.ifx(
      hydra.ext.haskell.Operators.lambdaOp(),
      hydra.Serialization.prefix(
        "\\",
        head.get()),
      body);
  }

  static hydra.ast.Expr literalToExpr(hydra.ext.haskell.syntax.Literal lit) {
    java.util.function.Function<Boolean, java.util.function.Function<String, String>> parensIfNeg = (java.util.function.Function<Boolean, java.util.function.Function<String, String>>) (b -> (java.util.function.Function<String, String>) (e -> hydra.lib.logic.IfElse.lazy(
      b,
      () -> hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
        "(",
        e,
        ")")),
      () -> e)));
    return hydra.Serialization.cst((lit).accept(new hydra.ext.haskell.syntax.Literal.PartialVisitor<>() {
      @Override
      public String visit(hydra.ext.haskell.syntax.Literal.Char c) {
        return hydra.lib.literals.ShowString.apply(hydra.lib.literals.ShowUint16.apply((c).value));
      }

      @Override
      public String visit(hydra.ext.haskell.syntax.Literal.Double_ d) {
        return (parensIfNeg).apply(hydra.lib.equality.Lt.apply(
          (d).value,
          0.0)).apply(hydra.lib.literals.ShowFloat64.apply((d).value));
      }

      @Override
      public String visit(hydra.ext.haskell.syntax.Literal.Float_ f) {
        return (parensIfNeg).apply(hydra.lib.equality.Lt.apply(
          (f).value,
          (float) (0.0))).apply(hydra.lib.literals.ShowFloat32.apply((f).value));
      }

      @Override
      public String visit(hydra.ext.haskell.syntax.Literal.Int i) {
        return (parensIfNeg).apply(hydra.lib.equality.Lt.apply(
          (i).value,
          0)).apply(hydra.lib.literals.ShowInt32.apply((i).value));
      }

      @Override
      public String visit(hydra.ext.haskell.syntax.Literal.Integer_ i) {
        return (parensIfNeg).apply(hydra.lib.equality.Lt.apply(
          (i).value,
          new java.math.BigInteger("0"))).apply(hydra.lib.literals.ShowBigint.apply((i).value));
      }

      @Override
      public String visit(hydra.ext.haskell.syntax.Literal.String_ s) {
        return hydra.lib.literals.ShowString.apply((s).value);
      }
    }));
  }

  static hydra.ast.Expr localBindingToExpr(hydra.ext.haskell.syntax.LocalBinding binding) {
    return (binding).accept(new hydra.ext.haskell.syntax.LocalBinding.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.LocalBinding.Signature ts) {
        return hydra.ext.haskell.Serde.typeSignatureToExpr((ts).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.LocalBinding.Value vb) {
        return hydra.ext.haskell.Serde.valueBindingToExpr((vb).value);
      }
    });
  }

  static hydra.ast.Expr moduleHeadToExpr(hydra.ext.haskell.syntax.ModuleHead moduleHead) {
    hydra.ext.haskell.syntax.ModuleName modName = (moduleHead).name;
    String mname = (modName).value;
    hydra.util.Lazy<hydra.ast.Expr> head = new hydra.util.Lazy<>(() -> hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
      hydra.Serialization.cst("module"),
      hydra.lib.lists.Cons.apply(
        hydra.Serialization.cst(mname),
        hydra.util.ConsList.of(hydra.Serialization.cst("where"))))));
    hydra.util.Maybe<String> mc = (moduleHead).comments;
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> head.get(),
      (java.util.function.Function<String, hydra.ast.Expr>) (c -> hydra.Serialization.newlineSep(hydra.lib.lists.Cons.apply(
        hydra.Serialization.cst(hydra.ext.haskell.Serde.toHaskellComments(c)),
        hydra.lib.lists.Cons.apply(
          hydra.Serialization.cst(""),
          hydra.util.ConsList.of(head.get()))))),
      mc);
  }

  static hydra.ast.Expr moduleToExpr(hydra.ext.haskell.syntax.Module module) {
    hydra.util.ConsList<hydra.ext.haskell.syntax.DeclarationWithComments> decls = (module).declarations;
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> declLines = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      hydra.ext.haskell.Serde::declarationWithCommentsToExpr,
      decls));
    hydra.util.Maybe<hydra.ext.haskell.syntax.ModuleHead> mh = (module).head;
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> headerLine = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (hydra.util.ConsList<hydra.ast.Expr>) (hydra.util.ConsList.<hydra.ast.Expr>empty()),
      (java.util.function.Function<hydra.ext.haskell.syntax.ModuleHead, hydra.util.ConsList<hydra.ast.Expr>>) (h -> hydra.util.ConsList.of(hydra.ext.haskell.Serde.moduleHeadToExpr(h))),
      mh));
    hydra.util.ConsList<hydra.ext.haskell.syntax.Import> imports = (module).imports;
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> importLines = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(imports),
      () -> (hydra.util.ConsList<hydra.ast.Expr>) (hydra.util.ConsList.<hydra.ast.Expr>empty()),
      () -> hydra.util.ConsList.of(hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
        hydra.ext.haskell.Serde::importToExpr,
        imports)))));
    hydra.util.ConsList<hydra.ast.Expr> warning = hydra.util.ConsList.of(hydra.Serialization.cst(hydra.ext.haskell.Serde.toSimpleComments(hydra.Constants.warningAutoGeneratedFile())));
    return hydra.Serialization.doubleNewlineSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
      warning,
      headerLine.get(),
      importLines.get(),
      declLines.get())));
  }

  static hydra.ast.Expr nameToExpr(hydra.ext.haskell.syntax.Name name) {
    return hydra.Serialization.cst((name).accept(new hydra.ext.haskell.syntax.Name.PartialVisitor<>() {
      @Override
      public String visit(hydra.ext.haskell.syntax.Name.Implicit qn) {
        return hydra.lib.strings.Cat2.apply(
          "?",
          hydra.ext.haskell.Serde.writeQualifiedName((qn).value));
      }

      @Override
      public String visit(hydra.ext.haskell.syntax.Name.Normal qn) {
        return hydra.ext.haskell.Serde.writeQualifiedName((qn).value);
      }

      @Override
      public String visit(hydra.ext.haskell.syntax.Name.Parens qn) {
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "(",
          hydra.ext.haskell.Serde.writeQualifiedName((qn).value),
          ")"));
      }
    }));
  }

  static hydra.ast.Expr patternToExpr(hydra.ext.haskell.syntax.Pattern pat) {
    return (pat).accept(new hydra.ext.haskell.syntax.Pattern.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Pattern.Application app) {
        return hydra.ext.haskell.Serde.applicationPatternToExpr((app).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Pattern.List pats) {
        return hydra.Serialization.bracketList(
          hydra.Serialization.halfBlockStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.haskell.Serde::patternToExpr,
            (pats).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Pattern.Literal lit) {
        return hydra.ext.haskell.Serde.literalToExpr((lit).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Pattern.Name name) {
        return hydra.ext.haskell.Serde.nameToExpr((name).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Pattern.Parens pat_) {
        return hydra.Serialization.parenthesize(hydra.ext.haskell.Serde.patternToExpr((pat_).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Pattern.Tuple pats) {
        return hydra.Serialization.parenList(
          false,
          hydra.lib.lists.Map.apply(
            hydra.ext.haskell.Serde::patternToExpr,
            (pats).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Pattern.Wildcard ignored) {
        return hydra.Serialization.cst("_");
      }
    });
  }

  static hydra.ast.Expr rightHandSideToExpr(hydra.ext.haskell.syntax.RightHandSide rhs) {
    return hydra.ext.haskell.Serde.expressionToExpr((rhs).value);
  }

  static hydra.ast.Expr statementToExpr(hydra.ext.haskell.syntax.Statement stmt) {
    return hydra.ext.haskell.Serde.expressionToExpr((stmt).value);
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

  static hydra.ast.Expr typeSignatureToExpr(hydra.ext.haskell.syntax.TypeSignature typeSig) {
    hydra.ext.haskell.syntax.Name name = (typeSig).name;
    hydra.ast.Expr nameExpr = hydra.ext.haskell.Serde.nameToExpr(name);
    hydra.ext.haskell.syntax.Type typ = (typeSig).type;
    hydra.ast.Expr typeExpr = hydra.ext.haskell.Serde.typeToExpr(typ);
    hydra.ast.Expr inlineSig = hydra.Serialization.structuralSpaceSep(hydra.util.ConsList.of(
      nameExpr,
      hydra.Serialization.cst("::"),
      typeExpr));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Gt.apply(
        hydra.Serialization.expressionLength(inlineSig),
        120),
      () -> hydra.Serialization.newlineSep(hydra.util.ConsList.of(
        hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          nameExpr,
          hydra.Serialization.cst("::"))),
        hydra.Serialization.tabIndent(typeExpr))),
      () -> inlineSig);
  }

  static hydra.ast.Expr typeToExpr(hydra.ext.haskell.syntax.Type htype) {
    return (htype).accept(new hydra.ext.haskell.syntax.Type.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Type.Application appType) {
        hydra.ext.haskell.syntax.Type lhs = (appType).value.context;
        hydra.ext.haskell.syntax.Type rhs = (appType).value.argument;
        return hydra.Serialization.ifx(
          hydra.ext.haskell.Operators.appOp(),
          hydra.ext.haskell.Serde.typeToExpr(lhs),
          hydra.ext.haskell.Serde.typeToExpr(rhs));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Type.Ctx ctxType) {
        hydra.ext.haskell.syntax.Assertion ctx = (ctxType).value.ctx;
        hydra.ext.haskell.syntax.Type typ = (ctxType).value.type;
        return hydra.Serialization.ifx(
          hydra.ext.haskell.Operators.assertOp(),
          hydra.ext.haskell.Serde.assertionToExpr(ctx),
          hydra.ext.haskell.Serde.typeToExpr(typ));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Type.Function funType) {
        hydra.ext.haskell.syntax.Type cod = (funType).value.codomain;
        hydra.ext.haskell.syntax.Type dom = (funType).value.domain;
        return hydra.Serialization.ifx(
          hydra.ext.haskell.Operators.arrowOp(),
          hydra.ext.haskell.Serde.typeToExpr(dom),
          hydra.ext.haskell.Serde.typeToExpr(cod));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Type.List htype_) {
        return hydra.Serialization.bracketList(
          hydra.Serialization.inlineStyle(),
          hydra.util.ConsList.of(hydra.ext.haskell.Serde.typeToExpr((htype_).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Type.Tuple types) {
        return hydra.Serialization.parenList(
          false,
          hydra.lib.lists.Map.apply(
            hydra.ext.haskell.Serde::typeToExpr,
            (types).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.Type.Variable name) {
        return hydra.ext.haskell.Serde.nameToExpr((name).value);
      }
    });
  }

  static hydra.ast.Expr valueBindingToExpr(hydra.ext.haskell.syntax.ValueBinding vb) {
    return (vb).accept(new hydra.ext.haskell.syntax.ValueBinding.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.syntax.ValueBinding.Simple simpleVB) {
        hydra.ext.haskell.syntax.Pattern pat = (simpleVB).value.pattern;
        hydra.ast.Expr lhsExpr = hydra.ext.haskell.Serde.patternToExpr(pat);
        hydra.ext.haskell.syntax.RightHandSide rhs = (simpleVB).value.rhs;
        hydra.ast.Expr rhsExpr = hydra.ext.haskell.Serde.rightHandSideToExpr(rhs);
        hydra.ast.Expr inlineBody = hydra.Serialization.structuralSpaceSep(hydra.util.ConsList.of(
          lhsExpr,
          hydra.Serialization.cst("="),
          rhsExpr));
        hydra.util.Lazy<hydra.ast.Expr> body = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Gt.apply(
            hydra.Serialization.expressionLength(inlineBody),
            120),
          () -> hydra.Serialization.newlineSep(hydra.util.ConsList.of(
            hydra.Serialization.spaceSep(hydra.util.ConsList.of(
              lhsExpr,
              hydra.Serialization.cst("="))),
            hydra.Serialization.tabIndent(rhsExpr))),
          () -> inlineBody));
        hydra.util.Maybe<hydra.ext.haskell.syntax.LocalBindings> local = (simpleVB).value.localBindings;
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> body.get(),
          (java.util.function.Function<hydra.ext.haskell.syntax.LocalBindings, hydra.ast.Expr>) (localBindings -> {
            hydra.util.ConsList<hydra.ext.haskell.syntax.LocalBinding> bindings = (localBindings).value;
            return hydra.Serialization.indentBlock(hydra.lib.lists.Cons.apply(
              body.get(),
              hydra.util.ConsList.of(hydra.Serialization.indentBlock(hydra.lib.lists.Cons.apply(
                hydra.Serialization.cst("where"),
                hydra.lib.lists.Map.apply(
                  hydra.ext.haskell.Serde::localBindingToExpr,
                  bindings))))));
          }),
          local);
      }
    });
  }

  static hydra.ast.Expr variableToExpr(hydra.ext.haskell.syntax.Variable variable) {
    return hydra.ext.haskell.Serde.nameToExpr((variable).value);
  }

  static String writeQualifiedName(hydra.ext.haskell.syntax.QualifiedName qname) {
    java.util.function.Function<hydra.ext.haskell.syntax.NamePart, String> h = (java.util.function.Function<hydra.ext.haskell.syntax.NamePart, String>) (namePart -> (namePart).value);
    hydra.util.ConsList<hydra.ext.haskell.syntax.NamePart> qualifiers = (qname).qualifiers;
    hydra.ext.haskell.syntax.NamePart unqual = (qname).unqualified;
    hydra.util.Lazy<hydra.util.ConsList<String>> allParts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
      hydra.lib.lists.Map.apply(
        h,
        qualifiers),
      hydra.util.ConsList.of((h).apply(unqual))));
    return hydra.lib.strings.Intercalate.apply(
      ".",
      allParts.get());
  }
}
