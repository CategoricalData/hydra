// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.serde;

/**
 * Haskell operator precendence and associativity are drawn from:
 * https://self-learning-java-tutorial.blogspot.com/2016/04/haskell-operator-precedence.html
 * Other operators were investigated using GHCi, e.g. ":info (-&gt;)"
 * Operator names are drawn (loosely) from:
 * https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators
 */
public interface Serde {
  static hydra.ast.Expr alternativeToExpr(hydra.ext.haskell.ast.Alternative alt) {
    return hydra.serialization.Serialization.ifx(
      hydra.ext.haskell.operators.Operators.caseOp(),
      hydra.ext.haskell.serde.Serde.patternToExpr((alt).pattern),
      hydra.ext.haskell.serde.Serde.caseRhsToExpr((alt).rhs));
  }
  
  static hydra.ast.Expr applicationExpressionToExpr(hydra.ext.haskell.ast.ApplicationExpression app) {
    return hydra.serialization.Serialization.ifx(
      hydra.ext.haskell.operators.Operators.appOp(),
      hydra.ext.haskell.serde.Serde.expressionToExpr((app).function),
      hydra.ext.haskell.serde.Serde.expressionToExpr((app).argument));
  }
  
  static hydra.ast.Expr applicationPatternToExpr(hydra.ext.haskell.ast.ApplicationPattern appPat) {
    hydra.ext.haskell.ast.Name name = (appPat).name;
    java.util.List<hydra.ext.haskell.ast.Pattern> pats = (appPat).args;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
      hydra.ext.haskell.serde.Serde.nameToExpr(name),
      hydra.lib.lists.Map.apply(
        hydra.ext.haskell.serde.Serde::patternToExpr,
        pats)));
  }
  
  static hydra.ast.Expr assertionToExpr(hydra.ext.haskell.ast.Assertion sert) {
    return (sert).accept(new hydra.ext.haskell.ast.Assertion.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Assertion.Class_ cls) {
        return hydra.ext.haskell.serde.Serde.classAssertionToExpr((cls).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Assertion.Tuple serts) {
        return hydra.serialization.Serialization.parenList(
          false,
          hydra.lib.lists.Map.apply(
            hydra.ext.haskell.serde.Serde::assertionToExpr,
            (serts).value));
      }
    });
  }
  
  static hydra.ast.Expr caseExpressionToExpr(hydra.ext.haskell.ast.CaseExpression caseExpr) {
    java.util.List<hydra.ext.haskell.ast.Alternative> alts = (caseExpr).alternatives;
    hydra.ext.haskell.ast.Expression cs = (caseExpr).case_;
    hydra.ast.Expr lhs = hydra.serialization.Serialization.spaceSep(java.util.List.of(
      hydra.serialization.Serialization.cst("case"),
      hydra.ext.haskell.serde.Serde.expressionToExpr(cs)));
    hydra.ast.Op ofOp = new hydra.ast.Op(new hydra.ast.Symbol("of"), new hydra.ast.Padding(new hydra.ast.Ws.Space(), new hydra.ast.Ws.BreakAndIndent("  ")), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None());
    hydra.util.Lazy<hydra.ast.Expr> rhs = new hydra.util.Lazy<>(() -> hydra.serialization.Serialization.newlineSep(hydra.lib.lists.Map.apply(
      hydra.ext.haskell.serde.Serde::alternativeToExpr,
      alts)));
    return hydra.serialization.Serialization.ifx(
      ofOp,
      lhs,
      rhs.get());
  }
  
  static hydra.ast.Expr caseRhsToExpr(hydra.ext.haskell.ast.CaseRhs rhs) {
    return hydra.ext.haskell.serde.Serde.expressionToExpr((rhs).value);
  }
  
  static hydra.ast.Expr classAssertionToExpr(hydra.ext.haskell.ast.ClassAssertion clsAsrt) {
    hydra.ext.haskell.ast.Name name = (clsAsrt).name;
    java.util.List<hydra.ext.haskell.ast.Type> types = (clsAsrt).types;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
      hydra.ext.haskell.serde.Serde.nameToExpr(name),
      java.util.List.of(hydra.serialization.Serialization.commaSep(
        hydra.serialization.Serialization.halfBlockStyle(),
        hydra.lib.lists.Map.apply(
          hydra.ext.haskell.serde.Serde::typeToExpr,
          types)))));
  }
  
  static hydra.ast.Expr constructorToExpr(hydra.ext.haskell.ast.Constructor cons) {
    return (cons).accept(new hydra.ext.haskell.ast.Constructor.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Constructor.Ordinary ord) {
        hydra.ext.haskell.ast.Name name = ((ord).value).name;
        java.util.List<hydra.ext.haskell.ast.Type> types = ((ord).value).fields;
        return hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
          hydra.ext.haskell.serde.Serde.nameToExpr(name),
          java.util.List.of(hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Map.apply(
            hydra.ext.haskell.serde.Serde::typeToExpr,
            types)))));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Constructor.Record rec) {
        java.util.List<hydra.ext.haskell.ast.FieldWithComments> fields = ((rec).value).fields;
        hydra.ext.haskell.ast.Name name = ((rec).value).name;
        return hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
          hydra.ext.haskell.serde.Serde.nameToExpr(name),
          java.util.List.of(hydra.serialization.Serialization.curlyBracesList(
            (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
            hydra.serialization.Serialization.halfBlockStyle(),
            hydra.lib.lists.Map.apply(
              hydra.ext.haskell.serde.Serde::fieldWithCommentsToExpr,
              fields)))));
      }
    });
  }
  
  static hydra.ast.Expr constructorWithCommentsToExpr(hydra.ext.haskell.ast.ConstructorWithComments consWithComments) {
    hydra.ext.haskell.ast.Constructor body = (consWithComments).body;
    hydra.util.Maybe<String> mc = (consWithComments).comments;
    return hydra.lib.maybes.Maybe.apply(
      hydra.ext.haskell.serde.Serde.constructorToExpr(body),
      (java.util.function.Function<String, hydra.ast.Expr>) (c -> hydra.serialization.Serialization.newlineSep(hydra.lib.lists.Cons.apply(
        hydra.serialization.Serialization.cst(hydra.ext.haskell.serde.Serde.toHaskellComments(c)),
        java.util.List.of(hydra.ext.haskell.serde.Serde.constructorToExpr(body))))),
      mc);
  }
  
  static hydra.ast.Expr dataOrNewtypeToExpr(hydra.ext.haskell.ast.DataOrNewtype kw) {
    return (kw).accept(new hydra.ext.haskell.ast.DataOrNewtype.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.DataOrNewtype.Data ignored) {
        return hydra.serialization.Serialization.cst("data");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.DataOrNewtype.Newtype ignored) {
        return hydra.serialization.Serialization.cst("newtype");
      }
    });
  }
  
  static hydra.ast.Expr declarationHeadToExpr(hydra.ext.haskell.ast.DeclarationHead hd) {
    return (hd).accept(new hydra.ext.haskell.ast.DeclarationHead.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.DeclarationHead.Application appHead) {
        hydra.ext.haskell.ast.DeclarationHead fun = ((appHead).value).function;
        hydra.ext.haskell.ast.Variable op = ((appHead).value).operand;
        return hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
          hydra.ext.haskell.serde.Serde.declarationHeadToExpr(fun),
          java.util.List.of(hydra.ext.haskell.serde.Serde.variableToExpr(op))));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.DeclarationHead.Simple name) {
        return hydra.ext.haskell.serde.Serde.nameToExpr((name).value);
      }
    });
  }
  
  static hydra.ast.Expr declarationToExpr(hydra.ext.haskell.ast.Declaration decl) {
    return (decl).accept(new hydra.ext.haskell.ast.Declaration.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Declaration.Data dataDecl) {
        java.util.List<hydra.ext.haskell.ast.ConstructorWithComments> cons = ((dataDecl).value).constructors;
        hydra.util.Lazy<hydra.ast.Expr> constructors = new hydra.util.Lazy<>(() -> hydra.serialization.Serialization.orSep(
          hydra.serialization.Serialization.halfBlockStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.haskell.serde.Serde::constructorWithCommentsToExpr,
            cons)));
        java.util.List<hydra.ext.haskell.ast.Deriving> deriv = ((dataDecl).value).deriving;
        hydra.util.Lazy<java.util.List<hydra.ext.haskell.ast.Name>> derivCat = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
          wrapped -> (wrapped).value,
          deriv)));
        hydra.util.Lazy<java.util.List<hydra.ast.Expr>> derivingClause = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(derivCat.get()),
          () -> (java.util.List<hydra.ast.Expr>) (java.util.List.<hydra.ast.Expr>of()),
          () -> java.util.List.of(hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
            hydra.serialization.Serialization.cst("deriving"),
            java.util.List.of(hydra.serialization.Serialization.parenList(
              false,
              hydra.lib.lists.Map.apply(
                hydra.ext.haskell.serde.Serde::nameToExpr,
                derivCat.get()))))))));
        hydra.ext.haskell.ast.DeclarationHead hd = ((dataDecl).value).head;
        hydra.ext.haskell.ast.DataOrNewtype kw = ((dataDecl).value).keyword;
        hydra.util.Lazy<java.util.List<hydra.ast.Expr>> mainParts = new hydra.util.Lazy<>(() -> java.util.List.of(
          hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
            hydra.ext.haskell.serde.Serde.dataOrNewtypeToExpr(kw),
            hydra.lib.lists.Cons.apply(
              hydra.ext.haskell.serde.Serde.declarationHeadToExpr(hd),
              java.util.List.of(hydra.serialization.Serialization.cst("="))))),
          constructors.get()));
        return hydra.serialization.Serialization.indentBlock(hydra.lib.lists.Concat2.apply(
          mainParts.get(),
          derivingClause.get()));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Declaration.Type typeDecl) {
        hydra.ext.haskell.ast.DeclarationHead hd = ((typeDecl).value).name;
        hydra.ext.haskell.ast.Type typ = ((typeDecl).value).type;
        return hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
          hydra.serialization.Serialization.cst("type"),
          hydra.lib.lists.Cons.apply(
            hydra.ext.haskell.serde.Serde.declarationHeadToExpr(hd),
            hydra.lib.lists.Cons.apply(
              hydra.serialization.Serialization.cst("="),
              java.util.List.of(hydra.ext.haskell.serde.Serde.typeToExpr(typ))))));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Declaration.ValueBinding vb) {
        return hydra.ext.haskell.serde.Serde.valueBindingToExpr((vb).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Declaration.TypedBinding typedBinding) {
        hydra.ext.haskell.ast.TypeSignature typeSig = ((typedBinding).value).typeSignature;
        hydra.ext.haskell.ast.Type htype = (typeSig).type;
        hydra.ext.haskell.ast.Name name = (typeSig).name;
        hydra.ext.haskell.ast.ValueBinding vb = ((typedBinding).value).valueBinding;
        return hydra.serialization.Serialization.newlineSep(hydra.lib.lists.Cons.apply(
          hydra.serialization.Serialization.ifx(
            hydra.ext.haskell.operators.Operators.typeOp(),
            hydra.ext.haskell.serde.Serde.nameToExpr(name),
            hydra.ext.haskell.serde.Serde.typeToExpr(htype)),
          java.util.List.of(hydra.ext.haskell.serde.Serde.valueBindingToExpr(vb))));
      }
    });
  }
  
  static hydra.ast.Expr declarationWithCommentsToExpr(hydra.ext.haskell.ast.DeclarationWithComments declWithComments) {
    hydra.ext.haskell.ast.Declaration body = (declWithComments).body;
    hydra.util.Maybe<String> mc = (declWithComments).comments;
    return hydra.lib.maybes.Maybe.apply(
      hydra.ext.haskell.serde.Serde.declarationToExpr(body),
      (java.util.function.Function<String, hydra.ast.Expr>) (c -> hydra.serialization.Serialization.newlineSep(hydra.lib.lists.Cons.apply(
        hydra.serialization.Serialization.cst(hydra.ext.haskell.serde.Serde.toHaskellComments(c)),
        java.util.List.of(hydra.ext.haskell.serde.Serde.declarationToExpr(body))))),
      mc);
  }
  
  static hydra.ast.Expr expressionToExpr(hydra.ext.haskell.ast.Expression expr) {
    return (expr).accept(new hydra.ext.haskell.ast.Expression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Expression.Application app) {
        return hydra.ext.haskell.serde.Serde.applicationExpressionToExpr((app).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Expression.Case cases) {
        return hydra.ext.haskell.serde.Serde.caseExpressionToExpr((cases).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Expression.ConstructRecord r) {
        return hydra.ext.haskell.serde.Serde.constructRecordExpressionToExpr((r).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Expression.Do statements) {
        return hydra.serialization.Serialization.indentBlock(hydra.lib.lists.Cons.apply(
          hydra.serialization.Serialization.cst("do"),
          hydra.lib.lists.Map.apply(
            hydra.ext.haskell.serde.Serde::statementToExpr,
            (statements).value)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Expression.If ifte) {
        return hydra.ext.haskell.serde.Serde.ifExpressionToExpr((ifte).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Expression.Literal lit) {
        return hydra.ext.haskell.serde.Serde.literalToExpr((lit).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Expression.Lambda lam) {
        return hydra.serialization.Serialization.parenthesize(hydra.ext.haskell.serde.Serde.lambdaExpressionToExpr((lam).value));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Expression.Let letExpr) {
        java.util.List<hydra.ext.haskell.ast.LocalBinding> bindings = ((letExpr).value).bindings;
        java.util.function.Function<hydra.ext.haskell.ast.LocalBinding, hydra.ast.Expr> encodeBinding = (java.util.function.Function<hydra.ext.haskell.ast.LocalBinding, hydra.ast.Expr>) (binding -> hydra.serialization.Serialization.indentSubsequentLines(
          "      ",
          hydra.ext.haskell.serde.Serde.localBindingToExpr(binding)));
        hydra.ext.haskell.ast.Expression inner = ((letExpr).value).inner;
        return hydra.serialization.Serialization.indentBlock(hydra.lib.lists.Cons.apply(
          hydra.serialization.Serialization.cst(""),
          hydra.lib.lists.Cons.apply(
            hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
              hydra.serialization.Serialization.cst("let"),
              java.util.List.of(hydra.serialization.Serialization.customIndentBlock(
                "    ",
                hydra.lib.lists.Map.apply(
                  encodeBinding,
                  bindings))))),
            java.util.List.of(hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
              hydra.serialization.Serialization.cst("in"),
              java.util.List.of(hydra.ext.haskell.serde.Serde.expressionToExpr(inner))))))));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Expression.List exprs) {
        return hydra.serialization.Serialization.bracketList(
          hydra.serialization.Serialization.halfBlockStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.haskell.serde.Serde::expressionToExpr,
            (exprs).value));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Expression.Parens expr_) {
        return hydra.serialization.Serialization.parenthesize(hydra.ext.haskell.serde.Serde.expressionToExpr((expr_).value));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Expression.Tuple exprs) {
        return hydra.serialization.Serialization.parenList(
          false,
          hydra.lib.lists.Map.apply(
            hydra.ext.haskell.serde.Serde::expressionToExpr,
            (exprs).value));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Expression.Variable name) {
        return hydra.ext.haskell.serde.Serde.nameToExpr((name).value);
      }
    });
  }
  
  static hydra.ast.Expr constructRecordExpressionToExpr(hydra.ext.haskell.ast.ConstructRecordExpression constructRecord) {
    java.util.function.Function<hydra.ext.haskell.ast.FieldUpdate, hydra.ast.Expr> fromUpdate = (java.util.function.Function<hydra.ext.haskell.ast.FieldUpdate, hydra.ast.Expr>) (update -> {
      hydra.ext.haskell.ast.Name fn = (update).name;
      hydra.ext.haskell.ast.Expression val = (update).value;
      return hydra.serialization.Serialization.ifx(
        hydra.ext.haskell.operators.Operators.defineOp(),
        hydra.ext.haskell.serde.Serde.nameToExpr(fn),
        hydra.ext.haskell.serde.Serde.expressionToExpr(val));
    });
    java.util.List<hydra.ext.haskell.ast.FieldUpdate> updates = (constructRecord).fields;
    hydra.util.Lazy<hydra.ast.Expr> body = new hydra.util.Lazy<>(() -> hydra.serialization.Serialization.commaSep(
      hydra.serialization.Serialization.halfBlockStyle(),
      hydra.lib.lists.Map.apply(
        fromUpdate,
        updates)));
    hydra.ext.haskell.ast.Name name = (constructRecord).name;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
      hydra.ext.haskell.serde.Serde.nameToExpr(name),
      java.util.List.of(hydra.serialization.Serialization.brackets(
        hydra.serialization.Serialization.curlyBraces(),
        hydra.serialization.Serialization.halfBlockStyle(),
        body.get()))));
  }
  
  static hydra.ast.Expr fieldToExpr(hydra.ext.haskell.ast.Field field) {
    hydra.ext.haskell.ast.Name name = (field).name;
    hydra.ext.haskell.ast.Type typ = (field).type;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
      hydra.ext.haskell.serde.Serde.nameToExpr(name),
      hydra.lib.lists.Cons.apply(
        hydra.serialization.Serialization.cst("::"),
        java.util.List.of(hydra.ext.haskell.serde.Serde.typeToExpr(typ)))));
  }
  
  static hydra.ast.Expr fieldWithCommentsToExpr(hydra.ext.haskell.ast.FieldWithComments fieldWithComments) {
    hydra.ext.haskell.ast.Field field = (fieldWithComments).field;
    hydra.util.Maybe<String> mc = (fieldWithComments).comments;
    return hydra.lib.maybes.Maybe.apply(
      hydra.ext.haskell.serde.Serde.fieldToExpr(field),
      (java.util.function.Function<String, hydra.ast.Expr>) (c -> hydra.serialization.Serialization.newlineSep(hydra.lib.lists.Cons.apply(
        hydra.serialization.Serialization.cst(hydra.ext.haskell.serde.Serde.toHaskellComments(c)),
        java.util.List.of(hydra.ext.haskell.serde.Serde.fieldToExpr(field))))),
      mc);
  }
  
  static hydra.ast.Expr ifExpressionToExpr(hydra.ext.haskell.ast.IfExpression ifExpr) {
    hydra.ext.haskell.ast.Expression eelse = (ifExpr).else_;
    hydra.ext.haskell.ast.Expression ethen = (ifExpr).then;
    hydra.util.Lazy<hydra.ast.Expr> body = new hydra.util.Lazy<>(() -> hydra.serialization.Serialization.newlineSep(hydra.lib.lists.Cons.apply(
      hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
        hydra.serialization.Serialization.cst("then"),
        java.util.List.of(hydra.ext.haskell.serde.Serde.expressionToExpr(ethen)))),
      java.util.List.of(hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
        hydra.serialization.Serialization.cst("else"),
        java.util.List.of(hydra.ext.haskell.serde.Serde.expressionToExpr(eelse))))))));
    hydra.ext.haskell.ast.Expression eif = (ifExpr).condition;
    hydra.ast.Op ifOp = new hydra.ast.Op(new hydra.ast.Symbol(""), new hydra.ast.Padding(new hydra.ast.Ws.None(), new hydra.ast.Ws.BreakAndIndent("  ")), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None());
    return hydra.serialization.Serialization.ifx(
      ifOp,
      hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
        hydra.serialization.Serialization.cst("if"),
        java.util.List.of(hydra.ext.haskell.serde.Serde.expressionToExpr(eif)))),
      body.get());
  }
  
  static hydra.ast.Expr importExportSpecToExpr(hydra.ext.haskell.ast.ImportExportSpec spec) {
    return hydra.ext.haskell.serde.Serde.nameToExpr((spec).name);
  }
  
  static hydra.ast.Expr importToExpr(hydra.ext.haskell.ast.Import import_) {
    java.util.function.Function<hydra.ext.haskell.ast.SpecImport, hydra.ast.Expr> hidingSec = (java.util.function.Function<hydra.ext.haskell.ast.SpecImport, hydra.ast.Expr>) (spec -> (spec).accept(new hydra.ext.haskell.ast.SpecImport.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.SpecImport.Hiding names) {
        return hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
          hydra.serialization.Serialization.cst("hiding "),
          java.util.List.of(hydra.serialization.Serialization.parens(hydra.serialization.Serialization.commaSep(
            hydra.serialization.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.ext.haskell.serde.Serde::importExportSpecToExpr,
              (names).value))))));
      }
    }));
    hydra.util.Maybe<hydra.ext.haskell.ast.ModuleName> mod = (import_).as;
    hydra.ext.haskell.ast.ModuleName modName = (import_).module;
    hydra.util.Maybe<hydra.ext.haskell.ast.SpecImport> mspec = (import_).spec;
    String name = (modName).value;
    Boolean qual = (import_).qualified;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> parts = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.util.Maybe.just(hydra.serialization.Serialization.cst("import")),
      hydra.lib.logic.IfElse.lazy(
        qual,
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.cst("qualified")),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing())),
      hydra.util.Maybe.just(hydra.serialization.Serialization.cst(name)),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.ext.haskell.ast.ModuleName, hydra.ast.Expr>) (m -> hydra.serialization.Serialization.cst(hydra.lib.strings.Cat2.apply(
          "as ",
          (m).value))),
        mod),
      hydra.lib.maybes.Map.apply(
        hidingSec,
        mspec))));
    return hydra.serialization.Serialization.spaceSep(parts.get());
  }
  
  static hydra.ast.Expr lambdaExpressionToExpr(hydra.ext.haskell.ast.LambdaExpression lambdaExpr) {
    java.util.List<hydra.ext.haskell.ast.Pattern> bindings = (lambdaExpr).bindings;
    hydra.ext.haskell.ast.Expression inner = (lambdaExpr).inner;
    hydra.ast.Expr body = hydra.ext.haskell.serde.Serde.expressionToExpr(inner);
    hydra.util.Lazy<hydra.ast.Expr> head = new hydra.util.Lazy<>(() -> hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Map.apply(
      hydra.ext.haskell.serde.Serde::patternToExpr,
      bindings)));
    return hydra.serialization.Serialization.ifx(
      hydra.ext.haskell.operators.Operators.lambdaOp(),
      hydra.serialization.Serialization.prefix(
        "\\",
        head.get()),
      body);
  }
  
  static hydra.ast.Expr literalToExpr(hydra.ext.haskell.ast.Literal lit) {
    java.util.function.Function<Boolean, java.util.function.Function<String, String>> parensIfNeg = (java.util.function.Function<Boolean, java.util.function.Function<String, String>>) (b -> (java.util.function.Function<String, String>) (e -> hydra.lib.logic.IfElse.lazy(
      b,
      () -> hydra.lib.strings.Cat.apply(java.util.List.of(
        "(",
        e,
        ")")),
      () -> e)));
    return hydra.serialization.Serialization.cst((lit).accept(new hydra.ext.haskell.ast.Literal.PartialVisitor<>() {
      @Override
      public String visit(hydra.ext.haskell.ast.Literal.Char c) {
        return hydra.lib.literals.ShowString.apply(hydra.lib.literals.ShowUint16.apply((c).value));
      }
      
      @Override
      public String visit(hydra.ext.haskell.ast.Literal.Double_ d) {
        return ((parensIfNeg).apply(hydra.lib.equality.Lt.apply(
          (d).value,
          0.0))).apply(hydra.lib.literals.ShowFloat64.apply((d).value));
      }
      
      @Override
      public String visit(hydra.ext.haskell.ast.Literal.Float_ f) {
        return ((parensIfNeg).apply(hydra.lib.equality.Lt.apply(
          (f).value,
          (float) (0.0)))).apply(hydra.lib.literals.ShowFloat32.apply((f).value));
      }
      
      @Override
      public String visit(hydra.ext.haskell.ast.Literal.Int i) {
        return ((parensIfNeg).apply(hydra.lib.equality.Lt.apply(
          (i).value,
          0))).apply(hydra.lib.literals.ShowInt32.apply((i).value));
      }
      
      @Override
      public String visit(hydra.ext.haskell.ast.Literal.Integer_ i) {
        return ((parensIfNeg).apply(hydra.lib.equality.Lt.apply(
          (i).value,
          new java.math.BigInteger("0")))).apply(hydra.lib.literals.ShowBigint.apply((i).value));
      }
      
      @Override
      public String visit(hydra.ext.haskell.ast.Literal.String_ s) {
        return hydra.lib.literals.ShowString.apply((s).value);
      }
    }));
  }
  
  static hydra.ast.Expr localBindingToExpr(hydra.ext.haskell.ast.LocalBinding binding) {
    return (binding).accept(new hydra.ext.haskell.ast.LocalBinding.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.LocalBinding.Signature ts) {
        return hydra.ext.haskell.serde.Serde.typeSignatureToExpr((ts).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.LocalBinding.Value vb) {
        return hydra.ext.haskell.serde.Serde.valueBindingToExpr((vb).value);
      }
    });
  }
  
  static hydra.ast.Expr moduleHeadToExpr(hydra.ext.haskell.ast.ModuleHead moduleHead) {
    hydra.ext.haskell.ast.ModuleName modName = (moduleHead).name;
    String mname = (modName).value;
    hydra.util.Lazy<hydra.ast.Expr> head = new hydra.util.Lazy<>(() -> hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
      hydra.serialization.Serialization.cst("module"),
      hydra.lib.lists.Cons.apply(
        hydra.serialization.Serialization.cst(mname),
        java.util.List.of(hydra.serialization.Serialization.cst("where"))))));
    hydra.util.Maybe<String> mc = (moduleHead).comments;
    return hydra.lib.maybes.Maybe.apply(
      head.get(),
      (java.util.function.Function<String, hydra.ast.Expr>) (c -> hydra.serialization.Serialization.newlineSep(hydra.lib.lists.Cons.apply(
        hydra.serialization.Serialization.cst(hydra.ext.haskell.serde.Serde.toHaskellComments(c)),
        hydra.lib.lists.Cons.apply(
          hydra.serialization.Serialization.cst(""),
          java.util.List.of(head.get()))))),
      mc);
  }
  
  static hydra.ast.Expr moduleToExpr(hydra.ext.haskell.ast.Module module) {
    java.util.List<hydra.ext.haskell.ast.DeclarationWithComments> decls = (module).declarations;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> declLines = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      hydra.ext.haskell.serde.Serde::declarationWithCommentsToExpr,
      decls));
    hydra.util.Maybe<hydra.ext.haskell.ast.ModuleHead> mh = (module).head;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> headerLine = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.apply(
      (java.util.List<hydra.ast.Expr>) (java.util.List.<hydra.ast.Expr>of()),
      (java.util.function.Function<hydra.ext.haskell.ast.ModuleHead, java.util.List<hydra.ast.Expr>>) (h -> java.util.List.of(hydra.ext.haskell.serde.Serde.moduleHeadToExpr(h))),
      mh));
    java.util.List<hydra.ext.haskell.ast.Import> imports = (module).imports;
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> importLines = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(imports),
      () -> (java.util.List<hydra.ast.Expr>) (java.util.List.<hydra.ast.Expr>of()),
      () -> java.util.List.of(hydra.serialization.Serialization.newlineSep(hydra.lib.lists.Map.apply(
        hydra.ext.haskell.serde.Serde::importToExpr,
        imports)))));
    java.util.List<hydra.ast.Expr> warning = java.util.List.of(hydra.serialization.Serialization.cst(hydra.ext.haskell.serde.Serde.toSimpleComments(hydra.constants.Constants.warningAutoGeneratedFile())));
    return hydra.serialization.Serialization.doubleNewlineSep(hydra.lib.lists.Concat.apply(java.util.List.of(
      warning,
      headerLine.get(),
      importLines.get(),
      declLines.get())));
  }
  
  static hydra.ast.Expr nameToExpr(hydra.ext.haskell.ast.Name name) {
    return hydra.serialization.Serialization.cst((name).accept(new hydra.ext.haskell.ast.Name.PartialVisitor<>() {
      @Override
      public String visit(hydra.ext.haskell.ast.Name.Implicit qn) {
        return hydra.lib.strings.Cat2.apply(
          "?",
          hydra.ext.haskell.serde.Serde.writeQualifiedName((qn).value));
      }
      
      @Override
      public String visit(hydra.ext.haskell.ast.Name.Normal qn) {
        return hydra.ext.haskell.serde.Serde.writeQualifiedName((qn).value);
      }
      
      @Override
      public String visit(hydra.ext.haskell.ast.Name.Parens qn) {
        return hydra.lib.strings.Cat.apply(java.util.List.of(
          "(",
          hydra.ext.haskell.serde.Serde.writeQualifiedName((qn).value),
          ")"));
      }
    }));
  }
  
  static hydra.ast.Expr patternToExpr(hydra.ext.haskell.ast.Pattern pat) {
    return (pat).accept(new hydra.ext.haskell.ast.Pattern.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Pattern.Application app) {
        return hydra.ext.haskell.serde.Serde.applicationPatternToExpr((app).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Pattern.List pats) {
        return hydra.serialization.Serialization.bracketList(
          hydra.serialization.Serialization.halfBlockStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.haskell.serde.Serde::patternToExpr,
            (pats).value));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Pattern.Literal lit) {
        return hydra.ext.haskell.serde.Serde.literalToExpr((lit).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Pattern.Name name) {
        return hydra.ext.haskell.serde.Serde.nameToExpr((name).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Pattern.Parens pat_) {
        return hydra.serialization.Serialization.parenthesize(hydra.ext.haskell.serde.Serde.patternToExpr((pat_).value));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Pattern.Tuple pats) {
        return hydra.serialization.Serialization.parenList(
          false,
          hydra.lib.lists.Map.apply(
            hydra.ext.haskell.serde.Serde::patternToExpr,
            (pats).value));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Pattern.Wildcard ignored) {
        return hydra.serialization.Serialization.cst("_");
      }
    });
  }
  
  static hydra.ast.Expr rightHandSideToExpr(hydra.ext.haskell.ast.RightHandSide rhs) {
    return hydra.ext.haskell.serde.Serde.expressionToExpr((rhs).value);
  }
  
  static hydra.ast.Expr statementToExpr(hydra.ext.haskell.ast.Statement stmt) {
    return hydra.ext.haskell.serde.Serde.expressionToExpr((stmt).value);
  }
  
  static hydra.ast.Expr typeSignatureToExpr(hydra.ext.haskell.ast.TypeSignature typeSig) {
    hydra.ext.haskell.ast.Name name = (typeSig).name;
    hydra.ext.haskell.ast.Type typ = (typeSig).type;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
      hydra.ext.haskell.serde.Serde.nameToExpr(name),
      hydra.lib.lists.Cons.apply(
        hydra.serialization.Serialization.cst("::"),
        java.util.List.of(hydra.ext.haskell.serde.Serde.typeToExpr(typ)))));
  }
  
  static hydra.ast.Expr typeToExpr(hydra.ext.haskell.ast.Type htype) {
    return (htype).accept(new hydra.ext.haskell.ast.Type.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Type.Application appType) {
        hydra.ext.haskell.ast.Type lhs = ((appType).value).context;
        hydra.ext.haskell.ast.Type rhs = ((appType).value).argument;
        return hydra.serialization.Serialization.ifx(
          hydra.ext.haskell.operators.Operators.appOp(),
          hydra.ext.haskell.serde.Serde.typeToExpr(lhs),
          hydra.ext.haskell.serde.Serde.typeToExpr(rhs));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Type.Ctx ctxType) {
        hydra.ext.haskell.ast.Assertion ctx = ((ctxType).value).ctx;
        hydra.ext.haskell.ast.Type typ = ((ctxType).value).type;
        return hydra.serialization.Serialization.ifx(
          hydra.ext.haskell.operators.Operators.assertOp(),
          hydra.ext.haskell.serde.Serde.assertionToExpr(ctx),
          hydra.ext.haskell.serde.Serde.typeToExpr(typ));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Type.Function funType) {
        hydra.ext.haskell.ast.Type cod = ((funType).value).codomain;
        hydra.ext.haskell.ast.Type dom = ((funType).value).domain;
        return hydra.serialization.Serialization.ifx(
          hydra.ext.haskell.operators.Operators.arrowOp(),
          hydra.ext.haskell.serde.Serde.typeToExpr(dom),
          hydra.ext.haskell.serde.Serde.typeToExpr(cod));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Type.List htype_) {
        return hydra.serialization.Serialization.bracketList(
          hydra.serialization.Serialization.inlineStyle(),
          java.util.List.of(hydra.ext.haskell.serde.Serde.typeToExpr((htype_).value)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Type.Tuple types) {
        return hydra.serialization.Serialization.parenList(
          false,
          hydra.lib.lists.Map.apply(
            hydra.ext.haskell.serde.Serde::typeToExpr,
            (types).value));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.Type.Variable name) {
        return hydra.ext.haskell.serde.Serde.nameToExpr((name).value);
      }
    });
  }
  
  static hydra.ast.Expr valueBindingToExpr(hydra.ext.haskell.ast.ValueBinding vb) {
    return (vb).accept(new hydra.ext.haskell.ast.ValueBinding.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.haskell.ast.ValueBinding.Simple simpleVB) {
        hydra.ext.haskell.ast.Pattern pat = ((simpleVB).value).pattern;
        hydra.ext.haskell.ast.RightHandSide rhs = ((simpleVB).value).rhs;
        hydra.ast.Expr body = hydra.serialization.Serialization.ifx(
          hydra.ext.haskell.operators.Operators.defineOp(),
          hydra.ext.haskell.serde.Serde.patternToExpr(pat),
          hydra.ext.haskell.serde.Serde.rightHandSideToExpr(rhs));
        hydra.util.Maybe<hydra.ext.haskell.ast.LocalBindings> local = ((simpleVB).value).localBindings;
        return hydra.lib.maybes.Maybe.apply(
          body,
          (java.util.function.Function<hydra.ext.haskell.ast.LocalBindings, hydra.ast.Expr>) (localBindings -> {
            java.util.List<hydra.ext.haskell.ast.LocalBinding> bindings = (localBindings).value;
            return hydra.serialization.Serialization.indentBlock(hydra.lib.lists.Cons.apply(
              body,
              java.util.List.of(hydra.serialization.Serialization.indentBlock(hydra.lib.lists.Cons.apply(
                hydra.serialization.Serialization.cst("where"),
                hydra.lib.lists.Map.apply(
                  hydra.ext.haskell.serde.Serde::localBindingToExpr,
                  bindings))))));
          }),
          local);
      }
    });
  }
  
  static hydra.ast.Expr variableToExpr(hydra.ext.haskell.ast.Variable variable) {
    return hydra.ext.haskell.serde.Serde.nameToExpr((variable).value);
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
  
  static String writeQualifiedName(hydra.ext.haskell.ast.QualifiedName qname) {
    java.util.function.Function<hydra.ext.haskell.ast.NamePart, String> h = (java.util.function.Function<hydra.ext.haskell.ast.NamePart, String>) (namePart -> (namePart).value);
    java.util.List<hydra.ext.haskell.ast.NamePart> qualifiers = (qname).qualifiers;
    hydra.ext.haskell.ast.NamePart unqual = (qname).unqualified;
    hydra.util.Lazy<java.util.List<String>> allParts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
      hydra.lib.lists.Map.apply(
        h,
        qualifiers),
      java.util.List.of((h).apply(unqual))));
    return hydra.lib.strings.Intercalate.apply(
      ".",
      allParts.get());
  }
}
