// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python;

/**
 * Python serializer: converts Python AST to concrete syntax
 */
public interface Serde {
  static hydra.ast.Expr encodeAnnotatedRhs(hydra.ext.python.syntax.AnnotatedRhs arhs) {
    return hydra.Serialization.spaceSep(hydra.util.ConsList.of(
      hydra.Serialization.cst("="),
      (arhs).accept(new hydra.ext.python.syntax.AnnotatedRhs.PartialVisitor<>() {
        @Override
        public hydra.ast.Expr visit(hydra.ext.python.syntax.AnnotatedRhs.Star ses) {
          return hydra.Serialization.commaSep(
            hydra.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.ext.python.Serde::encodeStarExpression,
              (ses).value));
        }

        @Override
        public hydra.ast.Expr visit(hydra.ext.python.syntax.AnnotatedRhs.Yield ignored) {
          return hydra.Serialization.cst("yield ...");
        }
      })));
  }

  static hydra.ast.Expr encodeAnnotatedStatement(hydra.ext.python.syntax.AnnotatedStatement as_) {
    String doc_ = (as_).comment;
    hydra.ext.python.syntax.Statement stmt = (as_).statement;
    return hydra.Serialization.newlineSep(hydra.util.ConsList.of(
      hydra.Serialization.cst(hydra.ext.python.Serde.toPythonComments(doc_)),
      hydra.ext.python.Serde.encodeStatement(stmt)));
  }

  static hydra.ast.Expr encodeAnnotation(hydra.ext.python.syntax.Annotation ann) {
    return hydra.Serialization.spaceSep(hydra.util.ConsList.of(
      hydra.Serialization.cst(":"),
      hydra.ext.python.Serde.encodeExpression((ann).value)));
  }

  static hydra.ast.Expr encodeArgs(hydra.ext.python.syntax.Args args) {
    hydra.util.ConsList<hydra.ext.python.syntax.KwargOrStarred> ks = (args).kwargOrStarred;
    hydra.util.ConsList<hydra.ext.python.syntax.KwargOrDoubleStarred> kss = (args).kwargOrDoubleStarred;
    hydra.util.ConsList<hydra.ext.python.syntax.PosArg> pos = (args).positional;
    return hydra.Serialization.commaSep(
      hydra.Serialization.inlineStyle(),
      hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
        hydra.lib.lists.Map.apply(
          hydra.ext.python.Serde::encodePosArg,
          pos),
        hydra.lib.lists.Map.apply(
          hydra.ext.python.Serde::encodeKwargOrStarred,
          ks),
        hydra.lib.lists.Map.apply(
          hydra.ext.python.Serde::encodeKwargOrDoubleStarred,
          kss))));
  }

  static hydra.ast.Expr encodeAssignment(hydra.ext.python.syntax.Assignment a) {
    return (a).accept(new hydra.ext.python.syntax.Assignment.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Assignment.Typed t) {
        return hydra.ext.python.Serde.encodeTypedAssignment((t).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Assignment.Untyped u) {
        return hydra.ext.python.Serde.encodeUntypedAssignment((u).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Assignment.Aug ignored) {
        return hydra.Serialization.cst("... += ...");
      }
    });
  }

  static hydra.ast.Expr encodeAssignmentExpression(hydra.ext.python.syntax.AssignmentExpression ae) {
    hydra.ext.python.syntax.Expression expr = (ae).expression;
    hydra.ext.python.syntax.Name name = (ae).name;
    return hydra.Serialization.spaceSep(hydra.util.ConsList.of(
      hydra.ext.python.Serde.encodeName(name),
      hydra.Serialization.cst(":="),
      hydra.ext.python.Serde.encodeExpression(expr)));
  }

  static hydra.ast.Expr encodeAtom(hydra.ext.python.syntax.Atom atom) {
    return (atom).accept(new hydra.ext.python.syntax.Atom.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Dict d) {
        return hydra.ext.python.Serde.encodeDict((d).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Dictcomp ignored) {
        return hydra.Serialization.cst("{...}");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Ellipsis ignored) {
        return hydra.Serialization.cst("...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.False ignored) {
        return hydra.Serialization.cst("False");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Genexp ignored) {
        return hydra.Serialization.cst("(...)");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Group g) {
        return hydra.ext.python.Serde.encodeGroup((g).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.List l) {
        return hydra.ext.python.Serde.encodeList((l).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Listcomp ignored) {
        return hydra.Serialization.cst("[...]");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Name n) {
        return hydra.ext.python.Serde.encodeName((n).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.None ignored) {
        return hydra.Serialization.cst("None");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Number_ n) {
        return hydra.ext.python.Serde.encodeNumber((n).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Set s) {
        return hydra.ext.python.Serde.encodeSet((s).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Setcomp ignored) {
        return hydra.Serialization.cst("{...}");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.String_ s) {
        return hydra.ext.python.Serde.encodeString((s).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.True ignored) {
        return hydra.Serialization.cst("True");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Tuple t) {
        return hydra.ext.python.Serde.encodeTuple((t).value);
      }
    });
  }

  static hydra.ast.Expr encodeAttribute(hydra.ext.python.syntax.Attribute attr) {
    return hydra.Serialization.dotSep(hydra.lib.lists.Map.apply(
      hydra.ext.python.Serde::encodeName,
      (attr).value));
  }

  static hydra.ast.Expr encodeAwaitPrimary(hydra.ext.python.syntax.AwaitPrimary ap) {
    Boolean await_ = (ap).await;
    hydra.ext.python.syntax.Primary primary = (ap).primary;
    return hydra.lib.logic.IfElse.lazy(
      await_,
      () -> hydra.Serialization.spaceSep(hydra.util.ConsList.of(
        hydra.Serialization.cst("await"),
        hydra.ext.python.Serde.encodePrimary(primary))),
      () -> hydra.ext.python.Serde.encodePrimary(primary));
  }

  static hydra.ast.Expr encodeBitwiseAnd(hydra.ext.python.syntax.BitwiseAnd band) {
    hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd> lhs = (band).lhs;
    hydra.ext.python.syntax.ShiftExpression rhs = (band).rhs;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.BitwiseAnd, hydra.ast.Expr>) (l -> hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.ext.python.Serde.encodeBitwiseAnd(l),
          hydra.Serialization.cst("&")))),
        lhs),
      hydra.util.Maybe.just(hydra.ext.python.Serde.encodeShiftExpression(rhs)))));
  }

  static hydra.ast.Expr encodeBitwiseOr(hydra.ext.python.syntax.BitwiseOr bor) {
    hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr> lhs = (bor).lhs;
    hydra.ext.python.syntax.BitwiseXor rhs = (bor).rhs;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.BitwiseOr, hydra.ast.Expr>) (l -> hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.ext.python.Serde.encodeBitwiseOr(l),
          hydra.Serialization.cst("|")))),
        lhs),
      hydra.util.Maybe.just(hydra.ext.python.Serde.encodeBitwiseXor(rhs)))));
  }

  static hydra.ast.Expr encodeBitwiseXor(hydra.ext.python.syntax.BitwiseXor bxor) {
    hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor> lhs = (bxor).lhs;
    hydra.ext.python.syntax.BitwiseAnd rhs = (bxor).rhs;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.BitwiseXor, hydra.ast.Expr>) (l -> hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.ext.python.Serde.encodeBitwiseXor(l),
          hydra.Serialization.cst("^")))),
        lhs),
      hydra.util.Maybe.just(hydra.ext.python.Serde.encodeBitwiseAnd(rhs)))));
  }

  static hydra.ast.Expr encodeBlock(hydra.ext.python.syntax.Block b) {
    return (b).accept(new hydra.ext.python.syntax.Block.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Block.Indented groups) {
        return hydra.Serialization.tabIndentDoubleSpace(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.python.syntax.Statement>, hydra.ast.Expr>) (stmts -> hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
            hydra.ext.python.Serde::encodeStatement,
            stmts))),
          (groups).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Block.Simple ss) {
        return hydra.Serialization.semicolonSep(hydra.lib.lists.Map.apply(
          hydra.ext.python.Serde::encodeSimpleStatement,
          (ss).value));
      }
    });
  }

  static hydra.ast.Expr encodeCapturePattern(hydra.ext.python.syntax.CapturePattern cp) {
    return hydra.ext.python.Serde.encodePatternCaptureTarget((cp).value);
  }

  static hydra.ast.Expr encodeCaseBlock(hydra.ext.python.syntax.CaseBlock cb) {
    hydra.ext.python.syntax.Block body = (cb).body;
    hydra.util.Maybe<hydra.ext.python.syntax.Guard> guard = (cb).guard;
    hydra.ext.python.syntax.Patterns patterns = (cb).patterns;
    return hydra.Serialization.newlineSep(hydra.util.ConsList.of(
      hydra.Serialization.noSep(hydra.util.ConsList.of(
        hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
          hydra.util.Maybe.just(hydra.Serialization.cst("case")),
          hydra.util.Maybe.just(hydra.ext.python.Serde.encodePatterns(patterns)),
          hydra.lib.maybes.Map.apply(
            hydra.ext.python.Serde::encodeGuard,
            guard)))),
        hydra.Serialization.cst(":"))),
      hydra.ext.python.Serde.encodeBlock(body)));
  }

  static hydra.ast.Expr encodeClassDefinition(hydra.ext.python.syntax.ClassDefinition cd) {
    hydra.util.Maybe<hydra.ext.python.syntax.Args> args = (cd).arguments;
    hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> argPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.ext.python.syntax.Args, hydra.ast.Expr>) (a -> hydra.Serialization.noSep(hydra.util.ConsList.of(
        hydra.Serialization.cst("("),
        hydra.ext.python.Serde.encodeArgs(a),
        hydra.Serialization.cst(")")))),
      args));
    hydra.ext.python.syntax.Block body = (cd).body;
    hydra.util.Maybe<hydra.ext.python.syntax.Decorators> decs = (cd).decorators;
    hydra.ext.python.syntax.Name name = (cd).name;
    return hydra.Serialization.newlineSep(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.lib.maybes.Map.apply(
        hydra.ext.python.Serde::encodeDecorators,
        decs),
      hydra.util.Maybe.just(hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
        hydra.util.Maybe.just(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("class"),
          hydra.ext.python.Serde.encodeName(name)))),
        argPart.get(),
        hydra.util.Maybe.just(hydra.Serialization.cst(":")))))),
      hydra.util.Maybe.just(hydra.ext.python.Serde.encodeBlock(body)))));
  }

  static hydra.ast.Expr encodeClassPattern(hydra.ext.python.syntax.ClassPattern cp) {
    hydra.util.Maybe<hydra.ext.python.syntax.KeywordPatterns> kw = (cp).keywordPatterns;
    hydra.ext.python.syntax.NameOrAttribute noa = (cp).nameOrAttribute;
    hydra.util.Maybe<hydra.ext.python.syntax.PositionalPatterns> pos = (cp).positionalPatterns;
    return hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.util.Maybe.just(hydra.ext.python.Serde.encodeNameOrAttribute(noa)),
      hydra.util.Maybe.just(hydra.Serialization.cst("(")),
      hydra.lib.maybes.Map.apply(
        hydra.ext.python.Serde::encodePositionalPatterns,
        pos),
      hydra.lib.maybes.Map.apply(
        hydra.ext.python.Serde::encodeKeywordPatterns,
        kw),
      hydra.util.Maybe.just(hydra.Serialization.cst(")")))));
  }

  static hydra.ast.Expr encodeClosedPattern(hydra.ext.python.syntax.ClosedPattern cp) {
    return (cp).accept(new hydra.ext.python.syntax.ClosedPattern.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ClosedPattern.Literal ignored) {
        return hydra.Serialization.cst("...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ClosedPattern.Capture c) {
        return hydra.ext.python.Serde.encodeCapturePattern((c).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ClosedPattern.Wildcard ignored) {
        return hydra.Serialization.cst("_");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ClosedPattern.Value v) {
        return hydra.ext.python.Serde.encodeValuePattern((v).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ClosedPattern.Group ignored) {
        return hydra.Serialization.cst("(...)");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ClosedPattern.Sequence ignored) {
        return hydra.Serialization.cst("[...]");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ClosedPattern.Mapping ignored) {
        return hydra.Serialization.cst("{...}");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ClosedPattern.Class_ c) {
        return hydra.ext.python.Serde.encodeClassPattern((c).value);
      }
    });
  }

  static hydra.ast.Expr encodeComparison(hydra.ext.python.syntax.Comparison cmp) {
    return hydra.ext.python.Serde.encodeBitwiseOr((cmp).lhs);
  }

  static hydra.ast.Expr encodeConditional(hydra.ext.python.syntax.Conditional c) {
    hydra.ext.python.syntax.Disjunction body = (c).body;
    hydra.ext.python.syntax.Disjunction cond = (c).if_;
    hydra.ext.python.syntax.Expression elseExpr = (c).else_;
    return hydra.Serialization.spaceSep(hydra.util.ConsList.of(
      hydra.ext.python.Serde.encodeDisjunction(body),
      hydra.Serialization.cst("if"),
      hydra.ext.python.Serde.encodeDisjunction(cond),
      hydra.Serialization.cst("else"),
      hydra.ext.python.Serde.encodeExpression(elseExpr)));
  }

  static hydra.ast.Expr encodeCompoundStatement(hydra.ext.python.syntax.CompoundStatement cs) {
    return (cs).accept(new hydra.ext.python.syntax.CompoundStatement.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.CompoundStatement.Function f) {
        return hydra.ext.python.Serde.encodeFunctionDefinition((f).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.CompoundStatement.If ignored) {
        return hydra.Serialization.cst("if ...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.CompoundStatement.ClassDef c) {
        return hydra.ext.python.Serde.encodeClassDefinition((c).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.CompoundStatement.With ignored) {
        return hydra.Serialization.cst("with ...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.CompoundStatement.For ignored) {
        return hydra.Serialization.cst("for ...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.CompoundStatement.Try ignored) {
        return hydra.Serialization.cst("try ...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.CompoundStatement.While w) {
        return hydra.ext.python.Serde.encodeWhileStatement((w).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.CompoundStatement.Match m) {
        return hydra.ext.python.Serde.encodeMatchStatement((m).value);
      }
    });
  }

  static hydra.ast.Expr encodeConjunction(hydra.ext.python.syntax.Conjunction c) {
    return hydra.Serialization.symbolSep(
      "and",
      hydra.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.ext.python.Serde::encodeInversion,
        (c).value));
  }

  static hydra.ast.Expr encodeDecorators(hydra.ext.python.syntax.Decorators decs) {
    return hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.python.syntax.NamedExpression, hydra.ast.Expr>) (ne -> hydra.Serialization.noSep(hydra.util.ConsList.of(
        hydra.Serialization.cst("@"),
        hydra.ext.python.Serde.encodeNamedExpression(ne)))),
      (decs).value));
  }

  static hydra.ast.Expr encodeDict(hydra.ext.python.syntax.Dict d) {
    return hydra.Serialization.curlyBracesList(
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
      hydra.Serialization.halfBlockStyle(),
      hydra.lib.lists.Map.apply(
        hydra.ext.python.Serde::encodeDoubleStarredKvpair,
        (d).value));
  }

  static hydra.ast.Expr encodeDisjunction(hydra.ext.python.syntax.Disjunction d) {
    return hydra.Serialization.symbolSep(
      "or",
      hydra.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.ext.python.Serde::encodeConjunction,
        (d).value));
  }

  static hydra.ast.Expr encodeDottedAsName(hydra.ext.python.syntax.DottedAsName dan) {
    hydra.util.Maybe<hydra.ext.python.syntax.Name> alias = (dan).as;
    hydra.ext.python.syntax.DottedName name = (dan).name;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.util.Maybe.just(hydra.ext.python.Serde.encodeDottedName(name)),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.Name, hydra.ast.Expr>) (a -> hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("as"),
          hydra.ext.python.Serde.encodeName(a)))),
        alias))));
  }

  static hydra.ast.Expr encodeDottedName(hydra.ext.python.syntax.DottedName dn) {
    return hydra.Serialization.cst(hydra.lib.strings.Intercalate.apply(
      ".",
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.Name, String>) (n -> (n).value),
        (dn).value)));
  }

  static hydra.ast.Expr encodeDoubleStarredKvpair(hydra.ext.python.syntax.DoubleStarredKvpair dskv) {
    return (dskv).accept(new hydra.ext.python.syntax.DoubleStarredKvpair.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.DoubleStarredKvpair.Pair p) {
        return hydra.ext.python.Serde.encodeKvpair((p).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.DoubleStarredKvpair.Starred e) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("**"),
          hydra.ext.python.Serde.encodeBitwiseOr((e).value)));
      }
    });
  }

  static hydra.ast.Expr encodeExpression(hydra.ext.python.syntax.Expression expr) {
    return (expr).accept(new hydra.ext.python.syntax.Expression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Expression.Simple d) {
        return hydra.ext.python.Serde.encodeDisjunction((d).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Expression.Conditional c) {
        return hydra.ext.python.Serde.encodeConditional((c).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Expression.Lambda l) {
        return hydra.ext.python.Serde.encodeLambda((l).value);
      }
    });
  }

  static hydra.ast.Expr encodeFactor(hydra.ext.python.syntax.Factor f) {
    return (f).accept(new hydra.ext.python.syntax.Factor.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Factor.Positive inner) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("+"),
          hydra.ext.python.Serde.encodeFactor((inner).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Factor.Negative inner) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("-"),
          hydra.ext.python.Serde.encodeFactor((inner).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Factor.Complement inner) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("~"),
          hydra.ext.python.Serde.encodeFactor((inner).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Factor.Simple p) {
        return hydra.ext.python.Serde.encodePower((p).value);
      }
    });
  }

  static hydra.ast.Expr encodeFunctionDefRaw(hydra.ext.python.syntax.FunctionDefRaw fdr) {
    Boolean async_ = (fdr).async;
    hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> asyncKw = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      async_,
      () -> hydra.util.Maybe.just(hydra.Serialization.cst("async")),
      () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing())));
    hydra.ext.python.syntax.Block block = (fdr).block;
    hydra.ext.python.syntax.Name name = (fdr).name;
    hydra.util.Maybe<hydra.ext.python.syntax.Parameters> params = (fdr).params;
    hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> paramPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      hydra.ext.python.Serde::encodeParameters,
      params));
    hydra.util.Maybe<hydra.ext.python.syntax.Expression> retType = (fdr).returnType;
    hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> retPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ast.Expr>) (t -> hydra.Serialization.spaceSep(hydra.util.ConsList.of(
        hydra.Serialization.cst("->"),
        hydra.ext.python.Serde.encodeExpression(t)))),
      retType));
    hydra.util.ConsList<hydra.ext.python.syntax.TypeParameter> tparams = (fdr).typeParams;
    hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> tparamPart = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(tparams),
      () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
      () -> hydra.util.Maybe.just(hydra.Serialization.bracketList(
        hydra.Serialization.inlineStyle(),
        hydra.lib.lists.Map.apply(
          hydra.ext.python.Serde::encodeTypeParameter,
          tparams)))));
    return hydra.Serialization.newlineSep(hydra.util.ConsList.of(
      hydra.Serialization.noSep(hydra.util.ConsList.of(
        hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
          asyncKw.get(),
          hydra.util.Maybe.just(hydra.Serialization.cst("def")),
          hydra.util.Maybe.just(hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
            hydra.util.Maybe.just(hydra.ext.python.Serde.encodeName(name)),
            tparamPart.get(),
            hydra.util.Maybe.just(hydra.Serialization.cst("(")),
            paramPart.get(),
            hydra.util.Maybe.just(hydra.Serialization.cst(")")))))),
          retPart.get()))),
        hydra.Serialization.cst(":"))),
      hydra.ext.python.Serde.encodeBlock(block)));
  }

  static hydra.ast.Expr encodeFunctionDefinition(hydra.ext.python.syntax.FunctionDefinition fd) {
    hydra.util.Maybe<hydra.ext.python.syntax.Decorators> decs = (fd).decorators;
    hydra.ext.python.syntax.FunctionDefRaw raw = (fd).raw;
    return hydra.Serialization.newlineSep(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.lib.maybes.Map.apply(
        hydra.ext.python.Serde::encodeDecorators,
        decs),
      hydra.util.Maybe.just(hydra.ext.python.Serde.encodeFunctionDefRaw(raw)))));
  }

  static hydra.ast.Expr encodeGroup(hydra.ext.python.syntax.Group g) {
    return (g).accept(new hydra.ext.python.syntax.Group.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Group.Expression ne) {
        return hydra.ext.python.Serde.encodeNamedExpression((ne).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Group.Yield ignored) {
        return hydra.Serialization.cst("(yield ...)");
      }
    });
  }

  static hydra.ast.Expr encodeGuard(hydra.ext.python.syntax.Guard g) {
    return hydra.Serialization.spaceSep(hydra.util.ConsList.of(
      hydra.Serialization.cst("if"),
      hydra.ext.python.Serde.encodeNamedExpression((g).value)));
  }

  static hydra.ast.Expr encodeImportFrom(hydra.ext.python.syntax.ImportFrom if_) {
    hydra.util.Maybe<hydra.ext.python.syntax.DottedName> name = (if_).dottedName;
    hydra.util.ConsList<hydra.ext.python.syntax.RelativeImportPrefix> prefixes = (if_).prefixes;
    hydra.util.Lazy<hydra.ast.Expr> lhs = new hydra.util.Lazy<>(() -> hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.RelativeImportPrefix, hydra.util.Maybe<hydra.ast.Expr>>) (p -> hydra.util.Maybe.just(hydra.ext.python.Serde.encodeRelativeImportPrefix(p))),
        prefixes),
      hydra.util.ConsList.of(hydra.lib.maybes.Map.apply(
        hydra.ext.python.Serde::encodeDottedName,
        name)))))));
    hydra.ext.python.syntax.ImportFromTargets targets = (if_).targets;
    return hydra.Serialization.spaceSep(hydra.util.ConsList.of(
      hydra.Serialization.cst("from"),
      lhs.get(),
      hydra.Serialization.cst("import"),
      hydra.ext.python.Serde.encodeImportFromTargets(targets)));
  }

  static hydra.ast.Expr encodeImportFromAsName(hydra.ext.python.syntax.ImportFromAsName ifan) {
    hydra.util.Maybe<hydra.ext.python.syntax.Name> alias = (ifan).as;
    hydra.ext.python.syntax.Name name = (ifan).name;
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.ext.python.Serde.encodeName(name),
      (java.util.function.Function<hydra.ext.python.syntax.Name, hydra.ast.Expr>) (a -> hydra.Serialization.spaceSep(hydra.util.ConsList.of(
        hydra.ext.python.Serde.encodeName(name),
        hydra.Serialization.cst("as"),
        hydra.ext.python.Serde.encodeName(a)))),
      alias);
  }

  static hydra.ast.Expr encodeImportFromTargets(hydra.ext.python.syntax.ImportFromTargets t) {
    return (t).accept(new hydra.ext.python.syntax.ImportFromTargets.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ImportFromTargets.Simple names) {
        return hydra.Serialization.commaSep(
          hydra.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.python.Serde::encodeImportFromAsName,
            (names).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ImportFromTargets.Parens names) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("("),
          hydra.Serialization.commaSep(
            hydra.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.ext.python.Serde::encodeImportFromAsName,
              (names).value)),
          hydra.Serialization.cst(")")));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ImportFromTargets.Star ignored) {
        return hydra.Serialization.cst("*");
      }
    });
  }

  static hydra.ast.Expr encodeImportName(hydra.ext.python.syntax.ImportName in_) {
    return hydra.Serialization.spaceSep(hydra.util.ConsList.of(
      hydra.Serialization.cst("import"),
      hydra.Serialization.commaSep(
        hydra.Serialization.inlineStyle(),
        hydra.lib.lists.Map.apply(
          hydra.ext.python.Serde::encodeDottedAsName,
          (in_).value))));
  }

  static hydra.ast.Expr encodeImportStatement(hydra.ext.python.syntax.ImportStatement is_) {
    return (is_).accept(new hydra.ext.python.syntax.ImportStatement.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ImportStatement.Name n) {
        return hydra.ext.python.Serde.encodeImportName((n).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ImportStatement.From f) {
        return hydra.ext.python.Serde.encodeImportFrom((f).value);
      }
    });
  }

  static hydra.ast.Expr encodeInversion(hydra.ext.python.syntax.Inversion i) {
    return (i).accept(new hydra.ext.python.syntax.Inversion.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Inversion.Not other) {
        return hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("not"),
          hydra.ext.python.Serde.encodeInversion((other).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Inversion.Simple c) {
        return hydra.ext.python.Serde.encodeComparison((c).value);
      }
    });
  }

  static hydra.ast.Expr encodeKeywordPattern(hydra.ext.python.syntax.KeywordPattern kp) {
    hydra.ext.python.syntax.Name name = (kp).name;
    hydra.ext.python.syntax.Pattern pat = (kp).pattern;
    return hydra.Serialization.noSep(hydra.util.ConsList.of(
      hydra.ext.python.Serde.encodeName(name),
      hydra.Serialization.cst("="),
      hydra.ext.python.Serde.encodePattern(pat)));
  }

  static hydra.ast.Expr encodeKeywordPatterns(hydra.ext.python.syntax.KeywordPatterns kp) {
    return hydra.Serialization.commaSep(
      hydra.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.ext.python.Serde::encodeKeywordPattern,
        (kp).value));
  }

  static hydra.ast.Expr encodeKvpair(hydra.ext.python.syntax.Kvpair kv) {
    hydra.ext.python.syntax.Expression k = (kv).key;
    hydra.ext.python.syntax.Expression v = (kv).value;
    return hydra.Serialization.spaceSep(hydra.util.ConsList.of(
      hydra.Serialization.noSep(hydra.util.ConsList.of(
        hydra.ext.python.Serde.encodeExpression(k),
        hydra.Serialization.cst(":"))),
      hydra.ext.python.Serde.encodeExpression(v)));
  }

  static hydra.ast.Expr encodeKwarg(hydra.ext.python.syntax.Kwarg k) {
    hydra.ext.python.syntax.Expression expr = (k).value;
    hydra.ext.python.syntax.Name name = (k).name;
    return hydra.Serialization.noSep(hydra.util.ConsList.of(
      hydra.ext.python.Serde.encodeName(name),
      hydra.Serialization.cst("="),
      hydra.ext.python.Serde.encodeExpression(expr)));
  }

  static hydra.ast.Expr encodeKwargOrDoubleStarred(hydra.ext.python.syntax.KwargOrDoubleStarred kds) {
    return (kds).accept(new hydra.ext.python.syntax.KwargOrDoubleStarred.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.KwargOrDoubleStarred.Kwarg k) {
        return hydra.ext.python.Serde.encodeKwarg((k).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.KwargOrDoubleStarred.DoubleStarred e) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("**"),
          hydra.ext.python.Serde.encodeExpression((e).value)));
      }
    });
  }

  static hydra.ast.Expr encodeKwargOrStarred(hydra.ext.python.syntax.KwargOrStarred ks) {
    return (ks).accept(new hydra.ext.python.syntax.KwargOrStarred.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.KwargOrStarred.Kwarg k) {
        return hydra.ext.python.Serde.encodeKwarg((k).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.KwargOrStarred.Starred se) {
        return hydra.ext.python.Serde.encodeStarredExpression((se).value);
      }
    });
  }

  static hydra.ast.Expr encodeLambda(hydra.ext.python.syntax.Lambda l) {
    hydra.ext.python.syntax.Expression body = (l).body;
    hydra.ext.python.syntax.LambdaParameters params = (l).params;
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(hydra.util.ConsList.of(
      hydra.Serialization.cst("lambda"),
      hydra.Serialization.noSep(hydra.util.ConsList.of(
        hydra.ext.python.Serde.encodeLambdaParameters(params),
        hydra.Serialization.cst(":"))),
      hydra.ext.python.Serde.encodeExpression(body))));
  }

  static hydra.ast.Expr encodeLambdaParamNoDefault(hydra.ext.python.syntax.LambdaParamNoDefault p) {
    return hydra.ext.python.Serde.encodeName((p).value);
  }

  static hydra.ast.Expr encodeLambdaParameters(hydra.ext.python.syntax.LambdaParameters lp) {
    hydra.util.ConsList<hydra.ext.python.syntax.LambdaParamNoDefault> nodef = (lp).paramNoDefault;
    return hydra.Serialization.commaSep(
      hydra.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.ext.python.Serde::encodeLambdaParamNoDefault,
        nodef));
  }

  static hydra.ast.Expr encodeLambdaStarEtc(hydra.ext.python.syntax.LambdaStarEtc lse) {
    return (lse).accept(new hydra.ext.python.syntax.LambdaStarEtc.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.LambdaStarEtc.ParamNoDefault p) {
        return hydra.ext.python.Serde.encodeLambdaParamNoDefault((p).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.LambdaStarEtc.Star ignored) {
        return hydra.Serialization.cst("*...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.LambdaStarEtc.ParamMaybeDefault ignored) {
        return hydra.Serialization.cst("...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.LambdaStarEtc.Kwds ignored) {
        return hydra.Serialization.cst("**...");
      }
    });
  }

  static hydra.ast.Expr encodeList(hydra.ext.python.syntax.List l) {
    return hydra.Serialization.bracketListAdaptive(hydra.lib.lists.Map.apply(
      hydra.ext.python.Serde::encodeStarNamedExpression,
      (l).value));
  }

  static hydra.ast.Expr encodeMatchStatement(hydra.ext.python.syntax.MatchStatement ms) {
    hydra.util.ConsList<hydra.ext.python.syntax.CaseBlock> cases = (ms).cases;
    hydra.ext.python.syntax.SubjectExpression subj = (ms).subject;
    return hydra.Serialization.newlineSep(hydra.util.ConsList.of(
      hydra.Serialization.spaceSep(hydra.util.ConsList.of(
        hydra.Serialization.cst("match"),
        hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.ext.python.Serde.encodeSubjectExpression(subj),
          hydra.Serialization.cst(":"))))),
      hydra.Serialization.tabIndentDoubleSpace(hydra.lib.lists.Map.apply(
        hydra.ext.python.Serde::encodeCaseBlock,
        cases))));
  }

  static hydra.ast.Expr encodeModule(hydra.ext.python.syntax.Module mod) {
    hydra.util.Lazy<hydra.util.ConsList<hydra.ast.Expr>> groups = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.ConsList<hydra.ext.python.syntax.Statement>, hydra.ast.Expr>) (group -> hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
        hydra.ext.python.Serde::encodeStatement,
        group))),
      (mod).value));
    hydra.ast.Expr warning = hydra.Serialization.cst(hydra.ext.python.Serde.toPythonComments(hydra.Constants.warningAutoGeneratedFile()));
    return hydra.Serialization.doubleNewlineSep(hydra.lib.lists.Cons.apply(
      warning,
      groups.get()));
  }

  static hydra.ast.Expr encodeName(hydra.ext.python.syntax.Name n) {
    return hydra.Serialization.cst((n).value);
  }

  static hydra.ast.Expr encodeNamedExpression(hydra.ext.python.syntax.NamedExpression ne) {
    return (ne).accept(new hydra.ext.python.syntax.NamedExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.NamedExpression.Simple e) {
        return hydra.ext.python.Serde.encodeExpression((e).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.NamedExpression.Assignment ae) {
        return hydra.ext.python.Serde.encodeAssignmentExpression((ae).value);
      }
    });
  }

  static hydra.ast.Expr encodeNameOrAttribute(hydra.ext.python.syntax.NameOrAttribute noa) {
    return hydra.Serialization.dotSep(hydra.lib.lists.Map.apply(
      hydra.ext.python.Serde::encodeName,
      (noa).value));
  }

  static hydra.ast.Expr encodeNumber(hydra.ext.python.syntax.Number_ num) {
    return (num).accept(new hydra.ext.python.syntax.Number_.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Number_.Float_ f) {
        return hydra.Serialization.cst(hydra.lib.literals.ShowBigfloat.apply((f).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Number_.Integer_ i) {
        return hydra.Serialization.cst(hydra.lib.literals.ShowBigint.apply((i).value));
      }
    });
  }

  static hydra.ast.Expr encodeOrPattern(hydra.ext.python.syntax.OrPattern op) {
    return hydra.Serialization.symbolSep(
      "|",
      hydra.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.ext.python.Serde::encodeClosedPattern,
        (op).value));
  }

  static hydra.ast.Expr encodeParam(hydra.ext.python.syntax.Param p) {
    hydra.util.Maybe<hydra.ext.python.syntax.Annotation> ann = (p).annotation;
    hydra.ext.python.syntax.Name name = (p).name;
    return hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.util.Maybe.just(hydra.ext.python.Serde.encodeName(name)),
      hydra.lib.maybes.Map.apply(
        hydra.ext.python.Serde::encodeAnnotation,
        ann))));
  }

  static hydra.ast.Expr encodeParamNoDefault(hydra.ext.python.syntax.ParamNoDefault pnd) {
    return hydra.ext.python.Serde.encodeParam((pnd).param);
  }

  static hydra.ast.Expr encodeParamNoDefaultParameters(hydra.ext.python.syntax.ParamNoDefaultParameters pndp) {
    hydra.util.ConsList<hydra.ext.python.syntax.ParamNoDefault> nodef = (pndp).paramNoDefault;
    return hydra.Serialization.commaSep(
      hydra.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.ext.python.Serde::encodeParamNoDefault,
        nodef));
  }

  static hydra.ast.Expr encodeParameters(hydra.ext.python.syntax.Parameters p) {
    return (p).accept(new hydra.ext.python.syntax.Parameters.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Parameters.ParamNoDefault pnd) {
        return hydra.ext.python.Serde.encodeParamNoDefaultParameters((pnd).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Parameters.SlashNoDefault ignored) {
        return hydra.Serialization.cst("...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Parameters.SlashWithDefault ignored) {
        return hydra.Serialization.cst("...");
      }
    });
  }

  static hydra.ast.Expr encodePattern(hydra.ext.python.syntax.Pattern p) {
    return (p).accept(new hydra.ext.python.syntax.Pattern.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Pattern.Or op) {
        return hydra.ext.python.Serde.encodeOrPattern((op).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Pattern.As ignored) {
        return hydra.Serialization.cst("... as ...");
      }
    });
  }

  static hydra.ast.Expr encodePatternCaptureTarget(hydra.ext.python.syntax.PatternCaptureTarget pct) {
    return hydra.ext.python.Serde.encodeName((pct).value);
  }

  static hydra.ast.Expr encodePatterns(hydra.ext.python.syntax.Patterns ps) {
    return (ps).accept(new hydra.ext.python.syntax.Patterns.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Patterns.Pattern p) {
        return hydra.ext.python.Serde.encodePattern((p).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Patterns.Sequence ignored) {
        return hydra.Serialization.cst("...");
      }
    });
  }

  static hydra.ast.Expr encodePosArg(hydra.ext.python.syntax.PosArg pa) {
    return (pa).accept(new hydra.ext.python.syntax.PosArg.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.PosArg.Starred se) {
        return hydra.ext.python.Serde.encodeStarredExpression((se).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.PosArg.Assignment ae) {
        return hydra.ext.python.Serde.encodeAssignmentExpression((ae).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.PosArg.Expression e) {
        return hydra.ext.python.Serde.encodeExpression((e).value);
      }
    });
  }

  static hydra.ast.Expr encodePositionalPatterns(hydra.ext.python.syntax.PositionalPatterns pp) {
    return hydra.Serialization.commaSep(
      hydra.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.ext.python.Serde::encodePattern,
        (pp).value));
  }

  static hydra.ast.Expr encodePower(hydra.ext.python.syntax.Power p) {
    hydra.ext.python.syntax.AwaitPrimary lhs = (p).lhs;
    hydra.util.Maybe<hydra.ext.python.syntax.Factor> rhs = (p).rhs;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.util.Maybe.just(hydra.ext.python.Serde.encodeAwaitPrimary(lhs)),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.Factor, hydra.ast.Expr>) (r -> hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("**"),
          hydra.ext.python.Serde.encodeFactor(r)))),
        rhs))));
  }

  static hydra.ast.Expr encodePrimary(hydra.ext.python.syntax.Primary p) {
    return (p).accept(new hydra.ext.python.syntax.Primary.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Primary.Simple a) {
        return hydra.ext.python.Serde.encodeAtom((a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Primary.Compound pwr) {
        return hydra.ext.python.Serde.encodePrimaryWithRhs((pwr).value);
      }
    });
  }

  static hydra.ast.Expr encodePrimaryRhs(hydra.ext.python.syntax.PrimaryRhs rhs) {
    return (rhs).accept(new hydra.ext.python.syntax.PrimaryRhs.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.PrimaryRhs.Call args) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("("),
          hydra.ext.python.Serde.encodeArgs((args).value),
          hydra.Serialization.cst(")")));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.PrimaryRhs.Project name) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("."),
          hydra.ext.python.Serde.encodeName((name).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.PrimaryRhs.Slices slices) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("["),
          hydra.ext.python.Serde.encodeSlices((slices).value),
          hydra.Serialization.cst("]")));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.PrimaryRhs.Genexp ignored) {
        return hydra.Serialization.cst("[...]");
      }
    });
  }

  static hydra.ast.Expr encodePrimaryWithRhs(hydra.ext.python.syntax.PrimaryWithRhs pwr) {
    hydra.ext.python.syntax.Primary prim = (pwr).primary;
    hydra.ext.python.syntax.PrimaryRhs rhs = (pwr).rhs;
    return hydra.Serialization.noSep(hydra.util.ConsList.of(
      hydra.ext.python.Serde.encodePrimary(prim),
      hydra.ext.python.Serde.encodePrimaryRhs(rhs)));
  }

  static hydra.ast.Expr encodeRaiseExpression(hydra.ext.python.syntax.RaiseExpression re) {
    hydra.ext.python.syntax.Expression expr = (re).expression;
    hydra.util.Maybe<hydra.ext.python.syntax.Expression> from_ = (re).from;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.util.Maybe.just(hydra.ext.python.Serde.encodeExpression(expr)),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ast.Expr>) (f -> hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("from"),
          hydra.ext.python.Serde.encodeExpression(f)))),
        from_))));
  }

  static hydra.ast.Expr encodeRaiseStatement(hydra.ext.python.syntax.RaiseStatement rs) {
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.util.Maybe.just(hydra.Serialization.cst("raise")),
      hydra.lib.maybes.Map.apply(
        hydra.ext.python.Serde::encodeRaiseExpression,
        (rs).value))));
  }

  static hydra.ast.Expr encodeRelativeImportPrefix(hydra.ext.python.syntax.RelativeImportPrefix p) {
    return (p).accept(new hydra.ext.python.syntax.RelativeImportPrefix.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.RelativeImportPrefix.Dot ignored) {
        return hydra.Serialization.cst(".");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.RelativeImportPrefix.Ellipsis ignored) {
        return hydra.Serialization.cst("...");
      }
    });
  }

  static hydra.ast.Expr encodeReturnStatement(hydra.ext.python.syntax.ReturnStatement rs) {
    return hydra.Serialization.spaceSep(hydra.util.ConsList.of(
      hydra.Serialization.cst("return"),
      hydra.Serialization.commaSep(
        hydra.Serialization.inlineStyle(),
        hydra.lib.lists.Map.apply(
          hydra.ext.python.Serde::encodeStarExpression,
          (rs).value))));
  }

  static hydra.ast.Expr encodeSet(hydra.ext.python.syntax.Set s) {
    return hydra.Serialization.bracesListAdaptive(hydra.lib.lists.Map.apply(
      hydra.ext.python.Serde::encodeStarNamedExpression,
      (s).value));
  }

  static hydra.ast.Expr encodeShiftExpression(hydra.ext.python.syntax.ShiftExpression se) {
    return hydra.ext.python.Serde.encodeSum((se).rhs);
  }

  static hydra.ast.Expr encodeSimpleStatement(hydra.ext.python.syntax.SimpleStatement ss) {
    return (ss).accept(new hydra.ext.python.syntax.SimpleStatement.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Assignment a) {
        return hydra.ext.python.Serde.encodeAssignment((a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.StarExpressions es) {
        return hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
          hydra.ext.python.Serde::encodeStarExpression,
          (es).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Return r) {
        return hydra.ext.python.Serde.encodeReturnStatement((r).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Raise r) {
        return hydra.ext.python.Serde.encodeRaiseStatement((r).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Pass ignored) {
        return hydra.Serialization.cst("pass");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Break ignored) {
        return hydra.Serialization.cst("break");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Continue ignored) {
        return hydra.Serialization.cst("continue");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Import i) {
        return hydra.ext.python.Serde.encodeImportStatement((i).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.TypeAlias t) {
        return hydra.ext.python.Serde.encodeTypeAlias((t).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Assert ignored) {
        return hydra.Serialization.cst("assert ...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Global ignored) {
        return hydra.Serialization.cst("global ...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Nonlocal ignored) {
        return hydra.Serialization.cst("nonlocal ...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Del ignored) {
        return hydra.Serialization.cst("del ...");
      }
    });
  }

  static hydra.ast.Expr encodeSimpleTypeParameter(hydra.ext.python.syntax.SimpleTypeParameter stp) {
    return hydra.ext.python.Serde.encodeName((stp).name);
  }

  static hydra.ast.Expr encodeSingleTarget(hydra.ext.python.syntax.SingleTarget st) {
    return (st).accept(new hydra.ext.python.syntax.SingleTarget.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SingleTarget.Name n) {
        return hydra.ext.python.Serde.encodeName((n).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SingleTarget.Parens ignored) {
        return hydra.Serialization.cst("(...)");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SingleTarget.SubscriptAttributeTarget ignored) {
        return hydra.Serialization.cst("...");
      }
    });
  }

  static hydra.ast.Expr encodeSlice(hydra.ext.python.syntax.Slice s) {
    return (s).accept(new hydra.ext.python.syntax.Slice.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Slice.Named ne) {
        return hydra.ext.python.Serde.encodeNamedExpression((ne).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Slice.Slice_ ignored) {
        return hydra.Serialization.cst(":");
      }
    });
  }

  static hydra.ast.Expr encodeSliceOrStarredExpression(hydra.ext.python.syntax.SliceOrStarredExpression s) {
    return (s).accept(new hydra.ext.python.syntax.SliceOrStarredExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SliceOrStarredExpression.Slice sl) {
        return hydra.ext.python.Serde.encodeSlice((sl).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SliceOrStarredExpression.Starred se) {
        return hydra.ext.python.Serde.encodeStarredExpression((se).value);
      }
    });
  }

  static hydra.ast.Expr encodeSlices(hydra.ext.python.syntax.Slices s) {
    hydra.ext.python.syntax.Slice hd = (s).head;
    hydra.util.ConsList<hydra.ext.python.syntax.SliceOrStarredExpression> tl = (s).tail;
    return hydra.Serialization.commaSep(
      hydra.Serialization.inlineStyle(),
      hydra.lib.lists.Cons.apply(
        hydra.ext.python.Serde.encodeSlice(hd),
        hydra.lib.lists.Map.apply(
          hydra.ext.python.Serde::encodeSliceOrStarredExpression,
          tl)));
  }

  static hydra.ast.Expr encodeStarAtom(hydra.ext.python.syntax.StarAtom sa) {
    return (sa).accept(new hydra.ext.python.syntax.StarAtom.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarAtom.Name n) {
        return hydra.ext.python.Serde.encodeName((n).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarAtom.TargetWithStarAtom ignored) {
        return hydra.Serialization.cst("(...)");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarAtom.StarTargetsTupleSeq ignored) {
        return hydra.Serialization.cst("(...)");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarAtom.StarTargetsListSeq ignored) {
        return hydra.Serialization.cst("[...]");
      }
    });
  }

  static hydra.ast.Expr encodeStarExpression(hydra.ext.python.syntax.StarExpression se) {
    return (se).accept(new hydra.ext.python.syntax.StarExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarExpression.Star bor) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("*"),
          hydra.ext.python.Serde.encodeBitwiseOr((bor).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarExpression.Simple e) {
        return hydra.ext.python.Serde.encodeExpression((e).value);
      }
    });
  }

  static hydra.ast.Expr encodeStarNamedExpression(hydra.ext.python.syntax.StarNamedExpression sne) {
    return (sne).accept(new hydra.ext.python.syntax.StarNamedExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarNamedExpression.Star bor) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("*"),
          hydra.ext.python.Serde.encodeBitwiseOr((bor).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarNamedExpression.Simple ne) {
        return hydra.ext.python.Serde.encodeNamedExpression((ne).value);
      }
    });
  }

  static hydra.ast.Expr encodeStarTarget(hydra.ext.python.syntax.StarTarget st) {
    return (st).accept(new hydra.ext.python.syntax.StarTarget.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarTarget.Unstarred t) {
        return hydra.ext.python.Serde.encodeTargetWithStarAtom((t).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarTarget.Starred inner) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("*"),
          hydra.ext.python.Serde.encodeStarTarget((inner).value)));
      }
    });
  }

  static hydra.ast.Expr encodeStarredExpression(hydra.ext.python.syntax.StarredExpression se) {
    return hydra.Serialization.noSep(hydra.util.ConsList.of(
      hydra.Serialization.cst("*"),
      hydra.ext.python.Serde.encodeExpression((se).value)));
  }

  static hydra.ast.Expr encodeStatement(hydra.ext.python.syntax.Statement stmt) {
    return (stmt).accept(new hydra.ext.python.syntax.Statement.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Statement.Annotated a) {
        return hydra.ext.python.Serde.encodeAnnotatedStatement((a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Statement.Simple ss) {
        return hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
          hydra.ext.python.Serde::encodeSimpleStatement,
          (ss).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Statement.Compound c) {
        return hydra.ext.python.Serde.encodeCompoundStatement((c).value);
      }
    });
  }

  static hydra.ast.Expr encodeString(hydra.ext.python.syntax.String_ s) {
    String content = (s).value;
    hydra.ext.python.syntax.QuoteStyle style = (s).quoteStyle;
    return (style).accept(new hydra.ext.python.syntax.QuoteStyle.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.QuoteStyle.Single ignored) {
        return hydra.Serialization.cst(hydra.ext.python.Serde.escapePythonString(
          false,
          content));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.QuoteStyle.Double_ ignored) {
        return hydra.Serialization.cst(hydra.ext.python.Serde.escapePythonString(
          true,
          content));
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.QuoteStyle.Triple ignored) {
        return hydra.Serialization.noSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("r\"\"\""),
          hydra.Serialization.cst(content),
          hydra.Serialization.cst("\"\"\"")));
      }
    });
  }

  static hydra.ast.Expr encodeSubjectExpression(hydra.ext.python.syntax.SubjectExpression se) {
    return (se).accept(new hydra.ext.python.syntax.SubjectExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SubjectExpression.Simple ne) {
        return hydra.ext.python.Serde.encodeNamedExpression((ne).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SubjectExpression.Tuple ignored) {
        return hydra.Serialization.cst("*...");
      }
    });
  }

  static hydra.ast.Expr encodeSum(hydra.ext.python.syntax.Sum s) {
    return hydra.ext.python.Serde.encodeTerm((s).rhs);
  }

  static hydra.ast.Expr encodeTerm(hydra.ext.python.syntax.Term t) {
    return hydra.ext.python.Serde.encodeFactor((t).rhs);
  }

  static hydra.ast.Expr encodeTargetWithStarAtom(hydra.ext.python.syntax.TargetWithStarAtom t) {
    return (t).accept(new hydra.ext.python.syntax.TargetWithStarAtom.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.TargetWithStarAtom.Atom a) {
        return hydra.ext.python.Serde.encodeStarAtom((a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.TargetWithStarAtom.Project pn) {
        return hydra.ext.python.Serde.encodeTPrimaryAndName((pn).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.TargetWithStarAtom.Slices ignored) {
        return hydra.Serialization.cst("...");
      }
    });
  }

  static hydra.ast.Expr encodeTPrimaryAndName(hydra.ext.python.syntax.TPrimaryAndName pn) {
    hydra.ext.python.syntax.Name name_ = (pn).name;
    hydra.ext.python.syntax.TPrimary prim = (pn).primary;
    return hydra.Serialization.noSep(hydra.util.ConsList.of(
      hydra.ext.python.Serde.encodeTPrimary(prim),
      hydra.Serialization.cst("."),
      hydra.ext.python.Serde.encodeName(name_)));
  }

  static hydra.ast.Expr encodeTPrimary(hydra.ext.python.syntax.TPrimary tp) {
    return (tp).accept(new hydra.ext.python.syntax.TPrimary.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.TPrimary.Atom a) {
        return hydra.ext.python.Serde.encodeAtom((a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.TPrimary.PrimaryAndName pn) {
        return hydra.ext.python.Serde.encodeTPrimaryAndName((pn).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.TPrimary.PrimaryAndSlices ignored) {
        return hydra.Serialization.cst("...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.TPrimary.PrimaryAndGenexp ignored) {
        return hydra.Serialization.cst("...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.TPrimary.PrimaryAndArguments ignored) {
        return hydra.Serialization.cst("...");
      }
    });
  }

  static hydra.ast.Expr encodeTuple(hydra.ext.python.syntax.Tuple t) {
    hydra.util.ConsList<hydra.ext.python.syntax.StarNamedExpression> es = (t).value;
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Length.apply(es),
        1),
      () -> hydra.Serialization.parens(hydra.Serialization.noSep(hydra.util.ConsList.of(
        hydra.ext.python.Serde.encodeStarNamedExpression(hydra.lib.lists.Head.apply(es)),
        hydra.Serialization.cst(",")))),
      () -> hydra.Serialization.parenList(
        false,
        hydra.lib.lists.Map.apply(
          hydra.ext.python.Serde::encodeStarNamedExpression,
          es)));
  }

  static hydra.ast.Expr encodeTypeAlias(hydra.ext.python.syntax.TypeAlias ta) {
    hydra.ext.python.syntax.Name name = (ta).name;
    hydra.util.ConsList<hydra.ext.python.syntax.TypeParameter> tparams = (ta).typeParams;
    hydra.util.Lazy<hydra.ast.Expr> alias = new hydra.util.Lazy<>(() -> hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.util.Maybe.just(hydra.ext.python.Serde.encodeName(name)),
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(tparams),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.bracketList(
          hydra.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.python.Serde::encodeTypeParameter,
            tparams))))))));
    hydra.ext.python.syntax.Expression expr = (ta).expression;
    return hydra.Serialization.spaceSep(hydra.util.ConsList.of(
      hydra.Serialization.cst("type"),
      alias.get(),
      hydra.Serialization.cst("="),
      hydra.ext.python.Serde.encodeExpression(expr)));
  }

  static hydra.ast.Expr encodeTypeParameter(hydra.ext.python.syntax.TypeParameter tp) {
    return (tp).accept(new hydra.ext.python.syntax.TypeParameter.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.TypeParameter.Simple s) {
        return hydra.ext.python.Serde.encodeSimpleTypeParameter((s).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.TypeParameter.Star ignored) {
        return hydra.Serialization.cst("*...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.TypeParameter.DoubleStar ignored) {
        return hydra.Serialization.cst("**...");
      }
    });
  }

  static hydra.ast.Expr encodeTypedAssignment(hydra.ext.python.syntax.TypedAssignment ta) {
    hydra.ext.python.syntax.SingleTarget lhs = (ta).lhs;
    hydra.util.Maybe<hydra.ext.python.syntax.AnnotatedRhs> rhs = (ta).rhs;
    hydra.ext.python.syntax.Expression typ = (ta).type;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.util.Maybe.just(hydra.Serialization.noSep(hydra.util.ConsList.of(
        hydra.ext.python.Serde.encodeSingleTarget(lhs),
        hydra.Serialization.cst(":")))),
      hydra.util.Maybe.just(hydra.ext.python.Serde.encodeExpression(typ)),
      hydra.lib.maybes.Map.apply(
        hydra.ext.python.Serde::encodeAnnotatedRhs,
        rhs))));
  }

  static hydra.ast.Expr encodeUntypedAssignment(hydra.ext.python.syntax.UntypedAssignment ua) {
    hydra.ext.python.syntax.AnnotatedRhs rhs = (ua).rhs;
    hydra.util.ConsList<hydra.ext.python.syntax.StarTarget> targets = (ua).targets;
    return hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(hydra.util.ConsList.of(
      hydra.lib.lists.Map.apply(
        hydra.ext.python.Serde::encodeStarTarget,
        targets),
      hydra.util.ConsList.of(hydra.ext.python.Serde.encodeAnnotatedRhs(rhs)))));
  }

  static hydra.ast.Expr encodeValuePattern(hydra.ext.python.syntax.ValuePattern vp) {
    return hydra.ext.python.Serde.encodeAttribute((vp).value);
  }

  static hydra.ast.Expr encodeWhileStatement(hydra.ext.python.syntax.WhileStatement ws) {
    hydra.ext.python.syntax.Block body = (ws).body;
    hydra.ext.python.syntax.NamedExpression cond = (ws).condition;
    hydra.util.Maybe<hydra.ext.python.syntax.Block> else_ = (ws).else_;
    return hydra.Serialization.newlineSep(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
      hydra.util.Maybe.just(hydra.Serialization.newlineSep(hydra.util.ConsList.of(
        hydra.Serialization.spaceSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("while"),
          hydra.Serialization.noSep(hydra.util.ConsList.of(
            hydra.ext.python.Serde.encodeNamedExpression(cond),
            hydra.Serialization.cst(":"))))),
        hydra.ext.python.Serde.encodeBlock(body)))),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.Block, hydra.ast.Expr>) (eb -> hydra.Serialization.newlineSep(hydra.util.ConsList.of(
          hydra.Serialization.cst("else:"),
          hydra.ext.python.Serde.encodeBlock(eb)))),
        else_))));
  }

  static String escapePythonString(Boolean doubleQuoted, String s) {
    java.util.function.Function<String, java.util.function.Function<String, java.util.function.Function<String, String>>> replace = (java.util.function.Function<String, java.util.function.Function<String, java.util.function.Function<String, String>>>) (old -> (java.util.function.Function<String, java.util.function.Function<String, String>>) (new_ -> (java.util.function.Function<String, String>) (str -> hydra.lib.strings.Intercalate.apply(
      new_,
      hydra.lib.strings.SplitOn.apply(
        old,
        str)))));
    String s1 = (replace).apply("\\").apply("\\\\").apply(s);
    String s2 = (replace).apply("\u0000").apply("\\x00").apply(s1);
    String s3 = (replace).apply("\n").apply("\\n").apply(s2);
    String s4 = (replace).apply("\t").apply("\\t").apply(s3);
    String s5 = (replace).apply("\r").apply("\\r").apply(s4);
    hydra.util.Lazy<String> escaped = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      doubleQuoted,
      () -> (replace).apply("\"").apply("\\\"").apply(s5),
      () -> (replace).apply("'").apply("\\'").apply(s5)));
    hydra.util.Lazy<String> quote = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      doubleQuoted,
      () -> "\"",
      () -> "'"));
    return hydra.lib.strings.Cat2.apply(
      quote.get(),
      hydra.lib.strings.Cat2.apply(
        escaped.get(),
        quote.get()));
  }

  static String toPythonComments(String doc_) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        doc_,
        ""),
      () -> "",
      () -> hydra.lib.strings.Intercalate.apply(
        "\n",
        hydra.lib.lists.Map.apply(
          (java.util.function.Function<String, String>) (line -> hydra.lib.strings.Cat2.apply(
            "# ",
            line)),
          hydra.lib.strings.Lines.apply(doc_))));
  }
}
