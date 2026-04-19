// Note: this is an automatically generated file. Do not edit.

package hydra.python;

/**
 * Python serializer: converts Python AST to concrete syntax
 */
public interface Serde {
  static hydra.ast.Expr encodeAnnotatedRhs(hydra.python.syntax.AnnotatedRhs arhs) {
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst("="),
      (arhs).accept(new hydra.python.syntax.AnnotatedRhs.PartialVisitor<>() {
        @Override
        public hydra.ast.Expr visit(hydra.python.syntax.AnnotatedRhs.Star ses) {
          return hydra.Serialization.commaSep(
            hydra.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.python.Serde::encodeStarExpression,
              (ses).value));
        }

        @Override
        public hydra.ast.Expr visit(hydra.python.syntax.AnnotatedRhs.Yield ignored) {
          return hydra.Serialization.cst("yield ...");
        }
      })));
  }

  static hydra.ast.Expr encodeAnnotatedStatement(hydra.python.syntax.AnnotatedStatement as_) {
    String doc_ = (as_).comment;
    hydra.python.syntax.Statement stmt = (as_).statement;
    return hydra.Serialization.newlineSep(java.util.Arrays.asList(
      hydra.Serialization.cst(hydra.python.Serde.toPythonComments(doc_)),
      hydra.python.Serde.encodeStatement(stmt)));
  }

  static hydra.ast.Expr encodeAnnotation(hydra.python.syntax.Annotation ann) {
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst(":"),
      hydra.python.Serde.encodeExpression((ann).value)));
  }

  static hydra.ast.Expr encodeArgs(hydra.python.syntax.Args args) {
    java.util.List<hydra.python.syntax.KwargOrStarred> ks = (args).kwargOrStarred;
    java.util.List<hydra.python.syntax.KwargOrDoubleStarred> kss = (args).kwargOrDoubleStarred;
    java.util.List<hydra.python.syntax.PosArg> pos = (args).positional;
    return hydra.Serialization.commaSep(
      hydra.Serialization.inlineStyle(),
      hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
        hydra.lib.lists.Map.apply(
          hydra.python.Serde::encodePosArg,
          pos),
        hydra.lib.lists.Map.apply(
          hydra.python.Serde::encodeKwargOrStarred,
          ks),
        hydra.lib.lists.Map.apply(
          hydra.python.Serde::encodeKwargOrDoubleStarred,
          kss))));
  }

  static hydra.ast.Expr encodeAssignment(hydra.python.syntax.Assignment a) {
    return (a).accept(new hydra.python.syntax.Assignment.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Assignment.Typed t) {
        return hydra.python.Serde.encodeTypedAssignment((t).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Assignment.Untyped u) {
        return hydra.python.Serde.encodeUntypedAssignment((u).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Assignment.Aug ignored) {
        return hydra.Serialization.cst("... += ...");
      }
    });
  }

  static hydra.ast.Expr encodeAssignmentExpression(hydra.python.syntax.AssignmentExpression ae) {
    hydra.python.syntax.Expression expr = (ae).expression;
    hydra.python.syntax.Name name = (ae).name;
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.python.Serde.encodeName(name),
      hydra.Serialization.cst(":="),
      hydra.python.Serde.encodeExpression(expr)));
  }

  static hydra.ast.Expr encodeAtom(hydra.python.syntax.Atom atom) {
    return (atom).accept(new hydra.python.syntax.Atom.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Atom.Dict d) {
        return hydra.python.Serde.encodeDict((d).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Atom.Dictcomp ignored) {
        return hydra.Serialization.cst("{...}");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Atom.Ellipsis ignored) {
        return hydra.Serialization.cst("...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Atom.False ignored) {
        return hydra.Serialization.cst("False");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Atom.Genexp ignored) {
        return hydra.Serialization.cst("(...)");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Atom.Group g) {
        return hydra.python.Serde.encodeGroup((g).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Atom.List l) {
        return hydra.python.Serde.encodeList((l).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Atom.Listcomp ignored) {
        return hydra.Serialization.cst("[...]");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Atom.Name n) {
        return hydra.python.Serde.encodeName((n).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Atom.None ignored) {
        return hydra.Serialization.cst("None");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Atom.Number_ n) {
        return hydra.python.Serde.encodeNumber((n).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Atom.Set s) {
        return hydra.python.Serde.encodeSet((s).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Atom.Setcomp ignored) {
        return hydra.Serialization.cst("{...}");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Atom.String_ s) {
        return hydra.python.Serde.encodeString((s).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Atom.True ignored) {
        return hydra.Serialization.cst("True");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Atom.Tuple t) {
        return hydra.python.Serde.encodeTuple((t).value);
      }
    });
  }

  static hydra.ast.Expr encodeAttribute(hydra.python.syntax.Attribute attr) {
    return hydra.Serialization.dotSep(hydra.lib.lists.Map.apply(
      hydra.python.Serde::encodeName,
      (attr).value));
  }

  static hydra.ast.Expr encodeAwaitPrimary(hydra.python.syntax.AwaitPrimary ap) {
    Boolean await_ = (ap).await;
    hydra.python.syntax.Primary primary = (ap).primary;
    return hydra.lib.logic.IfElse.lazy(
      await_,
      () -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
        hydra.Serialization.cst("await"),
        hydra.python.Serde.encodePrimary(primary))),
      () -> hydra.python.Serde.encodePrimary(primary));
  }

  static hydra.ast.Expr encodeBitwiseAnd(hydra.python.syntax.BitwiseAnd band) {
    hydra.util.Maybe<hydra.python.syntax.BitwiseAnd> lhs = (band).lhs;
    hydra.python.syntax.ShiftExpression rhs = (band).rhs;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.python.syntax.BitwiseAnd, hydra.ast.Expr>) (l -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.python.Serde.encodeBitwiseAnd(l),
          hydra.Serialization.cst("&")))),
        lhs),
      hydra.util.Maybe.just(hydra.python.Serde.encodeShiftExpression(rhs)))));
  }

  static hydra.ast.Expr encodeBitwiseOr(hydra.python.syntax.BitwiseOr bor) {
    hydra.util.Maybe<hydra.python.syntax.BitwiseOr> lhs = (bor).lhs;
    hydra.python.syntax.BitwiseXor rhs = (bor).rhs;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.python.syntax.BitwiseOr, hydra.ast.Expr>) (l -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.python.Serde.encodeBitwiseOr(l),
          hydra.Serialization.cst("|")))),
        lhs),
      hydra.util.Maybe.just(hydra.python.Serde.encodeBitwiseXor(rhs)))));
  }

  static hydra.ast.Expr encodeBitwiseXor(hydra.python.syntax.BitwiseXor bxor) {
    hydra.util.Maybe<hydra.python.syntax.BitwiseXor> lhs = (bxor).lhs;
    hydra.python.syntax.BitwiseAnd rhs = (bxor).rhs;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.python.syntax.BitwiseXor, hydra.ast.Expr>) (l -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.python.Serde.encodeBitwiseXor(l),
          hydra.Serialization.cst("^")))),
        lhs),
      hydra.util.Maybe.just(hydra.python.Serde.encodeBitwiseAnd(rhs)))));
  }

  static hydra.ast.Expr encodeBlock(hydra.python.syntax.Block b) {
    return (b).accept(new hydra.python.syntax.Block.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Block.Indented groups) {
        return hydra.Serialization.tabIndentDoubleSpace(hydra.lib.lists.Map.apply(
          (java.util.function.Function<java.util.List<hydra.python.syntax.Statement>, hydra.ast.Expr>) (stmts -> hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
            hydra.python.Serde::encodeStatement,
            stmts))),
          (groups).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Block.Simple ss) {
        return hydra.Serialization.semicolonSep(hydra.lib.lists.Map.apply(
          hydra.python.Serde::encodeSimpleStatement,
          (ss).value));
      }
    });
  }

  static hydra.ast.Expr encodeCapturePattern(hydra.python.syntax.CapturePattern cp) {
    return hydra.python.Serde.encodePatternCaptureTarget((cp).value);
  }

  static hydra.ast.Expr encodeCaseBlock(hydra.python.syntax.CaseBlock cb) {
    hydra.python.syntax.Block body = (cb).body;
    hydra.util.Maybe<hydra.python.syntax.Guard> guard = (cb).guard;
    hydra.python.syntax.Patterns patterns = (cb).patterns;
    return hydra.Serialization.newlineSep(java.util.Arrays.asList(
      hydra.Serialization.noSep(java.util.Arrays.asList(
        hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
          hydra.util.Maybe.just(hydra.Serialization.cst("case")),
          hydra.util.Maybe.just(hydra.python.Serde.encodePatterns(patterns)),
          hydra.lib.maybes.Map.apply(
            hydra.python.Serde::encodeGuard,
            guard)))),
        hydra.Serialization.cst(":"))),
      hydra.python.Serde.encodeBlock(body)));
  }

  static hydra.ast.Expr encodeClassDefinition(hydra.python.syntax.ClassDefinition cd) {
    hydra.util.Maybe<hydra.python.syntax.Args> args = (cd).arguments;
    hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> argPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.python.syntax.Args, hydra.ast.Expr>) (a -> hydra.Serialization.noSep(java.util.Arrays.asList(
        hydra.Serialization.cst("("),
        hydra.python.Serde.encodeArgs(a),
        hydra.Serialization.cst(")")))),
      args));
    hydra.python.syntax.Block body = (cd).body;
    hydra.util.Maybe<hydra.python.syntax.Decorators> decs = (cd).decorators;
    hydra.python.syntax.Name name = (cd).name;
    return hydra.Serialization.newlineSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.maybes.Map.apply(
        hydra.python.Serde::encodeDecorators,
        decs),
      hydra.util.Maybe.just(hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
        hydra.util.Maybe.just(hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("class"),
          hydra.python.Serde.encodeName(name)))),
        argPart.get(),
        hydra.util.Maybe.just(hydra.Serialization.cst(":")))))),
      hydra.util.Maybe.just(hydra.python.Serde.encodeBlock(body)))));
  }

  static hydra.ast.Expr encodeClassPattern(hydra.python.syntax.ClassPattern cp) {
    hydra.util.Maybe<hydra.python.syntax.KeywordPatterns> kw = (cp).keywordPatterns;
    hydra.python.syntax.NameOrAttribute noa = (cp).nameOrAttribute;
    hydra.util.Maybe<hydra.python.syntax.PositionalPatterns> pos = (cp).positionalPatterns;
    return hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.util.Maybe.just(hydra.python.Serde.encodeNameOrAttribute(noa)),
      hydra.util.Maybe.just(hydra.Serialization.cst("(")),
      hydra.lib.maybes.Map.apply(
        hydra.python.Serde::encodePositionalPatterns,
        pos),
      hydra.lib.maybes.Map.apply(
        hydra.python.Serde::encodeKeywordPatterns,
        kw),
      hydra.util.Maybe.just(hydra.Serialization.cst(")")))));
  }

  static hydra.ast.Expr encodeClosedPattern(hydra.python.syntax.ClosedPattern cp) {
    return (cp).accept(new hydra.python.syntax.ClosedPattern.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.ClosedPattern.Literal ignored) {
        return hydra.Serialization.cst("...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.ClosedPattern.Capture c) {
        return hydra.python.Serde.encodeCapturePattern((c).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.ClosedPattern.Wildcard ignored) {
        return hydra.Serialization.cst("_");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.ClosedPattern.Value v) {
        return hydra.python.Serde.encodeValuePattern((v).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.ClosedPattern.Group ignored) {
        return hydra.Serialization.cst("(...)");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.ClosedPattern.Sequence ignored) {
        return hydra.Serialization.cst("[...]");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.ClosedPattern.Mapping ignored) {
        return hydra.Serialization.cst("{...}");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.ClosedPattern.Class_ c) {
        return hydra.python.Serde.encodeClassPattern((c).value);
      }
    });
  }

  static hydra.ast.Expr encodeComparison(hydra.python.syntax.Comparison cmp) {
    return hydra.python.Serde.encodeBitwiseOr((cmp).lhs);
  }

  static hydra.ast.Expr encodeCompoundStatement(hydra.python.syntax.CompoundStatement cs) {
    return (cs).accept(new hydra.python.syntax.CompoundStatement.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.CompoundStatement.Function f) {
        return hydra.python.Serde.encodeFunctionDefinition((f).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.CompoundStatement.If ignored) {
        return hydra.Serialization.cst("if ...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.CompoundStatement.ClassDef c) {
        return hydra.python.Serde.encodeClassDefinition((c).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.CompoundStatement.With ignored) {
        return hydra.Serialization.cst("with ...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.CompoundStatement.For ignored) {
        return hydra.Serialization.cst("for ...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.CompoundStatement.Try ignored) {
        return hydra.Serialization.cst("try ...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.CompoundStatement.While w) {
        return hydra.python.Serde.encodeWhileStatement((w).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.CompoundStatement.Match m) {
        return hydra.python.Serde.encodeMatchStatement((m).value);
      }
    });
  }

  static hydra.ast.Expr encodeConditional(hydra.python.syntax.Conditional c) {
    hydra.python.syntax.Disjunction body = (c).body;
    hydra.python.syntax.Disjunction cond = (c).if_;
    hydra.python.syntax.Expression elseExpr = (c).else_;
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.python.Serde.encodeDisjunction(body),
      hydra.Serialization.cst("if"),
      hydra.python.Serde.encodeDisjunction(cond),
      hydra.Serialization.cst("else"),
      hydra.python.Serde.encodeExpression(elseExpr)));
  }

  static hydra.ast.Expr encodeConjunction(hydra.python.syntax.Conjunction c) {
    return hydra.Serialization.symbolSep(
      "and",
      hydra.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.python.Serde::encodeInversion,
        (c).value));
  }

  static hydra.ast.Expr encodeDecorators(hydra.python.syntax.Decorators decs) {
    return hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.python.syntax.NamedExpression, hydra.ast.Expr>) (ne -> hydra.Serialization.noSep(java.util.Arrays.asList(
        hydra.Serialization.cst("@"),
        hydra.python.Serde.encodeNamedExpression(ne)))),
      (decs).value));
  }

  static hydra.ast.Expr encodeDict(hydra.python.syntax.Dict d) {
    return hydra.Serialization.curlyBracesList(
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
      hydra.Serialization.halfBlockStyle(),
      hydra.lib.lists.Map.apply(
        hydra.python.Serde::encodeDoubleStarredKvpair,
        (d).value));
  }

  static hydra.ast.Expr encodeDisjunction(hydra.python.syntax.Disjunction d) {
    return hydra.Serialization.symbolSep(
      "or",
      hydra.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.python.Serde::encodeConjunction,
        (d).value));
  }

  static hydra.ast.Expr encodeDottedAsName(hydra.python.syntax.DottedAsName dan) {
    hydra.util.Maybe<hydra.python.syntax.Name> alias = (dan).as;
    hydra.python.syntax.DottedName name = (dan).name;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.util.Maybe.just(hydra.python.Serde.encodeDottedName(name)),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.python.syntax.Name, hydra.ast.Expr>) (a -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("as"),
          hydra.python.Serde.encodeName(a)))),
        alias))));
  }

  static hydra.ast.Expr encodeDottedName(hydra.python.syntax.DottedName dn) {
    return hydra.Serialization.cst(hydra.lib.strings.Intercalate.apply(
      ".",
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.python.syntax.Name, String>) (n -> (n).value),
        (dn).value)));
  }

  static hydra.ast.Expr encodeDoubleStarredKvpair(hydra.python.syntax.DoubleStarredKvpair dskv) {
    return (dskv).accept(new hydra.python.syntax.DoubleStarredKvpair.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.DoubleStarredKvpair.Pair p) {
        return hydra.python.Serde.encodeKvpair((p).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.DoubleStarredKvpair.Starred e) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("**"),
          hydra.python.Serde.encodeBitwiseOr((e).value)));
      }
    });
  }

  static hydra.ast.Expr encodeExpression(hydra.python.syntax.Expression expr) {
    return (expr).accept(new hydra.python.syntax.Expression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Expression.Simple d) {
        return hydra.python.Serde.encodeDisjunction((d).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Expression.Conditional c) {
        return hydra.python.Serde.encodeConditional((c).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Expression.Lambda l) {
        return hydra.python.Serde.encodeLambda((l).value);
      }
    });
  }

  static hydra.ast.Expr encodeFactor(hydra.python.syntax.Factor f) {
    return (f).accept(new hydra.python.syntax.Factor.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Factor.Positive inner) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("+"),
          hydra.python.Serde.encodeFactor((inner).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Factor.Negative inner) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("-"),
          hydra.python.Serde.encodeFactor((inner).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Factor.Complement inner) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("~"),
          hydra.python.Serde.encodeFactor((inner).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Factor.Simple p) {
        return hydra.python.Serde.encodePower((p).value);
      }
    });
  }

  static hydra.ast.Expr encodeFunctionDefRaw(hydra.python.syntax.FunctionDefRaw fdr) {
    Boolean async_ = (fdr).async;
    hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> asyncKw = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      async_,
      () -> hydra.util.Maybe.just(hydra.Serialization.cst("async")),
      () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing())));
    hydra.python.syntax.Block block = (fdr).block;
    hydra.python.syntax.Name name = (fdr).name;
    hydra.util.Maybe<hydra.python.syntax.Parameters> params = (fdr).params;
    hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> paramPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      hydra.python.Serde::encodeParameters,
      params));
    hydra.util.Maybe<hydra.python.syntax.Expression> retType = (fdr).returnType;
    hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> retPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.python.syntax.Expression, hydra.ast.Expr>) (t -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
        hydra.Serialization.cst("->"),
        hydra.python.Serde.encodeExpression(t)))),
      retType));
    java.util.List<hydra.python.syntax.TypeParameter> tparams = (fdr).typeParams;
    hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> tparamPart = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(tparams),
      () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
      () -> hydra.util.Maybe.just(hydra.Serialization.bracketList(
        hydra.Serialization.inlineStyle(),
        hydra.lib.lists.Map.apply(
          hydra.python.Serde::encodeTypeParameter,
          tparams)))));
    return hydra.Serialization.newlineSep(java.util.Arrays.asList(
      hydra.Serialization.noSep(java.util.Arrays.asList(
        hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
          asyncKw.get(),
          hydra.util.Maybe.just(hydra.Serialization.cst("def")),
          hydra.util.Maybe.just(hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
            hydra.util.Maybe.just(hydra.python.Serde.encodeName(name)),
            tparamPart.get(),
            hydra.util.Maybe.just(hydra.Serialization.cst("(")),
            paramPart.get(),
            hydra.util.Maybe.just(hydra.Serialization.cst(")")))))),
          retPart.get()))),
        hydra.Serialization.cst(":"))),
      hydra.python.Serde.encodeBlock(block)));
  }

  static hydra.ast.Expr encodeFunctionDefinition(hydra.python.syntax.FunctionDefinition fd) {
    hydra.util.Maybe<hydra.python.syntax.Decorators> decs = (fd).decorators;
    hydra.python.syntax.FunctionDefRaw raw = (fd).raw;
    return hydra.Serialization.newlineSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.maybes.Map.apply(
        hydra.python.Serde::encodeDecorators,
        decs),
      hydra.util.Maybe.just(hydra.python.Serde.encodeFunctionDefRaw(raw)))));
  }

  static hydra.ast.Expr encodeGroup(hydra.python.syntax.Group g) {
    return (g).accept(new hydra.python.syntax.Group.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Group.Expression ne) {
        return hydra.python.Serde.encodeNamedExpression((ne).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Group.Yield ignored) {
        return hydra.Serialization.cst("(yield ...)");
      }
    });
  }

  static hydra.ast.Expr encodeGuard(hydra.python.syntax.Guard g) {
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst("if"),
      hydra.python.Serde.encodeNamedExpression((g).value)));
  }

  static hydra.ast.Expr encodeImportFrom(hydra.python.syntax.ImportFrom if_) {
    hydra.util.Maybe<hydra.python.syntax.DottedName> name = (if_).dottedName;
    java.util.List<hydra.python.syntax.RelativeImportPrefix> prefixes = (if_).prefixes;
    hydra.util.Lazy<hydra.ast.Expr> lhs = new hydra.util.Lazy<>(() -> hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.python.syntax.RelativeImportPrefix, hydra.util.Maybe<hydra.ast.Expr>>) (p -> hydra.util.Maybe.just(hydra.python.Serde.encodeRelativeImportPrefix(p))),
        prefixes),
      java.util.Arrays.asList(hydra.lib.maybes.Map.apply(
        hydra.python.Serde::encodeDottedName,
        name)))))));
    hydra.python.syntax.ImportFromTargets targets = (if_).targets;
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst("from"),
      lhs.get(),
      hydra.Serialization.cst("import"),
      hydra.python.Serde.encodeImportFromTargets(targets)));
  }

  static hydra.ast.Expr encodeImportFromAsName(hydra.python.syntax.ImportFromAsName ifan) {
    hydra.util.Maybe<hydra.python.syntax.Name> alias = (ifan).as;
    hydra.python.syntax.Name name = (ifan).name;
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.python.Serde.encodeName(name),
      (java.util.function.Function<hydra.python.syntax.Name, hydra.ast.Expr>) (a -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
        hydra.python.Serde.encodeName(name),
        hydra.Serialization.cst("as"),
        hydra.python.Serde.encodeName(a)))),
      alias);
  }

  static hydra.ast.Expr encodeImportFromTargets(hydra.python.syntax.ImportFromTargets t) {
    return (t).accept(new hydra.python.syntax.ImportFromTargets.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.ImportFromTargets.Simple names) {
        return hydra.Serialization.commaSep(
          hydra.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.python.Serde::encodeImportFromAsName,
            (names).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.ImportFromTargets.Parens names) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("("),
          hydra.Serialization.commaSep(
            hydra.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.python.Serde::encodeImportFromAsName,
              (names).value)),
          hydra.Serialization.cst(")")));
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.ImportFromTargets.Star ignored) {
        return hydra.Serialization.cst("*");
      }
    });
  }

  static hydra.ast.Expr encodeImportName(hydra.python.syntax.ImportName in_) {
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst("import"),
      hydra.Serialization.commaSep(
        hydra.Serialization.inlineStyle(),
        hydra.lib.lists.Map.apply(
          hydra.python.Serde::encodeDottedAsName,
          (in_).value))));
  }

  static hydra.ast.Expr encodeImportStatement(hydra.python.syntax.ImportStatement is_) {
    return (is_).accept(new hydra.python.syntax.ImportStatement.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.ImportStatement.Name n) {
        return hydra.python.Serde.encodeImportName((n).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.ImportStatement.From f) {
        return hydra.python.Serde.encodeImportFrom((f).value);
      }
    });
  }

  static hydra.ast.Expr encodeInversion(hydra.python.syntax.Inversion i) {
    return (i).accept(new hydra.python.syntax.Inversion.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Inversion.Not other) {
        return hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("not"),
          hydra.python.Serde.encodeInversion((other).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Inversion.Simple c) {
        return hydra.python.Serde.encodeComparison((c).value);
      }
    });
  }

  static hydra.ast.Expr encodeKeywordPattern(hydra.python.syntax.KeywordPattern kp) {
    hydra.python.syntax.Name name = (kp).name;
    hydra.python.syntax.Pattern pat = (kp).pattern;
    return hydra.Serialization.noSep(java.util.Arrays.asList(
      hydra.python.Serde.encodeName(name),
      hydra.Serialization.cst("="),
      hydra.python.Serde.encodePattern(pat)));
  }

  static hydra.ast.Expr encodeKeywordPatterns(hydra.python.syntax.KeywordPatterns kp) {
    return hydra.Serialization.commaSep(
      hydra.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.python.Serde::encodeKeywordPattern,
        (kp).value));
  }

  static hydra.ast.Expr encodeKvpair(hydra.python.syntax.Kvpair kv) {
    hydra.python.syntax.Expression k = (kv).key;
    hydra.python.syntax.Expression v = (kv).value;
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.noSep(java.util.Arrays.asList(
        hydra.python.Serde.encodeExpression(k),
        hydra.Serialization.cst(":"))),
      hydra.python.Serde.encodeExpression(v)));
  }

  static hydra.ast.Expr encodeKwarg(hydra.python.syntax.Kwarg k) {
    hydra.python.syntax.Expression expr = (k).value;
    hydra.python.syntax.Name name = (k).name;
    return hydra.Serialization.noSep(java.util.Arrays.asList(
      hydra.python.Serde.encodeName(name),
      hydra.Serialization.cst("="),
      hydra.python.Serde.encodeExpression(expr)));
  }

  static hydra.ast.Expr encodeKwargOrDoubleStarred(hydra.python.syntax.KwargOrDoubleStarred kds) {
    return (kds).accept(new hydra.python.syntax.KwargOrDoubleStarred.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.KwargOrDoubleStarred.Kwarg k) {
        return hydra.python.Serde.encodeKwarg((k).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.KwargOrDoubleStarred.DoubleStarred e) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("**"),
          hydra.python.Serde.encodeExpression((e).value)));
      }
    });
  }

  static hydra.ast.Expr encodeKwargOrStarred(hydra.python.syntax.KwargOrStarred ks) {
    return (ks).accept(new hydra.python.syntax.KwargOrStarred.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.KwargOrStarred.Kwarg k) {
        return hydra.python.Serde.encodeKwarg((k).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.KwargOrStarred.Starred se) {
        return hydra.python.Serde.encodeStarredExpression((se).value);
      }
    });
  }

  static hydra.ast.Expr encodeLambda(hydra.python.syntax.Lambda l) {
    hydra.python.syntax.Expression body = (l).body;
    hydra.python.syntax.LambdaParameters params = (l).params;
    return hydra.Serialization.parens(hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst("lambda"),
      hydra.Serialization.noSep(java.util.Arrays.asList(
        hydra.python.Serde.encodeLambdaParameters(params),
        hydra.Serialization.cst(":"))),
      hydra.python.Serde.encodeExpression(body))));
  }

  static hydra.ast.Expr encodeLambdaParamNoDefault(hydra.python.syntax.LambdaParamNoDefault p) {
    return hydra.python.Serde.encodeName((p).value);
  }

  static hydra.ast.Expr encodeLambdaParameters(hydra.python.syntax.LambdaParameters lp) {
    java.util.List<hydra.python.syntax.LambdaParamNoDefault> nodef = (lp).paramNoDefault;
    return hydra.Serialization.commaSep(
      hydra.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.python.Serde::encodeLambdaParamNoDefault,
        nodef));
  }

  static hydra.ast.Expr encodeLambdaStarEtc(hydra.python.syntax.LambdaStarEtc lse) {
    return (lse).accept(new hydra.python.syntax.LambdaStarEtc.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.LambdaStarEtc.ParamNoDefault p) {
        return hydra.python.Serde.encodeLambdaParamNoDefault((p).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.LambdaStarEtc.Star ignored) {
        return hydra.Serialization.cst("*...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.LambdaStarEtc.ParamMaybeDefault ignored) {
        return hydra.Serialization.cst("...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.LambdaStarEtc.Kwds ignored) {
        return hydra.Serialization.cst("**...");
      }
    });
  }

  static hydra.ast.Expr encodeList(hydra.python.syntax.List l) {
    return hydra.Serialization.bracketListAdaptive(hydra.lib.lists.Map.apply(
      hydra.python.Serde::encodeStarNamedExpression,
      (l).value));
  }

  static hydra.ast.Expr encodeMatchStatement(hydra.python.syntax.MatchStatement ms) {
    java.util.List<hydra.python.syntax.CaseBlock> cases = (ms).cases;
    hydra.python.syntax.SubjectExpression subj = (ms).subject;
    return hydra.Serialization.newlineSep(java.util.Arrays.asList(
      hydra.Serialization.spaceSep(java.util.Arrays.asList(
        hydra.Serialization.cst("match"),
        hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.python.Serde.encodeSubjectExpression(subj),
          hydra.Serialization.cst(":"))))),
      hydra.Serialization.tabIndentDoubleSpace(hydra.lib.lists.Map.apply(
        hydra.python.Serde::encodeCaseBlock,
        cases))));
  }

  static hydra.ast.Expr encodeModule(hydra.python.syntax.Module mod) {
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> groups = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<hydra.python.syntax.Statement>, hydra.ast.Expr>) (group -> hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
        hydra.python.Serde::encodeStatement,
        group))),
      (mod).value));
    hydra.ast.Expr warning = hydra.Serialization.cst(hydra.python.Serde.toPythonComments(hydra.Constants.warningAutoGeneratedFile()));
    return hydra.Serialization.doubleNewlineSep(hydra.lib.lists.Cons.apply(
      warning,
      groups.get()));
  }

  static hydra.ast.Expr encodeName(hydra.python.syntax.Name n) {
    return hydra.Serialization.cst((n).value);
  }

  static hydra.ast.Expr encodeNameOrAttribute(hydra.python.syntax.NameOrAttribute noa) {
    return hydra.Serialization.dotSep(hydra.lib.lists.Map.apply(
      hydra.python.Serde::encodeName,
      (noa).value));
  }

  static hydra.ast.Expr encodeNamedExpression(hydra.python.syntax.NamedExpression ne) {
    return (ne).accept(new hydra.python.syntax.NamedExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.NamedExpression.Simple e) {
        return hydra.python.Serde.encodeExpression((e).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.NamedExpression.Assignment ae) {
        return hydra.python.Serde.encodeAssignmentExpression((ae).value);
      }
    });
  }

  static hydra.ast.Expr encodeNumber(hydra.python.syntax.Number_ num) {
    return (num).accept(new hydra.python.syntax.Number_.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Number_.Float_ f) {
        return hydra.Serialization.cst(hydra.python.Serde.pythonFloatLiteralText(hydra.lib.literals.ShowBigfloat.apply((f).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Number_.Integer_ i) {
        return hydra.Serialization.cst(hydra.lib.literals.ShowBigint.apply((i).value));
      }
    });
  }

  static hydra.ast.Expr encodeOrPattern(hydra.python.syntax.OrPattern op) {
    return hydra.Serialization.symbolSep(
      "|",
      hydra.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.python.Serde::encodeClosedPattern,
        (op).value));
  }

  static hydra.ast.Expr encodeParam(hydra.python.syntax.Param p) {
    hydra.util.Maybe<hydra.python.syntax.Annotation> ann = (p).annotation;
    hydra.python.syntax.Name name = (p).name;
    return hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.util.Maybe.just(hydra.python.Serde.encodeName(name)),
      hydra.lib.maybes.Map.apply(
        hydra.python.Serde::encodeAnnotation,
        ann))));
  }

  static hydra.ast.Expr encodeParamNoDefault(hydra.python.syntax.ParamNoDefault pnd) {
    return hydra.python.Serde.encodeParam((pnd).param);
  }

  static hydra.ast.Expr encodeParamNoDefaultParameters(hydra.python.syntax.ParamNoDefaultParameters pndp) {
    java.util.List<hydra.python.syntax.ParamNoDefault> nodef = (pndp).paramNoDefault;
    return hydra.Serialization.commaSep(
      hydra.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.python.Serde::encodeParamNoDefault,
        nodef));
  }

  static hydra.ast.Expr encodeParameters(hydra.python.syntax.Parameters p) {
    return (p).accept(new hydra.python.syntax.Parameters.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Parameters.ParamNoDefault pnd) {
        return hydra.python.Serde.encodeParamNoDefaultParameters((pnd).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Parameters.SlashNoDefault ignored) {
        return hydra.Serialization.cst("...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Parameters.SlashWithDefault ignored) {
        return hydra.Serialization.cst("...");
      }
    });
  }

  static hydra.ast.Expr encodePattern(hydra.python.syntax.Pattern p) {
    return (p).accept(new hydra.python.syntax.Pattern.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Pattern.Or op) {
        return hydra.python.Serde.encodeOrPattern((op).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Pattern.As ignored) {
        return hydra.Serialization.cst("... as ...");
      }
    });
  }

  static hydra.ast.Expr encodePatternCaptureTarget(hydra.python.syntax.PatternCaptureTarget pct) {
    return hydra.python.Serde.encodeName((pct).value);
  }

  static hydra.ast.Expr encodePatterns(hydra.python.syntax.Patterns ps) {
    return (ps).accept(new hydra.python.syntax.Patterns.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Patterns.Pattern p) {
        return hydra.python.Serde.encodePattern((p).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Patterns.Sequence ignored) {
        return hydra.Serialization.cst("...");
      }
    });
  }

  static hydra.ast.Expr encodePosArg(hydra.python.syntax.PosArg pa) {
    return (pa).accept(new hydra.python.syntax.PosArg.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.PosArg.Starred se) {
        return hydra.python.Serde.encodeStarredExpression((se).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.PosArg.Assignment ae) {
        return hydra.python.Serde.encodeAssignmentExpression((ae).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.PosArg.Expression e) {
        return hydra.python.Serde.encodeExpression((e).value);
      }
    });
  }

  static hydra.ast.Expr encodePositionalPatterns(hydra.python.syntax.PositionalPatterns pp) {
    return hydra.Serialization.commaSep(
      hydra.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.python.Serde::encodePattern,
        (pp).value));
  }

  static hydra.ast.Expr encodePower(hydra.python.syntax.Power p) {
    hydra.python.syntax.AwaitPrimary lhs = (p).lhs;
    hydra.util.Maybe<hydra.python.syntax.Factor> rhs = (p).rhs;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.util.Maybe.just(hydra.python.Serde.encodeAwaitPrimary(lhs)),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.python.syntax.Factor, hydra.ast.Expr>) (r -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("**"),
          hydra.python.Serde.encodeFactor(r)))),
        rhs))));
  }

  static hydra.ast.Expr encodePrimary(hydra.python.syntax.Primary p) {
    return (p).accept(new hydra.python.syntax.Primary.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Primary.Simple a) {
        return hydra.python.Serde.encodeAtom((a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Primary.Compound pwr) {
        return hydra.python.Serde.encodePrimaryWithRhs((pwr).value);
      }
    });
  }

  static hydra.ast.Expr encodePrimaryRhs(hydra.python.syntax.PrimaryRhs rhs) {
    return (rhs).accept(new hydra.python.syntax.PrimaryRhs.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.PrimaryRhs.Call args) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("("),
          hydra.python.Serde.encodeArgs((args).value),
          hydra.Serialization.cst(")")));
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.PrimaryRhs.Project name) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("."),
          hydra.python.Serde.encodeName((name).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.PrimaryRhs.Slices slices) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("["),
          hydra.python.Serde.encodeSlices((slices).value),
          hydra.Serialization.cst("]")));
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.PrimaryRhs.Genexp ignored) {
        return hydra.Serialization.cst("[...]");
      }
    });
  }

  static hydra.ast.Expr encodePrimaryWithRhs(hydra.python.syntax.PrimaryWithRhs pwr) {
    hydra.python.syntax.Primary prim = (pwr).primary;
    hydra.python.syntax.PrimaryRhs rhs = (pwr).rhs;
    return hydra.Serialization.noSep(java.util.Arrays.asList(
      hydra.python.Serde.encodePrimary(prim),
      hydra.python.Serde.encodePrimaryRhs(rhs)));
  }

  static hydra.ast.Expr encodeRaiseExpression(hydra.python.syntax.RaiseExpression re) {
    hydra.python.syntax.Expression expr = (re).expression;
    hydra.util.Maybe<hydra.python.syntax.Expression> from_ = (re).from;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.util.Maybe.just(hydra.python.Serde.encodeExpression(expr)),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.python.syntax.Expression, hydra.ast.Expr>) (f -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("from"),
          hydra.python.Serde.encodeExpression(f)))),
        from_))));
  }

  static hydra.ast.Expr encodeRaiseStatement(hydra.python.syntax.RaiseStatement rs) {
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.util.Maybe.just(hydra.Serialization.cst("raise")),
      hydra.lib.maybes.Map.apply(
        hydra.python.Serde::encodeRaiseExpression,
        (rs).value))));
  }

  static hydra.ast.Expr encodeRelativeImportPrefix(hydra.python.syntax.RelativeImportPrefix p) {
    return (p).accept(new hydra.python.syntax.RelativeImportPrefix.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.RelativeImportPrefix.Dot ignored) {
        return hydra.Serialization.cst(".");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.RelativeImportPrefix.Ellipsis ignored) {
        return hydra.Serialization.cst("...");
      }
    });
  }

  static hydra.ast.Expr encodeReturnStatement(hydra.python.syntax.ReturnStatement rs) {
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst("return"),
      hydra.Serialization.commaSep(
        hydra.Serialization.inlineStyle(),
        hydra.lib.lists.Map.apply(
          hydra.python.Serde::encodeStarExpression,
          (rs).value))));
  }

  static hydra.ast.Expr encodeSet(hydra.python.syntax.Set s) {
    return hydra.Serialization.bracesListAdaptive(hydra.lib.lists.Map.apply(
      hydra.python.Serde::encodeStarNamedExpression,
      (s).value));
  }

  static hydra.ast.Expr encodeShiftExpression(hydra.python.syntax.ShiftExpression se) {
    return hydra.python.Serde.encodeSum((se).rhs);
  }

  static hydra.ast.Expr encodeSimpleStatement(hydra.python.syntax.SimpleStatement ss) {
    return (ss).accept(new hydra.python.syntax.SimpleStatement.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SimpleStatement.Assignment a) {
        return hydra.python.Serde.encodeAssignment((a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SimpleStatement.StarExpressions es) {
        return hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
          hydra.python.Serde::encodeStarExpression,
          (es).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SimpleStatement.Return r) {
        return hydra.python.Serde.encodeReturnStatement((r).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SimpleStatement.Raise r) {
        return hydra.python.Serde.encodeRaiseStatement((r).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SimpleStatement.Pass ignored) {
        return hydra.Serialization.cst("pass");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SimpleStatement.Break ignored) {
        return hydra.Serialization.cst("break");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SimpleStatement.Continue ignored) {
        return hydra.Serialization.cst("continue");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SimpleStatement.Import i) {
        return hydra.python.Serde.encodeImportStatement((i).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SimpleStatement.TypeAlias t) {
        return hydra.python.Serde.encodeTypeAlias((t).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SimpleStatement.Assert ignored) {
        return hydra.Serialization.cst("assert ...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SimpleStatement.Global ignored) {
        return hydra.Serialization.cst("global ...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SimpleStatement.Nonlocal ignored) {
        return hydra.Serialization.cst("nonlocal ...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SimpleStatement.Del ignored) {
        return hydra.Serialization.cst("del ...");
      }
    });
  }

  static hydra.ast.Expr encodeSimpleTypeParameter(hydra.python.syntax.SimpleTypeParameter stp) {
    return hydra.python.Serde.encodeName((stp).name);
  }

  static hydra.ast.Expr encodeSingleTarget(hydra.python.syntax.SingleTarget st) {
    return (st).accept(new hydra.python.syntax.SingleTarget.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SingleTarget.Name n) {
        return hydra.python.Serde.encodeName((n).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SingleTarget.Parens ignored) {
        return hydra.Serialization.cst("(...)");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SingleTarget.SubscriptAttributeTarget ignored) {
        return hydra.Serialization.cst("...");
      }
    });
  }

  static hydra.ast.Expr encodeSlice(hydra.python.syntax.Slice s) {
    return (s).accept(new hydra.python.syntax.Slice.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Slice.Named ne) {
        return hydra.python.Serde.encodeNamedExpression((ne).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Slice.Slice_ ignored) {
        return hydra.Serialization.cst(":");
      }
    });
  }

  static hydra.ast.Expr encodeSliceOrStarredExpression(hydra.python.syntax.SliceOrStarredExpression s) {
    return (s).accept(new hydra.python.syntax.SliceOrStarredExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SliceOrStarredExpression.Slice sl) {
        return hydra.python.Serde.encodeSlice((sl).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SliceOrStarredExpression.Starred se) {
        return hydra.python.Serde.encodeStarredExpression((se).value);
      }
    });
  }

  static hydra.ast.Expr encodeSlices(hydra.python.syntax.Slices s) {
    hydra.python.syntax.Slice hd = (s).head;
    java.util.List<hydra.python.syntax.SliceOrStarredExpression> tl = (s).tail;
    return hydra.Serialization.commaSep(
      hydra.Serialization.inlineStyle(),
      hydra.lib.lists.Cons.apply(
        hydra.python.Serde.encodeSlice(hd),
        hydra.lib.lists.Map.apply(
          hydra.python.Serde::encodeSliceOrStarredExpression,
          tl)));
  }

  static hydra.ast.Expr encodeStarAtom(hydra.python.syntax.StarAtom sa) {
    return (sa).accept(new hydra.python.syntax.StarAtom.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.StarAtom.Name n) {
        return hydra.python.Serde.encodeName((n).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.StarAtom.TargetWithStarAtom ignored) {
        return hydra.Serialization.cst("(...)");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.StarAtom.StarTargetsTupleSeq ignored) {
        return hydra.Serialization.cst("(...)");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.StarAtom.StarTargetsListSeq ignored) {
        return hydra.Serialization.cst("[...]");
      }
    });
  }

  static hydra.ast.Expr encodeStarExpression(hydra.python.syntax.StarExpression se) {
    return (se).accept(new hydra.python.syntax.StarExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.StarExpression.Star bor) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("*"),
          hydra.python.Serde.encodeBitwiseOr((bor).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.StarExpression.Simple e) {
        return hydra.python.Serde.encodeExpression((e).value);
      }
    });
  }

  static hydra.ast.Expr encodeStarNamedExpression(hydra.python.syntax.StarNamedExpression sne) {
    return (sne).accept(new hydra.python.syntax.StarNamedExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.StarNamedExpression.Star bor) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("*"),
          hydra.python.Serde.encodeBitwiseOr((bor).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.StarNamedExpression.Simple ne) {
        return hydra.python.Serde.encodeNamedExpression((ne).value);
      }
    });
  }

  static hydra.ast.Expr encodeStarTarget(hydra.python.syntax.StarTarget st) {
    return (st).accept(new hydra.python.syntax.StarTarget.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.StarTarget.Unstarred t) {
        return hydra.python.Serde.encodeTargetWithStarAtom((t).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.StarTarget.Starred inner) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("*"),
          hydra.python.Serde.encodeStarTarget((inner).value)));
      }
    });
  }

  static hydra.ast.Expr encodeStarredExpression(hydra.python.syntax.StarredExpression se) {
    return hydra.Serialization.noSep(java.util.Arrays.asList(
      hydra.Serialization.cst("*"),
      hydra.python.Serde.encodeExpression((se).value)));
  }

  static hydra.ast.Expr encodeStatement(hydra.python.syntax.Statement stmt) {
    return (stmt).accept(new hydra.python.syntax.Statement.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Statement.Annotated a) {
        return hydra.python.Serde.encodeAnnotatedStatement((a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Statement.Simple ss) {
        return hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
          hydra.python.Serde::encodeSimpleStatement,
          (ss).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.Statement.Compound c) {
        return hydra.python.Serde.encodeCompoundStatement((c).value);
      }
    });
  }

  static hydra.ast.Expr encodeString(hydra.python.syntax.String_ s) {
    String content = (s).value;
    hydra.python.syntax.QuoteStyle style = (s).quoteStyle;
    return (style).accept(new hydra.python.syntax.QuoteStyle.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.QuoteStyle.Single ignored) {
        return hydra.Serialization.cst(hydra.python.Serde.escapePythonString(
          false,
          content));
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.QuoteStyle.Double_ ignored) {
        return hydra.Serialization.cst(hydra.python.Serde.escapePythonString(
          true,
          content));
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.QuoteStyle.Triple ignored) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("r\"\"\""),
          hydra.Serialization.cst(content),
          hydra.Serialization.cst("\"\"\"")));
      }
    });
  }

  static hydra.ast.Expr encodeSubjectExpression(hydra.python.syntax.SubjectExpression se) {
    return (se).accept(new hydra.python.syntax.SubjectExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SubjectExpression.Simple ne) {
        return hydra.python.Serde.encodeNamedExpression((ne).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.SubjectExpression.Tuple ignored) {
        return hydra.Serialization.cst("*...");
      }
    });
  }

  static hydra.ast.Expr encodeSum(hydra.python.syntax.Sum s) {
    return hydra.python.Serde.encodeTerm((s).rhs);
  }

  static hydra.ast.Expr encodeTPrimary(hydra.python.syntax.TPrimary tp) {
    return (tp).accept(new hydra.python.syntax.TPrimary.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.TPrimary.Atom a) {
        return hydra.python.Serde.encodeAtom((a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.TPrimary.PrimaryAndName pn) {
        return hydra.python.Serde.encodeTPrimaryAndName((pn).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.TPrimary.PrimaryAndSlices ignored) {
        return hydra.Serialization.cst("...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.TPrimary.PrimaryAndGenexp ignored) {
        return hydra.Serialization.cst("...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.TPrimary.PrimaryAndArguments ignored) {
        return hydra.Serialization.cst("...");
      }
    });
  }

  static hydra.ast.Expr encodeTPrimaryAndName(hydra.python.syntax.TPrimaryAndName pn) {
    hydra.python.syntax.Name name_ = (pn).name;
    hydra.python.syntax.TPrimary prim = (pn).primary;
    return hydra.Serialization.noSep(java.util.Arrays.asList(
      hydra.python.Serde.encodeTPrimary(prim),
      hydra.Serialization.cst("."),
      hydra.python.Serde.encodeName(name_)));
  }

  static hydra.ast.Expr encodeTargetWithStarAtom(hydra.python.syntax.TargetWithStarAtom t) {
    return (t).accept(new hydra.python.syntax.TargetWithStarAtom.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.TargetWithStarAtom.Atom a) {
        return hydra.python.Serde.encodeStarAtom((a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.TargetWithStarAtom.Project pn) {
        return hydra.python.Serde.encodeTPrimaryAndName((pn).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.TargetWithStarAtom.Slices ignored) {
        return hydra.Serialization.cst("...");
      }
    });
  }

  static hydra.ast.Expr encodeTerm(hydra.python.syntax.Term t) {
    return hydra.python.Serde.encodeFactor((t).rhs);
  }

  static hydra.ast.Expr encodeTuple(hydra.python.syntax.Tuple t) {
    java.util.List<hydra.python.syntax.StarNamedExpression> es = (t).value;
    return hydra.lib.maybes.FromMaybe.applyLazy(
      () -> hydra.Serialization.parenList(
        false,
        hydra.lib.lists.Map.apply(
          hydra.python.Serde::encodeStarNamedExpression,
          es)),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.python.syntax.StarNamedExpression, hydra.ast.Expr>) (firstEs -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            hydra.lib.lists.Length.apply(es),
            1),
          () -> hydra.Serialization.parens(hydra.Serialization.noSep(java.util.Arrays.asList(
            hydra.python.Serde.encodeStarNamedExpression(firstEs),
            hydra.Serialization.cst(",")))),
          () -> hydra.Serialization.parenList(
            false,
            hydra.lib.lists.Map.apply(
              hydra.python.Serde::encodeStarNamedExpression,
              es)))),
        hydra.lib.lists.MaybeHead.apply(es)));
  }

  static hydra.ast.Expr encodeTypeAlias(hydra.python.syntax.TypeAlias ta) {
    hydra.python.syntax.Name name = (ta).name;
    java.util.List<hydra.python.syntax.TypeParameter> tparams = (ta).typeParams;
    hydra.util.Lazy<hydra.ast.Expr> alias = new hydra.util.Lazy<>(() -> hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.util.Maybe.just(hydra.python.Serde.encodeName(name)),
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(tparams),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.bracketList(
          hydra.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.python.Serde::encodeTypeParameter,
            tparams))))))));
    hydra.python.syntax.Expression expr = (ta).expression;
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst("type"),
      alias.get(),
      hydra.Serialization.cst("="),
      hydra.python.Serde.encodeExpression(expr)));
  }

  static hydra.ast.Expr encodeTypeParameter(hydra.python.syntax.TypeParameter tp) {
    return (tp).accept(new hydra.python.syntax.TypeParameter.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.TypeParameter.Simple s) {
        return hydra.python.Serde.encodeSimpleTypeParameter((s).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.TypeParameter.Star ignored) {
        return hydra.Serialization.cst("*...");
      }

      @Override
      public hydra.ast.Expr visit(hydra.python.syntax.TypeParameter.DoubleStar ignored) {
        return hydra.Serialization.cst("**...");
      }
    });
  }

  static hydra.ast.Expr encodeTypedAssignment(hydra.python.syntax.TypedAssignment ta) {
    hydra.python.syntax.SingleTarget lhs = (ta).lhs;
    hydra.util.Maybe<hydra.python.syntax.AnnotatedRhs> rhs = (ta).rhs;
    hydra.python.syntax.Expression typ = (ta).type;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.util.Maybe.just(hydra.Serialization.noSep(java.util.Arrays.asList(
        hydra.python.Serde.encodeSingleTarget(lhs),
        hydra.Serialization.cst(":")))),
      hydra.util.Maybe.just(hydra.python.Serde.encodeExpression(typ)),
      hydra.lib.maybes.Map.apply(
        hydra.python.Serde::encodeAnnotatedRhs,
        rhs))));
  }

  static hydra.ast.Expr encodeUntypedAssignment(hydra.python.syntax.UntypedAssignment ua) {
    hydra.python.syntax.AnnotatedRhs rhs = (ua).rhs;
    java.util.List<hydra.python.syntax.StarTarget> targets = (ua).targets;
    return hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
      hydra.lib.lists.Map.apply(
        hydra.python.Serde::encodeStarTarget,
        targets),
      java.util.Arrays.asList(hydra.python.Serde.encodeAnnotatedRhs(rhs)))));
  }

  static hydra.ast.Expr encodeValuePattern(hydra.python.syntax.ValuePattern vp) {
    return hydra.python.Serde.encodeAttribute((vp).value);
  }

  static hydra.ast.Expr encodeWhileStatement(hydra.python.syntax.WhileStatement ws) {
    hydra.python.syntax.Block body = (ws).body;
    hydra.python.syntax.NamedExpression cond = (ws).condition;
    hydra.util.Maybe<hydra.python.syntax.Block> else_ = (ws).else_;
    return hydra.Serialization.newlineSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.util.Maybe.just(hydra.Serialization.newlineSep(java.util.Arrays.asList(
        hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("while"),
          hydra.Serialization.noSep(java.util.Arrays.asList(
            hydra.python.Serde.encodeNamedExpression(cond),
            hydra.Serialization.cst(":"))))),
        hydra.python.Serde.encodeBlock(body)))),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.python.syntax.Block, hydra.ast.Expr>) (eb -> hydra.Serialization.newlineSep(java.util.Arrays.asList(
          hydra.Serialization.cst("else:"),
          hydra.python.Serde.encodeBlock(eb)))),
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

  static String pythonFloatLiteralText(String s) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        s,
        "NaN"),
      () -> "float('nan')",
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          s,
          "Infinity"),
        () -> "float('inf')",
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            s,
            "-Infinity"),
          () -> "float('-inf')",
          () -> s)));
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
