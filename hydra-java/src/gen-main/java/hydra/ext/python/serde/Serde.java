// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.serde;

/**
 * Python serializer: converts Python AST to concrete syntax
 */
public interface Serde {
  static hydra.ast.Expr encodeAnnotatedRhs(hydra.ext.python.syntax.AnnotatedRhs arhs) {
    return hydra.serialization.Serialization.spaceSep(java.util.List.of(
      hydra.serialization.Serialization.cst("="),
      (arhs).accept(new hydra.ext.python.syntax.AnnotatedRhs.PartialVisitor<>() {
        @Override
        public hydra.ast.Expr visit(hydra.ext.python.syntax.AnnotatedRhs.Star ses) {
          return hydra.serialization.Serialization.commaSep(
            hydra.serialization.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.ext.python.serde.Serde::encodeStarExpression,
              (ses).value));
        }
        
        @Override
        public hydra.ast.Expr visit(hydra.ext.python.syntax.AnnotatedRhs.Yield ignored) {
          return hydra.serialization.Serialization.cst("yield ...");
        }
      })));
  }
  
  static hydra.ast.Expr encodeAnnotatedStatement(hydra.ext.python.syntax.AnnotatedStatement as_) {
    String doc_ = (as_).comment;
    hydra.ext.python.syntax.Statement stmt = (as_).statement;
    return hydra.serialization.Serialization.newlineSep(java.util.List.of(
      hydra.serialization.Serialization.cst(hydra.ext.python.serde.Serde.toPythonComments(doc_)),
      hydra.ext.python.serde.Serde.encodeStatement(stmt)));
  }
  
  static hydra.ast.Expr encodeAnnotation(hydra.ext.python.syntax.Annotation ann) {
    return hydra.serialization.Serialization.spaceSep(java.util.List.of(
      hydra.serialization.Serialization.cst(":"),
      hydra.ext.python.serde.Serde.encodeExpression((ann).value)));
  }
  
  static hydra.ast.Expr encodeArgs(hydra.ext.python.syntax.Args args) {
    java.util.List<hydra.ext.python.syntax.KwargOrStarred> ks = (args).kwargOrStarred;
    java.util.List<hydra.ext.python.syntax.KwargOrDoubleStarred> kss = (args).kwargOrDoubleStarred;
    java.util.List<hydra.ext.python.syntax.PosArg> pos = (args).positional;
    return hydra.serialization.Serialization.commaSep(
      hydra.serialization.Serialization.inlineStyle(),
      hydra.lib.lists.Concat.apply(java.util.List.of(
        hydra.lib.lists.Map.apply(
          hydra.ext.python.serde.Serde::encodePosArg,
          pos),
        hydra.lib.lists.Map.apply(
          hydra.ext.python.serde.Serde::encodeKwargOrStarred,
          ks),
        hydra.lib.lists.Map.apply(
          hydra.ext.python.serde.Serde::encodeKwargOrDoubleStarred,
          kss))));
  }
  
  static hydra.ast.Expr encodeAssignment(hydra.ext.python.syntax.Assignment a) {
    return (a).accept(new hydra.ext.python.syntax.Assignment.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Assignment.Typed t) {
        return hydra.ext.python.serde.Serde.encodeTypedAssignment((t).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Assignment.Untyped u) {
        return hydra.ext.python.serde.Serde.encodeUntypedAssignment((u).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Assignment.Aug ignored) {
        return hydra.serialization.Serialization.cst("... += ...");
      }
    });
  }
  
  static hydra.ast.Expr encodeAssignmentExpression(hydra.ext.python.syntax.AssignmentExpression ae) {
    hydra.ext.python.syntax.Expression expr = (ae).expression;
    hydra.ext.python.syntax.Name name = (ae).name;
    return hydra.serialization.Serialization.spaceSep(java.util.List.of(
      hydra.ext.python.serde.Serde.encodeName(name),
      hydra.serialization.Serialization.cst(":="),
      hydra.ext.python.serde.Serde.encodeExpression(expr)));
  }
  
  static hydra.ast.Expr encodeAtom(hydra.ext.python.syntax.Atom atom) {
    return (atom).accept(new hydra.ext.python.syntax.Atom.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Dict d) {
        return hydra.ext.python.serde.Serde.encodeDict((d).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Dictcomp ignored) {
        return hydra.serialization.Serialization.cst("{...}");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Ellipsis ignored) {
        return hydra.serialization.Serialization.cst("...");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.False ignored) {
        return hydra.serialization.Serialization.cst("False");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Genexp ignored) {
        return hydra.serialization.Serialization.cst("(...)");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Group g) {
        return hydra.ext.python.serde.Serde.encodeGroup((g).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.List l) {
        return hydra.ext.python.serde.Serde.encodeList((l).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Listcomp ignored) {
        return hydra.serialization.Serialization.cst("[...]");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Name n) {
        return hydra.ext.python.serde.Serde.encodeName((n).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.None ignored) {
        return hydra.serialization.Serialization.cst("None");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Number_ n) {
        return hydra.ext.python.serde.Serde.encodeNumber((n).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Set s) {
        return hydra.ext.python.serde.Serde.encodeSet((s).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Setcomp ignored) {
        return hydra.serialization.Serialization.cst("{...}");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.String_ s) {
        return hydra.ext.python.serde.Serde.encodeString((s).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.True ignored) {
        return hydra.serialization.Serialization.cst("True");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Atom.Tuple t) {
        return hydra.ext.python.serde.Serde.encodeTuple((t).value);
      }
    });
  }
  
  static hydra.ast.Expr encodeAttribute(hydra.ext.python.syntax.Attribute attr) {
    return hydra.serialization.Serialization.dotSep(hydra.lib.lists.Map.apply(
      hydra.ext.python.serde.Serde::encodeName,
      (attr).value));
  }
  
  static hydra.ast.Expr encodeAwaitPrimary(hydra.ext.python.syntax.AwaitPrimary ap) {
    Boolean await_ = (ap).await;
    hydra.ext.python.syntax.Primary primary = (ap).primary;
    return hydra.lib.logic.IfElse.lazy(
      await_,
      () -> hydra.serialization.Serialization.spaceSep(java.util.List.of(
        hydra.serialization.Serialization.cst("await"),
        hydra.ext.python.serde.Serde.encodePrimary(primary))),
      () -> hydra.ext.python.serde.Serde.encodePrimary(primary));
  }
  
  static hydra.ast.Expr encodeBitwiseAnd(hydra.ext.python.syntax.BitwiseAnd band) {
    hydra.util.Maybe<hydra.ext.python.syntax.BitwiseAnd> lhs = (band).lhs;
    hydra.ext.python.syntax.ShiftExpression rhs = (band).rhs;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.BitwiseAnd, hydra.ast.Expr>) (l -> hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.ext.python.serde.Serde.encodeBitwiseAnd(l),
          hydra.serialization.Serialization.cst("&")))),
        lhs),
      hydra.util.Maybe.just(hydra.ext.python.serde.Serde.encodeShiftExpression(rhs)))));
  }
  
  static hydra.ast.Expr encodeBitwiseOr(hydra.ext.python.syntax.BitwiseOr bor) {
    hydra.util.Maybe<hydra.ext.python.syntax.BitwiseOr> lhs = (bor).lhs;
    hydra.ext.python.syntax.BitwiseXor rhs = (bor).rhs;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.BitwiseOr, hydra.ast.Expr>) (l -> hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.ext.python.serde.Serde.encodeBitwiseOr(l),
          hydra.serialization.Serialization.cst("|")))),
        lhs),
      hydra.util.Maybe.just(hydra.ext.python.serde.Serde.encodeBitwiseXor(rhs)))));
  }
  
  static hydra.ast.Expr encodeBitwiseXor(hydra.ext.python.syntax.BitwiseXor bxor) {
    hydra.util.Maybe<hydra.ext.python.syntax.BitwiseXor> lhs = (bxor).lhs;
    hydra.ext.python.syntax.BitwiseAnd rhs = (bxor).rhs;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.BitwiseXor, hydra.ast.Expr>) (l -> hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.ext.python.serde.Serde.encodeBitwiseXor(l),
          hydra.serialization.Serialization.cst("^")))),
        lhs),
      hydra.util.Maybe.just(hydra.ext.python.serde.Serde.encodeBitwiseAnd(rhs)))));
  }
  
  static hydra.ast.Expr encodeBlock(hydra.ext.python.syntax.Block b) {
    return (b).accept(new hydra.ext.python.syntax.Block.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Block.Indented groups) {
        return hydra.serialization.Serialization.tabIndentDoubleSpace(hydra.lib.lists.Map.apply(
          (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.ast.Expr>) (stmts -> hydra.serialization.Serialization.newlineSep(hydra.lib.lists.Map.apply(
            hydra.ext.python.serde.Serde::encodeStatement,
            stmts))),
          (groups).value));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Block.Simple ss) {
        return hydra.serialization.Serialization.semicolonSep(hydra.lib.lists.Map.apply(
          hydra.ext.python.serde.Serde::encodeSimpleStatement,
          (ss).value));
      }
    });
  }
  
  static hydra.ast.Expr encodeCapturePattern(hydra.ext.python.syntax.CapturePattern cp) {
    return hydra.ext.python.serde.Serde.encodePatternCaptureTarget((cp).value);
  }
  
  static hydra.ast.Expr encodeCaseBlock(hydra.ext.python.syntax.CaseBlock cb) {
    hydra.ext.python.syntax.Block body = (cb).body;
    hydra.util.Maybe<hydra.ext.python.syntax.Guard> guard = (cb).guard;
    hydra.ext.python.syntax.Patterns patterns = (cb).patterns;
    return hydra.serialization.Serialization.newlineSep(java.util.List.of(
      hydra.serialization.Serialization.noSep(java.util.List.of(
        hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
          hydra.util.Maybe.just(hydra.serialization.Serialization.cst("case")),
          hydra.util.Maybe.just(hydra.ext.python.serde.Serde.encodePatterns(patterns)),
          hydra.lib.maybes.Map.apply(
            hydra.ext.python.serde.Serde::encodeGuard,
            guard)))),
        hydra.serialization.Serialization.cst(":"))),
      hydra.ext.python.serde.Serde.encodeBlock(body)));
  }
  
  static hydra.ast.Expr encodeClassDefinition(hydra.ext.python.syntax.ClassDefinition cd) {
    hydra.util.Maybe<hydra.ext.python.syntax.Args> args = (cd).arguments;
    hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> argPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.ext.python.syntax.Args, hydra.ast.Expr>) (a -> hydra.serialization.Serialization.noSep(java.util.List.of(
        hydra.serialization.Serialization.cst("("),
        hydra.ext.python.serde.Serde.encodeArgs(a),
        hydra.serialization.Serialization.cst(")")))),
      args));
    hydra.ext.python.syntax.Block body = (cd).body;
    hydra.util.Maybe<hydra.ext.python.syntax.Decorators> decs = (cd).decorators;
    hydra.ext.python.syntax.Name name = (cd).name;
    return hydra.serialization.Serialization.newlineSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.maybes.Map.apply(
        hydra.ext.python.serde.Serde::encodeDecorators,
        decs),
      hydra.util.Maybe.just(hydra.serialization.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
        hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.serialization.Serialization.cst("class"),
          hydra.ext.python.serde.Serde.encodeName(name)))),
        argPart.get(),
        hydra.util.Maybe.just(hydra.serialization.Serialization.cst(":")))))),
      hydra.util.Maybe.just(hydra.ext.python.serde.Serde.encodeBlock(body)))));
  }
  
  static hydra.ast.Expr encodeClassPattern(hydra.ext.python.syntax.ClassPattern cp) {
    hydra.util.Maybe<hydra.ext.python.syntax.KeywordPatterns> kw = (cp).keywordPatterns;
    hydra.ext.python.syntax.NameOrAttribute noa = (cp).nameOrAttribute;
    hydra.util.Maybe<hydra.ext.python.syntax.PositionalPatterns> pos = (cp).positionalPatterns;
    return hydra.serialization.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.util.Maybe.just(hydra.ext.python.serde.Serde.encodeNameOrAttribute(noa)),
      hydra.util.Maybe.just(hydra.serialization.Serialization.cst("(")),
      hydra.lib.maybes.Map.apply(
        hydra.ext.python.serde.Serde::encodePositionalPatterns,
        pos),
      hydra.lib.maybes.Map.apply(
        hydra.ext.python.serde.Serde::encodeKeywordPatterns,
        kw),
      hydra.util.Maybe.just(hydra.serialization.Serialization.cst(")")))));
  }
  
  static hydra.ast.Expr encodeClosedPattern(hydra.ext.python.syntax.ClosedPattern cp) {
    return (cp).accept(new hydra.ext.python.syntax.ClosedPattern.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ClosedPattern.Literal ignored) {
        return hydra.serialization.Serialization.cst("...");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ClosedPattern.Capture c) {
        return hydra.ext.python.serde.Serde.encodeCapturePattern((c).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ClosedPattern.Wildcard ignored) {
        return hydra.serialization.Serialization.cst("_");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ClosedPattern.Value v) {
        return hydra.ext.python.serde.Serde.encodeValuePattern((v).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ClosedPattern.Group ignored) {
        return hydra.serialization.Serialization.cst("(...)");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ClosedPattern.Sequence ignored) {
        return hydra.serialization.Serialization.cst("[...]");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ClosedPattern.Mapping ignored) {
        return hydra.serialization.Serialization.cst("{...}");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ClosedPattern.Class_ c) {
        return hydra.ext.python.serde.Serde.encodeClassPattern((c).value);
      }
    });
  }
  
  static hydra.ast.Expr encodeComparison(hydra.ext.python.syntax.Comparison cmp) {
    return hydra.ext.python.serde.Serde.encodeBitwiseOr((cmp).lhs);
  }
  
  static hydra.ast.Expr encodeCompoundStatement(hydra.ext.python.syntax.CompoundStatement cs) {
    return (cs).accept(new hydra.ext.python.syntax.CompoundStatement.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.CompoundStatement.Function f) {
        return hydra.ext.python.serde.Serde.encodeFunctionDefinition((f).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.CompoundStatement.If ignored) {
        return hydra.serialization.Serialization.cst("if ...");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.CompoundStatement.ClassDef c) {
        return hydra.ext.python.serde.Serde.encodeClassDefinition((c).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.CompoundStatement.With ignored) {
        return hydra.serialization.Serialization.cst("with ...");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.CompoundStatement.For ignored) {
        return hydra.serialization.Serialization.cst("for ...");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.CompoundStatement.Try ignored) {
        return hydra.serialization.Serialization.cst("try ...");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.CompoundStatement.While ignored) {
        return hydra.serialization.Serialization.cst("while ...");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.CompoundStatement.Match m) {
        return hydra.ext.python.serde.Serde.encodeMatchStatement((m).value);
      }
    });
  }
  
  static hydra.ast.Expr encodeConjunction(hydra.ext.python.syntax.Conjunction c) {
    return hydra.serialization.Serialization.symbolSep(
      "and",
      hydra.serialization.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.ext.python.serde.Serde::encodeInversion,
        (c).value));
  }
  
  static hydra.ast.Expr encodeDecorators(hydra.ext.python.syntax.Decorators decs) {
    return hydra.serialization.Serialization.newlineSep(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.python.syntax.NamedExpression, hydra.ast.Expr>) (ne -> hydra.serialization.Serialization.noSep(java.util.List.of(
        hydra.serialization.Serialization.cst("@"),
        hydra.ext.python.serde.Serde.encodeNamedExpression(ne)))),
      (decs).value));
  }
  
  static hydra.ast.Expr encodeDict(hydra.ext.python.syntax.Dict d) {
    return hydra.serialization.Serialization.curlyBracesList(
      (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
      hydra.serialization.Serialization.halfBlockStyle(),
      hydra.lib.lists.Map.apply(
        hydra.ext.python.serde.Serde::encodeDoubleStarredKvpair,
        (d).value));
  }
  
  static hydra.ast.Expr encodeDisjunction(hydra.ext.python.syntax.Disjunction d) {
    return hydra.serialization.Serialization.symbolSep(
      "or",
      hydra.serialization.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.ext.python.serde.Serde::encodeConjunction,
        (d).value));
  }
  
  static hydra.ast.Expr encodeDottedAsName(hydra.ext.python.syntax.DottedAsName dan) {
    hydra.util.Maybe<hydra.ext.python.syntax.Name> alias = (dan).as;
    hydra.ext.python.syntax.DottedName name = (dan).name;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.util.Maybe.just(hydra.ext.python.serde.Serde.encodeDottedName(name)),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.Name, hydra.ast.Expr>) (a -> hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.serialization.Serialization.cst("as"),
          hydra.ext.python.serde.Serde.encodeName(a)))),
        alias))));
  }
  
  static hydra.ast.Expr encodeDottedName(hydra.ext.python.syntax.DottedName dn) {
    return hydra.serialization.Serialization.cst(hydra.lib.strings.Intercalate.apply(
      ".",
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.Name, String>) (n -> (n).value),
        (dn).value)));
  }
  
  static hydra.ast.Expr encodeDoubleStarredKvpair(hydra.ext.python.syntax.DoubleStarredKvpair dskv) {
    return (dskv).accept(new hydra.ext.python.syntax.DoubleStarredKvpair.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.DoubleStarredKvpair.Pair p) {
        return hydra.ext.python.serde.Serde.encodeKvpair((p).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.DoubleStarredKvpair.Starred e) {
        return hydra.serialization.Serialization.noSep(java.util.List.of(
          hydra.serialization.Serialization.cst("**"),
          hydra.ext.python.serde.Serde.encodeBitwiseOr((e).value)));
      }
    });
  }
  
  static hydra.ast.Expr encodeExpression(hydra.ext.python.syntax.Expression expr) {
    return (expr).accept(new hydra.ext.python.syntax.Expression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Expression.Simple d) {
        return hydra.ext.python.serde.Serde.encodeDisjunction((d).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Expression.Conditional ignored) {
        return hydra.serialization.Serialization.cst("... if ... else ...");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Expression.Lambda l) {
        return hydra.ext.python.serde.Serde.encodeLambda((l).value);
      }
    });
  }
  
  static hydra.ast.Expr encodeFactor(hydra.ext.python.syntax.Factor f) {
    return (f).accept(new hydra.ext.python.syntax.Factor.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Factor.Positive inner) {
        return hydra.serialization.Serialization.noSep(java.util.List.of(
          hydra.serialization.Serialization.cst("+"),
          hydra.ext.python.serde.Serde.encodeFactor((inner).value)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Factor.Negative inner) {
        return hydra.serialization.Serialization.noSep(java.util.List.of(
          hydra.serialization.Serialization.cst("-"),
          hydra.ext.python.serde.Serde.encodeFactor((inner).value)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Factor.Complement inner) {
        return hydra.serialization.Serialization.noSep(java.util.List.of(
          hydra.serialization.Serialization.cst("~"),
          hydra.ext.python.serde.Serde.encodeFactor((inner).value)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Factor.Simple p) {
        return hydra.ext.python.serde.Serde.encodePower((p).value);
      }
    });
  }
  
  static hydra.ast.Expr encodeFunctionDefRaw(hydra.ext.python.syntax.FunctionDefRaw fdr) {
    Boolean async_ = (fdr).async;
    hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> asyncKw = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      async_,
      () -> hydra.util.Maybe.just(hydra.serialization.Serialization.cst("async")),
      () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing())));
    hydra.ext.python.syntax.Block block = (fdr).block;
    hydra.ext.python.syntax.Name name = (fdr).name;
    hydra.util.Maybe<hydra.ext.python.syntax.Parameters> params = (fdr).params;
    hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> paramPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      hydra.ext.python.serde.Serde::encodeParameters,
      params));
    hydra.util.Maybe<hydra.ext.python.syntax.Expression> retType = (fdr).returnType;
    hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> retPart = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ast.Expr>) (t -> hydra.serialization.Serialization.spaceSep(java.util.List.of(
        hydra.serialization.Serialization.cst("->"),
        hydra.ext.python.serde.Serde.encodeExpression(t)))),
      retType));
    java.util.List<hydra.ext.python.syntax.TypeParameter> tparams = (fdr).typeParams;
    hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> tparamPart = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(tparams),
      () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
      () -> hydra.util.Maybe.just(hydra.serialization.Serialization.bracketList(
        hydra.serialization.Serialization.inlineStyle(),
        hydra.lib.lists.Map.apply(
          hydra.ext.python.serde.Serde::encodeTypeParameter,
          tparams)))));
    return hydra.serialization.Serialization.newlineSep(java.util.List.of(
      hydra.serialization.Serialization.noSep(java.util.List.of(
        hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
          asyncKw.get(),
          hydra.util.Maybe.just(hydra.serialization.Serialization.cst("def")),
          hydra.util.Maybe.just(hydra.serialization.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
            hydra.util.Maybe.just(hydra.ext.python.serde.Serde.encodeName(name)),
            tparamPart.get(),
            hydra.util.Maybe.just(hydra.serialization.Serialization.cst("(")),
            paramPart.get(),
            hydra.util.Maybe.just(hydra.serialization.Serialization.cst(")")))))),
          retPart.get()))),
        hydra.serialization.Serialization.cst(":"))),
      hydra.ext.python.serde.Serde.encodeBlock(block)));
  }
  
  static hydra.ast.Expr encodeFunctionDefinition(hydra.ext.python.syntax.FunctionDefinition fd) {
    hydra.util.Maybe<hydra.ext.python.syntax.Decorators> decs = (fd).decorators;
    hydra.ext.python.syntax.FunctionDefRaw raw = (fd).raw;
    return hydra.serialization.Serialization.newlineSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.maybes.Map.apply(
        hydra.ext.python.serde.Serde::encodeDecorators,
        decs),
      hydra.util.Maybe.just(hydra.ext.python.serde.Serde.encodeFunctionDefRaw(raw)))));
  }
  
  static hydra.ast.Expr encodeGroup(hydra.ext.python.syntax.Group g) {
    return (g).accept(new hydra.ext.python.syntax.Group.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Group.Expression ne) {
        return hydra.ext.python.serde.Serde.encodeNamedExpression((ne).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Group.Yield ignored) {
        return hydra.serialization.Serialization.cst("(yield ...)");
      }
    });
  }
  
  static hydra.ast.Expr encodeGuard(hydra.ext.python.syntax.Guard g) {
    return hydra.serialization.Serialization.spaceSep(java.util.List.of(
      hydra.serialization.Serialization.cst("if"),
      hydra.ext.python.serde.Serde.encodeNamedExpression((g).value)));
  }
  
  static hydra.ast.Expr encodeImportFrom(hydra.ext.python.syntax.ImportFrom if_) {
    hydra.util.Maybe<hydra.ext.python.syntax.DottedName> name = (if_).dottedName;
    java.util.List<hydra.ext.python.syntax.RelativeImportPrefix> prefixes = (if_).prefixes;
    hydra.util.Lazy<hydra.ast.Expr> lhs = new hydra.util.Lazy<>(() -> hydra.serialization.Serialization.noSep(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Concat.apply(java.util.List.of(
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.RelativeImportPrefix, hydra.util.Maybe<hydra.ast.Expr>>) (p -> hydra.util.Maybe.just(hydra.ext.python.serde.Serde.encodeRelativeImportPrefix(p))),
        prefixes),
      java.util.List.of(hydra.lib.maybes.Map.apply(
        hydra.ext.python.serde.Serde::encodeDottedName,
        name)))))));
    hydra.ext.python.syntax.ImportFromTargets targets = (if_).targets;
    return hydra.serialization.Serialization.spaceSep(java.util.List.of(
      hydra.serialization.Serialization.cst("from"),
      lhs.get(),
      hydra.serialization.Serialization.cst("import"),
      hydra.ext.python.serde.Serde.encodeImportFromTargets(targets)));
  }
  
  static hydra.ast.Expr encodeImportFromAsName(hydra.ext.python.syntax.ImportFromAsName ifan) {
    hydra.util.Maybe<hydra.ext.python.syntax.Name> alias = (ifan).as;
    hydra.ext.python.syntax.Name name = (ifan).name;
    return hydra.lib.maybes.Maybe.apply(
      hydra.ext.python.serde.Serde.encodeName(name),
      (java.util.function.Function<hydra.ext.python.syntax.Name, hydra.ast.Expr>) (a -> hydra.serialization.Serialization.spaceSep(java.util.List.of(
        hydra.ext.python.serde.Serde.encodeName(name),
        hydra.serialization.Serialization.cst("as"),
        hydra.ext.python.serde.Serde.encodeName(a)))),
      alias);
  }
  
  static hydra.ast.Expr encodeImportFromTargets(hydra.ext.python.syntax.ImportFromTargets t) {
    return (t).accept(new hydra.ext.python.syntax.ImportFromTargets.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ImportFromTargets.Simple names) {
        return hydra.serialization.Serialization.commaSep(
          hydra.serialization.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.python.serde.Serde::encodeImportFromAsName,
            (names).value));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ImportFromTargets.Parens names) {
        return hydra.serialization.Serialization.noSep(java.util.List.of(
          hydra.serialization.Serialization.cst("("),
          hydra.serialization.Serialization.commaSep(
            hydra.serialization.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.ext.python.serde.Serde::encodeImportFromAsName,
              (names).value)),
          hydra.serialization.Serialization.cst(")")));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ImportFromTargets.Star ignored) {
        return hydra.serialization.Serialization.cst("*");
      }
    });
  }
  
  static hydra.ast.Expr encodeImportName(hydra.ext.python.syntax.ImportName in_) {
    return hydra.serialization.Serialization.spaceSep(java.util.List.of(
      hydra.serialization.Serialization.cst("import"),
      hydra.serialization.Serialization.commaSep(
        hydra.serialization.Serialization.inlineStyle(),
        hydra.lib.lists.Map.apply(
          hydra.ext.python.serde.Serde::encodeDottedAsName,
          (in_).value))));
  }
  
  static hydra.ast.Expr encodeImportStatement(hydra.ext.python.syntax.ImportStatement is_) {
    return (is_).accept(new hydra.ext.python.syntax.ImportStatement.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ImportStatement.Name n) {
        return hydra.ext.python.serde.Serde.encodeImportName((n).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.ImportStatement.From f) {
        return hydra.ext.python.serde.Serde.encodeImportFrom((f).value);
      }
    });
  }
  
  static hydra.ast.Expr encodeInversion(hydra.ext.python.syntax.Inversion i) {
    return (i).accept(new hydra.ext.python.syntax.Inversion.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Inversion.Not other) {
        return hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.serialization.Serialization.cst("not"),
          hydra.ext.python.serde.Serde.encodeInversion((other).value)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Inversion.Simple c) {
        return hydra.ext.python.serde.Serde.encodeComparison((c).value);
      }
    });
  }
  
  static hydra.ast.Expr encodeKeywordPattern(hydra.ext.python.syntax.KeywordPattern kp) {
    hydra.ext.python.syntax.Name name = (kp).name;
    hydra.ext.python.syntax.Pattern pat = (kp).pattern;
    return hydra.serialization.Serialization.noSep(java.util.List.of(
      hydra.ext.python.serde.Serde.encodeName(name),
      hydra.serialization.Serialization.cst("="),
      hydra.ext.python.serde.Serde.encodePattern(pat)));
  }
  
  static hydra.ast.Expr encodeKeywordPatterns(hydra.ext.python.syntax.KeywordPatterns kp) {
    return hydra.serialization.Serialization.commaSep(
      hydra.serialization.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.ext.python.serde.Serde::encodeKeywordPattern,
        (kp).value));
  }
  
  static hydra.ast.Expr encodeKvpair(hydra.ext.python.syntax.Kvpair kv) {
    hydra.ext.python.syntax.Expression k = (kv).key;
    hydra.ext.python.syntax.Expression v = (kv).value;
    return hydra.serialization.Serialization.spaceSep(java.util.List.of(
      hydra.serialization.Serialization.noSep(java.util.List.of(
        hydra.ext.python.serde.Serde.encodeExpression(k),
        hydra.serialization.Serialization.cst(":"))),
      hydra.ext.python.serde.Serde.encodeExpression(v)));
  }
  
  static hydra.ast.Expr encodeKwarg(hydra.ext.python.syntax.Kwarg k) {
    hydra.ext.python.syntax.Expression expr = (k).value;
    hydra.ext.python.syntax.Name name = (k).name;
    return hydra.serialization.Serialization.noSep(java.util.List.of(
      hydra.ext.python.serde.Serde.encodeName(name),
      hydra.serialization.Serialization.cst("="),
      hydra.ext.python.serde.Serde.encodeExpression(expr)));
  }
  
  static hydra.ast.Expr encodeKwargOrDoubleStarred(hydra.ext.python.syntax.KwargOrDoubleStarred kds) {
    return (kds).accept(new hydra.ext.python.syntax.KwargOrDoubleStarred.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.KwargOrDoubleStarred.Kwarg k) {
        return hydra.ext.python.serde.Serde.encodeKwarg((k).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.KwargOrDoubleStarred.DoubleStarred e) {
        return hydra.serialization.Serialization.noSep(java.util.List.of(
          hydra.serialization.Serialization.cst("**"),
          hydra.ext.python.serde.Serde.encodeExpression((e).value)));
      }
    });
  }
  
  static hydra.ast.Expr encodeKwargOrStarred(hydra.ext.python.syntax.KwargOrStarred ks) {
    return (ks).accept(new hydra.ext.python.syntax.KwargOrStarred.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.KwargOrStarred.Kwarg k) {
        return hydra.ext.python.serde.Serde.encodeKwarg((k).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.KwargOrStarred.Starred se) {
        return hydra.ext.python.serde.Serde.encodeStarredExpression((se).value);
      }
    });
  }
  
  static hydra.ast.Expr encodeLambda(hydra.ext.python.syntax.Lambda l) {
    hydra.ext.python.syntax.Expression body = (l).body;
    hydra.ext.python.syntax.LambdaParameters params = (l).params;
    return hydra.serialization.Serialization.parens(hydra.serialization.Serialization.spaceSep(java.util.List.of(
      hydra.serialization.Serialization.cst("lambda"),
      hydra.serialization.Serialization.noSep(java.util.List.of(
        hydra.ext.python.serde.Serde.encodeLambdaParameters(params),
        hydra.serialization.Serialization.cst(":"))),
      hydra.ext.python.serde.Serde.encodeExpression(body))));
  }
  
  static hydra.ast.Expr encodeLambdaParamNoDefault(hydra.ext.python.syntax.LambdaParamNoDefault p) {
    return hydra.ext.python.serde.Serde.encodeName((p).value);
  }
  
  static hydra.ast.Expr encodeLambdaParameters(hydra.ext.python.syntax.LambdaParameters lp) {
    java.util.List<hydra.ext.python.syntax.LambdaParamNoDefault> nodef = (lp).paramNoDefault;
    return hydra.serialization.Serialization.commaSep(
      hydra.serialization.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.ext.python.serde.Serde::encodeLambdaParamNoDefault,
        nodef));
  }
  
  static hydra.ast.Expr encodeLambdaStarEtc(hydra.ext.python.syntax.LambdaStarEtc lse) {
    return (lse).accept(new hydra.ext.python.syntax.LambdaStarEtc.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.LambdaStarEtc.ParamNoDefault p) {
        return hydra.ext.python.serde.Serde.encodeLambdaParamNoDefault((p).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.LambdaStarEtc.Star ignored) {
        return hydra.serialization.Serialization.cst("*...");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.LambdaStarEtc.ParamMaybeDefault ignored) {
        return hydra.serialization.Serialization.cst("...");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.LambdaStarEtc.Kwds ignored) {
        return hydra.serialization.Serialization.cst("**...");
      }
    });
  }
  
  static hydra.ast.Expr encodeList(hydra.ext.python.syntax.List l) {
    return hydra.serialization.Serialization.bracketListAdaptive(hydra.lib.lists.Map.apply(
      hydra.ext.python.serde.Serde::encodeStarNamedExpression,
      (l).value));
  }
  
  static hydra.ast.Expr encodeMatchStatement(hydra.ext.python.syntax.MatchStatement ms) {
    java.util.List<hydra.ext.python.syntax.CaseBlock> cases = (ms).cases;
    hydra.ext.python.syntax.SubjectExpression subj = (ms).subject;
    return hydra.serialization.Serialization.newlineSep(java.util.List.of(
      hydra.serialization.Serialization.spaceSep(java.util.List.of(
        hydra.serialization.Serialization.cst("match"),
        hydra.serialization.Serialization.noSep(java.util.List.of(
          hydra.ext.python.serde.Serde.encodeSubjectExpression(subj),
          hydra.serialization.Serialization.cst(":"))))),
      hydra.serialization.Serialization.tabIndentDoubleSpace(hydra.lib.lists.Map.apply(
        hydra.ext.python.serde.Serde::encodeCaseBlock,
        cases))));
  }
  
  static hydra.ast.Expr encodeModule(hydra.ext.python.syntax.Module mod) {
    hydra.util.Lazy<java.util.List<hydra.ast.Expr>> groups = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<hydra.ext.python.syntax.Statement>, hydra.ast.Expr>) (group -> hydra.serialization.Serialization.newlineSep(hydra.lib.lists.Map.apply(
        hydra.ext.python.serde.Serde::encodeStatement,
        group))),
      (mod).value));
    hydra.ast.Expr warning = hydra.serialization.Serialization.cst(hydra.ext.python.serde.Serde.toPythonComments(hydra.constants.Constants.warningAutoGeneratedFile()));
    return hydra.serialization.Serialization.doubleNewlineSep(hydra.lib.lists.Cons.apply(
      warning,
      groups.get()));
  }
  
  static hydra.ast.Expr encodeName(hydra.ext.python.syntax.Name n) {
    return hydra.serialization.Serialization.cst((n).value);
  }
  
  static hydra.ast.Expr encodeNamedExpression(hydra.ext.python.syntax.NamedExpression ne) {
    return (ne).accept(new hydra.ext.python.syntax.NamedExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.NamedExpression.Simple e) {
        return hydra.ext.python.serde.Serde.encodeExpression((e).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.NamedExpression.Assignment ae) {
        return hydra.ext.python.serde.Serde.encodeAssignmentExpression((ae).value);
      }
    });
  }
  
  static hydra.ast.Expr encodeNameOrAttribute(hydra.ext.python.syntax.NameOrAttribute noa) {
    return hydra.serialization.Serialization.dotSep(hydra.lib.lists.Map.apply(
      hydra.ext.python.serde.Serde::encodeName,
      (noa).value));
  }
  
  static hydra.ast.Expr encodeNumber(hydra.ext.python.syntax.Number_ num) {
    return (num).accept(new hydra.ext.python.syntax.Number_.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Number_.Float_ f) {
        return hydra.serialization.Serialization.cst(hydra.lib.literals.ShowBigfloat.apply((f).value));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Number_.Integer_ i) {
        return hydra.serialization.Serialization.cst(hydra.lib.literals.ShowBigint.apply((i).value));
      }
    });
  }
  
  static hydra.ast.Expr encodeOrPattern(hydra.ext.python.syntax.OrPattern op) {
    return hydra.serialization.Serialization.symbolSep(
      "|",
      hydra.serialization.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.ext.python.serde.Serde::encodeClosedPattern,
        (op).value));
  }
  
  static hydra.ast.Expr encodeParam(hydra.ext.python.syntax.Param p) {
    hydra.util.Maybe<hydra.ext.python.syntax.Annotation> ann = (p).annotation;
    hydra.ext.python.syntax.Name name = (p).name;
    return hydra.serialization.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.util.Maybe.just(hydra.ext.python.serde.Serde.encodeName(name)),
      hydra.lib.maybes.Map.apply(
        hydra.ext.python.serde.Serde::encodeAnnotation,
        ann))));
  }
  
  static hydra.ast.Expr encodeParamNoDefault(hydra.ext.python.syntax.ParamNoDefault pnd) {
    return hydra.ext.python.serde.Serde.encodeParam((pnd).param);
  }
  
  static hydra.ast.Expr encodeParamNoDefaultParameters(hydra.ext.python.syntax.ParamNoDefaultParameters pndp) {
    java.util.List<hydra.ext.python.syntax.ParamNoDefault> nodef = (pndp).paramNoDefault;
    return hydra.serialization.Serialization.commaSep(
      hydra.serialization.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.ext.python.serde.Serde::encodeParamNoDefault,
        nodef));
  }
  
  static hydra.ast.Expr encodeParameters(hydra.ext.python.syntax.Parameters p) {
    return (p).accept(new hydra.ext.python.syntax.Parameters.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Parameters.ParamNoDefault pnd) {
        return hydra.ext.python.serde.Serde.encodeParamNoDefaultParameters((pnd).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Parameters.SlashNoDefault ignored) {
        return hydra.serialization.Serialization.cst("...");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Parameters.SlashWithDefault ignored) {
        return hydra.serialization.Serialization.cst("...");
      }
    });
  }
  
  static hydra.ast.Expr encodePattern(hydra.ext.python.syntax.Pattern p) {
    return (p).accept(new hydra.ext.python.syntax.Pattern.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Pattern.Or op) {
        return hydra.ext.python.serde.Serde.encodeOrPattern((op).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Pattern.As ignored) {
        return hydra.serialization.Serialization.cst("... as ...");
      }
    });
  }
  
  static hydra.ast.Expr encodePatternCaptureTarget(hydra.ext.python.syntax.PatternCaptureTarget pct) {
    return hydra.ext.python.serde.Serde.encodeName((pct).value);
  }
  
  static hydra.ast.Expr encodePatterns(hydra.ext.python.syntax.Patterns ps) {
    return (ps).accept(new hydra.ext.python.syntax.Patterns.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Patterns.Pattern p) {
        return hydra.ext.python.serde.Serde.encodePattern((p).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Patterns.Sequence ignored) {
        return hydra.serialization.Serialization.cst("...");
      }
    });
  }
  
  static hydra.ast.Expr encodePosArg(hydra.ext.python.syntax.PosArg pa) {
    return (pa).accept(new hydra.ext.python.syntax.PosArg.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.PosArg.Starred se) {
        return hydra.ext.python.serde.Serde.encodeStarredExpression((se).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.PosArg.Assignment ae) {
        return hydra.ext.python.serde.Serde.encodeAssignmentExpression((ae).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.PosArg.Expression e) {
        return hydra.ext.python.serde.Serde.encodeExpression((e).value);
      }
    });
  }
  
  static hydra.ast.Expr encodePositionalPatterns(hydra.ext.python.syntax.PositionalPatterns pp) {
    return hydra.serialization.Serialization.commaSep(
      hydra.serialization.Serialization.inlineStyle(),
      hydra.lib.lists.Map.apply(
        hydra.ext.python.serde.Serde::encodePattern,
        (pp).value));
  }
  
  static hydra.ast.Expr encodePower(hydra.ext.python.syntax.Power p) {
    hydra.ext.python.syntax.AwaitPrimary lhs = (p).lhs;
    hydra.util.Maybe<hydra.ext.python.syntax.Factor> rhs = (p).rhs;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.util.Maybe.just(hydra.ext.python.serde.Serde.encodeAwaitPrimary(lhs)),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.Factor, hydra.ast.Expr>) (r -> hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.serialization.Serialization.cst("**"),
          hydra.ext.python.serde.Serde.encodeFactor(r)))),
        rhs))));
  }
  
  static hydra.ast.Expr encodePrimary(hydra.ext.python.syntax.Primary p) {
    return (p).accept(new hydra.ext.python.syntax.Primary.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Primary.Simple a) {
        return hydra.ext.python.serde.Serde.encodeAtom((a).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Primary.Compound pwr) {
        return hydra.ext.python.serde.Serde.encodePrimaryWithRhs((pwr).value);
      }
    });
  }
  
  static hydra.ast.Expr encodePrimaryRhs(hydra.ext.python.syntax.PrimaryRhs rhs) {
    return (rhs).accept(new hydra.ext.python.syntax.PrimaryRhs.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.PrimaryRhs.Call args) {
        return hydra.serialization.Serialization.noSep(java.util.List.of(
          hydra.serialization.Serialization.cst("("),
          hydra.ext.python.serde.Serde.encodeArgs((args).value),
          hydra.serialization.Serialization.cst(")")));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.PrimaryRhs.Project name) {
        return hydra.serialization.Serialization.noSep(java.util.List.of(
          hydra.serialization.Serialization.cst("."),
          hydra.ext.python.serde.Serde.encodeName((name).value)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.PrimaryRhs.Slices slices) {
        return hydra.serialization.Serialization.noSep(java.util.List.of(
          hydra.serialization.Serialization.cst("["),
          hydra.ext.python.serde.Serde.encodeSlices((slices).value),
          hydra.serialization.Serialization.cst("]")));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.PrimaryRhs.Genexp ignored) {
        return hydra.serialization.Serialization.cst("[...]");
      }
    });
  }
  
  static hydra.ast.Expr encodePrimaryWithRhs(hydra.ext.python.syntax.PrimaryWithRhs pwr) {
    hydra.ext.python.syntax.Primary prim = (pwr).primary;
    hydra.ext.python.syntax.PrimaryRhs rhs = (pwr).rhs;
    return hydra.serialization.Serialization.noSep(java.util.List.of(
      hydra.ext.python.serde.Serde.encodePrimary(prim),
      hydra.ext.python.serde.Serde.encodePrimaryRhs(rhs)));
  }
  
  static hydra.ast.Expr encodeRaiseExpression(hydra.ext.python.syntax.RaiseExpression re) {
    hydra.ext.python.syntax.Expression expr = (re).expression;
    hydra.util.Maybe<hydra.ext.python.syntax.Expression> from_ = (re).from;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.util.Maybe.just(hydra.ext.python.serde.Serde.encodeExpression(expr)),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.ext.python.syntax.Expression, hydra.ast.Expr>) (f -> hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.serialization.Serialization.cst("from"),
          hydra.ext.python.serde.Serde.encodeExpression(f)))),
        from_))));
  }
  
  static hydra.ast.Expr encodeRaiseStatement(hydra.ext.python.syntax.RaiseStatement rs) {
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.util.Maybe.just(hydra.serialization.Serialization.cst("raise")),
      hydra.lib.maybes.Map.apply(
        hydra.ext.python.serde.Serde::encodeRaiseExpression,
        (rs).value))));
  }
  
  static hydra.ast.Expr encodeRelativeImportPrefix(hydra.ext.python.syntax.RelativeImportPrefix p) {
    return (p).accept(new hydra.ext.python.syntax.RelativeImportPrefix.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.RelativeImportPrefix.Dot ignored) {
        return hydra.serialization.Serialization.cst(".");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.RelativeImportPrefix.Ellipsis ignored) {
        return hydra.serialization.Serialization.cst("...");
      }
    });
  }
  
  static hydra.ast.Expr encodeReturnStatement(hydra.ext.python.syntax.ReturnStatement rs) {
    return hydra.serialization.Serialization.spaceSep(java.util.List.of(
      hydra.serialization.Serialization.cst("return"),
      hydra.serialization.Serialization.commaSep(
        hydra.serialization.Serialization.inlineStyle(),
        hydra.lib.lists.Map.apply(
          hydra.ext.python.serde.Serde::encodeStarExpression,
          (rs).value))));
  }
  
  static hydra.ast.Expr encodeSet(hydra.ext.python.syntax.Set s) {
    return hydra.serialization.Serialization.bracesListAdaptive(hydra.lib.lists.Map.apply(
      hydra.ext.python.serde.Serde::encodeStarNamedExpression,
      (s).value));
  }
  
  static hydra.ast.Expr encodeShiftExpression(hydra.ext.python.syntax.ShiftExpression se) {
    return hydra.ext.python.serde.Serde.encodeSum((se).rhs);
  }
  
  static hydra.ast.Expr encodeSimpleStatement(hydra.ext.python.syntax.SimpleStatement ss) {
    return (ss).accept(new hydra.ext.python.syntax.SimpleStatement.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Assignment a) {
        return hydra.ext.python.serde.Serde.encodeAssignment((a).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.StarExpressions es) {
        return hydra.serialization.Serialization.newlineSep(hydra.lib.lists.Map.apply(
          hydra.ext.python.serde.Serde::encodeStarExpression,
          (es).value));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Return r) {
        return hydra.ext.python.serde.Serde.encodeReturnStatement((r).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Raise r) {
        return hydra.ext.python.serde.Serde.encodeRaiseStatement((r).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Pass ignored) {
        return hydra.serialization.Serialization.cst("pass");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Break ignored) {
        return hydra.serialization.Serialization.cst("break");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Continue ignored) {
        return hydra.serialization.Serialization.cst("continue");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Import i) {
        return hydra.ext.python.serde.Serde.encodeImportStatement((i).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.TypeAlias t) {
        return hydra.ext.python.serde.Serde.encodeTypeAlias((t).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Assert ignored) {
        return hydra.serialization.Serialization.cst("assert ...");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Global ignored) {
        return hydra.serialization.Serialization.cst("global ...");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Nonlocal ignored) {
        return hydra.serialization.Serialization.cst("nonlocal ...");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SimpleStatement.Del ignored) {
        return hydra.serialization.Serialization.cst("del ...");
      }
    });
  }
  
  static hydra.ast.Expr encodeSimpleTypeParameter(hydra.ext.python.syntax.SimpleTypeParameter stp) {
    return hydra.ext.python.serde.Serde.encodeName((stp).name);
  }
  
  static hydra.ast.Expr encodeSingleTarget(hydra.ext.python.syntax.SingleTarget st) {
    return (st).accept(new hydra.ext.python.syntax.SingleTarget.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SingleTarget.Name n) {
        return hydra.ext.python.serde.Serde.encodeName((n).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SingleTarget.Parens ignored) {
        return hydra.serialization.Serialization.cst("(...)");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SingleTarget.SubscriptAttributeTarget ignored) {
        return hydra.serialization.Serialization.cst("...");
      }
    });
  }
  
  static hydra.ast.Expr encodeSlice(hydra.ext.python.syntax.Slice s) {
    return (s).accept(new hydra.ext.python.syntax.Slice.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Slice.Named ne) {
        return hydra.ext.python.serde.Serde.encodeNamedExpression((ne).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Slice.Slice_ ignored) {
        return hydra.serialization.Serialization.cst(":");
      }
    });
  }
  
  static hydra.ast.Expr encodeSliceOrStarredExpression(hydra.ext.python.syntax.SliceOrStarredExpression s) {
    return (s).accept(new hydra.ext.python.syntax.SliceOrStarredExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SliceOrStarredExpression.Slice sl) {
        return hydra.ext.python.serde.Serde.encodeSlice((sl).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SliceOrStarredExpression.Starred se) {
        return hydra.ext.python.serde.Serde.encodeStarredExpression((se).value);
      }
    });
  }
  
  static hydra.ast.Expr encodeSlices(hydra.ext.python.syntax.Slices s) {
    hydra.ext.python.syntax.Slice hd = (s).head;
    java.util.List<hydra.ext.python.syntax.SliceOrStarredExpression> tl = (s).tail;
    return hydra.serialization.Serialization.commaSep(
      hydra.serialization.Serialization.inlineStyle(),
      hydra.lib.lists.Cons.apply(
        hydra.ext.python.serde.Serde.encodeSlice(hd),
        hydra.lib.lists.Map.apply(
          hydra.ext.python.serde.Serde::encodeSliceOrStarredExpression,
          tl)));
  }
  
  static hydra.ast.Expr encodeStarAtom(hydra.ext.python.syntax.StarAtom sa) {
    return (sa).accept(new hydra.ext.python.syntax.StarAtom.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarAtom.Name n) {
        return hydra.ext.python.serde.Serde.encodeName((n).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarAtom.TargetWithStarAtom ignored) {
        return hydra.serialization.Serialization.cst("(...)");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarAtom.StarTargetsTupleSeq ignored) {
        return hydra.serialization.Serialization.cst("(...)");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarAtom.StarTargetsListSeq ignored) {
        return hydra.serialization.Serialization.cst("[...]");
      }
    });
  }
  
  static hydra.ast.Expr encodeStarExpression(hydra.ext.python.syntax.StarExpression se) {
    return (se).accept(new hydra.ext.python.syntax.StarExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarExpression.Star bor) {
        return hydra.serialization.Serialization.noSep(java.util.List.of(
          hydra.serialization.Serialization.cst("*"),
          hydra.ext.python.serde.Serde.encodeBitwiseOr((bor).value)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarExpression.Simple e) {
        return hydra.ext.python.serde.Serde.encodeExpression((e).value);
      }
    });
  }
  
  static hydra.ast.Expr encodeStarNamedExpression(hydra.ext.python.syntax.StarNamedExpression sne) {
    return (sne).accept(new hydra.ext.python.syntax.StarNamedExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarNamedExpression.Star bor) {
        return hydra.serialization.Serialization.noSep(java.util.List.of(
          hydra.serialization.Serialization.cst("*"),
          hydra.ext.python.serde.Serde.encodeBitwiseOr((bor).value)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarNamedExpression.Simple ne) {
        return hydra.ext.python.serde.Serde.encodeNamedExpression((ne).value);
      }
    });
  }
  
  static hydra.ast.Expr encodeStarTarget(hydra.ext.python.syntax.StarTarget st) {
    return (st).accept(new hydra.ext.python.syntax.StarTarget.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarTarget.Unstarred t) {
        return hydra.ext.python.serde.Serde.encodeTargetWithStarAtom((t).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.StarTarget.Starred inner) {
        return hydra.serialization.Serialization.noSep(java.util.List.of(
          hydra.serialization.Serialization.cst("*"),
          hydra.ext.python.serde.Serde.encodeStarTarget((inner).value)));
      }
    });
  }
  
  static hydra.ast.Expr encodeStarredExpression(hydra.ext.python.syntax.StarredExpression se) {
    return hydra.serialization.Serialization.noSep(java.util.List.of(
      hydra.serialization.Serialization.cst("*"),
      hydra.ext.python.serde.Serde.encodeExpression((se).value)));
  }
  
  static hydra.ast.Expr encodeStatement(hydra.ext.python.syntax.Statement stmt) {
    return (stmt).accept(new hydra.ext.python.syntax.Statement.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Statement.Annotated a) {
        return hydra.ext.python.serde.Serde.encodeAnnotatedStatement((a).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Statement.Simple ss) {
        return hydra.serialization.Serialization.newlineSep(hydra.lib.lists.Map.apply(
          hydra.ext.python.serde.Serde::encodeSimpleStatement,
          (ss).value));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.Statement.Compound c) {
        return hydra.ext.python.serde.Serde.encodeCompoundStatement((c).value);
      }
    });
  }
  
  static hydra.ast.Expr encodeString(hydra.ext.python.syntax.String_ s) {
    String content = (s).value;
    hydra.ext.python.syntax.QuoteStyle style = (s).quoteStyle;
    return (style).accept(new hydra.ext.python.syntax.QuoteStyle.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.QuoteStyle.Single ignored) {
        return hydra.serialization.Serialization.cst(hydra.ext.python.serde.Serde.escapePythonString(
          false,
          content));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.QuoteStyle.Double_ ignored) {
        return hydra.serialization.Serialization.cst(hydra.ext.python.serde.Serde.escapePythonString(
          true,
          content));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.QuoteStyle.Triple ignored) {
        return hydra.serialization.Serialization.noSep(java.util.List.of(
          hydra.serialization.Serialization.cst("r\"\"\""),
          hydra.serialization.Serialization.cst(content),
          hydra.serialization.Serialization.cst("\"\"\"")));
      }
    });
  }
  
  static hydra.ast.Expr encodeSubjectExpression(hydra.ext.python.syntax.SubjectExpression se) {
    return (se).accept(new hydra.ext.python.syntax.SubjectExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SubjectExpression.Simple ne) {
        return hydra.ext.python.serde.Serde.encodeNamedExpression((ne).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.SubjectExpression.Tuple ignored) {
        return hydra.serialization.Serialization.cst("*...");
      }
    });
  }
  
  static hydra.ast.Expr encodeSum(hydra.ext.python.syntax.Sum s) {
    return hydra.ext.python.serde.Serde.encodeTerm((s).rhs);
  }
  
  static hydra.ast.Expr encodeTerm(hydra.ext.python.syntax.Term t) {
    return hydra.ext.python.serde.Serde.encodeFactor((t).rhs);
  }
  
  static hydra.ast.Expr encodeTargetWithStarAtom(hydra.ext.python.syntax.TargetWithStarAtom t) {
    return (t).accept(new hydra.ext.python.syntax.TargetWithStarAtom.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.TargetWithStarAtom.Atom a) {
        return hydra.ext.python.serde.Serde.encodeStarAtom((a).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.TargetWithStarAtom.Project ignored) {
        return hydra.serialization.Serialization.cst("...");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.TargetWithStarAtom.Slices ignored) {
        return hydra.serialization.Serialization.cst("...");
      }
    });
  }
  
  static hydra.ast.Expr encodeTuple(hydra.ext.python.syntax.Tuple t) {
    java.util.List<hydra.ext.python.syntax.StarNamedExpression> es = (t).value;
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Length.apply(es),
        1),
      () -> hydra.serialization.Serialization.parens(hydra.serialization.Serialization.noSep(java.util.List.of(
        hydra.ext.python.serde.Serde.encodeStarNamedExpression(hydra.lib.lists.Head.apply(es)),
        hydra.serialization.Serialization.cst(",")))),
      () -> hydra.serialization.Serialization.parenList(
        false,
        hydra.lib.lists.Map.apply(
          hydra.ext.python.serde.Serde::encodeStarNamedExpression,
          es)));
  }
  
  static hydra.ast.Expr encodeTypeAlias(hydra.ext.python.syntax.TypeAlias ta) {
    hydra.ext.python.syntax.Name name = (ta).name;
    java.util.List<hydra.ext.python.syntax.TypeParameter> tparams = (ta).typeParams;
    hydra.util.Lazy<hydra.ast.Expr> alias = new hydra.util.Lazy<>(() -> hydra.serialization.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.util.Maybe.just(hydra.ext.python.serde.Serde.encodeName(name)),
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(tparams),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.bracketList(
          hydra.serialization.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.python.serde.Serde::encodeTypeParameter,
            tparams))))))));
    hydra.ext.python.syntax.Expression expr = (ta).expression;
    return hydra.serialization.Serialization.spaceSep(java.util.List.of(
      hydra.serialization.Serialization.cst("type"),
      alias.get(),
      hydra.serialization.Serialization.cst("="),
      hydra.ext.python.serde.Serde.encodeExpression(expr)));
  }
  
  static hydra.ast.Expr encodeTypeParameter(hydra.ext.python.syntax.TypeParameter tp) {
    return (tp).accept(new hydra.ext.python.syntax.TypeParameter.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.TypeParameter.Simple s) {
        return hydra.ext.python.serde.Serde.encodeSimpleTypeParameter((s).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.TypeParameter.Star ignored) {
        return hydra.serialization.Serialization.cst("*...");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.python.syntax.TypeParameter.DoubleStar ignored) {
        return hydra.serialization.Serialization.cst("**...");
      }
    });
  }
  
  static hydra.ast.Expr encodeTypedAssignment(hydra.ext.python.syntax.TypedAssignment ta) {
    hydra.ext.python.syntax.SingleTarget lhs = (ta).lhs;
    hydra.util.Maybe<hydra.ext.python.syntax.AnnotatedRhs> rhs = (ta).rhs;
    hydra.ext.python.syntax.Expression typ = (ta).type;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.util.Maybe.just(hydra.serialization.Serialization.noSep(java.util.List.of(
        hydra.ext.python.serde.Serde.encodeSingleTarget(lhs),
        hydra.serialization.Serialization.cst(":")))),
      hydra.util.Maybe.just(hydra.ext.python.serde.Serde.encodeExpression(typ)),
      hydra.lib.maybes.Map.apply(
        hydra.ext.python.serde.Serde::encodeAnnotatedRhs,
        rhs))));
  }
  
  static hydra.ast.Expr encodeUntypedAssignment(hydra.ext.python.syntax.UntypedAssignment ua) {
    hydra.ext.python.syntax.AnnotatedRhs rhs = (ua).rhs;
    java.util.List<hydra.ext.python.syntax.StarTarget> targets = (ua).targets;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.List.of(
      hydra.lib.lists.Map.apply(
        hydra.ext.python.serde.Serde::encodeStarTarget,
        targets),
      java.util.List.of(hydra.ext.python.serde.Serde.encodeAnnotatedRhs(rhs)))));
  }
  
  static hydra.ast.Expr encodeValuePattern(hydra.ext.python.syntax.ValuePattern vp) {
    return hydra.ext.python.serde.Serde.encodeAttribute((vp).value);
  }
  
  static String escapePythonString(Boolean doubleQuoted, String s) {
    java.util.function.Function<String, java.util.function.Function<String, java.util.function.Function<String, String>>> replace = (java.util.function.Function<String, java.util.function.Function<String, java.util.function.Function<String, String>>>) (old -> (java.util.function.Function<String, java.util.function.Function<String, String>>) (new_ -> (java.util.function.Function<String, String>) (str -> hydra.lib.strings.Intercalate.apply(
      new_,
      hydra.lib.strings.SplitOn.apply(
        old,
        str)))));
    String s1 = (((replace).apply("\\")).apply("\\\\")).apply(s);
    String s2 = (((replace).apply("\u0000")).apply("\\x00")).apply(s1);
    String s3 = (((replace).apply("\n")).apply("\\n")).apply(s2);
    String s4 = (((replace).apply("\t")).apply("\\t")).apply(s3);
    String s5 = (((replace).apply("\r")).apply("\\r")).apply(s4);
    hydra.util.Lazy<String> escaped = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      doubleQuoted,
      () -> (((replace).apply("\"")).apply("\\\"")).apply(s5),
      () -> (((replace).apply("'")).apply("\\'")).apply(s5)));
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
