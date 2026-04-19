// Note: this is an automatically generated file. Do not edit.

package hydra.java;

/**
 * Java serializer: converts Java AST to concrete syntax
 */
public interface Serde {
  static String escapeJavaChar(Integer c) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        c,
        34),
      () -> "\\\"",
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          c,
          92),
        () -> "\\\\",
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            c,
            10),
          () -> "\\n",
          () -> hydra.lib.logic.IfElse.lazy(
            hydra.lib.equality.Equal.apply(
              c,
              13),
            () -> "\\r",
            () -> hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                c,
                9),
              () -> "\\t",
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.equality.Equal.apply(
                  c,
                  8),
                () -> "\\b",
                () -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.equality.Equal.apply(
                    c,
                    12),
                  () -> "\\f",
                  () -> hydra.lib.logic.IfElse.lazy(
                    hydra.lib.logic.And.apply(
                      hydra.lib.equality.Gte.apply(
                        c,
                        32),
                      hydra.lib.equality.Lt.apply(
                        c,
                        127)),
                    () -> hydra.lib.strings.FromList.apply(java.util.Arrays.asList(c)),
                    () -> hydra.java.Serde.javaUnicodeEscape(c)))))))));
  }

  static String escapeJavaString(String s) {
    return hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<Integer, String>) (c -> hydra.java.Serde.escapeJavaChar(c)),
      hydra.lib.strings.ToList.apply(s)));
  }

  static Integer hexDigit(Integer n) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Lt.apply(
        n,
        10),
      () -> hydra.lib.math.Add.apply(
        n,
        48),
      () -> hydra.lib.math.Add.apply(
        hydra.lib.math.Sub.apply(
          n,
          10),
        65));
  }

  static String javaFloatLiteralText(String s) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        s,
        "NaN"),
      () -> "Double.NaN",
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          s,
          "Infinity"),
        () -> "Double.POSITIVE_INFINITY",
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            s,
            "-Infinity"),
          () -> "Double.NEGATIVE_INFINITY",
          () -> s)));
  }

  static String javaUnicodeEscape(Integer n) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Gt.apply(
        n,
        65535),
      () -> ((java.util.function.Supplier<String>) (() -> {
        Integer n_ = hydra.lib.math.Sub.apply(
          n,
          65536);
        return ((java.util.function.Supplier<String>) (() -> {
          hydra.util.Lazy<Integer> hi = new hydra.util.Lazy<>(() -> hydra.lib.math.Add.apply(
            55296,
            hydra.lib.maybes.FromMaybe.applyLazy(
              () -> 0,
              hydra.lib.math.MaybeDiv.apply(
                n_,
                1024))));
          return ((java.util.function.Supplier<String>) (() -> {
            hydra.util.Lazy<Integer> lo = new hydra.util.Lazy<>(() -> hydra.lib.math.Add.apply(
              56320,
              hydra.lib.maybes.FromMaybe.applyLazy(
                () -> 0,
                hydra.lib.math.MaybeMod.apply(
                  n_,
                  1024))));
            return hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "\\u",
                hydra.java.Serde.padHex4(hi.get())),
              hydra.lib.strings.Cat2.apply(
                "\\u",
                hydra.java.Serde.padHex4(lo.get())));
          })).get();
        })).get();
      })).get(),
      () -> hydra.lib.strings.Cat2.apply(
        "\\u",
        hydra.java.Serde.padHex4(n)));
  }

  static String padHex4(Integer n) {
    hydra.util.Lazy<Integer> r3 = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
      () -> 0,
      hydra.lib.math.MaybeMod.apply(
        n,
        4096)));
    hydra.util.Lazy<Integer> r2 = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
      () -> 0,
      hydra.lib.math.MaybeMod.apply(
        r3.get(),
        256)));
    hydra.util.Lazy<Integer> d0 = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
      () -> 0,
      hydra.lib.math.MaybeMod.apply(
        r2.get(),
        16)));
    hydra.util.Lazy<Integer> d1 = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
      () -> 0,
      hydra.lib.math.MaybeDiv.apply(
        r2.get(),
        16)));
    hydra.util.Lazy<Integer> d2 = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
      () -> 0,
      hydra.lib.math.MaybeDiv.apply(
        r3.get(),
        256)));
    hydra.util.Lazy<Integer> d3 = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
      () -> 0,
      hydra.lib.math.MaybeDiv.apply(
        n,
        4096)));
    return hydra.lib.strings.FromList.apply(java.util.Arrays.asList(
      hydra.java.Serde.hexDigit(d3.get()),
      hydra.java.Serde.hexDigit(d2.get()),
      hydra.java.Serde.hexDigit(d1.get()),
      hydra.java.Serde.hexDigit(d0.get())));
  }

  static String sanitizeJavaComment(String s) {
    return hydra.lib.strings.Intercalate.apply(
      "&gt;",
      hydra.lib.strings.SplitOn.apply(
        ">",
        hydra.lib.strings.Intercalate.apply(
          "&lt;",
          hydra.lib.strings.SplitOn.apply(
            "<",
            s))));
  }

  static hydra.ast.Expr singleLineComment(String c) {
    return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
      "// ",
      hydra.java.Serde.sanitizeJavaComment(c)));
  }

  static hydra.ast.Expr withComments(hydra.util.Maybe<String> mc, hydra.ast.Expr expr) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> expr,
      (java.util.function.Function<String, hydra.ast.Expr>) (c -> hydra.Serialization.newlineSep(java.util.Arrays.asList(
        hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
          "/**\n",
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Intercalate.apply(
              "\n",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<String, String>) (l -> hydra.lib.strings.Cat2.apply(
                  " * ",
                  l)),
                hydra.lib.strings.Lines.apply(hydra.java.Serde.sanitizeJavaComment(c)))),
            "\n */"))),
        expr))),
      mc);
  }

  static hydra.ast.Expr writeAdditionalBound(hydra.java.syntax.AdditionalBound ab) {
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst("&"),
      hydra.java.Serde.writeInterfaceType((ab).value)));
  }

  static hydra.ast.Expr writeAdditiveExpression(hydra.java.syntax.AdditiveExpression e) {
    return (e).accept(new hydra.java.syntax.AdditiveExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.AdditiveExpression.Unary m) {
        return hydra.java.Serde.writeMultiplicativeExpression((m).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.AdditiveExpression.Plus b) {
        return hydra.Serialization.infixWs(
          "+",
          hydra.java.Serde.writeAdditiveExpression((b).value.lhs),
          hydra.java.Serde.writeMultiplicativeExpression((b).value.rhs));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.AdditiveExpression.Minus b) {
        return hydra.Serialization.infixWs(
          "-",
          hydra.java.Serde.writeAdditiveExpression((b).value.lhs),
          hydra.java.Serde.writeMultiplicativeExpression((b).value.rhs));
      }
    });
  }

  static hydra.ast.Expr writeAmbiguousName(hydra.java.syntax.AmbiguousName an) {
    return hydra.Serialization.dotSep(hydra.lib.lists.Map.apply(
      hydra.java.Serde::writeIdentifier,
      (an).value));
  }

  static hydra.ast.Expr writeAndExpression(hydra.java.syntax.AndExpression ae) {
    return hydra.Serialization.infixWsList(
      "&",
      hydra.lib.lists.Map.apply(
        hydra.java.Serde::writeEqualityExpression,
        (ae).value));
  }

  static hydra.ast.Expr writeAnnotatedIdentifier(hydra.java.syntax.AnnotatedIdentifier ai) {
    return hydra.java.Serde.writeIdentifier((ai).identifier);
  }

  static hydra.ast.Expr writeAnnotation(hydra.java.syntax.Annotation ann) {
    return (ann).accept(new hydra.java.syntax.Annotation.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Annotation.Normal n) {
        return hydra.java.Serde.writeNormalAnnotation((n).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Annotation.Marker m) {
        return hydra.java.Serde.writeMarkerAnnotation((m).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Annotation.SingleElement s) {
        return hydra.java.Serde.writeSingleElementAnnotation((s).value);
      }
    });
  }

  static <T0> hydra.ast.Expr writeAnnotationTypeDeclaration(T0 ignored) {
    return hydra.Serialization.cst("STUB:AnnotationTypeDeclaration");
  }

  static <T0> hydra.ast.Expr writeArrayAccess(T0 ignored) {
    return hydra.Serialization.cst("STUB:ArrayAccess");
  }

  static hydra.ast.Expr writeArrayCreationExpression(hydra.java.syntax.ArrayCreationExpression ace) {
    return (ace).accept(new hydra.java.syntax.ArrayCreationExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ArrayCreationExpression.PrimitiveArray pa) {
        hydra.java.syntax.ArrayInitializer ai = (pa).value.array;
        hydra.java.syntax.PrimitiveTypeWithAnnotations pt = (pa).value.type;
        return hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("new"),
          hydra.Serialization.noSep(java.util.Arrays.asList(
            hydra.java.Serde.writePrimitiveTypeWithAnnotations(pt),
            hydra.Serialization.cst("[]"))),
          hydra.java.Serde.writeArrayInitializer(ai)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ArrayCreationExpression.ClassOrInterfaceArray ignored) {
        return hydra.Serialization.cst("STUB:ArrayCreationExpression");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ArrayCreationExpression.Primitive ignored) {
        return hydra.Serialization.cst("STUB:ArrayCreationExpression");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ArrayCreationExpression.ClassOrInterface ignored) {
        return hydra.Serialization.cst("STUB:ArrayCreationExpression");
      }
    });
  }

  static hydra.ast.Expr writeArrayInitializer(hydra.java.syntax.ArrayInitializer ai) {
    java.util.List<java.util.List<hydra.java.syntax.VariableInitializer>> groups = (ai).value;
    return hydra.lib.maybes.FromMaybe.applyLazy(
      () -> hydra.Serialization.cst("{}"),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<java.util.List<hydra.java.syntax.VariableInitializer>, hydra.ast.Expr>) (firstGroup -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            hydra.lib.lists.Length.apply(groups),
            1),
          () -> hydra.Serialization.noSep(java.util.Arrays.asList(
            hydra.Serialization.cst("{"),
            hydra.Serialization.commaSep(
              hydra.Serialization.inlineStyle(),
              hydra.lib.lists.Map.apply(
                hydra.java.Serde::writeVariableInitializer,
                firstGroup)),
            hydra.Serialization.cst("}"))),
          () -> hydra.Serialization.cst("{}"))),
        hydra.lib.lists.MaybeHead.apply(groups)));
  }

  static hydra.ast.Expr writeArrayType(hydra.java.syntax.ArrayType at) {
    hydra.java.syntax.Dims dims = (at).dims;
    hydra.java.syntax.ArrayType_Variant variant = (at).variant;
    hydra.ast.Expr varExpr = (variant).accept(new hydra.java.syntax.ArrayType_Variant.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ArrayType_Variant.Primitive pt) {
        return hydra.java.Serde.writePrimitiveTypeWithAnnotations((pt).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ArrayType_Variant.ClassOrInterface cit) {
        return hydra.java.Serde.writeClassOrInterfaceType((cit).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ArrayType_Variant.Variable tv) {
        return hydra.java.Serde.writeTypeVariable((tv).value);
      }
    });
    return hydra.Serialization.noSep(java.util.Arrays.asList(
      varExpr,
      hydra.java.Serde.writeDims(dims)));
  }

  static <T0> hydra.ast.Expr writeAssertStatement(T0 ignored) {
    return hydra.Serialization.cst("STUB:AssertStatement");
  }

  static hydra.ast.Expr writeAssignment(hydra.java.syntax.Assignment a) {
    hydra.java.syntax.AssignmentOperator op = (a).op;
    String ctop = (op).accept(new hydra.java.syntax.AssignmentOperator.PartialVisitor<>() {
      @Override
      public String visit(hydra.java.syntax.AssignmentOperator.Simple ignored) {
        return "=";
      }

      @Override
      public String visit(hydra.java.syntax.AssignmentOperator.Times ignored) {
        return "*=";
      }

      @Override
      public String visit(hydra.java.syntax.AssignmentOperator.Div ignored) {
        return "/=";
      }

      @Override
      public String visit(hydra.java.syntax.AssignmentOperator.Mod ignored) {
        return "%=";
      }

      @Override
      public String visit(hydra.java.syntax.AssignmentOperator.Plus ignored) {
        return "+=";
      }

      @Override
      public String visit(hydra.java.syntax.AssignmentOperator.Minus ignored) {
        return "-=";
      }

      @Override
      public String visit(hydra.java.syntax.AssignmentOperator.ShiftLeft ignored) {
        return "<<=";
      }

      @Override
      public String visit(hydra.java.syntax.AssignmentOperator.ShiftRight ignored) {
        return ">>=";
      }

      @Override
      public String visit(hydra.java.syntax.AssignmentOperator.ShiftRightZeroFill ignored) {
        return ">>>=";
      }

      @Override
      public String visit(hydra.java.syntax.AssignmentOperator.And ignored) {
        return "&=";
      }

      @Override
      public String visit(hydra.java.syntax.AssignmentOperator.Xor ignored) {
        return "^=";
      }

      @Override
      public String visit(hydra.java.syntax.AssignmentOperator.Or ignored) {
        return "|=";
      }
    });
    hydra.java.syntax.LeftHandSide lhs = (a).lhs;
    hydra.java.syntax.Expression rhs = (a).expression;
    return hydra.Serialization.infixWs(
      ctop,
      hydra.java.Serde.writeLeftHandSide(lhs),
      hydra.java.Serde.writeExpression(rhs));
  }

  static hydra.ast.Expr writeAssignmentExpression(hydra.java.syntax.AssignmentExpression e) {
    return (e).accept(new hydra.java.syntax.AssignmentExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.AssignmentExpression.Conditional c) {
        return hydra.java.Serde.writeConditionalExpression((c).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.AssignmentExpression.Assignment a) {
        return hydra.java.Serde.writeAssignment((a).value);
      }
    });
  }

  static hydra.ast.Expr writeBlock(hydra.java.syntax.Block b) {
    return hydra.Serialization.curlyBlock(
      hydra.Serialization.fullBlockStyle(),
      hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
        hydra.java.Serde::writeBlockStatement,
        (b).value)));
  }

  static hydra.ast.Expr writeBlockStatement(hydra.java.syntax.BlockStatement s) {
    return (s).accept(new hydra.java.syntax.BlockStatement.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.BlockStatement.LocalVariableDeclaration d) {
        return hydra.java.Serde.writeLocalVariableDeclarationStatement((d).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.BlockStatement.Class_ cd) {
        return hydra.java.Serde.writeClassDeclaration((cd).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.BlockStatement.Statement s2) {
        return hydra.java.Serde.writeStatement((s2).value);
      }
    });
  }

  static hydra.ast.Expr writeBreakStatement(hydra.java.syntax.BreakStatement bs) {
    hydra.util.Maybe<hydra.java.syntax.Identifier> mlabel = (bs).value;
    return hydra.Serialization.withSemi(hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.util.Maybe.just(hydra.Serialization.cst("break")),
      hydra.lib.maybes.Map.apply(
        hydra.java.Serde::writeIdentifier,
        mlabel)))));
  }

  static hydra.ast.Expr writeCastExpression(hydra.java.syntax.CastExpression e) {
    return (e).accept(new hydra.java.syntax.CastExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.CastExpression.Primitive p) {
        return hydra.java.Serde.writeCastExpression_Primitive((p).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.CastExpression.NotPlusMinus npm) {
        return hydra.java.Serde.writeCastExpression_NotPlusMinus((npm).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.CastExpression.Lambda l) {
        return hydra.java.Serde.writeCastExpression_Lambda((l).value);
      }
    });
  }

  static <T0> hydra.ast.Expr writeCastExpression_Lambda(T0 ignored) {
    return hydra.Serialization.cst("STUB:CastExpression_Lambda");
  }

  static hydra.ast.Expr writeCastExpression_NotPlusMinus(hydra.java.syntax.CastExpression_NotPlusMinus npm) {
    hydra.java.syntax.UnaryExpression ex = (npm).expression;
    hydra.java.syntax.CastExpression_RefAndBounds rb = (npm).refAndBounds;
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.java.Serde.writeCastExpression_RefAndBounds(rb),
      hydra.java.Serde.writeUnaryExpression(ex)));
  }

  static hydra.ast.Expr writeCastExpression_Primitive(hydra.java.syntax.CastExpression_Primitive cp) {
    hydra.java.syntax.UnaryExpression ex = (cp).expression;
    hydra.java.syntax.PrimitiveTypeWithAnnotations pt = (cp).type;
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.parenList(
        false,
        java.util.Arrays.asList(hydra.java.Serde.writePrimitiveTypeWithAnnotations(pt))),
      hydra.java.Serde.writeUnaryExpression(ex)));
  }

  static hydra.ast.Expr writeCastExpression_RefAndBounds(hydra.java.syntax.CastExpression_RefAndBounds rab) {
    java.util.List<hydra.java.syntax.AdditionalBound> adds = (rab).bounds;
    hydra.java.syntax.ReferenceType rt = (rab).type;
    return hydra.Serialization.parenList(
      false,
      java.util.Arrays.asList(hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
        hydra.util.Maybe.just(hydra.java.Serde.writeReferenceType(rt)),
        hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(adds),
          () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
          () -> hydra.util.Maybe.just(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
            hydra.java.Serde::writeAdditionalBound,
            adds)))))))));
  }

  static hydra.ast.Expr writeClassBody(hydra.java.syntax.ClassBody cb) {
    return hydra.Serialization.curlyBlock(
      hydra.Serialization.fullBlockStyle(),
      hydra.Serialization.doubleNewlineSep(hydra.lib.lists.Map.apply(
        hydra.java.Serde::writeClassBodyDeclarationWithComments,
        (cb).value)));
  }

  static hydra.ast.Expr writeClassBodyDeclaration(hydra.java.syntax.ClassBodyDeclaration d) {
    return (d).accept(new hydra.java.syntax.ClassBodyDeclaration.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassBodyDeclaration.ClassMember d2) {
        return hydra.java.Serde.writeClassMemberDeclaration((d2).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassBodyDeclaration.InstanceInitializer i) {
        return hydra.java.Serde.writeInstanceInitializer((i).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassBodyDeclaration.StaticInitializer i) {
        return hydra.java.Serde.writeStaticInitializer((i).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassBodyDeclaration.ConstructorDeclaration d2) {
        return hydra.java.Serde.writeConstructorDeclaration((d2).value);
      }
    });
  }

  static hydra.ast.Expr writeClassBodyDeclarationWithComments(hydra.java.syntax.ClassBodyDeclarationWithComments cbdwc) {
    hydra.java.syntax.ClassBodyDeclaration d = (cbdwc).value;
    hydra.util.Maybe<String> mc = (cbdwc).comments;
    return hydra.java.Serde.withComments(
      mc,
      hydra.java.Serde.writeClassBodyDeclaration(d));
  }

  static hydra.ast.Expr writeClassDeclaration(hydra.java.syntax.ClassDeclaration d) {
    return (d).accept(new hydra.java.syntax.ClassDeclaration.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassDeclaration.Normal nd) {
        return hydra.java.Serde.writeNormalClassDeclaration((nd).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassDeclaration.Enum_ ed) {
        return hydra.java.Serde.writeEnumDeclaration((ed).value);
      }
    });
  }

  static hydra.ast.Expr writeClassInstanceCreationExpression(hydra.java.syntax.ClassInstanceCreationExpression cice) {
    hydra.java.syntax.UnqualifiedClassInstanceCreationExpression e = (cice).expression;
    hydra.util.Maybe<hydra.java.syntax.ClassInstanceCreationExpression_Qualifier> mqual = (cice).qualifier;
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.java.Serde.writeUnqualifiedClassInstanceCreationExpression(e),
      (java.util.function.Function<hydra.java.syntax.ClassInstanceCreationExpression_Qualifier, hydra.ast.Expr>) (q -> hydra.Serialization.dotSep(java.util.Arrays.asList(
        hydra.java.Serde.writeClassInstanceCreationExpression_Qualifier(q),
        hydra.java.Serde.writeUnqualifiedClassInstanceCreationExpression(e)))),
      mqual);
  }

  static hydra.ast.Expr writeClassInstanceCreationExpression_Qualifier(hydra.java.syntax.ClassInstanceCreationExpression_Qualifier q) {
    return (q).accept(new hydra.java.syntax.ClassInstanceCreationExpression_Qualifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassInstanceCreationExpression_Qualifier.Expression en) {
        return hydra.java.Serde.writeExpressionName((en).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassInstanceCreationExpression_Qualifier.Primary p) {
        return hydra.java.Serde.writePrimary((p).value);
      }
    });
  }

  static <T0> hydra.ast.Expr writeClassLiteral(T0 ignored) {
    return hydra.Serialization.cst("STUB:ClassLiteral");
  }

  static hydra.ast.Expr writeClassMemberDeclaration(hydra.java.syntax.ClassMemberDeclaration d) {
    return (d).accept(new hydra.java.syntax.ClassMemberDeclaration.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassMemberDeclaration.Field fd) {
        return hydra.java.Serde.writeFieldDeclaration((fd).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassMemberDeclaration.Method md) {
        return hydra.java.Serde.writeMethodDeclaration((md).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassMemberDeclaration.Class_ cd) {
        return hydra.java.Serde.writeClassDeclaration((cd).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassMemberDeclaration.Interface id) {
        return hydra.java.Serde.writeInterfaceDeclaration((id).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassMemberDeclaration.None ignored) {
        return hydra.Serialization.cst(";");
      }
    });
  }

  static hydra.ast.Expr writeClassModifier(hydra.java.syntax.ClassModifier m) {
    return (m).accept(new hydra.java.syntax.ClassModifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassModifier.Annotation ann) {
        return hydra.java.Serde.writeAnnotation((ann).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassModifier.Public ignored) {
        return hydra.Serialization.cst("public");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassModifier.Protected ignored) {
        return hydra.Serialization.cst("protected");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassModifier.Private ignored) {
        return hydra.Serialization.cst("private");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassModifier.Abstract ignored) {
        return hydra.Serialization.cst("abstract");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassModifier.Static ignored) {
        return hydra.Serialization.cst("static");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassModifier.Final ignored) {
        return hydra.Serialization.cst("final");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassModifier.Strictfp ignored) {
        return hydra.Serialization.cst("strictfp");
      }
    });
  }

  static hydra.ast.Expr writeClassOrInterfaceType(hydra.java.syntax.ClassOrInterfaceType cit) {
    return (cit).accept(new hydra.java.syntax.ClassOrInterfaceType.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassOrInterfaceType.Class_ ct) {
        return hydra.java.Serde.writeClassType((ct).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassOrInterfaceType.Interface it) {
        return hydra.java.Serde.writeInterfaceType((it).value);
      }
    });
  }

  static hydra.ast.Expr writeClassOrInterfaceTypeToInstantiate(hydra.java.syntax.ClassOrInterfaceTypeToInstantiate coitti) {
    java.util.List<hydra.java.syntax.AnnotatedIdentifier> ids = (coitti).identifiers;
    hydra.util.Maybe<hydra.java.syntax.TypeArgumentsOrDiamond> margs = (coitti).typeArguments;
    return hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.util.Maybe.just(hydra.Serialization.dotSep(hydra.lib.lists.Map.apply(
        hydra.java.Serde::writeAnnotatedIdentifier,
        ids))),
      hydra.lib.maybes.Map.apply(
        hydra.java.Serde::writeTypeArgumentsOrDiamond,
        margs))));
  }

  static hydra.ast.Expr writeClassType(hydra.java.syntax.ClassType ct) {
    java.util.List<hydra.java.syntax.Annotation> anns = (ct).annotations;
    java.util.List<hydra.java.syntax.TypeArgument> args = (ct).arguments;
    hydra.java.syntax.TypeIdentifier id = (ct).identifier;
    hydra.java.syntax.ClassTypeQualifier qual = (ct).qualifier;
    hydra.ast.Expr qualifiedId = (qual).accept(new hydra.java.syntax.ClassTypeQualifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassTypeQualifier.None ignored) {
        return hydra.java.Serde.writeTypeIdentifier(id);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassTypeQualifier.Package_ pkg) {
        return hydra.Serialization.dotSep(java.util.Arrays.asList(
          hydra.java.Serde.writePackageName((pkg).value),
          hydra.java.Serde.writeTypeIdentifier(id)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ClassTypeQualifier.Parent cit) {
        return hydra.Serialization.dotSep(java.util.Arrays.asList(
          hydra.java.Serde.writeClassOrInterfaceType((cit).value),
          hydra.java.Serde.writeTypeIdentifier(id)));
      }
    });
    return hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.util.Maybe.just(hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
        hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(anns),
          () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
          () -> hydra.util.Maybe.just(hydra.Serialization.commaSep(
            hydra.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.java.Serde::writeAnnotation,
              anns)))),
        hydra.util.Maybe.just(qualifiedId))))),
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(args),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.angleBracesList(
          hydra.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.java.Serde::writeTypeArgument,
            args)))))));
  }

  static hydra.ast.Expr writeCompilationUnit(hydra.java.syntax.CompilationUnit u) {
    return (u).accept(new hydra.java.syntax.CompilationUnit.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.CompilationUnit.Ordinary ocu) {
        java.util.List<hydra.java.syntax.ImportDeclaration> imports = (ocu).value.imports;
        hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> importsSec = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(imports),
          () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
          () -> hydra.util.Maybe.just(hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
            hydra.java.Serde::writeImportDeclaration,
            imports)))));
        hydra.util.Maybe<hydra.java.syntax.PackageDeclaration> mpkg = (ocu).value.package_;
        hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> pkgSec = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
          hydra.java.Serde::writePackageDeclaration,
          mpkg));
        java.util.List<hydra.java.syntax.TypeDeclarationWithComments> types = (ocu).value.types;
        hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> typesSec = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(types),
          () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
          () -> hydra.util.Maybe.just(hydra.Serialization.doubleNewlineSep(hydra.lib.lists.Map.apply(
            hydra.java.Serde::writeTypeDeclarationWithComments,
            types)))));
        hydra.util.Maybe<hydra.ast.Expr> warning = hydra.util.Maybe.just(hydra.java.Serde.singleLineComment(hydra.Constants.warningAutoGeneratedFile()));
        return hydra.Serialization.doubleNewlineSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
          warning,
          pkgSec.get(),
          importsSec.get(),
          typesSec.get())));
      }
    });
  }

  static hydra.ast.Expr writeConditionalAndExpression(hydra.java.syntax.ConditionalAndExpression cae) {
    return hydra.Serialization.infixWsList(
      "&&",
      hydra.lib.lists.Map.apply(
        hydra.java.Serde::writeInclusiveOrExpression,
        (cae).value));
  }

  static hydra.ast.Expr writeConditionalExpression(hydra.java.syntax.ConditionalExpression c) {
    return (c).accept(new hydra.java.syntax.ConditionalExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ConditionalExpression.Simple co) {
        return hydra.java.Serde.writeConditionalOrExpression((co).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ConditionalExpression.TernaryCond tc) {
        return hydra.java.Serde.writeConditionalExpression_TernaryCond((tc).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ConditionalExpression.TernaryLambda tl) {
        return hydra.java.Serde.writeConditionalExpression_TernaryLambda((tl).value);
      }
    });
  }

  static <T0> hydra.ast.Expr writeConditionalExpression_TernaryCond(T0 ignored) {
    return hydra.Serialization.cst("STUB:ConditionalExpression_TernaryCond");
  }

  static <T0> hydra.ast.Expr writeConditionalExpression_TernaryLambda(T0 ignored) {
    return hydra.Serialization.cst("STUB:ConditionalExpression_TernaryLambda");
  }

  static hydra.ast.Expr writeConditionalOrExpression(hydra.java.syntax.ConditionalOrExpression coe) {
    return hydra.Serialization.infixWsList(
      "||",
      hydra.lib.lists.Map.apply(
        hydra.java.Serde::writeConditionalAndExpression,
        (coe).value));
  }

  static hydra.ast.Expr writeConstantDeclaration(hydra.java.syntax.ConstantDeclaration cd) {
    java.util.List<hydra.java.syntax.ConstantModifier> mods = (cd).modifiers;
    hydra.java.syntax.UnannType typ = (cd).type;
    java.util.List<hydra.java.syntax.VariableDeclarator> vars = (cd).variables;
    return hydra.Serialization.withSemi(hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          p0 -> hydra.java.Serde.<hydra.java.syntax.ConstantModifier>writeConstantModifier(p0),
          mods)))),
      hydra.util.Maybe.just(hydra.java.Serde.writeUnannType(typ)),
      hydra.util.Maybe.just(hydra.Serialization.commaSep(
        hydra.Serialization.inlineStyle(),
        hydra.lib.lists.Map.apply(
          hydra.java.Serde::writeVariableDeclarator,
          vars)))))));
  }

  static <T0> hydra.ast.Expr writeConstantModifier(T0 ignored) {
    return hydra.Serialization.cst("STUB:ConstantModifier");
  }

  static hydra.ast.Expr writeConstructorBody(hydra.java.syntax.ConstructorBody cb) {
    hydra.util.Maybe<hydra.java.syntax.ExplicitConstructorInvocation> minvoc = (cb).invocation;
    java.util.List<hydra.java.syntax.BlockStatement> stmts = (cb).statements;
    return hydra.Serialization.curlyBlock(
      hydra.Serialization.fullBlockStyle(),
      hydra.Serialization.doubleNewlineSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
        hydra.lib.maybes.Map.apply(
          p0 -> hydra.java.Serde.<hydra.java.syntax.ExplicitConstructorInvocation>writeExplicitConstructorInvocation(p0),
          minvoc),
        hydra.util.Maybe.just(hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
          hydra.java.Serde::writeBlockStatement,
          stmts)))))));
  }

  static hydra.ast.Expr writeConstructorDeclaration(hydra.java.syntax.ConstructorDeclaration cd) {
    hydra.java.syntax.ConstructorBody body = (cd).body;
    hydra.java.syntax.ConstructorDeclarator cons = (cd).constructor;
    java.util.List<hydra.java.syntax.ConstructorModifier> mods = (cd).modifiers;
    hydra.util.Maybe<hydra.java.syntax.Throws> mthrows = (cd).throws_;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.java.Serde::writeConstructorModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.java.Serde.writeConstructorDeclarator(cons)),
      hydra.lib.maybes.Map.apply(
        p0 -> hydra.java.Serde.<hydra.java.syntax.Throws>writeThrows(p0),
        mthrows),
      hydra.util.Maybe.just(hydra.java.Serde.writeConstructorBody(body)))));
  }

  static hydra.ast.Expr writeConstructorDeclarator(hydra.java.syntax.ConstructorDeclarator cd) {
    java.util.List<hydra.java.syntax.FormalParameter> fparams = (cd).formalParameters;
    hydra.java.syntax.SimpleTypeName name = (cd).name;
    java.util.List<hydra.java.syntax.TypeParameter> tparams = (cd).parameters;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(tparams),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.angleBracesList(
          hydra.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.java.Serde::writeTypeParameter,
            tparams)))),
      hydra.util.Maybe.just(hydra.java.Serde.writeSimpleTypeName(name)),
      hydra.util.Maybe.just(hydra.Serialization.parenList(
        false,
        hydra.lib.lists.Map.apply(
          hydra.java.Serde::writeFormalParameter,
          fparams))))));
  }

  static hydra.ast.Expr writeConstructorModifier(hydra.java.syntax.ConstructorModifier m) {
    return (m).accept(new hydra.java.syntax.ConstructorModifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ConstructorModifier.Annotation ann) {
        return hydra.java.Serde.writeAnnotation((ann).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ConstructorModifier.Public ignored) {
        return hydra.Serialization.cst("public");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ConstructorModifier.Protected ignored) {
        return hydra.Serialization.cst("protected");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ConstructorModifier.Private ignored) {
        return hydra.Serialization.cst("private");
      }
    });
  }

  static hydra.ast.Expr writeContinueStatement(hydra.java.syntax.ContinueStatement cs) {
    hydra.util.Maybe<hydra.java.syntax.Identifier> mlabel = (cs).value;
    return hydra.Serialization.withSemi(hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.util.Maybe.just(hydra.Serialization.cst("continue")),
      hydra.lib.maybes.Map.apply(
        hydra.java.Serde::writeIdentifier,
        mlabel)))));
  }

  static hydra.ast.Expr writeDims(hydra.java.syntax.Dims d) {
    return hydra.Serialization.noSep(hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<hydra.java.syntax.Annotation>, hydra.ast.Expr>) (ignored -> hydra.Serialization.cst("[]")),
      (d).value));
  }

  static <T0> hydra.ast.Expr writeDoStatement(T0 ignored) {
    return hydra.Serialization.cst("STUB:DoStatement");
  }

  static hydra.ast.Expr writeElementValue(hydra.java.syntax.ElementValue ev) {
    return (ev).accept(new hydra.java.syntax.ElementValue.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ElementValue.ConditionalExpression c) {
        return hydra.java.Serde.writeConditionalExpression((c).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ElementValue.ElementValueArrayInitializer evai) {
        return hydra.Serialization.commaSep(
          hydra.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.java.Serde::writeElementValue,
            (evai).value.value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ElementValue.Annotation ann) {
        return hydra.java.Serde.writeAnnotation((ann).value);
      }
    });
  }

  static hydra.ast.Expr writeElementValuePair(hydra.java.syntax.ElementValuePair evp) {
    hydra.java.syntax.Identifier k = (evp).key;
    hydra.java.syntax.ElementValue v = (evp).value;
    return hydra.Serialization.infixWs(
      "=",
      hydra.java.Serde.writeIdentifier(k),
      hydra.java.Serde.writeElementValue(v));
  }

  static <T0> hydra.ast.Expr writeEnumDeclaration(T0 ignored) {
    return hydra.Serialization.cst("STUB:EnumDeclaration");
  }

  static hydra.ast.Expr writeEqualityExpression(hydra.java.syntax.EqualityExpression e) {
    return (e).accept(new hydra.java.syntax.EqualityExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.EqualityExpression.Unary r) {
        return hydra.java.Serde.writeRelationalExpression((r).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.EqualityExpression.Equal b) {
        return hydra.Serialization.infixWs(
          "==",
          hydra.java.Serde.writeEqualityExpression((b).value.lhs),
          hydra.java.Serde.writeRelationalExpression((b).value.rhs));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.EqualityExpression.NotEqual b) {
        return hydra.Serialization.infixWs(
          "!=",
          hydra.java.Serde.writeEqualityExpression((b).value.lhs),
          hydra.java.Serde.writeRelationalExpression((b).value.rhs));
      }
    });
  }

  static hydra.ast.Expr writeExclusiveOrExpression(hydra.java.syntax.ExclusiveOrExpression eoe) {
    return hydra.Serialization.infixWsList(
      "^",
      hydra.lib.lists.Map.apply(
        hydra.java.Serde::writeAndExpression,
        (eoe).value));
  }

  static <T0> hydra.ast.Expr writeExplicitConstructorInvocation(T0 ignored) {
    return hydra.Serialization.cst("STUB:ExplicitConstructorInvocation");
  }

  static hydra.ast.Expr writeExpression(hydra.java.syntax.Expression e) {
    return (e).accept(new hydra.java.syntax.Expression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Expression.Lambda l) {
        return hydra.java.Serde.writeLambdaExpression((l).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Expression.Assignment a) {
        return hydra.java.Serde.writeAssignmentExpression((a).value);
      }
    });
  }

  static hydra.ast.Expr writeExpressionName(hydra.java.syntax.ExpressionName en) {
    hydra.java.syntax.Identifier id = (en).identifier;
    hydra.util.Maybe<hydra.java.syntax.AmbiguousName> mqual = (en).qualifier;
    return hydra.Serialization.dotSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.maybes.Map.apply(
        hydra.java.Serde::writeAmbiguousName,
        mqual),
      hydra.util.Maybe.just(hydra.java.Serde.writeIdentifier(id)))));
  }

  static hydra.ast.Expr writeExpressionStatement(hydra.java.syntax.ExpressionStatement es) {
    return hydra.Serialization.withSemi(hydra.java.Serde.writeStatementExpression((es).value));
  }

  static hydra.ast.Expr writeFieldAccess(hydra.java.syntax.FieldAccess fa) {
    hydra.java.syntax.Identifier id = (fa).identifier;
    hydra.java.syntax.FieldAccess_Qualifier qual = (fa).qualifier;
    return (qual).accept(new hydra.java.syntax.FieldAccess_Qualifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.FieldAccess_Qualifier.Primary p) {
        return hydra.Serialization.dotSep(java.util.Arrays.asList(
          hydra.java.Serde.writePrimary((p).value),
          hydra.java.Serde.writeIdentifier(id)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.FieldAccess_Qualifier.Super ignored) {
        return hydra.Serialization.dotSep(java.util.Arrays.asList(
          hydra.Serialization.cst("super"),
          hydra.java.Serde.writeIdentifier(id)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.FieldAccess_Qualifier.Typed tn) {
        return hydra.Serialization.dotSep(java.util.Arrays.asList(
          hydra.java.Serde.writeTypeName((tn).value),
          hydra.Serialization.cst("super"),
          hydra.java.Serde.writeIdentifier(id)));
      }
    });
  }

  static hydra.ast.Expr writeFieldDeclaration(hydra.java.syntax.FieldDeclaration fd) {
    java.util.List<hydra.java.syntax.FieldModifier> mods = (fd).modifiers;
    hydra.java.syntax.UnannType typ = (fd).unannType;
    java.util.List<hydra.java.syntax.VariableDeclarator> vars = (fd).variableDeclarators;
    return hydra.Serialization.withSemi(hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.java.Serde::writeFieldModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.java.Serde.writeUnannType(typ)),
      hydra.util.Maybe.just(hydra.Serialization.commaSep(
        hydra.Serialization.inlineStyle(),
        hydra.lib.lists.Map.apply(
          hydra.java.Serde::writeVariableDeclarator,
          vars)))))));
  }

  static hydra.ast.Expr writeFieldModifier(hydra.java.syntax.FieldModifier m) {
    return (m).accept(new hydra.java.syntax.FieldModifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.FieldModifier.Annotation ann) {
        return hydra.java.Serde.writeAnnotation((ann).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.FieldModifier.Public ignored) {
        return hydra.Serialization.cst("public");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.FieldModifier.Protected ignored) {
        return hydra.Serialization.cst("protected");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.FieldModifier.Private ignored) {
        return hydra.Serialization.cst("private");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.FieldModifier.Static ignored) {
        return hydra.Serialization.cst("static");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.FieldModifier.Final ignored) {
        return hydra.Serialization.cst("final");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.FieldModifier.Transient ignored) {
        return hydra.Serialization.cst("transient");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.FieldModifier.Volatile ignored) {
        return hydra.Serialization.cst("volatile");
      }
    });
  }

  static hydra.ast.Expr writeFloatingPointLiteral(hydra.java.syntax.FloatingPointLiteral fl) {
    return hydra.Serialization.cst(hydra.java.Serde.javaFloatLiteralText(hydra.lib.literals.ShowBigfloat.apply((fl).value)));
  }

  static hydra.ast.Expr writeFloatingPointType(hydra.java.syntax.FloatingPointType ft) {
    return (ft).accept(new hydra.java.syntax.FloatingPointType.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.FloatingPointType.Float_ ignored) {
        return hydra.Serialization.cst("float");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.FloatingPointType.Double_ ignored) {
        return hydra.Serialization.cst("double");
      }
    });
  }

  static <T0> hydra.ast.Expr writeForStatement(T0 ignored) {
    return hydra.Serialization.cst("STUB:ForStatement");
  }

  static hydra.ast.Expr writeFormalParameter(hydra.java.syntax.FormalParameter p) {
    return (p).accept(new hydra.java.syntax.FormalParameter.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.FormalParameter.Simple s) {
        return hydra.java.Serde.writeFormalParameter_Simple((s).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.FormalParameter.VariableArity v) {
        return hydra.java.Serde.writeVariableArityParameter((v).value);
      }
    });
  }

  static hydra.ast.Expr writeFormalParameter_Simple(hydra.java.syntax.FormalParameter_Simple fps) {
    hydra.java.syntax.VariableDeclaratorId id = (fps).id;
    java.util.List<hydra.java.syntax.VariableModifier> mods = (fps).modifiers;
    hydra.java.syntax.UnannType typ = (fps).type;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.java.Serde::writeVariableModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.java.Serde.writeUnannType(typ)),
      hydra.util.Maybe.just(hydra.java.Serde.writeVariableDeclaratorId(id)))));
  }

  static hydra.ast.Expr writeIdentifier(hydra.java.syntax.Identifier id) {
    return hydra.Serialization.cst((id).value);
  }

  static <T0> hydra.ast.Expr writeIfThenElseStatement(T0 ignored) {
    return hydra.Serialization.cst("STUB:IfThenElseStatement");
  }

  static hydra.ast.Expr writeIfThenStatement(hydra.java.syntax.IfThenStatement its) {
    hydra.java.syntax.Expression cond = (its).expression;
    hydra.java.syntax.Statement thn = (its).statement;
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst("if"),
      hydra.Serialization.parenList(
        false,
        java.util.Arrays.asList(hydra.java.Serde.writeExpression(cond))),
      hydra.Serialization.curlyBlock(
        hydra.Serialization.fullBlockStyle(),
        hydra.java.Serde.writeStatement(thn))));
  }

  static hydra.ast.Expr writeImportDeclaration(hydra.java.syntax.ImportDeclaration imp) {
    return (imp).accept(new hydra.java.syntax.ImportDeclaration.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ImportDeclaration.SingleType st) {
        return hydra.Serialization.withSemi(hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("import"),
          hydra.java.Serde.writeTypeName((st).value.value))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ImportDeclaration.TypeImportOnDemand ignored) {
        return hydra.Serialization.cst("STUB:ImportDeclarationTypeImportOnDemand");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ImportDeclaration.SingleStaticImport ignored) {
        return hydra.Serialization.cst("STUB:ImportDeclarationSingleStaticImport");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ImportDeclaration.StaticImportOnDemand ignored) {
        return hydra.Serialization.cst("STUB:ImportDeclarationStaticImportOnDemand");
      }
    });
  }

  static hydra.ast.Expr writeInclusiveOrExpression(hydra.java.syntax.InclusiveOrExpression ioe) {
    return hydra.Serialization.infixWsList(
      "|",
      hydra.lib.lists.Map.apply(
        hydra.java.Serde::writeExclusiveOrExpression,
        (ioe).value));
  }

  static <T0> hydra.ast.Expr writeInstanceInitializer(T0 ignored) {
    return hydra.Serialization.cst("STUB:InstanceInitializer");
  }

  static hydra.ast.Expr writeIntegerLiteral(hydra.java.syntax.IntegerLiteral il) {
    java.math.BigInteger i = (il).value;
    hydra.util.Lazy<String> suffix = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Or.apply(
        hydra.lib.equality.Gt.apply(
          i,
          new java.math.BigInteger("2147483647")),
        hydra.lib.equality.Lt.apply(
          i,
          new java.math.BigInteger("-2147483648"))),
      () -> "L",
      () -> ""));
    return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
      hydra.lib.literals.ShowBigint.apply(i),
      suffix.get()));
  }

  static hydra.ast.Expr writeIntegralType(hydra.java.syntax.IntegralType t) {
    return (t).accept(new hydra.java.syntax.IntegralType.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.IntegralType.Byte_ ignored) {
        return hydra.Serialization.cst("byte");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.IntegralType.Short_ ignored) {
        return hydra.Serialization.cst("short");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.IntegralType.Int ignored) {
        return hydra.Serialization.cst("int");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.IntegralType.Long_ ignored) {
        return hydra.Serialization.cst("long");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.IntegralType.Char ignored) {
        return hydra.Serialization.cst("char");
      }
    });
  }

  static hydra.ast.Expr writeInterfaceBody(hydra.java.syntax.InterfaceBody ib) {
    return hydra.Serialization.curlyBlock(
      hydra.Serialization.fullBlockStyle(),
      hydra.Serialization.doubleNewlineSep(hydra.lib.lists.Map.apply(
        hydra.java.Serde::writeInterfaceMemberDeclaration,
        (ib).value)));
  }

  static hydra.ast.Expr writeInterfaceDeclaration(hydra.java.syntax.InterfaceDeclaration d) {
    return (d).accept(new hydra.java.syntax.InterfaceDeclaration.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceDeclaration.NormalInterface n) {
        return hydra.java.Serde.writeNormalInterfaceDeclaration((n).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceDeclaration.AnnotationType a) {
        return hydra.java.Serde.writeAnnotationTypeDeclaration((a).value);
      }
    });
  }

  static hydra.ast.Expr writeInterfaceMemberDeclaration(hydra.java.syntax.InterfaceMemberDeclaration d) {
    return (d).accept(new hydra.java.syntax.InterfaceMemberDeclaration.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceMemberDeclaration.Constant c) {
        return hydra.java.Serde.writeConstantDeclaration((c).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceMemberDeclaration.InterfaceMethod im) {
        return hydra.java.Serde.writeInterfaceMethodDeclaration((im).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceMemberDeclaration.Class_ cd) {
        return hydra.java.Serde.writeClassDeclaration((cd).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceMemberDeclaration.Interface id) {
        return hydra.java.Serde.writeInterfaceDeclaration((id).value);
      }
    });
  }

  static hydra.ast.Expr writeInterfaceMethodDeclaration(hydra.java.syntax.InterfaceMethodDeclaration imd) {
    hydra.java.syntax.MethodBody body = (imd).body;
    hydra.java.syntax.MethodHeader header = (imd).header;
    java.util.List<hydra.java.syntax.InterfaceMethodModifier> mods = (imd).modifiers;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.java.Serde::writeInterfaceMethodModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.java.Serde.writeMethodHeader(header)),
      hydra.util.Maybe.just(hydra.java.Serde.writeMethodBody(body)))));
  }

  static hydra.ast.Expr writeInterfaceMethodModifier(hydra.java.syntax.InterfaceMethodModifier m) {
    return (m).accept(new hydra.java.syntax.InterfaceMethodModifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceMethodModifier.Annotation a) {
        return hydra.java.Serde.writeAnnotation((a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceMethodModifier.Public ignored) {
        return hydra.Serialization.cst("public");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceMethodModifier.Private ignored) {
        return hydra.Serialization.cst("private");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceMethodModifier.Abstract ignored) {
        return hydra.Serialization.cst("abstract");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceMethodModifier.Default ignored) {
        return hydra.Serialization.cst("default");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceMethodModifier.Static ignored) {
        return hydra.Serialization.cst("static");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceMethodModifier.Strictfp ignored) {
        return hydra.Serialization.cst("strictfp");
      }
    });
  }

  static hydra.ast.Expr writeInterfaceModifier(hydra.java.syntax.InterfaceModifier m) {
    return (m).accept(new hydra.java.syntax.InterfaceModifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceModifier.Annotation a) {
        return hydra.java.Serde.writeAnnotation((a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceModifier.Public ignored) {
        return hydra.Serialization.cst("public");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceModifier.Protected ignored) {
        return hydra.Serialization.cst("protected");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceModifier.Private ignored) {
        return hydra.Serialization.cst("private");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceModifier.Abstract ignored) {
        return hydra.Serialization.cst("abstract");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceModifier.Static ignored) {
        return hydra.Serialization.cst("static");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.InterfaceModifier.Strictfb ignored) {
        return hydra.Serialization.cst("strictfb");
      }
    });
  }

  static hydra.ast.Expr writeInterfaceType(hydra.java.syntax.InterfaceType it) {
    return hydra.java.Serde.writeClassType((it).value);
  }

  static <T0> hydra.ast.Expr writeLabeledStatement(T0 ignored) {
    return hydra.Serialization.cst("STUB:LabeledStatement");
  }

  static hydra.ast.Expr writeLambdaBody(hydra.java.syntax.LambdaBody b) {
    return (b).accept(new hydra.java.syntax.LambdaBody.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.LambdaBody.Expression e) {
        return hydra.java.Serde.writeExpression((e).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.LambdaBody.Block b2) {
        return hydra.java.Serde.writeBlock((b2).value);
      }
    });
  }

  static hydra.ast.Expr writeLambdaExpression(hydra.java.syntax.LambdaExpression le) {
    hydra.java.syntax.LambdaBody body = (le).body;
    hydra.java.syntax.LambdaParameters params = (le).parameters;
    return hydra.Serialization.infixWs(
      "->",
      hydra.java.Serde.writeLambdaParameters(params),
      hydra.java.Serde.writeLambdaBody(body));
  }

  static hydra.ast.Expr writeLambdaParameters(hydra.java.syntax.LambdaParameters p) {
    return (p).accept(new hydra.java.syntax.LambdaParameters.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.LambdaParameters.Tuple l) {
        return hydra.Serialization.parenList(
          false,
          hydra.lib.lists.Map.apply(
            hydra.java.Serde::writeLambdaParameters,
            (l).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.LambdaParameters.Single id) {
        return hydra.java.Serde.writeIdentifier((id).value);
      }
    });
  }

  static hydra.ast.Expr writeLeftHandSide(hydra.java.syntax.LeftHandSide lhs) {
    return (lhs).accept(new hydra.java.syntax.LeftHandSide.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.LeftHandSide.ExpressionName en) {
        return hydra.java.Serde.writeExpressionName((en).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.LeftHandSide.FieldAccess fa) {
        return hydra.java.Serde.writeFieldAccess((fa).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.LeftHandSide.ArrayAccess aa) {
        return hydra.java.Serde.writeArrayAccess((aa).value);
      }
    });
  }

  static hydra.ast.Expr writeLiteral(hydra.java.syntax.Literal l) {
    return (l).accept(new hydra.java.syntax.Literal.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Literal.Null ignored) {
        return hydra.Serialization.cst("null");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Literal.Integer_ il) {
        return hydra.java.Serde.writeIntegerLiteral((il).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Literal.FloatingPoint fl) {
        return hydra.java.Serde.writeFloatingPointLiteral((fl).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Literal.Boolean_ b) {
        return hydra.Serialization.cst(hydra.lib.logic.IfElse.lazy(
          (b).value,
          () -> "true",
          () -> "false"));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Literal.Character_ c) {
        Integer ci = hydra.lib.literals.BigintToInt32.apply(hydra.lib.literals.Uint16ToBigint.apply((c).value));
        return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
          "'",
          hydra.lib.strings.Cat2.apply(
            hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Equal.apply(
                ci,
                39),
              () -> "\\'",
              () -> hydra.lib.logic.IfElse.lazy(
                hydra.lib.equality.Equal.apply(
                  ci,
                  92),
                () -> "\\\\",
                () -> hydra.lib.logic.IfElse.lazy(
                  hydra.lib.equality.Equal.apply(
                    ci,
                    10),
                  () -> "\\n",
                  () -> hydra.lib.logic.IfElse.lazy(
                    hydra.lib.equality.Equal.apply(
                      ci,
                      13),
                    () -> "\\r",
                    () -> hydra.lib.logic.IfElse.lazy(
                      hydra.lib.equality.Equal.apply(
                        ci,
                        9),
                      () -> "\\t",
                      () -> hydra.lib.logic.IfElse.lazy(
                        hydra.lib.logic.And.apply(
                          hydra.lib.equality.Gte.apply(
                            ci,
                            32),
                          hydra.lib.equality.Lt.apply(
                            ci,
                            127)),
                        () -> hydra.lib.strings.FromList.apply(java.util.Arrays.asList(ci)),
                        () -> hydra.java.Serde.javaUnicodeEscape(ci))))))),
            "'")));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Literal.String_ sl) {
        return hydra.java.Serde.writeStringLiteral((sl).value);
      }
    });
  }

  static hydra.ast.Expr writeLocalName(hydra.java.syntax.LocalVariableType t) {
    return (t).accept(new hydra.java.syntax.LocalVariableType.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.LocalVariableType.Type ut) {
        return hydra.java.Serde.writeUnannType((ut).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.LocalVariableType.Var ignored) {
        return hydra.Serialization.cst("var");
      }
    });
  }

  static hydra.ast.Expr writeLocalVariableDeclaration(hydra.java.syntax.LocalVariableDeclaration lvd) {
    java.util.List<hydra.java.syntax.VariableDeclarator> decls = (lvd).declarators;
    java.util.List<hydra.java.syntax.VariableModifier> mods = (lvd).modifiers;
    hydra.java.syntax.LocalVariableType t = (lvd).type;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.java.Serde::writeVariableModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.java.Serde.writeLocalName(t)),
      hydra.util.Maybe.just(hydra.Serialization.commaSep(
        hydra.Serialization.inlineStyle(),
        hydra.lib.lists.Map.apply(
          hydra.java.Serde::writeVariableDeclarator,
          decls))))));
  }

  static hydra.ast.Expr writeLocalVariableDeclarationStatement(hydra.java.syntax.LocalVariableDeclarationStatement lvds) {
    return hydra.Serialization.withSemi(hydra.java.Serde.writeLocalVariableDeclaration((lvds).value));
  }

  static hydra.ast.Expr writeMarkerAnnotation(hydra.java.syntax.MarkerAnnotation ma) {
    return hydra.Serialization.prefix(
      "@",
      hydra.java.Serde.writeTypeName((ma).value));
  }

  static hydra.ast.Expr writeMethodBody(hydra.java.syntax.MethodBody b) {
    return (b).accept(new hydra.java.syntax.MethodBody.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.MethodBody.Block block) {
        return hydra.java.Serde.writeBlock((block).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.MethodBody.None ignored) {
        return hydra.Serialization.cst(";");
      }
    });
  }

  static hydra.ast.Expr writeMethodDeclaration(hydra.java.syntax.MethodDeclaration md) {
    java.util.List<hydra.java.syntax.Annotation> anns = (md).annotations;
    hydra.java.syntax.MethodBody body = (md).body;
    hydra.java.syntax.MethodHeader header = (md).header;
    java.util.List<hydra.java.syntax.MethodModifier> mods = (md).modifiers;
    hydra.util.Lazy<hydra.ast.Expr> headerAndBody = new hydra.util.Lazy<>(() -> hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.java.Serde::writeMethodModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.java.Serde.writeMethodHeader(header)),
      hydra.util.Maybe.just(hydra.java.Serde.writeMethodBody(body))))));
    return hydra.Serialization.newlineSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(anns),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
          hydra.java.Serde::writeAnnotation,
          anns)))),
      hydra.util.Maybe.just(headerAndBody.get()))));
  }

  static hydra.ast.Expr writeMethodDeclarator(hydra.java.syntax.MethodDeclarator md) {
    hydra.java.syntax.Identifier id = (md).identifier;
    java.util.List<hydra.java.syntax.FormalParameter> params = (md).formalParameters;
    return hydra.Serialization.noSep(java.util.Arrays.asList(
      hydra.java.Serde.writeIdentifier(id),
      hydra.Serialization.parenList(
        false,
        hydra.lib.lists.Map.apply(
          hydra.java.Serde::writeFormalParameter,
          params))));
  }

  static hydra.ast.Expr writeMethodHeader(hydra.java.syntax.MethodHeader mh) {
    hydra.java.syntax.MethodDeclarator decl = (mh).declarator;
    hydra.util.Maybe<hydra.java.syntax.Throws> mthrows = (mh).throws_;
    java.util.List<hydra.java.syntax.TypeParameter> params = (mh).parameters;
    hydra.java.syntax.Result result = (mh).result;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(params),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.angleBracesList(
          hydra.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.java.Serde::writeTypeParameter,
            params)))),
      hydra.util.Maybe.just(hydra.java.Serde.writeResult(result)),
      hydra.util.Maybe.just(hydra.java.Serde.writeMethodDeclarator(decl)),
      hydra.lib.maybes.Map.apply(
        p0 -> hydra.java.Serde.<hydra.java.syntax.Throws>writeThrows(p0),
        mthrows))));
  }

  static hydra.ast.Expr writeMethodInvocation(hydra.java.syntax.MethodInvocation mi) {
    java.util.List<hydra.java.syntax.Expression> args = (mi).arguments;
    hydra.util.Lazy<hydra.ast.Expr> argSec = new hydra.util.Lazy<>(() -> hydra.Serialization.parenList(
      true,
      hydra.lib.lists.Map.apply(
        hydra.java.Serde::writeExpression,
        args)));
    hydra.java.syntax.MethodInvocation_Header header = (mi).header;
    hydra.util.Lazy<hydra.ast.Expr> headerSec = new hydra.util.Lazy<>(() -> (header).accept(new hydra.java.syntax.MethodInvocation_Header.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.MethodInvocation_Header.Simple mname) {
        return hydra.java.Serde.writeMethodName((mname).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.MethodInvocation_Header.Complex cx) {
        hydra.java.syntax.Identifier cid = (cx).value.identifier;
        hydra.java.syntax.MethodInvocation_Variant cvar = (cx).value.variant;
        java.util.List<hydra.java.syntax.TypeArgument> targs = (cx).value.typeArguments;
        hydra.util.Lazy<hydra.ast.Expr> idSec = new hydra.util.Lazy<>(() -> hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
          hydra.lib.logic.IfElse.lazy(
            hydra.lib.lists.Null.apply(targs),
            () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
            () -> hydra.util.Maybe.just(hydra.Serialization.angleBracesList(
              hydra.Serialization.inlineStyle(),
              hydra.lib.lists.Map.apply(
                hydra.java.Serde::writeTypeArgument,
                targs)))),
          hydra.util.Maybe.just(hydra.java.Serde.writeIdentifier(cid))))));
        return (cvar).accept(new hydra.java.syntax.MethodInvocation_Variant.PartialVisitor<>() {
          @Override
          public hydra.ast.Expr visit(hydra.java.syntax.MethodInvocation_Variant.Type tname) {
            return hydra.Serialization.dotSep(java.util.Arrays.asList(
              hydra.java.Serde.writeTypeName((tname).value),
              idSec.get()));
          }

          @Override
          public hydra.ast.Expr visit(hydra.java.syntax.MethodInvocation_Variant.Expression en) {
            return hydra.Serialization.dotSep(java.util.Arrays.asList(
              hydra.java.Serde.writeExpressionName((en).value),
              idSec.get()));
          }

          @Override
          public hydra.ast.Expr visit(hydra.java.syntax.MethodInvocation_Variant.Primary p) {
            return hydra.Serialization.dotSep(java.util.Arrays.asList(
              hydra.java.Serde.writePrimary((p).value),
              idSec.get()));
          }

          @Override
          public hydra.ast.Expr visit(hydra.java.syntax.MethodInvocation_Variant.Super ignored) {
            return hydra.Serialization.dotSep(java.util.Arrays.asList(
              hydra.Serialization.cst("super"),
              idSec.get()));
          }

          @Override
          public hydra.ast.Expr visit(hydra.java.syntax.MethodInvocation_Variant.TypeSuper tname) {
            return hydra.Serialization.dotSep(java.util.Arrays.asList(
              hydra.java.Serde.writeTypeName((tname).value),
              hydra.Serialization.cst("super"),
              idSec.get()));
          }
        });
      }
    }));
    return hydra.Serialization.noSep(java.util.Arrays.asList(
      headerSec.get(),
      argSec.get()));
  }

  static hydra.ast.Expr writeMethodModifier(hydra.java.syntax.MethodModifier m) {
    return (m).accept(new hydra.java.syntax.MethodModifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.MethodModifier.Annotation ann) {
        return hydra.java.Serde.writeAnnotation((ann).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.MethodModifier.Public ignored) {
        return hydra.Serialization.cst("public");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.MethodModifier.Protected ignored) {
        return hydra.Serialization.cst("protected");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.MethodModifier.Private ignored) {
        return hydra.Serialization.cst("private");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.MethodModifier.Abstract ignored) {
        return hydra.Serialization.cst("abstract");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.MethodModifier.Final ignored) {
        return hydra.Serialization.cst("final");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.MethodModifier.Synchronized ignored) {
        return hydra.Serialization.cst("synchronized");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.MethodModifier.Native ignored) {
        return hydra.Serialization.cst("native");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.MethodModifier.Strictfb ignored) {
        return hydra.Serialization.cst("strictfb");
      }
    });
  }

  static hydra.ast.Expr writeMethodName(hydra.java.syntax.MethodName mn) {
    return hydra.java.Serde.writeIdentifier((mn).value);
  }

  static <T0> hydra.ast.Expr writeMethodReference(T0 ignored) {
    return hydra.Serialization.cst("STUB:MethodReference");
  }

  static hydra.ast.Expr writeMultiplicativeExpression(hydra.java.syntax.MultiplicativeExpression e) {
    return (e).accept(new hydra.java.syntax.MultiplicativeExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.MultiplicativeExpression.Unary u) {
        return hydra.java.Serde.writeUnaryExpression((u).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.MultiplicativeExpression.Times b) {
        return hydra.Serialization.infixWs(
          "*",
          hydra.java.Serde.writeMultiplicativeExpression((b).value.lhs),
          hydra.java.Serde.writeUnaryExpression((b).value.rhs));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.MultiplicativeExpression.Divide b) {
        return hydra.Serialization.infixWs(
          "/",
          hydra.java.Serde.writeMultiplicativeExpression((b).value.lhs),
          hydra.java.Serde.writeUnaryExpression((b).value.rhs));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.MultiplicativeExpression.Mod b) {
        return hydra.Serialization.infixWs(
          "%",
          hydra.java.Serde.writeMultiplicativeExpression((b).value.lhs),
          hydra.java.Serde.writeUnaryExpression((b).value.rhs));
      }
    });
  }

  static hydra.ast.Expr writeNormalAnnotation(hydra.java.syntax.NormalAnnotation na) {
    java.util.List<hydra.java.syntax.ElementValuePair> pairs = (na).pairs;
    hydra.java.syntax.TypeName tname = (na).typeName;
    return hydra.Serialization.prefix(
      "@",
      hydra.Serialization.noSep(java.util.Arrays.asList(
        hydra.java.Serde.writeTypeName(tname),
        hydra.Serialization.commaSep(
          hydra.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.java.Serde::writeElementValuePair,
            pairs)))));
  }

  static hydra.ast.Expr writeNormalClassDeclaration(hydra.java.syntax.NormalClassDeclaration ncd) {
    hydra.java.syntax.ClassBody body = (ncd).body;
    hydra.java.syntax.TypeIdentifier id = (ncd).identifier;
    java.util.List<hydra.java.syntax.ClassModifier> mods = (ncd).modifiers;
    hydra.util.Maybe<hydra.java.syntax.ClassType> msuperc = (ncd).extends_;
    java.util.List<hydra.java.syntax.InterfaceType> superi = (ncd).implements_;
    java.util.List<hydra.java.syntax.TypeParameter> tparams = (ncd).parameters;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.java.Serde::writeClassModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.Serialization.cst("class")),
      hydra.util.Maybe.just(hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
        hydra.util.Maybe.just(hydra.java.Serde.writeTypeIdentifier(id)),
        hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(tparams),
          () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
          () -> hydra.util.Maybe.just(hydra.Serialization.angleBracesList(
            hydra.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.java.Serde::writeTypeParameter,
              tparams)))))))),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.java.syntax.ClassType, hydra.ast.Expr>) (c -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("extends"),
          hydra.java.Serde.writeClassType(c)))),
        msuperc),
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(superi),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("implements"),
          hydra.Serialization.commaSep(
            hydra.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.java.Serde::writeInterfaceType,
              superi)))))),
      hydra.util.Maybe.just(hydra.java.Serde.writeClassBody(body)))));
  }

  static hydra.ast.Expr writeNormalInterfaceDeclaration(hydra.java.syntax.NormalInterfaceDeclaration nid) {
    hydra.java.syntax.InterfaceBody body = (nid).body;
    java.util.List<hydra.java.syntax.InterfaceType> extends_ = (nid).extends_;
    hydra.java.syntax.TypeIdentifier id = (nid).identifier;
    java.util.List<hydra.java.syntax.InterfaceModifier> mods = (nid).modifiers;
    java.util.List<hydra.java.syntax.TypeParameter> tparams = (nid).parameters;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.java.Serde::writeInterfaceModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.Serialization.cst("interface")),
      hydra.util.Maybe.just(hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
        hydra.util.Maybe.just(hydra.java.Serde.writeTypeIdentifier(id)),
        hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(tparams),
          () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
          () -> hydra.util.Maybe.just(hydra.Serialization.angleBracesList(
            hydra.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.java.Serde::writeTypeParameter,
              tparams)))))))),
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(extends_),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("extends"),
          hydra.Serialization.commaSep(
            hydra.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.java.Serde::writeInterfaceType,
              extends_)))))),
      hydra.util.Maybe.just(hydra.java.Serde.writeInterfaceBody(body)))));
  }

  static hydra.ast.Expr writeNumericType(hydra.java.syntax.NumericType nt) {
    return (nt).accept(new hydra.java.syntax.NumericType.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.NumericType.Integral it) {
        return hydra.java.Serde.writeIntegralType((it).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.NumericType.FloatingPoint ft) {
        return hydra.java.Serde.writeFloatingPointType((ft).value);
      }
    });
  }

  static hydra.ast.Expr writePackageDeclaration(hydra.java.syntax.PackageDeclaration pd) {
    java.util.List<hydra.java.syntax.Identifier> ids = (pd).identifiers;
    java.util.List<hydra.java.syntax.PackageModifier> mods = (pd).modifiers;
    return hydra.Serialization.withSemi(hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.java.Serde::writePackageModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.Serialization.spaceSep(java.util.Arrays.asList(
        hydra.Serialization.cst("package"),
        hydra.Serialization.cst(hydra.lib.strings.Intercalate.apply(
          ".",
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.java.syntax.Identifier, String>) (id -> (id).value),
            ids))))))))));
  }

  static hydra.ast.Expr writePackageModifier(hydra.java.syntax.PackageModifier pm) {
    return hydra.java.Serde.writeAnnotation((pm).value);
  }

  static hydra.ast.Expr writePackageName(hydra.java.syntax.PackageName pn) {
    return hydra.Serialization.dotSep(hydra.lib.lists.Map.apply(
      hydra.java.Serde::writeIdentifier,
      (pn).value));
  }

  static hydra.ast.Expr writePackageOrTypeName(hydra.java.syntax.PackageOrTypeName potn) {
    return hydra.Serialization.dotSep(hydra.lib.lists.Map.apply(
      hydra.java.Serde::writeIdentifier,
      (potn).value));
  }

  static <T0> hydra.ast.Expr writePostDecrementExpression(T0 ignored) {
    return hydra.Serialization.cst("STUB:PostDecrementExpression");
  }

  static <T0> hydra.ast.Expr writePostIncrementExpression(T0 ignored) {
    return hydra.Serialization.cst("STUB:PostIncrementExpression");
  }

  static hydra.ast.Expr writePostfixExpression(hydra.java.syntax.PostfixExpression e) {
    return (e).accept(new hydra.java.syntax.PostfixExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.PostfixExpression.Primary p) {
        return hydra.java.Serde.writePrimary((p).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.PostfixExpression.Name en) {
        return hydra.java.Serde.writeExpressionName((en).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.PostfixExpression.PostIncrement pi) {
        return hydra.java.Serde.writePostIncrementExpression((pi).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.PostfixExpression.PostDecrement pd) {
        return hydra.java.Serde.writePostDecrementExpression((pd).value);
      }
    });
  }

  static <T0> hydra.ast.Expr writePreDecrementExpression(T0 ignored) {
    return hydra.Serialization.cst("STUB:PreDecrementExpression");
  }

  static <T0> hydra.ast.Expr writePreIncrementExpression(T0 ignored) {
    return hydra.Serialization.cst("STUB:PreIncrementExpression");
  }

  static hydra.ast.Expr writePrimary(hydra.java.syntax.Primary p) {
    return (p).accept(new hydra.java.syntax.Primary.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Primary.NoNewArray n) {
        return hydra.java.Serde.writePrimaryNoNewArrayExpressionExpression((n).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Primary.ArrayCreation a) {
        return hydra.java.Serde.writeArrayCreationExpression((a).value);
      }
    });
  }

  static hydra.ast.Expr writePrimaryNoNewArrayExpressionExpression(hydra.java.syntax.PrimaryNoNewArrayExpression p) {
    return (p).accept(new hydra.java.syntax.PrimaryNoNewArrayExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.PrimaryNoNewArrayExpression.Literal l) {
        return hydra.java.Serde.writeLiteral((l).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.PrimaryNoNewArrayExpression.ClassLiteral cl) {
        return hydra.java.Serde.writeClassLiteral((cl).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.PrimaryNoNewArrayExpression.This ignored) {
        return hydra.Serialization.cst("this");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.PrimaryNoNewArrayExpression.DotThis n) {
        return hydra.Serialization.dotSep(java.util.Arrays.asList(
          hydra.java.Serde.writeTypeName((n).value),
          hydra.Serialization.cst("this")));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.PrimaryNoNewArrayExpression.Parens e) {
        return hydra.Serialization.parenList(
          false,
          java.util.Arrays.asList(hydra.java.Serde.writeExpression((e).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.PrimaryNoNewArrayExpression.ClassInstance ci) {
        return hydra.java.Serde.writeClassInstanceCreationExpression((ci).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.PrimaryNoNewArrayExpression.FieldAccess fa) {
        return hydra.java.Serde.writeFieldAccess((fa).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.PrimaryNoNewArrayExpression.ArrayAccess aa) {
        return hydra.java.Serde.writeArrayAccess((aa).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.PrimaryNoNewArrayExpression.MethodInvocation mi) {
        return hydra.java.Serde.writeMethodInvocation((mi).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.PrimaryNoNewArrayExpression.MethodReference mr) {
        return hydra.java.Serde.writeMethodReference((mr).value);
      }
    });
  }

  static hydra.ast.Expr writePrimitiveType(hydra.java.syntax.PrimitiveType pt) {
    return (pt).accept(new hydra.java.syntax.PrimitiveType.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.PrimitiveType.Numeric nt) {
        return hydra.java.Serde.writeNumericType((nt).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.PrimitiveType.Boolean_ ignored) {
        return hydra.Serialization.cst("boolean");
      }
    });
  }

  static hydra.ast.Expr writePrimitiveTypeWithAnnotations(hydra.java.syntax.PrimitiveTypeWithAnnotations ptwa) {
    java.util.List<hydra.java.syntax.Annotation> anns = (ptwa).annotations;
    hydra.java.syntax.PrimitiveType pt = (ptwa).type;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(anns),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.java.Serde::writeAnnotation,
          anns)))),
      hydra.util.Maybe.just(hydra.java.Serde.writePrimitiveType(pt)))));
  }

  static <T0> hydra.ast.Expr writeReceiverParameter(T0 ignored) {
    return hydra.Serialization.cst("STUB:ReceiverParameter");
  }

  static hydra.ast.Expr writeReferenceType(hydra.java.syntax.ReferenceType rt) {
    return (rt).accept(new hydra.java.syntax.ReferenceType.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ReferenceType.ClassOrInterface cit) {
        return hydra.java.Serde.writeClassOrInterfaceType((cit).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ReferenceType.Variable v) {
        return hydra.java.Serde.writeTypeVariable((v).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ReferenceType.Array at) {
        return hydra.java.Serde.writeArrayType((at).value);
      }
    });
  }

  static hydra.ast.Expr writeRelationalExpression(hydra.java.syntax.RelationalExpression e) {
    return (e).accept(new hydra.java.syntax.RelationalExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.RelationalExpression.Simple s) {
        return hydra.java.Serde.writeShiftExpression((s).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.RelationalExpression.LessThan lt) {
        return hydra.java.Serde.writeRelationalExpression_LessThan((lt).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.RelationalExpression.GreaterThan gt) {
        return hydra.java.Serde.writeRelationalExpression_GreaterThan((gt).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.RelationalExpression.LessThanEqual lte) {
        return hydra.java.Serde.writeRelationalExpression_LessThanEqual((lte).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.RelationalExpression.GreaterThanEqual gte) {
        return hydra.java.Serde.writeRelationalExpression_GreaterThanEqual((gte).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.RelationalExpression.Instanceof i) {
        return hydra.java.Serde.writeRelationalExpression_InstanceOf((i).value);
      }
    });
  }

  static hydra.ast.Expr writeRelationalExpression_GreaterThan(hydra.java.syntax.RelationalExpression_GreaterThan gt) {
    return hydra.Serialization.infixWs(
      ">",
      hydra.java.Serde.writeRelationalExpression((gt).lhs),
      hydra.java.Serde.writeShiftExpression((gt).rhs));
  }

  static hydra.ast.Expr writeRelationalExpression_GreaterThanEqual(hydra.java.syntax.RelationalExpression_GreaterThanEqual gte) {
    return hydra.Serialization.infixWs(
      ">=",
      hydra.java.Serde.writeRelationalExpression((gte).lhs),
      hydra.java.Serde.writeShiftExpression((gte).rhs));
  }

  static hydra.ast.Expr writeRelationalExpression_InstanceOf(hydra.java.syntax.RelationalExpression_InstanceOf io) {
    return hydra.Serialization.infixWs(
      "instanceof",
      hydra.java.Serde.writeRelationalExpression((io).lhs),
      hydra.java.Serde.writeReferenceType((io).rhs));
  }

  static hydra.ast.Expr writeRelationalExpression_LessThan(hydra.java.syntax.RelationalExpression_LessThan lt) {
    return hydra.Serialization.infixWs(
      "<",
      hydra.java.Serde.writeRelationalExpression((lt).lhs),
      hydra.java.Serde.writeShiftExpression((lt).rhs));
  }

  static hydra.ast.Expr writeRelationalExpression_LessThanEqual(hydra.java.syntax.RelationalExpression_LessThanEqual lte) {
    return hydra.Serialization.infixWs(
      "<=",
      hydra.java.Serde.writeRelationalExpression((lte).lhs),
      hydra.java.Serde.writeShiftExpression((lte).rhs));
  }

  static hydra.ast.Expr writeResult(hydra.java.syntax.Result r) {
    return (r).accept(new hydra.java.syntax.Result.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Result.Type t) {
        return hydra.java.Serde.writeUnannType((t).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Result.Void_ ignored) {
        return hydra.Serialization.cst("void");
      }
    });
  }

  static hydra.ast.Expr writeReturnStatement(hydra.java.syntax.ReturnStatement rs) {
    hydra.util.Maybe<hydra.java.syntax.Expression> mex = (rs).value;
    return hydra.Serialization.withSemi(hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.util.Maybe.just(hydra.Serialization.cst("return")),
      hydra.lib.maybes.Map.apply(
        hydra.java.Serde::writeExpression,
        mex)))));
  }

  static hydra.ast.Expr writeShiftExpression(hydra.java.syntax.ShiftExpression e) {
    return (e).accept(new hydra.java.syntax.ShiftExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ShiftExpression.Unary a) {
        return hydra.java.Serde.writeAdditiveExpression((a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ShiftExpression.ShiftLeft b) {
        return hydra.Serialization.infixWs(
          "<<",
          hydra.java.Serde.writeShiftExpression((b).value.lhs),
          hydra.java.Serde.writeAdditiveExpression((b).value.rhs));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ShiftExpression.ShiftRight b) {
        return hydra.Serialization.infixWs(
          ">>",
          hydra.java.Serde.writeShiftExpression((b).value.lhs),
          hydra.java.Serde.writeAdditiveExpression((b).value.rhs));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.ShiftExpression.ShiftRightZeroFill b) {
        return hydra.Serialization.infixWs(
          ">>>",
          hydra.java.Serde.writeShiftExpression((b).value.lhs),
          hydra.java.Serde.writeAdditiveExpression((b).value.rhs));
      }
    });
  }

  static hydra.ast.Expr writeSimpleTypeName(hydra.java.syntax.SimpleTypeName stn) {
    return hydra.java.Serde.writeTypeIdentifier((stn).value);
  }

  static hydra.ast.Expr writeSingleElementAnnotation(hydra.java.syntax.SingleElementAnnotation sea) {
    hydra.util.Maybe<hydra.java.syntax.ElementValue> mv = (sea).value;
    hydra.java.syntax.TypeName tname = (sea).name;
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.java.Serde.writeMarkerAnnotation(new hydra.java.syntax.MarkerAnnotation(tname)),
      (java.util.function.Function<hydra.java.syntax.ElementValue, hydra.ast.Expr>) (v -> hydra.Serialization.prefix(
        "@",
        hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.java.Serde.writeTypeName(tname),
          hydra.Serialization.parenList(
            false,
            java.util.Arrays.asList(hydra.java.Serde.writeElementValue(v))))))),
      mv);
  }

  static hydra.ast.Expr writeStatement(hydra.java.syntax.Statement s) {
    return (s).accept(new hydra.java.syntax.Statement.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Statement.WithoutTrailing s2) {
        return hydra.java.Serde.writeStatementWithoutTrailingSubstatement((s2).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Statement.Labeled l) {
        return hydra.java.Serde.writeLabeledStatement((l).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Statement.IfThen it) {
        return hydra.java.Serde.writeIfThenStatement((it).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Statement.IfThenElse ite) {
        return hydra.java.Serde.writeIfThenElseStatement((ite).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Statement.While w) {
        return hydra.java.Serde.writeWhileStatement((w).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Statement.For f) {
        return hydra.java.Serde.writeForStatement((f).value);
      }
    });
  }

  static hydra.ast.Expr writeStatementExpression(hydra.java.syntax.StatementExpression e) {
    return (e).accept(new hydra.java.syntax.StatementExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementExpression.Assignment a) {
        return hydra.java.Serde.writeAssignment((a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementExpression.PreIncrement pi) {
        return hydra.java.Serde.writePreIncrementExpression((pi).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementExpression.PreDecrement pd) {
        return hydra.java.Serde.writePreDecrementExpression((pd).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementExpression.PostIncrement pi) {
        return hydra.java.Serde.writePostIncrementExpression((pi).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementExpression.PostDecrement pd) {
        return hydra.java.Serde.writePostDecrementExpression((pd).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementExpression.MethodInvocation m) {
        return hydra.java.Serde.writeMethodInvocation((m).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementExpression.ClassInstanceCreation cic) {
        return hydra.java.Serde.writeClassInstanceCreationExpression((cic).value);
      }
    });
  }

  static hydra.ast.Expr writeStatementWithoutTrailingSubstatement(hydra.java.syntax.StatementWithoutTrailingSubstatement s) {
    return (s).accept(new hydra.java.syntax.StatementWithoutTrailingSubstatement.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementWithoutTrailingSubstatement.Block b) {
        return hydra.java.Serde.writeBlock((b).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementWithoutTrailingSubstatement.Empty ignored) {
        return hydra.Serialization.cst(";");
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementWithoutTrailingSubstatement.Expression e) {
        return hydra.java.Serde.writeExpressionStatement((e).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementWithoutTrailingSubstatement.Assert a) {
        return hydra.java.Serde.writeAssertStatement((a).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementWithoutTrailingSubstatement.Switch s2) {
        return hydra.java.Serde.writeSwitchStatement((s2).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementWithoutTrailingSubstatement.Do d) {
        return hydra.java.Serde.writeDoStatement((d).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementWithoutTrailingSubstatement.Break b) {
        return hydra.java.Serde.writeBreakStatement((b).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementWithoutTrailingSubstatement.Continue c) {
        return hydra.java.Serde.writeContinueStatement((c).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementWithoutTrailingSubstatement.Return r) {
        return hydra.java.Serde.writeReturnStatement((r).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementWithoutTrailingSubstatement.Synchronized s2) {
        return hydra.java.Serde.writeSynchronizedStatement((s2).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementWithoutTrailingSubstatement.Throw t) {
        return hydra.java.Serde.writeThrowStatement((t).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.StatementWithoutTrailingSubstatement.Try t) {
        return hydra.java.Serde.writeTryStatement((t).value);
      }
    });
  }

  static <T0> hydra.ast.Expr writeStaticInitializer(T0 ignored) {
    return hydra.Serialization.cst("STUB:StaticInitializer");
  }

  static hydra.ast.Expr writeStringLiteral(hydra.java.syntax.StringLiteral sl) {
    String s = (sl).value;
    return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
      "\"",
      hydra.lib.strings.Cat2.apply(
        hydra.java.Serde.escapeJavaString(s),
        "\"")));
  }

  static <T0> hydra.ast.Expr writeSwitchStatement(T0 ignored) {
    return hydra.Serialization.cst("STUB:SwitchStatement");
  }

  static <T0> hydra.ast.Expr writeSynchronizedStatement(T0 ignored) {
    return hydra.Serialization.cst("STUB:SynchronizedStatement");
  }

  static hydra.ast.Expr writeThrowStatement(hydra.java.syntax.ThrowStatement ts) {
    return hydra.Serialization.withSemi(hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst("throw"),
      hydra.java.Serde.writeExpression((ts).value))));
  }

  static <T0> hydra.ast.Expr writeThrows(T0 ignored) {
    return hydra.Serialization.cst("STUB:Throws");
  }

  static <T0> hydra.ast.Expr writeTryStatement(T0 ignored) {
    return hydra.Serialization.cst("STUB:TryStatement");
  }

  static hydra.ast.Expr writeType(hydra.java.syntax.Type t) {
    return (t).accept(new hydra.java.syntax.Type.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Type.Primitive pt) {
        return hydra.java.Serde.writePrimitiveTypeWithAnnotations((pt).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.Type.Reference rt) {
        return hydra.java.Serde.writeReferenceType((rt).value);
      }
    });
  }

  static hydra.ast.Expr writeTypeArgument(hydra.java.syntax.TypeArgument a) {
    return (a).accept(new hydra.java.syntax.TypeArgument.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.TypeArgument.Reference rt) {
        return hydra.java.Serde.writeReferenceType((rt).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.TypeArgument.Wildcard w) {
        return hydra.java.Serde.writeWildcard((w).value);
      }
    });
  }

  static hydra.ast.Expr writeTypeArgumentsOrDiamond(hydra.java.syntax.TypeArgumentsOrDiamond targs) {
    return (targs).accept(new hydra.java.syntax.TypeArgumentsOrDiamond.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.TypeArgumentsOrDiamond.Arguments args) {
        return hydra.Serialization.angleBracesList(
          hydra.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.java.Serde::writeTypeArgument,
            (args).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.TypeArgumentsOrDiamond.Diamond ignored) {
        return hydra.Serialization.cst("<>");
      }
    });
  }

  static hydra.ast.Expr writeTypeBound(hydra.java.syntax.TypeBound b) {
    return (b).accept(new hydra.java.syntax.TypeBound.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.TypeBound.Variable tv) {
        return hydra.java.Serde.writeTypeVariable((tv).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.TypeBound.ClassOrInterface ci) {
        java.util.List<hydra.java.syntax.AdditionalBound> additional = (ci).value.additional;
        hydra.java.syntax.ClassOrInterfaceType cit = (ci).value.type;
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(additional),
          () -> hydra.java.Serde.writeClassOrInterfaceType(cit),
          () -> hydra.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
            hydra.java.Serde.writeClassOrInterfaceType(cit),
            hydra.lib.lists.Map.apply(
              hydra.java.Serde::writeAdditionalBound,
              additional))));
      }
    });
  }

  static hydra.ast.Expr writeTypeDeclaration(hydra.java.syntax.TypeDeclaration d) {
    return (d).accept(new hydra.java.syntax.TypeDeclaration.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.TypeDeclaration.Class_ d2) {
        return hydra.java.Serde.writeClassDeclaration((d2).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.TypeDeclaration.Interface d2) {
        return hydra.java.Serde.writeInterfaceDeclaration((d2).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.TypeDeclaration.None ignored) {
        return hydra.Serialization.cst(";");
      }
    });
  }

  static hydra.ast.Expr writeTypeDeclarationWithComments(hydra.java.syntax.TypeDeclarationWithComments tdwc) {
    hydra.java.syntax.TypeDeclaration d = (tdwc).value;
    hydra.util.Maybe<String> mc = (tdwc).comments;
    return hydra.java.Serde.withComments(
      mc,
      hydra.java.Serde.writeTypeDeclaration(d));
  }

  static hydra.ast.Expr writeTypeIdentifier(hydra.java.syntax.TypeIdentifier tid) {
    return hydra.java.Serde.writeIdentifier((tid).value);
  }

  static hydra.ast.Expr writeTypeName(hydra.java.syntax.TypeName tn) {
    hydra.java.syntax.TypeIdentifier id = (tn).identifier;
    hydra.util.Maybe<hydra.java.syntax.PackageOrTypeName> mqual = (tn).qualifier;
    return hydra.Serialization.dotSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.maybes.Map.apply(
        hydra.java.Serde::writePackageOrTypeName,
        mqual),
      hydra.util.Maybe.just(hydra.java.Serde.writeTypeIdentifier(id)))));
  }

  static hydra.ast.Expr writeTypeParameter(hydra.java.syntax.TypeParameter tp) {
    hydra.util.Maybe<hydra.java.syntax.TypeBound> bound = (tp).bound;
    hydra.java.syntax.TypeIdentifier id = (tp).identifier;
    java.util.List<hydra.java.syntax.TypeParameterModifier> mods = (tp).modifiers;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.java.Serde::writeTypeParameterModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.java.Serde.writeTypeIdentifier(id)),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.java.syntax.TypeBound, hydra.ast.Expr>) (b -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("extends"),
          hydra.java.Serde.writeTypeBound(b)))),
        bound))));
  }

  static hydra.ast.Expr writeTypeParameterModifier(hydra.java.syntax.TypeParameterModifier tpm) {
    return hydra.java.Serde.writeAnnotation((tpm).value);
  }

  static hydra.ast.Expr writeTypeVariable(hydra.java.syntax.TypeVariable tv) {
    java.util.List<hydra.java.syntax.Annotation> anns = (tv).annotations;
    hydra.java.syntax.TypeIdentifier id = (tv).identifier;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(anns),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.java.Serde::writeAnnotation,
          anns)))),
      hydra.util.Maybe.just(hydra.java.Serde.writeTypeIdentifier(id)))));
  }

  static hydra.ast.Expr writeUnannType(hydra.java.syntax.UnannType ut) {
    return hydra.java.Serde.writeType((ut).value);
  }

  static hydra.ast.Expr writeUnaryExpression(hydra.java.syntax.UnaryExpression e) {
    return (e).accept(new hydra.java.syntax.UnaryExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.UnaryExpression.PreIncrement pi) {
        return hydra.java.Serde.writePreIncrementExpression((pi).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.UnaryExpression.PreDecrement pd) {
        return hydra.java.Serde.writePreDecrementExpression((pd).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.UnaryExpression.Plus p) {
        return hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("+"),
          hydra.java.Serde.writeUnaryExpression((p).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.UnaryExpression.Minus m) {
        return hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("-"),
          hydra.java.Serde.writeUnaryExpression((m).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.UnaryExpression.Other o) {
        return hydra.java.Serde.writeUnaryExpressionNotPlusMinus((o).value);
      }
    });
  }

  static hydra.ast.Expr writeUnaryExpressionNotPlusMinus(hydra.java.syntax.UnaryExpressionNotPlusMinus e) {
    return (e).accept(new hydra.java.syntax.UnaryExpressionNotPlusMinus.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.UnaryExpressionNotPlusMinus.Postfix p) {
        return hydra.java.Serde.writePostfixExpression((p).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.UnaryExpressionNotPlusMinus.Tilde u) {
        return hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("~"),
          hydra.java.Serde.writeUnaryExpression((u).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.UnaryExpressionNotPlusMinus.Not u) {
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("!"),
          hydra.java.Serde.writeUnaryExpression((u).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.UnaryExpressionNotPlusMinus.Cast c) {
        return hydra.java.Serde.writeCastExpression((c).value);
      }
    });
  }

  static hydra.ast.Expr writeUnqualifiedClassInstanceCreationExpression(hydra.java.syntax.UnqualifiedClassInstanceCreationExpression ucice) {
    java.util.List<hydra.java.syntax.Expression> args = (ucice).arguments;
    hydra.java.syntax.ClassOrInterfaceTypeToInstantiate cit = (ucice).classOrInterface;
    hydra.util.Maybe<hydra.java.syntax.ClassBody> mbody = (ucice).body;
    java.util.List<hydra.java.syntax.TypeArgument> targs = (ucice).typeArguments;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.util.Maybe.just(hydra.Serialization.cst("new")),
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(targs),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.angleBracesList(
          hydra.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.java.Serde::writeTypeArgument,
            targs)))),
      hydra.util.Maybe.just(hydra.Serialization.noSep(java.util.Arrays.asList(
        hydra.java.Serde.writeClassOrInterfaceTypeToInstantiate(cit),
        hydra.Serialization.parenList(
          false,
          hydra.lib.lists.Map.apply(
            hydra.java.Serde::writeExpression,
            args))))),
      hydra.lib.maybes.Map.apply(
        hydra.java.Serde::writeClassBody,
        mbody))));
  }

  static <T0> hydra.ast.Expr writeVariableArityParameter(T0 ignored) {
    return hydra.Serialization.cst("STUB:VariableArityParameter");
  }

  static hydra.ast.Expr writeVariableDeclarator(hydra.java.syntax.VariableDeclarator vd) {
    hydra.java.syntax.VariableDeclaratorId id = (vd).id;
    hydra.ast.Expr idSec = hydra.java.Serde.writeVariableDeclaratorId(id);
    hydra.util.Maybe<hydra.java.syntax.VariableInitializer> minit = (vd).initializer;
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> idSec,
      (java.util.function.Function<hydra.java.syntax.VariableInitializer, hydra.ast.Expr>) (init -> hydra.Serialization.infixWs(
        "=",
        idSec,
        hydra.java.Serde.writeVariableInitializer(init))),
      minit);
  }

  static hydra.ast.Expr writeVariableDeclaratorId(hydra.java.syntax.VariableDeclaratorId vdi) {
    hydra.java.syntax.Identifier id = (vdi).identifier;
    hydra.util.Maybe<hydra.java.syntax.Dims> mdims = (vdi).dims;
    return hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.util.Maybe.just(hydra.java.Serde.writeIdentifier(id)),
      hydra.lib.maybes.Map.apply(
        hydra.java.Serde::writeDims,
        mdims))));
  }

  static hydra.ast.Expr writeVariableInitializer(hydra.java.syntax.VariableInitializer i) {
    return (i).accept(new hydra.java.syntax.VariableInitializer.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.VariableInitializer.Expression e) {
        return hydra.java.Serde.writeExpression((e).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.VariableInitializer.ArrayInitializer ai) {
        return hydra.java.Serde.writeArrayInitializer((ai).value);
      }
    });
  }

  static hydra.ast.Expr writeVariableModifier(hydra.java.syntax.VariableModifier m) {
    return (m).accept(new hydra.java.syntax.VariableModifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.VariableModifier.Annotation ann) {
        return hydra.java.Serde.writeAnnotation((ann).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.VariableModifier.Final ignored) {
        return hydra.Serialization.cst("final");
      }
    });
  }

  static hydra.ast.Expr writeWhileStatement(hydra.java.syntax.WhileStatement ws) {
    hydra.java.syntax.Statement body = (ws).body;
    hydra.util.Maybe<hydra.java.syntax.Expression> mcond = (ws).cond;
    hydra.util.Lazy<hydra.ast.Expr> condSer = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.Serialization.cst("true"),
      (java.util.function.Function<hydra.java.syntax.Expression, hydra.ast.Expr>) (c -> hydra.java.Serde.writeExpression(c)),
      mcond));
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst("while"),
      hydra.Serialization.parenList(
        false,
        java.util.Arrays.asList(condSer.get())),
      hydra.Serialization.curlyBlock(
        hydra.Serialization.fullBlockStyle(),
        hydra.java.Serde.writeStatement(body))));
  }

  static hydra.ast.Expr writeWildcard(hydra.java.syntax.Wildcard w) {
    java.util.List<hydra.java.syntax.Annotation> anns = (w).annotations;
    hydra.util.Maybe<hydra.java.syntax.WildcardBounds> mbounds = (w).wildcard;
    return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(anns),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.Serialization.commaSep(
          hydra.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.java.Serde::writeAnnotation,
            anns)))),
      hydra.util.Maybe.just(hydra.Serialization.cst("*")),
      hydra.lib.maybes.Map.apply(
        hydra.java.Serde::writeWildcardBounds,
        mbounds))));
  }

  static hydra.ast.Expr writeWildcardBounds(hydra.java.syntax.WildcardBounds b) {
    return (b).accept(new hydra.java.syntax.WildcardBounds.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.WildcardBounds.Extends rt) {
        return hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("extends"),
          hydra.java.Serde.writeReferenceType((rt).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.java.syntax.WildcardBounds.Super rt) {
        return hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("super"),
          hydra.java.Serde.writeReferenceType((rt).value)));
      }
    });
  }
}
