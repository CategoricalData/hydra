// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.serde;

/**
 * Java serializer: converts Java AST to concrete syntax
 */
public interface Serde {
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
  
  static String padHex4(Integer n) {
    Integer r3 = hydra.lib.math.Mod.apply(
      n,
      4096);
    Integer r2 = hydra.lib.math.Mod.apply(
      r3,
      256);
    Integer d0 = hydra.lib.math.Mod.apply(
      r2,
      16);
    Integer d1 = hydra.lib.math.Div.apply(
      r2,
      16);
    Integer d2 = hydra.lib.math.Div.apply(
      r3,
      256);
    Integer d3 = hydra.lib.math.Div.apply(
      n,
      4096);
    return hydra.lib.strings.FromList.apply(java.util.List.of(
      hydra.ext.java.serde.Serde.hexDigit(d3),
      hydra.ext.java.serde.Serde.hexDigit(d2),
      hydra.ext.java.serde.Serde.hexDigit(d1),
      hydra.ext.java.serde.Serde.hexDigit(d0)));
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
          Integer hi = hydra.lib.math.Add.apply(
            55296,
            hydra.lib.math.Div.apply(
              n_,
              1024));
          return ((java.util.function.Supplier<String>) (() -> {
            Integer lo = hydra.lib.math.Add.apply(
              56320,
              hydra.lib.math.Mod.apply(
                n_,
                1024));
            return hydra.lib.strings.Cat2.apply(
              hydra.lib.strings.Cat2.apply(
                "\\u",
                hydra.ext.java.serde.Serde.padHex4(hi)),
              hydra.lib.strings.Cat2.apply(
                "\\u",
                hydra.ext.java.serde.Serde.padHex4(lo)));
          })).get();
        })).get();
      })).get(),
      () -> hydra.lib.strings.Cat2.apply(
        "\\u",
        hydra.ext.java.serde.Serde.padHex4(n)));
  }
  
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
                    () -> hydra.lib.strings.FromList.apply(java.util.List.of(c)),
                    () -> hydra.ext.java.serde.Serde.javaUnicodeEscape(c)))))))));
  }
  
  static String escapeJavaString(String s) {
    return hydra.lib.strings.Cat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<Integer, String>) (c -> hydra.ext.java.serde.Serde.escapeJavaChar(c)),
      hydra.lib.strings.ToList.apply(s)));
  }
  
  static hydra.ast.Expr writeAdditionalBound(hydra.ext.java.syntax.AdditionalBound ab) {
    return hydra.serialization.Serialization.spaceSep(java.util.List.of(
      hydra.serialization.Serialization.cst("&"),
      hydra.ext.java.serde.Serde.writeInterfaceType((ab).value)));
  }
  
  static hydra.ast.Expr writeAdditiveExpression(hydra.ext.java.syntax.AdditiveExpression e) {
    return (e).accept(new hydra.ext.java.syntax.AdditiveExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.AdditiveExpression.Unary m) {
        return hydra.ext.java.serde.Serde.writeMultiplicativeExpression((m).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.AdditiveExpression.Plus b) {
        return hydra.serialization.Serialization.infixWs(
          "+",
          hydra.ext.java.serde.Serde.writeAdditiveExpression(((b).value).lhs),
          hydra.ext.java.serde.Serde.writeMultiplicativeExpression(((b).value).rhs));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.AdditiveExpression.Minus b) {
        return hydra.serialization.Serialization.infixWs(
          "-",
          hydra.ext.java.serde.Serde.writeAdditiveExpression(((b).value).lhs),
          hydra.ext.java.serde.Serde.writeMultiplicativeExpression(((b).value).rhs));
      }
    });
  }
  
  static hydra.ast.Expr writeAmbiguousName(hydra.ext.java.syntax.AmbiguousName an) {
    return hydra.serialization.Serialization.dotSep(hydra.lib.lists.Map.apply(
      hydra.ext.java.serde.Serde::writeIdentifier,
      (an).value));
  }
  
  static hydra.ast.Expr writeAndExpression(hydra.ext.java.syntax.AndExpression ae) {
    return hydra.serialization.Serialization.infixWsList(
      "&",
      hydra.lib.lists.Map.apply(
        hydra.ext.java.serde.Serde::writeEqualityExpression,
        (ae).value));
  }
  
  static hydra.ast.Expr writeAnnotatedIdentifier(hydra.ext.java.syntax.AnnotatedIdentifier ai) {
    return hydra.ext.java.serde.Serde.writeIdentifier((ai).identifier);
  }
  
  static hydra.ast.Expr writeAnnotation(hydra.ext.java.syntax.Annotation ann) {
    return (ann).accept(new hydra.ext.java.syntax.Annotation.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Annotation.Normal n) {
        return hydra.ext.java.serde.Serde.writeNormalAnnotation((n).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Annotation.Marker m) {
        return hydra.ext.java.serde.Serde.writeMarkerAnnotation((m).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Annotation.SingleElement s) {
        return hydra.ext.java.serde.Serde.writeSingleElementAnnotation((s).value);
      }
    });
  }
  
  static <T0> hydra.ast.Expr writeAnnotationTypeDeclaration(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:AnnotationTypeDeclaration");
  }
  
  static <T0> hydra.ast.Expr writeArrayAccess(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:ArrayAccess");
  }
  
  static hydra.ast.Expr writeArrayCreationExpression(hydra.ext.java.syntax.ArrayCreationExpression ace) {
    return (ace).accept(new hydra.ext.java.syntax.ArrayCreationExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ArrayCreationExpression.PrimitiveArray pa) {
        hydra.ext.java.syntax.ArrayInitializer ai = ((pa).value).array;
        hydra.ext.java.syntax.PrimitiveTypeWithAnnotations pt = ((pa).value).type;
        return hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.serialization.Serialization.cst("new"),
          hydra.serialization.Serialization.noSep(java.util.List.of(
            hydra.ext.java.serde.Serde.writePrimitiveTypeWithAnnotations(pt),
            hydra.serialization.Serialization.cst("[]"))),
          hydra.ext.java.serde.Serde.writeArrayInitializer(ai)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ArrayCreationExpression.ClassOrInterfaceArray ignored) {
        return hydra.serialization.Serialization.cst("STUB:ArrayCreationExpression");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ArrayCreationExpression.Primitive ignored) {
        return hydra.serialization.Serialization.cst("STUB:ArrayCreationExpression");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ArrayCreationExpression.ClassOrInterface ignored) {
        return hydra.serialization.Serialization.cst("STUB:ArrayCreationExpression");
      }
    });
  }
  
  static hydra.ast.Expr writeArrayInitializer(hydra.ext.java.syntax.ArrayInitializer ai) {
    java.util.List<java.util.List<hydra.ext.java.syntax.VariableInitializer>> groups = (ai).value;
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Length.apply(groups),
        1),
      () -> hydra.serialization.Serialization.noSep(java.util.List.of(
        hydra.serialization.Serialization.cst("{"),
        hydra.serialization.Serialization.commaSep(
          hydra.serialization.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.java.serde.Serde::writeVariableInitializer,
            hydra.lib.lists.Head.apply(groups))),
        hydra.serialization.Serialization.cst("}"))),
      () -> hydra.serialization.Serialization.cst("{}"));
  }
  
  static hydra.ast.Expr writeArrayType(hydra.ext.java.syntax.ArrayType at) {
    hydra.ext.java.syntax.Dims dims = (at).dims;
    hydra.ext.java.syntax.ArrayType_Variant variant = (at).variant;
    hydra.ast.Expr varExpr = (variant).accept(new hydra.ext.java.syntax.ArrayType_Variant.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ArrayType_Variant.Primitive pt) {
        return hydra.ext.java.serde.Serde.writePrimitiveTypeWithAnnotations((pt).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ArrayType_Variant.ClassOrInterface cit) {
        return hydra.ext.java.serde.Serde.writeClassOrInterfaceType((cit).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ArrayType_Variant.Variable tv) {
        return hydra.ext.java.serde.Serde.writeTypeVariable((tv).value);
      }
    });
    return hydra.serialization.Serialization.noSep(java.util.List.of(
      varExpr,
      hydra.ext.java.serde.Serde.writeDims(dims)));
  }
  
  static <T0> hydra.ast.Expr writeAssertStatement(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:AssertStatement");
  }
  
  static hydra.ast.Expr writeAssignment(hydra.ext.java.syntax.Assignment a) {
    hydra.ext.java.syntax.AssignmentOperator op = (a).op;
    String ctop = (op).accept(new hydra.ext.java.syntax.AssignmentOperator.PartialVisitor<>() {
      @Override
      public String visit(hydra.ext.java.syntax.AssignmentOperator.Simple ignored) {
        return "=";
      }
      
      @Override
      public String visit(hydra.ext.java.syntax.AssignmentOperator.Times ignored) {
        return "*=";
      }
      
      @Override
      public String visit(hydra.ext.java.syntax.AssignmentOperator.Div ignored) {
        return "/=";
      }
      
      @Override
      public String visit(hydra.ext.java.syntax.AssignmentOperator.Mod ignored) {
        return "%=";
      }
      
      @Override
      public String visit(hydra.ext.java.syntax.AssignmentOperator.Plus ignored) {
        return "+=";
      }
      
      @Override
      public String visit(hydra.ext.java.syntax.AssignmentOperator.Minus ignored) {
        return "-=";
      }
      
      @Override
      public String visit(hydra.ext.java.syntax.AssignmentOperator.ShiftLeft ignored) {
        return "<<=";
      }
      
      @Override
      public String visit(hydra.ext.java.syntax.AssignmentOperator.ShiftRight ignored) {
        return ">>=";
      }
      
      @Override
      public String visit(hydra.ext.java.syntax.AssignmentOperator.ShiftRightZeroFill ignored) {
        return ">>>=";
      }
      
      @Override
      public String visit(hydra.ext.java.syntax.AssignmentOperator.And ignored) {
        return "&=";
      }
      
      @Override
      public String visit(hydra.ext.java.syntax.AssignmentOperator.Xor ignored) {
        return "^=";
      }
      
      @Override
      public String visit(hydra.ext.java.syntax.AssignmentOperator.Or ignored) {
        return "|=";
      }
    });
    hydra.ext.java.syntax.LeftHandSide lhs = (a).lhs;
    hydra.ext.java.syntax.Expression rhs = (a).expression;
    return hydra.serialization.Serialization.infixWs(
      ctop,
      hydra.ext.java.serde.Serde.writeLeftHandSide(lhs),
      hydra.ext.java.serde.Serde.writeExpression(rhs));
  }
  
  static hydra.ast.Expr writeAssignmentExpression(hydra.ext.java.syntax.AssignmentExpression e) {
    return (e).accept(new hydra.ext.java.syntax.AssignmentExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.AssignmentExpression.Conditional c) {
        return hydra.ext.java.serde.Serde.writeConditionalExpression((c).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.AssignmentExpression.Assignment a) {
        return hydra.ext.java.serde.Serde.writeAssignment((a).value);
      }
    });
  }
  
  static hydra.ast.Expr writeBlock(hydra.ext.java.syntax.Block b) {
    return hydra.serialization.Serialization.curlyBlock(
      hydra.serialization.Serialization.fullBlockStyle(),
      hydra.serialization.Serialization.newlineSep(hydra.lib.lists.Map.apply(
        hydra.ext.java.serde.Serde::writeBlockStatement,
        (b).value)));
  }
  
  static hydra.ast.Expr writeBlockStatement(hydra.ext.java.syntax.BlockStatement s) {
    return (s).accept(new hydra.ext.java.syntax.BlockStatement.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.BlockStatement.LocalVariableDeclaration d) {
        return hydra.ext.java.serde.Serde.writeLocalVariableDeclarationStatement((d).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.BlockStatement.Class_ cd) {
        return hydra.ext.java.serde.Serde.writeClassDeclaration((cd).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.BlockStatement.Statement s2) {
        return hydra.ext.java.serde.Serde.writeStatement((s2).value);
      }
    });
  }
  
  static hydra.ast.Expr writeBreakStatement(hydra.ext.java.syntax.BreakStatement bs) {
    hydra.util.Maybe<hydra.ext.java.syntax.Identifier> mlabel = (bs).value;
    return hydra.serialization.Serialization.withSemi(hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.util.Maybe.just(hydra.serialization.Serialization.cst("break")),
      hydra.lib.maybes.Map.apply(
        hydra.ext.java.serde.Serde::writeIdentifier,
        mlabel)))));
  }
  
  static hydra.ast.Expr writeCastExpression(hydra.ext.java.syntax.CastExpression e) {
    return (e).accept(new hydra.ext.java.syntax.CastExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.CastExpression.Primitive p) {
        return hydra.ext.java.serde.Serde.writeCastExpression_Primitive((p).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.CastExpression.NotPlusMinus npm) {
        return hydra.ext.java.serde.Serde.writeCastExpression_NotPlusMinus((npm).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.CastExpression.Lambda l) {
        return hydra.ext.java.serde.Serde.writeCastExpression_Lambda((l).value);
      }
    });
  }
  
  static <T0> hydra.ast.Expr writeCastExpression_Lambda(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:CastExpression_Lambda");
  }
  
  static hydra.ast.Expr writeCastExpression_NotPlusMinus(hydra.ext.java.syntax.CastExpression_NotPlusMinus npm) {
    hydra.ext.java.syntax.UnaryExpression ex = (npm).expression;
    hydra.ext.java.syntax.CastExpression_RefAndBounds rb = (npm).refAndBounds;
    return hydra.serialization.Serialization.spaceSep(java.util.List.of(
      hydra.ext.java.serde.Serde.writeCastExpression_RefAndBounds(rb),
      hydra.ext.java.serde.Serde.writeUnaryExpression(ex)));
  }
  
  static hydra.ast.Expr writeCastExpression_RefAndBounds(hydra.ext.java.syntax.CastExpression_RefAndBounds rab) {
    java.util.List<hydra.ext.java.syntax.AdditionalBound> adds = (rab).bounds;
    hydra.ext.java.syntax.ReferenceType rt = (rab).type;
    return hydra.serialization.Serialization.parenList(
      false,
      java.util.List.of(hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
        hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeReferenceType(rt)),
        hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(adds),
          () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
          () -> hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Map.apply(
            hydra.ext.java.serde.Serde::writeAdditionalBound,
            adds)))))))));
  }
  
  static hydra.ast.Expr writeCastExpression_Primitive(hydra.ext.java.syntax.CastExpression_Primitive cp) {
    hydra.ext.java.syntax.UnaryExpression ex = (cp).expression;
    hydra.ext.java.syntax.PrimitiveTypeWithAnnotations pt = (cp).type;
    return hydra.serialization.Serialization.spaceSep(java.util.List.of(
      hydra.serialization.Serialization.parenList(
        false,
        java.util.List.of(hydra.ext.java.serde.Serde.writePrimitiveTypeWithAnnotations(pt))),
      hydra.ext.java.serde.Serde.writeUnaryExpression(ex)));
  }
  
  static hydra.ast.Expr writeClassBody(hydra.ext.java.syntax.ClassBody cb) {
    return hydra.serialization.Serialization.curlyBlock(
      hydra.serialization.Serialization.fullBlockStyle(),
      hydra.serialization.Serialization.doubleNewlineSep(hydra.lib.lists.Map.apply(
        hydra.ext.java.serde.Serde::writeClassBodyDeclarationWithComments,
        (cb).value)));
  }
  
  static hydra.ast.Expr writeClassBodyDeclaration(hydra.ext.java.syntax.ClassBodyDeclaration d) {
    return (d).accept(new hydra.ext.java.syntax.ClassBodyDeclaration.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassBodyDeclaration.ClassMember d2) {
        return hydra.ext.java.serde.Serde.writeClassMemberDeclaration((d2).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassBodyDeclaration.InstanceInitializer i) {
        return hydra.ext.java.serde.Serde.writeInstanceInitializer((i).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassBodyDeclaration.StaticInitializer i) {
        return hydra.ext.java.serde.Serde.writeStaticInitializer((i).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassBodyDeclaration.ConstructorDeclaration d2) {
        return hydra.ext.java.serde.Serde.writeConstructorDeclaration((d2).value);
      }
    });
  }
  
  static hydra.ast.Expr writeClassBodyDeclarationWithComments(hydra.ext.java.syntax.ClassBodyDeclarationWithComments cbdwc) {
    hydra.ext.java.syntax.ClassBodyDeclaration d = (cbdwc).value;
    hydra.util.Maybe<String> mc = (cbdwc).comments;
    return hydra.ext.java.serde.Serde.withComments(
      mc,
      hydra.ext.java.serde.Serde.writeClassBodyDeclaration(d));
  }
  
  static hydra.ast.Expr writeClassDeclaration(hydra.ext.java.syntax.ClassDeclaration d) {
    return (d).accept(new hydra.ext.java.syntax.ClassDeclaration.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassDeclaration.Normal nd) {
        return hydra.ext.java.serde.Serde.writeNormalClassDeclaration((nd).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassDeclaration.Enum_ ed) {
        return hydra.ext.java.serde.Serde.writeEnumDeclaration((ed).value);
      }
    });
  }
  
  static hydra.ast.Expr writeClassInstanceCreationExpression(hydra.ext.java.syntax.ClassInstanceCreationExpression cice) {
    hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression e = (cice).expression;
    hydra.util.Maybe<hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier> mqual = (cice).qualifier;
    return hydra.lib.maybes.Maybe.apply(
      hydra.ext.java.serde.Serde.writeUnqualifiedClassInstanceCreationExpression(e),
      (java.util.function.Function<hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier, hydra.ast.Expr>) (q -> hydra.serialization.Serialization.dotSep(java.util.List.of(
        hydra.ext.java.serde.Serde.writeClassInstanceCreationExpression_Qualifier(q),
        hydra.ext.java.serde.Serde.writeUnqualifiedClassInstanceCreationExpression(e)))),
      mqual);
  }
  
  static hydra.ast.Expr writeClassInstanceCreationExpression_Qualifier(hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier q) {
    return (q).accept(new hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier.Expression en) {
        return hydra.ext.java.serde.Serde.writeExpressionName((en).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier.Primary p) {
        return hydra.ext.java.serde.Serde.writePrimary((p).value);
      }
    });
  }
  
  static <T0> hydra.ast.Expr writeClassLiteral(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:ClassLiteral");
  }
  
  static hydra.ast.Expr writeClassMemberDeclaration(hydra.ext.java.syntax.ClassMemberDeclaration d) {
    return (d).accept(new hydra.ext.java.syntax.ClassMemberDeclaration.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassMemberDeclaration.Field fd) {
        return hydra.ext.java.serde.Serde.writeFieldDeclaration((fd).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassMemberDeclaration.Method md) {
        return hydra.ext.java.serde.Serde.writeMethodDeclaration((md).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassMemberDeclaration.Class_ cd) {
        return hydra.ext.java.serde.Serde.writeClassDeclaration((cd).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassMemberDeclaration.Interface id) {
        return hydra.ext.java.serde.Serde.writeInterfaceDeclaration((id).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassMemberDeclaration.None ignored) {
        return hydra.serialization.Serialization.cst(";");
      }
    });
  }
  
  static hydra.ast.Expr writeClassModifier(hydra.ext.java.syntax.ClassModifier m) {
    return (m).accept(new hydra.ext.java.syntax.ClassModifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassModifier.Annotation ann) {
        return hydra.ext.java.serde.Serde.writeAnnotation((ann).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassModifier.Public ignored) {
        return hydra.serialization.Serialization.cst("public");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassModifier.Protected ignored) {
        return hydra.serialization.Serialization.cst("protected");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassModifier.Private ignored) {
        return hydra.serialization.Serialization.cst("private");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassModifier.Abstract ignored) {
        return hydra.serialization.Serialization.cst("abstract");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassModifier.Static ignored) {
        return hydra.serialization.Serialization.cst("static");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassModifier.Final ignored) {
        return hydra.serialization.Serialization.cst("final");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassModifier.Strictfp ignored) {
        return hydra.serialization.Serialization.cst("strictfp");
      }
    });
  }
  
  static hydra.ast.Expr writeClassOrInterfaceType(hydra.ext.java.syntax.ClassOrInterfaceType cit) {
    return (cit).accept(new hydra.ext.java.syntax.ClassOrInterfaceType.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassOrInterfaceType.Class_ ct) {
        return hydra.ext.java.serde.Serde.writeClassType((ct).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassOrInterfaceType.Interface it) {
        return hydra.ext.java.serde.Serde.writeInterfaceType((it).value);
      }
    });
  }
  
  static hydra.ast.Expr writeClassOrInterfaceTypeToInstantiate(hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate coitti) {
    java.util.List<hydra.ext.java.syntax.AnnotatedIdentifier> ids = (coitti).identifiers;
    hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond> margs = (coitti).typeArguments;
    return hydra.serialization.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.util.Maybe.just(hydra.serialization.Serialization.dotSep(hydra.lib.lists.Map.apply(
        hydra.ext.java.serde.Serde::writeAnnotatedIdentifier,
        ids))),
      hydra.lib.maybes.Map.apply(
        hydra.ext.java.serde.Serde::writeTypeArgumentsOrDiamond,
        margs))));
  }
  
  static hydra.ast.Expr writeClassType(hydra.ext.java.syntax.ClassType ct) {
    java.util.List<hydra.ext.java.syntax.Annotation> anns = (ct).annotations;
    java.util.List<hydra.ext.java.syntax.TypeArgument> args = (ct).arguments;
    hydra.ext.java.syntax.TypeIdentifier id = (ct).identifier;
    hydra.ext.java.syntax.ClassTypeQualifier qual = (ct).qualifier;
    hydra.ast.Expr qualifiedId = (qual).accept(new hydra.ext.java.syntax.ClassTypeQualifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassTypeQualifier.None ignored) {
        return hydra.ext.java.serde.Serde.writeTypeIdentifier(id);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassTypeQualifier.Package_ pkg) {
        return hydra.serialization.Serialization.dotSep(java.util.List.of(
          hydra.ext.java.serde.Serde.writePackageName((pkg).value),
          hydra.ext.java.serde.Serde.writeTypeIdentifier(id)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ClassTypeQualifier.Parent cit) {
        return hydra.serialization.Serialization.dotSep(java.util.List.of(
          hydra.ext.java.serde.Serde.writeClassOrInterfaceType((cit).value),
          hydra.ext.java.serde.Serde.writeTypeIdentifier(id)));
      }
    });
    return hydra.serialization.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
        hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(anns),
          () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
          () -> hydra.util.Maybe.just(hydra.serialization.Serialization.commaSep(
            hydra.serialization.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.ext.java.serde.Serde::writeAnnotation,
              anns)))),
        hydra.util.Maybe.just(qualifiedId))))),
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(args),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.angleBracesList(
          hydra.serialization.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.java.serde.Serde::writeTypeArgument,
            args)))))));
  }
  
  static hydra.ast.Expr writeCompilationUnit(hydra.ext.java.syntax.CompilationUnit u) {
    return (u).accept(new hydra.ext.java.syntax.CompilationUnit.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.CompilationUnit.Ordinary ocu) {
        java.util.List<hydra.ext.java.syntax.ImportDeclaration> imports = ((ocu).value).imports;
        hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> importsSec = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(imports),
          () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
          () -> hydra.util.Maybe.just(hydra.serialization.Serialization.newlineSep(hydra.lib.lists.Map.apply(
            hydra.ext.java.serde.Serde::writeImportDeclaration,
            imports)))));
        hydra.util.Maybe<hydra.ext.java.syntax.PackageDeclaration> mpkg = ((ocu).value).package_;
        hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> pkgSec = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
          hydra.ext.java.serde.Serde::writePackageDeclaration,
          mpkg));
        java.util.List<hydra.ext.java.syntax.TypeDeclarationWithComments> types = ((ocu).value).types;
        hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> typesSec = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(types),
          () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
          () -> hydra.util.Maybe.just(hydra.serialization.Serialization.doubleNewlineSep(hydra.lib.lists.Map.apply(
            hydra.ext.java.serde.Serde::writeTypeDeclarationWithComments,
            types)))));
        hydra.util.Maybe<hydra.ast.Expr> warning = hydra.util.Maybe.just(hydra.ext.java.serde.Serde.singleLineComment(hydra.constants.Constants.warningAutoGeneratedFile()));
        return hydra.serialization.Serialization.doubleNewlineSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
          warning,
          pkgSec.get(),
          importsSec.get(),
          typesSec.get())));
      }
    });
  }
  
  static hydra.ast.Expr writeConditionalAndExpression(hydra.ext.java.syntax.ConditionalAndExpression cae) {
    return hydra.serialization.Serialization.infixWsList(
      "&&",
      hydra.lib.lists.Map.apply(
        hydra.ext.java.serde.Serde::writeInclusiveOrExpression,
        (cae).value));
  }
  
  static hydra.ast.Expr writeConditionalExpression(hydra.ext.java.syntax.ConditionalExpression c) {
    return (c).accept(new hydra.ext.java.syntax.ConditionalExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ConditionalExpression.Simple co) {
        return hydra.ext.java.serde.Serde.writeConditionalOrExpression((co).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ConditionalExpression.TernaryCond tc) {
        return hydra.ext.java.serde.Serde.writeConditionalExpression_TernaryCond((tc).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ConditionalExpression.TernaryLambda tl) {
        return hydra.ext.java.serde.Serde.writeConditionalExpression_TernaryLambda((tl).value);
      }
    });
  }
  
  static <T0> hydra.ast.Expr writeConditionalExpression_TernaryCond(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:ConditionalExpression_TernaryCond");
  }
  
  static <T0> hydra.ast.Expr writeConditionalExpression_TernaryLambda(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:ConditionalExpression_TernaryLambda");
  }
  
  static hydra.ast.Expr writeConditionalOrExpression(hydra.ext.java.syntax.ConditionalOrExpression coe) {
    return hydra.serialization.Serialization.infixWsList(
      "||",
      hydra.lib.lists.Map.apply(
        hydra.ext.java.serde.Serde::writeConditionalAndExpression,
        (coe).value));
  }
  
  static hydra.ast.Expr writeConstantDeclaration(hydra.ext.java.syntax.ConstantDeclaration cd) {
    java.util.List<hydra.ext.java.syntax.ConstantModifier> mods = (cd).modifiers;
    hydra.ext.java.syntax.UnannType typ = (cd).type;
    java.util.List<hydra.ext.java.syntax.VariableDeclarator> vars = (cd).variables;
    return hydra.serialization.Serialization.withSemi(hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          p0 -> hydra.ext.java.serde.Serde.<hydra.ext.java.syntax.ConstantModifier>writeConstantModifier(p0),
          mods)))),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeUnannType(typ)),
      hydra.util.Maybe.just(hydra.serialization.Serialization.commaSep(
        hydra.serialization.Serialization.inlineStyle(),
        hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writeVariableDeclarator,
          vars)))))));
  }
  
  static <T0> hydra.ast.Expr writeConstantModifier(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:ConstantModifier");
  }
  
  static hydra.ast.Expr writeConstructorBody(hydra.ext.java.syntax.ConstructorBody cb) {
    hydra.util.Maybe<hydra.ext.java.syntax.ExplicitConstructorInvocation> minvoc = (cb).invocation;
    java.util.List<hydra.ext.java.syntax.BlockStatement> stmts = (cb).statements;
    return hydra.serialization.Serialization.curlyBlock(
      hydra.serialization.Serialization.fullBlockStyle(),
      hydra.serialization.Serialization.doubleNewlineSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
        hydra.lib.maybes.Map.apply(
          p0 -> hydra.ext.java.serde.Serde.<hydra.ext.java.syntax.ExplicitConstructorInvocation>writeExplicitConstructorInvocation(p0),
          minvoc),
        hydra.util.Maybe.just(hydra.serialization.Serialization.newlineSep(hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writeBlockStatement,
          stmts)))))));
  }
  
  static hydra.ast.Expr writeConstructorDeclaration(hydra.ext.java.syntax.ConstructorDeclaration cd) {
    hydra.ext.java.syntax.ConstructorBody body = (cd).body;
    hydra.ext.java.syntax.ConstructorDeclarator cons = (cd).constructor;
    java.util.List<hydra.ext.java.syntax.ConstructorModifier> mods = (cd).modifiers;
    hydra.util.Maybe<hydra.ext.java.syntax.Throws> mthrows = (cd).throws_;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writeConstructorModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeConstructorDeclarator(cons)),
      hydra.lib.maybes.Map.apply(
        p0 -> hydra.ext.java.serde.Serde.<hydra.ext.java.syntax.Throws>writeThrows(p0),
        mthrows),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeConstructorBody(body)))));
  }
  
  static hydra.ast.Expr writeConstructorDeclarator(hydra.ext.java.syntax.ConstructorDeclarator cd) {
    java.util.List<hydra.ext.java.syntax.FormalParameter> fparams = (cd).formalParameters;
    hydra.ext.java.syntax.SimpleTypeName name = (cd).name;
    java.util.List<hydra.ext.java.syntax.TypeParameter> tparams = (cd).parameters;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(tparams),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.angleBracesList(
          hydra.serialization.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.java.serde.Serde::writeTypeParameter,
            tparams)))),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeSimpleTypeName(name)),
      hydra.util.Maybe.just(hydra.serialization.Serialization.parenList(
        false,
        hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writeFormalParameter,
          fparams))))));
  }
  
  static hydra.ast.Expr writeConstructorModifier(hydra.ext.java.syntax.ConstructorModifier m) {
    return (m).accept(new hydra.ext.java.syntax.ConstructorModifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ConstructorModifier.Annotation ann) {
        return hydra.ext.java.serde.Serde.writeAnnotation((ann).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ConstructorModifier.Public ignored) {
        return hydra.serialization.Serialization.cst("public");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ConstructorModifier.Protected ignored) {
        return hydra.serialization.Serialization.cst("protected");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ConstructorModifier.Private ignored) {
        return hydra.serialization.Serialization.cst("private");
      }
    });
  }
  
  static hydra.ast.Expr writeContinueStatement(hydra.ext.java.syntax.ContinueStatement cs) {
    hydra.util.Maybe<hydra.ext.java.syntax.Identifier> mlabel = (cs).value;
    return hydra.serialization.Serialization.withSemi(hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.util.Maybe.just(hydra.serialization.Serialization.cst("continue")),
      hydra.lib.maybes.Map.apply(
        hydra.ext.java.serde.Serde::writeIdentifier,
        mlabel)))));
  }
  
  static hydra.ast.Expr writeDims(hydra.ext.java.syntax.Dims d) {
    return hydra.serialization.Serialization.noSep(hydra.lib.lists.Map.apply(
      (java.util.function.Function<java.util.List<hydra.ext.java.syntax.Annotation>, hydra.ast.Expr>) (ignored -> hydra.serialization.Serialization.cst("[]")),
      (d).value));
  }
  
  static <T0> hydra.ast.Expr writeDoStatement(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:DoStatement");
  }
  
  static hydra.ast.Expr writeElementValue(hydra.ext.java.syntax.ElementValue ev) {
    return (ev).accept(new hydra.ext.java.syntax.ElementValue.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ElementValue.ConditionalExpression c) {
        return hydra.ext.java.serde.Serde.writeConditionalExpression((c).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ElementValue.ElementValueArrayInitializer evai) {
        return hydra.serialization.Serialization.commaSep(
          hydra.serialization.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.java.serde.Serde::writeElementValue,
            ((evai).value).value));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ElementValue.Annotation ann) {
        return hydra.ext.java.serde.Serde.writeAnnotation((ann).value);
      }
    });
  }
  
  static hydra.ast.Expr writeElementValuePair(hydra.ext.java.syntax.ElementValuePair evp) {
    hydra.ext.java.syntax.Identifier k = (evp).key;
    hydra.ext.java.syntax.ElementValue v = (evp).value;
    return hydra.serialization.Serialization.infixWs(
      "=",
      hydra.ext.java.serde.Serde.writeIdentifier(k),
      hydra.ext.java.serde.Serde.writeElementValue(v));
  }
  
  static <T0> hydra.ast.Expr writeEnumDeclaration(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:EnumDeclaration");
  }
  
  static hydra.ast.Expr writeEqualityExpression(hydra.ext.java.syntax.EqualityExpression e) {
    return (e).accept(new hydra.ext.java.syntax.EqualityExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.EqualityExpression.Unary r) {
        return hydra.ext.java.serde.Serde.writeRelationalExpression((r).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.EqualityExpression.Equal b) {
        return hydra.serialization.Serialization.infixWs(
          "==",
          hydra.ext.java.serde.Serde.writeEqualityExpression(((b).value).lhs),
          hydra.ext.java.serde.Serde.writeRelationalExpression(((b).value).rhs));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.EqualityExpression.NotEqual b) {
        return hydra.serialization.Serialization.infixWs(
          "!=",
          hydra.ext.java.serde.Serde.writeEqualityExpression(((b).value).lhs),
          hydra.ext.java.serde.Serde.writeRelationalExpression(((b).value).rhs));
      }
    });
  }
  
  static hydra.ast.Expr writeExclusiveOrExpression(hydra.ext.java.syntax.ExclusiveOrExpression eoe) {
    return hydra.serialization.Serialization.infixWsList(
      "^",
      hydra.lib.lists.Map.apply(
        hydra.ext.java.serde.Serde::writeAndExpression,
        (eoe).value));
  }
  
  static <T0> hydra.ast.Expr writeExplicitConstructorInvocation(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:ExplicitConstructorInvocation");
  }
  
  static hydra.ast.Expr writeExpression(hydra.ext.java.syntax.Expression e) {
    return (e).accept(new hydra.ext.java.syntax.Expression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Expression.Lambda l) {
        return hydra.ext.java.serde.Serde.writeLambdaExpression((l).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Expression.Assignment a) {
        return hydra.ext.java.serde.Serde.writeAssignmentExpression((a).value);
      }
    });
  }
  
  static hydra.ast.Expr writeExpressionName(hydra.ext.java.syntax.ExpressionName en) {
    hydra.ext.java.syntax.Identifier id = (en).identifier;
    hydra.util.Maybe<hydra.ext.java.syntax.AmbiguousName> mqual = (en).qualifier;
    return hydra.serialization.Serialization.dotSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.maybes.Map.apply(
        hydra.ext.java.serde.Serde::writeAmbiguousName,
        mqual),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeIdentifier(id)))));
  }
  
  static hydra.ast.Expr writeExpressionStatement(hydra.ext.java.syntax.ExpressionStatement es) {
    return hydra.serialization.Serialization.withSemi(hydra.ext.java.serde.Serde.writeStatementExpression((es).value));
  }
  
  static hydra.ast.Expr writeFieldAccess(hydra.ext.java.syntax.FieldAccess fa) {
    hydra.ext.java.syntax.Identifier id = (fa).identifier;
    hydra.ext.java.syntax.FieldAccess_Qualifier qual = (fa).qualifier;
    return (qual).accept(new hydra.ext.java.syntax.FieldAccess_Qualifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.FieldAccess_Qualifier.Primary p) {
        return hydra.serialization.Serialization.dotSep(java.util.List.of(
          hydra.ext.java.serde.Serde.writePrimary((p).value),
          hydra.ext.java.serde.Serde.writeIdentifier(id)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.FieldAccess_Qualifier.Super ignored) {
        return hydra.serialization.Serialization.dotSep(java.util.List.of(
          hydra.serialization.Serialization.cst("super"),
          hydra.ext.java.serde.Serde.writeIdentifier(id)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.FieldAccess_Qualifier.Typed tn) {
        return hydra.serialization.Serialization.dotSep(java.util.List.of(
          hydra.ext.java.serde.Serde.writeTypeName((tn).value),
          hydra.serialization.Serialization.cst("super"),
          hydra.ext.java.serde.Serde.writeIdentifier(id)));
      }
    });
  }
  
  static hydra.ast.Expr writeFieldDeclaration(hydra.ext.java.syntax.FieldDeclaration fd) {
    java.util.List<hydra.ext.java.syntax.FieldModifier> mods = (fd).modifiers;
    hydra.ext.java.syntax.UnannType typ = (fd).unannType;
    java.util.List<hydra.ext.java.syntax.VariableDeclarator> vars = (fd).variableDeclarators;
    return hydra.serialization.Serialization.withSemi(hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writeFieldModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeUnannType(typ)),
      hydra.util.Maybe.just(hydra.serialization.Serialization.commaSep(
        hydra.serialization.Serialization.inlineStyle(),
        hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writeVariableDeclarator,
          vars)))))));
  }
  
  static hydra.ast.Expr writeFieldModifier(hydra.ext.java.syntax.FieldModifier m) {
    return (m).accept(new hydra.ext.java.syntax.FieldModifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.FieldModifier.Annotation ann) {
        return hydra.ext.java.serde.Serde.writeAnnotation((ann).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.FieldModifier.Public ignored) {
        return hydra.serialization.Serialization.cst("public");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.FieldModifier.Protected ignored) {
        return hydra.serialization.Serialization.cst("protected");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.FieldModifier.Private ignored) {
        return hydra.serialization.Serialization.cst("private");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.FieldModifier.Static ignored) {
        return hydra.serialization.Serialization.cst("static");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.FieldModifier.Final ignored) {
        return hydra.serialization.Serialization.cst("final");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.FieldModifier.Transient ignored) {
        return hydra.serialization.Serialization.cst("transient");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.FieldModifier.Volatile ignored) {
        return hydra.serialization.Serialization.cst("volatile");
      }
    });
  }
  
  static hydra.ast.Expr writeFloatingPointLiteral(hydra.ext.java.syntax.FloatingPointLiteral fl) {
    return hydra.serialization.Serialization.cst(hydra.lib.literals.ShowBigfloat.apply((fl).value));
  }
  
  static hydra.ast.Expr writeFloatingPointType(hydra.ext.java.syntax.FloatingPointType ft) {
    return (ft).accept(new hydra.ext.java.syntax.FloatingPointType.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.FloatingPointType.Float_ ignored) {
        return hydra.serialization.Serialization.cst("float");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.FloatingPointType.Double_ ignored) {
        return hydra.serialization.Serialization.cst("double");
      }
    });
  }
  
  static <T0> hydra.ast.Expr writeForStatement(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:ForStatement");
  }
  
  static hydra.ast.Expr writeFormalParameter(hydra.ext.java.syntax.FormalParameter p) {
    return (p).accept(new hydra.ext.java.syntax.FormalParameter.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.FormalParameter.Simple s) {
        return hydra.ext.java.serde.Serde.writeFormalParameter_Simple((s).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.FormalParameter.VariableArity v) {
        return hydra.ext.java.serde.Serde.writeVariableArityParameter((v).value);
      }
    });
  }
  
  static hydra.ast.Expr writeFormalParameter_Simple(hydra.ext.java.syntax.FormalParameter_Simple fps) {
    hydra.ext.java.syntax.VariableDeclaratorId id = (fps).id;
    java.util.List<hydra.ext.java.syntax.VariableModifier> mods = (fps).modifiers;
    hydra.ext.java.syntax.UnannType typ = (fps).type;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writeVariableModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeUnannType(typ)),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeVariableDeclaratorId(id)))));
  }
  
  static hydra.ast.Expr writeIdentifier(hydra.ext.java.syntax.Identifier id) {
    return hydra.serialization.Serialization.cst((id).value);
  }
  
  static hydra.ast.Expr writeIfThenStatement(hydra.ext.java.syntax.IfThenStatement its) {
    hydra.ext.java.syntax.Expression cond = (its).expression;
    hydra.ext.java.syntax.Statement thn = (its).statement;
    return hydra.serialization.Serialization.spaceSep(java.util.List.of(
      hydra.serialization.Serialization.cst("if"),
      hydra.serialization.Serialization.parenList(
        false,
        java.util.List.of(hydra.ext.java.serde.Serde.writeExpression(cond))),
      hydra.serialization.Serialization.curlyBlock(
        hydra.serialization.Serialization.fullBlockStyle(),
        hydra.ext.java.serde.Serde.writeStatement(thn))));
  }
  
  static <T0> hydra.ast.Expr writeIfThenElseStatement(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:IfThenElseStatement");
  }
  
  static hydra.ast.Expr writeImportDeclaration(hydra.ext.java.syntax.ImportDeclaration imp) {
    return (imp).accept(new hydra.ext.java.syntax.ImportDeclaration.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ImportDeclaration.SingleType st) {
        return hydra.serialization.Serialization.withSemi(hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.serialization.Serialization.cst("import"),
          hydra.ext.java.serde.Serde.writeTypeName(((st).value).value))));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ImportDeclaration.TypeImportOnDemand ignored) {
        return hydra.serialization.Serialization.cst("STUB:ImportDeclarationTypeImportOnDemand");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ImportDeclaration.SingleStaticImport ignored) {
        return hydra.serialization.Serialization.cst("STUB:ImportDeclarationSingleStaticImport");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ImportDeclaration.StaticImportOnDemand ignored) {
        return hydra.serialization.Serialization.cst("STUB:ImportDeclarationStaticImportOnDemand");
      }
    });
  }
  
  static hydra.ast.Expr writeInclusiveOrExpression(hydra.ext.java.syntax.InclusiveOrExpression ioe) {
    return hydra.serialization.Serialization.infixWsList(
      "|",
      hydra.lib.lists.Map.apply(
        hydra.ext.java.serde.Serde::writeExclusiveOrExpression,
        (ioe).value));
  }
  
  static <T0> hydra.ast.Expr writeInstanceInitializer(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:InstanceInitializer");
  }
  
  static hydra.ast.Expr writeIntegerLiteral(hydra.ext.java.syntax.IntegerLiteral il) {
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
    return hydra.serialization.Serialization.cst(hydra.lib.strings.Cat2.apply(
      hydra.lib.literals.ShowBigint.apply(i),
      suffix.get()));
  }
  
  static hydra.ast.Expr writeIntegralType(hydra.ext.java.syntax.IntegralType t) {
    return (t).accept(new hydra.ext.java.syntax.IntegralType.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.IntegralType.Byte_ ignored) {
        return hydra.serialization.Serialization.cst("byte");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.IntegralType.Short_ ignored) {
        return hydra.serialization.Serialization.cst("short");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.IntegralType.Int ignored) {
        return hydra.serialization.Serialization.cst("int");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.IntegralType.Long_ ignored) {
        return hydra.serialization.Serialization.cst("long");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.IntegralType.Char ignored) {
        return hydra.serialization.Serialization.cst("char");
      }
    });
  }
  
  static hydra.ast.Expr writeInterfaceBody(hydra.ext.java.syntax.InterfaceBody ib) {
    return hydra.serialization.Serialization.curlyBlock(
      hydra.serialization.Serialization.fullBlockStyle(),
      hydra.serialization.Serialization.doubleNewlineSep(hydra.lib.lists.Map.apply(
        hydra.ext.java.serde.Serde::writeInterfaceMemberDeclaration,
        (ib).value)));
  }
  
  static hydra.ast.Expr writeInterfaceDeclaration(hydra.ext.java.syntax.InterfaceDeclaration d) {
    return (d).accept(new hydra.ext.java.syntax.InterfaceDeclaration.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceDeclaration.NormalInterface n) {
        return hydra.ext.java.serde.Serde.writeNormalInterfaceDeclaration((n).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceDeclaration.AnnotationType a) {
        return hydra.ext.java.serde.Serde.writeAnnotationTypeDeclaration((a).value);
      }
    });
  }
  
  static hydra.ast.Expr writeInterfaceMemberDeclaration(hydra.ext.java.syntax.InterfaceMemberDeclaration d) {
    return (d).accept(new hydra.ext.java.syntax.InterfaceMemberDeclaration.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceMemberDeclaration.Constant c) {
        return hydra.ext.java.serde.Serde.writeConstantDeclaration((c).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceMemberDeclaration.InterfaceMethod im) {
        return hydra.ext.java.serde.Serde.writeInterfaceMethodDeclaration((im).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceMemberDeclaration.Class_ cd) {
        return hydra.ext.java.serde.Serde.writeClassDeclaration((cd).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceMemberDeclaration.Interface id) {
        return hydra.ext.java.serde.Serde.writeInterfaceDeclaration((id).value);
      }
    });
  }
  
  static hydra.ast.Expr writeInterfaceMethodDeclaration(hydra.ext.java.syntax.InterfaceMethodDeclaration imd) {
    hydra.ext.java.syntax.MethodBody body = (imd).body;
    hydra.ext.java.syntax.MethodHeader header = (imd).header;
    java.util.List<hydra.ext.java.syntax.InterfaceMethodModifier> mods = (imd).modifiers;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writeInterfaceMethodModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeMethodHeader(header)),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeMethodBody(body)))));
  }
  
  static hydra.ast.Expr writeInterfaceMethodModifier(hydra.ext.java.syntax.InterfaceMethodModifier m) {
    return (m).accept(new hydra.ext.java.syntax.InterfaceMethodModifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceMethodModifier.Annotation a) {
        return hydra.ext.java.serde.Serde.writeAnnotation((a).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceMethodModifier.Public ignored) {
        return hydra.serialization.Serialization.cst("public");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceMethodModifier.Private ignored) {
        return hydra.serialization.Serialization.cst("private");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceMethodModifier.Abstract ignored) {
        return hydra.serialization.Serialization.cst("abstract");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceMethodModifier.Default ignored) {
        return hydra.serialization.Serialization.cst("default");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceMethodModifier.Static ignored) {
        return hydra.serialization.Serialization.cst("static");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceMethodModifier.Strictfp ignored) {
        return hydra.serialization.Serialization.cst("strictfp");
      }
    });
  }
  
  static hydra.ast.Expr writeInterfaceModifier(hydra.ext.java.syntax.InterfaceModifier m) {
    return (m).accept(new hydra.ext.java.syntax.InterfaceModifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceModifier.Annotation a) {
        return hydra.ext.java.serde.Serde.writeAnnotation((a).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceModifier.Public ignored) {
        return hydra.serialization.Serialization.cst("public");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceModifier.Protected ignored) {
        return hydra.serialization.Serialization.cst("protected");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceModifier.Private ignored) {
        return hydra.serialization.Serialization.cst("private");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceModifier.Abstract ignored) {
        return hydra.serialization.Serialization.cst("abstract");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceModifier.Static ignored) {
        return hydra.serialization.Serialization.cst("static");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.InterfaceModifier.Strictfb ignored) {
        return hydra.serialization.Serialization.cst("strictfb");
      }
    });
  }
  
  static hydra.ast.Expr writeInterfaceType(hydra.ext.java.syntax.InterfaceType it) {
    return hydra.ext.java.serde.Serde.writeClassType((it).value);
  }
  
  static <T0> hydra.ast.Expr writeLabeledStatement(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:LabeledStatement");
  }
  
  static hydra.ast.Expr writeLambdaBody(hydra.ext.java.syntax.LambdaBody b) {
    return (b).accept(new hydra.ext.java.syntax.LambdaBody.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.LambdaBody.Expression e) {
        return hydra.ext.java.serde.Serde.writeExpression((e).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.LambdaBody.Block b2) {
        return hydra.ext.java.serde.Serde.writeBlock((b2).value);
      }
    });
  }
  
  static hydra.ast.Expr writeLambdaExpression(hydra.ext.java.syntax.LambdaExpression le) {
    hydra.ext.java.syntax.LambdaBody body = (le).body;
    hydra.ext.java.syntax.LambdaParameters params = (le).parameters;
    return hydra.serialization.Serialization.infixWs(
      "->",
      hydra.ext.java.serde.Serde.writeLambdaParameters(params),
      hydra.ext.java.serde.Serde.writeLambdaBody(body));
  }
  
  static hydra.ast.Expr writeLambdaParameters(hydra.ext.java.syntax.LambdaParameters p) {
    return (p).accept(new hydra.ext.java.syntax.LambdaParameters.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.LambdaParameters.Tuple l) {
        return hydra.serialization.Serialization.parenList(
          false,
          hydra.lib.lists.Map.apply(
            hydra.ext.java.serde.Serde::writeLambdaParameters,
            (l).value));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.LambdaParameters.Single id) {
        return hydra.ext.java.serde.Serde.writeIdentifier((id).value);
      }
    });
  }
  
  static hydra.ast.Expr writeLeftHandSide(hydra.ext.java.syntax.LeftHandSide lhs2) {
    return (lhs2).accept(new hydra.ext.java.syntax.LeftHandSide.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.LeftHandSide.ExpressionName en) {
        return hydra.ext.java.serde.Serde.writeExpressionName((en).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.LeftHandSide.FieldAccess fa) {
        return hydra.ext.java.serde.Serde.writeFieldAccess((fa).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.LeftHandSide.ArrayAccess aa) {
        return hydra.ext.java.serde.Serde.writeArrayAccess((aa).value);
      }
    });
  }
  
  static hydra.ast.Expr writeLiteral(hydra.ext.java.syntax.Literal l) {
    return (l).accept(new hydra.ext.java.syntax.Literal.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Literal.Null ignored) {
        return hydra.serialization.Serialization.cst("null");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Literal.Integer_ il) {
        return hydra.ext.java.serde.Serde.writeIntegerLiteral((il).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Literal.FloatingPoint fl) {
        return hydra.ext.java.serde.Serde.writeFloatingPointLiteral((fl).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Literal.Boolean_ b) {
        return hydra.serialization.Serialization.cst(hydra.lib.logic.IfElse.lazy(
          (b).value,
          () -> "true",
          () -> "false"));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Literal.Character_ c) {
        Integer ci = hydra.lib.literals.BigintToInt32.apply(hydra.lib.literals.Uint16ToBigint.apply((c).value));
        return hydra.serialization.Serialization.cst(hydra.lib.strings.Cat2.apply(
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
                        () -> hydra.lib.strings.FromList.apply(java.util.List.of(ci)),
                        () -> hydra.ext.java.serde.Serde.javaUnicodeEscape(ci))))))),
            "'")));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Literal.String_ sl) {
        return hydra.ext.java.serde.Serde.writeStringLiteral((sl).value);
      }
    });
  }
  
  static hydra.ast.Expr writeLocalVariableDeclaration(hydra.ext.java.syntax.LocalVariableDeclaration lvd) {
    java.util.List<hydra.ext.java.syntax.VariableDeclarator> decls = (lvd).declarators;
    java.util.List<hydra.ext.java.syntax.VariableModifier> mods = (lvd).modifiers;
    hydra.ext.java.syntax.LocalVariableType t = (lvd).type;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writeVariableModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeLocalName(t)),
      hydra.util.Maybe.just(hydra.serialization.Serialization.commaSep(
        hydra.serialization.Serialization.inlineStyle(),
        hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writeVariableDeclarator,
          decls))))));
  }
  
  static hydra.ast.Expr writeLocalVariableDeclarationStatement(hydra.ext.java.syntax.LocalVariableDeclarationStatement lvds) {
    return hydra.serialization.Serialization.withSemi(hydra.ext.java.serde.Serde.writeLocalVariableDeclaration((lvds).value));
  }
  
  static hydra.ast.Expr writeLocalName(hydra.ext.java.syntax.LocalVariableType t) {
    return (t).accept(new hydra.ext.java.syntax.LocalVariableType.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.LocalVariableType.Type ut) {
        return hydra.ext.java.serde.Serde.writeUnannType((ut).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.LocalVariableType.Var ignored) {
        return hydra.serialization.Serialization.cst("var");
      }
    });
  }
  
  static hydra.ast.Expr writeMarkerAnnotation(hydra.ext.java.syntax.MarkerAnnotation ma) {
    return hydra.serialization.Serialization.prefix(
      "@",
      hydra.ext.java.serde.Serde.writeTypeName((ma).value));
  }
  
  static hydra.ast.Expr writeMethodBody(hydra.ext.java.syntax.MethodBody b) {
    return (b).accept(new hydra.ext.java.syntax.MethodBody.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.MethodBody.Block block) {
        return hydra.ext.java.serde.Serde.writeBlock((block).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.MethodBody.None ignored) {
        return hydra.serialization.Serialization.cst(";");
      }
    });
  }
  
  static hydra.ast.Expr writeMethodDeclaration(hydra.ext.java.syntax.MethodDeclaration md) {
    java.util.List<hydra.ext.java.syntax.Annotation> anns = (md).annotations;
    hydra.ext.java.syntax.MethodBody body = (md).body;
    hydra.ext.java.syntax.MethodHeader header = (md).header;
    java.util.List<hydra.ext.java.syntax.MethodModifier> mods = (md).modifiers;
    hydra.util.Lazy<hydra.ast.Expr> headerAndBody = new hydra.util.Lazy<>(() -> hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writeMethodModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeMethodHeader(header)),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeMethodBody(body))))));
    return hydra.serialization.Serialization.newlineSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(anns),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.newlineSep(hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writeAnnotation,
          anns)))),
      hydra.util.Maybe.just(headerAndBody.get()))));
  }
  
  static hydra.ast.Expr writeMethodDeclarator(hydra.ext.java.syntax.MethodDeclarator md) {
    hydra.ext.java.syntax.Identifier id = (md).identifier;
    java.util.List<hydra.ext.java.syntax.FormalParameter> params = (md).formalParameters;
    return hydra.serialization.Serialization.noSep(java.util.List.of(
      hydra.ext.java.serde.Serde.writeIdentifier(id),
      hydra.serialization.Serialization.parenList(
        false,
        hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writeFormalParameter,
          params))));
  }
  
  static hydra.ast.Expr writeMethodHeader(hydra.ext.java.syntax.MethodHeader mh) {
    hydra.ext.java.syntax.MethodDeclarator decl = (mh).declarator;
    hydra.util.Maybe<hydra.ext.java.syntax.Throws> mthrows = (mh).throws_;
    java.util.List<hydra.ext.java.syntax.TypeParameter> params = (mh).parameters;
    hydra.ext.java.syntax.Result result = (mh).result;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(params),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.angleBracesList(
          hydra.serialization.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.java.serde.Serde::writeTypeParameter,
            params)))),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeResult(result)),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeMethodDeclarator(decl)),
      hydra.lib.maybes.Map.apply(
        p0 -> hydra.ext.java.serde.Serde.<hydra.ext.java.syntax.Throws>writeThrows(p0),
        mthrows))));
  }
  
  static hydra.ast.Expr writeMethodInvocation(hydra.ext.java.syntax.MethodInvocation mi) {
    java.util.List<hydra.ext.java.syntax.Expression> args = (mi).arguments;
    hydra.util.Lazy<hydra.ast.Expr> argSec = new hydra.util.Lazy<>(() -> hydra.serialization.Serialization.parenList(
      true,
      hydra.lib.lists.Map.apply(
        hydra.ext.java.serde.Serde::writeExpression,
        args)));
    hydra.ext.java.syntax.MethodInvocation_Header header = (mi).header;
    hydra.util.Lazy<hydra.ast.Expr> headerSec = new hydra.util.Lazy<>(() -> (header).accept(new hydra.ext.java.syntax.MethodInvocation_Header.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.MethodInvocation_Header.Simple mname) {
        return hydra.ext.java.serde.Serde.writeMethodName((mname).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.MethodInvocation_Header.Complex cx) {
        hydra.ext.java.syntax.Identifier cid = ((cx).value).identifier;
        hydra.ext.java.syntax.MethodInvocation_Variant cvar = ((cx).value).variant;
        java.util.List<hydra.ext.java.syntax.TypeArgument> targs = ((cx).value).typeArguments;
        hydra.util.Lazy<hydra.ast.Expr> idSec = new hydra.util.Lazy<>(() -> hydra.serialization.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
          hydra.lib.logic.IfElse.lazy(
            hydra.lib.lists.Null.apply(targs),
            () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
            () -> hydra.util.Maybe.just(hydra.serialization.Serialization.angleBracesList(
              hydra.serialization.Serialization.inlineStyle(),
              hydra.lib.lists.Map.apply(
                hydra.ext.java.serde.Serde::writeTypeArgument,
                targs)))),
          hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeIdentifier(cid))))));
        return (cvar).accept(new hydra.ext.java.syntax.MethodInvocation_Variant.PartialVisitor<>() {
          @Override
          public hydra.ast.Expr visit(hydra.ext.java.syntax.MethodInvocation_Variant.Type tname) {
            return hydra.serialization.Serialization.dotSep(java.util.List.of(
              hydra.ext.java.serde.Serde.writeTypeName((tname).value),
              idSec.get()));
          }
          
          @Override
          public hydra.ast.Expr visit(hydra.ext.java.syntax.MethodInvocation_Variant.Expression en) {
            return hydra.serialization.Serialization.dotSep(java.util.List.of(
              hydra.ext.java.serde.Serde.writeExpressionName((en).value),
              idSec.get()));
          }
          
          @Override
          public hydra.ast.Expr visit(hydra.ext.java.syntax.MethodInvocation_Variant.Primary p) {
            return hydra.serialization.Serialization.dotSep(java.util.List.of(
              hydra.ext.java.serde.Serde.writePrimary((p).value),
              idSec.get()));
          }
          
          @Override
          public hydra.ast.Expr visit(hydra.ext.java.syntax.MethodInvocation_Variant.Super ignored) {
            return hydra.serialization.Serialization.dotSep(java.util.List.of(
              hydra.serialization.Serialization.cst("super"),
              idSec.get()));
          }
          
          @Override
          public hydra.ast.Expr visit(hydra.ext.java.syntax.MethodInvocation_Variant.TypeSuper tname) {
            return hydra.serialization.Serialization.dotSep(java.util.List.of(
              hydra.ext.java.serde.Serde.writeTypeName((tname).value),
              hydra.serialization.Serialization.cst("super"),
              idSec.get()));
          }
        });
      }
    }));
    return hydra.serialization.Serialization.noSep(java.util.List.of(
      headerSec.get(),
      argSec.get()));
  }
  
  static hydra.ast.Expr writeMethodModifier(hydra.ext.java.syntax.MethodModifier m) {
    return (m).accept(new hydra.ext.java.syntax.MethodModifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.MethodModifier.Annotation ann) {
        return hydra.ext.java.serde.Serde.writeAnnotation((ann).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.MethodModifier.Public ignored) {
        return hydra.serialization.Serialization.cst("public");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.MethodModifier.Protected ignored) {
        return hydra.serialization.Serialization.cst("protected");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.MethodModifier.Private ignored) {
        return hydra.serialization.Serialization.cst("private");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.MethodModifier.Abstract ignored) {
        return hydra.serialization.Serialization.cst("abstract");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.MethodModifier.Final ignored) {
        return hydra.serialization.Serialization.cst("final");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.MethodModifier.Synchronized ignored) {
        return hydra.serialization.Serialization.cst("synchronized");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.MethodModifier.Native ignored) {
        return hydra.serialization.Serialization.cst("native");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.MethodModifier.Strictfb ignored) {
        return hydra.serialization.Serialization.cst("strictfb");
      }
    });
  }
  
  static hydra.ast.Expr writeMethodName(hydra.ext.java.syntax.MethodName mn) {
    return hydra.ext.java.serde.Serde.writeIdentifier((mn).value);
  }
  
  static <T0> hydra.ast.Expr writeMethodReference(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:MethodReference");
  }
  
  static hydra.ast.Expr writeMultiplicativeExpression(hydra.ext.java.syntax.MultiplicativeExpression e) {
    return (e).accept(new hydra.ext.java.syntax.MultiplicativeExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.MultiplicativeExpression.Unary u) {
        return hydra.ext.java.serde.Serde.writeUnaryExpression((u).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.MultiplicativeExpression.Times b) {
        return hydra.serialization.Serialization.infixWs(
          "*",
          hydra.ext.java.serde.Serde.writeMultiplicativeExpression(((b).value).lhs),
          hydra.ext.java.serde.Serde.writeUnaryExpression(((b).value).rhs));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.MultiplicativeExpression.Divide b) {
        return hydra.serialization.Serialization.infixWs(
          "/",
          hydra.ext.java.serde.Serde.writeMultiplicativeExpression(((b).value).lhs),
          hydra.ext.java.serde.Serde.writeUnaryExpression(((b).value).rhs));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.MultiplicativeExpression.Mod b) {
        return hydra.serialization.Serialization.infixWs(
          "%",
          hydra.ext.java.serde.Serde.writeMultiplicativeExpression(((b).value).lhs),
          hydra.ext.java.serde.Serde.writeUnaryExpression(((b).value).rhs));
      }
    });
  }
  
  static hydra.ast.Expr writeNormalAnnotation(hydra.ext.java.syntax.NormalAnnotation na) {
    java.util.List<hydra.ext.java.syntax.ElementValuePair> pairs = (na).pairs;
    hydra.ext.java.syntax.TypeName tname = (na).typeName;
    return hydra.serialization.Serialization.prefix(
      "@",
      hydra.serialization.Serialization.noSep(java.util.List.of(
        hydra.ext.java.serde.Serde.writeTypeName(tname),
        hydra.serialization.Serialization.commaSep(
          hydra.serialization.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.java.serde.Serde::writeElementValuePair,
            pairs)))));
  }
  
  static hydra.ast.Expr writeNormalClassDeclaration(hydra.ext.java.syntax.NormalClassDeclaration ncd) {
    hydra.ext.java.syntax.ClassBody body = (ncd).body;
    hydra.ext.java.syntax.TypeIdentifier id = (ncd).identifier;
    java.util.List<hydra.ext.java.syntax.ClassModifier> mods = (ncd).modifiers;
    hydra.util.Maybe<hydra.ext.java.syntax.ClassType> msuperc = (ncd).extends_;
    java.util.List<hydra.ext.java.syntax.InterfaceType> superi = (ncd).implements_;
    java.util.List<hydra.ext.java.syntax.TypeParameter> tparams = (ncd).parameters;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writeClassModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.serialization.Serialization.cst("class")),
      hydra.util.Maybe.just(hydra.serialization.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
        hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeTypeIdentifier(id)),
        hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(tparams),
          () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
          () -> hydra.util.Maybe.just(hydra.serialization.Serialization.angleBracesList(
            hydra.serialization.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.ext.java.serde.Serde::writeTypeParameter,
              tparams)))))))),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.ext.java.syntax.ClassType, hydra.ast.Expr>) (c -> hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.serialization.Serialization.cst("extends"),
          hydra.ext.java.serde.Serde.writeClassType(c)))),
        msuperc),
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(superi),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.serialization.Serialization.cst("implements"),
          hydra.serialization.Serialization.commaSep(
            hydra.serialization.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.ext.java.serde.Serde::writeInterfaceType,
              superi)))))),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeClassBody(body)))));
  }
  
  static hydra.ast.Expr writeNormalInterfaceDeclaration(hydra.ext.java.syntax.NormalInterfaceDeclaration nid) {
    hydra.ext.java.syntax.InterfaceBody body = (nid).body;
    java.util.List<hydra.ext.java.syntax.InterfaceType> extends_ = (nid).extends_;
    hydra.ext.java.syntax.TypeIdentifier id = (nid).identifier;
    java.util.List<hydra.ext.java.syntax.InterfaceModifier> mods = (nid).modifiers;
    java.util.List<hydra.ext.java.syntax.TypeParameter> tparams = (nid).parameters;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writeInterfaceModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.serialization.Serialization.cst("interface")),
      hydra.util.Maybe.just(hydra.serialization.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
        hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeTypeIdentifier(id)),
        hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(tparams),
          () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
          () -> hydra.util.Maybe.just(hydra.serialization.Serialization.angleBracesList(
            hydra.serialization.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.ext.java.serde.Serde::writeTypeParameter,
              tparams)))))))),
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(extends_),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.serialization.Serialization.cst("extends"),
          hydra.serialization.Serialization.commaSep(
            hydra.serialization.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.ext.java.serde.Serde::writeInterfaceType,
              extends_)))))),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeInterfaceBody(body)))));
  }
  
  static hydra.ast.Expr writeNumericType(hydra.ext.java.syntax.NumericType nt) {
    return (nt).accept(new hydra.ext.java.syntax.NumericType.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.NumericType.Integral it) {
        return hydra.ext.java.serde.Serde.writeIntegralType((it).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.NumericType.FloatingPoint ft) {
        return hydra.ext.java.serde.Serde.writeFloatingPointType((ft).value);
      }
    });
  }
  
  static hydra.ast.Expr writePackageDeclaration(hydra.ext.java.syntax.PackageDeclaration pd) {
    java.util.List<hydra.ext.java.syntax.Identifier> ids = (pd).identifiers;
    java.util.List<hydra.ext.java.syntax.PackageModifier> mods = (pd).modifiers;
    return hydra.serialization.Serialization.withSemi(hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writePackageModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(java.util.List.of(
        hydra.serialization.Serialization.cst("package"),
        hydra.serialization.Serialization.cst(hydra.lib.strings.Intercalate.apply(
          ".",
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.ext.java.syntax.Identifier, String>) (id -> (id).value),
            ids))))))))));
  }
  
  static hydra.ast.Expr writePackageName(hydra.ext.java.syntax.PackageName pn) {
    return hydra.serialization.Serialization.dotSep(hydra.lib.lists.Map.apply(
      hydra.ext.java.serde.Serde::writeIdentifier,
      (pn).value));
  }
  
  static hydra.ast.Expr writePackageOrTypeName(hydra.ext.java.syntax.PackageOrTypeName potn) {
    return hydra.serialization.Serialization.dotSep(hydra.lib.lists.Map.apply(
      hydra.ext.java.serde.Serde::writeIdentifier,
      (potn).value));
  }
  
  static hydra.ast.Expr writePackageModifier(hydra.ext.java.syntax.PackageModifier pm) {
    return hydra.ext.java.serde.Serde.writeAnnotation((pm).value);
  }
  
  static <T0> hydra.ast.Expr writePostDecrementExpression(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:PostDecrementExpression");
  }
  
  static <T0> hydra.ast.Expr writePostIncrementExpression(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:PostIncrementExpression");
  }
  
  static hydra.ast.Expr writePostfixExpression(hydra.ext.java.syntax.PostfixExpression e) {
    return (e).accept(new hydra.ext.java.syntax.PostfixExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.PostfixExpression.Primary p) {
        return hydra.ext.java.serde.Serde.writePrimary((p).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.PostfixExpression.Name en) {
        return hydra.ext.java.serde.Serde.writeExpressionName((en).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.PostfixExpression.PostIncrement pi) {
        return hydra.ext.java.serde.Serde.writePostIncrementExpression((pi).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.PostfixExpression.PostDecrement pd) {
        return hydra.ext.java.serde.Serde.writePostDecrementExpression((pd).value);
      }
    });
  }
  
  static <T0> hydra.ast.Expr writePreDecrementExpression(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:PreDecrementExpression");
  }
  
  static <T0> hydra.ast.Expr writePreIncrementExpression(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:PreIncrementExpression");
  }
  
  static hydra.ast.Expr writePrimary(hydra.ext.java.syntax.Primary p) {
    return (p).accept(new hydra.ext.java.syntax.Primary.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Primary.NoNewArray n) {
        return hydra.ext.java.serde.Serde.writePrimaryNoNewArray((n).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Primary.ArrayCreation a) {
        return hydra.ext.java.serde.Serde.writeArrayCreationExpression((a).value);
      }
    });
  }
  
  static hydra.ast.Expr writePrimaryNoNewArray(hydra.ext.java.syntax.PrimaryNoNewArray p) {
    return (p).accept(new hydra.ext.java.syntax.PrimaryNoNewArray.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.PrimaryNoNewArray.Literal l) {
        return hydra.ext.java.serde.Serde.writeLiteral((l).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.PrimaryNoNewArray.ClassLiteral cl) {
        return hydra.ext.java.serde.Serde.writeClassLiteral((cl).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.PrimaryNoNewArray.This ignored) {
        return hydra.serialization.Serialization.cst("this");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.PrimaryNoNewArray.DotThis n) {
        return hydra.serialization.Serialization.dotSep(java.util.List.of(
          hydra.ext.java.serde.Serde.writeTypeName((n).value),
          hydra.serialization.Serialization.cst("this")));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.PrimaryNoNewArray.Parens e) {
        return hydra.serialization.Serialization.parenList(
          false,
          java.util.List.of(hydra.ext.java.serde.Serde.writeExpression((e).value)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.PrimaryNoNewArray.ClassInstance ci) {
        return hydra.ext.java.serde.Serde.writeClassInstanceCreationExpression((ci).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.PrimaryNoNewArray.FieldAccess fa) {
        return hydra.ext.java.serde.Serde.writeFieldAccess((fa).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.PrimaryNoNewArray.ArrayAccess aa) {
        return hydra.ext.java.serde.Serde.writeArrayAccess((aa).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.PrimaryNoNewArray.MethodInvocation mi) {
        return hydra.ext.java.serde.Serde.writeMethodInvocation((mi).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.PrimaryNoNewArray.MethodReference mr) {
        return hydra.ext.java.serde.Serde.writeMethodReference((mr).value);
      }
    });
  }
  
  static hydra.ast.Expr writePrimitiveType(hydra.ext.java.syntax.PrimitiveType pt) {
    return (pt).accept(new hydra.ext.java.syntax.PrimitiveType.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.PrimitiveType.Numeric nt) {
        return hydra.ext.java.serde.Serde.writeNumericType((nt).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.PrimitiveType.Boolean_ ignored) {
        return hydra.serialization.Serialization.cst("boolean");
      }
    });
  }
  
  static hydra.ast.Expr writePrimitiveTypeWithAnnotations(hydra.ext.java.syntax.PrimitiveTypeWithAnnotations ptwa) {
    java.util.List<hydra.ext.java.syntax.Annotation> anns = (ptwa).annotations;
    hydra.ext.java.syntax.PrimitiveType pt = (ptwa).type;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(anns),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writeAnnotation,
          anns)))),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writePrimitiveType(pt)))));
  }
  
  static <T0> hydra.ast.Expr writeReceiverParameter(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:ReceiverParameter");
  }
  
  static hydra.ast.Expr writeReferenceType(hydra.ext.java.syntax.ReferenceType rt) {
    return (rt).accept(new hydra.ext.java.syntax.ReferenceType.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ReferenceType.ClassOrInterface cit) {
        return hydra.ext.java.serde.Serde.writeClassOrInterfaceType((cit).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ReferenceType.Variable v) {
        return hydra.ext.java.serde.Serde.writeTypeVariable((v).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ReferenceType.Array at) {
        return hydra.ext.java.serde.Serde.writeArrayType((at).value);
      }
    });
  }
  
  static hydra.ast.Expr writeRelationalExpression(hydra.ext.java.syntax.RelationalExpression e) {
    return (e).accept(new hydra.ext.java.syntax.RelationalExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.RelationalExpression.Simple s) {
        return hydra.ext.java.serde.Serde.writeShiftExpression((s).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.RelationalExpression.LessThan lt) {
        return hydra.ext.java.serde.Serde.writeRelationalExpression_LessThan((lt).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.RelationalExpression.GreaterThan gt) {
        return hydra.ext.java.serde.Serde.writeRelationalExpression_GreaterThan((gt).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.RelationalExpression.LessThanEqual lte) {
        return hydra.ext.java.serde.Serde.writeRelationalExpression_LessThanEqual((lte).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.RelationalExpression.GreaterThanEqual gte) {
        return hydra.ext.java.serde.Serde.writeRelationalExpression_GreaterThanEqual((gte).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.RelationalExpression.Instanceof i) {
        return hydra.ext.java.serde.Serde.writeRelationalExpression_InstanceOf((i).value);
      }
    });
  }
  
  static hydra.ast.Expr writeRelationalExpression_GreaterThan(hydra.ext.java.syntax.RelationalExpression_GreaterThan gt) {
    return hydra.serialization.Serialization.infixWs(
      ">",
      hydra.ext.java.serde.Serde.writeRelationalExpression((gt).lhs),
      hydra.ext.java.serde.Serde.writeShiftExpression((gt).rhs));
  }
  
  static hydra.ast.Expr writeRelationalExpression_GreaterThanEqual(hydra.ext.java.syntax.RelationalExpression_GreaterThanEqual gte) {
    return hydra.serialization.Serialization.infixWs(
      ">=",
      hydra.ext.java.serde.Serde.writeRelationalExpression((gte).lhs),
      hydra.ext.java.serde.Serde.writeShiftExpression((gte).rhs));
  }
  
  static hydra.ast.Expr writeRelationalExpression_InstanceOf(hydra.ext.java.syntax.RelationalExpression_InstanceOf io) {
    return hydra.serialization.Serialization.infixWs(
      "instanceof",
      hydra.ext.java.serde.Serde.writeRelationalExpression((io).lhs),
      hydra.ext.java.serde.Serde.writeReferenceType((io).rhs));
  }
  
  static hydra.ast.Expr writeRelationalExpression_LessThan(hydra.ext.java.syntax.RelationalExpression_LessThan lt) {
    return hydra.serialization.Serialization.infixWs(
      "<",
      hydra.ext.java.serde.Serde.writeRelationalExpression((lt).lhs),
      hydra.ext.java.serde.Serde.writeShiftExpression((lt).rhs));
  }
  
  static hydra.ast.Expr writeRelationalExpression_LessThanEqual(hydra.ext.java.syntax.RelationalExpression_LessThanEqual lte) {
    return hydra.serialization.Serialization.infixWs(
      "<=",
      hydra.ext.java.serde.Serde.writeRelationalExpression((lte).lhs),
      hydra.ext.java.serde.Serde.writeShiftExpression((lte).rhs));
  }
  
  static hydra.ast.Expr writeResult(hydra.ext.java.syntax.Result r) {
    return (r).accept(new hydra.ext.java.syntax.Result.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Result.Type t) {
        return hydra.ext.java.serde.Serde.writeUnannType((t).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Result.Void_ ignored) {
        return hydra.serialization.Serialization.cst("void");
      }
    });
  }
  
  static hydra.ast.Expr writeReturnStatement(hydra.ext.java.syntax.ReturnStatement rs) {
    hydra.util.Maybe<hydra.ext.java.syntax.Expression> mex = (rs).value;
    return hydra.serialization.Serialization.withSemi(hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.util.Maybe.just(hydra.serialization.Serialization.cst("return")),
      hydra.lib.maybes.Map.apply(
        hydra.ext.java.serde.Serde::writeExpression,
        mex)))));
  }
  
  static hydra.ast.Expr writeShiftExpression(hydra.ext.java.syntax.ShiftExpression e) {
    return (e).accept(new hydra.ext.java.syntax.ShiftExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ShiftExpression.Unary a) {
        return hydra.ext.java.serde.Serde.writeAdditiveExpression((a).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ShiftExpression.ShiftLeft b) {
        return hydra.serialization.Serialization.infixWs(
          "<<",
          hydra.ext.java.serde.Serde.writeShiftExpression(((b).value).lhs),
          hydra.ext.java.serde.Serde.writeAdditiveExpression(((b).value).rhs));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ShiftExpression.ShiftRight b) {
        return hydra.serialization.Serialization.infixWs(
          ">>",
          hydra.ext.java.serde.Serde.writeShiftExpression(((b).value).lhs),
          hydra.ext.java.serde.Serde.writeAdditiveExpression(((b).value).rhs));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.ShiftExpression.ShiftRightZeroFill b) {
        return hydra.serialization.Serialization.infixWs(
          ">>>",
          hydra.ext.java.serde.Serde.writeShiftExpression(((b).value).lhs),
          hydra.ext.java.serde.Serde.writeAdditiveExpression(((b).value).rhs));
      }
    });
  }
  
  static hydra.ast.Expr writeSimpleTypeName(hydra.ext.java.syntax.SimpleTypeName stn) {
    return hydra.ext.java.serde.Serde.writeTypeIdentifier((stn).value);
  }
  
  static hydra.ast.Expr writeSingleElementAnnotation(hydra.ext.java.syntax.SingleElementAnnotation sea) {
    hydra.util.Maybe<hydra.ext.java.syntax.ElementValue> mv = (sea).value;
    hydra.ext.java.syntax.TypeName tname = (sea).name;
    return hydra.lib.maybes.Maybe.apply(
      hydra.ext.java.serde.Serde.writeMarkerAnnotation(new hydra.ext.java.syntax.MarkerAnnotation(tname)),
      (java.util.function.Function<hydra.ext.java.syntax.ElementValue, hydra.ast.Expr>) (v -> hydra.serialization.Serialization.prefix(
        "@",
        hydra.serialization.Serialization.noSep(java.util.List.of(
          hydra.ext.java.serde.Serde.writeTypeName(tname),
          hydra.serialization.Serialization.parenList(
            false,
            java.util.List.of(hydra.ext.java.serde.Serde.writeElementValue(v))))))),
      mv);
  }
  
  static hydra.ast.Expr writeStatement(hydra.ext.java.syntax.Statement s) {
    return (s).accept(new hydra.ext.java.syntax.Statement.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Statement.WithoutTrailing s2) {
        return hydra.ext.java.serde.Serde.writeStatementWithoutTrailingSubstatement((s2).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Statement.Labeled l) {
        return hydra.ext.java.serde.Serde.writeLabeledStatement((l).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Statement.IfThen it) {
        return hydra.ext.java.serde.Serde.writeIfThenStatement((it).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Statement.IfThenElse ite) {
        return hydra.ext.java.serde.Serde.writeIfThenElseStatement((ite).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Statement.While w) {
        return hydra.ext.java.serde.Serde.writeWhileStatement((w).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Statement.For f) {
        return hydra.ext.java.serde.Serde.writeForStatement((f).value);
      }
    });
  }
  
  static hydra.ast.Expr writeStatementExpression(hydra.ext.java.syntax.StatementExpression e) {
    return (e).accept(new hydra.ext.java.syntax.StatementExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementExpression.Assignment a) {
        return hydra.ext.java.serde.Serde.writeAssignment((a).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementExpression.PreIncrement pi) {
        return hydra.ext.java.serde.Serde.writePreIncrementExpression((pi).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementExpression.PreDecrement pd) {
        return hydra.ext.java.serde.Serde.writePreDecrementExpression((pd).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementExpression.PostIncrement pi) {
        return hydra.ext.java.serde.Serde.writePostIncrementExpression((pi).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementExpression.PostDecrement pd) {
        return hydra.ext.java.serde.Serde.writePostDecrementExpression((pd).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementExpression.MethodInvocation m) {
        return hydra.ext.java.serde.Serde.writeMethodInvocation((m).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementExpression.ClassInstanceCreation cic) {
        return hydra.ext.java.serde.Serde.writeClassInstanceCreationExpression((cic).value);
      }
    });
  }
  
  static hydra.ast.Expr writeStatementWithoutTrailingSubstatement(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement s) {
    return (s).accept(new hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Block b) {
        return hydra.ext.java.serde.Serde.writeBlock((b).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Empty ignored) {
        return hydra.serialization.Serialization.cst(";");
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Expression e) {
        return hydra.ext.java.serde.Serde.writeExpressionStatement((e).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Assert a) {
        return hydra.ext.java.serde.Serde.writeAssertStatement((a).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Switch s2) {
        return hydra.ext.java.serde.Serde.writeSwitchStatement((s2).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Do d) {
        return hydra.ext.java.serde.Serde.writeDoStatement((d).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Break b) {
        return hydra.ext.java.serde.Serde.writeBreakStatement((b).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Continue c) {
        return hydra.ext.java.serde.Serde.writeContinueStatement((c).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Return r) {
        return hydra.ext.java.serde.Serde.writeReturnStatement((r).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Synchronized s2) {
        return hydra.ext.java.serde.Serde.writeSynchronizedStatement((s2).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Throw t) {
        return hydra.ext.java.serde.Serde.writeThrowStatement((t).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.StatementWithoutTrailingSubstatement.Try t) {
        return hydra.ext.java.serde.Serde.writeTryStatement((t).value);
      }
    });
  }
  
  static <T0> hydra.ast.Expr writeStaticInitializer(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:StaticInitializer");
  }
  
  static hydra.ast.Expr writeStringLiteral(hydra.ext.java.syntax.StringLiteral sl) {
    String s = (sl).value;
    return hydra.serialization.Serialization.cst(hydra.lib.strings.Cat2.apply(
      "\"",
      hydra.lib.strings.Cat2.apply(
        hydra.ext.java.serde.Serde.escapeJavaString(s),
        "\"")));
  }
  
  static <T0> hydra.ast.Expr writeSwitchStatement(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:SwitchStatement");
  }
  
  static <T0> hydra.ast.Expr writeSynchronizedStatement(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:SynchronizedStatement");
  }
  
  static hydra.ast.Expr writeThrowStatement(hydra.ext.java.syntax.ThrowStatement ts) {
    return hydra.serialization.Serialization.withSemi(hydra.serialization.Serialization.spaceSep(java.util.List.of(
      hydra.serialization.Serialization.cst("throw"),
      hydra.ext.java.serde.Serde.writeExpression((ts).value))));
  }
  
  static <T0> hydra.ast.Expr writeThrows(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:Throws");
  }
  
  static <T0> hydra.ast.Expr writeTryStatement(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:TryStatement");
  }
  
  static hydra.ast.Expr writeType(hydra.ext.java.syntax.Type t) {
    return (t).accept(new hydra.ext.java.syntax.Type.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Type.Primitive pt) {
        return hydra.ext.java.serde.Serde.writePrimitiveTypeWithAnnotations((pt).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.Type.Reference rt) {
        return hydra.ext.java.serde.Serde.writeReferenceType((rt).value);
      }
    });
  }
  
  static hydra.ast.Expr writeTypeArgument(hydra.ext.java.syntax.TypeArgument a) {
    return (a).accept(new hydra.ext.java.syntax.TypeArgument.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.TypeArgument.Reference rt) {
        return hydra.ext.java.serde.Serde.writeReferenceType((rt).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.TypeArgument.Wildcard w) {
        return hydra.ext.java.serde.Serde.writeWildcard((w).value);
      }
    });
  }
  
  static hydra.ast.Expr writeTypeArgumentsOrDiamond(hydra.ext.java.syntax.TypeArgumentsOrDiamond targs) {
    return (targs).accept(new hydra.ext.java.syntax.TypeArgumentsOrDiamond.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.TypeArgumentsOrDiamond.Arguments args) {
        return hydra.serialization.Serialization.angleBracesList(
          hydra.serialization.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.java.serde.Serde::writeTypeArgument,
            (args).value));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.TypeArgumentsOrDiamond.Diamond ignored) {
        return hydra.serialization.Serialization.cst("<>");
      }
    });
  }
  
  static hydra.ast.Expr writeTypeBound(hydra.ext.java.syntax.TypeBound b) {
    return (b).accept(new hydra.ext.java.syntax.TypeBound.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.TypeBound.Variable tv) {
        return hydra.ext.java.serde.Serde.writeTypeVariable((tv).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.TypeBound.ClassOrInterface ci) {
        java.util.List<hydra.ext.java.syntax.AdditionalBound> additional = ((ci).value).additional;
        hydra.ext.java.syntax.ClassOrInterfaceType cit = ((ci).value).type;
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(additional),
          () -> hydra.ext.java.serde.Serde.writeClassOrInterfaceType(cit),
          () -> hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Cons.apply(
            hydra.ext.java.serde.Serde.writeClassOrInterfaceType(cit),
            hydra.lib.lists.Map.apply(
              hydra.ext.java.serde.Serde::writeAdditionalBound,
              additional))));
      }
    });
  }
  
  static hydra.ast.Expr writeTypeDeclaration(hydra.ext.java.syntax.TypeDeclaration d) {
    return (d).accept(new hydra.ext.java.syntax.TypeDeclaration.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.TypeDeclaration.Class_ d2) {
        return hydra.ext.java.serde.Serde.writeClassDeclaration((d2).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.TypeDeclaration.Interface d2) {
        return hydra.ext.java.serde.Serde.writeInterfaceDeclaration((d2).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.TypeDeclaration.None ignored) {
        return hydra.serialization.Serialization.cst(";");
      }
    });
  }
  
  static hydra.ast.Expr writeTypeDeclarationWithComments(hydra.ext.java.syntax.TypeDeclarationWithComments tdwc) {
    hydra.ext.java.syntax.TypeDeclaration d = (tdwc).value;
    hydra.util.Maybe<String> mc = (tdwc).comments;
    return hydra.ext.java.serde.Serde.withComments(
      mc,
      hydra.ext.java.serde.Serde.writeTypeDeclaration(d));
  }
  
  static hydra.ast.Expr writeTypeIdentifier(hydra.ext.java.syntax.TypeIdentifier tid) {
    return hydra.ext.java.serde.Serde.writeIdentifier((tid).value);
  }
  
  static hydra.ast.Expr writeTypeName(hydra.ext.java.syntax.TypeName tn) {
    hydra.ext.java.syntax.TypeIdentifier id = (tn).identifier;
    hydra.util.Maybe<hydra.ext.java.syntax.PackageOrTypeName> mqual = (tn).qualifier;
    return hydra.serialization.Serialization.dotSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.maybes.Map.apply(
        hydra.ext.java.serde.Serde::writePackageOrTypeName,
        mqual),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeTypeIdentifier(id)))));
  }
  
  static hydra.ast.Expr writeTypeParameter(hydra.ext.java.syntax.TypeParameter tp) {
    hydra.util.Maybe<hydra.ext.java.syntax.TypeBound> bound = (tp).bound;
    hydra.ext.java.syntax.TypeIdentifier id = (tp).identifier;
    java.util.List<hydra.ext.java.syntax.TypeParameterModifier> mods = (tp).modifiers;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(mods),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writeTypeParameterModifier,
          mods)))),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeTypeIdentifier(id)),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.ext.java.syntax.TypeBound, hydra.ast.Expr>) (b -> hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.serialization.Serialization.cst("extends"),
          hydra.ext.java.serde.Serde.writeTypeBound(b)))),
        bound))));
  }
  
  static hydra.ast.Expr writeTypeParameterModifier(hydra.ext.java.syntax.TypeParameterModifier tpm) {
    return hydra.ext.java.serde.Serde.writeAnnotation((tpm).value);
  }
  
  static hydra.ast.Expr writeTypeVariable(hydra.ext.java.syntax.TypeVariable tv) {
    java.util.List<hydra.ext.java.syntax.Annotation> anns = (tv).annotations;
    hydra.ext.java.syntax.TypeIdentifier id = (tv).identifier;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(anns),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.spaceSep(hydra.lib.lists.Map.apply(
          hydra.ext.java.serde.Serde::writeAnnotation,
          anns)))),
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeTypeIdentifier(id)))));
  }
  
  static hydra.ast.Expr writeUnannType(hydra.ext.java.syntax.UnannType ut) {
    return hydra.ext.java.serde.Serde.writeType((ut).value);
  }
  
  static hydra.ast.Expr writeUnaryExpression(hydra.ext.java.syntax.UnaryExpression e) {
    return (e).accept(new hydra.ext.java.syntax.UnaryExpression.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.UnaryExpression.PreIncrement pi) {
        return hydra.ext.java.serde.Serde.writePreIncrementExpression((pi).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.UnaryExpression.PreDecrement pd) {
        return hydra.ext.java.serde.Serde.writePreDecrementExpression((pd).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.UnaryExpression.Plus p) {
        return hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.serialization.Serialization.cst("+"),
          hydra.ext.java.serde.Serde.writeUnaryExpression((p).value)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.UnaryExpression.Minus m) {
        return hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.serialization.Serialization.cst("-"),
          hydra.ext.java.serde.Serde.writeUnaryExpression((m).value)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.UnaryExpression.Other o) {
        return hydra.ext.java.serde.Serde.writeUnaryExpressionNotPlusMinus((o).value);
      }
    });
  }
  
  static hydra.ast.Expr writeUnaryExpressionNotPlusMinus(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus e) {
    return (e).accept(new hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Postfix p) {
        return hydra.ext.java.serde.Serde.writePostfixExpression((p).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Tilde u) {
        return hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.serialization.Serialization.cst("~"),
          hydra.ext.java.serde.Serde.writeUnaryExpression((u).value)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Not u) {
        return hydra.serialization.Serialization.noSep(java.util.List.of(
          hydra.serialization.Serialization.cst("!"),
          hydra.ext.java.serde.Serde.writeUnaryExpression((u).value)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.UnaryExpressionNotPlusMinus.Cast c) {
        return hydra.ext.java.serde.Serde.writeCastExpression((c).value);
      }
    });
  }
  
  static hydra.ast.Expr writeUnqualifiedClassInstanceCreationExpression(hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression ucice) {
    java.util.List<hydra.ext.java.syntax.Expression> args = (ucice).arguments;
    hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate cit = (ucice).classOrInterface;
    hydra.util.Maybe<hydra.ext.java.syntax.ClassBody> mbody = (ucice).body;
    java.util.List<hydra.ext.java.syntax.TypeArgument> targs = (ucice).typeArguments;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.util.Maybe.just(hydra.serialization.Serialization.cst("new")),
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(targs),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.angleBracesList(
          hydra.serialization.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.java.serde.Serde::writeTypeArgument,
            targs)))),
      hydra.util.Maybe.just(hydra.serialization.Serialization.noSep(java.util.List.of(
        hydra.ext.java.serde.Serde.writeClassOrInterfaceTypeToInstantiate(cit),
        hydra.serialization.Serialization.parenList(
          false,
          hydra.lib.lists.Map.apply(
            hydra.ext.java.serde.Serde::writeExpression,
            args))))),
      hydra.lib.maybes.Map.apply(
        hydra.ext.java.serde.Serde::writeClassBody,
        mbody))));
  }
  
  static <T0> hydra.ast.Expr writeVariableArityParameter(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:VariableArityParameter");
  }
  
  static hydra.ast.Expr writeVariableDeclarator(hydra.ext.java.syntax.VariableDeclarator vd) {
    hydra.ext.java.syntax.VariableDeclaratorId id = (vd).id;
    hydra.ast.Expr idSec = hydra.ext.java.serde.Serde.writeVariableDeclaratorId(id);
    hydra.util.Maybe<hydra.ext.java.syntax.VariableInitializer> minit = (vd).initializer;
    return hydra.lib.maybes.Maybe.apply(
      idSec,
      (java.util.function.Function<hydra.ext.java.syntax.VariableInitializer, hydra.ast.Expr>) (init -> hydra.serialization.Serialization.infixWs(
        "=",
        idSec,
        hydra.ext.java.serde.Serde.writeVariableInitializer(init))),
      minit);
  }
  
  static hydra.ast.Expr writeVariableDeclaratorId(hydra.ext.java.syntax.VariableDeclaratorId vdi) {
    hydra.ext.java.syntax.Identifier id = (vdi).identifier;
    hydra.util.Maybe<hydra.ext.java.syntax.Dims> mdims = (vdi).dims;
    return hydra.serialization.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.util.Maybe.just(hydra.ext.java.serde.Serde.writeIdentifier(id)),
      hydra.lib.maybes.Map.apply(
        hydra.ext.java.serde.Serde::writeDims,
        mdims))));
  }
  
  static hydra.ast.Expr writeVariableInitializer(hydra.ext.java.syntax.VariableInitializer i) {
    return (i).accept(new hydra.ext.java.syntax.VariableInitializer.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.VariableInitializer.Expression e) {
        return hydra.ext.java.serde.Serde.writeExpression((e).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.VariableInitializer.ArrayInitializer ai) {
        return hydra.ext.java.serde.Serde.writeArrayInitializer((ai).value);
      }
    });
  }
  
  static hydra.ast.Expr writeVariableModifier(hydra.ext.java.syntax.VariableModifier m) {
    return (m).accept(new hydra.ext.java.syntax.VariableModifier.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.VariableModifier.Annotation ann) {
        return hydra.ext.java.serde.Serde.writeAnnotation((ann).value);
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.VariableModifier.Final ignored) {
        return hydra.serialization.Serialization.cst("final");
      }
    });
  }
  
  static <T0> hydra.ast.Expr writeWhileStatement(T0 ignored) {
    return hydra.serialization.Serialization.cst("STUB:WhileStatement");
  }
  
  static hydra.ast.Expr writeWildcard(hydra.ext.java.syntax.Wildcard w) {
    java.util.List<hydra.ext.java.syntax.Annotation> anns = (w).annotations;
    hydra.util.Maybe<hydra.ext.java.syntax.WildcardBounds> mbounds = (w).wildcard;
    return hydra.serialization.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.List.of(
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(anns),
        () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
        () -> hydra.util.Maybe.just(hydra.serialization.Serialization.commaSep(
          hydra.serialization.Serialization.inlineStyle(),
          hydra.lib.lists.Map.apply(
            hydra.ext.java.serde.Serde::writeAnnotation,
            anns)))),
      hydra.util.Maybe.just(hydra.serialization.Serialization.cst("*")),
      hydra.lib.maybes.Map.apply(
        hydra.ext.java.serde.Serde::writeWildcardBounds,
        mbounds))));
  }
  
  static hydra.ast.Expr writeWildcardBounds(hydra.ext.java.syntax.WildcardBounds b) {
    return (b).accept(new hydra.ext.java.syntax.WildcardBounds.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.WildcardBounds.Extends rt) {
        return hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.serialization.Serialization.cst("extends"),
          hydra.ext.java.serde.Serde.writeReferenceType((rt).value)));
      }
      
      @Override
      public hydra.ast.Expr visit(hydra.ext.java.syntax.WildcardBounds.Super rt) {
        return hydra.serialization.Serialization.spaceSep(java.util.List.of(
          hydra.serialization.Serialization.cst("super"),
          hydra.ext.java.serde.Serde.writeReferenceType((rt).value)));
      }
    });
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
    return hydra.serialization.Serialization.cst(hydra.lib.strings.Cat2.apply(
      "// ",
      hydra.ext.java.serde.Serde.sanitizeJavaComment(c)));
  }
  
  static hydra.ast.Expr withComments(hydra.util.Maybe<String> mc, hydra.ast.Expr expr) {
    return hydra.lib.maybes.Maybe.apply(
      expr,
      (java.util.function.Function<String, hydra.ast.Expr>) (c -> hydra.serialization.Serialization.newlineSep(java.util.List.of(
        hydra.serialization.Serialization.cst(hydra.lib.strings.Cat2.apply(
          "/**\n",
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Intercalate.apply(
              "\n",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<String, String>) (l -> hydra.lib.strings.Cat2.apply(
                  " * ",
                  l)),
                hydra.lib.strings.Lines.apply(hydra.ext.java.serde.Serde.sanitizeJavaComment(c)))),
            "\n */"))),
        expr))),
      mc);
  }
}
