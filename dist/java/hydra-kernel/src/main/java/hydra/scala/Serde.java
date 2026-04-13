// Note: this is an automatically generated file. Do not edit.

package hydra.scala;

/**
 * Serialization functions for converting Scala AST to abstract expressions
 */
public interface Serde {
  static hydra.ast.Op dotOp() {
    return new hydra.ast.Op(new hydra.ast.Symbol("."), new hydra.ast.Padding(new hydra.ast.Ws.None(), new hydra.ast.Ws.None()), new hydra.ast.Precedence(0), new hydra.ast.Associativity.Left());
  }

  static hydra.ast.Op functionArrowOp() {
    return hydra.Serialization.op(
      "=>",
      hydra.lib.math.Negate.apply(1),
      new hydra.ast.Associativity.Right());
  }

  static hydra.ast.Op matchOp() {
    return new hydra.ast.Op(new hydra.ast.Symbol("match"), new hydra.ast.Padding(new hydra.ast.Ws.Space(), new hydra.ast.Ws.BreakAndIndent("  ")), new hydra.ast.Precedence(0), new hydra.ast.Associativity.None());
  }

  static String scalaFloatLiteralText(String prefix, String suffix, String s) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        s,
        "NaN"),
      () -> hydra.lib.strings.Cat2.apply(
        prefix,
        ".NaN"),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          s,
          "Infinity"),
        () -> hydra.lib.strings.Cat2.apply(
          prefix,
          ".PositiveInfinity"),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            s,
            "-Infinity"),
          () -> hydra.lib.strings.Cat2.apply(
            prefix,
            ".NegativeInfinity"),
          () -> hydra.lib.strings.Cat2.apply(
            s,
            suffix))));
  }

  static hydra.ast.Expr writeCase(hydra.scala.syntax.Case c) {
    hydra.scala.syntax.Pat pat = (c).pat;
    hydra.scala.syntax.Data term = (c).body;
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst("case"),
      hydra.scala.Serde.writePat(pat),
      hydra.Serialization.cst("=>"),
      hydra.scala.Serde.writeTerm(term)));
  }

  static hydra.ast.Expr writeData_FunctionData(hydra.scala.syntax.Data_FunctionData ft) {
    return (ft).accept(new hydra.scala.syntax.Data_FunctionData.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Data_FunctionData.Function f) {
        hydra.scala.syntax.Data body = (f).value.body;
        hydra.ast.Expr bodyExpr = hydra.scala.Serde.writeTerm(body);
        Integer bodyLen = hydra.Serialization.expressionLength(bodyExpr);
        java.util.List<hydra.scala.syntax.Data_Param> params = (f).value.params;
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Gt.apply(
            bodyLen,
            60),
          () -> hydra.Serialization.noSep(java.util.Arrays.asList(
            hydra.Serialization.parenList(
              false,
              hydra.lib.lists.Map.apply(
                hydra.scala.Serde::writeData_Param,
                params)),
            hydra.Serialization.cst(" =>\n  "),
            bodyExpr)),
          () -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.Serialization.parenList(
              false,
              hydra.lib.lists.Map.apply(
                hydra.scala.Serde::writeData_Param,
                params)),
            hydra.Serialization.cst("=>"),
            bodyExpr)));
      }
    });
  }

  static hydra.ast.Expr writeData_Name(hydra.scala.syntax.Data_Name dn) {
    return hydra.Serialization.cst((dn).value.value);
  }

  static hydra.ast.Expr writeData_Param(hydra.scala.syntax.Data_Param dp) {
    hydra.scala.syntax.Name name = (dp).name;
    hydra.util.Maybe<hydra.scala.syntax.Type> stype = (dp).decltpe;
    return hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
      hydra.lib.maybes.Pure.apply(hydra.scala.Serde.writeName(name)),
      hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.scala.syntax.Type, hydra.ast.Expr>) (t -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst(":"),
          hydra.scala.Serde.writeType(t)))),
        stype))));
  }

  static hydra.ast.Expr writeData_Ref(hydra.scala.syntax.Data_Ref ref) {
    return (ref).accept(new hydra.scala.syntax.Data_Ref.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Data_Ref.Name name) {
        return hydra.scala.Serde.writeData_Name((name).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Data_Ref.Select sel) {
        return hydra.scala.Serde.writeData_Select((sel).value);
      }
    });
  }

  static hydra.ast.Expr writeData_Select(hydra.scala.syntax.Data_Select sel) {
    hydra.scala.syntax.Data arg = (sel).qual;
    hydra.scala.syntax.Data_Name name = (sel).name;
    return hydra.Serialization.ifx(
      hydra.scala.Serde.dotOp(),
      hydra.scala.Serde.writeTerm(arg),
      hydra.scala.Serde.writeTerm(new hydra.scala.syntax.Data.Ref(new hydra.scala.syntax.Data_Ref.Name(name))));
  }

  static hydra.ast.Expr writeDefn(hydra.scala.syntax.Defn def) {
    return (def).accept(new hydra.scala.syntax.Defn.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Defn.Def dd) {
        hydra.scala.syntax.Data body = (dd).value.body;
        hydra.ast.Expr bodyExpr = hydra.scala.Serde.writeTerm(body);
        Integer bodyLen = hydra.Serialization.expressionLength(bodyExpr);
        hydra.scala.syntax.Data_Name name = (dd).value.name;
        java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> paramss = (dd).value.paramss;
        hydra.util.Lazy<java.util.List<hydra.ast.Expr>> paramssExprs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<java.util.List<hydra.scala.syntax.Data_Param>, hydra.ast.Expr>) (ps -> hydra.Serialization.parenList(
            false,
            hydra.lib.lists.Map.apply(
              hydra.scala.Serde::writeData_Param,
              ps))),
          paramss));
        hydra.util.Maybe<hydra.scala.syntax.Type> scod = (dd).value.decltpe;
        hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> scodExpr = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.scala.syntax.Type, hydra.ast.Expr>) (t -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.Serialization.cst(":"),
            hydra.scala.Serde.writeType(t)))),
          scod));
        java.util.List<hydra.scala.syntax.Type_Param> tparams = (dd).value.tparams;
        hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> tparamsExpr = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(tparams),
          () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
          () -> hydra.lib.maybes.Pure.apply(hydra.Serialization.bracketList(
            hydra.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.scala.Serde::writeType_Param,
              tparams)))));
        hydra.util.Lazy<hydra.ast.Expr> nameAndParams = new hydra.util.Lazy<>(() -> hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(hydra.lib.maybes.Pure.apply(hydra.scala.Serde.writeData_Name(name))),
          java.util.Arrays.asList(tparamsExpr.get()),
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.ast.Expr, hydra.util.Maybe<hydra.ast.Expr>>) (pe -> hydra.lib.maybes.Pure.apply(pe)),
            paramssExprs.get()),
          java.util.Arrays.asList(scodExpr.get()))))));
        hydra.ast.Expr defSig = hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("def"),
          nameAndParams.get(),
          hydra.Serialization.cst("=")));
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Gt.apply(
            bodyLen,
            80),
          () -> hydra.Serialization.noSep(java.util.Arrays.asList(
            defSig,
            hydra.Serialization.cst("\n  "),
            bodyExpr)),
          () -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
            defSig,
            bodyExpr)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Defn.Type dt) {
        hydra.scala.syntax.Type body = (dt).value.body;
        hydra.scala.syntax.Type_Name name = (dt).value.name;
        java.util.List<hydra.scala.syntax.Type_Param> tparams = (dt).value.tparams;
        return hydra.Serialization.spaceSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
          hydra.lib.maybes.Pure.apply(hydra.Serialization.cst("type")),
          hydra.lib.maybes.Pure.apply(hydra.scala.Serde.writeType_Name(name)),
          hydra.lib.logic.IfElse.lazy(
            hydra.lib.lists.Null.apply(tparams),
            () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
            () -> hydra.lib.maybes.Pure.apply(hydra.Serialization.bracketList(
              hydra.Serialization.inlineStyle(),
              hydra.lib.lists.Map.apply(
                hydra.scala.Serde::writeType_Param,
                tparams)))),
          hydra.lib.maybes.Pure.apply(hydra.Serialization.cst("=")),
          hydra.lib.maybes.Pure.apply(hydra.scala.Serde.writeType(body)))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Defn.Val dv) {
        java.util.List<hydra.scala.syntax.Pat> pats = (dv).value.pats;
        hydra.util.Lazy<hydra.scala.syntax.Pat> firstPat = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply(pats));
        java.util.List<hydra.scala.syntax.Mod> mods = (dv).value.mods;
        hydra.scala.syntax.Data_Name patName = firstPat.get().accept(new hydra.scala.syntax.Pat.PartialVisitor<>() {
          @Override
          public hydra.scala.syntax.Data_Name visit(hydra.scala.syntax.Pat.Var pv) {
            return (pv).value.name;
          }
        });
        String nameStr = (patName).value.value;
        hydra.util.Maybe<hydra.scala.syntax.Type> typ = (dv).value.decltpe;
        hydra.util.Lazy<hydra.ast.Expr> nameAndType = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.Serialization.cst(nameStr),
          (java.util.function.Function<hydra.scala.syntax.Type, hydra.ast.Expr>) (t -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
              nameStr,
              ":")),
            hydra.scala.Serde.writeType(t)))),
          typ));
        hydra.scala.syntax.Data rhs = (dv).value.rhs;
        hydra.util.Lazy<String> valKeyword = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(mods),
          () -> "val",
          () -> "lazy val"));
        return hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst(valKeyword.get()),
          nameAndType.get(),
          hydra.Serialization.cst("="),
          hydra.scala.Serde.writeTerm(rhs)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Defn.Class_ dc) {
        hydra.scala.syntax.Ctor_Primary ctor = (dc).value.ctor;
        java.util.List<hydra.scala.syntax.Mod> mods = (dc).value.mods;
        hydra.scala.syntax.Type_Name name = (dc).value.name;
        java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> paramss = (ctor).paramss;
        hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> paramsExpr = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(paramss),
          () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
          () -> hydra.lib.maybes.Pure.apply(hydra.Serialization.parenList(
            false,
            hydra.lib.lists.Map.apply(
              hydra.scala.Serde::writeData_Param,
              hydra.lib.lists.Concat.apply(paramss))))));
        java.util.List<hydra.scala.syntax.Type_Param> tparams = (dc).value.tparams;
        hydra.util.Lazy<hydra.util.Maybe<hydra.ast.Expr>> tparamsExpr = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(tparams),
          () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
          () -> hydra.lib.maybes.Pure.apply(hydra.Serialization.bracketList(
            hydra.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.scala.Serde::writeType_Param,
              tparams)))));
        hydra.util.Lazy<hydra.ast.Expr> nameAndParams = new hydra.util.Lazy<>(() -> hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
          hydra.lib.maybes.Pure.apply(hydra.scala.Serde.writeType_Name(name)),
          tparamsExpr.get(),
          paramsExpr.get()))));
        return hydra.Serialization.spaceSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          hydra.lib.lists.Map.apply(
            hydra.scala.Serde::writeMod,
            mods),
          java.util.Arrays.asList(
            hydra.Serialization.cst("class"),
            nameAndParams.get()))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Defn.Enum_ de) {
        hydra.scala.syntax.Template template = (de).value.template;
        java.util.List<hydra.scala.syntax.Stat> stats = (template).stats;
        hydra.util.Lazy<java.util.List<hydra.ast.Expr>> enumCases = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.scala.syntax.Stat, hydra.ast.Expr>) (s -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.Serialization.cst("  "),
            hydra.scala.Serde.writeStat(s)))),
          stats));
        hydra.scala.syntax.Type_Name name = (de).value.name;
        java.util.List<hydra.scala.syntax.Type_Param> tparams = (de).value.tparams;
        hydra.util.Lazy<hydra.ast.Expr> enumHeader = new hydra.util.Lazy<>(() -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("enum"),
          hydra.Serialization.noSep(hydra.lib.maybes.Cat.apply(java.util.Arrays.asList(
            hydra.lib.maybes.Pure.apply(hydra.scala.Serde.writeType_Name(name)),
            hydra.lib.logic.IfElse.lazy(
              hydra.lib.lists.Null.apply(tparams),
              () -> (hydra.util.Maybe<hydra.ast.Expr>) (hydra.util.Maybe.<hydra.ast.Expr>nothing()),
              () -> hydra.lib.maybes.Pure.apply(hydra.Serialization.bracketList(
                hydra.Serialization.inlineStyle(),
                hydra.lib.lists.Map.apply(
                  hydra.scala.Serde::writeType_Param,
                  tparams))))))),
          hydra.Serialization.cst(":"))));
        return hydra.Serialization.newlineSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
          java.util.Arrays.asList(enumHeader.get()),
          enumCases.get())));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Defn.EnumCase dec) {
        hydra.scala.syntax.Ctor_Primary ctor = (dec).value.ctor;
        java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> paramss = (ctor).paramss;
        hydra.util.Lazy<java.util.List<hydra.scala.syntax.Data_Param>> allParams = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(paramss));
        java.util.List<hydra.scala.syntax.Init> inits = (dec).value.inits;
        hydra.util.Lazy<hydra.ast.Expr> extendsClause = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(inits),
          () -> hydra.Serialization.cst(""),
          () -> hydra.Serialization.spaceSep(java.util.Arrays.asList(
            hydra.Serialization.cst("extends"),
            hydra.Serialization.commaSep(
              hydra.Serialization.inlineStyle(),
              hydra.lib.lists.Map.apply(
                hydra.scala.Serde::writeInit,
                inits))))));
        hydra.scala.syntax.Data_Name name = (dec).value.name;
        hydra.util.Lazy<hydra.ast.Expr> params = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(allParams.get()),
          () -> hydra.Serialization.cst(""),
          () -> hydra.Serialization.parenList(
            false,
            hydra.lib.lists.Map.apply(
              hydra.scala.Serde::writeData_Param,
              allParams.get()))));
        return hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.Serialization.cst("case"),
          hydra.Serialization.noSep(java.util.Arrays.asList(
            hydra.scala.Serde.writeData_Name(name),
            params.get())),
          extendsClause.get()));
      }
    });
  }

  static hydra.ast.Expr writeImportExportStat(hydra.scala.syntax.ImportExportStat ie) {
    return (ie).accept(new hydra.scala.syntax.ImportExportStat.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.ImportExportStat.Import imp) {
        java.util.List<hydra.scala.syntax.Importer> importers = (imp).value.importers;
        return hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
          hydra.scala.Serde::writeImporter,
          importers));
      }
    });
  }

  static hydra.ast.Expr writeImporter(hydra.scala.syntax.Importer imp) {
    java.util.List<hydra.scala.syntax.Importee> importees = (imp).importees;
    hydra.util.Lazy<hydra.ast.Expr> forImportees = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(importees),
      () -> hydra.Serialization.cst(""),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          hydra.lib.lists.Length.apply(importees),
          1),
        () -> hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("."),
          hydra.lib.lists.Head.apply(importees).accept(new hydra.scala.syntax.Importee.PartialVisitor<>() {
            @Override
            public hydra.ast.Expr visit(hydra.scala.syntax.Importee.Wildcard ignored) {
              return hydra.Serialization.cst("*");
            }

            @Override
            public hydra.ast.Expr visit(hydra.scala.syntax.Importee.Name in) {
              return hydra.Serialization.cst((in).value.name.accept(new hydra.scala.syntax.Name.PartialVisitor<>() {
                @Override
                public String visit(hydra.scala.syntax.Name.Value s) {
                  return (s).value;
                }
              }));
            }
          }))),
        () -> hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.Serialization.cst("."),
          hydra.Serialization.curlyBracesList(
            (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
            hydra.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              (java.util.function.Function<hydra.scala.syntax.Importee, hydra.ast.Expr>) (it -> (it).accept(new hydra.scala.syntax.Importee.PartialVisitor<>() {
                @Override
                public hydra.ast.Expr visit(hydra.scala.syntax.Importee.Wildcard ignored) {
                  return hydra.Serialization.cst("*");
                }

                @Override
                public hydra.ast.Expr visit(hydra.scala.syntax.Importee.Name in) {
                  return hydra.Serialization.cst((in).value.name.accept(new hydra.scala.syntax.Name.PartialVisitor<>() {
                    @Override
                    public String visit(hydra.scala.syntax.Name.Value s) {
                      return (s).value;
                    }
                  }));
                }
              })),
              importees)))))));
    hydra.scala.syntax.Data_Ref ref = (imp).ref;
    String refName = (ref).accept(new hydra.scala.syntax.Data_Ref.PartialVisitor<>() {
      @Override
      public String visit(hydra.scala.syntax.Data_Ref.Name dn) {
        return (dn).value.value.value;
      }
    });
    return hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst("import"),
      hydra.Serialization.noSep(java.util.Arrays.asList(
        hydra.Serialization.cst(refName),
        forImportees.get()))));
  }

  static hydra.ast.Expr writeInit(hydra.scala.syntax.Init init) {
    return hydra.scala.Serde.writeType((init).tpe);
  }

  static hydra.ast.Expr writeLit(hydra.scala.syntax.Lit lit) {
    return (lit).accept(new hydra.scala.syntax.Lit.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr otherwise(hydra.scala.syntax.Lit instance) {
        return hydra.Serialization.cst("TODO:literal");
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Lit.Boolean_ b) {
        return hydra.Serialization.cst(hydra.lib.logic.IfElse.lazy(
          (b).value,
          () -> "true",
          () -> "false"));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Lit.Byte_ i) {
        return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
          hydra.lib.literals.ShowInt8.apply((i).value),
          ".toByte"));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Lit.Short_ i) {
        return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
          hydra.lib.literals.ShowInt16.apply((i).value),
          ".toShort"));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Lit.Int i) {
        return hydra.Serialization.cst(hydra.lib.literals.ShowInt32.apply((i).value));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Lit.Long_ i) {
        return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
          hydra.lib.literals.ShowInt64.apply((i).value),
          "L"));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Lit.Float_ f) {
        return hydra.Serialization.cst(hydra.scala.Serde.scalaFloatLiteralText(
          "Float",
          "f",
          hydra.lib.literals.ShowFloat32.apply((f).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Lit.Double_ f) {
        return hydra.Serialization.cst(hydra.scala.Serde.scalaFloatLiteralText(
          "Double",
          "",
          hydra.lib.literals.ShowFloat64.apply((f).value)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Lit.Unit ignored) {
        return hydra.Serialization.cst("()");
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Lit.String_ s) {
        return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
          "\"",
          hydra.lib.strings.Cat2.apply(
            hydra.java.Serde.escapeJavaString((s).value),
            "\"")));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Lit.Bytes bs) {
        return hydra.Serialization.cst(hydra.lib.strings.Cat2.apply(
          "Array[Byte](",
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<Integer, String>) (b -> hydra.lib.strings.Cat2.apply(
                  hydra.lib.literals.ShowInt32.apply(b),
                  ".toByte")),
                (bs).value)),
            ")")));
      }
    });
  }

  static hydra.ast.Expr writeMod(hydra.scala.syntax.Mod m) {
    return (m).accept(new hydra.scala.syntax.Mod.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Mod.Case ignored) {
        return hydra.Serialization.cst("case");
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Mod.Sealed ignored) {
        return hydra.Serialization.cst("sealed");
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Mod.Abstract ignored) {
        return hydra.Serialization.cst("abstract");
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Mod.Final ignored) {
        return hydra.Serialization.cst("final");
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Mod.Override_ ignored) {
        return hydra.Serialization.cst("override");
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Mod.Implicit ignored) {
        return hydra.Serialization.cst("implicit");
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Mod.Lazy ignored) {
        return hydra.Serialization.cst("lazy");
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Mod.Private ignored) {
        return hydra.Serialization.cst("private");
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Mod.Protected ignored) {
        return hydra.Serialization.cst("protected");
      }
    });
  }

  static hydra.ast.Expr writeName(hydra.scala.syntax.Name name) {
    return (name).accept(new hydra.scala.syntax.Name.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Name.Value s) {
        return hydra.Serialization.cst((s).value);
      }
    });
  }

  static hydra.ast.Expr writePat(hydra.scala.syntax.Pat pat) {
    return (pat).accept(new hydra.scala.syntax.Pat.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Pat.Extract pe) {
        java.util.List<hydra.scala.syntax.Pat> args = (pe).value.args;
        hydra.scala.syntax.Data fun = (pe).value.fun;
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(args),
          () -> hydra.scala.Serde.writeTerm(fun),
          () -> hydra.Serialization.noSep(java.util.Arrays.asList(
            hydra.scala.Serde.writeTerm(fun),
            hydra.Serialization.parenList(
              false,
              hydra.lib.lists.Map.apply(
                hydra.scala.Serde::writePat,
                args)))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Pat.Var pv) {
        return hydra.scala.Serde.writeData_Name((pv).value.name);
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Pat.Wildcard ignored) {
        return hydra.Serialization.cst("_");
      }
    });
  }

  static hydra.ast.Expr writePkg(hydra.scala.syntax.Pkg pkg) {
    hydra.scala.syntax.Data_Name name = (pkg).name;
    hydra.ast.Expr package_ = hydra.Serialization.spaceSep(java.util.Arrays.asList(
      hydra.Serialization.cst("package"),
      hydra.scala.Serde.writeData_Name(name)));
    java.util.List<hydra.scala.syntax.Stat> stats = (pkg).stats;
    return hydra.Serialization.doubleNewlineSep(hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
      java.util.Arrays.asList(package_),
      hydra.lib.lists.Map.apply(
        hydra.scala.Serde::writeStat,
        stats))));
  }

  static hydra.ast.Expr writeStat(hydra.scala.syntax.Stat stat) {
    return (stat).accept(new hydra.scala.syntax.Stat.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Stat.Term t) {
        return hydra.scala.Serde.writeTerm((t).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Stat.Defn def) {
        return hydra.scala.Serde.writeDefn((def).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Stat.ImportExport ie) {
        return hydra.scala.Serde.writeImportExportStat((ie).value);
      }
    });
  }

  static hydra.ast.Expr writeTerm(hydra.scala.syntax.Data term) {
    return (term).accept(new hydra.scala.syntax.Data.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Data.Lit lit) {
        return hydra.scala.Serde.writeLit((lit).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Data.Ref ref) {
        return hydra.scala.Serde.writeData_Ref((ref).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Data.Apply app) {
        java.util.List<hydra.scala.syntax.Data> args = (app).value.args;
        hydra.scala.syntax.Data fun = (app).value.fun;
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.scala.Serde.writeTerm(fun),
          hydra.Serialization.parenList(
            false,
            hydra.lib.lists.Map.apply(
              hydra.scala.Serde::writeTerm,
              args))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Data.Assign a) {
        hydra.scala.syntax.Data lhs = (a).value.lhs;
        hydra.scala.syntax.Data rhs = (a).value.rhs;
        return hydra.Serialization.spaceSep(java.util.Arrays.asList(
          hydra.scala.Serde.writeTerm(lhs),
          hydra.Serialization.cst("->"),
          hydra.scala.Serde.writeTerm(rhs)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Data.Tuple tup) {
        return hydra.Serialization.parenList(
          false,
          hydra.lib.lists.Map.apply(
            hydra.scala.Serde::writeTerm,
            (tup).value.args));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Data.Match m) {
        hydra.scala.syntax.Data expr = (m).value.expr;
        java.util.List<hydra.scala.syntax.Case> mCases = (m).value.cases;
        return hydra.Serialization.ifx(
          hydra.scala.Serde.matchOp(),
          hydra.scala.Serde.writeTerm(expr),
          hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
            hydra.scala.Serde::writeCase,
            mCases)));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Data.FunctionData ft) {
        return hydra.scala.Serde.writeData_FunctionData((ft).value);
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Data.Block blk) {
        java.util.List<hydra.scala.syntax.Stat> stats = (blk).value.stats;
        return hydra.Serialization.curlyBlock(
          hydra.Serialization.fullBlockStyle(),
          hydra.Serialization.newlineSep(hydra.lib.lists.Map.apply(
            hydra.scala.Serde::writeStat,
            stats)));
      }
    });
  }

  static hydra.ast.Expr writeType(hydra.scala.syntax.Type typ) {
    return (typ).accept(new hydra.scala.syntax.Type.PartialVisitor<>() {
      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Type.Ref tr) {
        return (tr).value.accept(new hydra.scala.syntax.Type_Ref.PartialVisitor<>() {
          @Override
          public hydra.ast.Expr visit(hydra.scala.syntax.Type_Ref.Name name) {
            return hydra.scala.Serde.writeType_Name((name).value);
          }
        });
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Type.Apply ta) {
        java.util.List<hydra.scala.syntax.Type> args = (ta).value.args;
        hydra.scala.syntax.Type fun = (ta).value.tpe;
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.scala.Serde.writeType(fun),
          hydra.Serialization.bracketList(
            hydra.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.scala.Serde::writeType,
              args))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Type.FunctionType ft) {
        return (ft).value.accept(new hydra.scala.syntax.Type_FunctionType.PartialVisitor<>() {
          @Override
          public hydra.ast.Expr visit(hydra.scala.syntax.Type_FunctionType.Function tf) {
            hydra.scala.syntax.Type cod = (tf).value.res;
            hydra.util.Lazy<hydra.scala.syntax.Type> dom = new hydra.util.Lazy<>(() -> hydra.lib.lists.Head.apply((tf).value.params));
            return hydra.Serialization.ifx(
              hydra.scala.Serde.functionArrowOp(),
              hydra.scala.Serde.writeType(dom.get()),
              hydra.scala.Serde.writeType(cod));
          }
        });
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Type.Lambda tl) {
        hydra.scala.syntax.Type body = (tl).value.tpe;
        java.util.List<hydra.scala.syntax.Type_Param> params = (tl).value.tparams;
        return hydra.Serialization.noSep(java.util.Arrays.asList(
          hydra.scala.Serde.writeType(body),
          hydra.Serialization.bracketList(
            hydra.Serialization.inlineStyle(),
            hydra.lib.lists.Map.apply(
              hydra.scala.Serde::writeType_Param,
              params))));
      }

      @Override
      public hydra.ast.Expr visit(hydra.scala.syntax.Type.Var tv) {
        return hydra.scala.Serde.writeType_Name((tv).value.name);
      }
    });
  }

  static hydra.ast.Expr writeType_Name(hydra.scala.syntax.Type_Name tn) {
    return hydra.Serialization.cst((tn).value);
  }

  static hydra.ast.Expr writeType_Param(hydra.scala.syntax.Type_Param tp) {
    return hydra.scala.Serde.writeName((tp).name);
  }
}
