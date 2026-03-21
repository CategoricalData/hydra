// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.testCodec;

/**
 * Java test code generation codec for JUnit-based generation tests
 */
public interface TestCodec {
  static hydra.util.Either<String, String> termToJava(hydra.core.Term term, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bimap.apply(
      (java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, String>) (ic -> hydra.show.errors.Errors.error(((java.util.function.Function<hydra.context.InContext<hydra.errors.Error_>, hydra.errors.Error_>) (projected -> projected.object)).apply(ic))),
      (java.util.function.Function<hydra.ext.java.syntax.Expression, String>) (arg_ -> hydra.serialization.Serialization.printExpr(hydra.serialization.Serialization.parenthesize(hydra.ext.java.serde.Serde.writeExpression(arg_)))),
      hydra.ext.java.coder.Coder.encodeTerm(
        new hydra.ext.java.helpers.JavaEnvironment(new hydra.ext.java.helpers.Aliases(new hydra.module.Namespace("test"), (hydra.util.PersistentMap<hydra.module.Namespace, hydra.ext.java.syntax.PackageName>) ((hydra.util.PersistentMap<hydra.module.Namespace, hydra.ext.java.syntax.PackageName>) (hydra.lib.maps.Empty.<hydra.module.Namespace, hydra.ext.java.syntax.PackageName>apply())), (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())), (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) ((hydra.util.PersistentMap<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())), (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), (hydra.util.PersistentSet<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())), g),
        term,
        hydra.lexical.Lexical.emptyContext(),
        g));
  }

  static <T0, T1, T2> hydra.util.Either<T2, String> typeToJava(T0 _t, T1 _g) {
    return hydra.util.Either.<T2, String>right("Object");
  }

  static hydra.testing.TestCodec javaTestCodec() {
    return new hydra.testing.TestCodec(new hydra.coders.LanguageName("java"), new hydra.module.FileExtension("java"), (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, String>>>) (p0 -> p1 -> hydra.ext.java.testCodec.TestCodec.termToJava(
      p0,
      p1)), p0 -> p1 -> hydra.ext.java.testCodec.TestCodec.<hydra.core.Type, hydra.graph.Graph, String>typeToJava(
      p0,
      p1), hydra.ext.java.testCodec.TestCodec::formatJavaTestName, hydra.ext.java.testCodec.TestCodec::namespaceToJavaClassName, hydra.ext.java.testCodec.TestCodec.javaTestCaseTemplate(), hydra.ext.java.testCodec.TestCodec.javaTestGroupTemplate(), hydra.ext.java.testCodec.TestCodec.javaModuleTemplate(), hydra.ext.java.testCodec.TestCodec.javaImportTemplate(), (java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.ConsList<String>>) (_names -> hydra.ext.java.testCodec.TestCodec.findJavaImports()));
  }

  static String formatJavaTestName(String name) {
    String replaced = hydra.lib.strings.Intercalate.apply(
      " Neg",
      hydra.lib.strings.SplitOn.apply(
        "-",
        hydra.lib.strings.Intercalate.apply(
          "Dot",
          hydra.lib.strings.SplitOn.apply(
            ".",
            hydra.lib.strings.Intercalate.apply(
              " Plus",
              hydra.lib.strings.SplitOn.apply(
                "+",
                hydra.lib.strings.Intercalate.apply(
                  " Div",
                  hydra.lib.strings.SplitOn.apply(
                    "/",
                    hydra.lib.strings.Intercalate.apply(
                      " Mul",
                      hydra.lib.strings.SplitOn.apply(
                        "*",
                        hydra.lib.strings.Intercalate.apply(
                          " Num",
                          hydra.lib.strings.SplitOn.apply(
                            "#",
                            name))))))))))));
    String sanitized = hydra.formatting.Formatting.nonAlnumToUnderscores(replaced);
    String pascal_ = hydra.formatting.Formatting.convertCase(
      new hydra.util.CaseConvention.LowerSnake(),
      new hydra.util.CaseConvention.Pascal(),
      sanitized);
    return hydra.lib.strings.Cat2.apply(
      "test",
      pascal_);
  }

  static String namespaceToJavaClassName(hydra.module.Namespace ns_) {
    return hydra.lib.strings.Intercalate.apply(
      ".",
      hydra.lib.lists.Map.apply(
        hydra.formatting.Formatting::capitalize,
        hydra.lib.strings.SplitOn.apply(
          ".",
          (ns_).value)));
  }

  static String javaTestCaseTemplate() {
    return hydra.lib.strings.Intercalate.apply(
      "\n",
      hydra.util.ConsList.of(
        "    @Test",
        "    public void {name}() {",
        "        assertEquals({output}, {input});",
        "    }"));
  }

  static String javaTestGroupTemplate() {
    return "// {groupName}";
  }

  static String javaModuleTemplate() {
    return hydra.lib.strings.Intercalate.apply(
      "\n",
      hydra.util.ConsList.of(
        hydra.lib.strings.Cat2.apply(
          "// ",
          hydra.constants.Constants.warningAutoGeneratedFile()),
        "",
        "package {package};",
        "",
        "{imports}",
        "",
        "public class {className} {",
        "    {testCases}",
        "}"));
  }

  static String javaImportTemplate() {
    return "import {namespace};";
  }

  static hydra.util.ConsList<String> findJavaImports() {
    return hydra.util.ConsList.of(
      "import org.junit.jupiter.api.Test;",
      "import static org.junit.jupiter.api.Assertions.*;",
      "import java.util.*;");
  }

  static hydra.util.Either<String, String> generateJavaTestGroupHierarchy(hydra.graph.Graph g, hydra.testing.TestCodec codec, hydra.util.ConsList<String> groupPath, hydra.testing.TestGroup testGroup) {
    hydra.util.ConsList<hydra.testing.TestCaseWithMetadata> cases_ = (testGroup).cases;
    hydra.util.ConsList<hydra.testing.TestGroup> subgroups = (testGroup).subgroups;
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Map.apply(
        (java.util.function.Function<hydra.util.ConsList<hydra.util.ConsList<String>>, String>) (lines_ -> hydra.lib.strings.Intercalate.apply(
          "\n\n",
          hydra.lib.lists.Concat.apply(lines_))),
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.testing.TestCaseWithMetadata, hydra.util.Either<String, hydra.util.ConsList<String>>>) (tc -> hydra.ext.java.testCodec.TestCodec.generateJavaTestCase(
            g,
            codec,
            groupPath,
            tc)),
          cases_)),
      (java.util.function.Function<String, hydra.util.Either<String, String>>) (testCasesStr -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<String, String>) (subgroupsStr -> hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          testCasesStr,
          hydra.lib.logic.IfElse.lazy(
            hydra.lib.logic.Or.apply(
              hydra.lib.equality.Equal.apply(
                testCasesStr,
                ""),
              hydra.lib.equality.Equal.apply(
                subgroupsStr,
                "")),
            () -> "",
            () -> "\n\n"),
          subgroupsStr))),
        hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.util.ConsList<String>, String>) (blocks -> hydra.lib.strings.Intercalate.apply(
            "\n\n",
            blocks)),
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.testing.TestGroup, hydra.util.Either<String, String>>) (subgroup -> {
              String groupName = (subgroup).name;
              String header = hydra.lib.strings.Cat2.apply(
                "    // ",
                groupName);
              return hydra.lib.eithers.Map.apply(
                (java.util.function.Function<String, String>) (content -> hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
                  header,
                  "\n\n",
                  content))),
                hydra.ext.java.testCodec.TestCodec.generateJavaTestGroupHierarchy(
                  g,
                  codec,
                  hydra.lib.lists.Concat2.apply(
                    groupPath,
                    hydra.util.ConsList.of(groupName)),
                  subgroup));
            }),
            subgroups)))));
  }

  static hydra.util.Either<String, hydra.util.ConsList<String>> generateJavaTestCase(hydra.graph.Graph g, hydra.testing.TestCodec codec, hydra.util.ConsList<String> groupPath, hydra.testing.TestCaseWithMetadata tcm) {
    String name_ = (tcm).name;
    hydra.testing.TestCase tcase = (tcm).case_;
    return (tcase).accept(new hydra.testing.TestCase.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.util.ConsList<String>> otherwise(hydra.testing.TestCase instance) {
        return hydra.util.Either.<String, hydra.util.ConsList<String>>right((hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty()));
      }

      @Override
      public hydra.util.Either<String, hydra.util.ConsList<String>> visit(hydra.testing.TestCase.DelegatedEvaluation delCase) {
        hydra.core.Term output_ = (delCase).value.output;
        String assertType = hydra.ext.java.testCodec.TestCodec.getAssertionType(output_);
        hydra.util.Lazy<String> fullName = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(groupPath),
          () -> name_,
          () -> hydra.lib.strings.Intercalate.apply(
            "_",
            hydra.lib.lists.Concat2.apply(
              groupPath,
              hydra.util.ConsList.of(name_)))));
        String formattedName = (codec).formatTestName.apply(fullName.get());
        hydra.core.Term input_ = (delCase).value.input;
        hydra.util.Lazy<hydra.util.ConsList<hydra.core.Name>> typeVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Sort.apply(hydra.lib.lists.Filter.apply(
          hydra.ext.java.testCodec.TestCodec::isInferenceVar,
          hydra.lib.sets.ToList.apply(hydra.lib.sets.Union.apply(
            hydra.rewriting.Rewriting.freeTypeVariablesInTerm(input_),
            hydra.rewriting.Rewriting.freeTypeVariablesInTerm(output_))))));
        hydra.util.Lazy<String> typeParamsStr = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(typeVars.get()),
          () -> "",
          () -> hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
            "<",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Name, String>) (n_ -> hydra.formatting.Formatting.capitalize((n_).value)),
                typeVars.get())),
            "> "))));
        return hydra.lib.eithers.Bind.apply(
          (codec).encodeTerm.apply(input_).apply(g),
          (java.util.function.Function<String, hydra.util.Either<String, hydra.util.ConsList<String>>>) (inputCode -> hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.util.ConsList<String>>) (outputCode -> {
              hydra.util.ConsList<String> assertionLines = hydra.ext.java.testCodec.TestCodec.generateAssertion(
                assertType,
                outputCode,
                inputCode);
              return hydra.lib.lists.Concat2.apply(
                hydra.util.ConsList.of(
                  "    @Test",
                  hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
                    "    public ",
                    typeParamsStr.get(),
                    "void ",
                    formattedName,
                    "() {"))),
                hydra.lib.lists.Concat2.apply(
                  assertionLines,
                  hydra.util.ConsList.of("    }")));
            }),
            (codec).encodeTerm.apply(output_).apply(g))));
      }
    });
  }

  static String getAssertionType(hydra.core.Term term) {
    return hydra.rewriting.Rewriting.deannotateTerm(term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public String otherwise(hydra.core.Term instance) {
        return "assertEquals";
      }

      @Override
      public String visit(hydra.core.Term.Literal lit) {
        return (lit).value.accept(new hydra.core.Literal.PartialVisitor<>() {
          @Override
          public String otherwise(hydra.core.Literal instance) {
            return "assertEquals";
          }

          @Override
          public String visit(hydra.core.Literal.Binary _b) {
            return "assertArrayEquals";
          }

          @Override
          public String visit(hydra.core.Literal.Float_ fv) {
            return (fv).value.accept(new hydra.core.FloatValue.PartialVisitor<>() {
              @Override
              public String otherwise(hydra.core.FloatValue instance) {
                return "assertDoubleEquals";
              }

              @Override
              public String visit(hydra.core.FloatValue.Bigfloat _bf) {
                return "assertBigDecimalEquals";
              }
            });
          }
        });
      }
    });
  }

  static hydra.util.ConsList<String> generateAssertion(String assertType, String outputCode, String inputCode) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        assertType,
        "assertArrayEquals"),
      () -> hydra.util.ConsList.of(
        "        assertArrayEquals(",
        hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "            ",
          outputCode,
          ",")),
        hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "            ",
          inputCode,
          ");"))),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          assertType,
          "assertBigDecimalEquals"),
        () -> hydra.util.ConsList.of(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          "        assertEquals(0, (",
          outputCode,
          ").compareTo(",
          inputCode,
          "));"))),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            assertType,
            "assertDoubleEquals"),
          () -> hydra.util.ConsList.of(
            "        assertEquals(",
            hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "            ",
              outputCode,
              ",")),
            hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "            ",
              inputCode,
              ",")),
            "            1e-15);"),
          () -> hydra.util.ConsList.of(
            "        assertEquals(",
            hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "            ",
              outputCode,
              ",")),
            hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "            ",
              inputCode,
              ");"))))));
  }

  static Boolean isInferenceVar(hydra.core.Name n) {
    String s = (n).value;
    hydra.util.ConsList<Integer> chars = hydra.lib.strings.ToList.apply(s);
    return hydra.lib.logic.And.apply(
      hydra.lib.equality.Equal.apply(
        hydra.lib.strings.CharAt.apply(
          0,
          s),
        116),
      hydra.lib.logic.And.apply(
        hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
          hydra.lib.strings.Length.apply(s),
          1)),
        hydra.lib.lists.Null.apply(hydra.lib.lists.Filter.apply(
          (java.util.function.Function<Integer, Boolean>) (c -> hydra.lib.logic.Not.apply(hydra.lib.logic.And.apply(
            hydra.lib.equality.Gte.apply(
              c,
              48),
            hydra.lib.equality.Lte.apply(
              c,
              57)))),
          hydra.lib.lists.Drop.apply(
            1,
            chars)))));
  }

  static hydra.util.Either<String, hydra.util.Pair<String, String>> generateTestFileWithJavaCodec(hydra.testing.TestCodec codec, hydra.module.Module testModule, hydra.testing.TestGroup testGroup, hydra.graph.Graph g) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<String, hydra.util.Pair<String, String>>) (testBody -> {
        hydra.module.Namespace ns_ = (testModule).namespace;
        hydra.util.ConsList<String> parts = hydra.lib.strings.SplitOn.apply(
          ".",
          (ns_).value);
        hydra.util.Lazy<String> className_ = new hydra.util.Lazy<>(() -> hydra.lib.strings.Cat2.apply(
          hydra.formatting.Formatting.capitalize(hydra.lib.lists.Last.apply(parts)),
          "Test"));
        hydra.util.Lazy<hydra.util.ConsList<String>> dirParts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Drop.apply(
          1,
          hydra.lib.lists.Init.apply(parts)));
        String fileName = hydra.lib.strings.Cat2.apply(
          className_.get(),
          ".java");
        String filePath = hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          hydra.lib.strings.Intercalate.apply(
            "/",
            dirParts.get()),
          "/",
          fileName));
        hydra.util.Lazy<String> testModuleContent = new hydra.util.Lazy<>(() -> hydra.ext.java.testCodec.TestCodec.buildJavaTestModule(
          codec,
          testModule,
          testGroup,
          testBody));
        return (hydra.util.Pair<String, String>) ((hydra.util.Pair<String, String>) (new hydra.util.Pair<String, String>(filePath, testModuleContent.get())));
      }),
      hydra.ext.java.testCodec.TestCodec.generateJavaTestGroupHierarchy(
        g,
        codec,
        (hydra.util.ConsList<String>) (hydra.util.ConsList.<String>empty()),
        testGroup));
  }

  static <T0> String buildJavaTestModule(T0 codec, hydra.module.Module testModule, hydra.testing.TestGroup testGroup, String testBody) {
    hydra.module.Namespace ns_ = (testModule).namespace;
    hydra.util.ConsList<String> parts = hydra.lib.strings.SplitOn.apply(
      ".",
      (ns_).value);
    hydra.util.Lazy<String> className_ = new hydra.util.Lazy<>(() -> hydra.lib.strings.Cat2.apply(
      hydra.formatting.Formatting.capitalize(hydra.lib.lists.Last.apply(parts)),
      "Test"));
    String groupName_ = (testGroup).name;
    hydra.util.Lazy<String> packageName = new hydra.util.Lazy<>(() -> hydra.lib.strings.Intercalate.apply(
      ".",
      hydra.lib.lists.Init.apply(parts)));
    hydra.util.ConsList<String> standardImports = hydra.util.ConsList.of(
      "import org.junit.jupiter.api.Test;",
      "import static org.junit.jupiter.api.Assertions.*;",
      "import java.util.*;",
      "import hydra.util.*;");
    String header = hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      hydra.lib.strings.Cat2.apply(
        "// ",
        hydra.constants.Constants.warningAutoGeneratedFile()),
      "\n",
      hydra.lib.strings.Cat2.apply(
        "// ",
        groupName_),
      "\n\n",
      hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
        "package ",
        packageName.get(),
        ";\n\n")),
      hydra.lib.strings.Intercalate.apply(
        "\n",
        standardImports),
      "\n\n",
      hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
        "public class ",
        className_.get(),
        " {\n\n"))));
    return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      header,
      testBody,
      "\n}\n"));
  }

  static hydra.util.Either<String, hydra.util.Pair<String, String>> generateJavaTestFile(hydra.module.Module testModule, hydra.testing.TestGroup testGroup, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.test.utils.Utils.inferTestGroupTerms(
        g,
        testGroup),
      (java.util.function.Function<hydra.testing.TestGroup, hydra.util.Either<String, hydra.util.Pair<String, String>>>) (inferredTestGroup -> hydra.ext.java.testCodec.TestCodec.generateTestFileWithJavaCodec(
        hydra.ext.java.testCodec.TestCodec.javaTestCodec(),
        testModule,
        inferredTestGroup,
        g)));
  }
}
