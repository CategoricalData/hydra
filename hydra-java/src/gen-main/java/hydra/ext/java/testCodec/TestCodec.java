// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.testCodec;

/**
 * Java test code generation codec for JUnit-based generation tests
 */
public interface TestCodec {
  static hydra.util.Either<String, String> termToJava(hydra.core.Term term, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bimap.apply(
      (java.util.function.Function<hydra.context.InContext<hydra.error.OtherError>, String>) (ic -> (((java.util.function.Function<hydra.context.InContext<hydra.error.OtherError>, hydra.error.OtherError>) (projected -> projected.object)).apply(ic)).value),
      (java.util.function.Function<hydra.ext.java.syntax.Expression, String>) (arg_ -> hydra.serialization.Serialization.printExpr(hydra.serialization.Serialization.parenthesize(hydra.ext.java.serde.Serde.writeExpression(arg_)))),
      hydra.ext.java.coder.Coder.encodeTerm(
        new hydra.ext.java.helpers.JavaEnvironment(new hydra.ext.java.helpers.Aliases(new hydra.module.Namespace("test"), (java.util.Map<hydra.module.Namespace, hydra.ext.java.syntax.PackageName>) ((java.util.Map<hydra.module.Namespace, hydra.ext.java.syntax.PackageName>) (hydra.lib.maps.Empty.<hydra.module.Namespace, hydra.ext.java.syntax.PackageName>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Map<hydra.core.Name, hydra.core.Name>) ((java.util.Map<hydra.core.Name, hydra.core.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Name>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (hydra.util.Maybe<hydra.core.Type>) (hydra.util.Maybe.<hydra.core.Type>nothing()), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())), g),
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
      p1), hydra.ext.java.testCodec.TestCodec::formatJavaTestName, hydra.ext.java.testCodec.TestCodec::namespaceToJavaClassName, hydra.ext.java.testCodec.TestCodec.javaTestCaseTemplate(), hydra.ext.java.testCodec.TestCodec.javaTestGroupTemplate(), hydra.ext.java.testCodec.TestCodec.javaModuleTemplate(), hydra.ext.java.testCodec.TestCodec.javaImportTemplate(), (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.List<String>>) (_names -> hydra.ext.java.testCodec.TestCodec.findJavaImports()));
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
      java.util.List.of(
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
      java.util.List.of(
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
  
  static java.util.List<String> findJavaImports() {
    return java.util.List.of(
      "import org.junit.jupiter.api.Test;",
      "import static org.junit.jupiter.api.Assertions.*;",
      "import java.util.*;");
  }
  
  static hydra.util.Either<String, String> generateJavaTestGroupHierarchy(hydra.graph.Graph g, hydra.testing.TestCodec codec, java.util.List<String> groupPath, hydra.testing.TestGroup testGroup) {
    java.util.List<hydra.testing.TestCaseWithMetadata> cases_ = (testGroup).cases;
    java.util.List<hydra.testing.TestGroup> subgroups = (testGroup).subgroups;
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Map.apply(
        (java.util.function.Function<java.util.List<java.util.List<String>>, String>) (lines_ -> hydra.lib.strings.Intercalate.apply(
          "\n\n",
          hydra.lib.lists.Concat.apply(lines_))),
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.testing.TestCaseWithMetadata, hydra.util.Either<String, java.util.List<String>>>) (tc -> hydra.ext.java.testCodec.TestCodec.generateJavaTestCase(
            g,
            codec,
            groupPath,
            tc)),
          cases_)),
      (java.util.function.Function<String, hydra.util.Either<String, String>>) (testCasesStr -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<String, String>) (subgroupsStr -> hydra.lib.strings.Cat.apply(java.util.List.of(
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
          (java.util.function.Function<java.util.List<String>, String>) (blocks -> hydra.lib.strings.Intercalate.apply(
            "\n\n",
            blocks)),
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.testing.TestGroup, hydra.util.Either<String, String>>) (subgroup -> {
              String groupName = (subgroup).name;
              String header = hydra.lib.strings.Cat2.apply(
                "    // ",
                groupName);
              return hydra.lib.eithers.Map.apply(
                (java.util.function.Function<String, String>) (content -> hydra.lib.strings.Cat.apply(java.util.List.of(
                  header,
                  "\n\n",
                  content))),
                hydra.ext.java.testCodec.TestCodec.generateJavaTestGroupHierarchy(
                  g,
                  codec,
                  hydra.lib.lists.Concat2.apply(
                    groupPath,
                    java.util.List.of(groupName)),
                  subgroup));
            }),
            subgroups)))));
  }
  
  static hydra.util.Either<String, java.util.List<String>> generateJavaTestCase(hydra.graph.Graph g, hydra.testing.TestCodec codec, java.util.List<String> groupPath, hydra.testing.TestCaseWithMetadata tcm) {
    String name_ = (tcm).name;
    hydra.testing.TestCase tcase = (tcm).case_;
    return (tcase).accept(new hydra.testing.TestCase.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, java.util.List<String>> otherwise(hydra.testing.TestCase instance) {
        return hydra.util.Either.<String, java.util.List<String>>right((java.util.List<String>) (java.util.List.<String>of()));
      }
      
      @Override
      public hydra.util.Either<String, java.util.List<String>> visit(hydra.testing.TestCase.DelegatedEvaluation delCase) {
        hydra.core.Term output_ = ((delCase).value).output;
        String assertType = hydra.ext.java.testCodec.TestCodec.getAssertionType(output_);
        hydra.util.Lazy<String> fullName = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(groupPath),
          () -> name_,
          () -> hydra.lib.strings.Intercalate.apply(
            "_",
            hydra.lib.lists.Concat2.apply(
              groupPath,
              java.util.List.of(name_)))));
        String formattedName = ((codec).formatTestName).apply(fullName.get());
        hydra.core.Term input_ = ((delCase).value).input;
        hydra.util.Lazy<java.util.List<hydra.core.Name>> typeVars = new hydra.util.Lazy<>(() -> hydra.lib.lists.Sort.apply(hydra.lib.lists.Filter.apply(
          hydra.ext.java.testCodec.TestCodec::isInferenceVar,
          hydra.lib.sets.ToList.apply(hydra.lib.sets.Union.apply(
            hydra.rewriting.Rewriting.freeTypeVariablesInTerm(input_),
            hydra.rewriting.Rewriting.freeTypeVariablesInTerm(output_))))));
        hydra.util.Lazy<String> typeParamsStr = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(typeVars.get()),
          () -> "",
          () -> hydra.lib.strings.Cat.apply(java.util.List.of(
            "<",
            hydra.lib.strings.Intercalate.apply(
              ", ",
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<hydra.core.Name, String>) (n_ -> hydra.formatting.Formatting.capitalize((n_).value)),
                typeVars.get())),
            "> "))));
        return hydra.lib.eithers.Bind.apply(
          (((codec).encodeTerm).apply(input_)).apply(g),
          (java.util.function.Function<String, hydra.util.Either<String, java.util.List<String>>>) (inputCode -> hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, java.util.List<String>>) (outputCode -> {
              java.util.List<String> assertionLines = hydra.ext.java.testCodec.TestCodec.generateAssertion(
                assertType,
                outputCode,
                inputCode);
              return hydra.lib.lists.Concat2.apply(
                java.util.List.of(
                  "    @Test",
                  hydra.lib.strings.Cat.apply(java.util.List.of(
                    "    public ",
                    typeParamsStr.get(),
                    "void ",
                    formattedName,
                    "() {"))),
                hydra.lib.lists.Concat2.apply(
                  assertionLines,
                  java.util.List.of("    }")));
            }),
            (((codec).encodeTerm).apply(output_)).apply(g))));
      }
    });
  }
  
  static String getAssertionType(hydra.core.Term term) {
    return (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public String otherwise(hydra.core.Term instance) {
        return "assertEquals";
      }
      
      @Override
      public String visit(hydra.core.Term.Literal lit) {
        return ((lit).value).accept(new hydra.core.Literal.PartialVisitor<>() {
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
            return ((fv).value).accept(new hydra.core.FloatValue.PartialVisitor<>() {
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
  
  static java.util.List<String> generateAssertion(String assertType, String outputCode, String inputCode) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        assertType,
        "assertArrayEquals"),
      () -> java.util.List.of(
        "        assertArrayEquals(",
        hydra.lib.strings.Cat.apply(java.util.List.of(
          "            ",
          outputCode,
          ",")),
        hydra.lib.strings.Cat.apply(java.util.List.of(
          "            ",
          inputCode,
          ");"))),
      () -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          assertType,
          "assertBigDecimalEquals"),
        () -> java.util.List.of(hydra.lib.strings.Cat.apply(java.util.List.of(
          "        assertEquals(0, (",
          outputCode,
          ").compareTo(",
          inputCode,
          "));"))),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            assertType,
            "assertDoubleEquals"),
          () -> java.util.List.of(
            "        assertEquals(",
            hydra.lib.strings.Cat.apply(java.util.List.of(
              "            ",
              outputCode,
              ",")),
            hydra.lib.strings.Cat.apply(java.util.List.of(
              "            ",
              inputCode,
              ",")),
            "            1e-15);"),
          () -> java.util.List.of(
            "        assertEquals(",
            hydra.lib.strings.Cat.apply(java.util.List.of(
              "            ",
              outputCode,
              ",")),
            hydra.lib.strings.Cat.apply(java.util.List.of(
              "            ",
              inputCode,
              ");"))))));
  }
  
  static Boolean isInferenceVar(hydra.core.Name n) {
    String s = (n).value;
    java.util.List<Integer> chars = hydra.lib.strings.ToList.apply(s);
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
        java.util.List<String> parts = hydra.lib.strings.SplitOn.apply(
          ".",
          (ns_).value);
        hydra.util.Lazy<String> className_ = new hydra.util.Lazy<>(() -> hydra.lib.strings.Cat2.apply(
          hydra.formatting.Formatting.capitalize(hydra.lib.lists.Last.apply(parts)),
          "Test"));
        hydra.util.Lazy<java.util.List<String>> dirParts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Drop.apply(
          1,
          hydra.lib.lists.Init.apply(parts)));
        String fileName = hydra.lib.strings.Cat2.apply(
          className_.get(),
          ".java");
        String filePath = hydra.lib.strings.Cat.apply(java.util.List.of(
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
        (java.util.List<String>) (java.util.List.<String>of()),
        testGroup));
  }
  
  static <T0> String buildJavaTestModule(T0 codec, hydra.module.Module testModule, hydra.testing.TestGroup testGroup, String testBody) {
    hydra.module.Namespace ns_ = (testModule).namespace;
    java.util.List<String> parts = hydra.lib.strings.SplitOn.apply(
      ".",
      (ns_).value);
    hydra.util.Lazy<String> className_ = new hydra.util.Lazy<>(() -> hydra.lib.strings.Cat2.apply(
      hydra.formatting.Formatting.capitalize(hydra.lib.lists.Last.apply(parts)),
      "Test"));
    String groupName_ = (testGroup).name;
    hydra.util.Lazy<String> packageName = new hydra.util.Lazy<>(() -> hydra.lib.strings.Intercalate.apply(
      ".",
      hydra.lib.lists.Init.apply(parts)));
    java.util.List<String> standardImports = java.util.List.of(
      "import org.junit.jupiter.api.Test;",
      "import static org.junit.jupiter.api.Assertions.*;",
      "import java.util.*;",
      "import hydra.util.*;");
    String header = hydra.lib.strings.Cat.apply(java.util.List.of(
      hydra.lib.strings.Cat2.apply(
        "// ",
        hydra.constants.Constants.warningAutoGeneratedFile()),
      "\n",
      hydra.lib.strings.Cat2.apply(
        "// ",
        groupName_),
      "\n\n",
      hydra.lib.strings.Cat.apply(java.util.List.of(
        "package ",
        packageName.get(),
        ";\n\n")),
      hydra.lib.strings.Intercalate.apply(
        "\n",
        standardImports),
      "\n\n",
      hydra.lib.strings.Cat.apply(java.util.List.of(
        "public class ",
        className_.get(),
        " {\n\n"))));
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      header,
      testBody,
      "\n}\n"));
  }
  
  static hydra.util.Either<String, hydra.util.Pair<String, String>> generateJavaTestFile(hydra.module.Module testModule, hydra.testing.TestGroup testGroup, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.java.testCodec.TestCodec.inferTestGroupTerms(
        g,
        testGroup),
      (java.util.function.Function<hydra.testing.TestGroup, hydra.util.Either<String, hydra.util.Pair<String, String>>>) (inferredTestGroup -> hydra.ext.java.testCodec.TestCodec.generateTestFileWithJavaCodec(
        hydra.ext.java.testCodec.TestCodec.javaTestCodec(),
        testModule,
        inferredTestGroup,
        g)));
  }
  
  static hydra.util.Either<String, hydra.testing.TestGroup> inferTestGroupTerms(hydra.graph.Graph g, hydra.testing.TestGroup tg) {
    java.util.List<hydra.testing.TestCaseWithMetadata> cases_ = (tg).cases;
    hydra.util.Maybe<String> desc = (tg).description;
    String name_ = (tg).name;
    java.util.List<hydra.testing.TestGroup> subgroups = (tg).subgroups;
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.testing.TestGroup, hydra.util.Either<String, hydra.testing.TestGroup>>) (sg -> hydra.ext.java.testCodec.TestCodec.inferTestGroupTerms(
          g,
          sg)),
        subgroups),
      (java.util.function.Function<java.util.List<hydra.testing.TestGroup>, hydra.util.Either<String, hydra.testing.TestGroup>>) (inferredSubgroups -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<java.util.List<hydra.testing.TestCaseWithMetadata>, hydra.testing.TestGroup>) (inferredCases -> new hydra.testing.TestGroup(name_, desc, inferredSubgroups, inferredCases)),
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.testing.TestCaseWithMetadata, hydra.util.Either<String, hydra.testing.TestCaseWithMetadata>>) (tc -> hydra.ext.java.testCodec.TestCodec.inferTestCase(
            g,
            tc)),
          cases_))));
  }
  
  static hydra.util.Either<String, hydra.testing.TestCaseWithMetadata> inferTestCase(hydra.graph.Graph g, hydra.testing.TestCaseWithMetadata tcm) {
    hydra.util.Maybe<String> desc = (tcm).description;
    String name_ = (tcm).name;
    java.util.List<hydra.testing.Tag> tags_ = (tcm).tags;
    hydra.testing.TestCase tcase = (tcm).case_;
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.testing.TestCase, hydra.testing.TestCaseWithMetadata>) (inferredCase -> new hydra.testing.TestCaseWithMetadata(name_, inferredCase, desc, tags_)),
      (tcase).accept(new hydra.testing.TestCase.PartialVisitor<>() {
        @Override
        public hydra.util.Either<String, hydra.testing.TestCase> otherwise(hydra.testing.TestCase instance) {
          return hydra.util.Either.<String, hydra.testing.TestCase>right(tcase);
        }
        
        @Override
        public hydra.util.Either<String, hydra.testing.TestCase> visit(hydra.testing.TestCase.DelegatedEvaluation delCase) {
          hydra.core.Term input_ = ((delCase).value).input;
          hydra.core.Term output_ = ((delCase).value).output;
          return hydra.lib.eithers.Bind.apply(
            hydra.ext.java.testCodec.TestCodec.inferTerm(
              g,
              input_),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<String, hydra.testing.TestCase>>) (inferredInput -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Term, hydra.testing.TestCase>) (inferredOutput -> new hydra.testing.TestCase.DelegatedEvaluation(new hydra.testing.DelegatedEvaluationTestCase(inferredInput, inferredOutput))),
              hydra.ext.java.testCodec.TestCodec.inferTerm(
                g,
                output_))));
        }
      }));
  }
  
  static hydra.util.Either<String, hydra.core.Term> inferTerm(hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bimap.apply(
      (java.util.function.Function<hydra.context.InContext<hydra.error.OtherError>, String>) (ic -> (((java.util.function.Function<hydra.context.InContext<hydra.error.OtherError>, hydra.error.OtherError>) (projected -> projected.object)).apply(ic)).value),
      (java.util.function.Function<hydra.typing.InferenceResult, hydra.core.Term>) (x -> (x).term),
      hydra.inference.Inference.inferInGraphContext(
        hydra.lexical.Lexical.emptyContext(),
        g,
        term));
  }
}
