// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.testCodec;

/**
 * Python test code generation codec for pytest-based generation tests
 */
public interface TestCodec {
  static hydra.ext.python.helpers.PythonModuleMetadata emptyPythonModuleMetadata(hydra.module.Namespace ns_) {
    return new hydra.ext.python.helpers.PythonModuleMetadata((hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>) (new hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>((hydra.util.Pair<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>) ((hydra.util.Pair<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>) (new hydra.util.Pair<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>(ns_, hydra.ext.python.names.Names.encodeNamespace(ns_)))), (java.util.Map<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>) ((java.util.Map<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>) (hydra.lib.maps.Empty.<hydra.module.Namespace, hydra.ext.python.syntax.DottedName>apply())))), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false);
  }
  
  static <T0> hydra.util.Either<String, String> termToPythonWithContext(hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces_, hydra.graph.Graph graph0, Boolean skipCasts, hydra.core.Term term, T0 _g) {
    return hydra.lib.eithers.Bimap.apply(
      (java.util.function.Function<hydra.context.InContext<hydra.error.Error_>, String>) (ic -> hydra.show.error.Error_.error(((java.util.function.Function<hydra.context.InContext<hydra.error.Error_>, hydra.error.Error_>) (projected -> projected.object)).apply(ic))),
      (java.util.function.Function<hydra.ext.python.syntax.Expression, String>) (arg_ -> hydra.serialization.Serialization.printExpr(hydra.ext.python.serde.Serde.encodeExpression(arg_))),
      hydra.ext.python.coder.Coder.encodeTermInline(
        hydra.lexical.Lexical.emptyContext(),
        new hydra.ext.python.helpers.PythonEnvironment(namespaces_, (hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>>) ((hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>>) (new hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>>((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()), (java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>) ((java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.ext.python.syntax.Name>apply()))))), graph0, (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), hydra.ext.python.utils.Utils.targetPythonVersion(), skipCasts, (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())),
        skipCasts,
        term));
  }
  
  static hydra.util.Either<String, String> termToPython(hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces_, hydra.core.Term term, hydra.graph.Graph g) {
    return hydra.ext.python.testCodec.TestCodec.termToPythonWithContext(
      namespaces_,
      g,
      false,
      term,
      g);
  }
  
  static hydra.util.Either<String, String> typeToPython(hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces_, hydra.core.Type typ, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bimap.apply(
      (java.util.function.Function<hydra.context.InContext<hydra.error.Error_>, String>) (ic -> hydra.show.error.Error_.error(((java.util.function.Function<hydra.context.InContext<hydra.error.Error_>, hydra.error.Error_>) (projected -> projected.object)).apply(ic))),
      (java.util.function.Function<hydra.ext.python.syntax.Expression, String>) (arg_ -> hydra.serialization.Serialization.printExpr(hydra.ext.python.serde.Serde.encodeExpression(arg_))),
      hydra.ext.python.coder.Coder.encodeType(
        new hydra.ext.python.helpers.PythonEnvironment(namespaces_, (hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>>) ((hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>>) (new hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>>((java.util.List<hydra.core.Name>) (java.util.List.<hydra.core.Name>of()), (java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>) ((java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.ext.python.syntax.Name>apply()))))), g, (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), hydra.ext.python.utils.Utils.targetPythonVersion(), false, (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())),
        typ));
  }
  
  static hydra.testing.TestCodec pythonTestCodec(hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces_) {
    return new hydra.testing.TestCodec(new hydra.coders.LanguageName("python"), new hydra.module.FileExtension("py"), (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, String>>>) (v1 -> (java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, String>>) (v2 -> hydra.ext.python.testCodec.TestCodec.termToPython(
      namespaces_,
      v1,
      v2))), (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, String>>>) (v1 -> (java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, String>>) (v2 -> hydra.ext.python.testCodec.TestCodec.typeToPython(
      namespaces_,
      v1,
      v2))), hydra.ext.python.testCodec.TestCodec::formatPythonTestName, hydra.ext.python.testCodec.TestCodec::namespaceToPythonModuleName, hydra.ext.python.testCodec.TestCodec.pythonTestCaseTemplate(), hydra.ext.python.testCodec.TestCodec.pythonTestGroupTemplate(), hydra.ext.python.testCodec.TestCodec.pythonModuleTemplate(), hydra.ext.python.testCodec.TestCodec.pythonImportTemplate(), (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.List<String>>) (v1 -> hydra.ext.python.testCodec.TestCodec.findPythonImports(
      namespaces_,
      v1)));
  }
  
  static hydra.testing.TestCodec pythonTestCodecWithContext(hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces_, hydra.graph.Graph tcontext) {
    return new hydra.testing.TestCodec(new hydra.coders.LanguageName("python"), new hydra.module.FileExtension("py"), (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, String>>>) (v1 -> (java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, String>>) (v2 -> hydra.ext.python.testCodec.TestCodec.termToPythonWithContext(
      namespaces_,
      tcontext,
      true,
      v1,
      v2))), (java.util.function.Function<hydra.core.Type, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, String>>>) (v1 -> (java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, String>>) (v2 -> hydra.ext.python.testCodec.TestCodec.typeToPython(
      namespaces_,
      v1,
      v2))), hydra.ext.python.testCodec.TestCodec::formatPythonTestName, hydra.ext.python.testCodec.TestCodec::namespaceToPythonModuleName, hydra.ext.python.testCodec.TestCodec.pythonTestCaseTemplate(), hydra.ext.python.testCodec.TestCodec.pythonTestGroupTemplate(), hydra.ext.python.testCodec.TestCodec.pythonModuleTemplate(), hydra.ext.python.testCodec.TestCodec.pythonImportTemplate(), (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.List<String>>) (v1 -> hydra.ext.python.testCodec.TestCodec.findPythonImports(
      namespaces_,
      v1)));
  }
  
  static String formatPythonTestName(String name) {
    return hydra.lib.strings.Cat2.apply(
      "test_",
      hydra.lib.strings.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<Integer, Integer>) (c -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.chars.IsAlphaNum.apply(c),
          () -> hydra.lib.chars.ToLower.apply(c),
          () -> 95)),
        hydra.lib.strings.ToList.apply(name))));
  }
  
  static String namespaceToPythonModuleName(hydra.module.Namespace ns_) {
    return (ns_).value;
  }
  
  static String pythonTestCaseTemplate() {
    return hydra.lib.strings.Intercalate.apply(
      "\n",
      java.util.List.of(
        "def {name}():",
        "    assert ({input}) == ({output})"));
  }
  
  static String pythonTestGroupTemplate() {
    return "# {groupName}";
  }
  
  static String pythonModuleTemplate() {
    return hydra.lib.strings.Intercalate.apply(
      "\n",
      java.util.List.of(
        hydra.lib.strings.Cat2.apply(
          "# ",
          hydra.constants.Constants.warningAutoGeneratedFile()),
        "",
        "{imports}",
        "",
        "{testGroup}",
        "",
        "{testCases}"));
  }
  
  static String pythonImportTemplate() {
    return "import {namespace}";
  }
  
  static <T0, T1> java.util.List<String> findPythonImports(hydra.module.Namespaces<T0> namespaces_, T1 names_) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.module.Namespace, T0>, String>) (entry -> hydra.lib.strings.Cat2.apply(
        "import ",
        (hydra.lib.pairs.First.apply(entry)).value)),
      hydra.lib.maps.ToList.apply(hydra.ext.python.testCodec.TestCodec.<T0>findPythonImports_filtered(namespaces_)));
  }
  
  static <T0> java.util.Map<hydra.module.Namespace, T0> findPythonImports_mapping_(hydra.module.Namespaces<T0> namespaces_) {
    return ((java.util.function.Function<hydra.module.Namespaces<T0>, java.util.Map<hydra.module.Namespace, T0>>) (projected -> projected.mapping)).apply(namespaces_);
  }
  
  static <T0> java.util.Map<hydra.module.Namespace, T0> findPythonImports_filtered(hydra.module.Namespaces<T0> namespaces_) {
    return hydra.lib.maps.FilterWithKey.apply(
      (java.util.function.Function<hydra.module.Namespace, java.util.function.Function<T0, Boolean>>) (ns_ -> (java.util.function.Function<T0, Boolean>) (_v -> hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Head.apply(hydra.lib.strings.SplitOn.apply(
          "hydra.test.",
          (ns_).value)),
        "")))),
      hydra.ext.python.testCodec.TestCodec.<T0>findPythonImports_mapping_(namespaces_));
  }
  
  static <T0> hydra.util.Either<T0, hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>> namespacesForPythonModule(hydra.module.Module mod, hydra.graph.Graph graph_) {
    java.util.List<hydra.core.Binding> bindings = hydra.lexical.Lexical.graphToBindings(graph_);
    hydra.util.Lazy<java.util.List<hydra.module.Definition>> defs = new hydra.util.Lazy<>(() -> hydra.lib.maybes.MapMaybe.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.util.Maybe<hydra.module.Definition>>) (b -> hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.core.TypeScheme, hydra.module.Definition>) (ts -> new hydra.module.Definition.Term(new hydra.module.TermDefinition((b).name, (b).term, ts))),
        (b).type)),
      bindings));
    return hydra.util.Either.<T0, hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>>right(hydra.ext.python.utils.Utils.findNamespaces(
      (mod).namespace,
      defs.get()));
  }
  
  static <T0> hydra.util.Either<String, String> generatePythonTestGroupHierarchy(hydra.graph.Graph g, T0 namespaces_, hydra.testing.TestCodec codec, java.util.List<String> groupPath, hydra.testing.TestGroup testGroup) {
    java.util.List<hydra.testing.TestCaseWithMetadata> cases_ = (testGroup).cases;
    java.util.List<hydra.testing.TestGroup> subgroups = (testGroup).subgroups;
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.testing.TestCaseWithMetadata, hydra.util.Either<String, java.util.List<String>>>) (tc -> hydra.ext.python.testCodec.TestCodec.<T0>generatePythonTestCase(
          g,
          namespaces_,
          codec,
          groupPath,
          tc)),
        cases_),
      (java.util.function.Function<java.util.List<java.util.List<String>>, hydra.util.Either<String, String>>) (testCaseLines -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.testing.TestGroup, hydra.util.Either<String, String>>) (subgroup -> {
            String groupName = (subgroup).name;
            String header = hydra.lib.strings.Cat2.apply(
              "# ",
              groupName);
            return hydra.lib.eithers.Map.apply(
              (java.util.function.Function<String, String>) (content -> hydra.lib.strings.Cat.apply(java.util.List.of(
                header,
                "\n\n",
                content))),
              hydra.ext.python.testCodec.TestCodec.<T0>generatePythonTestGroupHierarchy(
                g,
                namespaces_,
                codec,
                hydra.lib.lists.Concat2.apply(
                  groupPath,
                  java.util.List.of(groupName)),
                subgroup));
          }),
          subgroups),
        (java.util.function.Function<java.util.List<String>, hydra.util.Either<String, String>>) (subgroupBlocks -> {
          String subgroupsStr = hydra.lib.strings.Intercalate.apply(
            "\n\n",
            subgroupBlocks);
          hydra.util.Lazy<String> testCasesStr = new hydra.util.Lazy<>(() -> hydra.lib.strings.Intercalate.apply(
            "\n\n",
            hydra.lib.lists.Concat.apply(testCaseLines)));
          return hydra.util.Either.<String, String>right(hydra.lib.strings.Cat.apply(java.util.List.of(
            testCasesStr.get(),
            hydra.lib.logic.IfElse.lazy(
              hydra.lib.logic.Or.apply(
                hydra.lib.equality.Equal.apply(
                  testCasesStr.get(),
                  ""),
                hydra.lib.equality.Equal.apply(
                  subgroupsStr,
                  "")),
              () -> "",
              () -> "\n\n"),
            subgroupsStr)));
        }))));
  }
  
  static <T0> hydra.util.Either<String, java.util.List<String>> generatePythonTestCase(hydra.graph.Graph g, T0 namespaces_, hydra.testing.TestCodec codec, java.util.List<String> groupPath, hydra.testing.TestCaseWithMetadata tcm) {
    String name_ = (tcm).name;
    hydra.testing.TestCase tcase = (tcm).case_;
    return (tcase).accept(new hydra.testing.TestCase.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, java.util.List<String>> otherwise(hydra.testing.TestCase instance) {
        return hydra.util.Either.<String, java.util.List<String>>right((java.util.List<String>) (java.util.List.<String>of()));
      }
      
      @Override
      public hydra.util.Either<String, java.util.List<String>> visit(hydra.testing.TestCase.DelegatedEvaluation delCase) {
        hydra.util.Lazy<String> fullName = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.lists.Null.apply(groupPath),
          () -> name_,
          () -> hydra.lib.strings.Intercalate.apply(
            "__",
            hydra.lib.lists.Concat2.apply(
              groupPath,
              java.util.List.of(name_)))));
        String formattedName = ((codec).formatTestName).apply(fullName.get());
        hydra.core.Term input_ = ((delCase).value).input;
        hydra.core.Term output_ = ((delCase).value).output;
        return hydra.lib.eithers.Bind.apply(
          (((codec).encodeTerm).apply(input_)).apply(g),
          (java.util.function.Function<String, hydra.util.Either<String, java.util.List<String>>>) (inputCode -> hydra.lib.eithers.Bind.apply(
            (((codec).encodeTerm).apply(output_)).apply(g),
            (java.util.function.Function<String, hydra.util.Either<String, java.util.List<String>>>) (outputCode -> hydra.util.Either.<String, java.util.List<String>>right(java.util.List.of(
              hydra.lib.strings.Cat.apply(java.util.List.of(
                "def ",
                formattedName,
                "():")),
              hydra.lib.strings.Cat.apply(java.util.List.of(
                "    assert (",
                inputCode,
                ") == (",
                outputCode,
                ")"))))))));
      }
    });
  }
  
  static <T0> hydra.util.Either<String, hydra.util.Pair<String, String>> generateTestFileWithPythonCodec(hydra.testing.TestCodec codec, hydra.module.Module testModule, hydra.testing.TestGroup testGroup, T0 namespaces_, hydra.graph.Graph g) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<String, hydra.util.Pair<String, String>>) (testBody -> {
        hydra.module.Namespace ns_ = (testModule).namespace;
        java.util.List<String> parts = hydra.lib.strings.SplitOn.apply(
          ".",
          (ns_).value);
        hydra.util.Lazy<java.util.List<String>> dirParts = new hydra.util.Lazy<>(() -> hydra.lib.lists.Init.apply(parts));
        hydra.util.Lazy<String> fileName = new hydra.util.Lazy<>(() -> hydra.lib.strings.Cat.apply(java.util.List.of(
          "test_",
          hydra.lib.lists.Last.apply(parts),
          ".py")));
        String filePath = hydra.lib.strings.Cat.apply(java.util.List.of(
          hydra.lib.strings.Intercalate.apply(
            "/",
            dirParts.get()),
          "/",
          fileName.get()));
        hydra.util.Lazy<String> testModuleContent = new hydra.util.Lazy<>(() -> hydra.ext.python.testCodec.TestCodec.buildPythonTestModule(
          codec,
          testModule,
          testGroup,
          testBody,
          namespaces_));
        return (hydra.util.Pair<String, String>) ((hydra.util.Pair<String, String>) (new hydra.util.Pair<String, String>(filePath, testModuleContent.get())));
      }),
      hydra.ext.python.testCodec.TestCodec.<T0>generatePythonTestGroupHierarchy(
        g,
        namespaces_,
        codec,
        (java.util.List<String>) (java.util.List.<String>of()),
        testGroup));
  }
  
  static <T0, T1> String buildPythonTestModule(hydra.testing.TestCodec codec, T0 testModule, hydra.testing.TestGroup testGroup, String testBody, T1 namespaces_) {
    hydra.util.Lazy<java.util.List<String>> domainImports = new hydra.util.Lazy<>(() -> ((codec).findImports).apply((java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())));
    java.util.List<String> standardImports = java.util.List.of(
      "from __future__ import annotations",
      "from typing import cast",
      "from decimal import Decimal",
      "from hydra.dsl.python import FrozenDict, frozenlist, Either, Left, Right, Maybe, Just, Nothing");
    hydra.util.Lazy<java.util.List<String>> allImports = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
      standardImports,
      domainImports.get()));
    String groupName_ = (testGroup).name;
    String header = hydra.lib.strings.Cat.apply(java.util.List.of(
      hydra.lib.strings.Cat2.apply(
        "# ",
        hydra.constants.Constants.warningAutoGeneratedFile()),
      "\n",
      hydra.lib.strings.Cat2.apply(
        "# ",
        groupName_),
      "\n\n",
      hydra.lib.strings.Intercalate.apply(
        "\n",
        allImports.get()),
      "\n\n"));
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      header,
      testBody,
      "\n"));
  }
  
  static hydra.util.Either<String, hydra.util.Pair<String, String>> generatePythonTestFile(hydra.module.Module testModule, hydra.testing.TestGroup testGroup, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.test.utils.Utils.inferTestGroupTerms(
        g,
        testGroup),
      (java.util.function.Function<hydra.testing.TestGroup, hydra.util.Either<String, hydra.util.Pair<String, String>>>) (inferredTestGroup -> hydra.lib.eithers.Bind.apply(
        hydra.ext.python.testCodec.TestCodec.namespacesForPythonModule(
          testModule,
          g),
        (java.util.function.Function<hydra.module.Namespaces<hydra.ext.python.syntax.DottedName>, hydra.util.Either<String, hydra.util.Pair<String, String>>>) (namespaces_ -> hydra.ext.python.testCodec.TestCodec.generateTestFileWithPythonCodec(
          hydra.ext.python.testCodec.TestCodec.pythonTestCodecWithContext(
            namespaces_,
            g),
          testModule,
          inferredTestGroup,
          namespaces_,
          g)))));
  }
}
