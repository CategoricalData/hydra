// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell;

/**
 * Haskell test code generation for HSpec-based generation tests
 */
public interface Testing {
  static hydra.packaging.Namespaces<hydra.ext.haskell.syntax.ModuleName> addNamespacesToNamespaces(hydra.packaging.Namespaces<hydra.ext.haskell.syntax.ModuleName> ns0, java.util.Set<hydra.core.Name> names) {
    hydra.util.Lazy<java.util.Set<hydra.packaging.Namespace>> newNamespaces = new hydra.util.Lazy<>(() -> hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.lib.lists.Map.apply(
      hydra.Names::namespaceOf,
      hydra.lib.sets.ToList.apply(names)))));
    java.util.function.Function<hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName> toModuleName = (java.util.function.Function<hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName>) (namespace -> new hydra.ext.haskell.syntax.ModuleName(hydra.Formatting.capitalize(hydra.lib.lists.Last.apply(hydra.lib.strings.SplitOn.apply(
      ".",
      (namespace).value)))));
    hydra.util.Lazy<java.util.Map<hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName>> newMappings = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.packaging.Namespace, hydra.util.Pair<hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName>>) (ns_ -> (hydra.util.Pair<hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName>) ((hydra.util.Pair<hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName>) (new hydra.util.Pair<hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName>(ns_, (toModuleName).apply(ns_))))),
      hydra.lib.sets.ToList.apply(newNamespaces.get()))));
    return (hydra.packaging.Namespaces<hydra.ext.haskell.syntax.ModuleName>) (new hydra.packaging.Namespaces<hydra.ext.haskell.syntax.ModuleName>(((java.util.function.Function<hydra.packaging.Namespaces<hydra.ext.haskell.syntax.ModuleName>, hydra.util.Pair<hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName>>) (projected -> projected.focus)).apply(ns0), hydra.lib.maps.Union.apply(
      ((java.util.function.Function<hydra.packaging.Namespaces<hydra.ext.haskell.syntax.ModuleName>, java.util.Map<hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName>>) (projected -> projected.mapping)).apply(ns0),
      newMappings.get())));
  }

  static hydra.util.Either<String, hydra.packaging.Namespaces<hydra.ext.haskell.syntax.ModuleName>> buildNamespacesForTestGroup(hydra.packaging.Module mod, hydra.testing.TestGroup tgroup, hydra.graph.Graph graph_) {
    java.util.List<hydra.testing.TestCaseWithMetadata> testCases_ = hydra.ext.haskell.Testing.collectTestCases(tgroup);
    hydra.util.Lazy<java.util.List<hydra.core.Binding>> testBindings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Term, hydra.core.Binding>) (term -> new hydra.core.Binding(new hydra.core.Name("_test_"), term, (hydra.util.Maybe<hydra.core.TypeScheme>) (hydra.util.Maybe.<hydra.core.TypeScheme>nothing()))),
      hydra.ext.haskell.Testing.buildNamespacesForTestGroup_testTerms(testCases_)));
    hydra.util.Lazy<hydra.packaging.Module> tempModule = new hydra.util.Lazy<>(() -> new hydra.packaging.Module((mod).namespace, hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Binding, hydra.packaging.Definition>) (b -> new hydra.packaging.Definition.Term(new hydra.packaging.TermDefinition((b).name, (b).term, (b).type))),
      testBindings.get()), (mod).termDependencies, (mod).typeDependencies, (mod).description));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.errors.Error_, String>) (e -> hydra.show.Errors.error(e)),
        (java.util.function.Function<hydra.packaging.Namespaces<hydra.ext.haskell.syntax.ModuleName>, hydra.packaging.Namespaces<hydra.ext.haskell.syntax.ModuleName>>) (a -> a),
        hydra.ext.haskell.Utils.namespacesForModule(
          tempModule.get(),
          hydra.Lexical.emptyContext(),
          graph_)),
      (java.util.function.Function<hydra.packaging.Namespaces<hydra.ext.haskell.syntax.ModuleName>, hydra.util.Either<String, hydra.packaging.Namespaces<hydra.ext.haskell.syntax.ModuleName>>>) (baseNamespaces -> {
        hydra.util.Lazy<java.util.Set<hydra.core.Name>> encodedNames = new hydra.util.Lazy<>(() -> hydra.lib.sets.Unions.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>>) (t -> hydra.ext.haskell.Testing.extractEncodedTermVariableNames(
            graph_,
            t)),
          hydra.ext.haskell.Testing.buildNamespacesForTestGroup_testTerms(testCases_))));
        return hydra.util.Either.<String, hydra.packaging.Namespaces<hydra.ext.haskell.syntax.ModuleName>>right(hydra.ext.haskell.Testing.addNamespacesToNamespaces(
          baseNamespaces,
          encodedNames.get()));
      }));
  }

  static <T0> java.util.List<T0> buildNamespacesForTestGroup_testTerms(java.util.List<hydra.testing.TestCaseWithMetadata> testCases_) {
    return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      p0 -> hydra.ext.haskell.Testing.<hydra.testing.TestCaseWithMetadata, T0>extractTestTerms(p0),
      testCases_));
  }

  static <T0> String buildTestModule(hydra.packaging.Module testModule, hydra.testing.TestGroup testGroup, String testBody, hydra.packaging.Namespaces<hydra.ext.haskell.syntax.ModuleName> namespaces) {
    hydra.util.Lazy<java.util.List<String>> domainImports = new hydra.util.Lazy<>(() -> hydra.ext.haskell.Testing.findHaskellImports(
      namespaces,
      (java.util.Set<T0>) (hydra.lib.sets.Empty.<T0>apply())));
    java.util.List<String> standardImports = java.util.Arrays.asList(
      "import Hydra.Kernel",
      "import qualified Test.Hspec as H",
      "import qualified Data.List as L",
      "import qualified Data.Map as M",
      "import qualified Data.Set as S",
      "import qualified Data.Maybe as Y");
    hydra.util.Lazy<java.util.List<String>> allImports = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
      standardImports,
      domainImports.get()));
    String groupName_ = (testGroup).name;
    hydra.packaging.Namespace ns_ = (testModule).namespace;
    hydra.packaging.Namespace specNs = new hydra.packaging.Namespace(hydra.lib.strings.Cat2.apply(
      (ns_).value,
      "Spec"));
    String moduleNameString = hydra.ext.haskell.Testing.namespaceToModuleName(specNs);
    hydra.util.Lazy<String> header = new hydra.util.Lazy<>(() -> hydra.lib.strings.Intercalate.apply(
      "\n",
      hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
        java.util.Arrays.asList(
          hydra.lib.strings.Cat2.apply(
            "-- ",
            hydra.Constants.warningAutoGeneratedFile()),
          ""),
        java.util.Arrays.asList(
          "",
          hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "module ",
            moduleNameString,
            " where")),
          ""),
        allImports.get(),
        java.util.Arrays.asList(
          "",
          "spec :: H.Spec",
          hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
            "spec = H.describe ",
            hydra.lib.literals.ShowString.apply(groupName_),
            " $ do")))))));
    return hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      header.get(),
      "\n",
      testBody,
      "\n"));
  }

  static java.util.Set<hydra.core.Name> collectNames(hydra.graph.Graph graf, java.util.Set<hydra.core.Name> names, hydra.core.Term t) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.Predicates.isEncodedTerm(hydra.Strip.deannotateTerm(t)),
      () -> hydra.lib.eithers.Either.apply(
        (java.util.function.Function<hydra.errors.DecodingError, java.util.Set<hydra.core.Name>>) (ignored -> names),
        (java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>>) (decodedTerm -> hydra.lib.sets.Union.apply(
          names,
          hydra.Dependencies.termDependencyNames(
            true,
            true,
            true,
            decodedTerm))),
        hydra.lib.eithers.Bimap.apply(
          (java.util.function.Function<hydra.errors.DecodingError, hydra.errors.DecodingError>) (_e -> _e),
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (_a -> _a),
          hydra.decode.Core.term(
            graf,
            t))),
      () -> names);
  }

  static java.util.List<hydra.testing.TestCaseWithMetadata> collectTestCases(hydra.testing.TestGroup tg) {
    return hydra.lib.lists.Concat2.apply(
      (tg).cases,
      hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
        hydra.ext.haskell.Testing::collectTestCases,
        (tg).subgroups)));
  }

  static java.util.Set<hydra.core.Name> extractEncodedTermVariableNames(hydra.graph.Graph graf, hydra.core.Term term) {
    return hydra.Rewriting.foldOverTerm(
      new hydra.coders.TraversalOrder.Pre(),
      (java.util.function.Function<java.util.Set<hydra.core.Name>, java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>>>) (v1 -> (java.util.function.Function<hydra.core.Term, java.util.Set<hydra.core.Name>>) (v2 -> hydra.ext.haskell.Testing.collectNames(
        graf,
        v1,
        v2))),
      (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()),
      term);
  }

  static <T0, T1> java.util.List<T1> extractTestTerms(T0 tcm) {
    return (java.util.List<T1>) (java.util.Collections.<T1>emptyList());
  }

  static <T0> java.util.List<String> findHaskellImports(hydra.packaging.Namespaces<hydra.ext.haskell.syntax.ModuleName> namespaces, T0 names_) {
    hydra.util.Lazy<java.util.Map<hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName>> mapping_ = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.packaging.Namespaces<hydra.ext.haskell.syntax.ModuleName>, java.util.Map<hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName>>) (projected -> projected.mapping)).apply(namespaces));
    hydra.util.Lazy<java.util.Map<hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName>> filtered = new hydra.util.Lazy<>(() -> hydra.lib.maps.FilterWithKey.apply(
      (java.util.function.Function<hydra.packaging.Namespace, java.util.function.Function<hydra.ext.haskell.syntax.ModuleName, Boolean>>) (ns_ -> (java.util.function.Function<hydra.ext.haskell.syntax.ModuleName, Boolean>) (_v -> hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
        hydra.lib.lists.Head.apply(hydra.lib.strings.SplitOn.apply(
          "hydra.test.",
          (ns_).value)),
        "")))),
      mapping_.get()));
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName>, String>) (entry -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
        "import qualified ",
        hydra.lib.strings.Intercalate.apply(
          ".",
          hydra.lib.lists.Map.apply(
            hydra.Formatting::capitalize,
            hydra.lib.strings.SplitOn.apply(
              ".",
              hydra.lib.pairs.First.apply(entry).value))),
        " as ",
        hydra.lib.pairs.Second.apply(entry).value))),
      hydra.lib.maps.ToList.apply(filtered.get()));
  }

  static hydra.util.Either<String, hydra.util.Pair<String, String>> generateHaskellTestFile(hydra.packaging.Module testModule, hydra.testing.TestGroup testGroup, hydra.graph.Graph g) {
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.haskell.Testing.buildNamespacesForTestGroup(
        testModule,
        testGroup,
        g),
      (java.util.function.Function<hydra.packaging.Namespaces<hydra.ext.haskell.syntax.ModuleName>, hydra.util.Either<String, hydra.util.Pair<String, String>>>) (namespaces -> hydra.ext.haskell.Testing.generateTestFile(
        testModule,
        testGroup,
        namespaces)));
  }

  static <T0, T1> hydra.util.Either<T1, java.util.List<String>> generateTestCase(T0 depth, hydra.testing.TestCaseWithMetadata tcm) {
    hydra.testing.TestCase tcase = (tcm).case_;
    hydra.testing.UniversalTestCase universal = (tcase).accept(new hydra.testing.TestCase.PartialVisitor<>() {
      @Override
      public hydra.testing.UniversalTestCase visit(hydra.testing.TestCase.Universal u) {
        return (u).value;
      }
    });
    String actual_ = (universal).actual;
    String expected_ = (universal).expected;
    String name_ = (tcm).name;
    return hydra.util.Either.<T1, java.util.List<String>>right(java.util.Arrays.asList(
      hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
        "H.it ",
        hydra.lib.literals.ShowString.apply(name_),
        " $ H.shouldBe")),
      hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
        "  (",
        actual_,
        ")")),
      hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
        "  (",
        expected_,
        ")"))));
  }

  static <T0> hydra.util.Either<T0, hydra.util.Pair<String, String>> generateTestFile(hydra.packaging.Module testModule, hydra.testing.TestGroup testGroup, hydra.packaging.Namespaces<hydra.ext.haskell.syntax.ModuleName> namespaces) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<String, hydra.util.Pair<String, String>>) (testBody -> {
        hydra.packaging.Namespace ns_ = (testModule).namespace;
        hydra.packaging.Namespace specNs = new hydra.packaging.Namespace(hydra.lib.strings.Cat2.apply(
          (ns_).value,
          "Spec"));
        String filePath = hydra.Names.namespaceToFilePath(
          new hydra.util.CaseConvention.Pascal(),
          new hydra.packaging.FileExtension("hs"),
          specNs);
        String testModuleContent = hydra.ext.haskell.Testing.buildTestModule(
          testModule,
          testGroup,
          testBody,
          namespaces);
        return (hydra.util.Pair<String, String>) ((hydra.util.Pair<String, String>) (new hydra.util.Pair<String, String>(filePath, testModuleContent)));
      }),
      hydra.ext.haskell.Testing.<T0>generateTestGroupHierarchy(
        1,
        testGroup));
  }

  static <T0> hydra.util.Either<T0, String> generateTestGroupHierarchy(Integer depth, hydra.testing.TestGroup testGroup) {
    java.util.List<hydra.testing.TestCaseWithMetadata> cases_ = (testGroup).cases;
    hydra.util.Lazy<String> indent = new hydra.util.Lazy<>(() -> hydra.lib.strings.FromList.apply(hydra.lib.lists.Replicate.apply(
      hydra.lib.math.Mul.apply(
        depth,
        2),
      32)));
    java.util.List<hydra.testing.TestGroup> subgroups = (testGroup).subgroups;
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.testing.TestCaseWithMetadata, hydra.util.Either<T0, java.util.List<String>>>) (tc -> hydra.ext.haskell.Testing.<Integer, T0>generateTestCase(
          depth,
          tc)),
        cases_),
      (java.util.function.Function<java.util.List<java.util.List<String>>, hydra.util.Either<T0, String>>) (testCaseLinesRaw -> {
        hydra.util.Lazy<java.util.List<java.util.List<String>>> testCaseLines = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          (java.util.function.Function<java.util.List<String>, java.util.List<String>>) (lines_ -> hydra.lib.lists.Map.apply(
            (java.util.function.Function<String, String>) (line -> hydra.lib.strings.Cat2.apply(
              indent.get(),
              line)),
            lines_)),
          testCaseLinesRaw));
        hydra.util.Lazy<String> testCasesStr = new hydra.util.Lazy<>(() -> hydra.lib.strings.Intercalate.apply(
          "\n",
          hydra.lib.lists.Concat.apply(testCaseLines.get())));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<String, String>) (subgroupsStr -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
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
              () -> "\n"),
            subgroupsStr))),
          hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.util.List<String>, String>) (blocks -> hydra.lib.strings.Intercalate.apply(
              "\n",
              blocks)),
            hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.testing.TestGroup, hydra.util.Either<T0, String>>) (subgroup -> {
                String groupName_ = (subgroup).name;
                return hydra.lib.eithers.Map.apply(
                  (java.util.function.Function<String, String>) (content -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
                    indent.get(),
                    "H.describe ",
                    hydra.lib.literals.ShowString.apply(groupName_),
                    " $ do\n",
                    content))),
                  hydra.ext.haskell.Testing.<T0>generateTestGroupHierarchy(
                    hydra.lib.math.Add.apply(
                      depth,
                      1),
                    subgroup));
              }),
              subgroups)));
      }));
  }

  static String namespaceToModuleName(hydra.packaging.Namespace ns_) {
    return hydra.lib.strings.Intercalate.apply(
      ".",
      hydra.lib.lists.Map.apply(
        hydra.Formatting::capitalize,
        hydra.lib.strings.SplitOn.apply(
          ".",
          (ns_).value)));
  }
}
