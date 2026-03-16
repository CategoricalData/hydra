// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A language-agnostic test generator abstraction, parameterized by the namespace/module name type
 */
public class TestGenerator<A> implements Serializable, Comparable<TestGenerator<A>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.testing.TestGenerator");
  
  public static final hydra.core.Name NAMESPACES_FOR_MODULE = new hydra.core.Name("namespacesForModule");
  
  public static final hydra.core.Name CREATE_CODEC = new hydra.core.Name("createCodec");
  
  public static final hydra.core.Name GENERATE_TEST_FILE = new hydra.core.Name("generateTestFile");
  
  public static final hydra.core.Name AGGREGATOR_FILE = new hydra.core.Name("aggregatorFile");
  
  /**
   * Build namespaces for a module, resolving all imports and primitives
   */
  public final java.util.function.Function<hydra.module.Module, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, hydra.module.Namespaces<A>>>> namespacesForModule;
  
  /**
   * Create a test codec from resolved namespaces
   */
  public final java.util.function.Function<hydra.module.Namespaces<A>, hydra.testing.TestCodec> createCodec;
  
  /**
   * Generate a complete test file for a module and test group
   */
  public final java.util.function.Function<hydra.module.Module, java.util.function.Function<hydra.testing.TestGroup, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, hydra.util.Pair<String, String>>>>> generateTestFile;
  
  /**
   * Generate an aggregator file (e.g., Spec.hs for Haskell, conftest.py for Python). Takes base directory and list of modules, returns (filepath, content) or Nothing if not needed
   */
  public final hydra.util.Maybe<java.util.function.Function<String, java.util.function.Function<hydra.util.ConsList<hydra.module.Module>, hydra.util.Pair<String, String>>>> aggregatorFile;
  
  public TestGenerator (java.util.function.Function<hydra.module.Module, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, hydra.module.Namespaces<A>>>> namespacesForModule, java.util.function.Function<hydra.module.Namespaces<A>, hydra.testing.TestCodec> createCodec, java.util.function.Function<hydra.module.Module, java.util.function.Function<hydra.testing.TestGroup, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, hydra.util.Pair<String, String>>>>> generateTestFile, hydra.util.Maybe<java.util.function.Function<String, java.util.function.Function<hydra.util.ConsList<hydra.module.Module>, hydra.util.Pair<String, String>>>> aggregatorFile) {
    this.namespacesForModule = namespacesForModule;
    this.createCodec = createCodec;
    this.generateTestFile = generateTestFile;
    this.aggregatorFile = aggregatorFile;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TestGenerator)) {
      return false;
    }
    TestGenerator o = (TestGenerator) other;
    return java.util.Objects.equals(
      this.namespacesForModule,
      o.namespacesForModule) && java.util.Objects.equals(
      this.createCodec,
      o.createCodec) && java.util.Objects.equals(
      this.generateTestFile,
      o.generateTestFile) && java.util.Objects.equals(
      this.aggregatorFile,
      o.aggregatorFile);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(namespacesForModule) + 3 * java.util.Objects.hashCode(createCodec) + 5 * java.util.Objects.hashCode(generateTestFile) + 7 * java.util.Objects.hashCode(aggregatorFile);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TestGenerator other) {
    int cmp = 0;
    cmp = Integer.compare(
      namespacesForModule.hashCode(),
      other.namespacesForModule.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      createCodec.hashCode(),
      other.createCodec.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      generateTestFile.hashCode(),
      other.generateTestFile.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) aggregatorFile).compareTo(other.aggregatorFile);
  }
  
  public TestGenerator withNamespacesForModule(java.util.function.Function<hydra.module.Module, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, hydra.module.Namespaces<A>>>> namespacesForModule) {
    return new TestGenerator(namespacesForModule, createCodec, generateTestFile, aggregatorFile);
  }
  
  public TestGenerator withCreateCodec(java.util.function.Function<hydra.module.Namespaces<A>, hydra.testing.TestCodec> createCodec) {
    return new TestGenerator(namespacesForModule, createCodec, generateTestFile, aggregatorFile);
  }
  
  public TestGenerator withGenerateTestFile(java.util.function.Function<hydra.module.Module, java.util.function.Function<hydra.testing.TestGroup, java.util.function.Function<hydra.graph.Graph, hydra.util.Either<String, hydra.util.Pair<String, String>>>>> generateTestFile) {
    return new TestGenerator(namespacesForModule, createCodec, generateTestFile, aggregatorFile);
  }
  
  public TestGenerator withAggregatorFile(hydra.util.Maybe<java.util.function.Function<String, java.util.function.Function<hydra.util.ConsList<hydra.module.Module>, hydra.util.Pair<String, String>>>> aggregatorFile) {
    return new TestGenerator(namespacesForModule, createCodec, generateTestFile, aggregatorFile);
  }
}
