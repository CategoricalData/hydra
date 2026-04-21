# Hydra-Java

This directory contains a **complete Java implementation** of Hydra.
Hydra-Java passes all tests in the
[common test suite](https://github.com/CategoricalData/hydra/wiki/Testing),
ensuring identical behavior with Hydra-Haskell and Hydra-Python.

Hydra is a type-aware data transformation toolkit which aims to be highly flexible and portable.
It has its roots in graph databases and type theory, and provides APIs in Haskell, Java, Python, Scala, and Lisp.
See the main Hydra [README](https://github.com/CategoricalData/hydra) for more details.

JavaDocs for Hydra-Java can be found [here](https://categoricaldata.github.io/hydra/hydra-java/javadoc),
and releases can be found on Maven Central [here](https://central.sonatype.com/artifact/net.fortytwo.hydra/hydra-java).

## Getting Started

Hydra-Java requires Java 11 or later. Build the project with Gradle (from the repo root):

```bash
./gradlew :hydra-java:build
```

To publish the resulting JAR to your local Maven repository:

```bash
./gradlew :hydra-java:publishToMavenLocal
```

You may need to set the `JAVA_HOME` environment variable:

```bash
JAVA_HOME=/path/to/java11/installation ./gradlew :hydra-java:build
```

### Performance note: native JDK on Apple Silicon

On Apple Silicon (M1/M2/M3/M4) Macs, using an x86_64 JDK (which runs under Rosetta 2
translation) can cause **~20x slower** code generation and test execution compared to a
native arm64 JDK. This applies to any Java version — the critical factor is architecture,
not version number.

To check your JDK's architecture:

```bash
file "$(which java)"
# arm64 = native (fast), x86_64 = Rosetta (slow)
```

If you see `x86_64`, install a native arm64 JDK. Downloads are available from
[Oracle](https://www.oracle.com/java/technologies/downloads/),
[Adoptium](https://adoptium.net/), or via Homebrew (`brew install openjdk@11`).

To compare Hydra's performance across JDK versions on your machine, use the benchmark
test runner with `--tag` to label each run and `--repeat` for statistical reliability:

```bash
export JAVA_HOME=$(/usr/libexec/java_home -v 11)
bin/run-benchmark-tests.sh --hosts java --tag java11 --repeat 5

export JAVA_HOME=$(/usr/libexec/java_home -v 17)
bin/run-benchmark-tests.sh --hosts java --tag java17 --repeat 5

# Compare results
bin/run-benchmark-tests.sh dashboard diff --old java11 --new java17
```

## Documentation

For comprehensive documentation about Hydra's architecture and usage, see:

- **[Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts)** - Core concepts and type system
- **[Implementation](https://github.com/CategoricalData/hydra/blob/main/docs/implementation.md)** - Implementation guide
- **[Code Organization](https://github.com/CategoricalData/hydra/wiki/Code-organization)** -
  The `packages/`, `heads/`, `dist/` layout
- **[Testing](https://github.com/CategoricalData/hydra/wiki/Testing)** -
  Common test suite documentation
- **[Developer Recipes](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/index.md)** -
  Step-by-step guides

## Testing

Hydra-Java has two types of tests: the **common test suite** (shared across all Hydra implementations)
and **Java-specific tests**.
See the [Testing wiki page](https://github.com/CategoricalData/hydra/wiki/Testing) for comprehensive documentation.

### Common Test Suite

The common test suite (`hydra.test.testSuite`) ensures parity across all Hydra implementations.
**Passing all common test suite cases is the criterion for a true Hydra implementation.**

To run all tests:

```bash
./gradlew :hydra-java:test
```

The test suite is generated from Hydra DSL sources and includes:
- Primitive function tests (lists, strings, math, etc.)
- Case conversion tests (camelCase, snake_case, etc.)
- Type inference tests
- Type checking tests
- Evaluation tests
- JSON coder tests
- Rewriting and hoisting tests

### Java-Specific Tests

Java-specific tests validate implementation details and Java-specific functionality.
These are located in `src/test/java/` alongside the common test suite runner.

To run a specific test class:

```bash
./gradlew :hydra-java:test --tests "hydra.VisitorTest"

# (Note: run all gradle commands from the repo root; this package does not have
# its own gradle wrapper.)
```

## Code organization

In 0.15, Hydra's Java code is split across three locations
(see [Code organization wiki page](https://github.com/CategoricalData/hydra/wiki/Code-organization) for the full picture):

- **This package** (`packages/hydra-java/src/main/haskell/`) — the Java coder DSL sources
  (written in Haskell): `Hydra/Sources/Java/` contains `Syntax`, `Language`, `Coder`, `Serde`,
  `Names`, `Utils`, `Environment`, and `Testing` modules.

- **Java head** ([`heads/java/src/main/java/`](https://github.com/CategoricalData/hydra/tree/main/heads/java/src/main/java))
  — hand-written Java runtime
  - `hydra/lib/` — primitive function implementations
  - `hydra/dsl/` — Java DSL (Terms, Types, Expect, ...)
  - `hydra/util/` — core utilities (Either, Maybe, Pair, Lazy) and internal collection classes
  - `hydra/tools/` — framework classes (PrimitiveFunction, MapperBase, ...)

- **Generated Java kernel** ([`dist/java/hydra-kernel/src/main/java/`](https://github.com/CategoricalData/hydra/tree/main/dist/java/hydra-kernel/src/main/java))
  — code-generated from the kernel DSL sources
  - `hydra/core/`, `hydra/graph/`, `hydra/packaging/`, `hydra/coders/`, `hydra/typing/`, ...
  - `hydra/reduction/`, `hydra/rewriting/`, `hydra/hoisting/`
  - `hydra/inference/`, `hydra/checking/`

- **Generated Java test suite** (`dist/java/hydra-kernel/src/test/java/`) —
  the common test suite compiled into Java.

## Generate Java code

Java code is generated from the Haskell head, using the Java coder DSL sources in this package.
See the [Hydra-Haskell README](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-haskell)
for more information on how code generation works.

The recommended way to regenerate all Java code is the sync script (from the repo root):

```bash
bin/sync-java.sh
```

(equivalent to `bin/sync.sh --hosts java --targets java`)

This will:
1. Generate the kernel modules into `dist/java/hydra-kernel/src/main/java`
2. Generate the eval lib modules
3. Generate the kernel tests into `dist/java/hydra-kernel/src/test/java`
4. Build and run all tests

For manual generation, enter GHCi from `heads/haskell/`:

```bash
cd heads/haskell && stack ghci
```

And run the following in the REPL:

```haskell
import Hydra.Generation
import Hydra.Sources.All

-- Generate the kernel
writeJava "../../dist/java/hydra-kernel/src/main/java" kernelModules kernelModules

-- Generate the test suite
let allModules = mainModules ++ testModules
writeJava "../../dist/java/hydra-kernel/src/test/java" allModules baseTestModules
```

## Design notes

### Algebraic data types

The Java coder DSL sources that drive Java code generation live
[here](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-java/src/main/haskell/Hydra/Sources/Java).
A variety of techniques are used in order to materialize Hydra's core language in Java,
including a [pattern](https://garciat.com/posts/java-adt) for representing algebraic data types
which was originally proposed by Gabriel Garcia,
and used in [Dragon](https://eng.uber.com/dragon-schema-integration-at-uber-scale).

For example, the generated `Vertex` class represents a property graph vertex, and corresponds to a record type:

```java
public class Vertex<V> {
  public final hydra.pg.model.VertexLabel label;
  public final V id;
  public final java.util.Map<hydra.pg.model.PropertyKey, V> properties;

  public Vertex (hydra.pg.model.VertexLabel label, V id, java.util.Map<hydra.pg.model.PropertyKey, V> properties) {
    java.util.Objects.requireNonNull((label));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((properties));
    this.label = label;
    this.id = id;
    this.properties = properties;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Vertex)) {
      return false;
    }
    Vertex o = (Vertex) (other);
    return label.equals(o.label) && id.equals(o.id) && properties.equals(o.properties);
  }

  @Override
  public int hashCode() {
    return 2 * label.hashCode() + 3 * id.hashCode() + 5 * properties.hashCode();
  }

  // ... with* methods for immutable updates
}
```

See
[Vertex.java](https://github.com/CategoricalData/hydra/blob/main/dist/java/hydra-kernel/src/main/java/hydra/pg/model/Vertex.java)
for the complete class, as well as the `Vertex` type in
[Pg/Model.hs](https://github.com/CategoricalData/hydra/blob/main/dist/haskell/hydra-pg/src/main/haskell/Hydra/Pg/Model.hs)
for comparison.
Both files were generated from the property graph model defined
[here](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-pg/src/main/haskell/Hydra/Sources/Pg/Model.hs).

### Union Types and Visitors

Union types (sum types) are represented using the visitor pattern.
For example, the `Element` type is a tagged union of `Vertex` and `Edge`:

```java
public abstract class Element<V> {
  private Element () {}

  public abstract <R> R accept(Visitor<V, R> visitor) ;

  public interface Visitor<V, R> {
    R visit(Vertex<V> instance) ;
    R visit(Edge<V> instance) ;
  }

  public interface PartialVisitor<V, R> extends Visitor<V, R> {
    default R otherwise(Element<V> instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    default R visit(Vertex<V> instance) { return otherwise((instance)); }
    default R visit(Edge<V> instance) { return otherwise((instance)); }
  }

  public static final class Vertex<V> extends hydra.pg.model.Element<V> {
    public final hydra.pg.model.Vertex<V> value;
    // ... constructor, equals, hashCode, accept
  }

  public static final class Edge<V> extends hydra.pg.model.Element<V> {
    public final hydra.pg.model.Edge<V> value;
    // ... constructor, equals, hashCode, accept
  }
}
```

See
[Element.java](https://github.com/CategoricalData/hydra/blob/main/dist/java/hydra-kernel/src/main/java/hydra/pg/model/Element.java)
for the complete class.
The `Visitor` class is for pattern matching over the alternatives,
and `PartialVisitor` is a convenient extension which allows supplying a default value
for alternatives not matched explicitly.

The
[Rewriting](https://github.com/CategoricalData/hydra/blob/main/dist/java/hydra-kernel/src/main/java/hydra/rewriting/Rewriting.java)
and
[Reduction](https://github.com/CategoricalData/hydra/blob/main/dist/java/hydra-kernel/src/main/java/hydra/reduction/Reduction.java)
classes are good examples of pattern matching in action, and there are simpler examples in
[VisitorTest.java](https://github.com/CategoricalData/hydra/blob/main/heads/java/src/test/java/hydra/VisitorTest.java).
