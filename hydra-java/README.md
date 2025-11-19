# Hydra-Java

Hydra is a type-aware data transformation toolkit which aims to be highly flexible and portable.
It has its roots in graph databases and type theory, and provides APIs in Haskell and Java.
See the main Hydra [README](https://github.com/CategoricalData/hydra) for more details.
This package contains Hydra's Java API and Java sources specifically.
JavaDocs for Hydra-Java can be found [here](https://categoricaldata.github.io/hydra/hydra-java/javadoc),
and releases can be found on Maven Central [here](https://central.sonatype.com/artifact/net.fortytwo.hydra/hydra-java).

## Code Organization

Hydra-Java uses the **src/main vs src/gen-main** separation pattern (see [Code organization wiki page](https://github.com/CategoricalData/hydra/wiki/Code-organization) for details).

- **`src/main/java/`** - Hand-written Java code
  - `hydra/lib/` - Primitive function implementations
  - `hydra/util/` - Core utilities (Either, Maybe, Tuple, Unit, etc.)
  - `hydra/tools/` - Framework classes (PrimitiveFunction, MapperBase, etc.)
  - `hydra/Rewriting.java`, `hydra/Reduction.java` - Core algorithms
  - Language-specific parsers and extensions

- **`src/gen-main/java/`** - Generated Java code
  - `hydra/core/` - Core types (Term, Type, Literal, etc.)
  - `hydra/graph/`, `hydra/module/` - Graph and module structures
  - `hydra/coders/`, `hydra/compute/` - Type adapters and computational abstractions
  - Generated from Haskell DSL sources using `writeJava` in hydra-ext

- **`src/gen-test/java/`** - Generated test suite
  - Common tests ensuring parity with Haskell and Python
  - Generated using `writeJava "../hydra-java/src/gen-test/java" testModules`

To regenerate the Java code, see the [Hydra-Ext README](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/README.md#code-generation).

## Build

Build the Java project with `./gradlew build`, or publish the resulting JAR to your local Maven repository with `./gradlew publishToMavenLocal`.
This project requires at least Java 11, so you may need to set the `JAVA_HOME` environment variable accordingly: `JAVA_HOME=/path/to/java11/installation ./gradlew build`.

To run the tests, use `./gradlew test`.

## Design notes

### Algebraic data types

The Hydra coder which generates everything in `src/gen-main` and `src/gen-test` can be found [here](https://github.com/CategoricalData/hydra/tree/main/hydra-ext/src/main/haskell/Hydra/Ext/Staging/Java).
A variety of techniques are used in order to materialize Hydra's core language in Java,
including a [pattern](https://garciat.com/posts/java-adt) for representing algebraic data types which was originally proposed by Gabriel Garcia, and used in [Dragon](https://eng.uber.com/dragon-schema-integration-at-uber-scale).
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

See [Vertex.java](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/gen-main/java/hydra/pg/model/Vertex.java) for the complete class,
as well as the `Vertex` type in [Pg/Model.hs](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/gen-main/haskell/Hydra/Pg/Model.hs) for comparison.
Both files were generated from the property graph model defined [here](https://github.com/CategoricalData/hydra/blob/main/hydra-ext/src/main/haskell/Hydra/Ext/Sources/Pg/Model.hs).
In each instantiation of the model, a vertex is an object with a label, an id, and a key/value map of properties.
The `Edge` type is defined similarly (see [Edge.java](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/gen-main/java/hydra/pg/model/Edge.java)).
Now compare this with the generated `Element` class, which is a tagged union of `Vertex` and `Edge`:

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

    default R visit(Vertex<V> instance) {
      return otherwise((instance));
    }

    default R visit(Edge<V> instance) {
      return otherwise((instance));
    }
  }

  public static final class Vertex<V> extends hydra.pg.model.Element<V> {
    public final hydra.pg.model.Vertex<V> value;

    public Vertex (hydra.pg.model.Vertex<V> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Vertex)) {
        return false;
      }
      Vertex o = (Vertex) (other);
      return value.equals(o.value);
    }

    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }

    @Override
    public <R> R accept(Visitor<V, R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Edge<V> extends hydra.pg.model.Element<V> {
    public final hydra.pg.model.Edge<V> value;

    public Edge (hydra.pg.model.Edge<V> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Edge)) {
        return false;
      }
      Edge o = (Edge) (other);
      return value.equals(o.value);
    }

    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }

    @Override
    public <R> R accept(Visitor<V, R> visitor) {
      return visitor.visit(this);
    }
  }
}
```

See [Element.java](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/gen-main/java/hydra/pg/model/Element.java) for the complete class.
Notice that we have defined two inner classes, `Element.Vertex` and `Element.Edge`, for each alternative of the union,
as well as a pair of "visitor" classes: `Element.Visitor` and `Element.PartialVisitor`.
The first two classes are used to instantiate an `Element` either as the first alternative (a vertex) or the second (an edge).
The `Visitor` class is for pattern matching over the alternatives, and `PartialVisitor` is a convenient extension of the same which allows us to supply a default value for alternatives we do not care to match explicitly.
The [Rewriting](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/main/java/hydra/Rewriting.java) and [Reduction](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/main/java/hydra/Reduction.java) classes are good examples of pattern matching in action,
and there are simpler examples in [VisitorTest.java](https://github.com/CategoricalData/hydra/blob/main/hydra-java/src/test/java/hydra/VisitorTest.java).
