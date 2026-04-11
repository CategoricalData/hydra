// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalFunctionArgumentOrStringArgumentOrNestedTraversal implements Serializable, Comparable<TraversalFunctionArgumentOrStringArgumentOrNestedTraversal> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal");

  public static final hydra.core.Name FUNCTION = new hydra.core.Name("function");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  public static final hydra.core.Name TRAVERSAL = new hydra.core.Name("traversal");

  private TraversalFunctionArgumentOrStringArgumentOrNestedTraversal () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Function instance) ;

    R visit(String_ instance) ;

    R visit(Traversal instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalFunctionArgumentOrStringArgumentOrNestedTraversal instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Function instance) {
      return otherwise(instance);
    }

    default R visit(String_ instance) {
      return otherwise(instance);
    }

    default R visit(Traversal instance) {
      return otherwise(instance);
    }
  }

  public static final class Function extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalFunctionArgument value;

    public Function (hydra.ext.org.apache.tinkerpop.gremlin.TraversalFunctionArgument value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Function)) {
        return false;
      }
      Function o = (Function) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalFunctionArgumentOrStringArgumentOrNestedTraversal other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Function o = (Function) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class String_ extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value;

    public String_ (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalFunctionArgumentOrStringArgumentOrNestedTraversal other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      String_ o = (String_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Traversal extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value;

    public Traversal (hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Traversal)) {
        return false;
      }
      Traversal o = (Traversal) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalFunctionArgumentOrStringArgumentOrNestedTraversal other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Traversal o = (Traversal) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
