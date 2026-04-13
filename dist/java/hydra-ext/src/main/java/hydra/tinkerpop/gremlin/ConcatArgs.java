// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class ConcatArgs implements Serializable, Comparable<ConcatArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.ConcatArgs");

  public static final hydra.core.Name TRAVERSAL = new hydra.core.Name("traversal");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  private ConcatArgs () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Traversal instance) ;

    R visit(String_ instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ConcatArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Traversal instance) {
      return otherwise(instance);
    }

    default R visit(String_ instance) {
      return otherwise(instance);
    }
  }

  public static final class Traversal extends hydra.tinkerpop.gremlin.ConcatArgs implements Serializable {
    public final java.util.List<hydra.tinkerpop.gremlin.NestedTraversal> value;

    public Traversal (java.util.List<hydra.tinkerpop.gremlin.NestedTraversal> value) {
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
    public int compareTo(ConcatArgs other) {
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

  public static final class String_ extends hydra.tinkerpop.gremlin.ConcatArgs implements Serializable {
    public final java.util.List<hydra.tinkerpop.gremlin.StringNullableArgument> value;

    public String_ (java.util.List<hydra.tinkerpop.gremlin.StringNullableArgument> value) {
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
    public int compareTo(ConcatArgs other) {
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
}
