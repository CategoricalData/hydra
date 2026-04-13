// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class WhereArgs implements Serializable, Comparable<WhereArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.WhereArgs");

  public static final hydra.core.Name PREDICATE = new hydra.core.Name("predicate");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  public static final hydra.core.Name TRAVERSAL = new hydra.core.Name("traversal");

  private WhereArgs () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Predicate instance) ;

    R visit(String_ instance) ;

    R visit(Traversal instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(WhereArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Predicate instance) {
      return otherwise(instance);
    }

    default R visit(String_ instance) {
      return otherwise(instance);
    }

    default R visit(Traversal instance) {
      return otherwise(instance);
    }
  }

  public static final class Predicate extends hydra.tinkerpop.gremlin.WhereArgs implements Serializable {
    public final hydra.tinkerpop.gremlin.WhereWithPredicateArgs value;

    public Predicate (hydra.tinkerpop.gremlin.WhereWithPredicateArgs value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Predicate)) {
        return false;
      }
      Predicate o = (Predicate) other;
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
    public int compareTo(WhereArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Predicate o = (Predicate) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class String_ extends hydra.tinkerpop.gremlin.WhereArgs implements Serializable {
    public final hydra.tinkerpop.gremlin.StringArgument value;

    public String_ (hydra.tinkerpop.gremlin.StringArgument value) {
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
    public int compareTo(WhereArgs other) {
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

  public static final class Traversal extends hydra.tinkerpop.gremlin.WhereArgs implements Serializable {
    public final hydra.tinkerpop.gremlin.NestedTraversal value;

    public Traversal (hydra.tinkerpop.gremlin.NestedTraversal value) {
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
    public int compareTo(WhereArgs other) {
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
