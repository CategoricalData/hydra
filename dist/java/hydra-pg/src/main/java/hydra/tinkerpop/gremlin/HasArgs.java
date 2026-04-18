// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class HasArgs implements Serializable, Comparable<HasArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.HasArgs");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  public static final hydra.core.Name TRAVERSAL_TOKEN = new hydra.core.Name("traversalToken");

  private HasArgs () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(String_ instance) ;

    R visit(TraversalToken instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(HasArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(String_ instance) {
      return otherwise(instance);
    }

    default R visit(TraversalToken instance) {
      return otherwise(instance);
    }
  }

  public static final class String_ extends hydra.tinkerpop.gremlin.HasArgs implements Serializable {
    public final hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs value;

    public String_ (hydra.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs value) {
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
    public int compareTo(HasArgs other) {
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

  public static final class TraversalToken extends hydra.tinkerpop.gremlin.HasArgs implements Serializable {
    public final hydra.tinkerpop.gremlin.HasTraversalTokenArgs value;

    public TraversalToken (hydra.tinkerpop.gremlin.HasTraversalTokenArgs value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TraversalToken)) {
        return false;
      }
      TraversalToken o = (TraversalToken) other;
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
    public int compareTo(HasArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TraversalToken o = (TraversalToken) other;
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
